(in-package #:supervisor-protocol)

;;; One lock per supervisor (slot LOCK) guards the registry, the counters, every
;;; unit's state slots and the event log; one condition variable (slot CV) is
;;; broadcast on every change. No user code (executor GFs, sinks, callbacks,
;;; thread bodies, condition handlers) runs while the lock is held.
;;;
;;; Each launch is watched by one waiter thread: EXECUTOR-WAIT (no timeout),
;;; EXECUTOR-RELEASE, then the final state is published under the lock. A unit's
;;; GEN counts its launches; a waiter publishes only while its generation is
;;; current and the unit was not abandoned. A restart is the same waiter thread
;;; sleeping the backoff on the cv (a stop clears RESTART-PENDING and wakes it),
;;; then relaunching and waiting for the new launch.

(defvar *supervisors* '() "Live supervisors, oldest first.")
(defvar *supervisors-lock* (bt2:make-lock :name "supervisor registry"))

;;; ------------------------------------------------------------------
;;; Internal helpers

(defun %graceful-p (unit)
  (and (executor-supports-p (unit-executor unit) :graceful-stop) t))

(defun %stale-p (unit gen)
  (or (slot-value unit 'abandoned-p) (/= gen (slot-value unit 'gen))))

(defun %lru (units)
  (let ((best nil))
    (dolist (u units best)
      (when (or (null best) (< (unit-last-used u) (unit-last-used best)))
        (setf best u)))))

(defun %touch (sup unit)
  "Lock held."
  (setf (slot-value unit 'last-used) (incf (slot-value sup 'tick))))

(defun %scope-subtree (scope)
  "SCOPE and its descendants (lock held)."
  (cons scope (loop for c in (scope-children scope) append (%scope-subtree c))))

(defun %owned-by-p (unit scope recursive)
  (if recursive
      (loop for s = (unit-owner unit) then (scope-parent s)
            while s thereis (eq s scope))
      (eq (unit-owner unit) scope)))

(defun %matches-p (value spec)
  (if (listp spec) (member value spec) (eq value spec)))

(defun %labels-match-p (unit labels)
  (let ((have (unit-labels unit)))
    (loop for (k v) on labels by #'cddr
          always (loop for (hk hv) on have by #'cddr
                       thereis (and (eq hk k) (equal hv v))))))

(defun %sink-close-quietly (unit)
  (let ((sink (unit-output-sink unit)))
    (when sink
      ;; A broken sink must not take the supervisor's own threads down.
      (handler-case (sink-close sink) (error () nil)))))

(defun %after-finish (unit)
  "UNIT became finished (lock NOT held): drop its file (unless abandoned), close its sink."
  (unless (slot-value unit 'abandoned-p) (%delete-unit-file unit))
  (%sink-close-quietly unit))

(defun %send-stop (unit force)
  "EXECUTOR-STOP on UNIT's current handle (lock NOT held)."
  (let ((handle (slot-value unit 'handle)))
    (when handle
      ;; A misbehaving custom executor: the escalation (force, stop-failed) still applies.
      (handler-case (executor-stop (unit-executor unit) unit handle :force force)
        (error () nil)))))

(defun %launch (unit)
  "Launch UNIT (lock NOT held). → (values handle pid pgid os-start). Errors propagate."
  (let* ((exec (unit-executor unit))
         (handle (executor-launch exec unit))
         (pid (executor-pid exec handle))
         (pgid (executor-pgid exec handle)))
    (values handle pid pgid (and pid (os-process-start-time pid)))))

(defun %set-launched (sup unit handle pid pgid os-start)
  "Lock held: record a successful launch and publish :STARTING → :RUNNING."
  (setf (slot-value unit 'handle) handle
        (slot-value unit 'pid) pid
        (slot-value unit 'pgid) pgid
        (slot-value unit 'os-start-time) os-start
        (slot-value unit 'start-time) (get-universal-time)
        (slot-value unit 'state) :running)
  (%publish sup unit :state :starting :running))

(defun %restart-allowed-p (sup unit state)
  (and (eq state :failed)
       (eq (unit-restart-policy unit) :on-failure)
       (< (unit-restart-count unit) (unit-max-restarts unit))
       (not (supervisor-closed-p sup))
       (not (unit-stop-requested-p unit))))

(defun %schedule-restart (sup unit)
  "Lock held: mark a restart pending, publish :RESTART-SCHEDULED. → the delay."
  (let* ((attempt (1+ (unit-restart-count unit)))
         (delay (restart-delay (unit-backoff unit) attempt)))
    (setf (slot-value unit 'restart-pending) t)
    (%publish sup unit :restart-scheduled :failed :starting
              (list :attempt attempt :delay delay))
    delay))

(defun %final-state (unit code condition)
  (cond ((unit-stop-requested-p unit) :killed)
        ((or condition (and (integerp code) (/= code 0))) :failed)
        (t :exited)))

(defun %spawn-waiter (unit gen handle)
  (bt2:make-thread (lambda () (%waiter unit gen handle))
                   :name (format nil "supervisor waiter ~A" (unit-id unit))))

(defun %waiter (unit gen handle)
  "One per launch: wait for HANDLE to end, publish the final state; relaunch on a
scheduled restart and go on waiting for the new launch."
  (let ((sup (unit-supervisor unit))
        (exec (unit-executor unit)))
    ;; This thread must never reach the debugger: a custom executor's error is the
    ;; launch's ending condition.
    (loop
      (multiple-value-bind (ended code condition result)
          (handler-case (executor-wait exec unit handle)
            (error (e) (values t nil e nil)))
        (declare (ignore ended))
        (handler-case (executor-release exec unit handle)
          (error () nil))
        (unless (%stale-p unit gen) (%delete-unit-file unit))
        (let ((delay nil) (finished nil))
          (%with-lock (sup)
            (unless (%stale-p unit gen)
              (let ((from (unit-state unit))
                    (to (%final-state unit code condition)))
                (setf (slot-value unit 'state) to
                      (slot-value unit 'exit-code) code
                      (slot-value unit 'end-condition) condition
                      (slot-value unit 'result) result)
                (%publish sup unit :state from to
                          (list :exit-code code :signal (unit-signal unit)))
                (if (%restart-allowed-p sup unit to)
                    (setf delay (%schedule-restart sup unit))
                    (setf finished t))
                (%broadcast sup))))
          (cond (finished (%after-finish unit) (return))
                ((null delay) (return))
                (t (multiple-value-bind (new-gen new-handle) (%relaunch unit delay)
                     (unless new-handle (return))
                     (setf gen new-gen handle new-handle)))))))))

(defun %relaunch (unit delay)
  "Wait DELAY seconds (a stop cancels), relaunch UNIT. Each failed relaunch is an
attempt. → (values gen handle) of the new launch, or NIL when none happened."
  (let ((sup (unit-supervisor unit))
        (exec (unit-executor unit)))
    (loop
      (let ((deadline (+ (%now) delay)) (go nil) (gen nil) (closed nil))
        (%with-lock (sup)
          (loop
            (cond ((not (slot-value unit 'restart-pending)) (return))
                  ((supervisor-closed-p sup)
                   (setf (slot-value unit 'restart-pending) nil closed t)
                   (%broadcast sup)
                   (return))
                  ((>= (%now) deadline)
                   (setf (slot-value unit 'restart-pending) nil
                         (slot-value unit 'state) :starting
                         gen (incf (slot-value unit 'gen))
                         go t)
                   (incf (slot-value unit 'restart-count))
                   (%publish sup unit :state :failed :starting)
                   (return))
                  (t (%cv-wait sup (- deadline (%now)))))))
        (unless go
          (when closed (%after-finish unit))
          (return nil))
        (multiple-value-bind (handle pid pgid os-start error)
            (handler-case (%launch unit)
              (error (e) (values nil nil nil nil e)))
          (if error
              (let ((next nil) (finished nil))
                (%with-lock (sup)
                  (unless (%stale-p unit gen)
                    (setf (slot-value unit 'state) :failed
                          (slot-value unit 'exit-code) nil
                          (slot-value unit 'end-condition) error)
                    (%publish sup unit :state :starting :failed
                              (list :exit-code nil :signal (unit-signal unit)))
                    (if (%restart-allowed-p sup unit :failed)
                        (setf next (%schedule-restart sup unit))
                        (setf finished t))
                    (%broadcast sup)))
                (cond (finished (%after-finish unit) (return nil))
                      ((null next) (return nil))
                      (t (setf delay next))))
              (let ((stale nil) (stop nil))
                (%with-lock (sup)
                  (if (%stale-p unit gen)
                      (setf stale t)
                      (progn
                        (%set-launched sup unit handle pid pgid os-start)
                        (setf stop (unit-stop-requested-p unit))
                        (%broadcast sup))))
                (when stale
                  ;; Abandoned while relaunching: the new launch must not linger.
                  (handler-case (executor-stop exec unit handle :force t) (error () nil))
                  (return nil))
                (%executor-started exec unit handle)
                (unwind-protect (write-unit-file unit)
                  (when stop (%send-stop unit (not (%graceful-p unit)))))
                (return (values gen handle)))))))))

(defun %await (sup units deadline)
  "Wait until none of UNITS is live, or DEADLINE (%NOW seconds). → T when none is."
  (%with-lock (sup)
    (loop
      (when (notany #'unit-live-p units) (return t))
      (let ((left (- deadline (%now))))
        (when (<= left 0) (return nil))
        (%cv-wait sup left)))))

(defun %abandon (sup unit)
  (%with-lock (sup)
    (when (unit-live-p unit)
      (let ((from (unit-state unit)))
        (incf (slot-value unit 'gen))
        (setf (slot-value unit 'abandoned-p) t
              (slot-value unit 'state) :killed)
        (%publish sup unit :state from :killed
                  (list :exit-code nil :signal (unit-signal unit) :abandoned t))
        (%broadcast sup))))
  (%sink-close-quietly unit))

(defun %graceful-phase-escalates-p (sup unit t0 grace)
  "UNIT outlived the graceful phase: signal STOP-TIMEOUT until it ends (→ NIL) or
the stop is to be forced (→ T; also when the condition is unhandled)."
  (loop
    (unless (unit-live-p unit) (return nil))
    (let ((decision
            (restart-case
                (progn (signal 'stop-timeout :supervisor sup :unit unit :phase :graceful
                                             :waited (- (%now) t0))
                       :force)
              (force ()
                :report "Force the stop now."
                :force)
              (keep-waiting ()
                :report "Wait another grace period, then ask again."
                :wait))))
      (ecase decision
        (:force (return t))
        (:wait (%await sup (list unit) (+ (%now) grace)))))))

(defun %stop-wave (sup units grace force-grace)
  "Stop UNITS in one wave: graceful stop to all, one GRACE, STOP-TIMEOUT per unit
still running, force, one FORCE-GRACE, STOP-FAILED per survivor. Pending restarts
are cancelled. → the units it stopped (live or restart pending), in UNITS order."
  (let ((grace (or grace (supervisor-grace sup)))
        (force-grace (or force-grace (supervisor-force-grace sup)))
        (live '()) (cancelled '()) (stopped '()))
    (%with-lock (sup)
      (dolist (u units)
        (cond ((slot-value u 'restart-pending)
               (setf (slot-value u 'restart-pending) nil
                     (slot-value u 'stop-requested-p) t)
               (push u cancelled)
               (push u stopped))
              ((and (unit-live-p u) (not (slot-value u 'abandoned-p)))
               (setf (slot-value u 'stop-requested-p) t)
               (when (unit-pid u)
                 (setf (slot-value u 'stop-signal) (if (%graceful-p u) :term :kill)))
               (push u stopped)
               ;; A thread unit stopping itself only gets the request.
               (unless (eq u *current-unit*) (push u live)))))
      (%broadcast sup))
    (setf live (nreverse live) stopped (nreverse stopped))
    (mapc #'%after-finish cancelled)
    (dolist (u live) (%send-stop u (not (%graceful-p u))))
    (let ((t0 (%now)) (force '()))
      (%await sup live (+ t0 grace))
      (dolist (u live)
        (when (%graceful-phase-escalates-p sup u t0 grace)
          (push u force)))
      (setf force (nreverse force))
      (when force
        (%with-lock (sup)
          (dolist (u force)
            (when (unit-pid u) (setf (slot-value u 'stop-signal) :kill))))
        (dolist (u force) (%send-stop u t))
        (%await sup force (+ (%now) force-grace))
        (dolist (u force)
          (when (unit-live-p u)
            (restart-case (error 'stop-failed :supervisor sup :unit u)
              (abandon ()
                :report "Mark the unit :KILLED and leave it (its unit file stays for the sweep)."
                (%abandon sup u)))))))
    stopped))

(defun %evict-locked (sup unit)
  "Lock held: remove UNIT from the registry, publish :EVICTED."
  (setf (slot-value sup 'units) (delete unit (slot-value sup 'units)))
  (remhash (unit-id unit) (slot-value sup 'by-id))
  (%publish sup unit :evicted (unit-state unit) nil))

(defun %after-evict (unit)
  (%delete-unit-file unit)
  (%sink-close-quietly unit))

(defun %evictable-p (unit)
  (and (unit-finished-p unit) (not (unit-interacting-p unit))))

(defun %admit (sup kind owner)
  "Lock held. Evict LRU finished units at a cap. → (values verdict limit victim
cap-owner evicted): VERDICT :OK or :FULL."
  (let ((evicted '()))
    (flet ((check (limit member-p)
             (loop
               (let ((set (remove-if-not member-p (slot-value sup 'units))))
                 (when (< (length set) limit) (return nil))
                 (let ((lru (%lru (remove-if-not #'%evictable-p set))))
                   (if lru
                       (progn (%evict-locked sup lru) (push lru evicted))
                       (return (list (%lru (remove-if (lambda (u) (or (unit-finished-p u)
                                                                      (unit-interacting-p u)))
                                                      set))))))))))
      (let ((kind-cap (getf (supervisor-kind-caps sup) kind))
            (owner-cap (supervisor-owner-cap sup)))
        (let ((full (and kind-cap (check kind-cap (lambda (u) (eq (unit-kind u) kind))))))
          (when full
            (return-from %admit (values :full kind-cap (first full) nil evicted))))
        (let ((full (and owner-cap (check owner-cap (lambda (u) (eq (unit-owner u) owner))))))
          (when full
            (return-from %admit (values :full owner-cap (first full) owner evicted))))
        (values :ok nil nil nil evicted)))))

(defun %resolve-owner (sup owner)
  (cond ((null owner)
         (if (and *current-scope* (eq sup (scope-supervisor *current-scope*)))
             *current-scope*
             (supervisor-root-scope sup)))
        ((not (typep owner 'scope))
         (error 'supervisor-error :supervisor sup :message (format nil "not a scope: ~S" owner)))
        ((not (eq sup (scope-supervisor owner)))
         (error 'supervisor-error :supervisor sup
                                  :message "the owner scope belongs to another supervisor"))
        (t owner)))

(defun %exit-hook-function (sup)
  (lambda ()
    (handler-bind ((stop-failed (lambda (c)
                                  (let ((r (find-restart 'abandon c)))
                                    (when r (invoke-restart r))))))
      (shutdown-supervisor sup))))

(defun %install-exit-hook (hook)
  #+sbcl (push hook sb-ext:*exit-hooks*)
  #-sbcl (uiop:register-image-exit-hook hook nil))

(defun %remove-exit-hook (hook)
  #+sbcl (setf sb-ext:*exit-hooks* (remove hook sb-ext:*exit-hooks*))
  #-sbcl (setf uiop:*image-exit-hook* (remove hook uiop:*image-exit-hook*)))

;;; ------------------------------------------------------------------
;;; Supervisor lifecycle

(defun make-supervisor (&key state-directory (sweep t) (exit-hook t) kind-caps owner-cap
                             (grace +default-grace+) (force-grace +default-force-grace+)
                             (event-capacity +default-event-capacity+)
                             (output-sink-factory 'default-output-sink)
                             (process-executor (make-process-executor))
                             (thread-executor (make-thread-executor))
                             (log t)
                             (log-segment-size +default-log-segment-size+)
                             (log-segments +default-log-segments+)
                             (log-total-cap +default-log-total-cap+)
                             (log-max-record +default-log-max-record+)
                             log-error-handler)
  "A new supervisor with its root scope (kind :SUPERVISOR). With STATE-DIRECTORY
(created if missing) process units get unit files, and with SWEEP the orphan
sweep runs first (SUPERVISOR-SWEEP-REPORT). EXIT-HOOK registers a function that
shuts the supervisor down when the image exits normally (SBCL
SB-EXT:*EXIT-HOOKS*; elsewhere UIOP:REGISTER-IMAGE-EXIT-HOOK); on exit an
unhandled STOP-TIMEOUT forces and STOP-FAILED abandons. KIND-CAPS: plist kind →
limit. OWNER-CAP: limit per directly owning scope.
H8b: with STATE-DIRECTORY and LOG, every unit's stdout / stderr goes to rotated
segment files under <state-directory>/units/<supervisor-id>--<unit-id>/ (see
DEFAULT-OUTPUT-SINK): LOG-SEGMENT-SIZE bytes × LOG-SEGMENTS per unit, LOG-MAX-RECORD
output octets per record, LOG-TOTAL-CAP bytes for all of this supervisor's logs.
LOG-ERROR-HANDLER: a function of a LOG-WRITE-ERROR, called when the dynamic
handlers declined; it may invoke RETRY, DROP-TO-MEMORY-ONLY or ABORT-UNIT."
  (check-type event-capacity (integer 0))
  (check-type log-segment-size (integer 1))
  (check-type log-segments (integer 1))
  (check-type log-total-cap (integer 0))
  (check-type log-max-record (integer 1))
  (let* ((dir (and state-directory (uiop:ensure-directory-pathname state-directory)))
         (id (format nil "~D-~(~8,'0x~)" (current-pid)
                     (random #x100000000 (make-random-state t))))
         (sup (make-instance 'supervisor :id id :state-directory dir
                                         :kind-caps kind-caps :owner-cap owner-cap
                                         :grace grace :force-grace force-grace
                                         :event-capacity event-capacity
                                         :output-sink-factory output-sink-factory
                                         :process-executor process-executor
                                         :thread-executor thread-executor
                                         :log log :log-segment-size log-segment-size
                                         :log-segments log-segments :log-total-cap log-total-cap
                                         :log-max-record log-max-record
                                         :log-error-handler log-error-handler)))
    (setf (slot-value sup 'root-scope)
          (make-instance 'scope :supervisor sup :kind :supervisor :name (supervisor-id sup)))
    (when dir
      (ensure-directories-exist (unit-file-directory dir))
      (when sweep
        (setf (slot-value sup 'sweep-report) (sweep-orphans dir))))
    (%start-dispatcher sup)
    (bt2:with-lock-held (*supervisors-lock*)
      (setf *supervisors* (append *supervisors* (list sup))))
    (when exit-hook
      (let ((hook (%exit-hook-function sup)))
        (setf (slot-value sup 'exit-hook) hook)
        (%install-exit-hook hook)))
    sup))

(defun live-supervisors ()
  "Supervisors in this image not yet shut down, oldest first."
  (bt2:with-lock-held (*supervisors-lock*) (copy-list *supervisors*)))

(defun shutdown-supervisor (supervisor &key grace force-grace)
  "Stop every live unit (one stop wave, see STOP-UNITS), delete the unit files,
stop event delivery (subscriptions become inactive), remove the exit hook. Later
START-UNIT / MAKE-SCOPE → SUPERVISOR-CLOSED; queries still answer. Idempotent. → NIL."
  (unless (%with-lock (supervisor)
            (unless (supervisor-closed-p supervisor)
              (setf (slot-value supervisor 'closed-p) t)
              (%broadcast supervisor)
              t))
    (return-from shutdown-supervisor nil))
  (bt2:with-lock-held (*supervisors-lock*)
    (setf *supervisors* (remove supervisor *supervisors*)))
  (unwind-protect
       (%stop-wave supervisor (supervisor-units supervisor) grace force-grace)
    (let ((units (supervisor-units supervisor)))
      (%with-lock (supervisor)
        (dolist (s (%scope-subtree (supervisor-root-scope supervisor)))
          (setf (slot-value s 'closed-p) t)))
      (dolist (u units)
        (unless (slot-value u 'abandoned-p) (%delete-unit-file u))))
    (%stop-dispatcher supervisor)
    (let ((hook (supervisor-exit-hook supervisor)))
      (when hook (%remove-exit-hook hook))))
  nil)

(defun supervisor-units (supervisor)
  "Every unit in the registry, in start order (evicted ones are gone)."
  (%with-lock (supervisor) (reverse (slot-value supervisor 'units))))

;;; ------------------------------------------------------------------
;;; Scopes: session › agent › tool source (any depth, any kind keyword)

(defun make-scope (supervisor kind name &key (parent nil parent-p))
  "A child scope of PARENT (default *CURRENT-SCOPE* when it belongs to SUPERVISOR,
else the root). Closed parent → SCOPE-CLOSED; shut-down supervisor →
SUPERVISOR-CLOSED; a PARENT of another supervisor → SUPERVISOR-ERROR."
  (check-type kind keyword)
  (let ((parent (cond ((and parent-p parent)
                       (unless (and (typep parent 'scope) (eq supervisor (scope-supervisor parent)))
                         (error 'supervisor-error :supervisor supervisor
                                                  :message "the parent scope belongs to another supervisor"))
                       parent)
                      ((and (not parent-p) *current-scope*
                            (eq supervisor (scope-supervisor *current-scope*)))
                       *current-scope*)
                      (t (supervisor-root-scope supervisor))))
        (problem nil))
    (let ((scope (%with-lock (supervisor)
                   (cond ((supervisor-closed-p supervisor) (setf problem :supervisor) nil)
                         ((scope-closed-p parent) (setf problem :scope) nil)
                         (t (let ((s (make-instance 'scope :supervisor supervisor :kind kind
                                                           :name name :parent parent)))
                              (setf (slot-value parent 'children)
                                    (append (scope-children parent) (list s)))
                              s))))))
      (case problem
        (:supervisor (error 'supervisor-closed :supervisor supervisor))
        (:scope (error 'scope-closed :supervisor supervisor :scope parent))
        (t scope)))))

(defun scope-units (scope &key (recursive t))
  "Units owned by SCOPE (and, RECURSIVE, by its descendants), in start order."
  (find-units (scope-supervisor scope) :owner scope :recursive recursive))

(defun close-scope (scope &key grace force-grace)
  "Close SCOPE and its descendants and stop every live unit they own, in one stop
wave. Units stay queryable. → the units it stopped. Idempotent (→ NIL). The root
scope → SUPERVISOR-ERROR (use SHUTDOWN-SUPERVISOR)."
  (let ((sup (scope-supervisor scope))
        (targets '()))
    (unless (scope-parent scope)
      (error 'supervisor-error :supervisor sup
                               :message "the root scope closes with shutdown-supervisor"))
    (unless (%with-lock (sup)
              (unless (scope-closed-p scope)
                (let ((scopes (%scope-subtree scope)))
                  (dolist (s scopes) (setf (slot-value s 'closed-p) t))
                  (setf targets (remove-if-not (lambda (u) (and (member (unit-owner u) scopes)
                                                                (not (unit-finished-p u))))
                                               (reverse (slot-value sup 'units)))))
                t))
      (return-from close-scope nil))
    (%stop-wave sup targets grace force-grace)))

(defmacro with-scope ((var supervisor kind name &rest keys) &body body)
  "Run BODY with VAR and *CURRENT-SCOPE* bound to a new scope; close it on any exit
(normal return, non-local exit, an interrupt that unwinds)."
  `(let* ((,var (make-scope ,supervisor ,kind ,name ,@keys))
          (*current-scope* ,var))
     (unwind-protect (progn ,@body)
       (close-scope ,var))))

;;; ------------------------------------------------------------------
;;; Units

(defun start-unit (supervisor kind &key command function owner labels cwd env executor
                                        restart max-restarts backoff interacting
                                        (log t) log-segment-size log-segments)
  "Register and launch a unit. → the UNIT (state :RUNNING), or NIL when the ABORT
restart was taken.
KIND: a keyword (open set: :SHELL-JOB :LSP :RPC :IMAGE :PTY :SUBAGENT …).
COMMAND (argv list) → the process executor; FUNCTION (of the unit) → the thread
executor; exactly one of them unless EXECUTOR is given.
OWNER: a scope of SUPERVISOR (default: *CURRENT-SCOPE* when it belongs to
SUPERVISOR, else the root). LABELS: plist keyword → value. CWD, ENV: as
PROCESS-PROTOCOL:LAUNCH :DIRECTORY / :ENV.
RESTART :NEVER | :ON-FAILURE (default KIND-DEFAULT-RESTART), MAX-RESTARTS
(default +DEFAULT-MAX-RESTARTS+), BACKOFF (initial factor max), default
+DEFAULT-BACKOFF+. INTERACTING: the initial UNIT-INTERACTING-P.
H8b: LOG NIL keeps this unit's output in memory only; LOG-SEGMENT-SIZE and
LOG-SEGMENTS override the supervisor's (UNIT-LOG-SETTINGS has the effective ones).
Caps (SUPERVISOR-KIND-CAPS, SUPERVISOR-OWNER-CAP) count retained units, any state.
At a cap the least recently used finished unit is evicted silently; with none,
UNIT-LIMIT-REACHED is signalled (ERROR) with restarts EVICT-LRU (offered when a
live non-interacting victim exists: stop it, evict it, try again) and ABORT
(return NIL). A refused or failed start consumes no id.
Launch failure → UNIT-START-ERROR, nothing registered. Closed owner →
SCOPE-CLOSED. Shut-down supervisor → SUPERVISOR-CLOSED."
  (check-type kind keyword)
  (let ((sup supervisor))
    (when (supervisor-closed-p sup)
      (error 'supervisor-closed :supervisor sup))
    (let* ((exec (cond (executor executor)
                       ((and command (not function)) (slot-value sup 'process-executor))
                       ((and function (not command)) (slot-value sup 'thread-executor))
                       (t (error 'unit-start-error
                                 :supervisor sup :kind kind :command command
                                 :message (if command
                                              "give :command or :function, not both"
                                              "give :command or :function (or :executor)")))))
           (owner (%resolve-owner sup owner)))
      ;; Caps: evict silently, or ask (EVICT-LRU / ABORT).
      (loop
        (multiple-value-bind (verdict limit victim cap-owner evicted)
            (%with-lock (sup) (%admit sup kind owner))
          (mapc #'%after-evict evicted)
          (when (eq verdict :ok) (return))
          (restart-case (error 'unit-limit-reached :supervisor sup :kind kind :owner cap-owner
                                                   :limit limit :victim victim)
            (evict-lru ()
              :report "Stop the least recently used live unit, evict it, try again."
              :test (lambda (c) (declare (ignore c)) victim)
              (stop-unit victim)
              (let ((gone (%with-lock (sup)
                            (when (and (gethash (unit-id victim) (slot-value sup 'by-id))
                                       (unit-finished-p victim))
                              (%evict-locked sup victim)
                              t))))
                (when gone (%after-evict victim))))
            (abort ()
              :report "Do not start the unit (START-UNIT returns NIL)."
              (return-from start-unit nil)))))
      ;; Reserve the id; roll it back when the launch fails.
      (let* ((problem nil)
             (n (%with-lock (sup)
                  (cond ((supervisor-closed-p sup) (setf problem :supervisor) nil)
                        ((scope-closed-p owner) (setf problem :scope) nil)
                        (t (incf (gethash kind (slot-value sup 'kind-counters) 0)))))))
        (case problem
          (:supervisor (error 'supervisor-closed :supervisor sup))
          (:scope (error 'scope-closed :supervisor sup :scope owner)))
        (let ((unit (make-instance 'unit :id (format nil "~(~A~)-~D" kind n) :kind kind
                                         :owner owner :labels labels :command command
                                         :function function :cwd cwd :env env
                                         :restart-policy (or restart (kind-default-restart kind))
                                         :max-restarts (or max-restarts +default-max-restarts+)
                                         :backoff (or backoff +default-backoff+)
                                         :interacting interacting
                                         :executor exec :supervisor sup
                                         :log-settings
                                         (list :enabled (and log (supervisor-log-p sup)
                                                             (supervisor-state-directory sup) t)
                                               :segment-size (or log-segment-size
                                                                 (supervisor-log-segment-size sup))
                                               :segments (or log-segments
                                                             (supervisor-log-segments sup))))))
          (flet ((roll-back ()
                   (%with-lock (sup)
                     (when (eql n (gethash kind (slot-value sup 'kind-counters)))
                       (decf (gethash kind (slot-value sup 'kind-counters)))))))
            (multiple-value-bind (handle pid pgid os-start)
                (handler-case
                    (progn
                      (setf (slot-value unit 'output-sink)
                            (funcall (supervisor-output-sink-factory sup) unit))
                      (%launch unit))
                  (unit-start-error (e) (roll-back) (error e))
                  (error (e)
                    (roll-back)
                    (error 'unit-start-error :supervisor sup :kind kind :command command
                                             :cause e)))
              (let ((refused nil) (gen nil))
                (%with-lock (sup)
                  (cond ((supervisor-closed-p sup) (setf refused :supervisor))
                        ((scope-closed-p owner) (setf refused :scope))
                        (t
                         (push unit (slot-value sup 'units))
                         (setf (gethash (unit-id unit) (slot-value sup 'by-id)) unit)
                         (%touch sup unit)
                         (%publish sup unit :state nil :starting)
                         (%set-launched sup unit handle pid pgid os-start)
                         (setf gen (slot-value unit 'gen))
                         (%broadcast sup))))
                (when refused
                  ;; Closed while launching: the launch must not outlive the refusal.
                  (setf (slot-value unit 'stop-requested-p) t
                        (slot-value unit 'handle) handle)
                  (%executor-started exec unit handle)
                  (%send-stop unit t)
                  (roll-back)
                  (if (eq refused :supervisor)
                      (error 'supervisor-closed :supervisor sup)
                      (error 'scope-closed :supervisor sup :scope owner)))
                (%executor-started exec unit handle)
                (unwind-protect (write-unit-file unit)
                  (%spawn-waiter unit gen handle))
                unit))))))))

(defun stop-unit (unit &key grace force-grace)
  "Stop UNIT and wait for it. → its final state (:KILLED; a unit already finished
keeps its state). Graceful stop (UNIT-STOP-REQUESTED-P becomes T), wait GRACE
(default SUPERVISOR-GRACE) → if still running, SIGNAL STOP-TIMEOUT (phase
:GRACEFUL) with restarts FORCE and KEEP-WAITING (wait another GRACE, then signal
again); unhandled → FORCE. Forced stop, wait FORCE-GRACE → if still running,
ERROR STOP-FAILED with restart ABANDON (mark :KILLED, detail :ABANDONED T, keep
the unit file for the next sweep). A pending restart is cancelled."
  (%stop-wave (unit-supervisor unit) (list unit) grace force-grace)
  (unit-state unit))

(defun stop-units (supervisor &key kind owner (recursive t) labels state grace force-grace)
  "STOP-UNIT on every live unit FIND-UNITS selects, in one wave: graceful stop to
all, one GRACE, STOP-TIMEOUT per unit still running, force, one FORCE-GRACE.
→ the units it stopped."
  (%stop-wave supervisor
              (remove-if (lambda (u) (unit-finished-p u))
                         (find-units supervisor :kind kind :owner owner :recursive recursive
                                                :labels labels :state state))
              grace force-grace))

(defun wait-unit (unit &key timeout)
  "Block until UNIT is finished (final state, no restart pending) or TIMEOUT seconds.
→ (values state exit-code), or NIL on timeout."
  (let* ((sup (unit-supervisor unit))
         (deadline (and timeout (+ (%now) timeout)))
         (done nil) (state nil) (code nil))
    (%with-lock (sup)
      (loop
        (when (unit-finished-p unit)
          (setf done t state (unit-state unit) code (unit-exit-code unit))
          (return))
        (let ((left (and deadline (- deadline (%now)))))
          (when (and left (<= left 0)) (return))
          (%cv-wait sup left))))
    (if done (values state code) nil)))

(defun touch-unit (unit)
  "Mark UNIT used now (LRU). SEND-UNIT-INPUT and UNIT-OUTPUT touch too. → UNIT."
  (let ((sup (unit-supervisor unit)))
    (%with-lock (sup) (%touch sup unit)))
  unit)

(defun send-unit-input (unit text)
  "Write TEXT to UNIT's stdin and flush. Finished → UNIT-NOT-RUNNING; the executor
cannot → EXECUTOR-UNSUPPORTED. → T."
  (check-type text string)
  (let* ((sup (unit-supervisor unit))
         (handle (%with-lock (sup)
                   (%touch sup unit)
                   (and (eq :running (unit-state unit)) (slot-value unit 'handle)))))
    (unless handle
      (error 'unit-not-running :supervisor sup :unit unit))
    (executor-send-input (unit-executor unit) unit handle text)
    t))

(defun unit-output (unit)
  "SINK-TEXT of UNIT's output sink; touches UNIT."
  (touch-unit unit)
  (let ((sink (unit-output-sink unit)))
    (if sink (sink-text sink) "")))

(defmacro with-unit-interaction ((unit) &body body)
  "BODY with UNIT-INTERACTING-P true (never evicted meanwhile); restored after."
  (let ((u (gensym "UNIT")) (old (gensym "OLD")))
    `(let* ((,u ,unit) (,old (unit-interacting-p ,u)))
       (setf (unit-interacting-p ,u) t)
       (unwind-protect (progn ,@body)
         (setf (unit-interacting-p ,u) ,old)))))

;;; ------------------------------------------------------------------
;;; Queries

(defun find-unit (supervisor id)
  "The unit with ID (a string), or NIL."
  (%with-lock (supervisor) (values (gethash id (slot-value supervisor 'by-id)))))

(defun find-units (supervisor &key kind owner (recursive t) labels state)
  "Registry units matching every given filter, in start order. KIND, STATE: a
keyword or a list of them. OWNER: a scope (with its descendants unless RECURSIVE
is NIL). LABELS: a plist; each pair must be in UNIT-LABELS (EQUAL values)."
  (remove-if-not (lambda (u)
                   (and (or (null kind) (%matches-p (unit-kind u) kind))
                        (or (null state) (%matches-p (unit-state u) state))
                        (or (null owner) (%owned-by-p u owner recursive))
                        (or (null labels) (%labels-match-p u labels))))
                 (supervisor-units supervisor)))

;;; ------------------------------------------------------------------
;;; H8b: the supervisor's log accounting

(defun supervisor-log-usage (supervisor)
  "Bytes of segment and index files of every unit log this supervisor wrote (units
evicted from the registry included; segments a Windows reader still holds open
count until they are gone). The total cap (SUPERVISOR-LOG-TOTAL-CAP) evicts, after
each rotation and each finished unit, while this is over it: first the segments of
the finished unit that finished earliest (oldest segment first; its newest becomes
a stub), then the oldest segment of a live unit that is not that unit's newest."
  (declare (ignore supervisor))
  (%h8b-pending "supervisor-log-usage"))
