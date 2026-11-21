(in-package #:supervisor-protocol)

;;; ------------------------------------------------------------------
;;; Supervisor lifecycle

(defun make-supervisor (&key state-directory (sweep t) (exit-hook t) kind-caps owner-cap
                             (grace +default-grace+) (force-grace +default-force-grace+)
                             (event-capacity +default-event-capacity+)
                             (output-sink-factory 'default-output-sink)
                             (process-executor (make-process-executor))
                             (thread-executor (make-thread-executor)))
  "A new supervisor with its root scope (kind :SUPERVISOR). With STATE-DIRECTORY
(created if missing) process units get unit files, and with SWEEP the orphan
sweep runs first (SUPERVISOR-SWEEP-REPORT). EXIT-HOOK registers a function that
shuts the supervisor down when the image exits normally (SBCL
SB-EXT:*EXIT-HOOKS*; elsewhere UIOP:REGISTER-IMAGE-EXIT-HOOK); on exit an
unhandled STOP-TIMEOUT forces and STOP-FAILED abandons. KIND-CAPS: plist kind →
limit. OWNER-CAP: limit per directly owning scope."
  (declare (ignore state-directory sweep exit-hook kind-caps owner-cap grace force-grace
                   event-capacity output-sink-factory process-executor thread-executor))
  (%h8a-pending "make-supervisor"))

(defun live-supervisors ()
  "Supervisors in this image not yet shut down, oldest first."
  (%h8a-pending "live-supervisors"))

(defun shutdown-supervisor (supervisor &key grace force-grace)
  "Stop every live unit (one stop wave, see STOP-UNITS), delete the unit files,
stop event delivery (subscriptions become inactive), remove the exit hook. Later
START-UNIT / MAKE-SCOPE → SUPERVISOR-CLOSED; queries still answer. Idempotent. → NIL."
  (declare (ignore supervisor grace force-grace))
  (%h8a-pending "shutdown-supervisor"))

(defun supervisor-units (supervisor)
  "Every unit in the registry, in start order (evicted ones are gone)."
  (declare (ignore supervisor))
  (%h8a-pending "supervisor-units"))

;;; ------------------------------------------------------------------
;;; Scopes: session › agent › tool source (any depth, any kind keyword)

(defun make-scope (supervisor kind name &key (parent *current-scope*))
  "A child scope of PARENT (default *CURRENT-SCOPE* when it belongs to SUPERVISOR,
else the root). Closed parent → SCOPE-CLOSED; shut-down supervisor →
SUPERVISOR-CLOSED; a PARENT of another supervisor → SUPERVISOR-ERROR."
  (declare (ignore supervisor kind name parent))
  (%h8a-pending "make-scope"))

(defun scope-units (scope &key (recursive t))
  "Units owned by SCOPE (and, RECURSIVE, by its descendants), in start order."
  (declare (ignore scope recursive))
  (%h8a-pending "scope-units"))

(defun close-scope (scope &key grace force-grace)
  "Close SCOPE and its descendants and stop every live unit they own, in one stop
wave. Units stay queryable. → the units it stopped. Idempotent (→ NIL). The root
scope → SUPERVISOR-ERROR (use SHUTDOWN-SUPERVISOR)."
  (declare (ignore scope grace force-grace))
  (%h8a-pending "close-scope"))

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
                                        restart max-restarts backoff interacting)
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
Caps (SUPERVISOR-KIND-CAPS, SUPERVISOR-OWNER-CAP) count retained units, any state.
At a cap the least recently used finished unit is evicted silently; with none,
UNIT-LIMIT-REACHED is signalled (ERROR) with restarts EVICT-LRU (offered when a
live non-interacting victim exists: stop it, evict it, try again) and ABORT
(return NIL). A refused or failed start consumes no id.
Launch failure → UNIT-START-ERROR, nothing registered. Closed owner →
SCOPE-CLOSED. Shut-down supervisor → SUPERVISOR-CLOSED."
  (declare (ignore supervisor kind command function owner labels cwd env executor
                   restart max-restarts backoff interacting))
  (%h8a-pending "start-unit"))

(defun stop-unit (unit &key grace force-grace)
  "Stop UNIT and wait for it. → its final state (:KILLED; a unit already finished
keeps its state). Graceful stop (UNIT-STOP-REQUESTED-P becomes T), wait GRACE
(default SUPERVISOR-GRACE) → if still running, SIGNAL STOP-TIMEOUT (phase
:GRACEFUL) with restarts FORCE and KEEP-WAITING (wait another GRACE, then signal
again); unhandled → FORCE. Forced stop, wait FORCE-GRACE → if still running,
ERROR STOP-FAILED with restart ABANDON (mark :KILLED, detail :ABANDONED T, keep
the unit file for the next sweep). A pending restart is cancelled."
  (declare (ignore unit grace force-grace))
  (%h8a-pending "stop-unit"))

(defun stop-units (supervisor &key kind owner (recursive t) labels state grace force-grace)
  "STOP-UNIT on every live unit FIND-UNITS selects, in one wave: graceful stop to
all, one GRACE, STOP-TIMEOUT per unit still running, force, one FORCE-GRACE.
→ the units it stopped."
  (declare (ignore supervisor kind owner recursive labels state grace force-grace))
  (%h8a-pending "stop-units"))

(defun wait-unit (unit &key timeout)
  "Block until UNIT is finished (final state, no restart pending) or TIMEOUT seconds.
→ (values state exit-code), or NIL on timeout."
  (declare (ignore unit timeout))
  (%h8a-pending "wait-unit"))

(defun touch-unit (unit)
  "Mark UNIT used now (LRU). SEND-UNIT-INPUT and UNIT-OUTPUT touch too. → UNIT."
  (declare (ignore unit))
  (%h8a-pending "touch-unit"))

(defun send-unit-input (unit text)
  "Write TEXT to UNIT's stdin and flush. Finished → UNIT-NOT-RUNNING; the executor
cannot → EXECUTOR-UNSUPPORTED. → T."
  (declare (ignore unit text))
  (%h8a-pending "send-unit-input"))

(defun unit-output (unit)
  "SINK-TEXT of UNIT's output sink; touches UNIT."
  (declare (ignore unit))
  (%h8a-pending "unit-output"))

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
  (declare (ignore supervisor id))
  (%h8a-pending "find-unit"))

(defun find-units (supervisor &key kind owner (recursive t) labels state)
  "Registry units matching every given filter, in start order. KIND, STATE: a
keyword or a list of them. OWNER: a scope (with its descendants unless RECURSIVE
is NIL). LABELS: a plist; each pair must be in UNIT-LABELS (EQUAL values)."
  (declare (ignore supervisor kind owner recursive labels state))
  (%h8a-pending "find-units"))
