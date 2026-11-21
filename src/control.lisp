(in-package #:supervisor-protocol)

;;; H8c control additions (0.3.0): a manual restart and closing a unit's stdin.
;;; Both are what the agent tool source (agent-tools unit_control, shell-tools
;;; foreground commands) needs on top of the 0.2.0 surface.

;;; RESTART-UNIT. The old run must be over before the unit is reused: the stop
;;; wave (as STOP-UNIT) ends it, then its waiter thread is joined, because the
;;; waiter closes the sink (the end record) and deletes the unit file after it
;;; published the final state. Then the sink is reopened (segment N+1 with a
;;; header), the unit is launched, and only then published: FINAL → :STARTING
;;; (:RESTART :MANUAL) → :RUNNING (or → :FAILED). Launching first, as START-UNIT
;;; does, means a concurrent SHUTDOWN-SUPERVISOR never waits on a :STARTING unit
;;; without a handle; RESTARTING-P keeps the unit from being evicted meanwhile and
;;; a second RESTART-UNIT out.

(defun %restart-refusal (sup unit)
  "Lock held: the condition RESTART-UNIT signals for UNIT now, or NIL."
  (cond ((supervisor-closed-p sup) (make-condition 'supervisor-closed :supervisor sup))
        ((not (eq unit (gethash (unit-id unit) (slot-value sup 'by-id))))
         (make-condition 'supervisor-error :supervisor sup
                                           :message (format nil "unit ~A was evicted" (unit-id unit))))
        ((slot-value unit 'restarting-p)
         (make-condition 'supervisor-error :supervisor sup
                                           :message (format nil "unit ~A is restarting" (unit-id unit))))
        ((eq unit *current-unit*)
         (make-condition 'supervisor-error :supervisor sup
                                           :message (format nil "unit ~A cannot restart itself"
                                                            (unit-id unit))))))

(defun restart-unit (unit &key grace force-grace)
  "Relaunch UNIT now: same unit, same id, a new run. A live unit is stopped first
(as STOP-UNIT with GRACE / FORCE-GRACE: STOP-TIMEOUT and STOP-FAILED behave as
there). A pending policy restart is cancelled; UNIT-RESTART-COUNT is not
incremented (it counts policy restarts only).
Events: :STATE FINAL → :STARTING with detail (:RESTART :MANUAL), then :STARTING →
:RUNNING. The unit file is rewritten. The unit's log continues: the new run's
records follow the previous ones in one line sequence (a new segment with a
header starts after an end record; LOG-CLOSED-P and LOG-FOLLOW's ENDED-P look at
the last end record only).
→ UNIT in :RUNNING. A unit no longer in its supervisor's registry (evicted) →
SUPERVISOR-ERROR; a shut-down supervisor → SUPERVISOR-CLOSED; a launch failure →
UNIT-START-ERROR with the unit left :FAILED (and its :STATE event)."
  (check-type unit unit)
  (let* ((sup (unit-supervisor unit))
         (exec (unit-executor unit))
         (refusal (%with-lock (sup)
                    (or (%restart-refusal sup unit)
                        (progn (setf (slot-value unit 'restarting-p) t) nil)))))
    (when refusal (error refusal))
    (unwind-protect
         (progn
           ;; 1. End the old run (live, or a policy restart pending).
           (%stop-wave sup (list unit) grace force-grace)
           (let ((waiter (%with-lock (sup)
                           (and (not (slot-value unit 'abandoned-p)) (slot-value unit 'waiter)))))
             (when (and waiter (not (eq waiter (bt2:current-thread))))
               (bt2:join-thread waiter)))
           ;; 2. The log continues.
           (let ((sink (unit-output-sink unit)))
             (when (typep sink 'log-sink) (%log-sink-reopen sink))
             (when (and sink (not (typep sink 'log-sink)) (typep sink 'head-tail-buffer))
               (bt2:with-lock-held ((slot-value sink 'lock))
                 (setf (slot-value sink 'closed-p) nil))))
           ;; 3. Launch, then publish.
           (multiple-value-bind (handle pid pgid os-start failure)
               (handler-case (%launch unit)
                 (error (e) (values nil nil nil nil e)))
             (let ((gen nil) (refused nil))
               (%with-lock (sup)
                 (setf refused (and (not failure) (%restart-refusal-after-launch sup)))
                 (let ((from (unit-state unit)))
                   (setf gen (incf (slot-value unit 'gen))
                         (slot-value unit 'abandoned-p) nil
                         (slot-value unit 'stop-requested-p) nil
                         (slot-value unit 'stop-signal) nil
                         (slot-value unit 'restart-pending) nil
                         (slot-value unit 'exit-code) nil
                         (slot-value unit 'end-condition) nil
                         (slot-value unit 'result) nil
                         (slot-value unit 'handle) nil
                         (slot-value unit 'pid) nil
                         (slot-value unit 'pgid) nil
                         (slot-value unit 'state) :starting)
                   (%touch sup unit)
                   (%publish sup unit :state from :starting '(:restart :manual))
                   (cond (failure
                          (setf (slot-value unit 'state) :failed
                                (slot-value unit 'end-condition) failure)
                          (%publish sup unit :state :starting :failed
                                    (list :exit-code nil :signal nil)))
                         (refused
                          (setf (slot-value unit 'state) :killed
                                (slot-value unit 'stop-requested-p) t
                                (slot-value unit 'handle) handle)
                          (%publish sup unit :state :starting :killed
                                    (list :exit-code nil :signal nil)))
                         (t (%set-launched sup unit handle pid pgid os-start))))
                 (%broadcast sup))
               (cond
                 (failure
                  (%after-finish unit)
                  (error (if (typep failure 'unit-start-error)
                             failure
                             (make-condition 'unit-start-error :supervisor sup :kind (unit-kind unit)
                                                               :command (unit-command unit)
                                                               :cause failure))))
                 (refused
                  ;; Shut down while launching: the launch must not outlive it.
                  (%executor-started exec unit handle)
                  (%send-stop unit t)
                  (handler-case (executor-wait exec unit handle :timeout 5) (error () nil))
                  (handler-case (executor-release exec unit handle) (error () nil))
                  (%after-finish unit)
                  (error refused))
                 (t
                  (%executor-started exec unit handle)
                  (unwind-protect (write-unit-file unit)
                    (%spawn-waiter unit gen handle))))))
           unit)
      (%with-lock (sup) (setf (slot-value unit 'restarting-p) nil)))))

(defun %restart-refusal-after-launch (sup)
  "Lock held: a shut-down supervisor refuses the new run."
  (and (supervisor-closed-p sup) (make-condition 'supervisor-closed :supervisor sup)))

(defun close-unit-input (unit)
  "Close UNIT's stdin (EXECUTOR-CLOSE-INPUT), so a program reading it sees end of
file. → T when this call closed it; NIL when it was already closed or the unit is
finished. Later SEND-UNIT-INPUT on this run → UNIT-INPUT-CLOSED. A restart opens a
new stdin. Thread units → EXECUTOR-UNSUPPORTED."
  (check-type unit unit)
  (let* ((sup (unit-supervisor unit))
         (handle (%with-lock (sup)
                   (%touch sup unit)
                   (and (unit-live-p unit) (slot-value unit 'handle)))))
    (and handle (executor-close-input (unit-executor unit) unit handle) t)))
