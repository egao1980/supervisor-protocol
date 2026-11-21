(in-package #:supervisor-protocol)

;;; CLHS 9. SUPERVISOR-CONDITION is the root; SUPERVISOR-ERROR the errors.
;;; STOP-TIMEOUT is not an error: it is SIGNALled and, unhandled, the stop
;;; escalates (FORCE). Restart names: EVICT-LRU, CL:ABORT, FORCE, KEEP-WAITING,
;;; ABANDON; H8b adds RETRY, DROP-TO-MEMORY-ONLY, ABORT-UNIT.

(define-condition supervisor-condition (condition)
  ((supervisor :initarg :supervisor :initform nil :reader supervisor-condition-supervisor)))

(define-condition supervisor-error (supervisor-condition error)
  ((message :initarg :message :initform nil :reader supervisor-error-message))
  (:report (lambda (c s)
             (format s "Supervisor error~@[: ~A~]" (supervisor-error-message c)))))

(define-condition supervisor-closed (supervisor-error)
  ()
  (:report (lambda (c s)
             (format s "Supervisor ~A is shut down"
                     (let ((sup (supervisor-condition-supervisor c)))
                       (if sup (supervisor-id sup) "?"))))))

(define-condition scope-closed (supervisor-error)
  ((scope :initarg :scope :reader scope-closed-scope))
  (:report (lambda (c s)
             (format s "Scope ~{~A~^ › ~} is closed" (scope-path (scope-closed-scope c))))))

(define-condition unit-error (supervisor-error)
  ((unit :initarg :unit :reader unit-error-unit))
  (:report (lambda (c s)
             (format s "Unit ~A: ~A" (unit-id (unit-error-unit c))
                     (or (supervisor-error-message c) "error")))))

(define-condition unit-start-error (supervisor-error)
  ((kind :initarg :kind :reader unit-start-error-kind)
   (command :initarg :command :initform nil :reader unit-start-error-command)
   (cause :initarg :cause :initform nil :reader unit-start-error-cause))
  (:report (lambda (c s)
             (format s "Cannot start ~S unit~@[ ~S~]~@[: ~A~]"
                     (unit-start-error-kind c) (unit-start-error-command c)
                     (or (unit-start-error-cause c) (supervisor-error-message c))))))

(define-condition unit-limit-reached (supervisor-error)
  ((kind :initarg :kind :reader unit-limit-reached-kind)
   (owner :initarg :owner :initform nil :reader unit-limit-reached-owner)
   (limit :initarg :limit :reader unit-limit-reached-limit)
   (victim :initarg :victim :initform nil :reader unit-limit-reached-victim))
  (:report (lambda (c s)
             (let ((owner (unit-limit-reached-owner c))
                   (victim (unit-limit-reached-victim c)))
               (format s "Unit limit ~D reached for ~A~@[; least recently used live unit: ~A~]"
                       (unit-limit-reached-limit c)
                       (if owner
                           (format nil "scope ~{~A~^ › ~}" (scope-path owner))
                           (format nil "kind ~S" (unit-limit-reached-kind c)))
                       (and victim (unit-id victim)))))))

(define-condition stop-timeout (supervisor-condition)
  ((unit :initarg :unit :reader stop-timeout-unit)
   (phase :initarg :phase :reader stop-timeout-phase)   ; :graceful
   (waited :initarg :waited :reader stop-timeout-waited)) ; seconds
  (:report (lambda (c s)
             (format s "Unit ~A still running ~,1F s after a ~(~A~) stop"
                     (unit-id (stop-timeout-unit c)) (stop-timeout-waited c)
                     (stop-timeout-phase c)))))

(define-condition stop-failed (unit-error)
  ()
  (:report (lambda (c s)
             (format s "Unit ~A survived a forced stop" (unit-id (unit-error-unit c))))))

(define-condition unit-not-running (unit-error)
  ()
  (:report (lambda (c s)
             (let ((u (unit-error-unit c)))
               (format s "Unit ~A is not running (state ~S)" (unit-id u) (unit-state u))))))

(define-condition unit-input-closed (unit-error)
  ()
  (:documentation "H8c: SEND-UNIT-INPUT after CLOSE-UNIT-INPUT on the unit's current run.")
  (:report (lambda (c s)
             (format s "Unit ~A: its stdin was closed" (unit-id (unit-error-unit c))))))

(define-condition executor-unsupported (supervisor-error)
  ((executor :initarg :executor :reader executor-unsupported-executor)
   (operation :initarg :operation :reader executor-unsupported-operation))
  (:report (lambda (c s)
             (format s "~A does not support ~S"
                     (type-of (executor-unsupported-executor c))
                     (executor-unsupported-operation c)))))

(define-condition unit-file-unreadable (warning)
  ((path :initarg :path :reader unit-file-unreadable-path)
   (cause :initarg :cause :initform nil :reader unit-file-unreadable-cause))
  (:report (lambda (c s)
             (format s "Unreadable unit file ~A~@[: ~A~]"
                     (unit-file-unreadable-path c) (unit-file-unreadable-cause c)))))

;;; H8b. LOG-WRITE-ERROR is SIGNALled (not ERRORed) by a log sink on the thread
;;; that wrote (a pump thread, or a thread unit's own thread), inside
;;; RESTART-CASE with RETRY (reopen the segment and write the record again),
;;; DROP-TO-MEMORY-ONLY (no more files for this unit; the head + tail buffer
;;; keeps going) and, when the sink has a unit, ABORT-UNIT (request a forced
;;; stop without waiting; the unit ends :KILLED). The sink SIGNALs it, so the
;;; dynamic handlers of the writing thread see it first; when they decline, the
;;; sink calls the supervisor's LOG-ERROR-HANDLER (if any) with it, inside the
;;; same RESTART-CASE; when that returns too: DROP-TO-MEMORY-ONLY. Every
;;; outcome publishes one :LOG-ERROR event (sinks with a unit). The unit is
;;; never stopped unless ABORT-UNIT is chosen. Like STOP-TIMEOUT it is a
;;; signalled condition, not an ERROR: handlers on ERROR never see it.

(define-condition log-write-error (supervisor-condition)
  ((sink :initarg :sink :initform nil :reader log-write-error-sink)
   (unit :initarg :unit :initform nil :reader log-write-error-unit)
   (path :initarg :path :initform nil :reader log-write-error-path)
   (cause :initarg :cause :initform nil :reader log-write-error-cause))
  (:report (lambda (c s)
             (format s "Cannot write unit log~@[ ~A~]~@[ of ~A~]~@[: ~A~]"
                     (let ((p (log-write-error-path c))) (and p (uiop:native-namestring p)))
                     (let ((u (log-write-error-unit c))) (and u (unit-id u)))
                     (log-write-error-cause c)))))

(define-condition log-not-found (supervisor-error)
  ((path :initarg :path :initform nil :reader log-not-found-path))
  (:report (lambda (c s)
             (format s "No unit log~@[ at ~A~]~@[: ~A~]"
                     (let ((p (log-not-found-path c))) (and p (uiop:native-namestring p)))
                     (supervisor-error-message c)))))
