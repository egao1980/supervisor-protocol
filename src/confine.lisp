(in-package #:supervisor-protocol)

;;; R4d-0 — confined process units (brief: cl-stack
;;; docs/capabilities/confinement.md § R4d-0b). A COMPUTE-EXECUTOR is a
;;; PROCESS-EXECUTOR whose launch goes through a compute-protocol confining
;;; backend: STACK-COMPUTE:CONFINE-COMMAND turns the unit's command, cwd and
;;; env into a confined launch (argv, complete environment, directory), the
;;; process executor runs it with :ENV-MODE :REPLACE and :PROCESS-GROUP T,
;;; and the launch is released (private temp directory, backend cleanups)
;;; after the tree ended. Stop, wait, input, pumps and logs are the process
;;; executor's, unchanged. Plan-mode bash and the area's Lisp image become
;;; confined units this way (R4d-4).
;;;
;;; Refusals are UNIT-START-ERRORs. START-UNIT's routing (%CONFINED-EXECUTOR)
;;; refuses a wrong call, then checks the confinement against the backend
;;; before caps, ids or sinks exist; every launch (restarts included) is
;;; checked again by CONFINE-COMMAND. A compute condition becomes the cause,
;;; signalled from a HANDLER-BIND so CHECK-CONFINEMENT's USE-VALUE restart
;;; stays reachable: (use-value other (unit-start-error-cause e)).

(defclass compute-executor (process-executor)
  ((compute-backend :initarg :compute-backend :reader compute-executor-compute-backend
                    :documentation "A STACK-COMPUTE:CONFINING-BACKEND.")
   (confinement :initarg :confinement :initform nil :reader compute-executor-confinement
                :documentation "The confinement for units started without one, or NIL."))
  (:documentation "Process units launched confined: STACK-COMPUTE:CONFINE-COMMAND of
UNIT-COMMAND with :DIRECTORY UNIT-CWD and :ENV UNIT-ENV (added after the scrub),
under UNIT-CONFINEMENT; then PROCESS-PROTOCOL:LAUNCH of the launch's argv with
exactly its environment (:ENV-MODE :REPLACE), its directory and :PROCESS-GROUP T,
octet pipes, pumps, stop / wait / input exactly as PROCESS-EXECUTOR. Each launch
\(restarts included) is a new confined launch; EXECUTOR-RELEASE releases it after the
tree is gone. A refused confinement → UNIT-START-ERROR (cause: the compute condition),
nothing created on disk."))

(defun make-compute-executor (&key compute-backend confinement backend)
  "A COMPUTE-EXECUTOR on COMPUTE-BACKEND (a confining backend, required). CONFINEMENT:
the default for units started without one. BACKEND: the process-protocol backend (NIL:
PROCESS-PROTOCOL:*PROCESS-BACKEND*)."
  (unless (typep compute-backend 'sc:confining-backend)
    (error 'supervisor-error
           :message (format nil "make-compute-executor needs a confining backend, got ~s"
                            compute-backend)))
  (when confinement
    (check-type confinement sc:confinement))
  (make-instance 'compute-executor :compute-backend compute-backend
                                   :confinement confinement :backend backend))

(defmethod executor-supports-p ((executor compute-executor) capability)
  (if (eq capability :confined)
      t
      (call-next-method)))

(defun unit-confined-p (unit)
  "True when UNIT runs (or ran) under a confinement."
  (and (unit-confinement unit) t))

(defun %call-with-compute-errors-as-start-errors (supervisor kind command thunk)
  "Call THUNK; a STACK-COMPUTE:COMPUTE-ERROR it signals → UNIT-START-ERROR with it as
cause, signalled while the compute condition's restarts are still in place."
  (handler-bind ((sc:compute-error
                   (lambda (c)
                     (error 'unit-start-error :supervisor supervisor :kind kind
                                              :command command :cause c))))
    (funcall thunk)))

(defun %confined-executor (supervisor kind command function executor confinement)
  "START-UNIT's routing for confined units → (values executor confinement): EXECUTOR
when it is a COMPUTE-EXECUTOR, else SUPERVISOR-COMPUTE-EXECUTOR; CONFINEMENT, else the
executor's default. FUNCTION given (or no COMMAND), another kind of EXECUTOR, no
compute executor, no confinement, or CONFINEMENT not a STACK-COMPUTE:CONFINEMENT →
UNIT-START-ERROR (KIND, COMMAND, a message). Then STACK-COMPUTE:CHECK-CONFINEMENT on
the executor's backend: a refusal → UNIT-START-ERROR whose cause is the compute
condition (its USE-VALUE restart stays available); the confinement returned is the
checked one."
  (flet ((refuse (format-control &rest args)
           (error 'unit-start-error :supervisor supervisor :kind kind :command command
                                    :message (apply #'format nil format-control args))))
    (when function
      (refuse "confined units run a :command, not a :function"))
    (unless command
      (refuse "confined units run a :command"))
    (when (and executor (not (typep executor 'compute-executor)))
      (refuse "a confined unit needs a compute executor, not ~s" executor))
    (let ((exec (or executor (supervisor-compute-executor supervisor))))
      (unless exec
        (refuse "no compute executor (make-supervisor :compute-executor)"))
      (let ((conf (or confinement (compute-executor-confinement exec))))
        (unless conf
          (refuse "no confinement (start-unit :confinement, or the executor's default)"))
        (unless (typep conf 'sc:confinement)
          (refuse "~s is not a confinement" conf))
        (values exec
                (%call-with-compute-errors-as-start-errors
                 supervisor kind command
                 (lambda ()
                   (sc:check-confinement (compute-executor-compute-backend exec) conf))))))))

(defclass compute-unit-handle (process-unit-handle)
  ((launch :initarg :launch :reader %ch-launch
           :documentation "The STACK-COMPUTE:CONFINED-LAUNCH this process runs."))
  (:documentation "A process handle that owns one confined launch."))

(defmethod executor-launch ((executor compute-executor) unit)
  "CONFINE-COMMAND of UNIT-COMMAND (:DIRECTORY UNIT-CWD, :ENV UNIT-ENV) under
UNIT-CONFINEMENT (a compute condition → UNIT-START-ERROR, its cause), then the process
executor's launch of the launch's argv with exactly its environment (:ENV-MODE
:REPLACE) in its directory. A failure after CONFINE-COMMAND releases the launch."
  (let* ((command (unit-command unit))
         (launch (%call-with-compute-errors-as-start-errors
                  (unit-supervisor unit) (unit-kind unit) command
                  (lambda ()
                    (sc:confine-command (compute-executor-compute-backend executor)
                                        (unit-confinement unit) command
                                        :directory (unit-cwd unit) :env (unit-env unit)))))
         (process nil)
         (handle nil))
    (unwind-protect
         (progn
           (setf process (%launch-process executor unit (sc:confined-launch-argv launch)
                                          :env (sc:confined-launch-env launch)
                                          :env-mode :replace
                                          :directory (sc:confined-launch-directory launch)))
           (setf handle (%start-process-handle unit process 'compute-unit-handle
                                               :launch launch)))
      (unless handle
        ;; The tree goes before its private directory does.
        (when process (ignore-errors (%kill-process-tree process t)))
        (sc:release-confined-launch launch)))
    handle))

(defmethod executor-release ((executor compute-executor) unit handle)
  "The process executor's release (rest of the tree killed, pipes closed, pumps
joined), then STACK-COMPUTE:RELEASE-CONFINED-LAUNCH. Idempotent."
  (declare (ignorable unit))
  (call-next-method)
  (when (typep handle 'compute-unit-handle)
    (sc:release-confined-launch (%ch-launch handle)))
  nil)

(defun unit-confined-launch (unit)
  "The STACK-COMPUTE:CONFINED-LAUNCH of UNIT's current launch, or NIL (not confined,
or not launched). Between the end of a launch and its restart: the last launch
\(already released)."
  (let ((handle (slot-value unit 'handle)))
    (and (typep handle 'compute-unit-handle)
         (%ch-launch handle))))

(defun %unit-file-confinement (unit)
  "Extra unit-file keys for a confined UNIT: (:confinement <STACK-COMPUTE:CONFINEMENT-SUMMARY
of the launch's resolved confinement> :compute-backend \"<backend class name, downcased>\").
+UNIT-FILE-FORMAT+ stays 1; READ-UNIT-FILE reads them back."
  (let* ((launch (unit-confined-launch unit))
         (exec (unit-executor unit))
         (backend (cond (launch (sc:confined-launch-backend launch))
                        ((typep exec 'compute-executor)
                         (compute-executor-compute-backend exec)))))
    (append (list :confinement
                  (sc:confinement-summary (if launch
                                              (sc:confined-launch-confinement launch)
                                              (unit-confinement unit))))
            (and backend
                 (list :compute-backend
                       (string-downcase (symbol-name (class-name (class-of backend)))))))))
