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
;;; Given: the class, its constructor and readers, EXECUTOR-SUPPORTS-P,
;;; UNIT-CONFINED-P. Stubbed (%R4D0-PENDING): %CONFINED-EXECUTOR (START-UNIT's
;;; routing), EXECUTOR-LAUNCH and EXECUTOR-RELEASE on the class,
;;; UNIT-CONFINED-LAUNCH, %UNIT-FILE-CONFINEMENT.

(defun %r4d0-pending (what)
  (error "R4d-0 pending: ~a" what))

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

(defun %confined-executor (supervisor kind command function executor confinement)
  "START-UNIT's routing for confined units → (values executor confinement): EXECUTOR
when it is a COMPUTE-EXECUTOR, else SUPERVISOR-COMPUTE-EXECUTOR; CONFINEMENT, else the
executor's default. FUNCTION given, another kind of EXECUTOR, no compute executor, no
confinement, or CONFINEMENT not a STACK-COMPUTE:CONFINEMENT → UNIT-START-ERROR (KIND,
COMMAND, a message)."
  (declare (ignore supervisor kind command function executor confinement))
  (%r4d0-pending '%confined-executor))

(defmethod executor-launch ((executor compute-executor) unit)
  (declare (ignore unit))
  (%r4d0-pending 'executor-launch/compute))

(defmethod executor-release ((executor compute-executor) unit handle)
  "The process executor's release (rest of the tree killed, pipes closed, pumps
joined), then STACK-COMPUTE:RELEASE-CONFINED-LAUNCH. Idempotent."
  (declare (ignore unit handle))
  (%r4d0-pending 'executor-release/compute))

(defun unit-confined-launch (unit)
  "The STACK-COMPUTE:CONFINED-LAUNCH of UNIT's current launch, or NIL (not confined,
or not launched)."
  (declare (ignore unit))
  (%r4d0-pending 'unit-confined-launch))

(defun %unit-file-confinement (unit)
  "Extra unit-file keys for a confined UNIT: (:confinement <STACK-COMPUTE:CONFINEMENT-SUMMARY
of the launch's resolved confinement> :compute-backend \"<backend class name, downcased>\").
+UNIT-FILE-FORMAT+ stays 1; READ-UNIT-FILE reads them back."
  (declare (ignore unit))
  (%r4d0-pending '%unit-file-confinement))
