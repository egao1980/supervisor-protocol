(in-package #:supervisor-protocol)

;;; Executors run a unit. The supervisor owns state, events, caps, restarts
;;; and unit files; an executor only launches, stops, waits and feeds input.
;;; H8a: PROCESS-EXECUTOR (process-protocol) and THREAD-EXECUTOR (bt2).
;;; H8a-1 adds pty, tmux and compute-backend executors behind the same GFs.
;;;
;;; The supervisor calls EXECUTOR-WAIT with no timeout on a thread of its own
;;; per launch, and publishes the unit's final state from there, so a state
;;; never stays :RUNNING after the work ended (no caller needs to poll).

(defclass executor () ())

(defclass process-executor (executor)
  ((backend :initarg :backend :initform nil :reader process-executor-backend
            :documentation "A PROCESS-PROTOCOL backend, or NIL for PROCESS-PROTOCOL:*PROCESS-BACKEND*."))
  (:documentation "UNIT-COMMAND through PROCESS-PROTOCOL:LAUNCH :PROCESS-GROUP T, stdin,
stdout and stderr as UTF-8 character pipes, :DIRECTORY UNIT-CWD, :ENV UNIT-ENV.
Graceful stop: KILL :TREE T (Unix SIGTERM to the group; Windows the job object,
always forced). Force: KILL :TREE T :FORCE T. On exit (by itself or not) the
rest of the tree is killed too (EXECUTOR-RELEASE), so nothing the unit started
outlives it."))

(defclass thread-executor (executor) ()
  (:documentation "UNIT-FUNCTION, called with the unit on a new bt2 thread, with
*CURRENT-UNIT* bound to the unit and *CURRENT-SCOPE* to its owner. Normal
return → :EXITED (value in UNIT-RESULT); an unhandled ERROR → :FAILED (in
UNIT-CONDITION; the thread does not enter the debugger). Graceful stop sets
UNIT-STOP-REQUESTED-P (cooperative: the body polls it). Force interrupts the
thread (bt2:interrupt-thread) and unwinds it to the unit's top, running its
UNWIND-PROTECT cleanups."))

(defun make-process-executor (&key backend)
  (make-instance 'process-executor :backend backend))

(defun make-thread-executor ()
  (make-instance 'thread-executor))

(defgeneric executor-launch (executor unit)
  (:documentation "Start UNIT (read its command / function / cwd / env; write output to
UNIT-OUTPUT-SINK). → a handle. Cannot start → UNIT-START-ERROR."))

(defgeneric executor-pid (executor handle)
  (:documentation "The OS pid of the direct child, or NIL.")
  (:method ((executor executor) handle) (declare (ignore handle)) nil))

(defgeneric executor-pgid (executor handle)
  (:documentation "The OS process group id (Unix, tree-capable launch), or NIL.")
  (:method ((executor executor) handle) (declare (ignore handle)) nil))

(defgeneric executor-stop (executor unit handle &key force)
  (:documentation "Ask HANDLE to stop: gracefully, or with FORCE so it cannot refuse;
tree-wide where the executor supports :TREE. Returns at once. → T."))

(defgeneric executor-wait (executor unit handle &key timeout)
  (:documentation "Block until HANDLE has ended or TIMEOUT seconds passed (NIL: no limit).
→ (values ended-p exit-code condition result): EXIT-CODE an integer for a
process, NIL for a thread; CONDITION the error that ended a thread; RESULT a
thread's return value."))

(defgeneric executor-alive-p (executor unit handle))

(defgeneric executor-send-input (executor unit handle text)
  (:documentation "Write TEXT to the unit's stdin and flush. → T. Not supported →
EXECUTOR-UNSUPPORTED."))

(defgeneric executor-release (executor unit handle)
  (:documentation "After the end: kill what is left of the tree, close the pipes, join the
pump threads, close nothing that belongs to the sink. → NIL.")
  (:method ((executor executor) unit handle) (declare (ignore unit handle)) nil))

(defgeneric executor-supports-p (executor capability)
  (:documentation "Capabilities: :PID (EXECUTOR-PID answers), :TREE (stops reach
descendants), :INPUT (EXECUTOR-SEND-INPUT works), :GRACEFUL-STOP (a graceful stop
differs from a forced one). Unknown → NIL.")
  (:method ((executor executor) capability) (declare (ignore capability)) nil)
  (:method ((executor process-executor) capability)
    (case capability
      ((:pid :tree :input) t)
      (:graceful-stop (not (uiop:os-windows-p)))
      (t nil)))
  (:method ((executor thread-executor) capability)
    (eq capability :graceful-stop)))

(defmethod executor-send-input ((executor executor) unit handle text)
  (declare (ignore unit handle text))
  (error 'executor-unsupported :executor executor :operation :input))

;;; process-executor

(defmethod executor-launch ((executor process-executor) unit)
  (declare (ignore unit))
  (%h8a-pending "executor-launch process"))

(defmethod executor-pid ((executor process-executor) handle)
  (declare (ignore handle))
  (%h8a-pending "executor-pid process"))

(defmethod executor-pgid ((executor process-executor) handle)
  (declare (ignore handle))
  (%h8a-pending "executor-pgid process"))

(defmethod executor-stop ((executor process-executor) unit handle &key force)
  (declare (ignore unit handle force))
  (%h8a-pending "executor-stop process"))

(defmethod executor-wait ((executor process-executor) unit handle &key timeout)
  (declare (ignore unit handle timeout))
  (%h8a-pending "executor-wait process"))

(defmethod executor-alive-p ((executor process-executor) unit handle)
  (declare (ignore unit handle))
  (%h8a-pending "executor-alive-p process"))

(defmethod executor-send-input ((executor process-executor) unit handle text)
  (declare (ignore unit handle text))
  (%h8a-pending "executor-send-input process"))

(defmethod executor-release ((executor process-executor) unit handle)
  (declare (ignore unit handle))
  (%h8a-pending "executor-release process"))

;;; thread-executor

(defmethod executor-launch ((executor thread-executor) unit)
  (declare (ignore unit))
  (%h8a-pending "executor-launch thread"))

(defmethod executor-stop ((executor thread-executor) unit handle &key force)
  (declare (ignore unit handle force))
  (%h8a-pending "executor-stop thread"))

(defmethod executor-wait ((executor thread-executor) unit handle &key timeout)
  (declare (ignore unit handle timeout))
  (%h8a-pending "executor-wait thread"))

(defmethod executor-alive-p ((executor thread-executor) unit handle)
  (declare (ignore unit handle))
  (%h8a-pending "executor-alive-p thread"))
