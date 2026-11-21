(in-package #:supervisor-protocol)

;;; OS processes: the fallback path for crash recovery (the orphan sweep) and
;;; the pid-reuse guard. Portable, no native code: every query and kill runs an
;;; OS command through PROCESS-PROTOCOL:RUN.
;;;
;;;   Unix  (PS-INSPECTOR):  ps -A -o pid= -o ppid= -o pgid= -o lstart=
;;;                          with LC_ALL=C TZ=UTC; kills with kill(1).
;;;   Windows (CIM-INSPECTOR): powershell.exe -NoProfile -NonInteractive -Command
;;;                          Get-CimInstance Win32_Process (ProcessId,
;;;                          ParentProcessId, CreationDate as UTC
;;;                          "yyyyMMddHHmmss.ffffff"); kills with taskkill /F /PID.
;;;
;;; START is an opaque string compared with STRING=; on Windows it also sorts
;;; by time (STRING<). On Unix, whitespace runs in lstart collapse to one space.

(defun current-pid ()
  "The OS pid of this image."
  #+(and sbcl win32)
  (sb-alien:alien-funcall
   (sb-alien:extern-alien "GetCurrentProcessId" (function sb-alien:unsigned-long)))
  #+(and sbcl (not win32))
  (sb-unix:unix-getpid)
  #-sbcl
  (%h8a-pending "current-pid on this implementation"))

(defclass os-process ()
  ((pid :initarg :pid :reader os-process-pid)
   (ppid :initarg :ppid :reader os-process-ppid)
   (pgid :initarg :pgid :initform nil :reader os-process-pgid :documentation "NIL on Windows.")
   (start :initarg :start :reader os-process-start :documentation "Opaque OS start time string.")))

(defmethod print-object ((p os-process) stream)
  (print-unreadable-object (p stream :type t)
    (format stream "~D ppid ~D~@[ pgid ~D~] ~S"
            (os-process-pid p) (os-process-ppid p) (os-process-pgid p) (os-process-start p))))

(defclass os-inspector () ())
(defclass ps-inspector (os-inspector) ()
  (:documentation "Unix (macOS, Linux, BSD): ps(1) and kill(1)."))
(defclass cim-inspector (os-inspector) ()
  (:documentation "Windows: PowerShell Get-CimInstance Win32_Process and taskkill."))

(defun make-ps-inspector () (make-instance 'ps-inspector))
(defun make-cim-inspector () (make-instance 'cim-inspector))

(defun default-os-inspector ()
  "CIM-INSPECTOR on Windows, PS-INSPECTOR elsewhere."
  (if (uiop:os-windows-p) (make-cim-inspector) (make-ps-inspector)))

(defgeneric inspector-list-processes (inspector)
  (:documentation "Every process the OS shows this user, as OS-PROCESSes (one snapshot)."))

(defmethod inspector-list-processes ((inspector ps-inspector))
  (%h8a-pending "inspector-list-processes ps"))

(defmethod inspector-list-processes ((inspector cim-inspector))
  (%h8a-pending "inspector-list-processes cim"))

(defun list-os-processes (&key (inspector (default-os-inspector)))
  (inspector-list-processes inspector))

(defun os-process-start-time (pid &key (inspector (default-os-inspector)))
  "The START string of PID (as LIST-OS-PROCESSES reports it), or NIL when no such process."
  (declare (ignore pid inspector))
  (%h8a-pending "os-process-start-time"))

(defun os-process-alive-p (pid &key start-time (inspector (default-os-inspector)))
  "True when PID exists (and is not a zombie) and, given START-TIME, started then."
  (declare (ignore pid start-time inspector))
  (%h8a-pending "os-process-alive-p"))

(defun kill-os-processes (pids &key force (inspector (default-os-inspector)))
  "Signal each of PIDS: TERM, or KILL with FORCE (Windows: always taskkill /F, one
/PID each, no /T). Pids already gone are not an error. → NIL."
  (declare (ignore pids force inspector))
  (%h8a-pending "kill-os-processes"))

(defun kill-os-process-group (pgid &key force)
  "Unix: kill -TERM (-KILL with FORCE) -- -PGID. Gone is not an error. Windows:
SUPERVISOR-ERROR (no process groups). → NIL."
  (declare (ignore pgid force))
  (%h8a-pending "kill-os-process-group"))
