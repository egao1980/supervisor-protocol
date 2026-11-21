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
  (error 'supervisor-error :message "current-pid is not implemented on this Lisp"))

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

;;; Parsing

(defun %whitespace-p (c)
  (member c '(#\Space #\Tab #\Newline #\Return #\Page) :test #'char=))

(defun %split-words (line)
  (loop with words = '() and start = nil
        for i from 0 to (length line)
        for c = (if (< i (length line)) (char line i) #\Space)
        do (cond ((%whitespace-p c)
                  (when start (push (subseq line start i) words) (setf start nil)))
                 ((null start) (setf start i)))
        finally (return (nreverse words))))

(defun %lines (text)
  (with-input-from-string (s text)
    (loop for l = (read-line s nil nil) while l collect l)))

(defun %integer-word (word)
  (and word (every #'digit-char-p word) (plusp (length word))
       (parse-integer word)))

(defun %run-output (argv &rest keys)
  "Run ARGV through PROCESS-PROTOCOL:RUN; → its stdout as a string (exit status ignored)."
  (or (nth-value 1 (apply #'pp:run argv :output :string :error-output nil keys)) ""))

(defun %run-quietly (argv)
  "Run ARGV for its effect; a non-zero exit (e.g. a pid already gone) is not an error."
  (pp:run argv :output nil :error-output nil)
  nil)

(defmethod inspector-list-processes ((inspector ps-inspector))
  (loop for line in (%lines (%run-output '("ps" "-A" "-o" "pid=" "-o" "ppid=" "-o" "pgid=" "-o" "lstart=")
                                         :env '(("LC_ALL" . "C") ("TZ" . "UTC"))))
        for words = (%split-words line)
        for pid = (%integer-word (first words))
        for ppid = (%integer-word (second words))
        for pgid = (%integer-word (third words))
        when (and pid ppid pgid (nthcdr 3 words))
          collect (make-instance 'os-process :pid pid :ppid ppid :pgid pgid
                                             :start (format nil "~{~A~^ ~}" (nthcdr 3 words)))))

(defparameter +cim-command+
  "Get-CimInstance Win32_Process | ForEach-Object { '{0} {1} {2}' -f $_.ProcessId, $_.ParentProcessId, $(if ($_.CreationDate) { $_.CreationDate.ToUniversalTime().ToString('yyyyMMddHHmmss.ffffff') } else { '' }) }"
  "PowerShell: one line per process, pid ppid CreationDate (UTC yyyyMMddHHmmss.ffffff).")

(defmethod inspector-list-processes ((inspector cim-inspector))
  (loop for line in (%lines (%run-output (list "powershell.exe" "-NoProfile" "-NonInteractive"
                                               "-Command" +cim-command+)))
        for words = (%split-words line)
        for pid = (%integer-word (first words))
        for ppid = (%integer-word (second words))
        when (and pid ppid)
          collect (make-instance 'os-process :pid pid :ppid ppid :pgid nil
                                             :start (or (third words) ""))))

(defun list-os-processes (&key (inspector (default-os-inspector)))
  (inspector-list-processes inspector))

(defun %find-os-process (pid inspector)
  (find pid (list-os-processes :inspector inspector) :key #'os-process-pid))

(defun os-process-start-time (pid &key (inspector (default-os-inspector)))
  "The START string of PID (as LIST-OS-PROCESSES reports it), or NIL when no such process."
  (let ((p (%find-os-process pid inspector)))
    (and p (os-process-start p))))

(defun %zombie-p (pid inspector)
  "Unix: ps reports PID as a zombie (exited, not reaped)."
  (and (typep inspector 'ps-inspector)
       (let ((stat (string-trim '(#\Space #\Tab #\Newline #\Return)
                                (%run-output (list "ps" "-o" "stat=" "-p" (princ-to-string pid))))))
         (and (plusp (length stat)) (char-equal (char stat 0) #\Z)))))

(defun os-process-alive-p (pid &key start-time (inspector (default-os-inspector)))
  "True when PID exists (and is not a zombie) and, given START-TIME, started then."
  (let ((p (%find-os-process pid inspector)))
    (and p
         (or (null start-time) (string= start-time (os-process-start p)))
         (not (%zombie-p pid inspector)))))

(defun kill-os-processes (pids &key force (inspector (default-os-inspector)))
  "Signal each of PIDS: TERM, or KILL with FORCE (Windows: always taskkill /F, one
/PID each, no /T). Pids already gone are not an error. → NIL."
  (when pids
    (if (typep inspector 'cim-inspector)
        (dolist (pid pids)
          (%run-quietly (list "taskkill" "/F" "/PID" (princ-to-string pid))))
        (%run-quietly (list* "kill" (if force "-KILL" "-TERM")
                             (mapcar #'princ-to-string pids)))))
  nil)

(defun kill-os-process-group (pgid &key force)
  "Unix: kill -TERM (-KILL with FORCE) -- -PGID. Gone is not an error. Windows:
SUPERVISOR-ERROR (no process groups). → NIL."
  (when (uiop:os-windows-p)
    (error 'supervisor-error :message "Windows has no process groups (kill-os-process-group)"))
  (check-type pgid (integer 1))
  (%run-quietly (list "kill" (if force "-KILL" "-TERM") "--" (format nil "-~D" pgid)))
  nil)
