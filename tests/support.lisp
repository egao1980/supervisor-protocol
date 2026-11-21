(in-package #:supervisor-protocol/tests)

;;; Test support. Child processes are this SBCL itself (runtime + core, no
;;; init files), so the same programs run on macOS, Linux and Windows: sleepers
;;; that write their pid to a file, an echo server on stdin/stdout, a "tree"
;;; that starts grandchildren, a program that exits with a given code. Shell
;;; jobs use /bin/sh -c or cmd.exe /d /s /c. Liveness and external kills use
;;; ps / kill (Unix) and tasklist / taskkill (Windows) directly, never the
;;; code under test.

(defun windows-p () (uiop:os-windows-p))
(defun unix-p () (not (uiop:os-windows-p)))

(defmacro deftest-unix (name &body body)
  "BODY on Unix; on Windows a skip with a note."
  `(deftest ,name
     (if (unix-p)
         (progn ,@body)
         (skip "POSIX only (process groups / signals / ps): skipped on Windows"))))

(defmacro deftest-windows (name &body body)
  `(deftest ,name
     (if (windows-p)
         (progn ,@body)
         (skip "Windows only (CIM / taskkill): skipped on this OS"))))

;;; time

(defun now () (/ (get-internal-real-time) internal-time-units-per-second))

(defun wait-until (predicate &key (timeout 5) (step 0.02))
  "Poll PREDICATE until true or TIMEOUT seconds; → its last value."
  (loop with deadline = (+ (now) timeout)
        for v = (funcall predicate)
        when (or v (> (now) deadline)) return v
        do (sleep step)))

(defun elapsed (fn)
  "→ (values seconds result) of calling FN."
  (let* ((t0 (now)) (r (funcall fn)))
    (values (- (now) t0) r)))

;;; directories

(defun %fresh-name (prefix)
  (format nil "~A-~36R" prefix (random (expt 36 8) (make-random-state t))))

(defun call-with-temp-dir (fn &key (prefix "supervisor-protocol"))
  (let ((dir (uiop:ensure-directory-pathname
              (merge-pathnames (%fresh-name prefix) (uiop:temporary-directory)))))
    (ensure-directories-exist dir)
    (unwind-protect (funcall fn dir)
      (ignore-errors
       (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore)))))

(defmacro with-temp-dir ((var &rest keys) &body body)
  `(call-with-temp-dir (lambda (,var) ,@body) ,@keys))

(defun units-dir-files (state-dir)
  (directory (merge-pathnames "*.unit" (unit-file-directory state-dir))))

;;; supervisors

(defun %quiet-shutdown (sup)
  "Shut SUP down for cleanup: short grace, abandon what will not die."
  (ignore-errors
   (handler-bind ((stop-failed (lambda (c)
                                 (let ((r (find-restart 'abandon c)))
                                   (when r (invoke-restart r))))))
     (shutdown-supervisor sup :grace 0.5 :force-grace 1))))

(defmacro with-supervisor ((var &rest keys) &body body)
  "BODY with VAR a new supervisor (no exit hook unless KEYS say so), shut down after."
  `(let ((,var (make-supervisor :exit-hook nil ,@keys)))
     (unwind-protect (progn ,@body)
       (%quiet-shutdown ,var))))

;;; child programs (this SBCL)

(defun lisp-argv (&rest forms)
  "argv running this SBCL with no init files, evaluating FORMS (strings) in order."
  (append (list (uiop:native-namestring sb-ext:*runtime-pathname*)
                "--core" (uiop:native-namestring sb-ext:*core-pathname*)
                "--noinform" "--no-sysinit" "--no-userinit" "--non-interactive")
          (loop for f in forms append (list "--eval" f))))

(defparameter +pid-expr+
  "#+win32 (sb-alien:alien-funcall (sb-alien:extern-alien \"GetCurrentProcessId\" (function sb-alien:unsigned-long))) #-win32 (sb-unix:unix-getpid)")

(defun lit (object)
  "OBJECT printed readably for embedding in a child's --eval form."
  (with-standard-io-syntax (prin1-to-string object)))

(defun pid-file-form (pid-file)
  (if pid-file
      (format nil "(with-open-file (s ~A :direction :output :if-exists :supersede) (print ~A s))"
              (lit (namestring pid-file)) +pid-expr+)
      "nil"))

(defun sleeper-argv (seconds &key pid-file)
  "A child that writes its pid to PID-FILE (if any), then sleeps SECONDS."
  (lisp-argv (format nil "(progn ~A (sleep ~D))" (pid-file-form pid-file) seconds)))

(defun exit-argv (code &key (delay 0))
  "A child that sleeps DELAY seconds, then exits with CODE."
  (lisp-argv (format nil "(progn (sleep ~A) (sb-ext:exit :code ~D :abort t))" delay code)))

(defun echo-server-argv ()
  "A fake stdio server: echoes every stdin line to stdout until EOF."
  (lisp-argv "(loop for l = (read-line *standard-input* nil) while l do (write-line l) (finish-output))"))

(defun term-ignoring-argv (seconds)
  "Unix: a child that ignores SIGTERM and sleeps SECONDS."
  (lisp-argv (format nil "(progn (sb-sys:enable-interrupt sb-unix:sigterm :ignore) (sleep ~D))" seconds)))

(defun tree-argv (pid-dir n seconds)
  "A child that writes PID-DIR/root.pid, starts N grandchild sleepers (each writes
PID-DIR/gc-<i>.pid; stdin inherited, so they stay in its process group on Unix),
then sleeps SECONDS."
  (let ((dir (uiop:ensure-directory-pathname pid-dir)))
    (lisp-argv
     (format nil "(progn ~A ~{~A ~}(sleep ~D))"
             (pid-file-form (merge-pathnames "root.pid" dir))
             (loop for i from 1 to n
                   for argv = (sleeper-argv seconds
                                            :pid-file (merge-pathnames (format nil "gc-~D.pid" i) dir))
                   collect (format nil "(sb-ext:run-program ~A '~A :wait nil :input t :output nil :error nil)"
                                   (lit (first argv)) (lit (rest argv))))
             seconds))))

(defun shell-argv (line)
  (if (windows-p)
      (list "cmd.exe" "/d" "/s" "/c" line)
      (list "/bin/sh" "-c" line)))

(defun shell-sleep-argv (seconds)
  (shell-argv (if (windows-p)
                  (format nil "ping -n ~D 127.0.0.1 >nul" (1+ seconds))
                  (format nil "sleep ~D" seconds))))

(defun posix-tree-argv (pid-dir)
  "Unix: sh with two background sleeps, their pids in PID-DIR/gc-1.pid, gc-2.pid."
  (let ((d (uiop:native-namestring (uiop:ensure-directory-pathname pid-dir))))
    (list "/bin/sh" "-c"
          (format nil "sleep 300 & echo $! > '~Agc-1.pid'; sleep 300 & echo $! > '~Agc-2.pid'; wait" d d))))

(defun read-pid-files (dir)
  "Pids in DIR/*.pid (complete files only)."
  (loop for p in (directory (merge-pathnames "*.pid" (uiop:ensure-directory-pathname dir)))
        for pid = (ignore-errors
                   (with-open-file (s p) (let ((*read-eval* nil)) (read s nil nil))))
        when (integerp pid) collect pid))

(defun wait-pid-files (dir n &key (timeout 30))
  "Wait until DIR holds N pid files; → their pids (or what is there at TIMEOUT)."
  (wait-until (lambda () (>= (length (read-pid-files dir)) n)) :timeout timeout)
  (read-pid-files dir))

;;; liveness and kills, independent of the code under test

(defun pid-alive-p (pid)
  (if (windows-p)
      (let ((out (nth-value 1 (pp:run (list "tasklist" "/FI" (format nil "PID eq ~D" pid) "/NH" "/FO" "CSV")
                                      :output :string :error-output nil))))
        (and (search (format nil "\"~D\"" pid) out) t))
      (let ((out (string-trim '(#\Space #\Tab #\Newline #\Return)
                              (nth-value 1 (pp:run (list "ps" "-o" "stat=" "-p" (princ-to-string pid))
                                                   :output :string :error-output nil)))))
        (and (plusp (length out)) (char/= (char out 0) #\Z)))))

(defun all-dead-p (pids &key (timeout 10))
  (wait-until (lambda () (notany #'pid-alive-p pids)) :timeout timeout :step 0.1))

(defun os-kill (pid)
  "Kill PID from outside (SIGKILL / taskkill /F), as a crash or the OOM killer would."
  (if (windows-p)
      (pp:run (list "taskkill" "/F" "/PID" (princ-to-string pid)) :output nil :error-output nil)
      (pp:run (list "kill" "-9" (princ-to-string pid)) :output nil :error-output nil)))

(defun kill-pids-quietly (pids)
  (dolist (p pids) (ignore-errors (os-kill p))))

;;; events

(defstruct (collector (:constructor %make-collector))
  (lock (bt2:make-lock :name "collector"))
  (events '())
  subscription)

(defun make-collector (sup &key since)
  (let ((c (%make-collector)))
    (setf (collector-subscription c)
          (subscribe sup (lambda (e)
                           (bt2:with-lock-held ((collector-lock c))
                             (push e (collector-events c))))
                     :since since))
    c))

(defun collected (c)
  (bt2:with-lock-held ((collector-lock c)) (reverse (collector-events c))))

(defun wait-collected (c predicate &key (timeout 10))
  "Wait until PREDICATE is true of the collected events; → them."
  (wait-until (lambda () (funcall predicate (collected c))) :timeout timeout)
  (collected c))

(defun unit-events (events unit-id)
  (remove unit-id events :key #'event-unit-id :test-not #'equal))

(defun transitions (events unit-id)
  "(type from to) of UNIT-ID's events."
  (mapcar (lambda (e) (list (event-type e) (event-from e) (event-to e)))
          (unit-events events unit-id)))

(defun ids (units) (mapcar #'unit-id units))

(defun wait-state (unit state &key (timeout 10))
  (wait-until (lambda () (eq (unit-state unit) state)) :timeout timeout))

;;; test executors (the executor protocol, exercised without the OS)

(defclass stubborn-executor (executor)
  ((obeys-force :initarg :obeys-force :initform t :reader obeys-force)
   (lock :initform (bt2:make-lock :name "stubborn"))
   (stops :initform '() :accessor stubborn-stops)))

(defstruct stubborn-handle (alive t))

(defmethod executor-launch ((e stubborn-executor) unit)
  (declare (ignore unit))
  (make-stubborn-handle))

(defmethod executor-stop ((e stubborn-executor) unit handle &key force)
  (declare (ignore unit))
  (bt2:with-lock-held ((slot-value e 'lock))
    (push (if force :force :graceful) (stubborn-stops e)))
  (when (and force (obeys-force e))
    (setf (stubborn-handle-alive handle) nil))
  t)

(defmethod executor-wait ((e stubborn-executor) unit handle &key timeout)
  (declare (ignore unit))
  (loop with deadline = (and timeout (+ (now) timeout))
        while (stubborn-handle-alive handle)
        when (and deadline (> (now) deadline)) do (return-from executor-wait (values nil))
        do (sleep 0.01))
  (values t nil nil nil))

(defmethod executor-alive-p ((e stubborn-executor) unit handle)
  (declare (ignore unit))
  (stubborn-handle-alive handle))

(defmethod executor-supports-p ((e stubborn-executor) capability)
  (eq capability :graceful-stop))

(defun stops-received (e)
  (bt2:with-lock-held ((slot-value e 'lock)) (reverse (stubborn-stops e))))

(defclass failing-executor (executor) ())

(defmethod executor-launch ((e failing-executor) unit)
  (error 'unit-start-error :kind (unit-kind unit) :cause "refused by the test executor"))

;;; thread bodies

(defun cooperative-body (&optional (result :done))
  "A thread unit body that runs until a stop is requested, then returns RESULT."
  (lambda (u)
    (loop until (unit-stop-requested-p u) do (sleep 0.01))
    result))

;;; child images that load supervisor-protocol (crash and exit simulations)

(defun loaded-system-directories ()
  (remove-duplicates
   (loop for name in (asdf:already-loaded-systems)
         for dir = (ignore-errors (asdf:system-source-directory name))
         when dir collect dir)
   :test #'equal))

(defun child-setup-form ()
  (format nil "(progn (require :asdf) (funcall (intern \"INITIALIZE-SOURCE-REGISTRY\" :asdf) '~A) (let ((*standard-output* (make-broadcast-stream)) (*error-output* (make-broadcast-stream))) (funcall (intern \"LOAD-SYSTEM\" :asdf) \"supervisor-protocol\")))"
          (lit `(:source-registry
                 ,@(mapcar (lambda (d) (list :directory d))
                           (loaded-system-directories))
                 :ignore-inherited-configuration))))

(defstruct child handle (lock (bt2:make-lock :name "child")) (lines '()) reader)

(defun launch-child-image (form)
  "Start a child SBCL that loads supervisor-protocol, then evaluates FORM (a string,
read in CL-USER). Its stdout lines are collected; it has no process group, so a
kill reaches the child alone."
  (let* ((h (pp:launch (lisp-argv (child-setup-form) form)
                       :input nil :output :stream :error-output :output))
         (c (make-child :handle h)))
    (setf (child-reader c)
          (bt2:make-thread
           (lambda ()
             (ignore-errors
              (loop for l = (read-line (pp:stdout h) nil) while l
                    do (bt2:with-lock-held ((child-lock c)) (push l (child-lines c))))))
           :name "child-reader"))
    c))

(defun child-ready-line (c &key (timeout 240))
  "The child's first stdout line starting with READY, or NIL (also when the child
exits first; its output is then printed for the diagnosis)."
  (wait-until (lambda ()
                (or (bt2:with-lock-held ((child-lock c))
                      (find-if (lambda (l) (uiop:string-prefix-p "READY" l)) (child-lines c)))
                    (when (pp:exit-code (child-handle c))
                      (sleep 0.2)
                      (format *error-output* "~&child exited early:~%~{  ~A~%~}"
                              (last (reverse (child-lines c)) 20))
                      (return-from child-ready-line nil))))
              :timeout timeout :step 0.1))

(defun ready-pids (line)
  "The integers after READY."
  (with-input-from-string (s (subseq line 5))
    (let ((*read-eval* nil))
      (loop for x = (read s nil nil) while x collect x))))

(defun sp-form (string)
  "STRING with sp: standing for supervisor-protocol: (child forms are read in CL-USER)."
  (let ((out (make-string-output-stream)))
    (loop with start = 0
          for pos = (search "sp:" string :start2 start)
          do (write-string string out :start start :end (or pos (length string)))
             (if pos
                 (progn (write-string "supervisor-protocol:" out) (setf start (+ pos 3)))
                 (return)))
    (get-output-stream-string out)))

(defun child-units-form (state-dir pid-dir &key exit-hook then)
  "A child program: a supervisor on STATE-DIR, a :shell-job tree (2 grandchildren
with pid files in PID-DIR), an :rpc sleeper and a :subagent thread; once the pid
files are there it prints READY <tree-pid> <rpc-pid>, then evaluates THEN."
  (sp-form
   (format nil "(let* ((sup (sp:make-supervisor :state-directory ~A :exit-hook ~A)) (tree (sp:start-unit sup :shell-job :command '~A)) (srv (sp:start-unit sup :rpc :command '~A))) (sp:start-unit sup :subagent :function (lambda (u) (loop until (sp:unit-stop-requested-p u) do (sleep 0.02)))) (loop repeat 600 until (= 3 (length (directory ~A))) do (sleep 0.05)) (format t \"READY ~~D ~~D~~%\" (sp:unit-pid tree) (sp:unit-pid srv)) (finish-output) ~A)"
           (lit (namestring state-dir))
           (if exit-hook "t" "nil")
           (lit (tree-argv pid-dir 2 300))
           (lit (sleeper-argv 300))
           (lit (namestring (merge-pathnames "*.pid" (uiop:ensure-directory-pathname pid-dir))))
           then)))

(defun spawn (fn &key (name "test-worker"))
  "A thread running FN; an ERROR ends it quietly (its value is then the condition),
so a stub or a bug fails the assertions instead of killing the test image."
  (bt2:make-thread (lambda () (handler-case (funcall fn) (error (e) e))) :name name))
