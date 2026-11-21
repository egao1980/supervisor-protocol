(in-package #:supervisor-protocol/tests)

;;; OS process inspection and the orphan sweep (crash recovery).

(defun self-process (&optional (inspector (default-os-inspector)))
  (find (current-pid) (list-os-processes :inspector inspector) :key #'os-process-pid))

(defun check-inspector (inspector)
  (let ((me (self-process inspector)))
    (ok me "this image is listed")
    (ok (typep (os-process-ppid me) '(integer 0)))
    (ok (and (stringp (os-process-start me)) (plusp (length (os-process-start me)))))
    (if (windows-p)
        (ok (null (os-process-pgid me)))
        (ok (typep (os-process-pgid me) '(integer 1))))
    (ok (equal (os-process-start me) (os-process-start-time (current-pid) :inspector inspector)))
    (ok (equal (os-process-start-time (current-pid) :inspector inspector)
               (os-process-start-time (current-pid) :inspector inspector))
        "stable")))

(deftest-unix ps-inspector-lists-processes
  (check-inspector (make-ps-inspector))
  (ok (not (find #\Tab (os-process-start (self-process (make-ps-inspector)))))))

(deftest-windows cim-inspector-lists-processes
  (check-inspector (make-cim-inspector))
  (let ((start (os-process-start (self-process (make-cim-inspector)))))
    (ok (= 21 (length start)) "yyyyMMddHHmmss.ffffff")))

(deftest os-process-liveness
  (ok (os-process-alive-p (current-pid)))
  (ok (os-process-alive-p (current-pid) :start-time (os-process-start-time (current-pid))))
  (ok (not (os-process-alive-p (current-pid) :start-time "not the start time")))
  (let ((h (pp:launch (exit-argv 0) :output nil :error-output nil)))
    (let ((pid (pp:pid h)))
      (pp:wait h :timeout 30)
      (ok (not (os-process-alive-p pid)) "an exited, reaped child")
      (ok (null (os-process-start-time pid))))))

(deftest kill-os-processes-kills
  (let* ((h (pp:launch (sleeper-argv 60) :output nil :error-output nil))
         (pid (pp:pid h)))
    (unwind-protect
         (progn
           (ok (wait-until (lambda () (os-process-alive-p pid))))
           (ok (null (kill-os-processes (list pid) :force t)))
           (ok (wait-until (lambda () (pp:exit-code h)) :timeout 10))
           (ok (null (kill-os-processes (list pid))) "gone is not an error"))
      (ignore-errors (pp:kill h :force t)))))

(deftest-unix kill-os-process-group-kills
  (with-temp-dir (pids)
    (let* ((h (pp:launch (posix-tree-argv pids) :process-group t :output nil :error-output nil))
           (gc (wait-pid-files pids 2)))
      (unwind-protect
           (progn
             (ok (null (kill-os-process-group (pp:pid h))))
             (ok (all-dead-p gc)))
        (ignore-errors (pp:kill h :force t :tree t))))))

(deftest-windows kill-os-process-group-unsupported
  (ok (signals (kill-os-process-group 1234) 'supervisor-error)))

;;; Unit files the sweep reads, written by hand (a dead or live owner).

(defun fake-unit-file (dir name &rest plist)
  (let ((p (merge-pathnames (format nil "~A.unit" name) (unit-file-directory dir))))
    (ensure-directories-exist p)
    (with-open-file (s p :direction :output :if-exists :supersede)
      (with-standard-io-syntax
        (let ((*package* (find-package :keyword)))
          (prin1 (append '(:format 1) plist) s))))
    p))

(defun dead-pid ()
  "The pid of a child that has exited and been reaped."
  (let ((h (pp:launch (exit-argv 0) :output nil :error-output nil)))
    (prog1 (pp:pid h) (pp:wait h :timeout 30))))

(defun launch-orphan-tree (pids)
  "A process tree started outside any supervisor (as a dead one left it)."
  (pp:launch (tree-argv pids 2 300) :process-group t :output nil :error-output nil))

(defun owner-plist (&key (pid (dead-pid)) (start "gone") (id "0-deadbeef"))
  (list :supervisor-id id :supervisor-pid pid :supervisor-os-start start))

(defun record-for (records name)
  (find name records :key #'sweep-record-unit-id :test #'equal))

(deftest sweep-missing-directory
  (with-temp-dir (dir)
    (ok (null (sweep-orphans (merge-pathnames "nope/" dir))))))

(deftest sweep-kills-orphans-of-a-dead-supervisor
  (with-temp-dir (dir)
    (with-temp-dir (pids)
      (let* ((h (launch-orphan-tree pids))
             (all (wait-pid-files pids 3))
             (pid (pp:pid h)))
        (unwind-protect
             (let ((path (apply #'fake-unit-file dir "0-deadbeef--shell-job-1"
                                :unit-id "shell-job-1" :kind :shell-job :pid pid
                                :pgid (if (windows-p) nil pid)
                                :os-start (os-process-start-time pid)
                                :command '("tree") :cwd nil
                                :inspector (if (windows-p) :cim :ps)
                                (owner-plist))))
               (let* ((records (sweep-orphans dir))
                      (r (record-for records "shell-job-1")))
                 (ok (= 1 (length records)))
                 (ok (eq :killed (sweep-record-action r)))
                 (ok (= pid (sweep-record-pid r)))
                 (ok (equal path (sweep-record-path r)))
                 (ok (member pid (sweep-record-killed-pids r)))
                 (ok (all-dead-p all) "root and grandchildren")
                 (ok (not (probe-file path)) "file deleted")))
          (kill-pids-quietly all)
          (ignore-errors (pp:kill h :force t :tree t)))))))

(deftest sweep-guards-pid-reuse
  (with-temp-dir (dir)
    (let* ((h (pp:launch (sleeper-argv 60) :process-group t :output nil :error-output nil))
           (pid (pp:pid h)))
      (unwind-protect
           (let ((path (apply #'fake-unit-file dir "0-deadbeef--rpc-1"
                              :unit-id "rpc-1" :kind :rpc :pid pid :pgid (if (windows-p) nil pid)
                              :os-start (if (windows-p) "19700101000000.000000" "Thu Jan 1 00:00:00 1970")
                              :command '("x") :cwd nil :inspector (if (windows-p) :cim :ps)
                              (owner-plist))))
             (let ((r (first (sweep-orphans dir))))
               (ok (eq :reused (sweep-record-action r)))
               (ok (null (sweep-record-killed-pids r)))
               (sleep 0.5)
               (ok (pid-alive-p pid) "an unrelated process with that pid survives")
               (ok (not (probe-file path)))))
        (ignore-errors (pp:kill h :force t :tree t))))))

(deftest sweep-reports-gone
  (with-temp-dir (dir)
    (let ((pid (dead-pid)))
      (apply #'fake-unit-file dir "0-deadbeef--shell-job-9"
             :unit-id "shell-job-9" :kind :shell-job :pid pid :pgid (if (windows-p) nil pid)
             :os-start "whatever" :command '("x") :cwd nil :inspector (if (windows-p) :cim :ps)
             (owner-plist))
      (let ((r (first (sweep-orphans dir))))
        (ok (eq :gone (sweep-record-action r)))
        (ok (null (units-dir-files dir)))))))

(deftest sweep-skips-a-live-owner
  (with-temp-dir (dir)
    (with-supervisor (live)
      (let* ((h (pp:launch (sleeper-argv 60) :process-group t :output nil :error-output nil))
             (pid (pp:pid h)))
        (unwind-protect
             (let ((path (apply #'fake-unit-file dir (format nil "~A--shell-job-1" (supervisor-id live))
                                :unit-id "shell-job-1" :kind :shell-job :pid pid
                                :pgid (if (windows-p) nil pid) :os-start (os-process-start-time pid)
                                :command '("x") :cwd nil :inspector (if (windows-p) :cim :ps)
                                (owner-plist :pid (current-pid)
                                             :start (os-process-start-time (current-pid))
                                             :id (supervisor-id live)))))
               (let ((r (first (sweep-orphans dir))))
                 (ok (eq :live-owner (sweep-record-action r)))
                 (ok (pid-alive-p pid))
                 (ok (probe-file path) "kept")))
          (ignore-errors (pp:kill h :force t :tree t)))))))

(deftest sweep-same-image-dead-supervisor-is-an-orphan
  ;; This image's pid, but a supervisor id that is not live here.
  (with-temp-dir (dir)
    (let* ((h (pp:launch (sleeper-argv 60) :process-group t :output nil :error-output nil))
           (pid (pp:pid h)))
      (unwind-protect
           (progn
             (apply #'fake-unit-file dir (format nil "~D-00000000--shell-job-1" (current-pid))
                    :unit-id "shell-job-1" :kind :shell-job :pid pid
                    :pgid (if (windows-p) nil pid) :os-start (os-process-start-time pid)
                    :command '("x") :cwd nil :inspector (if (windows-p) :cim :ps)
                    (owner-plist :pid (current-pid) :start (os-process-start-time (current-pid))
                                 :id (format nil "~D-00000000" (current-pid))))
             (ok (eq :killed (sweep-record-action (first (sweep-orphans dir)))))
             (ok (wait-until (lambda () (pp:exit-code h)) :timeout 10)))
        (ignore-errors (pp:kill h :force t :tree t))))))

(deftest sweep-deletes-unreadable-files
  (with-temp-dir (dir)
    (let ((p (merge-pathnames "junk.unit" (unit-file-directory dir))))
      (ensure-directories-exist p)
      (with-open-file (s p :direction :output) (write-string "(:format 1 :pid" s))
      (let* ((warned nil)
             (records (handler-bind ((unit-file-unreadable
                                       (lambda (w) (setf warned t) (muffle-warning w))))
                        (sweep-orphans dir))))
        (ok warned)
        (ok (eq :unreadable (sweep-record-action (first records))))
        (ok (not (probe-file p)))))))

(deftest make-supervisor-sweeps-first
  (with-temp-dir (dir)
    (let* ((h (pp:launch (sleeper-argv 60) :process-group t :output nil :error-output nil))
           (pid (pp:pid h)))
      (unwind-protect
           (progn
             (apply #'fake-unit-file dir "0-deadbeef--rpc-1"
                    :unit-id "rpc-1" :kind :rpc :pid pid :pgid (if (windows-p) nil pid)
                    :os-start (os-process-start-time pid) :command '("x") :cwd nil
                    :inspector (if (windows-p) :cim :ps) (owner-plist))
             (with-supervisor (sup :state-directory dir)
               (let ((report (supervisor-sweep-report sup)))
                 (ok (= 1 (length report)))
                 (ok (eq :killed (sweep-record-action (first report))))
                 (ok (wait-until (lambda () (pp:exit-code h)) :timeout 10))))
             (apply #'fake-unit-file dir "0-deadbeef--rpc-2"
                    :unit-id "rpc-2" :kind :rpc :pid (dead-pid) :pgid nil :os-start "x"
                    :command '("x") :cwd nil :inspector (if (windows-p) :cim :ps) (owner-plist))
             (with-supervisor (sup :state-directory dir :sweep nil)
               (ok (null (supervisor-sweep-report sup)))
               (ok (= 1 (length (units-dir-files dir))) ":sweep nil leaves the files")))
        (ignore-errors (pp:kill h :force t :tree t))))))

;;; The crash simulation: a child image with a supervisor and units is killed
;;; (SIGKILL / TerminateProcess: no exit hook runs). A new supervisor on the
;;; same state directory reaps every orphan, grandchildren included.

(deftest crash-then-sweep-reaps-every-orphan
  (with-temp-dir (state)
    (with-temp-dir (pids)
      (let* ((child (launch-child-image (child-units-form state pids :then "(sleep 600)")))
             (line (child-ready-line child))
             (orphans '()))
        (unwind-protect
             (progn
               (ok line "the child started its units")
               (when line
                 (destructuring-bind (tree-pid rpc-pid) (ready-pids line)
                   (setf orphans (list* rpc-pid (read-pid-files pids)))
                   (ok (member tree-pid orphans) "the tree root wrote its pid")
                   (ok (= 4 (length orphans)) "tree root, 2 grandchildren, rpc server")
                   (ok (= 2 (length (units-dir-files state))) "two process units, no thread file")
                   (pp:kill (child-handle child) :force t)
                   (pp:wait (child-handle child) :timeout 30)
                   (sleep 0.5)
                   (ok (every #'pid-alive-p orphans) "the crash left orphans behind")
                   (with-supervisor (sup :state-directory state)
                     (let ((report (supervisor-sweep-report sup)))
                       (ok (= 2 (length report)))
                       (ok (every (lambda (r) (eq :killed (sweep-record-action r))) report))
                       (ok (equal '("rpc-1" "shell-job-1")
                                  (sort (mapcar #'sweep-record-unit-id report) #'string<)))
                       (ok (all-dead-p orphans) "every orphan reaped, grandchildren included")
                       (ok (null (units-dir-files state))))))))
          (kill-pids-quietly orphans)
          (ignore-errors (pp:kill (child-handle child) :force t)))))))
