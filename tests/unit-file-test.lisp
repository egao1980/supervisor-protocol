(in-package #:supervisor-protocol/tests)

;;; Durable unit files: one per live process unit, gone once it is finished.

(deftest unit-file-directory-path
  (with-temp-dir (dir)
    (ok (equal (merge-pathnames (make-pathname :directory '(:relative "units")) dir)
               (unit-file-directory dir)))
    (ok (equal (unit-file-directory dir)
               (unit-file-directory (string-right-trim "/\\" (namestring dir))))
        "a namestring without the trailing separator")))

(deftest unit-file-lifecycle
  ;; The child lives 8 s: the Windows CIM inspector can take seconds, and the
  ;; "OS's own value" check needs the process still running (2 s flaked on CI).
  (with-temp-dir (dir)
    (with-supervisor (sup :state-directory dir)
      (ok (equal (uiop:ensure-directory-pathname dir)
                 (uiop:ensure-directory-pathname (supervisor-state-directory sup))))
      (let* ((u (start-unit sup :shell-job :command (exit-argv 0 :delay 8) :cwd dir))
             (path (unit-file-path u)))
        (ok (and path (probe-file path)) "written at launch")
        (ok (equal (unit-file-directory dir) (uiop:pathname-directory-pathname path)))
        (ok (string= (format nil "~A--~A" (supervisor-id sup) (unit-id u)) (pathname-name path)))
        (ok (string= "unit" (pathname-type path)))
        (let ((plist (read-unit-file path)))
          (ok (= 1 (getf plist :format)))
          (ok (string= (supervisor-id sup) (getf plist :supervisor-id)))
          (ok (= (current-pid) (getf plist :supervisor-pid)))
          (ok (equal (os-process-start-time (current-pid)) (getf plist :supervisor-os-start)))
          (ok (string= (unit-id u) (getf plist :unit-id)))
          (ok (eq :shell-job (getf plist :kind)))
          (ok (= (unit-pid u) (getf plist :pid)))
          (ok (eql (unit-pgid u) (getf plist :pgid)))
          (ok (equal (unit-os-start-time u) (getf plist :os-start)))
          (ok (equal (os-process-start-time (unit-pid u)) (getf plist :os-start)) "the OS's own value")
          (ok (equal (unit-command u) (getf plist :command)))
          (ok (stringp (getf plist :cwd)))
          (ok (eq (if (windows-p) :cim :ps) (getf plist :inspector))))
        (ok (eq :exited (wait-unit u :timeout 30)))
        (ok (null (unit-file-path u)) "deleted once finished")
        (ok (not (probe-file path)))
        (ok (null (units-dir-files dir)))))))

(deftest no-unit-file-without-a-state-directory-or-a-process
  (with-temp-dir (dir)
    (with-supervisor (plain)
      (ok (null (unit-file-path (start-unit plain :shell-job :command (sleeper-argv 30))))))
    (with-supervisor (sup :state-directory dir)
      (ok (null (unit-file-path (start-unit sup :subagent :function (cooperative-body))))
          "threads die with the image")
      (ok (null (units-dir-files dir))))))

(deftest unit-file-follows-restarts
  (with-temp-dir (dir)
    (with-supervisor (sup :state-directory dir)
      (let* ((srv (start-unit sup :rpc :command (sleeper-argv 60) :backoff '(0.1 1 0.1)))
             (pid1 (unit-pid srv)))
        (os-kill pid1)
        (ok (wait-until (lambda () (and (unit-pid srv) (/= pid1 (unit-pid srv))
                                        (eq :running (unit-state srv))))
                        :timeout 20))
        (ok (wait-until (lambda () (eql (unit-pid srv) (getf (read-unit-file (unit-file-path srv)) :pid)))
                        :timeout 5)
            "rewritten with the new pid")
        (ok (= 1 (length (units-dir-files dir))))))))

(deftest unit-file-gone-after-stop-and-eviction
  (with-temp-dir (dir)
    (with-supervisor (sup :state-directory dir :kind-caps '(:shell-job 1))
      (let ((a (start-unit sup :shell-job :command (sleeper-argv 60))))
        (ok (= 1 (length (units-dir-files dir))))
        (stop-unit a)
        (ok (null (units-dir-files dir)) "stopped")
        (handler-bind ((unit-limit-reached (lambda (c) (invoke-restart (find-restart 'evict-lru c)))))
          (let ((b (start-unit sup :shell-job :command (sleeper-argv 60))))
            (ok (= 1 (length (units-dir-files dir))))
            (handler-bind ((unit-limit-reached (lambda (c) (invoke-restart (find-restart 'evict-lru c)))))
              (start-unit sup :shell-job :command (sleeper-argv 60)))
            (ok (null (find-unit sup (unit-id b))))
            (ok (= 1 (length (units-dir-files dir))) "the evicted unit's file is gone")))))))

(deftest unit-file-round-trip
  (with-temp-dir (dir)
    (with-supervisor (sup :state-directory dir)
      (let* ((u (start-unit sup :shell-job :command (sleeper-argv 30)))
             (path (unit-file-path u)))
        (delete-file path)
        (ok (equal path (write-unit-file u)) "rewrites at the same path")
        (ok (= (unit-pid u) (getf (read-unit-file path) :pid)))
        (ok (null (directory (merge-pathnames "*.tmp" (unit-file-directory dir))))
            "no temporary file left")))))

(deftest unreadable-unit-files
  (with-temp-dir (dir)
    (let ((units (unit-file-directory dir)))
      (makunbound (intern "H8A-EVALUATED" :cl-user))
      (ensure-directories-exist units)
      (flet ((put (name text)
               (let ((p (merge-pathnames name units)))
                 (with-open-file (s p :direction :output :if-exists :supersede) (write-string text s))
                 p)))
        (dolist (text (list "(:format 1 :pid"                    ; truncated
                            "#.(setf (symbol-value (intern \"H8A-EVALUATED\" :cl-user)) t)"
                            "(:format 1 :pid 12 :kind)"          ; odd plist
                            "(:format 99 :pid 12)"               ; unknown format
                            "just-a-symbol"))
          (let ((p (put "x--y.unit" text)) (warned nil))
            (ok (null (handler-bind ((unit-file-unreadable
                                       (lambda (w) (setf warned w) (muffle-warning w))))
                        (read-unit-file p)))
                text)
            (ok (and warned (equal p (unit-file-unreadable-path warned))))))
        (ok (not (boundp (intern "H8A-EVALUATED" :cl-user))) "#. is never evaluated")))))
