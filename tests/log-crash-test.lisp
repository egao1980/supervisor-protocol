(in-package #:supervisor-protocol/tests)

;;; H8b crash recovery: a child image (a supervisor with a chatty thread unit) is
;;; SIGKILLed / TerminateProcess'd mid-output. Its segments stay readable by the
;;; next image, a torn tail is skipped, indexes are rebuilt, and a new
;;; supervisor on the same state directory leaves the old logs alone.

(defun chatty-child-form (state-dir)
  (sp-form
   (format nil "(let* ((ready nil) (sup (sp:make-supervisor :state-directory ~A :exit-hook nil :log-segment-size 16384 :log-segments 1000)) (u (sp:start-unit sup :subagent :function (lambda (u) (declare (ignore u)) (loop for i from 1 do (format t \"n-~~D~~%\" i) (finish-output) (when (= i 2000) (setf ready t)) (sleep 0.0005)))))) (loop repeat 6000 until ready do (sleep 0.01)) (format t \"READY ~~S ~~S~~%\" (sp:supervisor-id sup) (sp:unit-id u)) (finish-output) (sleep 300))"
           (lit (namestring state-dir)))))

(deftest log-segments-survive-a-killed-supervisor
  (with-temp-dir (dir)
    (let* ((child (launch-child-image (chatty-child-form dir)))
           (line (child-ready-line child))
           (pid (pp:pid (child-handle child))))
      (unwind-protect
           (progn
             (ok line "the child supervisor is writing")
             (when line
               (sleep 0.2)
               (os-kill pid)
               (ok (all-dead-p (list pid)) "the child image is gone")
               (destructuring-bind (sup-id unit-id) (ready-pids line)
                 (let ((logs (find-unit-logs dir)))
                   (ok (= 1 (length logs)))
                   (let ((log (first logs)))
                     (ok (equal sup-id (unit-log-supervisor-id log)))
                     (ok (equal unit-id (unit-log-unit-id log)))
                     (multiple-value-bind (records gaps) (log-read log)
                       (let ((k (length records)))
                         (ok (>= k 2000) "everything written before the kill")
                         (ok (null gaps))
                         (ok (equal (loop for i from 1 to k collect (format nil "n-~D" i))
                                    (texts records))
                             "contiguous, in order, nothing corrupt")
                         (ok (> (length (log-segments log)) 2) "across several segments")
                         (ok (not (log-closed-p log)) "no end record after a SIGKILL")
                         ;; As if the kill had torn the last write.
                         (append-file-octets (log-segment-path (car (last (log-segments log))))
                                             (utf8 "999999 17000000 1 ot. n-tor"))
                         (ok (equal (texts records) (texts (log-read log))) "the torn tail is skipped")
                         (ok (log-segment-torn-p (car (last (log-segments log)))))
                         (mapc (lambda (p) (delete-file p)) (index-files (unit-log-directory log)))
                         (ok (equal (texts records) (texts (log-read log))) "without any index")
                         (ok (= (length (log-segments log)) (rebuild-log-index log)))
                         (ok (= (length (log-segments log)) (length (index-files (unit-log-directory log)))))
                         (ok (equal (last (texts records) 5) (texts (log-tail log 5))))))
                     ;; A new supervisor on the same directory: its total cap counts
                     ;; only its own logs.
                     (let ((before (files-bytes (segment-files (unit-log-directory log)))))
                       (with-supervisor (sup :state-directory dir :log-total-cap 20000
                                             :log-segment-size 4096 :log-segments 50)
                         (let ((u (start-unit sup :subagent
                                              :function (writer-body (numbered-lines "new-" 500 :pad 40)))))
                           (ok (eq :exited (wait-unit u :timeout 20)))
                           (ok (<= (supervisor-log-usage sup) 20000))
                           (ok (= 2 (length (find-unit-logs dir))))
                           (ok (= before (files-bytes (segment-files (unit-log-directory log))))
                               "the old supervisor's log is untouched")))))))))
        (kill-pids-quietly (list pid))))))
