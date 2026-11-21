(in-package #:supervisor-protocol/tests)

;;; Stop = graceful → grace → force, tree-wide for processes; restarts
;;; FORCE / KEEP-WAITING on STOP-TIMEOUT, ABANDON on STOP-FAILED.

(deftest stop-process-gracefully
  (with-supervisor (sup)
    (let* ((u (start-unit sup :shell-job :command (sleeper-argv 60)))
           (pid (unit-pid u))
           (timeouts 0))
      (multiple-value-bind (secs state)
          (elapsed (lambda ()
                     (handler-bind ((stop-timeout (lambda (c) (declare (ignore c)) (incf timeouts))))
                       (stop-unit u :grace 5))))
        (ok (eq :killed state))
        (ok (< secs 5) "no full grace for a process that obeys"))
      (ok (= 0 timeouts))
      (ok (eq :killed (unit-state u)))
      (ok (unit-stop-requested-p u))
      (ok (eq (if (windows-p) :kill :term) (unit-signal u))
          "Unix: TERM was enough; Windows stops are always forced")
      (ok (all-dead-p (list pid)))
      (ok (eq :killed (stop-unit u)) "stopping a finished unit: its state"))))

(deftest-unix stop-escalates-on-a-term-ignoring-process
  (with-supervisor (sup)
    (let* ((u (start-unit sup :shell-job :command (term-ignoring-argv 60)))
           (seen '()))
      (sleep 1) ; let the child install its handler
      (multiple-value-bind (secs state)
          (elapsed (lambda ()
                     (handler-bind ((stop-timeout (lambda (c) (push (list (stop-timeout-phase c)
                                                                          (stop-timeout-unit c))
                                                                    seen))))
                       (stop-unit u :grace 0.5 :force-grace 3))))
        (ok (eq :killed state))
        (ok (< 0.45 secs 4)))
      (ok (equal (list (list :graceful u)) seen) "one stop-timeout, then the default: force")
      (ok (eq :kill (unit-signal u)))
      (ok (all-dead-p (list (unit-pid u)))))))

(deftest stop-timeout-keep-waiting-then-force
  (with-supervisor (sup)
    (let* ((e (make-instance 'stubborn-executor))
           (u (start-unit sup :pty :executor e))
           (n 0))
      (ok (eq :killed
              (handler-bind ((stop-timeout
                               (lambda (c)
                                 (ok (find-restart 'force c))
                                 (ok (find-restart 'keep-waiting c))
                                 (if (< (incf n) 3)
                                     (invoke-restart 'keep-waiting)
                                     (invoke-restart 'force)))))
                (stop-unit u :grace 0.1 :force-grace 1))))
      (ok (= 3 n) "signalled again after each extra grace")
      (ok (equal '(:graceful :force) (stops-received e))))))

(deftest stop-timeout-waited
  (with-supervisor (sup)
    (let ((u (start-unit sup :pty :executor (make-instance 'stubborn-executor)))
          (waited nil))
      (handler-bind ((stop-timeout (lambda (c) (setf waited (stop-timeout-waited c)))))
        (stop-unit u :grace 0.2))
      (ok (and waited (<= 0.15 waited 1))))))

(deftest stop-failed-and-abandon
  (with-supervisor (sup)
    (let* ((e (make-instance 'stubborn-executor :obeys-force nil))
           (u (start-unit sup :pty :executor e)))
      (ok (signals (stop-unit u :grace 0.1 :force-grace 0.1) 'stop-failed) "unhandled: an error")
      (let ((c nil))
        (ok (eq :killed (handler-bind ((stop-failed (lambda (x)
                                                      (setf c x)
                                                      (invoke-restart (find-restart 'abandon x)))))
                          (stop-unit u :grace 0.1 :force-grace 0.1))))
        (ok (eq u (unit-error-unit c))))
      (ok (eq :killed (unit-state u)))
      (ok (not (unit-live-p u))))))

(deftest abandoned-unit-event-detail
  (with-supervisor (sup)
    (let* ((c (make-collector sup))
           (u (start-unit sup :pty :executor (make-instance 'stubborn-executor :obeys-force nil))))
      (handler-bind ((stop-failed (lambda (x) (invoke-restart (find-restart 'abandon x)))))
        (stop-unit u :grace 0.1 :force-grace 0.1))
      (let ((last (car (last (wait-collected c (lambda (es) (= 3 (length es))))))))
        (ok (eq :killed (event-to last)))
        (ok (eq t (getf (event-detail last) :abandoned)))))))

(deftest stop-thread-cooperatively
  (with-supervisor (sup)
    (let ((u (start-unit sup :subagent :function (cooperative-body :stopped-cleanly))))
      (multiple-value-bind (secs state) (elapsed (lambda () (stop-unit u :grace 5)))
        (ok (eq :killed state))
        (ok (< secs 1)))
      (ok (eq :stopped-cleanly (unit-result u)) "the body returned after the stop request")
      (ok (null (unit-signal u))))))

(deftest stop-thread-by-force-runs-cleanups
  (with-supervisor (sup)
    (let* ((cleaned nil)
           (u (start-unit sup :subagent
                          :function (lambda (u) (declare (ignore u))
                                      (unwind-protect (sleep 60) (setf cleaned t))))))
      (sleep 0.1)
      (multiple-value-bind (secs state) (elapsed (lambda () (stop-unit u :grace 0.3 :force-grace 3)))
        (ok (eq :killed state))
        (ok (< secs 3)))
      (ok cleaned "unwind-protect cleanup ran")
      (ok (null (unit-condition u)) "a forced stop is not a failure"))))

(deftest-unix stop-kills-the-posix-tree
  (with-temp-dir (pids)
    (with-supervisor (sup)
      (let* ((u (start-unit sup :shell-job :command (posix-tree-argv pids)))
             (gc (wait-pid-files pids 2)))
        (ok (= 2 (length gc)))
        (ok (every #'pid-alive-p gc))
        (stop-unit u :grace 3)
        (ok (all-dead-p gc) "sh's background children died with it")))))

(deftest stop-kills-the-tree
  ;; Every OS: a Lisp child with two grandchildren (job object on Windows).
  (with-temp-dir (pids)
    (with-supervisor (sup)
      (let* ((u (start-unit sup :shell-job :command (tree-argv pids 2 300)))
             (all (wait-pid-files pids 3)))
        (ok (= 3 (length all)) "root + 2 grandchildren")
        (stop-unit u :grace 3)
        (ok (all-dead-p all))))))

(deftest exit-reaps-the-rest-of-the-tree
  ;; The direct child exits by itself; what it left in its group/job dies too.
  (with-temp-dir (pids)
    (with-supervisor (sup)
      (let* ((dir (uiop:ensure-directory-pathname pids))
             (u (start-unit sup :shell-job
                            :command (lisp-argv
                                      (format nil "(progn (sb-ext:run-program ~A '~A :wait nil :input t :output nil :error nil) (loop repeat 600 until (probe-file ~A) do (sleep 0.05)) (sb-ext:exit :code 0 :abort t))"
                                              (lit (first (sleeper-argv 300)))
                                              (lit (rest (sleeper-argv 300 :pid-file (merge-pathnames "gc.pid" dir))))
                                              (lit (namestring (merge-pathnames "gc.pid" dir))))))))
        (ok (eq :exited (wait-unit u :timeout 60)))
        (let ((gc (read-pid-files pids)))
          (ok (= 1 (length gc)))
          (ok (all-dead-p gc) "the grandchild did not outlive the unit"))))))

(deftest stop-units-selects
  (with-supervisor (sup)
    (let ((a (start-unit sup :subagent :function (cooperative-body) :labels '(:batch 1)))
          (b (start-unit sup :subagent :function (cooperative-body) :labels '(:batch 2)))
          (c (start-unit sup :rpc :function (cooperative-body) :labels '(:batch 1))))
      (ok (equal (list a) (stop-units sup :kind :subagent :labels '(:batch 1))))
      (ok (eq :killed (unit-state a)))
      (ok (eq :running (unit-state b)))
      (ok (eq :running (unit-state c)))
      (ok (equal '() (stop-units sup :state :exited)) "only live units are stopped"))))
