(in-package #:supervisor-protocol/tests)

;;; Given code: constants, conditions, the restart-policy defaults, the pid.

(deftest constants
  (ok (equal +unit-states+ '(:starting :running :exited :killed :failed)))
  (ok (equal +live-states+ '(:starting :running)))
  (ok (equal +final-states+ '(:exited :killed :failed)))
  (ok (equal +event-types+ '(:state :restart-scheduled :evicted :log-error)) "H8b adds :log-error")
  (ok (equal +scope-kinds+ '(:supervisor :session :agent :tool-source)))
  (ok (= 1 +unit-file-format+))
  (ok (equal +default-backoff+ '(0.5 2 30)))
  (ok (= 3 +default-max-restarts+))
  (ok (= 5 +default-grace+))
  (ok (= 2 +default-force-grace+)))

(deftest condition-classes
  (ok (subtypep 'supervisor-error 'error))
  (ok (subtypep 'supervisor-error 'supervisor-condition))
  (ok (not (subtypep 'stop-timeout 'error)) "stop-timeout is signalled, not an error")
  (ok (subtypep 'stop-timeout 'supervisor-condition))
  (dolist (c '(supervisor-closed scope-closed unit-error unit-start-error unit-limit-reached
               executor-unsupported))
    (ok (subtypep c 'supervisor-error) (format nil "~(~A~) is a supervisor-error" c)))
  (ok (subtypep 'stop-failed 'unit-error))
  (ok (subtypep 'unit-not-running 'unit-error))
  (ok (subtypep 'unit-file-unreadable 'warning))
  (let ((c (make-condition 'unit-start-error :kind :rpc :command '("x") :cause "no such program")))
    (ok (eq :rpc (unit-start-error-kind c)))
    (ok (equal '("x") (unit-start-error-command c)))
    (ok (search "no such program" (princ-to-string c)))))

(deftest kind-default-restart-policies
  (ok (eq :on-failure (kind-default-restart :rpc)))
  (ok (eq :on-failure (kind-default-restart :lsp)))
  (dolist (k '(:shell-job :image :pty :subagent :anything-else))
    (ok (eq :never (kind-default-restart k)))))

(deftest current-pid-is-this-image
  (ok (typep (current-pid) '(integer 1)))
  (ok (= (current-pid) (current-pid))))

(deftest default-inspector-per-os
  (ok (typep (default-os-inspector) (if (windows-p) 'cim-inspector 'ps-inspector))))

(deftest-parametrize restart-delay-backoff
    ((backoff attempt expected)
     ('(0.5 2 30) 1 0.5)
     ('(0.5 2 30) 2 1.0)
     ('(0.5 2 30) 3 2.0)
     ('(0.5 2 30) 6 16.0)
     ('(0.5 2 30) 7 30)
     ('(0.5 2 30) 50 30)
     ('(1 1 1) 9 1)
     ('(0.25 3 10) 3 2.25))
  (ok (< (abs (- (restart-delay backoff attempt) expected)) 1d-9)))

(deftest head-tail-buffer-construction
  (let ((b (make-head-tail-buffer :head-size 4 :tail-size 6)))
    (ok (typep b 'output-sink))
    (ok (= 4 (head-tail-buffer-head-size b)))
    (ok (= 6 (head-tail-buffer-tail-size b))))
  (let ((d (make-head-tail-buffer)))
    (ok (= +default-head-size+ (head-tail-buffer-head-size d)))
    (ok (= +default-tail-size+ (head-tail-buffer-tail-size d))))
  (ok (signals (make-head-tail-buffer :head-size -1) 'type-error)))

;;; The test programs themselves (support.lisp), checked with process-protocol
;;; only, so a failure elsewhere is never a broken fixture.

(defun ps-pgid (pid)
  (let ((*read-eval* nil))
    (read-from-string (nth-value 1 (pp:run (list "ps" "-o" "pgid=" "-p" (princ-to-string pid))
                                           :output :string :error-output nil))
                      nil nil)))

(deftest test-programs-work
  (with-temp-dir (pids)
    (let* ((h (pp:launch (tree-argv pids 2 300) :process-group t :output nil :error-output nil))
           (all (wait-pid-files pids 3)))
      (unwind-protect
           (progn
             (ok (= 3 (length all)) "tree: root + 2 grandchildren wrote their pids")
             (ok (member (pp:pid h) all) "root.pid is the direct child")
             (ok (every #'pid-alive-p all))
             (unless (windows-p)
               (ok (every (lambda (p) (eql (pp:pid h) (ps-pgid p))) all)
                   "Unix: the grandchildren stay in the root's process group")))
        (pp:kill h :force t :tree t)
        (kill-pids-quietly all))
      (ok (all-dead-p all))))
  (let ((h (pp:launch (exit-argv 3) :output nil :error-output nil)))
    (ok (eql 3 (pp:wait h :timeout 30)) "exit-argv"))
  (let ((h (pp:launch (echo-server-argv) :error-output nil)))
    (write-line "hello" (pp:stdin h)) (finish-output (pp:stdin h))
    (ok (equal "hello" (read-line (pp:stdout h) nil)) "echo server")
    (close (pp:stdin h))
    (ok (eql 0 (pp:wait h :timeout 30))))
  (let ((h (pp:launch (shell-sleep-argv 30) :process-group t :output nil :error-output nil)))
    (ok (pid-alive-p (pp:pid h)) "shell sleep")
    (os-kill (pp:pid h))
    (ok (pp:wait h :timeout 30))
    (pp:kill h :force t :tree t))
  (unless (windows-p)
    (with-temp-dir (pids)
      (let* ((h (pp:launch (posix-tree-argv pids) :process-group t :output nil :error-output nil))
             (gc (wait-pid-files pids 2)))
        (ok (= 2 (length gc)) "posix tree")
        (pp:kill h :force t :tree t)
        (ok (all-dead-p gc))))
    (let ((h (pp:launch (term-ignoring-argv 60) :process-group t :output nil :error-output nil)))
      (sleep 1)
      (pp:kill h :tree t)
      (sleep 0.5)
      (ok (pid-alive-p (pp:pid h)) "the TERM-ignoring child survives SIGTERM")
      (pp:kill h :force t :tree t)
      (ok (pp:wait h :timeout 30)))))
