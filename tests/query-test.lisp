(in-package #:supervisor-protocol/tests)

;;; The H8a fixture (HARNESS-PLAN H8a, scaled): session s1 › agent main ›
;;; tool source shell. main owns a fake stdio server (:rpc, echo); shell owns
;;; shell-job-1; the subagent scope sub-1 (under main) owns the subagent
;;; thread, which starts shell-job-2 itself (owner from *current-scope*).

(defstruct fixture sup session main shell sub job1 job2 server agent)

(defun call-with-h8a-fixture (fn &rest supervisor-keys)
  (let ((sup (apply #'make-supervisor :exit-hook nil supervisor-keys)))
    (unwind-protect
         (let* ((session (make-scope sup :session "s1"))
                (main (make-scope sup :agent "main" :parent session))
                (shell (make-scope sup :tool-source "shell" :parent main))
                (sub (make-scope sup :agent "sub-1" :parent main))
                (job1 (start-unit sup :shell-job :command (shell-sleep-argv 120) :owner shell
                                                 :labels '(:tool "bash")))
                (server (start-unit sup :rpc :command (echo-server-argv) :owner main
                                             :labels '(:role "server" :protocol "echo")))
                (job2-box nil)
                (agent (start-unit sup :subagent :owner sub
                                   :function (lambda (u)
                                               (setf job2-box
                                                     (start-unit sup :shell-job
                                                                 :command (shell-sleep-argv 120)
                                                                 :labels '(:tool "bash")))
                                               (loop until (unit-stop-requested-p u)
                                                     do (sleep 0.01))))))
           (wait-until (lambda () job2-box) :timeout 10)
           (funcall fn (make-fixture :sup sup :session session :main main :shell shell :sub sub
                                     :job1 job1 :job2 job2-box :server server :agent agent)))
      (%quiet-shutdown sup))))

(defmacro with-h8a-fixture ((var &rest supervisor-keys) &body body)
  `(call-with-h8a-fixture (lambda (,var) ,@body) ,@supervisor-keys))

(deftest fixture-shape
  (with-h8a-fixture (f)
    (ok (fixture-job2 f) "the subagent started its shell job")
    (ok (equal '("shell-job-1" "rpc-1" "subagent-1" "shell-job-2")
               (ids (supervisor-units (fixture-sup f)))))
    (ok (eq (fixture-sub f) (unit-owner (fixture-job2 f))) "owner from *current-scope*")
    (ok (equal '("s1" "main" "sub-1") (scope-path (fixture-sub f))))
    (ok (equal (list (fixture-main f)) (scope-children (fixture-session f))))
    (ok (equal (list (fixture-shell f) (fixture-sub f)) (scope-children (fixture-main f))))
    (ok (eq :on-failure (unit-restart-policy (fixture-server f))))
    (ok (eq :never (unit-restart-policy (fixture-job1 f))))
    (ok (every (lambda (u) (eq :running (unit-state u))) (supervisor-units (fixture-sup f))))))

(deftest queries-are-exact
  (with-h8a-fixture (f)
    (let ((sup (fixture-sup f)))
      (flet ((q (&rest keys) (ids (apply #'find-units sup keys))))
        (ok (equal '("shell-job-1" "shell-job-2") (q :kind :shell-job)))
        (ok (equal '("rpc-1") (q :kind :rpc)))
        (ok (equal '("rpc-1" "subagent-1") (q :kind '(:rpc :subagent))))
        (ok (equal '() (q :kind :lsp)))
        (ok (equal '("shell-job-1" "rpc-1" "subagent-1" "shell-job-2") (q :owner (fixture-main f))))
        (ok (equal '("shell-job-1" "rpc-1" "subagent-1" "shell-job-2") (q :owner (fixture-session f))))
        (ok (equal '("rpc-1") (q :owner (fixture-main f) :recursive nil)))
        (ok (equal '("subagent-1" "shell-job-2") (q :owner (fixture-sub f))))
        (ok (equal '("shell-job-1") (q :owner (fixture-shell f))))
        (ok (equal '("shell-job-2") (q :owner (fixture-sub f) :kind :shell-job)))
        (ok (equal '("rpc-1") (q :labels '(:role "server"))))
        (ok (equal '("rpc-1") (q :labels '(:role "server" :protocol "echo"))))
        (ok (equal '() (q :labels '(:role "server" :protocol "lsp"))))
        (ok (equal '("shell-job-1" "shell-job-2") (q :labels '(:tool "bash"))))
        (ok (equal '("shell-job-1" "rpc-1" "subagent-1" "shell-job-2") (q :state :running)))
        (ok (equal '() (q :state '(:exited :failed :killed))))
        (ok (equal '("shell-job-1" "rpc-1" "subagent-1" "shell-job-2") (q)))
        (ok (equal (ids (scope-units (fixture-sub f))) '("subagent-1" "shell-job-2")))
        (ok (equal (ids (scope-units (fixture-main f) :recursive nil)) '("rpc-1")))
        (ok (eq (fixture-server f) (find-unit sup "rpc-1")))))))

(deftest fake-server-round-trip
  (with-h8a-fixture (f)
    (let ((srv (fixture-server f)))
      (ok (eq t (send-unit-input srv (format nil "ping-h8a~%"))))
      (ok (wait-until (lambda () (search "ping-h8a" (unit-output srv))) :timeout 20)))))

(deftest closing-the-subagent-scope-stops-only-its-units
  (with-h8a-fixture (f)
    (let* ((sub (fixture-sub f))
           (job2-pid (unit-pid (fixture-job2 f)))
           (stopped (close-scope sub :grace 2)))
      (ok (equal '("subagent-1" "shell-job-2") (sort (ids stopped) #'string<)) "→ the units it stopped")
      (ok (eq :killed (unit-state (fixture-agent f))))
      (ok (eq :killed (unit-state (fixture-job2 f))))
      (ok (all-dead-p (list job2-pid)))
      (ok (eq :running (unit-state (fixture-job1 f))))
      (ok (eq :running (unit-state (fixture-server f))))
      (ok (pid-alive-p (unit-pid (fixture-job1 f))))
      (ok (scope-closed-p sub))
      (ok (not (scope-closed-p (fixture-main f))))
      (ok (equal '("shell-job-1" "rpc-1") (ids (find-units (fixture-sup f) :state :running))))
      (ok (equal '("subagent-1" "shell-job-2")
                 (ids (find-units (fixture-sup f) :owner sub))) "still queryable")
      (ok (null (close-scope sub)) "idempotent"))))
