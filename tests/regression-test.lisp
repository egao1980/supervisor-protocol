(in-package #:supervisor-protocol/tests)

;;; Regression cases named after Claude Code background-task bugs (session
;;; end and Windows tree kill are also covered by shutdown-test and stop-test).

(deftest claude-code-status-never-stale-after-exit
  ;; A task kept showing "running" after its process had exited.
  (with-supervisor (sup)
    (let ((u (start-unit sup :shell-job :command (exit-argv 0 :delay 0.5))))
      (ok (eq :running (unit-state u)))
      (ok (wait-until (lambda () (eq :exited (unit-state u))) :timeout 15)
          "no wait-unit, no output read: the state still moves")
      (ok (eql 0 (unit-exit-code u)))
      (ok (not (member u (find-units sup :state :running)))))))

(deftest claude-code-status-never-stale-after-external-kill
  (with-supervisor (sup)
    (let* ((u (start-unit sup :shell-job :command (sleeper-argv 60)))
           (pid (unit-pid u)))
      (os-kill pid)
      (multiple-value-bind (secs ended)
          (elapsed (lambda () (wait-until (lambda () (eq :failed (unit-state u))) :timeout 10)))
        (ok ended)
        (ok (< secs 2) "within the second")))))

(deftest claude-code-status-never-stale-for-threads
  (with-supervisor (sup)
    (let ((u (start-unit sup :subagent :function (lambda (u) (declare (ignore u)) (sleep 0.2) :done))))
      (ok (wait-until (lambda () (eq :exited (unit-state u))) :timeout 3)))))

(deftest claude-code-scope-close-on-interrupt
  ;; Interrupting the agent (Esc) left its subagents and shells running.
  (with-supervisor (sup)
    (let* ((units '()) (lock (bt2:make-lock)) (scope nil)
           (worker (spawn
                    (lambda ()
                      (catch 'interrupted
                        (with-scope (s sup :agent "turn")
                          (setf scope s)
                          (bt2:with-lock-held (lock)
                            (push (start-unit sup :shell-job :command (sleeper-argv 60)) units)
                            (push (start-unit sup :subagent :function (cooperative-body)) units))
                          (sleep 60))))
                    :name "agent-turn")))
      (ok (wait-until (lambda () (bt2:with-lock-held (lock) (= 2 (length units))))))
      (let ((pid (unit-pid (find :shell-job units :key #'unit-kind))))
        (bt2:interrupt-thread worker (lambda () (throw 'interrupted nil)))
        (bt2:join-thread worker)
        (ok (scope-closed-p scope))
        (ok (every (lambda (u) (eq :killed (unit-state u))) units))
        (ok (all-dead-p (list pid)))))))

(deftest claude-code-units-stay-tracked-after-compaction
  ;; After a context compaction the agent lost track of its background shells.
  ;; The registry does not depend on anyone holding the unit objects.
  (with-supervisor (sup)
    (let ((s (make-scope sup :agent "main")) (pids '()))
      (dotimes (i 3)
        (push (unit-pid (start-unit sup :shell-job :command (sleeper-argv 60) :owner s)) pids))
      #+sbcl (sb-ext:gc :full t)
      (ok (= 3 (length (find-units sup :owner s :state :running))))
      (ok (find-unit sup "shell-job-2"))
      (close-scope s :grace 3)
      (ok (all-dead-p pids)))))

(deftest claude-code-windows-tree-kill
  ;; On Windows, KillShell killed cmd.exe and left its children running.
  ;; Runs on every OS: a shell job whose child starts grandchildren.
  (with-temp-dir (pids)
    (with-supervisor (sup)
      (let* ((u (start-unit sup :shell-job :command (tree-argv pids 2 300)))
             (all (wait-pid-files pids 3)))
        (ok (= 3 (length all)))
        (stop-unit u :grace 2)
        (ok (all-dead-p all) "the whole tree, not just the direct child")))))
