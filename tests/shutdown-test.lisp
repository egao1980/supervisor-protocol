(in-package #:supervisor-protocol/tests)

;;; Supervisor shutdown and the image exit hook.

(deftest shutdown-stops-everything
  (with-temp-dir (dir)
    (with-temp-dir (pids)
      (let* ((sup (make-supervisor :exit-hook nil :state-directory dir))
             (s (make-scope sup :session "s"))
             (job (start-unit sup :shell-job :command (tree-argv pids 2 300) :owner s))
             (th (start-unit sup :subagent :function (cooperative-body)))
             (all (wait-pid-files pids 3)))
        (multiple-value-bind (secs r) (elapsed (lambda () (shutdown-supervisor sup :grace 3)))
          (ok (null r))
          (ok (< secs 4)))
        (ok (supervisor-closed-p sup))
        (ok (eq :killed (unit-state job)))
        (ok (eq :killed (unit-state th)))
        (ok (all-dead-p all))
        (ok (null (units-dir-files dir)) "unit files deleted")
        (ok (scope-closed-p s))
        (ok (equal (list job th) (supervisor-units sup)) "queries still answer")))))

(deftest exit-hook-registration
  (let ((sup (make-supervisor)))
    (unwind-protect
         (let ((hook (supervisor-exit-hook sup)))
           (ok (functionp hook))
           #+sbcl (ok (member hook sb-ext:*exit-hooks*))
           (shutdown-supervisor sup)
           #+sbcl (ok (not (member hook sb-ext:*exit-hooks*)) "removed on shutdown"))
      (%quiet-shutdown sup)))
  (let ((sup (make-supervisor :exit-hook nil)))
    (ok (null (supervisor-exit-hook sup)))
    (shutdown-supervisor sup)))

(deftest exit-hook-stops-units-when-the-image-exits
  ;; Claude Code: background shells survived the end of the session.
  (with-temp-dir (state)
    (with-temp-dir (pids)
      (let* ((child (launch-child-image
                     (child-units-form state pids :exit-hook t :then "(sb-ext:exit :code 0)")))
             (line (child-ready-line child))
             (pids-seen '()))
        (unwind-protect
             (progn
               (ok line)
               (when line
                 (setf pids-seen (list* (second (ready-pids line)) (read-pid-files pids)))
                 (ok (= 4 (length pids-seen)))
                 (ok (eql 0 (pp:wait (child-handle child) :timeout 60)) "the child exited normally")
                 (ok (all-dead-p pids-seen) "the exit hook stopped every unit and its tree")
                 (ok (null (units-dir-files state)))))
          (kill-pids-quietly pids-seen)
          (ignore-errors (pp:kill (child-handle child) :force t)))))))
