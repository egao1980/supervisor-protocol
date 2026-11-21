(in-package #:supervisor-protocol/tests)

;;; Owner scopes: session › agent › tool source.

(deftest scope-tree
  (with-supervisor (sup)
    (let* ((s (make-scope sup :session "s1"))
           (a (make-scope sup :agent "a1" :parent s))
           (b (make-scope sup :agent "a2" :parent s))
           (custom (make-scope sup :workflow "w" :parent a)))
      (ok (eq (supervisor-root-scope sup) (scope-parent s)) "default parent: the root")
      (ok (eq sup (scope-supervisor a)))
      (ok (eq :agent (scope-kind a)))
      (ok (string= "a1" (scope-name a)))
      (ok (equal (list a b) (scope-children s)))
      (ok (equal '("s1" "a1" "w") (scope-path custom)))
      (ok (eq :workflow (scope-kind custom)) "open set of kinds")
      (ok (equal '() (scope-path (supervisor-root-scope sup))))
      (ok (not (scope-closed-p s))))))

(deftest current-scope-is-the-default
  (with-supervisor (sup)
    (with-scope (s sup :session "s1")
      (ok (eq s *current-scope*))
      (with-scope (a sup :agent "a1")
        (ok (eq s (scope-parent a)) "parent from *current-scope*")
        (let ((u (start-unit sup :subagent :function (cooperative-body))))
          (ok (eq a (unit-owner u)) "owner from *current-scope*"))))
    (with-supervisor (other)
      (with-scope (s other :session "x")
        (ok (eq s *current-scope*))
        (let ((u (start-unit sup :subagent :function (cooperative-body))))
          (ok (eq (supervisor-root-scope sup) (unit-owner u))
              "*current-scope* of another supervisor is ignored"))))))

(deftest with-scope-closes-on-exit
  (with-supervisor (sup)
    (let (scope unit)
      (with-scope (s sup :agent "a")
        (setf scope s
              unit (start-unit sup :subagent :function (cooperative-body))))
      (ok (scope-closed-p scope))
      (ok (eq :killed (unit-state unit)))
      (setf unit nil)
      (ignore-errors
       (with-scope (s sup :agent "b")
         (setf scope s
               unit (start-unit sup :subagent :function (cooperative-body)))
         (error "tool failed")))
      (ok (scope-closed-p scope) "closed on a non-local exit")
      (ok (eq :killed (unit-state unit))))))

(deftest closed-scope-refuses
  (with-supervisor (sup)
    (let ((s (make-scope sup :session "s")))
      (close-scope s)
      (ok (signals (start-unit sup :subagent :function (cooperative-body) :owner s) 'scope-closed))
      (ok (signals (make-scope sup :agent "late" :parent s) 'scope-closed))
      (ok (signals (close-scope (supervisor-root-scope sup)) 'supervisor-error)
          "the root closes with shutdown-supervisor"))))

(deftest closing-a-parent-closes-descendants
  (with-supervisor (sup)
    (let* ((s (make-scope sup :session "s"))
           (a (make-scope sup :agent "a" :parent s))
           (tsrc (make-scope sup :tool-source "t" :parent a))
           (other (make-scope sup :session "other"))
           (u1 (start-unit sup :subagent :owner a :function (cooperative-body)))
           (u2 (start-unit sup :shell-job :owner tsrc :command (sleeper-argv 60)))
           (keep (start-unit sup :subagent :owner other :function (cooperative-body))))
      (multiple-value-bind (secs stopped) (elapsed (lambda () (close-scope s :grace 3)))
        (ok (= 2 (length stopped)))
        (ok (< secs 3) "one wave, no per-unit grace"))
      (ok (every #'scope-closed-p (list s a tsrc)))
      (ok (eq :killed (unit-state u1)))
      (ok (eq :killed (unit-state u2)))
      (ok (eq :running (unit-state keep)))
      (ok (not (scope-closed-p other))))))

(deftest close-scope-stops-in-one-wave
  (with-supervisor (sup)
    (let* ((s (make-scope sup :agent "slow"))
           (units (loop repeat 4
                        collect (start-unit sup :subagent :owner s
                                            :function (lambda (u) (declare (ignore u)) (sleep 60))))))
      (multiple-value-bind (secs) (elapsed (lambda () (close-scope s :grace 0.5 :force-grace 2)))
        (ok (< secs 2) "4 uncooperative threads: one grace, then force — not 4 graces"))
      (ok (every (lambda (u) (eq :killed (unit-state u))) units)))))
