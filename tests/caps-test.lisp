(in-package #:supervisor-protocol/tests)

;;; Caps per kind and per owner; LRU eviction (finished first, never interacting).

(defun live-job (sup &rest keys)
  (apply #'start-unit sup :subagent :function (cooperative-body) keys))

(defun done-job (sup)
  (let ((u (start-unit sup :subagent :function (lambda (u) (declare (ignore u)) t))))
    (wait-unit u :timeout 5)
    u))

(defun invoking (restart)
  "A handler that invokes RESTART."
  (lambda (c) (invoke-restart (find-restart restart c))))

(deftest cap-evicts-lru-finished-silently
  (with-supervisor (sup :kind-caps '(:subagent 2))
    (let* ((c (make-collector sup))
           (a (done-job sup))
           (b (done-job sup)))
      (touch-unit a)
      (let ((n (live-job sup)))
        (ok (string= "subagent-3" (unit-id n)))
        (ok (null (find-unit sup (unit-id b))) "b was least recently used")
        (ok (eq a (find-unit sup (unit-id a))))
        (ok (= 2 (length (find-units sup :kind :subagent))))
        (let ((ev (find :evicted (wait-collected c (lambda (es) (find :evicted es :key #'event-type)))
                        :key #'event-type)))
          (ok (string= (unit-id b) (event-unit-id ev)))
          (ok (eq :exited (event-from ev)))
          (ok (null (event-to ev))))))))

(deftest cap-other-kinds-unaffected
  (with-supervisor (sup :kind-caps '(:subagent 1))
    (live-job sup)
    (ok (start-unit sup :shell-job :function (cooperative-body)) "no cap on :shell-job")
    (ok (= 2 (length (supervisor-units sup))))))

(deftest cap-with-only-live-units-signals
  (with-supervisor (sup :kind-caps '(:subagent 2))
    (let ((a (live-job sup)) (b (live-job sup)))
      (touch-unit b)
      (let ((c (handler-case (progn (live-job sup) nil) (unit-limit-reached (c) c))))
        (ok c "unit-limit-reached")
        (ok (eq :subagent (unit-limit-reached-kind c)))
        (ok (= 2 (unit-limit-reached-limit c)))
        (ok (null (unit-limit-reached-owner c)))
        (ok (eq a (unit-limit-reached-victim c)) "the least recently used live unit"))
      (ok (signals (live-job sup) 'unit-limit-reached) "unhandled: an error")
      (ok (eq :running (unit-state a)))
      (ok (= 2 (length (supervisor-units sup)))))))

(deftest cap-restart-evict-lru
  (with-supervisor (sup :kind-caps '(:subagent 2))
    (let* ((a (live-job sup)) (b (live-job sup))
           (n (handler-bind ((unit-limit-reached (invoking 'evict-lru)))
                (live-job sup))))
      (ok (typep n 'unit))
      (ok (string= "subagent-3" (unit-id n)))
      (ok (eq :killed (unit-state a)) "the victim was stopped")
      (ok (null (find-unit sup (unit-id a))) "and evicted")
      (ok (eq :running (unit-state b))))))

(deftest cap-restart-abort
  (with-supervisor (sup :kind-caps '(:subagent 1))
    (let ((a (live-job sup)))
      (ok (null (handler-bind ((unit-limit-reached (invoking 'abort)))
                  (live-job sup)))
          "start-unit → NIL")
      (ok (eq :running (unit-state a)))
      (ok (= 1 (length (supervisor-units sup))))
      (stop-unit a)
      (ok (string= "subagent-2" (unit-id (live-job sup))) "the refused start consumed no id"))))

(deftest cap-never-evicts-interacting
  (with-supervisor (sup :kind-caps '(:subagent 2))
    (let ((a (live-job sup :interacting t)) (b (live-job sup)))
      (ok (unit-interacting-p a))
      (let ((c (handler-case (progn (live-job sup) nil) (unit-limit-reached (c) c))))
        (ok (eq b (unit-limit-reached-victim c)) "a is mid-interaction, so b"))
      (with-unit-interaction (b)
        (ok (unit-interacting-p b))
        (let ((c (handler-case (progn (live-job sup) nil) (unit-limit-reached (c) c))))
          (ok (null (unit-limit-reached-victim c)))
          (ok (null (find-restart 'evict-lru c)) "no victim: no evict-lru restart")
          (ok (find-restart 'abort c))))
      (ok (not (unit-interacting-p b)) "restored"))))

(deftest cap-finished-interacting-is-still-finished
  (with-supervisor (sup :kind-caps '(:subagent 1))
    (let ((a (done-job sup)))
      (setf (unit-interacting-p a) t)
      (ok (signals (live-job sup) 'unit-limit-reached) "an interacting unit is never evicted, even finished")
      (setf (unit-interacting-p a) nil)
      (ok (live-job sup)))))

(deftest owner-cap
  (with-supervisor (sup :owner-cap 1)
    (let* ((x (make-scope sup :agent "x"))
           (y (make-scope sup :agent "y"))
           (inner (make-scope sup :tool-source "t" :parent x)))
      (live-job sup :owner x)
      (let ((c (handler-case (progn (live-job sup :owner x) nil) (unit-limit-reached (c) c))))
        (ok c)
        (ok (eq x (unit-limit-reached-owner c)))
        (ok (= 1 (unit-limit-reached-limit c))))
      (ok (live-job sup :owner y) "another scope")
      (ok (live-job sup :owner inner) "the cap counts direct ownership"))))

(deftest lru-is-touched-by-use
  (with-supervisor (sup)
    (let* ((a (live-job sup)) (b (live-job sup)) (t0 (unit-last-used b)))
      (ok (< (unit-last-used a) t0) "start touches")
      (touch-unit a)
      (ok (> (unit-last-used a) t0))
      (let ((t1 (unit-last-used a)))
        (unit-output b)
        (ok (> (unit-last-used b) t1) "unit-output touches")))))

(deftest cap-on-processes
  (with-supervisor (sup :kind-caps '(:shell-job 1))
    (let* ((a (start-unit sup :shell-job :command (sleeper-argv 60)))
           (pid (unit-pid a))
           (b (handler-bind ((unit-limit-reached (invoking 'evict-lru)))
                (start-unit sup :shell-job :command (sleeper-argv 60)))))
      (ok (typep b 'unit))
      (ok (all-dead-p (list pid)) "the evicted process is gone")
      (ok (equal (list b) (supervisor-units sup))))))
