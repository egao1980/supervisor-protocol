(in-package #:supervisor-protocol/tests)

;;; H8b write errors: a sink subclass whose segment streams fail (a closed
;;; stream, as a full disk or a revoked permission would), the restarts RETRY,
;;; DROP-TO-MEMORY-ONLY and ABORT-UNIT, the default, the order of handlers, and
;;; the :LOG-ERROR event.

(defclass flaky-log-sink (log-sink)
  ((failures :initarg :failures :initform 1 :accessor flaky-failures)
   (opens :initform 0 :accessor flaky-opens)))

(defmethod log-open-segment-stream ((sink flaky-log-sink) pathname)
  ;; Only segment files fail; indexes open normally.
  (unless (equal "seg" (pathname-type pathname))
    (return-from log-open-segment-stream (call-next-method)))
  (incf (flaky-opens sink))
  (if (plusp (flaky-failures sink))
      (progn
        (decf (flaky-failures sink))
        ;; A stream every write to which signals an error.
        (let ((s (call-next-method)))
          (close s)
          s))
      (call-next-method)))

(defun flaky-factory (failures)
  (lambda (u)
    (let ((sup (unit-supervisor u)))
      (make-log-sink (unit-log-pathname (supervisor-state-directory sup) (supervisor-id sup)
                                        (unit-id u))
                     :class 'flaky-log-sink :unit u :failures failures
                     :segment-size 65536 :segments 3))))

(defun log-error-events (events)
  (remove :log-error events :key #'event-type :test-not #'eq))

(deftest log-error-default-drops-to-memory-only
  (with-temp-dir (dir)
    (with-supervisor (sup :state-directory dir :output-sink-factory (flaky-factory 1000))
      (let* ((col (make-collector sup))
             (u (start-unit sup :subagent :function (writer-body '("one" "two" "three")))))
        (ok (eq :exited (wait-unit u :timeout 10)) "the unit is not killed")
        (ok (log-sink-memory-only-p (unit-output-sink u)))
        (ok (string= (format nil "one~%two~%three~%") (unit-output u)) "memory keeps everything")
        (let ((errs (log-error-events (wait-collected col (lambda (es) (log-error-events es))))))
          (ok (= 1 (length errs)) "one event: memory-only is permanent")
          (ok (equal (unit-id u) (event-unit-id (first errs))))
          (ok (eq :memory-only (getf (event-detail (first errs)) :action)))
          (ok (stringp (getf (event-detail (first errs)) :path)))
          (ok (stringp (getf (event-detail (first errs)) :message))))))))

(deftest log-error-handler-retry
  (with-temp-dir (dir)
    (let ((seen '()))
      (with-supervisor (sup :state-directory dir :output-sink-factory (flaky-factory 1)
                            :log-error-handler (lambda (c)
                                                 (push (type-of c) seen)
                                                 (invoke-restart 'retry)))
        (let* ((col (make-collector sup))
               (u (start-unit sup :subagent :function (writer-body '("one" "two")))))
          (ok (eq :exited (wait-unit u :timeout 10)))
          (ok (equal '(log-write-error) seen))
          (ok (not (log-sink-memory-only-p (unit-output-sink u))))
          (ok (equal '("one" "two") (texts (log-read u))) "nothing lost on disk")
          (ok (= 2 (flaky-opens (unit-output-sink u))) "the retry reopened the segment once")
          (let ((errs (log-error-events (wait-collected col (lambda (es) (log-error-events es))))))
            (ok (equal '(:retry) (mapcar (lambda (e) (getf (event-detail e) :action)) errs)))))))))

(deftest log-error-handler-abort-unit
  (with-temp-dir (dir)
    (let ((offered nil))
      (with-supervisor (sup :state-directory dir :output-sink-factory (flaky-factory 1000)
                          :log-error-handler (lambda (c)
                                               (setf offered (and (find-restart 'abort-unit c) t))
                                               (invoke-restart 'abort-unit)))
      (let* ((col (make-collector sup))
             (u (start-unit sup :shell-job
                            :command (lisp-argv "(progn (write-line \"x\") (finish-output) (sleep 60))"))))
        (ok (eq :killed (wait-unit u :timeout 20)) "the unit was stopped")
        (ok offered "ABORT-UNIT is offered for a sink with a unit")
        (let ((errs (log-error-events (wait-collected col (lambda (es) (log-error-events es))))))
          (ok (eq :abort-unit (getf (event-detail (first errs)) :action)))))))))

(deftest log-error-dynamic-handler-runs-first
  (with-temp-dir (dir)
    (let ((supervisor-calls 0) (seen nil))
      (with-supervisor (sup :state-directory dir :output-sink-factory (flaky-factory 1000)
                            :log-error-handler (lambda (c)
                                                 (declare (ignore c))
                                                 (incf supervisor-calls)
                                                 (invoke-restart 'retry)))
        (let* ((col (make-collector sup))
               (u (start-unit sup :subagent
                              :function (lambda (u)
                                          (declare (ignore u))
                                          (handler-bind ((log-write-error
                                                           (lambda (c)
                                                             (setf seen (log-write-error-unit c))
                                                             (invoke-restart 'drop-to-memory-only))))
                                            (write-line "in the body")
                                            (finish-output))
                                          :done))))
          (ok (eq :exited (wait-unit u :timeout 10)))
          (ok (eq u seen) "the body's handler saw it, on the unit's thread")
          (ok (= 0 supervisor-calls) "the supervisor's handler was not needed")
          (ok (log-sink-memory-only-p (unit-output-sink u)))
          (let ((errs (log-error-events (wait-collected col (lambda (es) (log-error-events es))))))
            (ok (eq :memory-only (getf (event-detail (first errs)) :action)))))))))

(deftest log-error-standalone-sink-restarts
  (with-temp-dir (dir)
    (let ((s (make-log-sink dir :class 'flaky-log-sink :failures 1))
          (restarts nil) (condition nil))
      (unwind-protect
           (progn
             (handler-bind ((log-write-error
                              (lambda (c)
                                (setf condition c
                                      restarts (list (and (find-restart 'retry c) t)
                                                     (and (find-restart 'drop-to-memory-only c) t)
                                                     (and (find-restart 'abort-unit c) t)))
                                (invoke-restart 'retry))))
               (write-lines s :stdout '("a" "b")))
             (ok (equal '(t t nil) restarts) "no ABORT-UNIT without a unit")
             (ok (eq s (log-write-error-sink condition)))
             (ok (null (log-write-error-unit condition)))
             (ok (pathnamep (log-write-error-path condition)))
             (ok (log-write-error-cause condition))
             (ok (equal '("a" "b") (texts (log-read dir)))))
        (ignore-errors (sink-close s))))
    (with-temp-dir (dir2)
      (let ((s (make-log-sink dir2 :class 'flaky-log-sink :failures 1000)))
        (ok (no-error-p (lambda () (write-lines s :stdout '("unhandled")))) "unhandled: no error")
        (ok (log-sink-memory-only-p s))
        (ok (search "unhandled" (sink-text s)))
        (ok (no-error-p (lambda () (sink-close s))) "close after memory-only: no error")))))
