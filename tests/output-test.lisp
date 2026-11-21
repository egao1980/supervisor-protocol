(in-package #:supervisor-protocol/tests)

;;; The head + tail buffer (the H8b attach point is the sink factory).

(defun marker (n) (format nil "~%[~D characters dropped]~%" n))

(deftest head-tail-small-output-kept-whole
  (let ((b (make-head-tail-buffer :head-size 4 :tail-size 6)))
    (sink-write b :stdout "abc")
    (sink-write b :stderr "def")
    (ok (string= "abcdef" (sink-text b)))
    (ok (equal '(6 0) (multiple-value-list (sink-stats b))))))

(deftest head-tail-exactly-full-drops-nothing
  (let ((b (make-head-tail-buffer :head-size 4 :tail-size 6)))
    (sink-write b :stdout "0123456789")
    (ok (string= "0123456789" (sink-text b)))
    (ok (equal '(10 0) (multiple-value-list (sink-stats b))))))

(deftest head-tail-overflow-keeps-head-marker-tail
  (let ((b (make-head-tail-buffer :head-size 4 :tail-size 6)))
    (sink-write b :stdout "0123456789")
    (sink-write b :stdout "abcdefghij")
    (ok (string= (concatenate 'string "0123" (marker 10) "efghij") (sink-text b)))
    (ok (equal '(20 10) (multiple-value-list (sink-stats b))))))

(deftest head-tail-many-small-writes
  (let ((b (make-head-tail-buffer :head-size 3 :tail-size 3)))
    (loop for c across "abcdefghijklmnopqrstuvwxyz"
          do (sink-write b (if (evenp (char-code c)) :stdout :stderr) (string c)))
    (ok (string= (concatenate 'string "abc" (marker 20) "xyz") (sink-text b)))
    (ok (equal '(26 20) (multiple-value-list (sink-stats b))))))

(deftest head-tail-zero-sizes
  (let ((b (make-head-tail-buffer :head-size 0 :tail-size 2)))
    (sink-write b :stdout "hello")
    (ok (string= (concatenate 'string (marker 3) "lo") (sink-text b)))))

(deftest head-tail-close-ignores-later-writes
  (let ((b (make-head-tail-buffer :head-size 10 :tail-size 10)))
    (sink-write b :stdout "kept")
    (ok (null (sink-close b)))
    (ok (null (sink-close b)) "idempotent")
    (sink-write b :stdout "ignored")
    (ok (string= "kept" (sink-text b)))))

(deftest head-tail-concurrent-writers
  (let* ((b (make-head-tail-buffer :head-size 100000 :tail-size 100000))
         (threads (loop for i below 8
                        collect (spawn
                                 (lambda () (dotimes (k 1000) (sink-write b :stdout "x")))))))
    (mapc #'bt2:join-thread threads)
    (ok (= 8000 (length (sink-text b))))
    (ok (equal '(8000 0) (multiple-value-list (sink-stats b))))))

(deftest custom-sink-factory-is-used
  (let ((made '()))
    (with-supervisor (sup :output-sink-factory
                          (lambda (u)
                            (push (unit-id u) made)
                            (make-head-tail-buffer :head-size 2 :tail-size 2)))
      (let ((u (start-unit sup :subagent
                           :function (lambda (u)
                                       (sink-write (unit-output-sink u) :stdout "abcdefgh")
                                       :ok))))
        (wait-unit u :timeout 5)
        (ok (equal (list (unit-id u)) made))
        (ok (string= (concatenate 'string "ab" (marker 4) "gh") (unit-output u)))))))
