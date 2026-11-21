(in-package #:supervisor-protocol/tests)

;;; supervisor-protocol#5: the process pump's read size must not decide where a
;;; line is split. A stdout-only program's output used to get an extra '+'
;;; record (one more line number) whenever a 4096-octet read ended mid-line,
;;; which depended on timing, so refs differed between runs and OSes.

(defclass paced-octet-stream (trivial-gray-streams:fundamental-binary-input-stream)
  ((octets :initarg :octets)
   (pos :initform 0)
   (pauses :initarg :pauses :initform '()
           :documentation "Positions where LISTEN says nothing is waiting (the writer paused).")
   (on-pause :initarg :on-pause :initform nil
             :documentation "Called with the position when a read resumes after a pause."))
  (:documentation "Octets as a pipe delivers them: LISTEN is true except at a pause."))

(defmethod trivial-gray-streams:stream-read-byte ((s paced-octet-stream))
  (with-slots (octets pos pauses on-pause) s
    (cond ((>= pos (length octets)) :eof)
          (t (when (and on-pause (member pos pauses)) (funcall on-pause pos))
             (prog1 (aref octets pos) (incf pos))))))

(defmethod trivial-gray-streams:stream-listen ((s paced-octet-stream))
  (with-slots (octets pos pauses) s
    (and (< pos (length octets)) (not (member pos pauses)))))

(defun build-output-octets (n)
  "What `echo` prints for N cc lines, an error line after the 120th, and make's last line."
  (apply #'octets
         (append (loop for i from 1 to n
                       collect (line-octets (format nil "cc -O2 -c -o build/mod~D.o src/mod~D.c" i i))
                       when (= i 120)
                         collect (line-octets "src/mod120.c:14:5: error: 'count' undeclared (first use in this function)"))
                 (list (line-octets "make: *** [all] Error 1")))))

(defun line-starts (octets)
  (cons 0 (loop for i from 0 below (1- (length octets))
                when (= (aref octets i) +lf+) collect (1+ i))))

(defun pumped-records (octets &key chunk pauses on-pause)
  "Pump OCTETS through a log sink with buffer CHUNK; → the output records as
(line continued-p text)."
  (with-temp-dir (dir)
    (let ((records nil))
      (with-log-sink (sink dir)
        (supervisor-protocol::%pump
         (make-instance 'paced-octet-stream :octets octets :pauses pauses
                                            :on-pause (and on-pause (lambda (pos) (funcall on-pause sink pos))))
         sink :stdout :chunk chunk)
        (sink-close sink)
        (setf records (mapcar (lambda (r) (list (log-record-line r) (log-record-continued-p r)
                                                (log-record-text r)))
                              (log-read sink))))
      records)))

(deftest pump-line-numbers-depend-only-on-the-bytes
  (let* ((bytes (build-output-octets 200))
         (starts (line-starts bytes))
         (state (sb-ext:seed-random-state 5))
         (reference (pumped-records bytes :chunk 16384)))
    (ok (= 202 (length reference)) "one record per printed line")
    (ok (notany #'second reference) "no continuation records")
    (ok (equal '(122 nil "src/mod120.c:14:5: error: 'count' undeclared (first use in this function)")
               (nth 120 reference))
        "the error line is line 122 (header = 1)")
    ;; Reads cut by buffers of every size, with the writer pausing at random line
    ;; ends (one write per line, as echo does): the same records every time.
    (dolist (chunk '(100 128 333 1000 4096))
      (dotimes (trial 3)
        (let ((pauses (loop for p in (rest starts) when (< (random 1.0 state) 0.05) collect p)))
          (ok (equal reference (pumped-records bytes :chunk chunk :pauses pauses))
              (format nil "chunk ~D, ~D pauses: same lines" chunk (length pauses))))))))

(deftest pump-hands-on-a-partial-line-at-a-pause
  ;; A prompt without an LF is visible as soon as the writer pauses after it.
  (let* ((bytes (octets "Password: " (line-octets "hunter2") (line-octets "ok")))
         (seen nil)
         (records (pumped-records bytes :chunk 16 :pauses '(10)
                                        :on-pause (lambda (sink pos)
                                                    (declare (ignore pos))
                                                    (setf seen (texts (log-read sink)))))))
    (ok (equal '("Password: ") seen) "the prompt was written before the pump read on")
    (ok (equal '((2 t "Password: ") (3 nil "hunter2") (4 nil "ok")) records)))
  ;; A full buffer without an LF goes on whole (no unbounded carry).
  (let* ((long (make-string 40 :initial-element #\x))
         (records (pumped-records (line-octets long) :chunk 16)))
    (ok (equal long (apply #'concatenate 'string (mapcar #'third records))))
    (ok (every #'second (butlast records)))
    (ok (not (second (car (last records)))))))

(deftest-unix pump-stdout-only-shell-job-has-one-line-per-printed-line
  ;; The H8c demo's build.sh shape: 200 echo lines, then make's error. Before
  ;; #5 a 4096-octet read ending mid-line added a '+' record now and then.
  (dotimes (run 5)
    (with-temp-dir (dir)
      (with-supervisor (sup :state-directory dir)
        (let ((u (start-unit sup :shell-job
                             :command (list "sh" "-c" "for i in $(seq 1 200); do echo \"cc -O2 -c -o build/mod$i.o src/mod$i.c\"; done; echo 'make: *** [all] Error 1'; exit 2"))))
          (wait-unit u :timeout 60)
          (ok (wait-until (lambda () (log-closed-p u)) :timeout 20))
          (let ((records (log-read u)))
            (ok (= 201 (length records)) (format nil "run ~D: 201 output records" run))
            (ok (notany #'log-record-continued-p records) (format nil "run ~D: no continuation" run))
            (ok (= 203 (log-last-line u)) (format nil "run ~D: header + 201 lines + end" run))))))))
