(in-package #:supervisor-protocol/search)

;;; Structured hit kinds (+TOKENIZER-VERSION+ 1). The brief's "Kinds" table is
;;; the specification; tests/search-kinds-test.lisp locks it. All rules look at
;;; the line after STRIP-ANSI. "Trimmed" = without leading spaces and tabs.
;;;
;;; Word set W of a line: the words and sub-words of TOKENIZE's rules 2, 5 and 6
;;; (lowercased, no length filter), except that a path-like span (one with / or
;;; \, or with a . between a word character and a letter) contributes only the
;;; words of its last component (the text after its last / \ or .). In order.
;;;
;;; :ERROR    W contains error, err, fatal, panic, panicked, exception or critical,
;;;           not directly preceded in W by no, 0, zero or without.
;;; :WARNING  W contains warning, warn, deprecated or deprecation, with the same
;;;           exclusion.
;;; :TEST-FAILURE, trimmed line:
;;;   Rove     starts with "× " (U+00D7) then digits then ")"
;;;   pytest   starts with "FAILED " and contains "::"; or contains "::" and,
;;;            after it, the word FAILED (uppercase, whole word)
;;;   jest     starts with "✕ " (U+2715), or starts with "● " (U+25CF) and contains " › "
;;;   cargo    starts with "test " and ends with " ... FAILED" (trailing whitespace ignored)
;;;   go       starts with "--- FAIL: "
;;;   generic  starts with "FAIL" followed by a space or a tab
;;; :STACK-TRACE (several lines; the kind goes on the trace's first line):
;;;   Python   a trimmed line starting with "Traceback (most recent call last):"
;;;            starts it; following lines that start with a space or tab belong
;;;            to it; the first non-blank line that does not ends it and belongs
;;;            to it (the exception line); a blank line ends it without belonging.
;;;   frames   a frame line is "<spaces><digits>: <text>". A run of two or more
;;;            frame lines numbered 0, 1, 2, … is a trace; lines that start with
;;;            whitespace followed by "at " inside the run continue it (Rust).
;;;            It starts at the line before frame 0 when that line contains
;;;            "Backtrace for:" or its trimmed text is "stack backtrace:" (SBCL
;;;            debugger, Rust), else at frame 0 (SBCL / Rove backtraces).
;;;   at-lines an at-line is whitespace, "at ", then a non-space (JS, Java). A
;;;            run of two or more at-lines (also continued by trimmed lines starting with
;;;            "Caused by: " or "... " digits " more") is a trace. It starts at
;;;            the line before the run when that line is not blank and does not
;;;            start with whitespace (the "TypeError: …" header), else at the
;;;            first at-line.
;;;   A line belongs to at most one trace; the first rule to claim it wins.
;;;   The index classifies each stream of a log separately (its lines in line
;;;   order, across segments), so interleaved stdout does not break a stderr trace.
;;;   Frame and at-lines keep their own line kinds (:ERROR etc.).
;;; :EXIT is not a line rule: it is a log's end record (see SEARCH-LOGS).


;;; ------------------------------------------------------------------
;;; One-line kinds

(defparameter *error-words* '("error" "err" "fatal" "panic" "panicked" "exception" "critical"))
(defparameter *warning-words* '("warning" "warn" "deprecated" "deprecation"))
(defparameter *negating-words* '("no" "0" "zero" "without"))

(defun %path-like-p (text start end)
  "A span with a slash, or with a . between a word character and a letter."
  (loop for i from start below end
        for c = (char text i)
        thereis (or (char= c #\/)
                    (and (char= c #\.) (> i start) (< (1+ i) end)
                         (%word-char-p (char text (1- i)))
                         (alpha-char-p (char text (1+ i)))))))

(defun %word-set (prepared)
  "W: the words and sub-words of PREPARED (STRIP-ANSI'd, backslashes as slashes),
lowercased, in order; a path-like span gives only its last component's."
  (let ((w '()))
    (%map-spans (lambda (s e)
                  (when (%path-like-p prepared s e)
                    (setf s (1+ (or (position-if (lambda (c) (member c '(#\/ #\.)))
                                                 prepared :start s :end e :from-end t)
                                    (1- s)))))
                  (dolist (word (%word-runs prepared s e))
                    (push (string-downcase word) w)
                    (let ((parts (%sub-words word)))
                      (when (>= (length parts) 2)
                        (dolist (p parts) (push (string-downcase p) w))))))
                prepared)
    (nreverse w)))

(defun %word-kind-p (w words)
  "Some member of WORDS occurs in W not right after a negating word."
  (loop for (prev . rest) on (cons nil w)
        for word = (car rest)
        while rest
        thereis (and (member word words :test #'string=)
                     (not (and prev (member prev *negating-words* :test #'string=))))))

(defun %blank-char-p (c) (or (char= c #\Space) (char= c #\Tab)))

(defun %left-trim (s) (string-left-trim '(#\Space #\Tab) s))

(defun %starts-with-p (prefix s &key (start 0))
  (let ((end (+ start (length prefix))))
    (and (<= end (length s)) (string= prefix s :start2 start :end2 end))))

(defun %whole-word-at-p (s pos len)
  (and (or (zerop pos) (not (%word-char-p (char s (1- pos)))))
       (or (= (+ pos len) (length s)) (not (%word-char-p (char s (+ pos len)))))))

(defun %test-failure-p (stripped)
  (let* ((s (%left-trim stripped)) (n (length s)))
    (or
     ;; Rove: "× <digits>)"
     (and (%starts-with-p (format nil "~C " (code-char #xd7)) s)
          (let ((i 2))
            (and (< i n) (digit-char-p (char s i))
                 (progn (loop while (and (< i n) (digit-char-p (char s i))) do (incf i))
                        (and (< i n) (char= (char s i) #\)))))))
     ;; pytest
     (and (%starts-with-p "FAILED " s) (search "::" s) t)
     (let ((cc (search "::" s)))
       (and cc
            (loop for p = (search "FAILED" s :start2 (+ cc 2)) then (search "FAILED" s :start2 (1+ p))
                  while p
                  thereis (%whole-word-at-p s p 6))))
     ;; jest
     (%starts-with-p (format nil "~C " (code-char #x2715)) s)
     (and (%starts-with-p (format nil "~C " (code-char #x25cf)) s)
          (search (format nil " ~C " (code-char #x203a)) s) t)
     ;; cargo
     (and (%starts-with-p "test " s)
          (let ((r (string-right-trim '(#\Space #\Tab #\Return #\Newline) s)))
            (and (>= (length r) 11) (string= " ... FAILED" r :start2 (- (length r) 11)))))
     ;; go
     (%starts-with-p "--- FAIL: " s)
     ;; generic
     (and (> n 4) (%starts-with-p "FAIL" s) (%blank-char-p (char s 4))))))

(defun %line-kinds (stripped)
  "LINE-KINDS of an already STRIP-ANSI'd line."
  (let ((w (%word-set (substitute #\/ #\\ stripped))))
    (append (and (%word-kind-p w *error-words*) '(:error))
            (and (%word-kind-p w *warning-words*) '(:warning))
            (and (%test-failure-p stripped) '(:test-failure)))))

(defun line-kinds (text)
  "→ the one-line kinds of TEXT (:ERROR :WARNING :TEST-FAILURE), in +HIT-KINDS+
order. Stateless; :STACK-TRACE needs CLASSIFY-LINES."
  (check-type text string)
  (%line-kinds (strip-ansi text)))

;;; ------------------------------------------------------------------
;;; Stack traces

(defun %blank-line-p (s) (every #'%blank-char-p s))

(defun %indented-p (s) (and (plusp (length s)) (%blank-char-p (char s 0))))

(defun %traceback-p (s)
  (%starts-with-p "Traceback (most recent call last):" (%left-trim s)))

(defun %frame-number (s)
  "N for a frame line \"<spaces><digits>: <text>\", else NIL."
  (let* ((n (length s))
         (i (or (position-if-not #'%blank-char-p s) n))
         (d i))
    (loop while (and (< d n) (digit-char-p (char s d))) do (incf d))
    (and (< i d) (< (1+ d) n) (char= (char s d) #\:) (char= (char s (1+ d)) #\Space)
         (parse-integer s :start i :end d))))

(defun %frame-header-p (s)
  (or (search "Backtrace for:" s)
      (string= "stack backtrace:" (string-trim '(#\Space #\Tab) s))))

(defun %at-line-p (s)
  "Whitespace, \"at \", then a non-space."
  (let* ((n (length s))
         (i (or (position-if-not #'%blank-char-p s) n)))
    (and (plusp i) (%starts-with-p "at " s :start i)
         (< (+ i 3) n) (not (%blank-char-p (char s (+ i 3)))))))

(defun %at-continuation-p (s)
  (let ((tr (%left-trim s)))
    (or (%starts-with-p "Caused by: " tr)
        (and (%starts-with-p "... " tr)
             (let* ((i 4) (d i) (n (length tr)))
               (loop while (and (< d n) (digit-char-p (char tr d))) do (incf d))
               (and (< i d) (%starts-with-p " more" tr :start d)))))))

(defun %classify-traces (texts start end)
  "Stack traces among the STRIP-ANSI'd lines TEXTS[START, END) (a vector), the
input ending at END. → (values TRACES PENDING): TRACES a list of (FIRST . LAST)
indices in order; PENDING the first index whose outcome could still change if
lines were appended after END (END when none). Classifying again from PENDING
with more lines gives the same result as classifying everything."
  (let ((traces '()) (pending end) (i start))
    (labels ((line (j) (aref texts j))
             (touch () (setf pending (min pending i)))
             (python ()
               (when (%traceback-p (line i))
                 (let ((j (1+ i)))
                   (loop
                     (cond ((>= j end) (touch) (return (1- j)))
                           ((%indented-p (line j)) (incf j))
                           ((%blank-line-p (line j)) (return (1- j)))
                           (t (return j)))))))
             (frames-from (f)
               (let ((k 1) (frames 1) (j (1+ f)) (last f))
                 (loop
                   (when (>= j end) (touch) (return))
                   (let ((num (%frame-number (line j))))
                     (cond ((eql num k) (incf k) (incf frames) (setf last j) (incf j))
                           ((%at-line-p (line j)) (setf last j) (incf j))
                           (t (return)))))
                 (and (>= frames 2) last)))
             (frames ()
               (cond ((eql 0 (%frame-number (line i))) (frames-from i))
                     ((%frame-header-p (line i))
                      (cond ((>= (1+ i) end) (touch) nil)
                            ((eql 0 (%frame-number (line (1+ i)))) (frames-from (1+ i)))))))
             (at-run-from (f)
               (let ((count 1) (j (1+ f)) (last f))
                 (loop
                   (when (>= j end) (touch) (return))
                   (cond ((%at-line-p (line j)) (incf count) (setf last j) (incf j))
                         ((%at-continuation-p (line j)) (setf last j) (incf j))
                         (t (return))))
                 (and (>= count 2) last)))
             (at-lines ()
               (let ((s (line i)))
                 (cond ((%at-line-p s) (at-run-from i))
                       ((or (%blank-line-p s) (%indented-p s)) nil)
                       ((>= (1+ i) end) (touch) nil)
                       ((%at-line-p (line (1+ i))) (at-run-from (1+ i)))))))
      (loop while (< i end)
            do (let ((last (or (python) (frames) (at-lines))))
                 (cond (last (push (cons i last) traces) (setf i (1+ last)))
                       (t (incf i))))))
    (values (nreverse traces) pending)))

(defun classify-lines (texts)
  "TEXTS: a list of line strings (consecutive lines of one stream or log). →
a list of (INDEX KINDS END) for each line that has at least one kind, in order:
INDEX 1-based into TEXTS, KINDS in +HIT-KINDS+ order (LINE-KINDS plus
:STACK-TRACE on a trace's first line), END the 1-based index of the trace's last
line when KINDS has :STACK-TRACE, else NIL."
  (check-type texts list)
  (let* ((stripped (map 'vector #'strip-ansi texts))
         (traces (%classify-traces stripped 0 (length stripped)))
         (result '()))
    (loop for i from 0 below (length stripped)
          for kinds = (%line-kinds (aref stripped i))
          for trace = (and traces (= i (car (first traces))) (pop traces))
          when (or kinds trace)
            do (push (list (1+ i)
                           (%merge-kinds kinds (and trace '(:stack-trace)))
                           (and trace (1+ (cdr trace))))
                     result))
    (nreverse result)))

(defun %merge-kinds (&rest lists)
  "The union of LISTS of kinds, in +HIT-KINDS+ order."
  (remove-if-not (lambda (k) (some (lambda (l) (member k l)) lists)) +hit-kinds+))
