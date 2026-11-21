(in-package #:supervisor-protocol/tests)

;;; H8c ranking and filters: BM25 by hand on tiny logs, the kind boost, hit line
;;; choice, one hit per block, ordering, streams / time / units / phrases /
;;; kinds, :EXIT, argument errors.

(defun call-with-tiny-index (fn logs &rest index-keys)
  "LOGS: ((key . lines) …), each a closed standalone log."
  (with-temp-dir (root)
    (let ((index (apply #'ss:make-log-index index-keys)))
      (unwind-protect
           (progn
             (loop for (key . lines) in logs
                   do (ss:index-add-log index (make-corpus-log root key :lines lines) :key key))
             (funcall fn index))
        (ss:close-log-index index)))))

(defmacro with-tiny-index ((index logs &rest index-keys) &body body)
  `(call-with-tiny-index (lambda (,index) ,@body) ,logs ,@index-keys))

(defun bm25 (tf dl avgdl n df &key (k1 1.2) (b 0.75))
  (* (log (+ 1 (/ (+ (- n df) 0.5d0) (+ df 0.5d0))))
     (/ (* tf (+ k1 1)) (+ tf (* k1 (+ (- 1 b) (* b (/ dl avgdl))))))))

(defun close-to (a b) (and (realp a) (< (abs (- a b)) 1d-4)))

(deftest bm25-scores-by-hand
  (with-tiny-index (index '(("a" "alpha beta" "gamma") ("b" "alpha" "delta delta delta")))
    (let ((hits (ss:search-logs index "alpha")))
      (ok (equal '("a" "b") (mapcar #'ss:hit-key hits)) "the shorter block ranks first")
      ;; N = 2 blocks, df = 2, dl 3 and 4, avgdl 3.5.
      (ok (close-to (ss:hit-score (first hits)) (bm25 1 3 3.5d0 2 2)))
      (ok (close-to (ss:hit-score (second hits)) (bm25 1 4 3.5d0 2 2))))))

(deftest bm25-parameters-are-the-index-s
  (with-tiny-index (index '(("a" "alpha beta" "gamma") ("b" "alpha" "delta delta delta"))
                    :k1 2.0 :b 0.5)
    (ok (close-to (ss:hit-score (first (ss:search-logs index "alpha")))
                  (bm25 1 3 3.5d0 2 2 :k1 2.0d0 :b 0.5d0)))))

(deftest kind-boost-is-added
  (with-tiny-index (index '(("a" "alpha error")))
    ;; N = 1, df = 1, dl = avgdl = 2 → BM25 = ln(4/3); the line has :ERROR.
    (ok (close-to (ss:hit-score (first (ss:search-logs index "alpha"))) (+ (log 4/3) 0.5d0))))
  (with-tiny-index (index '(("a" "alpha error")) :kind-boost 0)
    (ok (close-to (ss:hit-score (first (ss:search-logs index "alpha"))) (log 4/3))))
  (with-tiny-index (index '(("a" "alpha warning")))
    (ok (close-to (ss:hit-score (first (ss:search-logs index "alpha"))) (log 4/3))
        ":WARNING gets no boost")))

(deftest hit-line-is-the-best-line-of-its-block
  (with-tiny-index (index '(("a" "one alpha" "alpha beta gamma" "beta" "gamma alpha beta")))
    (let ((hit (first (ss:search-logs index "alpha beta gamma"))))
      (ok (= 3 (ss:hit-line hit)) "most distinct query terms; the earliest of a tie")
      (ok (equal "alpha beta gamma" (ss:hit-text hit)))
      (ok (eq :stdout (ss:hit-stream hit)))
      (ok (integerp (ss:hit-time hit)))))
  (with-tiny-index (index '(("a" "alpha beta" "alpha beta error")))
    (ok (= 3 (ss:hit-line (first (ss:search-logs index "alpha beta"))))
        "a tie goes to the line with :ERROR")))

(deftest one-hit-per-block-and-the-total
  (with-corpus-index (index root :names '("build"))
    ;; "cc" is on fixture lines 1-18, 28-45, 50 → log blocks 0, 1, 2, 3.
    (multiple-value-bind (hits total) (ss:search-logs index "cc" :k 2)
      (ok (= 2 (length hits)))
      (ok (= 4 total) "TOTAL counts every block before the cut")
      (ok (= 2 (length (remove-duplicates (mapcar (lambda (h) (floor (1- (ss:hit-line h)) 16)) hits))))))
    (multiple-value-bind (hits total) (ss:search-logs index "cc" :k 50)
      (ok (= 4 (length hits)))
      (ok (= 4 total)))))

(deftest kinds-only-search-newest-first
  (with-corpus-index (index root :names '("rove"))
    (multiple-value-bind (hits total) (ss:search-logs index "" :kinds '(:test-failure))
      (ok (= 2 total))
      (ok (equal (list (corpus-ref index "rove" 29) (corpus-ref index "rove" 20)) (hit-refs hits)))
      (ok (every #'null (mapcar #'ss:hit-score hits)) "no terms → no score"))
    (let ((hit (first (ss:search-logs index "" :kinds '(:stack-trace)))))
      (ok (equal (corpus-ref index "rove" 54) (ss:hit-ref hit)))
      (ok (equal (cons (line-of-fixture index "rove" 54) (line-of-fixture index "rove" 68))
                 (ss:hit-extent hit))))
    (ok (equal (list (corpus-ref index "rove" 29))
               (top-refs index "currency" :kinds '(:test-failure)))
        "terms and kinds together")))

(deftest exit-hits-are-end-records
  (with-corpus-index (index root :names '("build" "pytest"))
    (let ((hits (ss:search-logs index "" :kinds '(:exit))))
      (ok (= 2 (length hits)))
      (ok (equal '("build" "pytest") (sort (mapcar #'ss:hit-key hits) #'string<)))
      (dolist (h hits)
        (ok (equal "[end of output]" (ss:hit-text h)))
        (ok (null (ss:hit-stream h)))
        (ok (equal '(:exit) (ss:hit-kinds h)))
        (ok (= (log-last-line (ss:hit-log h)) (ss:hit-line h)))))
    (ok (null (ss:search-logs index "end of output" :kinds '(:exit))) ":EXIT never matches terms")))

(deftest streams-filter
  (with-corpus-index (index root :names '("devserver"))
    (let ((err (ss:search-logs index "roles" :streams '(:stderr) :k 50))
          (out (ss:search-logs index "roles" :streams '(:stdout) :k 50)))
      (ok (and err (every (lambda (h) (eq :stderr (ss:hit-stream h))) err)))
      (ok (and out (every (lambda (h) (eq :stdout (ss:hit-stream h))) out)))
      (ok (member (corpus-ref index "devserver" 6) (hit-refs out) :test #'equal)
          "GET /api/users/1/roles is on stdout"))))

(defun error-lines (from to)
  (loop for i from from to to collect (format nil "error item ~D" i)))

(deftest time-range-filter
  (with-temp-dir (root)
    (let ((index (ss:make-log-index)))
      (multiple-value-bind (dir sink) (make-corpus-log root "t" :close nil :lines (error-lines 1 5))
        (sleep 0.05)
        (write-corpus-lines sink "t" (error-lines 6 10))
        (sink-close sink)
        (ss:index-add-log index dir :key "t")
        (let* ((records (log-read dir))
               (lines (mapcar #'log-record-line records))
               (t5 (log-record-wall-time (nth 4 records)))
               (t6 (log-record-wall-time (nth 5 records))))
          (ok (< t5 t6) "two batches, 50 ms apart")
          (flet ((hit-lines (&rest keys)
                   (sort (mapcar #'ss:hit-line (apply #'ss:search-logs index "" :kinds '(:error) :k 50 keys))
                         #'<))
                 (expected (since until)
                   (loop for r in records
                         for tm = (log-record-wall-time r)
                         when (and (or (null since) (>= tm since)) (or (null until) (<= tm until)))
                           collect (log-record-line r))))
            (ok (equal (subseq lines 5) (hit-lines :since t6)) "since is inclusive")
            (ok (equal (subseq lines 0 5) (hit-lines :until t5)) "until is inclusive")
            (ok (equal (expected t5 t6) (hit-lines :since t5 :until t6)))
            (ok (null (hit-lines :since (1+ (log-record-wall-time (car (last records))))))))))
      (ss:close-log-index index))))

(deftest units-filter-globs
  (with-corpus-index (index root)
    (ok (equal '("build") (remove-duplicates (mapcar #'ss:hit-key (ss:search-logs index "error" :units '("b*") :k 50))
                                             :test #'equal)))
    (ok (equal '("pytest" "rove")
               (sort (remove-duplicates (mapcar #'ss:hit-key (ss:search-logs index "error" :units '("rove" "p?test") :k 50))
                                        :test #'equal)
                     #'string<)))
    (ok (null (ss:search-logs index "error" :units '("nope"))) "a pattern matching nothing is not an error")))

(deftest phrase-filter
  (with-corpus-index (index root :names '("build" "pytest"))
    (ok (equal (list (corpus-ref index "build" 19)) (top-refs index "\"undeclared identifier\"")))
    (ok (null (ss:search-logs index "\"identifier undeclared\"")) "words in another order")
    (ok (equal (list (corpus-ref index "pytest" 40)) (top-refs index "\"KeyError: 'kiwi'\"")))))

(deftest compound-clause-matching
  ;; A candidate has a clause term of the query: per span, the whole compound
  ;; for a compound, else the word. Parts of a query compound only score.
  (with-tiny-index (index '(("a" "not here") ("b" "see zzzz-not-there now")))
    (ok (equal '("b") (mapcar #'ss:hit-key (ss:search-logs index "zzzz-not-there")))
        "the compound must occur whole: \"not here\" is not a candidate")
    (ok (equal '("a" "b") (sort (mapcar #'ss:hit-key (ss:search-logs index "not")) #'string<))
        "a plain word matches a compound's part in the text")))

(deftest search-argument-errors
  (with-corpus-index (index root :names '("build"))
    (ok (signals (ss:search-logs index "") 'ss:search-error) "no terms, no kinds")
    (ok (signals (ss:search-logs index "  ;; ") 'ss:search-error))
    (ok (signals (ss:search-logs index "x" :kinds '(:bogus)) 'ss:search-error))
    (ok (signals (ss:search-logs index "x" :streams '(:stdin)) 'ss:search-error))
    (ok (null (ss:search-logs index "zzzz-not-there")))))
