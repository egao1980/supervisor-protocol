(in-package #:supervisor-protocol/tests)

;;; H8c golden queries over the recorded corpus, all four logs in one index:
;;; the expected ref (looked up in the log, not through the index) is in the
;;; top K. Fixture line numbers are 1-based lines of tests/fixtures/corpus/*.log.

(deftest-parametrize golden-queries
    ((key fixture-line query k kinds)
     ;; build.log: cc compile error and warning
     ("build" 19 "undeclared identifier tokn" 1 nil)
     ("build" 19 "parser.c error" 3 nil)
     ("build" 46 "unused variable" 3 nil)
     ("build" 19 "" 3 '(:error))
     ;; rove.log: two failures, a TYPE-ERROR, an SBCL backtrace
     ("rove" 29 "currency" 1 '(:test-failure))
     ("rove" 20 "ADD-FLOATS 0.3" 3 '(:test-failure))
     ("rove" 48 "TYPE-ERROR NUMBER" 3 nil)
     ("rove" 54 "" 3 '(:stack-trace))
     ;; devserver.log: request log, two TypeErrors with JS stacks, a deprecation
     ("devserver" 15 "DeprecationWarning Buffer" 3 nil)
     ("devserver" 7 "Cannot read properties of undefined" 3 nil)
     ("devserver" 18 "Cannot read properties of undefined" 3 nil)
     ("devserver" 8 "userRoles routes.js" 3 nil)
     ;; pytest.log: two failures with assertion and KeyError details
     ("pytest" 40 "KeyError kiwi" 3 nil)
     ("pytest" 44 "test_total_counts_all_items" 3 '(:test-failure))
     ("pytest" 8 "test_total_counts_all_items" 3 '(:test-failure))
     ("pytest" 45 "test_remove_missing_item_raises" 3 '(:test-failure)))
  (with-corpus-index (index root)
    (let ((ref (if (and (equal key "devserver") (= fixture-line 18))
                   (corpus-ref index key 18 :nth 1) ; the second identical TypeError line
                   (corpus-ref index key fixture-line))))
      (ok ref "the fixture line is in the log")
      (ok (in-top-p ref index query :k k :kinds kinds)
          (format nil "~A:~D in the top ~D for ~S~@[ ~S~]" key fixture-line k query kinds)))))

(deftest golden-stack-traces-across-the-corpus
  (with-corpus-index (index root)
    (multiple-value-bind (hits total) (ss:search-logs index "" :kinds '(:stack-trace) :k 10)
      (ok (= 3 total))
      (ok (equal (list (corpus-ref index "devserver" 18 :nth 1)
                       (corpus-ref index "devserver" 7)
                       (corpus-ref index "rove" 54))
                 (hit-refs hits))
          "newest first")
      (ok (equal (list (cons (line-of-fixture index "devserver" 7) (line-of-fixture index "devserver" 12)))
                 (mapcar #'ss:hit-extent (remove (corpus-ref index "devserver" 7) hits
                                                 :key #'ss:hit-ref :test-not #'equal)))))))

(deftest golden-errors-across-the-corpus
  (with-corpus-index (index root)
    (let ((refs (top-refs index "" :kinds '(:error) :k 50)))
      (dolist (want (list (corpus-ref index "build" 19) (corpus-ref index "rove" 48)
                          (corpus-ref index "pytest" 40) (corpus-ref index "devserver" 7)))
        (ok (member want refs :test #'equal) want)))
    (ok (null (ss:search-logs index "undeclared identifier" :units '("rove" "pytest")))
        "the units filter keeps the build log out")
    (ok (equal (list (corpus-ref index "build" 19))
               (top-refs index "undeclared identifier tokn" :k 10))
        "exactly one block of the corpus has these words")))
