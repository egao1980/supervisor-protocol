(in-package #:supervisor-protocol/tests)

;;; H8c structured kinds: the one-line table, stack traces, and the classes
;;; of every line of the recorded corpus.

(deftest-parametrize line-kinds-table
    ((text expected)
     ("src/parser.c:7:12: error: use of undeclared identifier 'tokn'" '(:error))
     ("; caught ERROR:" '(:error))
     ("; caught WARNING:" '(:warning))
     ("; caught STYLE-WARNING:" '(:warning))
     ("debugger invoked on a SIMPLE-ERROR in thread" '(:error))
     ("npm ERR! code ELIFECYCLE" '(:error))
     ("FATAL: database is locked" '(:error))
     ("thread 'main' panicked at src/main.rs:2:5:" '(:error))
     ("Exception in thread \"main\" java.lang.NullPointerException" '(:error))
     ("E       KeyError: 'kiwi'" '(:error))
     ("critical: disk full" '(:error))
     ("1 error generated." '(:error))
     ("Wrote /var/log/error.log" '())
     ("see src/errors.c" '())
     ("0 errors, 2 warnings" '())
     ("Build succeeded: 0 Error(s), 0 Warning(s)" '())
     ("no error found" '())
     ("WARN  deprecated package foo@1.0" '(:warning))
     ("DeprecationWarning: Buffer() is deprecated" '(:warning))
     ("2026-11-21T12:00:00.500Z GET /api/users 200 5ms" '())
     ("  × 0) leading currency sign is skipped" '(:test-failure))
     ("× 1 of 1 test failed" '())
     ("FAILED tests/test_inventory.py::test_total - assert 8 == 7" '(:test-failure))
     ("tests/test_inventory.py::test_total FAILED              [ 60%]" '(:test-failure))
     ("tests/test_inventory.py::test_add PASSED                [ 20%]" '())
     ("  ✕ adds two numbers (3 ms)" '(:test-failure))
     ("  ● Calculator › adds two numbers" '(:test-failure))
     ("● nginx.service - A high performance web server" '())
     ("test parser::tests::parses_empty ... FAILED" '(:test-failure))
     ("test parser::tests::parses_one ... ok" '())
     ("--- FAIL: TestParse (0.00s)" '(:test-failure))
     ("FAIL src/sum.test.js" '(:test-failure))
     ("FAIL	example.com/pkg	0.004s" '(:test-failure))
     ("FAILURES" '())
     ("2 failed, 3 passed in 0.01s" '())
     ("FAILED tests/test_inventory.py::test_x - KeyError: 'kiwi'" '(:error :test-failure)))
  (ok (equal expected (ss:line-kinds text)) text))

(deftest line-kinds-see-through-ansi
  (ok (equal '(:test-failure)
             (ss:line-kinds (esc "  " :esc "[31m× " :esc "[0m" :esc "[31m1) leading currency sign is skipped" :esc "[0m"))))
  (ok (equal '(:error) (ss:line-kinds (esc :esc "[31mTYPE-ERROR: The value" :esc "[0m")))))

(deftest classify-python-traceback
  (ok (equal '((1 (:stack-trace) 6) (5 (:error) nil) (6 (:error) nil))
             (ss:classify-lines '("Traceback (most recent call last):"
                                  "  File \"app.py\", line 3, in <module>"
                                  "    main()"
                                  "  File \"app.py\", line 2, in main"
                                  "    raise ValueError(\"bad\")"
                                  "ValueError: bad"
                                  "next line")))))

(deftest classify-sbcl-debugger-backtrace
  (ok (equal '((1 (:error) nil) (4 (:stack-trace) 7))
             (ss:classify-lines
              '("Unhandled SIMPLE-ERROR in thread #<SB-THREAD:THREAD \"main thread\" RUNNING {1001}>:"
                "  boom"
                ""
                "Backtrace for: #<SB-THREAD:THREAD \"main thread\" RUNNING {1001}>"
                "0: (FOO 1)"
                "1: (BAR)"
                "2: (SB-INT:SIMPLE-EVAL-IN-LEXENV (BAR) #<NULL-LEXENV>)"
                "unhandled condition in --disable-debugger mode, quitting")))
      "the header line starts the trace"))

(deftest classify-rust-backtrace
  (ok (equal '((1 (:error) nil) (3 (:stack-trace) 7) (6 (:error) nil))
             (ss:classify-lines
              '("thread 'main' panicked at src/main.rs:2:5:"
                "boom"
                "stack backtrace:"
                "   0: rust_begin_unwind"
                "             at /rustc/abc/library/std/src/panicking.rs:665:5"
                "   1: core::panicking::panic_fmt"
                "   2: app::main"
                "note: Some details are omitted")))
      "frames, with at-lines inside the run"))

(deftest classify-java-and-js-traces
  (ok (equal '((1 (:error :stack-trace) 6) (4 (:error) nil))
             (ss:classify-lines
              (list "Exception in thread \"main\" java.lang.IllegalStateException: bad"
                    (format nil "~Cat com.example.App.run(App.java:10)" #\Tab)
                    (format nil "~Cat com.example.App.main(App.java:5)" #\Tab)
                    "Caused by: java.io.IOException: disk"
                    (format nil "~Cat com.example.Io.read(Io.java:3)" #\Tab)
                    (format nil "~C... 2 more" #\Tab)
                    "done")))
      "Java: header, at-lines, Caused by, … N more")
  (ok (equal '((2 (:stack-trace) 3))
             (ss:classify-lines '("" "    at a (x.js:1:1)" "    at b (x.js:2:1)")))
      "no header after a blank line: the trace starts at the first at-line")
  (ok (equal '((2 (:error) nil))
             (ss:classify-lines '("    at ./calc-test.lisp:16:2" "   TYPE-ERROR: The value")))
      "one at-line is not a trace")
  (ok (null (ss:classify-lines '("Steps:" "1: open" "2: close")))
      "numbered lines that do not start at 0 are not frames"))

(deftest-parametrize corpus-classification
    ((name expected)
     ("build" '((19 (:error) nil) (26 (:error) nil) (27 (:error) nil)
               (46 (:warning) nil) (49 (:warning) nil)))
     ("rove" '((1 (:warning) nil) (3 (:warning) nil) (8 (:warning) nil) (9 (:warning) nil)
              (20 (:test-failure) nil) (29 (:test-failure) nil) (48 (:error) nil)
              (54 (:stack-trace) 68)))
     ("pytest" '((8 (:test-failure) nil) (9 (:test-failure) nil) (24 (:error) nil)
                (29 (:error) nil) (40 (:error) nil) (42 (:error) nil)
                (44 (:test-failure) nil) (45 (:error :test-failure) nil)))
     ("devserver" '((7 (:error :stack-trace) 12) (15 (:warning) nil) (16 (:warning) nil)
                   (18 (:error :stack-trace) 23))))
  (ok (equal expected (ss:classify-lines (corpus-lines name))) name))
