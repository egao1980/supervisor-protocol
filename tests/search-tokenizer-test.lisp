(in-package #:supervisor-protocol/tests)

;;; H8c tokenizer table (+tokenizer-version+ 1). Expected values are the sorted
;;; distinct terms; occurrence counts and order are checked separately.

(deftest search-constants-and-conditions
  (ok (equal '(:error :warning :stack-trace :test-failure :exit) ss:+hit-kinds+))
  (ok (= 1 ss:+search-index-format+))
  (ok (= 1 ss:+tokenizer-version+))
  (ok (= 16 ss:+default-block-lines+))
  (ok (= 1.2 ss:+default-bm25-k1+))
  (ok (= 0.75 ss:+default-bm25-b+))
  (ok (= 0.5 ss:+default-kind-boost+))
  (ok (= 64 ss:+max-term-length+))
  (ok (= 10 ss:+default-search-k+))
  (ok (subtypep 'ss:search-error 'error))
  (ok (subtypep 'ss:index-closed 'ss:search-error))
  (ok (subtypep 'ss:index-file-error 'warning))
  (ok (not (subtypep 'ss:index-file-error 'error)))
  (ok (subtypep 'unit-input-closed 'unit-error)))

(deftest-parametrize tokenizer-table
    ((text expected)
     ("Hello World" '("hello" "world"))
     ("src/foo.lisp:12:5: error: bad"
      '("12" "5" "bad" "error" "foo" "foo.lisp" "lisp" "src" "src/foo.lisp" "src/foo.lisp:12:5"))
     ("parseHTTPResponse_v2" '("http" "parse" "parsehttpresponse_v2" "response" "v2"))
     ("my-package::some-function"
      '("function" "my" "my-package" "my-package::some-function" "package" "some" "some-function"))
     ("TypeError: Cannot read properties of undefined (reading 'map')"
      '("cannot" "error" "map" "of" "properties" "read" "reading" "type" "typeerror" "undefined"))
     ("    at Object.<anonymous> (/app/src/index.js:42:13)"
      '("13" "42" "anonymous" "app" "app/src/index.js" "app/src/index.js:42:13" "at" "index"
       "index.js" "js" "object" "src"))
     ("0x7f3a2c 1.25 404" '("0x7f3a2c" "1" "1.25" "25" "404"))
     ("ERROR [main] Connection refused" '("connection" "error" "main" "refused"))
     ("C:\\Users\\dev\\proj\\build.ps1"
      '("build" "build.ps1" "c:/users/dev/proj/build.ps1" "dev" "proj" "ps1" "users"))
     ("日本語 テスト" '("テスト" "日本語"))
     ("a b c 7" '("7"))
     ("--log-level=debug" '("debug" "level" "log" "log-level"))
     ("user@example.com sent 3 msgs" '("3" "com" "example" "msgs" "sent" "user" "user@example.com"))
     ("http://localhost:3000/api/users?id=7"
      '("3000" "7" "api" "http" "http://localhost:3000/api/users" "id" "localhost" "users"))
     ("__init__ md5Sum sha256 utf8Decoder"
      '("__init__" "decoder" "md5" "md5sum" "sha256" "sum" "utf8" "utf8decoder"))
     ("SB-KERNEL:TWO-ARG-= 42 NIL"
      '("42" "arg" "kernel" "nil" "sb" "sb-kernel" "sb-kernel:two-arg" "two" "two-arg"))
     ("make: *** [src/parser.o] Error 1" '("1" "error" "make" "parser" "parser.o" "src" "src/parser.o"))
     ("foo_foo FOO_BAR" '("bar" "foo" "foo_bar" "foo_foo")))
  (ok (equal expected (sorted-terms text)) text))

(deftest tokenizer-ansi-and-length-limits
  (ok (equal '("add" "failed" "py" "test" "test_add" "test_x" "test_x.py" "tests"
               "tests/test_x.py::test_add")
             (sorted-terms (esc :esc "[31mFAILED" :esc "[0m tests/test_x.py::test_add")))
      "CSI colour codes are stripped before tokenizing")
  (ok (null (ss:tokenize (make-string 70 :initial-element #\a))) "a 70-character word is dropped")
  (ok (equal (list (make-string 40 :initial-element #\x) (make-string 30 :initial-element #\y))
             (sorted-terms (format nil "~A-~A" (make-string 40 :initial-element #\x)
                                   (make-string 30 :initial-element #\y))))
      "a 71-character compound is dropped, its words stay")
  (ok (equal (list (make-string 64 :initial-element #\b))
             (ss:tokenize (make-string 64 :initial-element #\b)))
      "64 characters is still a term"))

(deftest tokenizer-occurrences-and-order
  (ok (equal '("error" "error" "error") (ss:tokenize "error error Error")))
  (ok (equal '("src/a.c" "a.c" "src" "src/a.c" "a.c" "src") (ss:tokenize "src/a.c src/a.c"))
      "compound-level terms first, then words; per occurrence")
  (ok (equal '("foo_foo" "foo") (ss:tokenize "foo_foo")) "sub-words of one word are distinct")
  (ok (equal '("parsehttpresponse_v2" "parse" "http" "response" "v2")
             (ss:tokenize "parseHTTPResponse_v2"))
      "word, then its sub-words in order"))

(deftest strip-ansi-sequences
  (ok (equal "red plain" (ss:strip-ansi (esc :esc "[1;31mred" :esc "[0m plain"))) "CSI")
  (ok (equal "title text" (ss:strip-ansi (esc :esc "]0;title" (u 7) "title text"))) "OSC … BEL")
  (ok (equal "link" (ss:strip-ansi (esc :esc "]8;;http://x" :esc "\\" "link"))) "OSC … ESC \\")
  (ok (equal "ab" (ss:strip-ansi (esc "a" :esc "(B" "b"))) "ESC, an intermediate, a final byte")
  (ok (equal "keep	tabs" (ss:strip-ansi "keep	tabs")) "other characters stay"))

(deftest query-terms-and-phrases
  (multiple-value-bind (terms phrases) (ss:query-terms "undeclared identifier tokn")
    (ok (equal '("undeclared" "identifier" "tokn") terms))
    (ok (null phrases)))
  (multiple-value-bind (terms phrases) (ss:query-terms "\"Connection refused\" port")
    (ok (equal '("connection" "refused" "port") terms))
    (ok (equal '("connection refused") phrases)))
  (multiple-value-bind (terms phrases) (ss:query-terms "error error \"\" \"open quote")
    (ok (equal '("error" "open" "quote") terms) "duplicates removed")
    (ok (equal '("open quote") phrases) "empty phrase dropped; an open quote runs to the end"))
  (ok (null (ss:query-terms "  ")) "blank → no terms"))
