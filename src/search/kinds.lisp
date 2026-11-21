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

(defun line-kinds (text)
  "→ the one-line kinds of TEXT (:ERROR :WARNING :TEST-FAILURE), in +HIT-KINDS+
order. Stateless; :STACK-TRACE needs CLASSIFY-LINES."
  (check-type text string)
  (%h8c-pending "line-kinds"))

(defun classify-lines (texts)
  "TEXTS: a list of line strings (consecutive lines of one stream or log). →
a list of (INDEX KINDS END) for each line that has at least one kind, in order:
INDEX 1-based into TEXTS, KINDS in +HIT-KINDS+ order (LINE-KINDS plus
:STACK-TRACE on a trace's first line), END the 1-based index of the trace's last
line when KINDS has :STACK-TRACE, else NIL."
  (check-type texts list)
  (%h8c-pending "classify-lines"))
