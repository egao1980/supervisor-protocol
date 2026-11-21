(in-package #:supervisor-protocol/search)

;;; Tokenizer for code and log text (+TOKENIZER-VERSION+ 1). The brief's
;;; "Tokenizer" table is the specification; tests/search-tokenizer-test.lisp
;;; locks it. Summary:
;;;
;;;  1. STRIP-ANSI, then every #\\ becomes #\/.
;;;  2. Spans: maximal runs of span characters = word characters
;;;     (ALPHANUMERICP or #\_) and the separators - . / : @ #. Everything else
;;;     (whitespace, quotes, brackets, = , ; | etc.) ends a span.
;;;  3. Each span is trimmed of leading and trailing separators.
;;;  4. A trimmed span that still contains a separator is a compound: emit it
;;;     whole. If it ends in one or more ":<digits>" groups, also emit the prefix
;;;     before them (trimmed). Split the prefix (or the compound) at / and :
;;;     and emit each component that still contains a separator (- . @ #).
;;;  5. Words: split the span at every non-word character; emit each run.
;;;  6. Sub-words: a word with #\_ or a case boundary splits at #\_, between a
;;;     lowercase letter or digit and an uppercase letter (aB, 5B), and before the
;;;     last uppercase letter of an uppercase run followed by a lowercase one
;;;     (HTTPResponse → HTTP Response). Emit the parts when there are two or more.
;;;     Letters and digits are not split from each other (sha256, v2, utf8).
;;;  7. Everything is lowercased (CHAR-DOWNCASE). Terms of one character are
;;;     dropped unless it is a digit; terms over +MAX-TERM-LENGTH+ are dropped.
;;;
;;; Per span the terms come out in this order: the compound-level terms (the
;;; compound, the prefix, the components; each distinct term once), then for
;;; each word in order the word and its sub-words (the sub-words of one word
;;; distinct). Everything else counts once per occurrence: "error error" has
;;; the term "error" twice.

(defun strip-ansi (string)
  "STRING without ANSI escape sequences: CSI (ESC [ … final byte 0x40–0x7E), OSC
(ESC ] … BEL or ESC \\), and any other ESC with its intermediates (0x20–0x2F)
and one final byte. Other characters are kept as they are."
  (check-type string string)
  (%h8c-pending "strip-ansi"))

(defun tokenize (text)
  "→ the list of terms of TEXT (a string), in order, one per occurrence. See the
rules above."
  (check-type text string)
  (%h8c-pending "tokenize"))

(defun query-terms (query)
  "→ (values TERMS PHRASES). PHRASES: the lowercased text of each double-quoted part
of QUERY (an unterminated quote runs to the end; empty phrases dropped), in order.
TERMS: TOKENIZE of the whole QUERY (quotes are not span characters, so phrase
words count as terms too), duplicates removed, first occurrence order."
  (check-type query string)
  (%h8c-pending "query-terms"))
