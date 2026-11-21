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


(declaim (inline %word-char-p %separator-p %span-char-p))

(defun %word-char-p (c)
  (or (alphanumericp c) (char= c #\_)))

(defun %separator-p (c)
  (member c '(#\- #\. #\/ #\: #\@ #\#)))

(defun %span-char-p (c)
  (or (%word-char-p c) (%separator-p c)))

(defun strip-ansi (string)
  "STRING without ANSI escape sequences: CSI (ESC [ … final byte 0x40–0x7E), OSC
(ESC ] … BEL or ESC \\), and any other ESC with its intermediates (0x20–0x2F)
and one final byte. Other characters are kept as they are."
  (check-type string string)
  (let ((esc (code-char 27)))
    (if (not (find esc string))
        string
        (let ((n (length string)))
          (with-output-to-string (out)
            (let ((i 0))
              (loop while (< i n)
                    do (let ((c (char string i)))
                         (cond
                           ((char/= c esc) (write-char c out) (incf i))
                           ((and (< (1+ i) n) (char= (char string (1+ i)) #\[))
                            ;; CSI: parameters and intermediates up to the final byte
                            (setf i (+ i 2))
                            (loop while (and (< i n)
                                             (not (<= #x40 (char-code (char string i)) #x7e)))
                                  do (incf i))
                            (incf i))
                           ((and (< (1+ i) n) (char= (char string (1+ i)) #\]))
                            ;; OSC: up to BEL or ESC \
                            (setf i (+ i 2))
                            (loop
                              (cond ((>= i n) (return))
                                    ((= 7 (char-code (char string i))) (incf i) (return))
                                    ((and (char= (char string i) esc) (< (1+ i) n)
                                          (char= (char string (1+ i)) #\\))
                                     (setf i (+ i 2)) (return))
                                    (t (incf i)))))
                           (t
                            ;; ESC, intermediates 0x20-0x2F, one final byte
                            (incf i)
                            (loop while (and (< i n) (<= #x20 (char-code (char string i)) #x2f))
                                  do (incf i))
                            (incf i)))))))))))

(defun %prepare (text)
  "STRIP-ANSI, then every backslash becomes a slash."
  (substitute #\/ #\\ (strip-ansi text)))

(defun %map-spans (fn text)
  "Call FN with (START END) of every span of the prepared TEXT, trimmed of
separators at both ends (empty spans skipped)."
  (let ((n (length text)) (i 0))
    (loop
      (loop while (and (< i n) (not (%span-char-p (char text i)))) do (incf i))
      (when (>= i n) (return))
      (let ((start i))
        (loop while (and (< i n) (%span-char-p (char text i))) do (incf i))
        (let ((s start) (e i))
          (loop while (and (< s e) (%separator-p (char text s))) do (incf s))
          (loop while (and (< s e) (%separator-p (char text (1- e)))) do (decf e))
          (when (< s e) (funcall fn s e)))))))

(defun %word-runs (text start end)
  "→ the runs of word characters of TEXT between START and END (fresh strings)."
  (let ((runs '()) (i start))
    (loop
      (loop while (and (< i end) (not (%word-char-p (char text i)))) do (incf i))
      (when (>= i end) (return))
      (let ((s i))
        (loop while (and (< i end) (%word-char-p (char text i))) do (incf i))
        (push (subseq text s i) runs)))
    (nreverse runs)))

(defun %sub-words (word)
  "→ WORD's parts: split at #\\_, at lower/digit → upper, and before the last
capital of a capital run followed by a lowercase letter. Empty parts dropped."
  (let ((parts '()) (n (length word)) (start 0))
    (flet ((cut (end)
             (when (< start end) (push (subseq word start end) parts))))
      (loop for i from 0 below n
            for c = (char word i)
            do (cond ((char= c #\_)
                      (cut i)
                      (setf start (1+ i)))
                     ((and (> i start) (upper-case-p c)
                           (let ((p (char word (1- i))))
                             (or (lower-case-p p) (digit-char-p p)
                                 (and (upper-case-p p) (< (1+ i) n)
                                      (lower-case-p (char word (1+ i)))))))
                      (cut i)
                      (setf start i))))
      (cut n))
    (nreverse parts)))

(defun %term (string)
  "STRING lowercased, or NIL when it is not a term (one non-digit character, or
longer than +MAX-TERM-LENGTH+)."
  (let ((n (length string)))
    (cond ((or (zerop n) (> n +max-term-length+)) nil)
          ((and (= n 1) (not (digit-char-p (char string 0)))) nil)
          (t (string-downcase string)))))

(defun %trailing-line-col-start (text start end)
  "When TEXT[START, END) ends in one or more \":<digits>\" groups, → the index
where the first of those groups starts, else NIL."
  (let ((e end) (found nil))
    (loop
      (let ((d e))
        (loop while (and (> d start) (digit-char-p (char text (1- d)))) do (decf d))
        (if (and (< d e) (> d start) (char= (char text (1- d)) #\:))
            (setf e (1- d) found e)
            (return))))
    found))

(defun %span-terms (text start end emit)
  "Call EMIT with each term of the span TEXT[START, END), in order."
  (let ((compound-p (loop for i from start below end thereis (%separator-p (char text i)))))
    (when compound-p
      (let ((level '()))
        (flet ((add (s) (let ((term (%term s)))
                          (when (and term (not (member term level :test #'string=)))
                            (push term level)))))
          (add (subseq text start end))
          (let* ((cut (%trailing-line-col-start text start end))
                 (pstart start) (pend (or cut end)))
            (when cut
              (loop while (and (< pstart pend) (%separator-p (char text pstart))) do (incf pstart))
              (loop while (and (< pstart pend) (%separator-p (char text (1- pend)))) do (decf pend))
              (when (< pstart pend) (add (subseq text pstart pend))))
            ;; components of the prefix (or the compound) split at / and :
            (let ((i pstart))
              (loop while (< i pend)
                    do (let ((s i))
                         (loop while (and (< i pend) (not (member (char text i) '(#\/ #\:))))
                               do (incf i))
                         (when (loop for j from s below i thereis (%separator-p (char text j)))
                           (add (subseq text s i)))
                         (incf i))))))
        (dolist (term (nreverse level)) (funcall emit term))))
    (dolist (word (%word-runs text start end))
      (let ((term (%term word)))
        (when term (funcall emit term)))
      (let ((parts (%sub-words word)))
        (when (>= (length parts) 2)
          (let ((seen '()))
            (dolist (p parts)
              (let ((term (%term p)))
                (when (and term (not (member term seen :test #'string=)))
                  (push term seen)
                  (funcall emit term))))))))))

(defun tokenize (text)
  "→ the list of terms of TEXT (a string), in order, one per occurrence. See the
rules above."
  (check-type text string)
  (let* ((prepared (%prepare text))
         (terms '()))
    (%map-spans (lambda (s e)
                  (%span-terms prepared s e (lambda (term) (push term terms))))
                prepared)
    (nreverse terms)))

(defun %query-clauses (query)
  "→ one term per span of QUERY (the first it emits: the compound for a compound,
else the word), distinct. SEARCH-LOGS matches a line by these: a compound in a
query must occur whole, its parts only add to the score."
  (let* ((prepared (%prepare query))
         (clauses '()))
    (%map-spans (lambda (s e)
                  (let ((first nil))
                    (%span-terms prepared s e (lambda (term) (unless first (setf first term))))
                    (when (and first (not (member first clauses :test #'string=)))
                      (push first clauses))))
                prepared)
    (nreverse clauses)))

(defun query-terms (query)
  "→ (values TERMS PHRASES). PHRASES: the lowercased text of each double-quoted part
of QUERY (an unterminated quote runs to the end; empty phrases dropped), in order.
TERMS: TOKENIZE of the whole QUERY (quotes are not span characters, so phrase
words count as terms too), duplicates removed, first occurrence order."
  (check-type query string)
  (let ((phrases '()))
    (loop for piece in (uiop:split-string query :separator "\"")
          for odd = nil then (not odd)
          when (and odd (plusp (length piece)))
            do (push (string-downcase piece) phrases))
    (values (remove-duplicates (tokenize query) :test #'string= :from-end t)
            (nreverse phrases))))
