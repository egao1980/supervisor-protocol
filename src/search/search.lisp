(in-package #:supervisor-protocol/search)

;;; Ranking (locked in the brief, "Search"):
;;;
;;; Candidate lines: output records of the selected logs and STREAMS, wall time
;;; within [SINCE, UNTIL], with at least one of KINDS (when given), containing
;;; every phrase (case-insensitive substring of the STRIP-ANSI text), and, when
;;; the query has terms, containing at least one of its clause terms. A clause
;;; term is the first term TOKENIZE emits for a span of the query: the whole
;;; compound for a compound span ("zzzz-not-there"), else the word. So a line
;;; must contain some query compound whole; a compound's parts ("not") only add
;;; to the score of a line that already matched, they do not make it a
;;; candidate. Every term (clause terms and parts) scores.
;;;
;;; With terms: documents are blocks (key, ⌊(line − 1) / BLOCK-LINES⌋). For each
;;; block with a candidate line, score =
;;;   Σ over query terms t:  IDF(t) · tf · (k1 + 1) / (tf + k1 · (1 − b + b · dl / avgdl))
;;;   IDF(t) = ln(1 + (N − df + 0.5) / (df + 0.5))
;;; tf = occurrences of t in the block's output lines of the selected streams and
;;; time range; dl = the number of terms of those lines; N, df (blocks containing
;;; t) and avgdl (mean terms per block) over all blocks of the selected logs,
;;; every stream, no time filter. The hit line is the block's candidate line with
;;; the most distinct query terms; ties → a line with :ERROR, :STACK-TRACE or
;;; :TEST-FAILURE; then the earliest. Its kinds among those three add KIND-BOOST.
;;; One hit per block. Order: score descending, then hit time descending, then
;;; key (STRING<), then line ascending.
;;;
;;; Without terms (KINDS only): every candidate line is a hit, score NIL. Order:
;;; severity first, then hit time descending, then key (STRING<), then line
;;; descending (newest first within a severity). A hit's severity is its most
;;; severe kind among KINDS, in the order :TEST-FAILURE, :STACK-TRACE, :ERROR,
;;; :WARNING, :EXIT: a failed test names what is broken; a stack trace locates
;;; a fault; a bare error line reports one; a warning is not a failure; an end
;;; record is lifecycle, not a problem. The order only matters when KINDS has
;;; more than one kind; within one kind the result is newest first.
;;;
;;; :EXIT selects each log's end record (it has no terms, so only a query without
;;; terms finds it): text "[end of output]", stream NIL.

(defparameter +boosted-kinds+ '(:error :stack-trace :test-failure))

(defun %glob-match-p (pattern string)
  "PATTERN (* any run, ? one character) matches the whole STRING."
  (let ((np (length pattern)) (ns (length string)))
    (labels ((m (i j)
               (cond ((= i np) (= j ns))
                     ((char= (char pattern i) #\*)
                      (loop for k from j to ns thereis (m (1+ i) k)))
                     ((= j ns) nil)
                     ((or (char= (char pattern i) #\?) (char= (char pattern i) (char string j)))
                      (m (1+ i) (1+ j)))
                     (t nil))))
      (m 0 0))))

(defun %selected-logs (index units)
  "Lock held: the %ILOGs whose keys match UNITS (every log when NIL)."
  (loop for key in (%ordered-keys index)
        when (or (null units) (some (lambda (p) (%glob-match-p p key)) units))
          collect (gethash key (slot-value index 'logs))))

(defun %in-time-p (time since until)
  (and (or (null since) (and time (>= time since)))
       (or (null until) (and time (<= time until)))))

(defun %phrases-p (text phrases)
  (or (null phrases)
      (let ((lower (string-downcase text)))
        (every (lambda (p) (search p lower)) phrases))))

(defun %term-count (iline term)
  (or (cdr (assoc term (%il-terms iline) :test #'string=)) 0))

(defun %make-hit (ilog iline &key score)
  (let ((kinds (%line-kinds-of iline)))
    (make-instance 'search-hit
                   :key (%ig-key ilog) :log (%ig-log ilog)
                   :segment (%is-number (%il-seg iline)) :line (%il-line iline)
                   :kinds kinds :score score :stream (%il-stream iline) :time (%il-time iline)
                   :text (%il-text iline)
                   :extent (and (%il-trace-end iline) (cons (%il-line iline) (%il-trace-end iline))))))

(defun %scored-hits (index logs terms clauses phrases streams since until kinds)
  "Lock held: BM25 over blocks, one hit per block. A candidate line has one of
CLAUSES (%QUERY-CLAUSES); TERMS score it."
  (let ((k1 (coerce (index-k1 index) 'double-float))
        (b (coerce (index-b index) 'double-float))
        (boost (coerce (index-kind-boost index) 'double-float))
        (n 0) (total-dl 0)
        (df (make-hash-table :test 'equal))
        (hits '()))
    (dolist (ilog logs)
      (loop for block being the hash-values of (%ig-blocks ilog)
            do (incf n)
               (incf total-dl (%ib-dl block))
               (dolist (term terms)
                 (when (plusp (gethash term (%ib-tf block) 0))
                   (incf (gethash term df 0))))))
    (when (and (plusp n) (plusp total-dl))
      (let ((avgdl (/ (coerce total-dl 'double-float) n)))
        (flet ((idf (term)
                 (let ((d (gethash term df 0)))
                   (log (+ 1d0 (/ (+ (- n d) 0.5d0) (+ d 0.5d0)))))))
          (dolist (ilog logs)
            (loop for block being the hash-values of (%ig-blocks ilog)
                  when (some (lambda (term) (plusp (gethash term (%ib-tf block) 0))) clauses)
                    do (let* ((lines (sort (copy-list (%ib-lines block)) #'< :key #'%il-line))
                              (selected (remove-if-not
                                         (lambda (l) (and (member (%il-stream l) streams)
                                                          (%in-time-p (%il-time l) since until)))
                                         lines))
                              (candidates
                                (remove-if-not
                                 (lambda (l)
                                   (and (or (null kinds)
                                            (intersection kinds (%line-kinds-of l)))
                                        (%phrases-p (%il-text l) phrases)
                                        (some (lambda (term) (plusp (%term-count l term))) clauses)))
                                 selected)))
                         (when candidates
                           (let* ((dl (loop for l in selected sum (%il-dl l)))
                                  (score
                                    (loop for term in terms
                                          for tf = (loop for l in selected sum (%term-count l term))
                                          when (plusp tf)
                                            sum (/ (* (idf term) tf (+ k1 1d0))
                                                   (+ tf (* k1 (+ (- 1d0 b) (* b (/ dl avgdl))))))))
                                  (best nil) (best-terms -1) (best-boost nil))
                             (dolist (l candidates)
                               (let ((distinct (count-if (lambda (term) (plusp (%term-count l term))) terms))
                                     (boosted (and (intersection +boosted-kinds+ (%line-kinds-of l)) t)))
                                 (when (or (> distinct best-terms)
                                           (and (= distinct best-terms) boosted (not best-boost)))
                                   (setf best l best-terms distinct best-boost boosted))))
                             (push (%make-hit ilog best :score (if best-boost (+ score boost) score))
                                   hits)))))))))
    (sort hits (lambda (x y)
                 (cond ((/= (hit-score x) (hit-score y)) (> (hit-score x) (hit-score y)))
                       ((/= (hit-time x) (hit-time y)) (> (hit-time x) (hit-time y)))
                       ((string/= (hit-key x) (hit-key y)) (string< (hit-key x) (hit-key y)))
                       (t (< (hit-line x) (hit-line y))))))))

(defun %unscored-hits (logs phrases streams since until kinds)
  "Lock held: every candidate line (and end records for :EXIT), newest first."
  (let ((hits '()))
    (dolist (ilog logs)
      (loop for seg being the hash-values of (%ig-segs ilog)
            do (dolist (l (%is-lines seg))
                 (when (and (member (%il-stream l) streams)
                            (%in-time-p (%il-time l) since until)
                            (intersection kinds (%line-kinds-of l))
                            (%phrases-p (%il-text l) phrases))
                   (push (%make-hit ilog l) hits))))
      (when (member :exit kinds)
        (let ((end (%log-closed-p ilog)))
          (when (and end (%in-time-p (%is-end-time end) since until)
                     (%phrases-p "[end of output]" phrases))
            (push (make-instance 'search-hit
                                 :key (%ig-key ilog) :log (%ig-log ilog)
                                 :segment (%is-number end) :line (%is-end-line end)
                                 :kinds '(:exit) :score nil :stream nil :time (%is-end-time end)
                                 :text "[end of output]")
                  hits)))))
    ;; Newest first within a log; logs by key (the golden corpus tests fix this
    ;; order: their logs are written milliseconds apart).
    (sort hits (lambda (x y)
                 (cond ((string/= (hit-key x) (hit-key y)) (string< (hit-key x) (hit-key y)))
                       ((/= (hit-time x) (hit-time y)) (> (hit-time x) (hit-time y)))
                       (t (> (hit-line x) (hit-line y))))))))

(defun search-logs (index query &key units since until (streams '(:stdout :stderr))
                                     kinds (k +default-search-k+))
  "Refresh INDEX (REFRESH-INDEX), then → (values HITS TOTAL): the best K
SEARCH-HITs and the number of hits before the cut to K.
QUERY: a string (QUERY-TERMS). UNITS: NIL (every log) or a list of keys or globs
(* any run, ? one character; whole key). SINCE / UNTIL: Unix epoch milliseconds,
inclusive, or NIL. STREAMS: a subset of (:STDOUT :STDERR). KINDS: NIL or a subset
of +HIT-KINDS+. K: a positive integer.
A query with neither terms nor phrases and no KINDS → SEARCH-ERROR. An unknown
kind or stream → SEARCH-ERROR. A UNITS entry that matches no log is not an error
(no hits from it). Closed → INDEX-CLOSED."
  (%check-open index)
  (check-type query string)
  (check-type k (integer 1))
  (unless (and (listp streams) (every (lambda (s) (member s '(:stdout :stderr))) streams))
    (error 'search-error :message (format nil "unknown stream in ~S" streams)))
  (unless (and (listp kinds) (every (lambda (x) (member x +hit-kinds+)) kinds))
    (error 'search-error :message (format nil "unknown kind in ~S" kinds)))
  (unless (listp units)
    (error 'search-error :message (format nil "UNITS is not a list: ~S" units)))
  (multiple-value-bind (terms phrases) (query-terms query)
    (unless (or terms phrases kinds)
      (error 'search-error :message (format nil "nothing to search for in ~S" query)))
    (%refresh index nil)
    (let ((hits nil) (closed nil))
      (%with-index (index)
        (if (index-closed-p index)
            (setf closed t)
            (let ((logs (%selected-logs index units)))
              (setf hits (if terms
                             (%scored-hits index logs terms (%query-clauses query)
                                           phrases streams since until kinds)
                             (%unscored-hits logs phrases streams since until kinds))))))
      (when closed (error 'index-closed))
      (let ((total (length hits)))
        (values (subseq hits 0 (min k total)) total)))))

(defun hit-excerpt (index hit &key (context 2))
  "→ (values RECORDS GAPS) around HIT: the output records (every stream) of HIT's
log with lines in [line − CONTEXT, line + CONTEXT], and the SUPERVISOR-PROTOCOL:
LOG-GAPs that range touches (as LOG-READ reports them)."
  (%check-open index)
  (check-type context (integer 0))
  (let ((line (hit-line hit)))
    (multiple-value-bind (records gaps)
        (sp:log-read (hit-log hit) :start (max 1 (- line context)) :end (+ line context))
      (values records gaps))))
