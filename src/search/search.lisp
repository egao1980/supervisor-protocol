(in-package #:supervisor-protocol/search)

;;; Ranking (locked in the brief, "Search"):
;;;
;;; Candidate lines: output records of the selected logs and STREAMS, wall time
;;; within [SINCE, UNTIL], with at least one of KINDS (when given), containing
;;; every phrase (case-insensitive substring of the STRIP-ANSI text), and, when
;;; the query has terms, containing at least one of them.
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
;;; hit time descending, then key, then line descending (newest first).
;;;
;;; :EXIT selects each log's end record (it has no terms, so only a query without
;;; terms finds it): text "[end of output]", stream NIL.

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
  (declare (ignore index query units since until streams kinds k))
  (%h8c-pending "search-logs"))

(defun hit-excerpt (index hit &key (context 2))
  "→ (values RECORDS GAPS) around HIT: the output records (every stream) of HIT's
log with lines in [line − CONTEXT, line + CONTEXT], and the SUPERVISOR-PROTOCOL:
LOG-GAPs that range touches (as LOG-READ reports them)."
  (declare (ignore index hit context))
  (%h8c-pending "hit-excerpt"))
