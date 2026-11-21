(in-package #:supervisor-protocol/search)

;;; The index. Per log: the output records of each retained segment, tokenized
;;; (TOKENIZE of the record text) and classified (CLASSIFY-LINES per stream),
;;; held as line-level postings. A sealed segment (not the newest one, or the
;;; newest of a closed log) is persisted as NNNNNNNN.six next to its .seg.
;;;
;;; .six layout (little-endian): bytes 0-3 "SPSI", u32 +SEARCH-INDEX-FORMAT+,
;;; u32 +TOKENIZER-VERSION+, u64 segment number, u64 the segment file's byte
;;; size, then the implementation's body, then a u32 CRC-32 (IEEE, as the
;;; segment records) of every byte before it. The bytes are a function of the
;;; segment's content only: rebuilding gives a byte-identical file. Readers treat
;;; it as untrusted: a missing file, another format or tokenizer version, another
;;; segment number or size, a bad CRC, or a body that does not parse → rebuild
;;; from the segment and rewrite atomically (cl-stack-pathlib fs-write-atomic).
;;; The newest segment of an open log is indexed in memory only.
;;;
;;; Drops: a segment that is no longer listed (LOG-SEGMENTS) is forgotten at the
;;; next refresh and its .six deleted; nothing in a gap is searchable.

;;; ------------------------------------------------------------------
;;; Refs (given)

(defun format-log-ref (key segment line)
  "→ \"<key>:<segment>:<line>\"."
  (format nil "~A:~D:~D" key segment line))

(defun parse-log-ref (string)
  "→ (values KEY SEGMENT LINE) for \"<key>:<segment>:<line>\" (the last two fields
decimal; KEY non-empty and may itself contain colons), else NIL."
  (check-type string string)
  (let* ((c2 (position #\: string :from-end t))
         (c1 (and c2 (position #\: string :from-end t :end c2))))
    (flet ((num (start end)
             (and (< start end)
                  (every #'digit-char-p (subseq string start end))
                  (parse-integer string :start start :end end))))
      (let ((segment (and c1 (num (1+ c1) c2)))
            (line (and c2 (num (1+ c2) (length string)))))
        (if (and segment line (plusp c1))
            (values (subseq string 0 c1) segment line)
            nil)))))

;;; ------------------------------------------------------------------
;;; Construction and logs

(defun make-log-index (&key supervisor state-directory (previous t) (persist t)
                            (block-lines +default-block-lines+) (k1 +default-bm25-k1+)
                            (b +default-bm25-b+) (kind-boost +default-kind-boost+)
                            embedder)
  "→ a LOG-INDEX.
SUPERVISOR: the index covers every log FIND-UNIT-LOGS finds under its state
directory for its id (units evicted from the registry included), keyed by unit id;
with PREVIOUS (default T) also the other supervisors' logs in that directory,
keyed by directory name (<supervisor-id>--<unit-id>). STATE-DIRECTORY without a
SUPERVISOR: every log in it, keyed by directory name (PREVIOUS is ignored). Neither:
only logs added with INDEX-ADD-LOG. A supervisor without a state directory has
no logs. New logs are discovered at every refresh.
PERSIST NIL: no .six files are read or written. BLOCK-LINES, K1, B, KIND-BOOST:
the BM25 parameters (see SEARCH-LOGS). EMBEDDER: see EMBED-TEXTS.
Thread-safe: one lock per index; searches from several threads are fine."
  (check-type block-lines (integer 1))
  (check-type k1 (real 0))
  (check-type b (real 0 1))
  (check-type kind-boost (real 0))
  (when (and supervisor (not (typep supervisor 'sp:supervisor)))
    (error 'type-error :datum supervisor :expected-type 'sp:supervisor))
  (make-instance 'log-index :supervisor supervisor
                            :state-directory (or state-directory
                                                 (and supervisor (sp:supervisor-state-directory supervisor)))
                            :previous previous :persist persist :block-lines block-lines
                            :k1 k1 :b b :kind-boost kind-boost :embedder embedder))

(defun index-add-log (index designator &key key)
  "Add a log (any SUPERVISOR-PROTOCOL:UNIT-LOG designator: a UNIT-LOG, UNIT,
LOG-SINK or directory) under KEY (default: the directory name, or the unit id of
a unit of the index's supervisor). Adding the same directory again is a no-op.
A KEY already used for another directory → SEARCH-ERROR. Not a log →
SUPERVISOR-PROTOCOL:LOG-NOT-FOUND. → KEY. Closed index → INDEX-CLOSED."
  (declare (ignore index designator key))
  (%h8c-pending "index-add-log"))

(defun index-remove-log (index key)
  "Forget the log under KEY (its files stay). → T, or NIL when unknown."
  (declare (ignore index key))
  (%h8c-pending "index-remove-log"))

(defun index-log-keys (index)
  "→ the keys of every covered log (after discovering new ones): the index's
supervisor's units first, in unit start order (units evicted from the registry by
directory name after them), then the other logs sorted by key (STRING<)."
  (declare (ignore index))
  (%h8c-pending "index-log-keys"))

(defun index-unit-log (index key)
  "→ the SUPERVISOR-PROTOCOL:UNIT-LOG under KEY, or NIL."
  (declare (ignore index key))
  (%h8c-pending "index-unit-log"))

(defun index-log-key (index designator)
  "→ the key the index uses (or would use) for DESIGNATOR's log: the unit id for a
log of the index's supervisor, else the directory name, or the INDEX-ADD-LOG key."
  (declare (ignore index designator))
  (%h8c-pending "index-log-key"))

;;; ------------------------------------------------------------------
;;; Refresh, stats, close

(defun refresh-index (index &key keys)
  "Bring the index up to date with the files: discover new logs, forget dropped
segments (deleting their .six), load valid .six files of sealed segments, and
tokenize only records not indexed yet (incremental: a running unit's new lines,
never the old ones again in this index). SEARCH-LOGS calls it first. KEYS limits
it to those logs. → the number of records tokenized by this call."
  (declare (ignore index keys))
  (%h8c-pending "refresh-index"))

(defun index-stats (index)
  "→ a plist: :LOGS covered logs, :SEGMENTS indexed segments, :RECORDS indexed
output records, :TERMS distinct terms, :TOKENIZED records tokenized since the index
was made (records loaded from .six files are not counted), :LOADED segments loaded
from .six files, :WRITTEN .six files written, :REBUILT .six files that were
present but invalid and were rebuilt."
  (declare (ignore index))
  (%h8c-pending "index-stats"))

(defun close-log-index (index)
  "Write pending .six files of sealed segments and release memory. Idempotent.
Later calls other than INDEX-CLOSED-P and the readers → INDEX-CLOSED. → NIL."
  (declare (ignore index))
  (%h8c-pending "close-log-index"))

(defun index-block-embedding (index key block)
  "→ the vector EMBED-TEXTS returned for block BLOCK (0-based: lines
BLOCK·BLOCK-LINES+1 …) of the log under KEY, or NIL (no embedder, block not
complete yet, or unknown)."
  (declare (ignore index key block))
  (%h8c-pending "index-block-embedding"))
