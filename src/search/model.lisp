(in-package #:supervisor-protocol/search)

;;; ------------------------------------------------------------------
;;; Constants

(defconstant +search-index-format+ 1
  "Version of the per-segment index file layout (NNNNNNNN.six).")

(defconstant +tokenizer-version+ 1
  "Version of TOKENIZE and of the kind rules. An index file written by another
version is rebuilt.")

(defparameter +hit-kinds+ '(:error :warning :stack-trace :test-failure :exit)
  "Structured hit kinds, in this order wherever a list of kinds is returned.
:ERROR / :WARNING / :TEST-FAILURE come from one line; :STACK-TRACE marks the
first line of a trace (HIT-EXTENT covers the whole trace); :EXIT is a log's end
record.")

(defconstant +default-block-lines+ 16
  "Lines per BM25 document: block B of a log holds lines 16B+1 … 16B+16.")

(defparameter +default-bm25-k1+ 1.2)
(defparameter +default-bm25-b+ 0.75)

(defparameter +default-kind-boost+ 0.5
  "Added to a block's BM25 score when its hit line has :ERROR, :STACK-TRACE or
:TEST-FAILURE.")

(defconstant +max-term-length+ 64
  "Longer terms are dropped (their parts are still indexed).")

(defconstant +default-search-k+ 10)

;;; ------------------------------------------------------------------
;;; Conditions (CLHS 9)

(define-condition search-condition (condition) ())

(define-condition search-error (search-condition error)
  ((message :initarg :message :initform nil :reader search-error-message))
  (:report (lambda (c s)
             (format s "Log search error~@[: ~A~]" (search-error-message c)))))

(define-condition index-closed (search-error)
  ()
  (:report (lambda (c s)
             (declare (ignore c))
             (format s "The log index is closed"))))

(define-condition index-file-error (search-condition warning)
  ((path :initarg :path :initform nil :reader index-file-error-path)
   (cause :initarg :cause :initform nil :reader index-file-error-cause))
  (:documentation "A WARNING: an index file could not be written or deleted. The
index keeps that segment in memory and carries on.")
  (:report (lambda (c s)
             (format s "Cannot write log index file~@[ ~A~]~@[: ~A~]"
                     (let ((p (index-file-error-path c))) (and p (uiop:native-namestring p)))
                     (index-file-error-cause c)))))

;;; ------------------------------------------------------------------
;;; Classes

(defclass log-index ()
  ((supervisor :initarg :supervisor :initform nil :reader index-supervisor)
   (state-directory :initarg :state-directory :initform nil :reader index-state-directory)
   (previous :initarg :previous :initform t :reader index-previous-p)
   (persist :initarg :persist :initform t :reader index-persist-p)
   (block-lines :initarg :block-lines :initform +default-block-lines+ :reader index-block-lines)
   (k1 :initarg :k1 :initform +default-bm25-k1+ :reader index-k1)
   (b :initarg :b :initform +default-bm25-b+ :reader index-b)
   (kind-boost :initarg :kind-boost :initform +default-kind-boost+ :reader index-kind-boost)
   (embedder :initarg :embedder :initform nil :reader index-embedder)
   (closed :initform nil :reader index-closed-p)
   (lock :initform (bt2:make-lock :name "log-index"))
   ;; internal state, guarded by LOCK (src/search/index.lisp)
   (logs :initform (make-hash-table :test 'equal) :documentation "Key → %ILOG.")
   (by-dir :initform (make-hash-table :test 'equal) :documentation "Directory namestring → key.")
   (removed :initform (make-hash-table :test 'equal)
            :documentation "Directories INDEX-REMOVE-LOG took out of discovery.")
   (own-order :initform '() :documentation "The supervisor's keys, unit start order first.")
   (terms :initform (make-hash-table :test 'equal) :documentation "Term interning.")
   (tokenized :initform 0)
   (loaded :initform 0)
   (written :initform 0)
   (rebuilt :initform 0))
  (:documentation "An incremental index over unit logs. See MAKE-LOG-INDEX."))

(defmethod print-object ((index log-index) stream)
  (print-unreadable-object (index stream :type t :identity t)
    (format stream "~@[~A~]" (let ((s (index-supervisor index))) (and s (sp:supervisor-id s))))))

(defclass search-hit ()
  ((key :initarg :key :reader hit-key
        :documentation "The log's key: a unit id for the index's supervisor, else the
log directory name (<supervisor-id>--<unit-id>), or INDEX-ADD-LOG's key.")
   (log :initarg :log :reader hit-log :documentation "The SUPERVISOR-PROTOCOL:UNIT-LOG.")
   (segment :initarg :segment :reader hit-segment)
   (line :initarg :line :reader hit-line)
   (kinds :initarg :kinds :initform nil :reader hit-kinds
          :documentation "The hit line's kinds, in +HIT-KINDS+ order.")
   (score :initarg :score :initform nil :reader hit-score
          :documentation "BM25 + kind boost (a REAL); NIL for a query without terms.")
   (stream :initarg :stream :initform nil :reader hit-stream
           :documentation ":STDOUT / :STDERR; NIL for an :EXIT hit.")
   (time :initarg :time :initform nil :reader hit-time :documentation "Wall ms of the hit line.")
   (text :initarg :text :initform "" :reader hit-text
         :documentation "The hit line's text, ANSI escapes stripped; \"[end of output]\" for :EXIT.")
   (extent :initarg :extent :initform nil :reader hit-extent
           :documentation "(FIRST . LAST) lines of the stack trace for a :STACK-TRACE hit, else NIL."))
  (:documentation "One search result: a line of a log."))

(defun hit-ref (hit)
  "\"<key>:<segment>:<line>\" (FORMAT-LOG-REF)."
  (format nil "~A:~D:~D" (hit-key hit) (hit-segment hit) (hit-line hit)))

(defmethod print-object ((hit search-hit) stream)
  (print-unreadable-object (hit stream :type t)
    (format stream "~A~{ ~S~}" (hit-ref hit) (hit-kinds hit))))

;;; ------------------------------------------------------------------
;;; Embeddings hook (no default implementation)

(defgeneric embed-texts (embedder texts)
  (:documentation "→ a list of vectors (one (SIMPLE-ARRAY SINGLE-FLOAT (*)) per string
of TEXTS, in order) for EMBEDDER. The hook for optional semantic search: with
MAKE-LOG-INDEX :EMBEDDER E, each refresh calls (EMBED-TEXTS E texts) once per log
whose blocks it completed (a block is complete when the log has a record past its
last line, or the log is closed), TEXTS being those blocks' output lines joined by
LF, in block order; every block is embedded exactly once. The vectors are kept
(INDEX-BLOCK-EMBEDDING). Ranking
in 0.3.0 ignores them. supervisor-protocol defines no method: a backend over
llm-protocol embeddings is a later rung (H8c-1)."))
