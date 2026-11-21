(defpackage #:supervisor-protocol/search
  (:use #:cl)
  (:local-nicknames (#:sp #:supervisor-protocol)
                    (#:pathlib #:cl-stack-pathlib)
                    (#:stack-encoding #:encoding-protocol))
  (:documentation "H8c: an incremental lexical index over supervisor unit logs.
Line records are tokenized for code and log text, scored with BM25 over blocks
of consecutive lines, and tagged with structured hit kinds (:ERROR :WARNING
:STACK-TRACE :TEST-FAILURE :EXIT). Per-segment index files live next to the
segments and go with them. Pure Lisp; no agent or LLM dependency.")
  (:export
   ;; constants
   #:+search-index-format+
   #:+tokenizer-version+
   #:+hit-kinds+
   #:+default-block-lines+
   #:+default-bm25-k1+
   #:+default-bm25-b+
   #:+default-kind-boost+
   #:+max-term-length+
   #:+default-search-k+
   ;; tokenizer
   #:strip-ansi
   #:tokenize
   #:query-terms
   ;; structured kinds
   #:line-kinds
   #:classify-lines
   ;; refs
   #:format-log-ref
   #:parse-log-ref
   ;; index
   #:log-index
   #:make-log-index
   #:index-supervisor
   #:index-state-directory
   #:index-previous-p
   #:index-persist-p
   #:index-block-lines
   #:index-k1
   #:index-b
   #:index-kind-boost
   #:index-embedder
   #:index-closed-p
   #:index-add-log
   #:index-remove-log
   #:index-log-keys
   #:index-unit-log
   #:index-log-key
   #:refresh-index
   #:index-stats
   #:close-log-index
   ;; search
   #:search-logs
   #:search-hit
   #:hit-key
   #:hit-log
   #:hit-segment
   #:hit-line
   #:hit-ref
   #:hit-kinds
   #:hit-score
   #:hit-stream
   #:hit-time
   #:hit-text
   #:hit-extent
   #:hit-excerpt
   ;; embeddings hook
   #:embed-texts
   #:index-block-embedding
   ;; conditions
   #:search-condition
   #:search-error
   #:index-closed
   #:index-file-error
   #:index-file-error-path
   #:index-file-error-cause))
