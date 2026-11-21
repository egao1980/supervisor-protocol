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

;;; ------------------------------------------------------------------
;;; Index state (all guarded by the index LOCK)
;;;
;;; Per log (%ILOG): its segments (%ISEG, by number) with the output lines
;;; (%ILINE) indexed from them; blocks (%IBLOCK: term frequencies and length of
;;; lines 16B+1 … 16B+16, every stream); per stream the lines in line order and
;;; the first one whose stack-trace classification may still change (PENDING,
;;; see %CLASSIFY-TRACES). Each record is parsed from the segment bytes once
;;; (the open newest segment from the byte offset reached last time) and
;;; tokenized at most once; a valid .six gives the terms and one-line kinds of
;;; a sealed segment instead.
;;;
;;; Locking: every public function takes LOCK for its state; file I/O (the
;;; index's own reads and .six writes) runs under it, user code never does:
;;; EMBED-TEXTS and the INDEX-FILE-ERROR warnings run after it is released.

(defstruct (%iline (:conc-name %il-))
  line seg stream time text
  (terms '())                          ; ((term . count) …), first-occurrence order
  (dl 0) (kinds '()) (trace-end nil))

(defstruct (%iseg (:conc-name %is-))
  number (size 0) (offset 0) (last-line 0)
  (lines '())                          ; %ILINEs, ascending
  (pending '())                        ; parsed output records not indexed yet: (line stream time text)
  (fresh t)                            ; nothing of it indexed yet (a .six may be loaded)
  end-line end-time six-ok)

(defstruct (%iblock (:conc-name %ib-))
  (lines '()) (tf (make-hash-table :test 'equal)) (dl 0))

(defstruct (%ilog (:conc-name %ig-))
  key log dir origin
  (segs (make-hash-table))
  (dropped 0)
  (blocks (make-hash-table))
  (streams (list (cons :stdout (make-array 16 :adjustable t :fill-pointer 0))
                 (cons :stderr (make-array 16 :adjustable t :fill-pointer 0))))
  (pending (list (cons :stdout 0) (cons :stderr 0)))
  (dirty '())                          ; (stream . all): classify again from PENDING (from 0 when ALL)
  (last-line 0)
  (emb-next 0)
  (embeddings (make-hash-table)))

(defmacro %with-index ((index) &body body)
  `(bt2:with-lock-held ((slot-value ,index 'lock)) ,@body))

(defun %check-open (index)
  "Signal INDEX-CLOSED (no lock held)."
  (when (index-closed-p index) (error 'index-closed)))

(defun %signal-all (conditions)
  "The INDEX-FILE-ERROR warnings queued under the lock, now that it is released."
  (dolist (c conditions) (warn c)))

(defun %dir-name (directory)
  "The canonical namestring of a log directory (truename when it exists)."
  (let ((dir (uiop:ensure-directory-pathname directory)))
    (uiop:native-namestring (or (handler-case (truename dir) (file-error () nil)) dir))))

(defun %dir-base (directory)
  (car (last (pathname-directory (uiop:ensure-directory-pathname directory)))))

(defun %intern-term (index term)
  (let ((table (slot-value index 'terms)))
    (or (gethash term table) (setf (gethash term table) term))))

;;; ------------------------------------------------------------------
;;; Keys and discovery

(defun %default-key (index log)
  (let ((sup (index-supervisor index)))
    (if (and sup (equal (sp:unit-log-supervisor-id log) (sp:supervisor-id sup))
             (sp:unit-log-unit-id log))
        (sp:unit-log-unit-id log)
        (%dir-base (sp:unit-log-directory log)))))

(defun %discover (index)
  "No lock held. → (values entries own-order): ENTRIES (key log origin) of every log
the index's supervisor or state directory covers; OWN-ORDER the supervisor's keys
(registry units in start order, then evicted ones by directory name)."
  (let ((sup (index-supervisor index))
        (sd (index-state-directory index)))
    (cond
      ((and sup sd)
       (let* ((sid (sp:supervisor-id sup))
              (all (sp:find-unit-logs sd))
              (own (remove-if-not (lambda (l) (equal sid (sp:unit-log-supervisor-id l))) all))
              (registry (mapcar #'sp:unit-id (sp:supervisor-units sup)))
              (own-keys (mapcar #'sp:unit-log-unit-id own))
              (order (append (remove-if-not (lambda (id) (member id own-keys :test #'equal)) registry)
                             (remove-if (lambda (id) (member id registry :test #'equal)) own-keys))))
         (values (append (mapcar (lambda (l) (list (sp:unit-log-unit-id l) l :own)) own)
                         (and (index-previous-p index)
                              (loop for l in all
                                    unless (equal sid (sp:unit-log-supervisor-id l))
                                      collect (list (%dir-base (sp:unit-log-directory l)) l :previous))))
                 order)))
      (sup (values '() '()))
      (sd (values (mapcar (lambda (l) (list (%dir-base (sp:unit-log-directory l)) l :state))
                          (sp:find-unit-logs sd))
                  '()))
      (t (values '() '())))))

(defun %merge-discovered (index entries order)
  "Lock held: logs found by %DISCOVER join the index (unless removed or present)."
  (setf (slot-value index 'own-order) order)
  (let ((logs (slot-value index 'logs))
        (by-dir (slot-value index 'by-dir)))
    (loop for (key log origin) in entries
          for name = (%dir-name (sp:unit-log-directory log))
          unless (or (gethash name (slot-value index 'removed))
                     (gethash name by-dir)
                     (gethash key logs))
            do (setf (gethash key logs)
                     (make-%ilog :key key :log log :dir (sp:unit-log-directory log) :origin origin)
                     (gethash name by-dir) key))))

(defun %discover-into (index)
  "Discovery (no lock held), then merged under the lock. → NIL; INDEX-CLOSED when closed."
  (multiple-value-bind (entries order) (%discover index)
    (unless (%with-index (index)
              (unless (index-closed-p index)
                (%merge-discovered index entries order)
                t))
      (error 'index-closed))))

(defun %ordered-keys (index)
  "Lock held."
  (let* ((logs (slot-value index 'logs))
         (own (remove-if-not (lambda (k) (gethash k logs)) (slot-value index 'own-order)))
         (rest (sort (loop for k being the hash-keys of logs
                           unless (member k own :test #'equal) collect k)
                     #'string<)))
    (append own rest)))

(defun index-add-log (index designator &key key)
  "Add a log (any SUPERVISOR-PROTOCOL:UNIT-LOG designator: a UNIT-LOG, UNIT,
LOG-SINK or directory) under KEY (default: the directory name, or the unit id of
a unit of the index's supervisor). Adding the same directory again is a no-op.
A KEY already used for another directory → SEARCH-ERROR. Not a log →
SUPERVISOR-PROTOCOL:LOG-NOT-FOUND. → KEY. Closed index → INDEX-CLOSED."
  (%check-open index)
  (let* ((log (sp:unit-log designator))
         (name (%dir-name (sp:unit-log-directory log)))
         (key (or key (%default-key index log)))
         (problem nil))
    (check-type key string)
    (let ((result
            (%with-index (index)
              (let* ((logs (slot-value index 'logs))
                     (by-dir (slot-value index 'by-dir))
                     (have (gethash name by-dir))
                     (other (gethash key logs)))
                (cond ((index-closed-p index) (setf problem :closed) nil)
                      ((and other (not (equal name (%dir-name (%ig-dir other)))))
                       (setf problem :key) nil)
                      (have have)
                      (t (remhash name (slot-value index 'removed))
                         (setf (gethash key logs)
                               (make-%ilog :key key :log log :dir (sp:unit-log-directory log)
                                           :origin :added)
                               (gethash name by-dir) key)
                         key))))))
      (case problem
        (:closed (error 'index-closed))
        (:key (error 'search-error
                     :message (format nil "the key ~S is used for another log" key))))
      result)))

(defun index-remove-log (index key)
  "Forget the log under KEY (its files stay). → T, or NIL when unknown."
  (%check-open index)
  (%with-index (index)
    (let ((ilog (gethash key (slot-value index 'logs))))
      (when ilog
        (let ((name (%dir-name (%ig-dir ilog))))
          (remhash key (slot-value index 'logs))
          (remhash name (slot-value index 'by-dir))
          (setf (gethash name (slot-value index 'removed)) t))
        t))))

(defun index-log-keys (index)
  "→ the keys of every covered log (after discovering new ones): the index's
supervisor's units first, in unit start order (units evicted from the registry by
directory name after them), then the other logs sorted by key (STRING<)."
  (%check-open index)
  (%discover-into index)
  (%with-index (index) (%ordered-keys index)))

(defun index-unit-log (index key)
  "→ the SUPERVISOR-PROTOCOL:UNIT-LOG under KEY, or NIL."
  (%check-open index)
  (%discover-into index)
  (%with-index (index)
    (let ((ilog (gethash key (slot-value index 'logs))))
      (and ilog (%ig-log ilog)))))

(defun index-log-key (index designator)
  "→ the key the index uses (or would use) for DESIGNATOR's log: the unit id for a
log of the index's supervisor, else the directory name, or the INDEX-ADD-LOG key."
  (%check-open index)
  (let* ((log (sp:unit-log designator))
         (name (%dir-name (sp:unit-log-directory log))))
    (%discover-into index)
    (or (%with-index (index) (gethash name (slot-value index 'by-dir)))
        (%default-key index log))))

;;; ------------------------------------------------------------------
;;; Lines, blocks, streams

(defun %block-of (index line)
  (floor (1- line) (index-block-lines index)))

(defun %stream-lines (ilog stream) (cdr (assoc stream (%ig-streams ilog))))

(defun %mark-dirty (ilog stream &optional all)
  (let ((entry (assoc stream (%ig-dirty ilog))))
    (if entry
        (when all (setf (cdr entry) t))
        (push (cons stream all) (%ig-dirty ilog)))))

(defun %add-line (index ilog seg iline)
  "Index ILINE (of SEG): its block and its stream."
  (let* ((b (%block-of index (%il-line iline)))
         (blocks (%ig-blocks ilog))
         (block (or (gethash b blocks) (setf (gethash b blocks) (make-%iblock)))))
    (loop for (term . count) in (%il-terms iline)
          do (incf (gethash term (%ib-tf block) 0) count))
    (incf (%ib-dl block) (%il-dl iline))
    (push iline (%ib-lines block))
    (let* ((vec (%stream-lines ilog (%il-stream iline)))
           (n (fill-pointer vec)))
      (vector-push-extend iline vec)
      (if (and (plusp n) (> (%il-line (aref vec (1- n))) (%il-line iline)))
          (progn (setf (cdr (assoc (%il-stream iline) (%ig-streams ilog)))
                       (sort vec #'< :key #'%il-line))
                 (%mark-dirty ilog (%il-stream iline) t))
          (%mark-dirty ilog (%il-stream iline))))
    (setf (%is-lines seg) (nconc (%is-lines seg) (list iline)))))

(defun %forget-segment (index ilog seg)
  "Lock held: SEG's lines leave the blocks and streams; SEG leaves the log."
  (let ((blocks (%ig-blocks ilog)))
    (dolist (iline (%is-lines seg))
      (let* ((b (%block-of index (%il-line iline)))
             (block (gethash b blocks)))
        (when block
          (loop for (term . count) in (%il-terms iline)
                do (let ((left (- (gethash term (%ib-tf block) 0) count)))
                     (if (plusp left)
                         (setf (gethash term (%ib-tf block)) left)
                         (remhash term (%ib-tf block)))))
          (decf (%ib-dl block) (%il-dl iline))
          (setf (%ib-lines block) (delete iline (%ib-lines block)))
          (unless (%ib-lines block) (remhash b blocks))))))
  (when (%is-lines seg)
    (dolist (entry (%ig-streams ilog))
      (let ((kept (remove seg (cdr entry) :key #'%il-seg :test #'eql)))
        (setf (cdr entry) (make-array (length kept) :adjustable t :fill-pointer (length kept)
                                                    :initial-contents kept))
        (%mark-dirty ilog (car entry) t))))
  (remhash (%is-number seg) (%ig-segs ilog)))

(defun %classify-stream (ilog stream all)
  "Stack traces of STREAM again from its PENDING line (from the first when ALL)."
  (let* ((vec (%stream-lines ilog stream))
         (n (length vec))
         (cell (assoc stream (%ig-pending ilog)))
         (p (if all 0 (min (cdr cell) n)))
         (texts (make-array (- n p))))
    (loop for i from p below n
          do (setf (svref texts (- i p)) (%il-text (aref vec i))
                   (%il-trace-end (aref vec i)) nil))
    (multiple-value-bind (traces pending) (%classify-traces texts 0 (- n p))
      (loop for (s . e) in traces
            do (setf (%il-trace-end (aref vec (+ p s))) (%il-line (aref vec (+ p e)))))
      (setf (cdr cell) (+ p pending)))))

(defun %line-kinds-of (iline)
  "The hit kinds of ILINE, in +HIT-KINDS+ order."
  (if (%il-trace-end iline)
      (%merge-kinds (%il-kinds iline) '(:stack-trace))
      (%il-kinds iline)))

;;; ------------------------------------------------------------------
;;; Reading segments

(defun %segment-files (dir)
  "((number . size) …) of the NNNNNNNN.seg files in DIR, ascending."
  (let ((fs pathlib:*filesystem*))
    (sort (loop for p in (and (uiop:directory-exists-p dir)
                              (handler-case (pathlib:fs-iterdir fs dir) (error () nil)))
                for name = (pathname-name p)
                for size = (and (equal "seg" (pathname-type p)) (stringp name) (= 8 (length name))
                                (every #'digit-char-p name)
                                (handler-case (pathlib:fs-file-size fs p) (error () nil)))
                when size collect (cons (parse-integer name) size))
          #'< :key #'car)))

(defun %seg-path (dir number type)
  (merge-pathnames (format nil "~8,'0D.~A" number type) (uiop:ensure-directory-pathname dir)))

(defun %read-new-records (ilog seg)
  "Parse SEG's complete lines past its offset: drops, the end record, new output
records (into PENDING). Updates SIZE / OFFSET."
  (let* ((path (%seg-path (%ig-dir ilog) (%is-number seg) "seg"))
         (start (%is-offset seg))
         (v (handler-case (pathlib:fs-read-range pathlib:*filesystem* path start nil)
              (error () nil))))
    (when v
      (let ((pos 0) (n (length v)) (new '()))
        (loop
          (let ((lf (and (< pos n) (position 10 v :start pos))))
            (unless lf (return))
            (let ((r (sp:parse-log-record v :start pos :end (1+ lf))))
              (when (and r (> (sp:log-record-line r) (%is-last-line seg)))
                (let ((line (sp:log-record-line r)))
                  (setf (%is-last-line seg) line
                        (%ig-last-line ilog) (max line (%ig-last-line ilog)))
                  (case (sp:log-record-type r)
                    (:header
                     (setf (%ig-dropped ilog)
                           (max (%ig-dropped ilog)
                                (or (getf (sp:log-record-detail r) :dropped-through) 0))))
                    (:drop
                     (setf (%ig-dropped ilog)
                           (max (%ig-dropped ilog) (or (getf (sp:log-record-detail r) :segment) 0))))
                    (:end
                     (setf (%is-end-line seg) line
                           (%is-end-time seg) (sp:log-record-wall-time r)))
                    (:output
                     (push (list line (sp:log-record-stream r) (sp:log-record-wall-time r)
                                 (strip-ansi (sp:log-record-text r)))
                           new))))))
            (setf pos (1+ lf))))
        (setf (%is-offset seg) (+ start pos)
              (%is-size seg) (+ start n)
              (%is-pending seg) (nconc (%is-pending seg) (nreverse new)))))))

(defun %tokenize-record (index ilog seg record)
  (destructuring-bind (line stream time text) record
    (let ((counts '()) (dl 0))
      (dolist (term (tokenize text))
        (let ((cell (assoc term counts :test #'string=)))
          (if cell
              (incf (cdr cell))
              (push (cons (%intern-term index term) 1) counts)))
        (incf dl))
      (incf (slot-value index 'tokenized))
      (%add-line index ilog seg
                 (make-%iline :line line :seg seg :stream stream :time time :text text
                              :terms (nreverse counts) :dl dl :kinds (%line-kinds text))))))

;;; ------------------------------------------------------------------
;;; .six files
;;;
;;; Body: u32 R, then R records in line order: u64 line, u8 stream (0 stdout, 1
;;; stderr), u8 one-line kinds (1 :ERROR, 2 :WARNING, 4 :TEST-FAILURE), u32 T,
;;; T × (u16 octets, the UTF-8 term, u32 count) in first-occurrence order.
;;; :STACK-TRACE is not stored: it depends on the stream around the segment.

(defparameter +six-kind-bits+ '((:error . 1) (:warning . 2) (:test-failure . 4)))

(defun %octet-buffer ()
  (make-array 256 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))

(defun %put-uint (buf n bytes)
  (dotimes (k bytes) (vector-push-extend (ldb (byte 8 (* 8 k)) n) buf)))

(defun %get-uint (v pos bytes)
  (when (> (+ pos bytes) (length v)) (error "truncated"))
  (loop for k below bytes sum (ash (aref v (+ pos k)) (* 8 k))))

(defun %utf8 (string) (stack-encoding:encode-to-octets string :encoding :utf-8))

(defun %six-octets (seg)
  "The .six bytes of SEG (a function of its content only)."
  (let ((buf (%octet-buffer)))
    (loop for c across "SPSI" do (vector-push-extend (char-code c) buf))
    (%put-uint buf +search-index-format+ 4)
    (%put-uint buf +tokenizer-version+ 4)
    (%put-uint buf (%is-number seg) 8)
    (%put-uint buf (%is-size seg) 8)
    (%put-uint buf (length (%is-lines seg)) 4)
    (dolist (iline (%is-lines seg))
      (%put-uint buf (%il-line iline) 8)
      (%put-uint buf (if (eq :stderr (%il-stream iline)) 1 0) 1)
      (%put-uint buf (loop for (k . bit) in +six-kind-bits+
                           when (member k (%il-kinds iline)) sum bit)
                 1)
      (%put-uint buf (length (%il-terms iline)) 4)
      (loop for (term . count) in (%il-terms iline)
            for octets = (%utf8 term)
            do (%put-uint buf (length octets) 2)
               (loop for o across octets do (vector-push-extend o buf))
               (%put-uint buf count 4)))
    ;; %CRC32 wants a simple octet vector.
    (let* ((body (coerce buf '(simple-array (unsigned-byte 8) (*))))
           (out (make-array (+ 4 (length body)) :element-type '(unsigned-byte 8))))
      (replace out body)
      (let ((crc (sp::%crc32 body 0 (length body))))
        (dotimes (k 4) (setf (aref out (+ (length body) k)) (ldb (byte 8 (* 8 k)) crc))))
      out)))

(defun %decode-six (index v seg)
  "→ a list of (terms kinds) per pending record of SEG, or NIL when V is not a
valid .six of SEG as it is now."
  (handler-case
      (let ((n (length v)))
        (setf v (coerce v '(simple-array (unsigned-byte 8) (*))))
        (when (and (>= n 36)
                   (every (lambda (c i) (= (char-code c) (aref v i))) "SPSI" '(0 1 2 3))
                   (= (%get-uint v 4 4) +search-index-format+)
                   (= (%get-uint v 8 4) +tokenizer-version+)
                   (= (%get-uint v 12 8) (%is-number seg))
                   (= (%get-uint v 20 8) (%is-size seg))
                   (= (%get-uint v (- n 4) 4) (sp::%crc32 v 0 (- n 4))))
          (let ((pos 28) (out '()) (records (%is-pending seg)) (end (- n 4)))
            (flet ((take (bytes) (prog1 (%get-uint v pos bytes) (incf pos bytes))))
              (let ((count (take 4)))
                (unless (= count (length records)) (return-from %decode-six nil))
                (dolist (r records)
                  (destructuring-bind (line stream time text) r
                    (declare (ignore time text))
                    (unless (and (= line (take 8))
                                 (= (take 1) (if (eq stream :stderr) 1 0)))
                      (return-from %decode-six nil))
                    (let* ((bits (take 1))
                           (kinds (loop for (k . bit) in +six-kind-bits+
                                        when (logtest bits bit) collect k))
                           (terms (loop repeat (take 4)
                                        collect (let* ((len (take 2))
                                                       (stop (+ pos len)))
                                                  (when (> stop end) (error "truncated"))
                                                  (let ((term (stack-encoding:decode-octets
                                                               (subseq v pos stop) :encoding :utf-8)))
                                                    (setf pos stop)
                                                    (cons (%intern-term index term) (take 4)))))))
                      (push (list terms kinds) out))))
                (and (= pos end) (nreverse out)))))))
    (error () nil)))

(defun %load-six (index ilog seg)
  "Try SEG's .six: → :LOADED (lines indexed from it), :INVALID (present, not
valid) or :MISSING."
  (let* ((path (%seg-path (%ig-dir ilog) (%is-number seg) "six"))
         (v (and (probe-file path)
                 (handler-case (pathlib:fs-read-range pathlib:*filesystem* path 0 nil)
                   (error () nil)))))
    (if (null v)
        :missing
        (let ((decoded (%decode-six index v seg)))
          (if (null decoded)
              :invalid
              (progn
                (loop for r in (%is-pending seg)
                      for (terms kinds) in decoded
                      do (destructuring-bind (line stream time text) r
                           (%add-line index ilog seg
                                      (make-%iline :line line :seg seg :stream stream :time time
                                                   :text text :terms terms
                                                   :dl (loop for c in terms sum (cdr c))
                                                   :kinds kinds))))
                (setf (%is-pending seg) '() (%is-six-ok seg) t)
                (incf (slot-value index 'loaded))
                :loaded))))))

(defun %write-six (index ilog seg)
  "Write SEG's .six atomically. → NIL, or an INDEX-FILE-ERROR to signal later."
  (let* ((dir (%ig-dir ilog))
         (path (%seg-path dir (%is-number seg) "six")))
    (handler-case
        (progn
          (pathlib:fs-write-atomic pathlib:*filesystem* path (%six-octets seg))
          (setf (%is-six-ok seg) t)
          (incf (slot-value index 'written))
          ;; The sink dropped the segment meanwhile: no orphan .six.
          (unless (probe-file (%seg-path dir (%is-number seg) "seg"))
            (pathlib:fs-unlink pathlib:*filesystem* path :missing-ok t))
          nil)
      (error (e) (make-condition 'index-file-error :path path :cause e)))))

(defun %delete-six (ilog number)
  "→ NIL, or an INDEX-FILE-ERROR."
  (let ((path (%seg-path (%ig-dir ilog) number "six")))
    (handler-case (progn (pathlib:fs-unlink pathlib:*filesystem* path :missing-ok t) nil)
      (error (e) (make-condition 'index-file-error :path path :cause e)))))

;;; ------------------------------------------------------------------
;;; Refresh

(defun %newest-number (ilog)
  (loop for n being the hash-keys of (%ig-segs ilog) maximize n))

(defun %sealed-p (ilog seg)
  "Not the newest segment, or the newest of a closed log."
  (or (%is-end-line seg) (< (%is-number seg) (%newest-number ilog))))

(defun %log-closed-p (ilog)
  (let ((newest (gethash (%newest-number ilog) (%ig-segs ilog))))
    (and newest (%is-end-line newest) newest)))

(defun %refresh-log (index ilog)
  "Lock held. → (values records-tokenized conditions)."
  (let* ((before (slot-value index 'tokenized))
         (persist (index-persist-p index))
         (segs (%ig-segs ilog))
         (files (%segment-files (%ig-dir ilog)))
         (conditions '()))
    ;; 1. New bytes of changed segments (a sealed one that changed is read again).
    (loop for (n . size) in files
          do (let ((seg (gethash n segs)))
               (when (and seg (/= size (%is-size seg))
                          (or (< size (%is-size seg)) (%is-end-line seg)))
                 (%forget-segment index ilog seg)
                 (setf seg nil))
               (unless seg
                 (setf seg (setf (gethash n segs) (make-%iseg :number n))))
               (when (/= size (%is-size seg))
                 (%read-new-records ilog seg))))
    ;; 2. Dropped or vanished segments are forgotten (and their .six deleted).
    (loop for seg in (loop for s being the hash-values of segs collect s)
          when (or (<= (%is-number seg) (%ig-dropped ilog))
                   (not (assoc (%is-number seg) files)))
            do (%forget-segment index ilog seg)
               (when persist
                 (let ((c (%delete-six ilog (%is-number seg))))
                   (when c (push c conditions)))))
    (setf (%ig-last-line ilog)
          (loop for s being the hash-values of segs maximize (%is-last-line s)))
    ;; 3. Index what is new: a sealed segment seen for the first time from its .six.
    (dolist (seg (sort (loop for s being the hash-values of segs collect s) #'< :key #'%is-number))
      (when (and (%is-fresh seg) persist (%sealed-p ilog seg) (= (%is-offset seg) (%is-size seg)))
        (when (eq :invalid (%load-six index ilog seg))
          (incf (slot-value index 'rebuilt))))
      (setf (%is-fresh seg) nil)
      (dolist (r (shiftf (%is-pending seg) '()))
        (%tokenize-record index ilog seg r)))
    ;; 4. Stack traces of the streams that changed.
    (loop for (stream . all) in (shiftf (%ig-dirty ilog) '())
          do (%classify-stream ilog stream all))
    ;; 5. Sealed segments are persisted.
    (when persist
      (loop for seg being the hash-values of segs
            when (and (not (%is-six-ok seg)) (%sealed-p ilog seg))
              do (let ((c (%write-six index ilog seg)))
                   (when c (push c conditions)))))
    (values (- (slot-value index 'tokenized) before) (nreverse conditions))))

(defun %claim-embeddings (index ilog)
  "Lock held: the blocks completed since the last claim. → (blocks . texts), or NIL."
  (let* ((bl (index-block-lines index))
         (closed (%log-closed-p ilog))
         (last (%ig-last-line ilog))
         (limit (if closed
                    (1+ (%block-of index (max 1 last)))
                    ;; complete: the log has a record past the block's last line
                    (floor (max 0 (1- last)) bl)))
         (blocks '()) (texts '()))
    (loop for b from (%ig-emb-next ilog) below limit
          for block = (gethash b (%ig-blocks ilog))
          when block
            do (push b blocks)
               (push (format nil "~{~A~^~%~}"
                             (mapcar #'%il-text (sort (copy-list (%ib-lines block)) #'< :key #'%il-line)))
                     texts))
    (when (> limit (%ig-emb-next ilog))
      (setf (%ig-emb-next ilog) limit))
    (and blocks (cons (nreverse blocks) (nreverse texts)))))

(defun %run-embeddings (index claims)
  "No lock held: one EMBED-TEXTS call per log, the vectors stored under the lock.
An error releases the claim (the next refresh tries again) and propagates."
  (let ((embedder (index-embedder index)))
    (loop for (ilog blocks . texts) in claims
          do (let ((done nil))
               (unwind-protect
                    (let ((vectors (embed-texts embedder texts)))
                      (%with-index (index)
                        (loop for b in blocks
                              for v in vectors
                              do (setf (gethash b (%ig-embeddings ilog)) v)))
                      (setf done t))
                 (unless done
                   (%with-index (index)
                     (setf (%ig-emb-next ilog) (min (%ig-emb-next ilog) (first blocks))))))))))

(defun %refresh (index keys)
  "→ the number of records tokenized. Discovery and user code run without the lock."
  (%discover-into index)
  (let ((count 0) (conditions '()) (claims '()) (closed nil))
    (%with-index (index)
      (if (index-closed-p index)
          (setf closed t)
          (dolist (key (%ordered-keys index))
            (when (or (null keys) (member key keys :test #'equal))
              (let ((ilog (gethash key (slot-value index 'logs))))
                (multiple-value-bind (n cs) (%refresh-log index ilog)
                  (incf count n)
                  (setf conditions (append conditions cs)))
                (when (index-embedder index)
                  (let ((claim (%claim-embeddings index ilog)))
                    (when claim (push (cons ilog claim) claims)))))))))
    (when closed (error 'index-closed))
    (%signal-all conditions)
    (when claims (%run-embeddings index (nreverse claims)))
    count))

(defun refresh-index (index &key keys)
  "Bring the index up to date with the files: discover new logs, forget dropped
segments (deleting their .six), load valid .six files of sealed segments, and
tokenize only records not indexed yet (incremental: a running unit's new lines,
never the old ones again in this index). SEARCH-LOGS calls it first. KEYS limits
it to those logs. → the number of records tokenized by this call."
  (%check-open index)
  (%refresh index keys))

(defun index-stats (index)
  "→ a plist: :LOGS covered logs, :SEGMENTS indexed segments, :RECORDS indexed
output records, :TERMS distinct terms, :TOKENIZED records tokenized since the index
was made (records loaded from .six files are not counted), :LOADED segments loaded
from .six files, :WRITTEN .six files written, :REBUILT .six files that were
present but invalid and were rebuilt."
  (%check-open index)
  (%with-index (index)
    (let ((segments 0) (records 0) (terms (make-hash-table :test 'equal)))
      (loop for ilog being the hash-values of (slot-value index 'logs)
            do (loop for seg being the hash-values of (%ig-segs ilog)
                     do (incf segments)
                        (incf records (length (%is-lines seg))))
               (loop for block being the hash-values of (%ig-blocks ilog)
                     do (loop for term being the hash-keys of (%ib-tf block)
                              do (setf (gethash term terms) t))))
      (list :logs (hash-table-count (slot-value index 'logs))
            :segments segments :records records :terms (hash-table-count terms)
            :tokenized (slot-value index 'tokenized) :loaded (slot-value index 'loaded)
            :written (slot-value index 'written) :rebuilt (slot-value index 'rebuilt)))))

(defun close-log-index (index)
  "Write pending .six files of sealed segments and release memory. Idempotent.
Later calls other than INDEX-CLOSED-P and the readers → INDEX-CLOSED. → NIL."
  (let ((conditions '()))
    (%with-index (index)
      (unless (index-closed-p index)
        (when (index-persist-p index)
          (loop for ilog being the hash-values of (slot-value index 'logs)
                do (loop for seg being the hash-values of (%ig-segs ilog)
                         when (and (not (%is-six-ok seg)) (null (%is-pending seg))
                                   (%sealed-p ilog seg))
                           do (let ((c (%write-six index ilog seg)))
                                (when c (push c conditions))))))
        (clrhash (slot-value index 'logs))
        (clrhash (slot-value index 'by-dir))
        (clrhash (slot-value index 'terms))
        (setf (slot-value index 'closed) t)))
    (%signal-all (nreverse conditions))
    nil))

(defun index-block-embedding (index key block)
  "→ the vector EMBED-TEXTS returned for block BLOCK (0-based: lines
BLOCK·BLOCK-LINES+1 …) of the log under KEY, or NIL (no embedder, block not
complete yet, or unknown)."
  (%check-open index)
  (%with-index (index)
    (let ((ilog (gethash key (slot-value index 'logs))))
      (and ilog (values (gethash block (%ig-embeddings ilog)))))))
