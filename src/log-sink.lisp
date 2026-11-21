(in-package #:supervisor-protocol)

;;; H8b log sinks: a unit's output as rotated, size-capped segment files.
;;;
;;; Directory: (UNIT-LOG-PATHNAME state-directory supervisor-id unit-id) =
;;; <state-directory>/units/<supervisor-id>--<unit-id>/ (next to the unit's
;;; .unit file; the supervisor id keeps two supervisors on one state directory
;;; apart). Files: NNNNNNNN.seg (segment N, 8 decimal digits, from 00000001) and
;;; NNNNNNNN.idx (its index). The directory is made by MAKE-LOG-SINK; segment 1
;;; is opened with the first record (a sink closed before any output still gets
;;; segment 1 with its header and end record).
;;;
;;; Writing. SINK-WRITE-OCTETS splits each stream's bytes at LF into :OUTPUT
;;; records (record format: src/log-format.lisp). Per call and stream: every
;;; complete line becomes a '.' record; what is left after the last LF becomes
;;; a '+' record at once (nothing waits in memory for a line feed), except an
;;; incomplete UTF-8 sequence at the very end (at most 3 octets), which is kept
;;; for that stream's next call and flushed by SINK-CLOSE. A line longer than
;;; MAX-RECORD octets is split into '+' records of at most MAX-RECORD octets
;;; (the split moves back to a UTF-8 character boundary when the bytes are
;;; valid UTF-8 there). Lines are numbered in arrival order across both streams.
;;; Every call ends with FINISH-OUTPUT on the segment, so readers in this and
;;; other images see the records at once, and a SIGKILL loses at most a torn
;;; last line. Everything a unit wrote is in its records before its final state
;;; is published (pumps joined, thread streams flushed); the :END record follows
;;; from SINK-CLOSE in the finish path. The sink is a HEAD-TAIL-BUFFER too: the same bytes, decoded,
;;; feed the in-memory head + tail (SINK-TEXT, UNIT-OUTPUT, SINK-STATS count
;;; characters as in 0.1.0).
;;;
;;; Rotation. No segment file exceeds SEGMENT-SIZE bytes: when the next record
;;; would not fit, the segment and its index are closed and segment N+1 is
;;; opened with its :HEADER record. When more than SEGMENTS segments would be
;;; retained, the oldest is dropped: a :DROP marker (reason :ROTATED) goes into
;;; the new segment right after its header, then the file and its index are
;;; deleted. Dropped segments are always the oldest ones (a prefix by number),
;;; so readers ignore any segment numbered at or below the highest
;;; :DROPPED-THROUGH / :DROP :SEGMENT they see.
;;;
;;; Windows. SBCL opens files with FILE_SHARE_READ | FILE_SHARE_WRITE only (no
;;; FILE_SHARE_DELETE, src/code/win32.lisp), so a segment that a reader holds
;;; open cannot be deleted. The drop is logical first (marker written, segment
;;; no longer listed or read), the delete is physical and deferred: a delete
;;; that fails is retried at every later rotation, at SINK-CLOSE and at
;;; SHUTDOWN-SUPERVISOR. Readers open a segment only for the duration of one
;;; call. Unix deletes at once (an open reader keeps reading the old inode).
;;;
;;; Index (NNNNNNNN.idx, little-endian): bytes 0-3 "SPLX", 4-7 u32 stride,
;;; 8-15 u64 first line of the segment, then one u64 byte offset per STRIDE
;;; records: entry k = offset of line first-line + k × stride (entry 0 = 0, the
;;; header). Appended as records are written, flushed with the segment. A reader
;;; that finds it missing, with a bad header, or with an entry that does not
;;; point at the expected line, scans the segment instead and rewrites the
;;; index atomically unless the segment is the newest one of an open log (no
;;; :END record). A partial last entry is ignored.
;;;
;;; Errors. Any ERROR while opening, writing or flushing a segment or index →
;;; LOG-WRITE-ERROR (src/conditions.lisp) with RETRY / DROP-TO-MEMORY-ONLY /
;;; ABORT-UNIT. Memory-only is permanent for the sink: later output goes to the
;;; head + tail buffer only; LOG-SINK-MEMORY-ONLY-P becomes T.

;;; Locks. One recursive writer lock per sink (WLOCK) guards every slot below
;;; and the files; it is held across file I/O and while LOG-WRITE-ERROR is
;;; signalled (a handler that writes to the same sink on the same thread
;;; re-enters, see WRITER, and feeds the head + tail only). Inside it: the
;;; head + tail LOCK (leaf), the supervisor lock (:LOG-ERROR events and the
;;; ABORT-UNIT request, no user code), the supervisor's log registry lock
;;; (leaf) and the follow lock NLOCK (leaf). Nothing takes WLOCK while holding
;;; any of those. The total cap runs after WLOCK is released.

(defclass log-sink (head-tail-buffer)
  ((directory :initarg :directory :reader log-sink-directory)
   (unit :initarg :unit :initform nil :reader log-sink-unit)
   (supervisor-id :initarg :supervisor-id :initform nil :reader log-sink-supervisor-id)
   (unit-id :initarg :unit-id :initform nil :reader log-sink-unit-id)
   (kind :initarg :kind :initform nil :reader log-sink-kind)
   (segment-size :initarg :segment-size :reader log-sink-segment-size)
   (segments :initarg :segments :reader log-sink-segments)
   (max-record :initarg :max-record :reader log-sink-max-record)
   (index-stride :initarg :index-stride :reader log-sink-index-stride)
   (memory-only-p :initform nil :reader log-sink-memory-only-p)
   ;; internal writer state, guarded by WLOCK
   (wlock :initform (bt2:make-recursive-lock :name "log-sink"))
   (writer :initform nil :documentation "The thread inside the writer, or NIL.")
   (log-closed-p :initform nil)
   (seg-number :initform 0 :documentation "The newest segment, 0 before the first.")
   (seg-path :initform nil)
   (idx-path :initform nil)
   (seg-stream :initform nil)
   (idx-stream :initform nil)
   (seg-bytes :initform 0 :documentation "Bytes of the newest segment on disk.")
   (seg-first-line :initform 0)
   (seg-last-line :initform 0)
   (seg-records :initform 0)
   (seg-output-bytes :initform 0)
   (retained :initform '() :documentation "%SEGs older than the newest, oldest first.")
   (next-line :initform 1)
   (dropped-through :initform 0)
   (dropped-records :initform 0)
   (dropped-bytes :initform 0)
   (output-bytes :initform 0 :documentation "Every output octet accepted (the :END :BYTES).")
   (carry-out :initform nil)
   (carry-err :initform nil)
   (buf :initform (%make-obuf 16384) :documentation "Records not yet written to the segment.")
   (ibuf :initform (%make-obuf 256) :documentation "Index bytes not yet written.")
   (wall :initform 0)
   (mono :initform 0)
   (rotated-p :initform nil)
   (flushed :initform 0 :documentation "Bytes written to files since the supervisor was told.")
   (deferred :initform '() :documentation "Paths whose delete failed (Windows); retried.")
   (entry :initform nil :documentation "The supervisor's %LOG-ENTRY, NIL for a standalone sink.")
   ;; follow notifications
   (nlock :initform (bt2:make-lock :name "log-sink follow"))
   (ncv :initform (bt2:make-condition-variable :name "log-sink follow"))
   (version :initform 0))
  (:documentation "An OUTPUT-SINK writing rotated segment files (see above) and keeping
the head + tail in memory. Thread-safe; readers in any thread or image may read
its files while it writes."))

(defstruct (%seg (:conc-name %seg-))
  number first-line last-line records output-bytes)

(defstruct (%log-entry (:conc-name %le-))
  "One log a supervisor wrote (the total cap and SUPERVISOR-LOG-USAGE)."
  supervisor directory sink finished stub-p)

;;; Defined later (src/events.lisp, src/supervisor.lisp).
(declaim (ftype function %publish %register-log %log-sink-finished %log-cap-changed
                %log-grew %abort-unit-no-wait))

(defun unit-log-pathname (state-directory supervisor-id unit-id)
  "<STATE-DIRECTORY>/units/<SUPERVISOR-ID>--<UNIT-ID>/ (a directory pathname)."
  (merge-pathnames (make-pathname :directory (list :relative "units"
                                                   (format nil "~A--~A" supervisor-id unit-id)))
                   (uiop:ensure-directory-pathname state-directory)))

(defun %segment-pathname (directory number type)
  "DIRECTORY/NNNNNNNN.<TYPE> (TYPE \"seg\" or \"idx\")."
  (merge-pathnames (format nil "~8,'0D.~A" number type) (uiop:ensure-directory-pathname directory)))

(defparameter +make-log-sink-keys+
  '(:class :unit :supervisor-id :unit-id :kind :segment-size :segments :max-record
    :index-stride :head-size :tail-size))

(defun make-log-sink (directory &rest initargs
                      &key (class 'log-sink) unit supervisor-id unit-id kind
                        (segment-size +default-log-segment-size+)
                        (segments +default-log-segments+)
                        (max-record +default-log-max-record+)
                        (index-stride +log-index-stride+)
                        (head-size +default-head-size+) (tail-size +default-tail-size+)
                      &allow-other-keys)
  "A LOG-SINK (or an instance of CLASS, a subclass) writing into DIRECTORY (created).
UNIT: the unit it belongs to (supplies SUPERVISOR-ID, UNIT-ID and KIND when they
are not given, the supervisor's LOG-ERROR-HANDLER, :LOG-ERROR events and the
ABORT-UNIT restart); NIL for a standalone sink. SEGMENT-SIZE must be at least
512 + 2 × MAX-RECORD, else SUPERVISOR-ERROR. Other INITARGS go to MAKE-INSTANCE."
  (check-type segment-size (integer 1))
  (check-type segments (integer 1))
  (check-type max-record (integer 1))
  (check-type index-stride (integer 1))
  (let ((sup (and unit (unit-supervisor unit))))
    (when (< segment-size (+ 512 (* 2 max-record)))
      (error 'supervisor-error
             :supervisor sup
             :message (format nil "log segment size ~D is below 512 + 2 × max-record (~D)"
                              segment-size (+ 512 (* 2 max-record)))))
    (let ((dir (uiop:ensure-directory-pathname directory))
          (others (loop for (k v) on initargs by #'cddr
                        unless (member k +make-log-sink-keys+) append (list k v))))
      (pathlib:fs-mkdir pathlib:*filesystem* dir :parents t :exist-ok t)
      (let ((sink (apply #'make-instance class
                         :directory dir :unit unit
                         :supervisor-id (or supervisor-id (and sup (supervisor-id sup)))
                         :unit-id (or unit-id (and unit (unit-id unit)))
                         :kind (or kind (and unit (unit-kind unit)))
                         :segment-size segment-size :segments segments
                         :max-record max-record :index-stride index-stride
                         :head-size head-size :tail-size tail-size
                         others)))
        (when sup (%register-log sup sink))
        sink))))

(defgeneric log-open-segment-stream (sink pathname)
  (:documentation "Open the file PATHNAME (a segment or an index of SINK) for appending
octets, creating it. → an output stream of (unsigned-byte 8). The sink calls this
for every file it writes; a subclass may specialize it (tests inject failing
streams this way). An ERROR from it or from writing to its stream becomes a
LOG-WRITE-ERROR."))

(defmethod log-open-segment-stream ((sink log-sink) pathname)
  (open pathname :direction :output :element-type '(unsigned-byte 8)
                 :if-exists :append :if-does-not-exist :create))

(defgeneric log-sink-last-line (sink)
  (:documentation "The number of the last record written to disk, 0 before the first."))

(defmethod log-sink-last-line ((sink log-sink))
  (1- (slot-value sink 'next-line)))

;;; ------------------------------------------------------------------
;;; The writer

(defmacro %with-writer ((sink) &body body)
  "BODY under SINK's writer lock. A forced stop of this thread is deferred: the caller
runs %RUN-DEFERRED-STOP once it is done (after the lock is released)."
  `(let ((*in-critical* t))
     (bt2:with-recursive-lock-held ((slot-value ,sink 'wlock)) ,@body)))

(defun %notify (sink)
  "Wake LOG-FOLLOW waiters of SINK."
  (bt2:with-lock-held ((slot-value sink 'nlock))
    (incf (slot-value sink 'version))
    (bt2:condition-broadcast (slot-value sink 'ncv))))

(defun %sink-version (sink)
  (bt2:with-lock-held ((slot-value sink 'nlock)) (slot-value sink 'version)))

(defun %sink-wait (sink version seconds)
  "Wait until SINK's version is not VERSION, or SECONDS passed."
  (let ((deadline (+ (%now) seconds)))
    (bt2:with-lock-held ((slot-value sink 'nlock))
      (loop while (= version (slot-value sink 'version))
            do (let ((left (- deadline (%now))))
                 (when (<= left 0) (return))
                 (bt2:condition-wait (slot-value sink 'ncv) (slot-value sink 'nlock)
                                     :timeout (max left 0.001)))))))

(defvar *wall-base* nil
  "(unix-ms . internal-real-time) when the wall clock was last read from the OS.")

(defun %wall-ms ()
  "Unix epoch milliseconds: SBCL's gettimeofday where it has one, else the universal
time at the first call plus the internal real time since."
  (let ((gtod #+sbcl (find-symbol "GET-TIME-OF-DAY" "SB-EXT") #-sbcl nil))
    (if (and gtod (fboundp gtod))
        (multiple-value-bind (sec usec) (funcall gtod)
          (+ (* sec 1000) (floor usec 1000)))
        (let ((base (or *wall-base*
                        (setf *wall-base* (cons (* 1000 (- (get-universal-time) 2208988800))
                                                (get-internal-real-time))))))
          (+ (car base) (floor (* 1000 (- (get-internal-real-time) (cdr base)))
                               internal-time-units-per-second))))))

(defun %mono-ms ()
  (floor (* 1000 (get-internal-real-time)) internal-time-units-per-second))

(defun %stamp (sink)
  (setf (slot-value sink 'wall) (%wall-ms)
        (slot-value sink 'mono) (%mono-ms)))

;;; Write errors (CLHS 9). LOG-WRITE-ERROR is a signalled condition, not an
;;; ERROR: the writing thread's handlers for it (or a supertype short of ERROR)
;;; see it; handlers on ERROR never do, so an unhandled write error keeps going
;;; (memory-only) instead of unwinding.

(defun %publish-log-error (unit action path cause)
  (let ((sup (unit-supervisor unit)))
    (when sup
      (%with-lock (sup)
        (let ((state (unit-state unit)))
          (%publish sup unit :log-error state state
                    (list :action action
                          :path (ignore-errors (uiop:native-namestring path))
                          :message (or (ignore-errors (princ-to-string cause)) "?"))))))))

(defun %resolve-write-error (sink path cause)
  "Signal LOG-WRITE-ERROR for CAUSE with the restarts; → :RETRY, :MEMORY-ONLY or
:ABORT-UNIT. Publishes one :LOG-ERROR event (sinks with a unit)."
  (let* ((unit (log-sink-unit sink))
         (sup (and unit (unit-supervisor unit)))
         (c (make-condition 'log-write-error :supervisor sup :sink sink :unit unit
                                             :path path :cause cause))
         (action
           (flet ((offer ()
                    (restart-case
                        (progn
                          ;; 1. the writing thread's handlers
                          (signal c)
                          ;; 2. the supervisor's handler
                          (let ((h (and sup (supervisor-log-error-handler sup))))
                            (when h
                              (handler-case (funcall h c)
                                (error () nil))))
                          ;; 3. the default
                          :memory-only)
                      (retry ()
                        :report "Reopen the log segment and write the records again."
                        :retry)
                      (drop-to-memory-only ()
                        :report "Stop writing log files for this sink; keep the head + tail in memory."
                        :memory-only))))
             (if unit
                 (restart-case (offer)
                   (abort-unit ()
                     :report "Stop the unit (forced, without waiting)."
                     :abort-unit))
                 (offer)))))
    (when unit (%publish-log-error unit action path cause))
    action))

(defun %close-quietly (stream)
  (when stream (ignore-errors (close stream))))

(defun %close-streams (sink)
  (%close-quietly (shiftf (slot-value sink 'seg-stream) nil))
  (%close-quietly (shiftf (slot-value sink 'idx-stream) nil)))

(defun %go-memory-only (sink)
  (%close-streams sink)
  (setf (%obuf-fill (slot-value sink 'buf)) 0
        (%obuf-fill (slot-value sink 'ibuf)) 0
        (slot-value sink 'memory-only-p) t))

(defun %stream-of (sink which)
  "The open segment (WHICH :SEG) or index (:IDX) stream, opened when needed."
  (let ((slot (if (eq which :seg) 'seg-stream 'idx-stream)))
    (or (slot-value sink slot)
        (setf (slot-value sink slot)
              (log-open-segment-stream sink (slot-value sink (if (eq which :seg) 'seg-path 'idx-path)))))))

(defun %io (sink which thunk)
  "Run THUNK (file I/O on WHICH, :SEG or :IDX). An ERROR → LOG-WRITE-ERROR: RETRY
reopens and runs THUNK again; memory-only and ABORT-UNIT throw to %MEMORY-ONLY."
  (loop
    (let ((failure (handler-case (progn (funcall thunk) nil)
                     (error (e) e))))
      (unless failure (return t))
      (let ((path (slot-value sink (if (eq which :seg) 'seg-path 'idx-path))))
        (ecase (%resolve-write-error sink path failure)
          (:retry
           (%close-quietly (shiftf (slot-value sink (if (eq which :seg) 'seg-stream 'idx-stream)) nil)))
          (:memory-only
           (%go-memory-only sink)
           (throw '%memory-only nil))
          (:abort-unit
           (%go-memory-only sink)
           (%abort-unit-no-wait (log-sink-unit sink))
           (throw '%memory-only nil)))))))

(defun %flush (sink)
  "Write the pending records and index entries, then FINISH-OUTPUT both."
  (let ((buf (slot-value sink 'buf)) (ibuf (slot-value sink 'ibuf)))
    (when (plusp (%obuf-fill buf))
      (%io sink :seg
           (lambda ()
             (let ((s (%stream-of sink :seg)))
               ;; A retry after a partial write: end the torn fragment first, so it
               ;; stays one bad line and hides no record.
               (let ((size (ignore-errors (file-length s))))
                 (when (and size (> size (slot-value sink 'seg-bytes)))
                   (write-byte 10 s)
                   (setf (slot-value sink 'seg-bytes) (1+ size))))
               (write-sequence (%obuf-vec buf) s :end (%obuf-fill buf))
               (finish-output s))))
      (incf (slot-value sink 'seg-bytes) (%obuf-fill buf))
      (incf (slot-value sink 'flushed) (%obuf-fill buf))
      (setf (%obuf-fill buf) 0))
    (when (plusp (%obuf-fill ibuf))
      (%io sink :idx
           (lambda ()
             (let ((s (%stream-of sink :idx)))
               (write-sequence (%obuf-vec ibuf) s :end (%obuf-fill ibuf))
               (finish-output s))))
      (incf (slot-value sink 'flushed) (%obuf-fill ibuf))
      (setf (%obuf-fill ibuf) 0))))

(defun %index-entry (sink line)
  "An index entry for LINE when it is first-line + k × stride (offset = where it starts)."
  (when (zerop (mod (- line (slot-value sink 'seg-first-line)) (log-sink-index-stride sink)))
    (%obuf-u64 (slot-value sink 'ibuf)
               (+ (slot-value sink 'seg-bytes) (%obuf-fill (slot-value sink 'buf))))))

(defun %append-record (sink tchar cont-p v start end text-p output-p)
  "One record with the next line number into the newest segment (room ensured)."
  (let ((line (slot-value sink 'next-line)))
    (%index-entry sink line)
    (%write-record (slot-value sink 'buf) line (slot-value sink 'wall) (slot-value sink 'mono)
                   tchar cont-p v start end text-p)
    (incf (slot-value sink 'seg-records))
    (setf (slot-value sink 'seg-last-line) line)
    (when output-p
      (incf (slot-value sink 'seg-output-bytes) (+ (- end start) (if cont-p 0 1))))
    (incf (slot-value sink 'next-line))
    line))

(defun %fits-p (sink length)
  (<= (+ (slot-value sink 'seg-bytes) (%obuf-fill (slot-value sink 'buf)) length)
      (log-sink-segment-size sink)))

(defun %ensure-room (sink length)
  "Open segment 1, or rotate when a record of LENGTH octets would not fit (a segment
holding only its header is never rotated)."
  (cond ((zerop (slot-value sink 'seg-number)) (%open-next-segment sink))
        ((and (not (%fits-p sink length)) (> (slot-value sink 'seg-records) 1))
         (%rotate sink))))

(defun %meta-length (sink detail)
  (%record-length (slot-value sink 'next-line) (slot-value sink 'wall) (slot-value sink 'mono)
                  (let ((v (%meta-octets detail))) (%escaped-length v 0 (length v)))))

(defun %append-meta (sink type detail-fn)
  "A meta record; DETAIL-FN is called with its line. Room is the caller's business."
  (let* ((line (slot-value sink 'next-line))
         (v (%meta-octets (funcall detail-fn line))))
    (%append-record sink (%type-char type nil) nil v 0 (length v) t nil)))

(defun %open-next-segment (sink)
  "Segment N+1 (with its index header and :HEADER record) becomes the newest."
  (let* ((n (1+ (slot-value sink 'seg-number)))
         (dir (log-sink-directory sink))
         (first (slot-value sink 'next-line)))
    (setf (slot-value sink 'seg-number) n
          (slot-value sink 'seg-path) (%segment-pathname dir n "seg")
          (slot-value sink 'idx-path) (%segment-pathname dir n "idx")
          (slot-value sink 'seg-bytes) 0
          (slot-value sink 'seg-first-line) first
          (slot-value sink 'seg-last-line) (1- first)
          (slot-value sink 'seg-records) 0
          (slot-value sink 'seg-output-bytes) 0)
    (%io sink :seg (lambda () (%stream-of sink :seg)))
    (%io sink :idx (lambda () (%stream-of sink :idx)))
    (let ((ibuf (slot-value sink 'ibuf)))
      (%obuf-ascii ibuf "SPLX")
      (%obuf-u32 ibuf (log-sink-index-stride sink))
      (%obuf-u64 ibuf first))
    (%append-meta sink :header
                  (lambda (line)
                    (list :format +log-format+
                          :supervisor-id (log-sink-supervisor-id sink)
                          :unit-id (log-sink-unit-id sink)
                          :kind (log-sink-kind sink)
                          :segment n :first-line line
                          :dropped-through (slot-value sink 'dropped-through)
                          :dropped-records (slot-value sink 'dropped-records)
                          :dropped-bytes (slot-value sink 'dropped-bytes))))))

(defun %delete-or-defer (sink path)
  (ignore-errors (pathlib:fs-unlink pathlib:*filesystem* path :missing-ok t))
  (when (probe-file path)
    (push path (slot-value sink 'deferred))))

(defun %retry-deferred (sink)
  (let ((paths (shiftf (slot-value sink 'deferred) '())))
    (dolist (p paths)
      (%delete-or-defer sink p))))

(defun %drop-oldest-now (sink reason)
  "The oldest retained segment is dropped: marker into the newest segment (on disk
first), then its files are deleted (or the delete deferred)."
  (let ((j (pop (slot-value sink 'retained))))
    (when j
      (let ((records (1+ (- (%seg-last-line j) (%seg-first-line j)))))
        (%append-meta sink :drop
                      (lambda (line)
                        (declare (ignore line))
                        (list :segment (%seg-number j) :first-line (%seg-first-line j)
                              :last-line (%seg-last-line j) :records records
                              :bytes (%seg-output-bytes j) :reason reason)))
        (%flush sink)
        (setf (slot-value sink 'dropped-through) (%seg-number j))
        (incf (slot-value sink 'dropped-records) records)
        (incf (slot-value sink 'dropped-bytes) (%seg-output-bytes j))
        (let ((dir (log-sink-directory sink)))
          (%delete-or-defer sink (%segment-pathname dir (%seg-number j) "seg"))
          (%delete-or-defer sink (%segment-pathname dir (%seg-number j) "idx")))
        t))))

(defun %rotate (sink)
  "Close the newest segment, open the next; drop the oldest beyond SEGMENTS."
  (%flush sink)
  (%close-streams sink)
  (setf (slot-value sink 'retained)
        (append (slot-value sink 'retained)
                (list (make-%seg :number (slot-value sink 'seg-number)
                                 :first-line (slot-value sink 'seg-first-line)
                                 :last-line (slot-value sink 'seg-last-line)
                                 :records (slot-value sink 'seg-records)
                                 :output-bytes (slot-value sink 'seg-output-bytes)))))
  (%open-next-segment sink)
  (loop while (> (1+ (length (slot-value sink 'retained))) (log-sink-segments sink))
        do (%drop-oldest-now sink :rotated))
  (%retry-deferred sink)
  (setf (slot-value sink 'rotated-p) t))

(defun %drop-for-cap (sink)
  "Total cap, writer lock held: drop the oldest segment that is not the newest."
  (when (slot-value sink 'retained)
    (let ((j (first (slot-value sink 'retained))))
      (unless (%fits-p sink (%meta-length sink (list :segment (%seg-number j) :first-line (%seg-first-line j)
                                                     :last-line (%seg-last-line j)
                                                     :records (1+ (- (%seg-last-line j) (%seg-first-line j)))
                                                     :bytes (%seg-output-bytes j) :reason :total-cap)))
        (%rotate sink))))
  (%drop-oldest-now sink :total-cap))

(defun %emit-output (sink stream v start end cont-p)
  (multiple-value-bind (text-p plen) (%payload-plan v start end)
    (%ensure-room sink (%record-length (slot-value sink 'next-line) (slot-value sink 'wall)
                                       (slot-value sink 'mono) plen))
    (%append-record sink (if (eq stream :stdout) #\o #\e) cont-p v start end text-p t)))

(defun %cut (v start end max)
  "Where a piece longer than MAX octets is cut: START + MAX, moved back to a UTF-8
character boundary (at most 3 octets) when the bytes there continue a character."
  (let ((cut (+ start max)))
    (if (>= cut end)
        end
        (let ((c cut))
          (loop repeat 3
                while (and (> c (1+ start)) (%utf8-continuation-p (aref v c)))
                do (decf c))
          (if (%utf8-continuation-p (aref v c)) cut c)))))

(defun %emit-pieces (sink stream v start end cont-p)
  "V [START, END) (no LF) as records of at most MAX-RECORD octets; the last one with CONT-P."
  (let ((max (log-sink-max-record sink)) (i start))
    (loop while (> (- end i) max)
          do (let ((cut (%cut v i end max)))
               (%emit-output sink stream v i cut t)
               (setf i cut)))
    (%emit-output sink stream v i end cont-p)))

(defun %emit-octets (sink stream v start end)
  "Split V [START, END) at LF into records: complete lines '.', the rest '+'."
  (let ((i start))
    (loop
      (let ((lf (position 10 v :start i :end end)))
        (cond (lf (%emit-pieces sink stream v i lf nil)
                  (setf i (1+ lf)))
              (t (when (< i end) (%emit-pieces sink stream v i end t))
                 (return)))
        (when (>= i end) (return))))))

(defun %take-carry (sink stream v start end)
  "Join STREAM's carried octets with V [START, END); cut off a new incomplete UTF-8
tail. → (values v start end), the complete part."
  (let* ((slot (if (eq stream :stdout) 'carry-out 'carry-err))
         (carry (slot-value sink slot)))
    (when carry
      (let ((joined (make-array (+ (length carry) (- end start)) :element-type '(unsigned-byte 8))))
        (replace joined carry)
        (replace joined v :start1 (length carry) :start2 start :end2 end)
        (setf v joined start 0 end (length joined))))
    (let ((k (%utf8-incomplete-tail v start end)))
      (setf (slot-value sink slot) (and (plusp k) (subseq v (- end k) end)))
      (values v start (- end k)))))

(defmacro %as-writer ((sink) &body body)
  "BODY as SINK's writer (lock held), unless this thread already is (→ NIL)."
  `(unless (eq (slot-value ,sink 'writer) (bt2:current-thread))
     (setf (slot-value ,sink 'writer) (bt2:current-thread))
     (unwind-protect (progn ,@body)
       (setf (slot-value ,sink 'writer) nil))))

(defun %write-log-octets (sink stream octets start end)
  (let ((rotated nil) (wrote nil) (flushed 0))
    (%with-writer (sink)
      (multiple-value-bind (v s e) (%as-octets octets start end)
        (cond ((slot-value sink 'log-closed-p))
              ((eq (slot-value sink 'writer) (bt2:current-thread))
               ;; A log-error handler writing to its own failing sink: memory only.
               (%head-tail-append sink (%utf8-decode v s e)))
              (t
               (%as-writer (sink)
                 (multiple-value-bind (v s e) (%take-carry sink stream v s e)
                   (when (> e s)
                     (%head-tail-append sink (%utf8-decode v s e))
                     (incf (slot-value sink 'output-bytes) (- e s))
                     (unless (log-sink-memory-only-p sink)
                       (setf wrote t)
                       (catch '%memory-only
                         (%stamp sink)
                         (%emit-octets sink stream v s e)
                         (%flush sink))
                       (setf rotated (shiftf (slot-value sink 'rotated-p) nil)
                             flushed (shiftf (slot-value sink 'flushed) 0))))))))))
    (when wrote (%notify sink))
    (when (or rotated (%log-grew sink flushed))
      (%log-cap-changed sink))
    (%run-deferred-stop)
    nil))

(defmethod sink-write-octets ((sink log-sink) stream octets &key (start 0) end)
  (check-type stream (member :stdout :stderr))
  (%write-log-octets sink stream octets start (or end (length octets))))

(defmethod sink-write ((sink log-sink) stream text)
  ;; TEXT encoded as UTF-8, then as SINK-WRITE-OCTETS.
  (check-type stream (member :stdout :stderr))
  (check-type text string)
  (let ((v (%utf8-encode text)))
    (%write-log-octets sink stream v 0 (length v))))

(defmethod sink-close ((sink log-sink))
  ;; Flush carried bytes, write the :END record, close the segment and index,
  ;; retry deferred deletes, tell the supervisor (total cap: the unit finished).
  ;; Idempotent; later writes are ignored.
  (let ((closed-now nil))
    (%with-writer (sink)
      (unless (or (slot-value sink 'log-closed-p)
                  (eq (slot-value sink 'writer) (bt2:current-thread)))
        (setf closed-now t)
        (%as-writer (sink)
          (unwind-protect
               (let ((carries (list (cons :stdout (shiftf (slot-value sink 'carry-out) nil))
                                    (cons :stderr (shiftf (slot-value sink 'carry-err) nil)))))
                 (loop for entry in carries
                       for c = (cdr entry)
                       when c
                         do (%head-tail-append sink (%utf8-decode c 0 (length c)))
                            (incf (slot-value sink 'output-bytes) (length c)))
                 (unless (log-sink-memory-only-p sink)
                   (catch '%memory-only
                     (%stamp sink)
                     (loop for (stream . c) in carries
                           when c do (%emit-pieces sink stream c 0 (length c) t))
                     (let ((detail (lambda (line)
                                     (list :last-line line :bytes (slot-value sink 'output-bytes)))))
                       (%ensure-room sink (%meta-length sink (funcall detail (slot-value sink 'next-line))))
                       (%append-meta sink :end detail))
                     (%flush sink))))
            (%close-streams sink)
            (%retry-deferred sink)
            (setf (slot-value sink 'log-closed-p) t)
            (setf (slot-value sink 'rotated-p) nil)))
        (call-next-method)))
    (when closed-now
      (%notify sink)
      (%log-sink-finished sink))
    (%run-deferred-stop)
    nil))

(defun %log-sink-drop-for-cap (sink)
  "The total cap asks a live SINK to drop its oldest non-newest segment. → T when it did."
  (let ((done nil))
    (%with-writer (sink)
      (unless (or (slot-value sink 'log-closed-p) (log-sink-memory-only-p sink)
                  (null (slot-value sink 'retained))
                  (slot-value sink 'writer))
        (%as-writer (sink)
          (catch '%memory-only
            (%stamp sink)
            (%drop-for-cap sink)
            (%flush sink)
            (setf done t))
          (setf (slot-value sink 'rotated-p) nil))))
    (when done (%notify sink))
    done))

(defun %log-sink-retry-deferred (sink)
  (%with-writer (sink)
    (unless (slot-value sink 'writer)
      (%retry-deferred sink))))

(defun %log-sink-go-memory-only (sink)
  "DELETE-UNIT-LOG: no more files for SINK."
  (%with-writer (sink)
    (%go-memory-only sink)
    (setf (slot-value sink 'deferred) '())))

;;; ------------------------------------------------------------------
;;; Thread units: *STANDARD-OUTPUT* / *ERROR-OUTPUT* inside the unit's thread.

(defconstant +sink-stream-buffer+ 4096)

(defclass sink-output-stream (trivial-gray-streams:fundamental-character-output-stream)
  ((sink :initarg :sink :reader sink-output-stream-sink)
   (stream-name :initarg :stream-name :reader sink-output-stream-name
                :documentation ":STDOUT or :STDERR.")
   (buffer :initform (make-string +sink-stream-buffer+))
   (fill-count :initform 0)
   (column :initform 0)
   (slock :initform (bt2:make-recursive-lock :name "sink-output-stream")))
  (:documentation "A character output stream into an OUTPUT-SINK. Characters are buffered
until a newline, 4096 characters, FINISH-OUTPUT / FORCE-OUTPUT or CLOSE; each
flush is one SINK-WRITE. Thread-safe. The thread executor binds *STANDARD-OUTPUT*
and *ERROR-OUTPUT* of a thread unit to two of these (:STDOUT, :STDERR) and
flushes them when the body ends (normally, by error or forced)."))

(defun make-sink-output-stream (sink stream-name)
  "A SINK-OUTPUT-STREAM writing into SINK as STREAM-NAME (:STDOUT or :STDERR)."
  (check-type stream-name (member :stdout :stderr))
  (make-instance 'sink-output-stream :sink sink :stream-name stream-name))

(defun %sos-flush (stream)
  (bt2:with-recursive-lock-held ((slot-value stream 'slock))
    (let ((n (slot-value stream 'fill-count)))
      (when (plusp n)
        (let ((text (subseq (slot-value stream 'buffer) 0 n)))
          (setf (slot-value stream 'fill-count) 0)
          (sink-write (sink-output-stream-sink stream) (sink-output-stream-name stream) text)))))
  nil)

(defmethod trivial-gray-streams:stream-write-char ((stream sink-output-stream) char)
  (bt2:with-recursive-lock-held ((slot-value stream 'slock))
    (let ((n (slot-value stream 'fill-count)))
      (setf (char (slot-value stream 'buffer) n) char
            (slot-value stream 'fill-count) (1+ n)))
    (if (char= char #\Newline)
        (setf (slot-value stream 'column) 0)
        (incf (slot-value stream 'column)))
    (when (or (char= char #\Newline) (>= (slot-value stream 'fill-count) +sink-stream-buffer+))
      (%sos-flush stream)))
  char)

(defmethod trivial-gray-streams:stream-write-string ((stream sink-output-stream) string
                                                     &optional (start 0) end)
  (bt2:with-recursive-lock-held ((slot-value stream 'slock))
    (loop for i from start below (or end (length string))
          do (trivial-gray-streams:stream-write-char stream (char string i))))
  string)

(defmethod trivial-gray-streams:stream-line-column ((stream sink-output-stream))
  (slot-value stream 'column))

(defmethod trivial-gray-streams:stream-finish-output ((stream sink-output-stream))
  (%sos-flush stream))

(defmethod trivial-gray-streams:stream-force-output ((stream sink-output-stream))
  (%sos-flush stream))

(defmethod close ((stream sink-output-stream) &key abort)
  (declare (ignore abort))
  (%sos-flush stream)
  (call-next-method))
