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
   (memory-only-p :initform nil :reader log-sink-memory-only-p))
  (:documentation "An OUTPUT-SINK writing rotated segment files (see above) and keeping
the head + tail in memory. Thread-safe; readers in any thread or image may read
its files while it writes."))

(defun unit-log-pathname (state-directory supervisor-id unit-id)
  "<STATE-DIRECTORY>/units/<SUPERVISOR-ID>--<UNIT-ID>/ (a directory pathname)."
  (merge-pathnames (make-pathname :directory (list :relative "units"
                                                   (format nil "~A--~A" supervisor-id unit-id)))
                   (uiop:ensure-directory-pathname state-directory)))

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
  (declare (ignore class unit supervisor-id unit-id kind segment-size segments max-record
                   index-stride head-size tail-size initargs))
  (%h8b-pending (format nil "make-log-sink ~A" directory)))

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
  (%h8b-pending "log-sink-last-line"))

(defmethod sink-write ((sink log-sink) stream text)
  ;; TEXT encoded as UTF-8, then as SINK-WRITE-OCTETS.
  (declare (ignore stream text))
  (%h8b-pending "sink-write log-sink"))

(defmethod sink-write-octets ((sink log-sink) stream octets &key (start 0) end)
  (declare (ignore stream octets start end))
  (%h8b-pending "sink-write-octets log-sink"))

(defmethod sink-close ((sink log-sink))
  ;; Flush carried bytes, write the :END record, close the segment and index,
  ;; retry deferred deletes, tell the supervisor (total cap: the unit finished).
  ;; Idempotent; later writes are ignored.
  (%h8b-pending "sink-close log-sink"))

;;; Thread units: *STANDARD-OUTPUT* / *ERROR-OUTPUT* inside the unit's thread.

(defclass sink-output-stream (trivial-gray-streams:fundamental-character-output-stream)
  ((sink :initarg :sink :reader sink-output-stream-sink)
   (stream-name :initarg :stream-name :reader sink-output-stream-name
                :documentation ":STDOUT or :STDERR."))
  (:documentation "A character output stream into an OUTPUT-SINK. Characters are buffered
until a newline, 4096 characters, FINISH-OUTPUT / FORCE-OUTPUT or CLOSE; each
flush is one SINK-WRITE. Thread-safe. The thread executor binds *STANDARD-OUTPUT*
and *ERROR-OUTPUT* of a thread unit to two of these (:STDOUT, :STDERR) and
flushes them when the body ends (normally, by error or forced)."))

(defun make-sink-output-stream (sink stream-name)
  "A SINK-OUTPUT-STREAM writing into SINK as STREAM-NAME (:STDOUT or :STDERR)."
  (declare (ignore sink))
  (check-type stream-name (member :stdout :stderr))
  (%h8b-pending "make-sink-output-stream"))
