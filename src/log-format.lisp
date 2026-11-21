(in-package #:supervisor-protocol)

;;; H8b record format (+LOG-FORMAT+ 1). A segment file is a sequence of
;;; records, one per line, every byte of a line ASCII except the payload of a
;;; text record (UTF-8). A line is
;;;
;;;   <line> SP <wall-ms> SP <mono-ms> SP <tag> SP <payload> SP <crc> LF
;;;
;;;   line     decimal, 1-based record number, contiguous across the segments of
;;;            one unit log (every record takes one: headers, markers, end).
;;;   wall-ms  decimal, Unix epoch milliseconds (wall clock) when written.
;;;   mono-ms  decimal, milliseconds on the writing image's monotonic clock.
;;;   tag      3 characters: TYPE ENCODING CONTINUATION.
;;;              TYPE  o stdout output · e stderr output · h header · d drop
;;;                    marker · x end.
;;;              ENCODING  t text: the payload octets are valid UTF-8, written
;;;                    as is except \ → \\ and every C0 control but TAB, and
;;;                    DEL, → \xHH (two lowercase hex digits); b base64: RFC 4648
;;;                    standard alphabet, padded. Output records use t when the
;;;                    octets are valid UTF-8 and the escaped form is no longer
;;;                    than the base64 form, else b. Header / drop / end records
;;;                    are always t.
;;;              CONTINUATION  . a line feed ended these octets in the output
;;;                    (it is not stored) · + no line feed ended them: the line
;;;                    continues in the next record of the same stream, or the
;;;                    output stopped there. Header / drop / end records use '.'.
;;;   payload  output records: the encoded octets (may be empty). Header, drop
;;;            and end records: their detail plist printed WITH-STANDARD-IO-SYNTAX
;;;            with *PACKAGE* KEYWORD on one line (keys print without a colon,
;;;            as in unit files), read back with *READ-EVAL* NIL.
;;;   crc      8 lowercase hex digits: CRC-32 (IEEE 802.3, as zip / gzip /
;;;            zlib.crc32) of every byte of the line before " <crc>".
;;;
;;; A line counts only if it ends in LF and its CRC matches. A torn last line
;;; (a crash mid-write) is skipped; it never hides the records before it. Bytes
;;; that are not valid records between valid ones are skipped up to the next
;;; LF and reported as a :CORRUPT gap.
;;;
;;; Record details:
;;;   :header  (:format 1 :supervisor-id "…" :unit-id "…" :kind :shell-job
;;;             :segment n :first-line l :dropped-through k :dropped-records r
;;;             :dropped-bytes b)  — every segment's first record; the dropped-*
;;;             fields count everything dropped before this segment was opened.
;;;   :drop    (:segment n :first-line a :last-line b :records r :bytes o
;;;             :reason :rotated|:total-cap)  — one per dropped segment, written
;;;             to the unit's newest segment when it is dropped. RECORDS = b − a + 1
;;;             (every record of the segment); BYTES = output octets it carried
;;;             (payload octets + 1 per '.' output record).
;;;   :end     (:last-line l :bytes o)  — written by SINK-CLOSE; O = all output
;;;             octets the sink ever accepted.

(defclass log-record ()
  ((line :initarg :line :initform nil :reader log-record-line)
   (wall-time :initarg :wall-time :initform nil :reader log-record-wall-time
              :documentation "Unix epoch milliseconds.")
   (monotonic :initarg :monotonic :initform nil :reader log-record-monotonic
              :documentation "Milliseconds on the writing image's monotonic clock.")
   (record-type :initarg :type :initform :output :reader log-record-type
                :documentation "One of +LOG-RECORD-TYPES+.")
   (record-stream :initarg :stream :initform nil :reader log-record-stream
                  :documentation ":STDOUT or :STDERR for :OUTPUT records, else NIL.")
   (octets :initarg :octets :initform nil :reader log-record-octets
           :documentation "The exact output octets ((unsigned-byte 8) vector), no line feed.")
   (continued-p :initarg :continued-p :initform nil :reader log-record-continued-p
                :documentation "T: no line feed ended these octets.")
   (detail :initarg :detail :initform nil :reader log-record-detail
           :documentation "Plist of a :HEADER, :DROP or :END record.")
   (segment :initarg :segment :initform nil :reader log-record-segment
            :documentation "Number of the segment it was read from, or NIL.")
   (offset :initarg :offset :initform nil :reader log-record-offset
           :documentation "Byte offset of its line in that segment, or NIL."))
  (:documentation "One record of a unit log (see the format above)."))

(defmethod print-object ((record log-record) stream)
  (print-unreadable-object (record stream :type t)
    (format stream "~@[~D ~]~(~A~)~@[ ~(~A~)~]" (log-record-line record)
            (log-record-type record) (log-record-stream record))))

(defun make-log-record (&key line wall-time monotonic (type :output) stream octets
                             continued-p detail)
  "A LOG-RECORD. OCTETS: any sequence of octets (copied to an (unsigned-byte 8) vector)."
  (make-instance 'log-record
                 :line line :wall-time wall-time :monotonic monotonic :type type
                 :stream stream
                 :octets (and octets (coerce octets '(simple-array (unsigned-byte 8) (*))))
                 :continued-p continued-p :detail detail))

(defgeneric log-record-text (record)
  (:documentation "The octets decoded as UTF-8 (invalid bytes → U+FFFD); \"\" for
records without octets."))

(defmethod log-record-text ((record log-record))
  (%h8b-pending "log-record-text"))

(defun format-log-record (record)
  "RECORD as one segment line: a fresh (unsigned-byte 8) vector ending in LF.
LINE, WALL-TIME, MONOTONIC must be non-negative integers."
  (declare (ignore record))
  (%h8b-pending "format-log-record"))

(defun parse-log-record (octets &key (start 0) end)
  "The record whose line is OCTETS [START, END) — END NIL: up to and including the
first LF at or after START. → a LOG-RECORD (SEGMENT and OFFSET NIL), or NIL when
the bytes are not one valid line (no LF, bad fields, bad escape or base64, CRC
mismatch). Second value: the index after the line's LF (or NIL)."
  (declare (ignore octets start end))
  (%h8b-pending "parse-log-record"))
