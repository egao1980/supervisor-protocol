(in-package #:supervisor-protocol)

;;; H8b reader API (H8c builds search / read / tail tools on it).
;;;
;;; Every reader takes a LOG designator: a UNIT-LOG, a UNIT (its log sink's
;;; directory; a unit without one → LOG-NOT-FOUND), a LOG-SINK, or a pathname /
;;; namestring of a log directory. Readers parse the files, so they work the
;;; same for a running unit, a finished one, and a log a previous supervisor
;;; (or a crashed image) left behind. They never modify segments; they may
;;; rewrite an index (see src/log-sink.lisp). Thread-safe, and safe while the
;;; sink writes.
;;;
;;; Lines are record numbers (src/log-format.lisp). Records skipped by the
;;; STREAMS / TYPES filters are not gaps. A gap is a maximal range of missing
;;; line numbers: dropped segments (exact RECORDS and BYTES from the :DROP
;;; markers and the newest header's cumulative :DROPPED-* fields), corrupt
;;; bytes, or lines missing with no marker (:MISSING, BYTES NIL). A read whose
;;; range touches a gap returns the whole gap (not the overlapping part).

(defclass unit-log ()
  ((directory :initarg :directory :reader unit-log-directory)
   (supervisor-id :initarg :supervisor-id :initform nil :reader unit-log-supervisor-id)
   (unit-id :initarg :unit-id :initform nil :reader unit-log-unit-id)
   (sink :initarg :sink :initform nil :documentation "The live LOG-SINK in this image, or NIL."))
  (:documentation "A handle on one unit log directory. IDs come from the directory name
<supervisor-id>--<unit-id>."))

(defmethod print-object ((log unit-log) stream)
  (print-unreadable-object (log stream :type t)
    (format stream "~A--~A" (unit-log-supervisor-id log) (unit-log-unit-id log))))

(defclass log-gap ()
  ((first-line :initarg :first-line :reader log-gap-first-line)
   (last-line :initarg :last-line :reader log-gap-last-line)
   (records :initarg :records :reader log-gap-records
            :documentation "Missing records (= LAST-LINE − FIRST-LINE + 1).")
   (bytes :initarg :bytes :initform nil :reader log-gap-bytes
          :documentation "Output octets they carried, or NIL when unknown (:CORRUPT, :MISSING).")
   (reasons :initarg :reasons :reader log-gap-reasons
            :documentation "The distinct +LOG-GAP-REASONS+ inside, oldest first."))
  (:documentation "A range of lines a read crossed that is not on disk."))

(defmethod print-object ((gap log-gap) stream)
  (print-unreadable-object (gap stream :type t)
    (format stream "~D-~D ~S" (log-gap-first-line gap) (log-gap-last-line gap)
            (log-gap-reasons gap))))

(defclass log-segment-info ()
  ((number :initarg :number :reader log-segment-number)
   (path :initarg :path :reader log-segment-path)
   (index-path :initarg :index-path :reader log-segment-index-path)
   (first-line :initarg :first-line :reader log-segment-first-line)
   (last-line :initarg :last-line :reader log-segment-last-line
              :documentation "Last valid record's line.")
   (records :initarg :records :reader log-segment-records :documentation "Valid records.")
   (bytes :initarg :bytes :reader log-segment-bytes :documentation "File size.")
   (output-bytes :initarg :output-bytes :reader log-segment-output-bytes
                 :documentation "Output octets carried (payload + 1 per '.' output record).")
   (first-time :initarg :first-time :reader log-segment-first-time :documentation "Wall ms.")
   (last-time :initarg :last-time :reader log-segment-last-time :documentation "Wall ms.")
   (torn-p :initarg :torn-p :reader log-segment-torn-p
           :documentation "T when bytes after the last valid record are not a record."))
  (:documentation "Stats of one retained segment."))

(defmethod print-object ((info log-segment-info) stream)
  (print-unreadable-object (info stream :type t)
    (format stream "~D lines ~D-~D" (log-segment-number info)
            (log-segment-first-line info) (log-segment-last-line info))))

(defgeneric unit-log (designator)
  (:documentation "The UNIT-LOG of DESIGNATOR (a UNIT-LOG is returned as is). A unit
whose output is not logged, or a directory that is not a unit log → LOG-NOT-FOUND."))

(defmethod unit-log ((designator t))
  (%h8b-pending "unit-log"))

(defun find-unit-logs (state-directory &key supervisor-id unit-id)
  "UNIT-LOGs under <STATE-DIRECTORY>/units/, sorted by directory name, filtered by
SUPERVISOR-ID / UNIT-ID (strings) when given. Includes logs of supervisors that are
gone (crash recovery). A missing directory → ()."
  (declare (ignore state-directory supervisor-id unit-id))
  (%h8b-pending "find-unit-logs"))

(defgeneric log-segments (log)
  (:documentation "LOG-SEGMENT-INFOs of the retained segments, oldest first."))

(defgeneric log-read (log &key start end streams types limit)
  (:documentation "Records with line in [START, END] (START default 1; END NIL = the
last), in line order. STREAMS (default (:STDOUT :STDERR)) filters :OUTPUT records;
TYPES (default (:OUTPUT); T = all) filters by type. LIMIT: at most that many
records. → (values records gaps next-line): GAPS the LOG-GAPs the range touched
(in order); NEXT-LINE the line after the last one examined (pass it as START to
continue)."))

(defgeneric log-tail (log n &key streams)
  (:documentation "The last N :OUTPUT records (of STREAMS). → (values records gaps): GAPS
when fewer than N retained records exist before a gap that the N would reach into."))

(defgeneric log-follow (log &key from wait streams limit)
  (:documentation "Records (:OUTPUT, of STREAMS) with line ≥ FROM (default 1), at most
LIMIT. When there are none yet, wait up to WAIT seconds (default 0) for new ones:
woken by the sink for a live LOG-SINK of this image, polling the files (at most
100 ms apart) otherwise. → (values records gaps next-line ended-p): ENDED-P true
when the log has its :END record and nothing at or after FROM is left."))

(defgeneric map-log-records (function log &key start end streams types)
  (:documentation "Call FUNCTION on each record LOG-READ with the same arguments would
return, in order, without collecting them. → NIL."))

(defgeneric log-last-line (log)
  (:documentation "The line of the last valid record, 0 for an empty log."))

(defgeneric log-closed-p (log)
  (:documentation "T when the log has its :END record (SINK-CLOSE ran)."))

(defgeneric rebuild-log-index (log &key segment)
  (:documentation "Rewrite the index of SEGMENT (a number), or of every retained segment,
from the segment's records (atomically). Unlike the readers' automatic rebuild this
also rewrites the newest segment of an open log. → the number of indexes written."))

(defgeneric delete-unit-log (log)
  (:documentation "Delete the log directory and everything in it. A live sink of this
image goes memory-only first. → T."))

(defmethod log-segments ((log t))
  (%h8b-pending "log-segments"))

(defmethod log-read ((log t) &key start end streams types limit)
  (declare (ignore start end streams types limit))
  (%h8b-pending "log-read"))

(defmethod log-tail ((log t) n &key streams)
  (declare (ignore n streams))
  (%h8b-pending "log-tail"))

(defmethod log-follow ((log t) &key from wait streams limit)
  (declare (ignore from wait streams limit))
  (%h8b-pending "log-follow"))

(defmethod map-log-records (function (log t) &key start end streams types)
  (declare (ignore function start end streams types))
  (%h8b-pending "map-log-records"))

(defmethod log-last-line ((log t))
  (%h8b-pending "log-last-line"))

(defmethod log-closed-p ((log t))
  (%h8b-pending "log-closed-p"))

(defmethod rebuild-log-index ((log t) &key segment)
  (declare (ignore segment))
  (%h8b-pending "rebuild-log-index"))

(defmethod delete-unit-log ((log t))
  (%h8b-pending "delete-unit-log"))
