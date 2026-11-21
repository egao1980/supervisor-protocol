(in-package #:supervisor-protocol)

;;; Output sinks. Every unit has one, made by the supervisor's
;;; OUTPUT-SINK-FACTORY before launch. Process units: the executor pumps
;;; stdout and stderr into it (STREAM :STDOUT / :STDERR, text decoded as
;;; UTF-8). Thread units: the body calls SINK-WRITE itself. The default keeps
;;; a head and a tail in memory; H8b replaces the factory with rotated,
;;; size-capped log files and keeps this buffer for the in-memory view.

(defclass output-sink () ()
  (:documentation "Where a unit's output goes. Methods must be thread-safe."))

(defgeneric sink-write (sink stream text)
  (:documentation "Append TEXT (a string) that arrived on STREAM (:STDOUT or :STDERR). → NIL."))

(defgeneric sink-text (sink)
  (:documentation "The retained text. HEAD-TAIL-BUFFER: everything when nothing was
dropped; otherwise the first HEAD-SIZE characters, then
  (format nil \"~%[~D characters dropped]~%\" dropped)
then the last TAIL-SIZE characters. Streams are interleaved in arrival order."))

(defgeneric sink-stats (sink)
  (:documentation "→ (values total-characters dropped-characters)."))

(defgeneric sink-close (sink)
  (:documentation "No more writes (the unit finished or was evicted). Later SINK-WRITEs are
ignored; SINK-TEXT still answers. Idempotent. → NIL.")
  (:method ((sink output-sink)) nil))

(defclass head-tail-buffer (output-sink)
  ((head-size :initarg :head-size :reader head-tail-buffer-head-size)
   (tail-size :initarg :tail-size :reader head-tail-buffer-tail-size)
   (lock :initform (bt2:make-lock :name "head-tail-buffer")))
  (:documentation "The first HEAD-SIZE and the last TAIL-SIZE characters of a unit's output
(Codex unified_exec). Memory is bounded by HEAD-SIZE + TAIL-SIZE."))

(defun make-head-tail-buffer (&key (head-size +default-head-size+) (tail-size +default-tail-size+))
  (check-type head-size (integer 0))
  (check-type tail-size (integer 0))
  (make-instance 'head-tail-buffer :head-size head-size :tail-size tail-size))

(defun default-output-sink (unit)
  "The default OUTPUT-SINK-FACTORY: a HEAD-TAIL-BUFFER with the default sizes."
  (declare (ignore unit))
  (make-head-tail-buffer))

(defmethod sink-write ((sink head-tail-buffer) stream text)
  (declare (ignore stream text))
  (%h8a-pending "sink-write head-tail-buffer"))

(defmethod sink-text ((sink head-tail-buffer))
  (%h8a-pending "sink-text head-tail-buffer"))

(defmethod sink-stats ((sink head-tail-buffer))
  (%h8a-pending "sink-stats head-tail-buffer"))

(defmethod sink-close ((sink head-tail-buffer))
  (%h8a-pending "sink-close head-tail-buffer"))
