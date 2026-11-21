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
   (lock :initform (bt2:make-lock :name "head-tail-buffer"))
   (head :initform (make-array 64 :element-type 'character :adjustable t :fill-pointer 0))
   (tail :initform (make-array 64 :element-type 'character :adjustable t :fill-pointer 0))
   (tail-start :initform 0)
   (total :initform 0)
   (dropped :initform 0)
   (closed-p :initform nil))
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

;;; HEAD fills first (up to HEAD-SIZE); everything after goes to TAIL, a string
;;; whose live part is [TAIL-START, fill-pointer) and at most TAIL-SIZE long.
;;; Characters pushed out of the front of the tail are counted in DROPPED; the
;;; dead prefix is compacted away once it is longer than TAIL-SIZE.

(defmethod sink-write ((sink head-tail-buffer) stream text)
  (declare (ignore stream))
  (check-type text string)
  (bt2:with-lock-held ((slot-value sink 'lock))
    (with-slots (head-size tail-size head tail tail-start total dropped closed-p) sink
      (unless closed-p
        (let* ((n (length text))
               (room (- head-size (length head)))
               (into-head (max 0 (min room n))))
          (incf total n)
          (loop for i below into-head do (vector-push-extend (char text i) head))
          (loop for i from into-head below n do (vector-push-extend (char text i) tail))
          (let ((excess (- (fill-pointer tail) tail-start tail-size)))
            (when (plusp excess)
              (incf tail-start excess)
              (incf dropped excess)))
          (when (> tail-start (max tail-size 1024))
            (let ((live (- (fill-pointer tail) tail-start)))
              (replace tail tail :start1 0 :start2 tail-start :end2 (fill-pointer tail))
              (setf (fill-pointer tail) live
                    tail-start 0)))))))
  nil)

(defmethod sink-text ((sink head-tail-buffer))
  (bt2:with-lock-held ((slot-value sink 'lock))
    (with-slots (head tail tail-start dropped) sink
      (let ((live-tail (subseq tail tail-start (fill-pointer tail))))
        (if (zerop dropped)
            (concatenate 'string head live-tail)
            (concatenate 'string head
                         (format nil "~%[~D characters dropped]~%" dropped)
                         live-tail))))))

(defmethod sink-stats ((sink head-tail-buffer))
  (bt2:with-lock-held ((slot-value sink 'lock))
    (values (slot-value sink 'total) (slot-value sink 'dropped))))

(defmethod sink-close ((sink head-tail-buffer))
  (bt2:with-lock-held ((slot-value sink 'lock))
    (setf (slot-value sink 'closed-p) t))
  nil)
