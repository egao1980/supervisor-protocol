(in-package #:supervisor-protocol)

;;; Output sinks. Every unit has one, made by the supervisor's
;;; OUTPUT-SINK-FACTORY before launch. Process units (H8b): the executor
;;; launches with octet pipes and pumps stdout and stderr into the sink with
;;; SINK-WRITE-OCTETS (STREAM :STDOUT / :STDERR, the exact bytes). Thread units
;;; (H8b): *STANDARD-OUTPUT* and *ERROR-OUTPUT* are bound to SINK-OUTPUT-STREAMs
;;; that SINK-WRITE into the sink; the body may also call SINK-WRITE itself.
;;; DEFAULT-OUTPUT-SINK makes a LOG-SINK (rotated segment files, src/log-sink.lisp,
;;; itself a HEAD-TAIL-BUFFER for the in-memory view) when the unit logs, and a
;;; plain HEAD-TAIL-BUFFER otherwise.

;;; ------------------------------------------------------------------
;;; UTF-8 (shared by the default SINK-WRITE-OCTETS, the log sink and the readers)

(deftype %octets () '(simple-array (unsigned-byte 8) (*)))

(defun %as-octets (octets start end)
  "→ (values simple-octet-vector start end) for any octet sequence."
  (if (typep octets '%octets)
      (values octets start end)
      (let ((v (make-array (- end start) :element-type '(unsigned-byte 8))))
        (replace v octets :start2 start :end2 end)
        (values v 0 (length v)))))

(declaim (inline %utf8-continuation-p))
(defun %utf8-continuation-p (b)
  (= (logand b #xC0) #x80))

(defun %utf8-sequence-length (v i end)
  "Length of the valid UTF-8 sequence at I in V (below END), or 0 when the bytes
there are not one (invalid, overlong, surrogate, beyond U+10FFFF, or cut by END)."
  (declare (type %octets v) (type fixnum i end) (optimize speed))
  (let ((b0 (aref v i)))
    (flet ((cont (k lo hi)
             (and (< (+ i k) end)
                  (<= lo (aref v (+ i k)) hi))))
      (cond ((< b0 #x80) 1)
            ((< b0 #xC2) 0)
            ((< b0 #xE0) (if (cont 1 #x80 #xBF) 2 0))
            ((< b0 #xF0)
             (if (and (cond ((= b0 #xE0) (cont 1 #xA0 #xBF))
                            ((= b0 #xED) (cont 1 #x80 #x9F))
                            (t (cont 1 #x80 #xBF)))
                      (cont 2 #x80 #xBF))
                 3 0))
            ((< b0 #xF5)
             (if (and (cond ((= b0 #xF0) (cont 1 #x90 #xBF))
                            ((= b0 #xF4) (cont 1 #x80 #x8F))
                            (t (cont 1 #x80 #xBF)))
                      (cont 2 #x80 #xBF)
                      (cont 3 #x80 #xBF))
                 4 0))
            (t 0)))))

(defun %utf8-incomplete-tail (v start end)
  "Octets at the end of V [START, END) that begin a UTF-8 sequence END cuts short
(0 to 3): they are carried to the stream's next write."
  (declare (type %octets v) (type fixnum start end))
  (loop for i of-type fixnum from (1- end) downto (max start (- end 3))
        for b = (aref v i)
        do (cond ((%utf8-continuation-p b))
                 ((< b #xC2) (return 0))
                 (t (let ((need (cond ((< b #xE0) 2) ((< b #xF0) 3) ((< b #xF5) 4) (t 0))))
                      (return (if (and (> need (- end i))
                                       ;; the bytes so far are a valid prefix
                                       (let ((probe (make-array 4 :element-type '(unsigned-byte 8)
                                                                  :initial-element #x80)))
                                         ;; the smallest continuation the lead allows
                                         (setf (aref probe 1) (case b (#xE0 #xA0) (#xF0 #x90) (t #x80)))
                                         (replace probe v :start2 i :end2 end)
                                         (= need (%utf8-sequence-length probe 0 4))))
                                  (- end i)
                                  0)))))
        finally (return 0)))

(defun %utf8-decode (v start end)
  "V [START, END) decoded as UTF-8; every byte that is not part of a valid
sequence becomes U+FFFD."
  (declare (type %octets v) (type fixnum start end) (optimize speed))
  (let ((out (make-string (- end start)))
        (n 0) (i start))
    (declare (type fixnum n i))
    (loop while (< i end)
          do (let ((b (aref v i)))
               (if (< b #x80)
                   (progn (setf (schar out n) (code-char b)) (incf i))
                   (let ((len (%utf8-sequence-length v i end)))
                     (declare (type fixnum len))
                     (setf (schar out n)
                           (case len
                             (2 (code-char (logior (ash (logand b #x1F) 6)
                                                   (logand (aref v (+ i 1)) #x3F))))
                             (3 (code-char (logior (ash (logand b #x0F) 12)
                                                   (ash (logand (aref v (+ i 1)) #x3F) 6)
                                                   (logand (aref v (+ i 2)) #x3F))))
                             (4 (code-char (logior (ash (logand b #x07) 18)
                                                   (ash (logand (aref v (+ i 1)) #x3F) 12)
                                                   (ash (logand (aref v (+ i 2)) #x3F) 6)
                                                   (logand (aref v (+ i 3)) #x3F))))
                             (t (code-char #xFFFD))))
                     (incf i (max len 1))))
               (incf n)))
    (if (= n (length out)) out (subseq out 0 n))))

(defun %utf8-encode (string)
  "STRING as a fresh simple octet vector (UTF-8)."
  (let ((v (stack-encoding:encode string :encoding :utf-8)))
    (if (typep v '%octets) v (coerce v '%octets))))

;;; ------------------------------------------------------------------
;;; Sinks

(defclass output-sink ()
  ((octet-lock :initform (bt2:make-lock :name "output-sink octets"))
   (octet-carry :initform (cons nil nil)
                :documentation "Incomplete UTF-8 tails kept by the default SINK-WRITE-OCTETS:
(stdout-octets . stderr-octets)."))
  (:documentation "Where a unit's output goes. Methods must be thread-safe."))

(defgeneric sink-write (sink stream text)
  (:documentation "Append TEXT (a string) that arrived on STREAM (:STDOUT or :STDERR). → NIL."))

(defgeneric sink-text (sink)
  (:documentation "The retained text. HEAD-TAIL-BUFFER: everything when nothing was
dropped; otherwise the first HEAD-SIZE characters, then
  (format nil \"~%[~D characters dropped]~%\" dropped)
then the last TAIL-SIZE characters. Streams are interleaved in arrival order."))

(defgeneric sink-write-octets (sink stream octets &key start end)
  (:documentation "H8b. Append the OCTETS [START, END) — raw bytes that arrived on STREAM
(:STDOUT or :STDERR) — to SINK. → NIL. The default method (any OUTPUT-SINK)
decodes UTF-8 incrementally per stream (an incomplete sequence at the end is kept
for that stream's next call; invalid bytes become U+FFFD) and calls SINK-WRITE
with the text, so 0.1.0 sinks keep working. LOG-SINK stores the exact bytes."))

(defmethod sink-write-octets ((sink output-sink) stream octets &key (start 0) end)
  (check-type stream (member :stdout :stderr))
  (let ((text
          (multiple-value-bind (v s e) (%as-octets octets start (or end (length octets)))
            (bt2:with-lock-held ((slot-value sink 'octet-lock))
              (let* ((cell (slot-value sink 'octet-carry))
                     (carry (if (eq stream :stdout) (car cell) (cdr cell))))
                (when carry
                  (let ((joined (make-array (+ (length carry) (- e s))
                                            :element-type '(unsigned-byte 8))))
                    (replace joined carry)
                    (replace joined v :start1 (length carry) :start2 s :end2 e)
                    (setf v joined s 0 e (length joined))))
                (let* ((k (%utf8-incomplete-tail v s e))
                       (new (and (plusp k) (subseq v (- e k) e))))
                  (if (eq stream :stdout) (setf (car cell) new) (setf (cdr cell) new))
                  (%utf8-decode v s (- e k))))))))
    (when (plusp (length text))
      (sink-write sink stream text))
    nil))

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
  "The default OUTPUT-SINK-FACTORY. H8b: when (GETF (UNIT-LOG-SETTINGS UNIT) :ENABLED)
— the supervisor has a state directory, SUPERVISOR-LOG-P, and START-UNIT got no
:LOG NIL — a LOG-SINK in (UNIT-LOG-PATHNAME state-directory supervisor-id unit-id)
with the unit's :SEGMENT-SIZE / :SEGMENTS, the supervisor's LOG-MAX-RECORD and the
default head / tail sizes; otherwise a HEAD-TAIL-BUFFER with the default sizes."
  (let ((settings (unit-log-settings unit))
        (sup (unit-supervisor unit)))
    (if (and (getf settings :enabled) sup (supervisor-state-directory sup))
        (make-log-sink (unit-log-pathname (supervisor-state-directory sup) (supervisor-id sup)
                                          (unit-id unit))
                       :unit unit
                       :segment-size (getf settings :segment-size)
                       :segments (getf settings :segments)
                       :max-record (supervisor-log-max-record sup))
        (make-head-tail-buffer))))

;;; HEAD fills first (up to HEAD-SIZE); everything after goes to TAIL, a string
;;; whose live part is [TAIL-START, fill-pointer) and at most TAIL-SIZE long.
;;; Characters pushed out of the front of the tail are counted in DROPPED; the
;;; dead prefix is compacted away once it is longer than TAIL-SIZE.

(defun %push-chars (vector text start end)
  "Append TEXT [START, END) to the adjustable VECTOR (fill pointer)."
  (let* ((fp (fill-pointer vector))
         (need (+ fp (- end start))))
    (when (> need (array-dimension vector 0))
      (setf vector (adjust-array vector (max need (* 2 (array-dimension vector 0))))))
    (setf (fill-pointer vector) need)
    (replace vector text :start1 fp :start2 start :end2 end)
    vector))

(defun %head-tail-append (sink text)
  "The HEAD-TAIL-BUFFER part of SINK-WRITE: TEXT into the head + tail (lock taken)."
  (bt2:with-lock-held ((slot-value sink 'lock))
    (with-slots (head-size tail-size head tail tail-start total dropped closed-p) sink
      (unless closed-p
        (let* ((n (length text))
               (room (- head-size (length head)))
               (into-head (max 0 (min room n)))
               (rest (- n into-head)))
          (incf total n)
          (when (plusp into-head)
            (setf head (%push-chars head text 0 into-head)))
          (when (plusp rest)
            (if (>= rest tail-size)
                ;; Everything live in the tail and the front of TEXT fall out.
                (progn
                  (incf dropped (+ (- (fill-pointer tail) tail-start) (- rest tail-size)))
                  (setf (fill-pointer tail) 0 tail-start 0)
                  (setf tail (%push-chars tail text (- n tail-size) n)))
                (progn
                  (setf tail (%push-chars tail text into-head n))
                  (let ((excess (- (fill-pointer tail) tail-start tail-size)))
                    (when (plusp excess)
                      (incf tail-start excess)
                      (incf dropped excess))))))
          (when (> tail-start (max tail-size 1024))
            (let ((live (- (fill-pointer tail) tail-start)))
              (replace tail tail :start1 0 :start2 tail-start :end2 (fill-pointer tail))
              (setf (fill-pointer tail) live
                    tail-start 0)))))))
  nil)

(defmethod sink-write ((sink head-tail-buffer) stream text)
  (declare (ignore stream))
  (check-type text string)
  (%head-tail-append sink text))

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
