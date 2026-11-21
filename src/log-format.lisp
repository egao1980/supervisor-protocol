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
  (let ((v (log-record-octets record)))
    (if v (%utf8-decode v 0 (length v)) "")))

;;; ------------------------------------------------------------------
;;; Octet buffers (the writer appends records into one, then writes it once)

(defstruct (%obuf (:constructor %make-obuf
                      (&optional (size 256)
                       &aux (vec (make-array size :element-type '(unsigned-byte 8))))))
  (vec nil :type %octets)
  (fill 0 :type fixnum))

(defun %obuf-reserve (b n)
  "Room for N more octets in B. → B's vector."
  (declare (type %obuf b) (type fixnum n))
  (let ((vec (%obuf-vec b)) (need (+ (%obuf-fill b) n)))
    (when (> need (length vec))
      (let ((new (make-array (max need (* 2 (length vec))) :element-type '(unsigned-byte 8))))
        (replace new vec :end2 (%obuf-fill b))
        (setf (%obuf-vec b) new vec new)))
    vec))

(declaim (inline %obuf-byte))
(defun %obuf-byte (b x)
  (let ((vec (%obuf-reserve b 1)))
    (setf (aref vec (%obuf-fill b)) x)
    (incf (%obuf-fill b))))

(defun %obuf-bytes (b v start end)
  (let ((vec (%obuf-reserve b (- end start))))
    (replace vec v :start1 (%obuf-fill b) :start2 start :end2 end)
    (incf (%obuf-fill b) (- end start))))

(defun %obuf-ascii (b string)
  (loop for c across string do (%obuf-byte b (char-code c))))

(defun %decimal-length (n)
  (declare (type (integer 0) n))
  (if (< n 10) 1 (loop for k from 1 while (>= n 10) do (setf n (floor n 10)) finally (return k))))

(defun %obuf-decimal (b n)
  (declare (type (integer 0) n))
  (let* ((k (%decimal-length n))
         (vec (%obuf-reserve b k))
         (end (+ (%obuf-fill b) k)))
    (loop for i from (1- end) downto (%obuf-fill b)
          do (multiple-value-bind (q r) (floor n 10)
               (setf (aref vec i) (+ 48 r) n q)))
    (setf (%obuf-fill b) end)))

(defun %obuf-u32 (b n) (dotimes (k 4) (%obuf-byte b (ldb (byte 8 (* 8 k)) n))))
(defun %obuf-u64 (b n) (dotimes (k 8) (%obuf-byte b (ldb (byte 8 (* 8 k)) n))))

(defun %obuf-octets (b)
  (subseq (%obuf-vec b) 0 (%obuf-fill b)))

;;; ------------------------------------------------------------------
;;; CRC-32 (IEEE 802.3, reflected, as zlib.crc32)

(defparameter +crc32-table+
    (let ((table (make-array 256 :element-type '(unsigned-byte 32))))
      (dotimes (n 256 table)
        (let ((c n))
          (dotimes (k 8)
            (setf c (if (logbitp 0 c) (logxor #xEDB88320 (ash c -1)) (ash c -1))))
          (setf (aref table n) c)))))

(defun %crc32 (v start end)
  (declare (type %octets v) (type fixnum start end) (optimize speed (safety 0)))
  (let ((c #xFFFFFFFF) (table +crc32-table+))
    (declare (type (unsigned-byte 32) c) (type (simple-array (unsigned-byte 32) (256)) table))
    (loop for i of-type fixnum from start below end
          do (setf c (logxor (aref table (logand #xFF (logxor c (aref v i)))) (ash c -8))))
    (logxor c #xFFFFFFFF)))

(defparameter +hex-digits+ "0123456789abcdef")

;;; ------------------------------------------------------------------
;;; Payloads: escaped text or base64

(defun %payload-plan (v start end)
  "→ (values text-p payload-length): text when V [START, END) is valid UTF-8 and its
escaped form is no longer than base64."
  (declare (type %octets v) (type fixnum start end) (optimize speed))
  (let ((esc 0) (valid t) (i start))
    (declare (type fixnum esc i))
    (loop while (< i end)
          do (let ((b (aref v i)))
               (cond ((= b 92) (incf esc 2) (incf i))
                     ((or (= b 127) (and (< b 32) (/= b 9))) (incf esc 4) (incf i))
                     ((< b #x80) (incf esc) (incf i))
                     (t (let ((len (%utf8-sequence-length v i end)))
                          (declare (type fixnum len))
                          (if (zerop len)
                              (progn (setf valid nil) (incf esc) (incf i))
                              (progn (incf esc len) (incf i len))))))))
    (let ((b64 (* 4 (ceiling (the fixnum (- end start)) 3))))
      (if (and valid (<= esc b64))
          (values t esc)
          (values nil b64)))))

(defun %escaped-length (v start end)
  (loop for i from start below end
        for x = (aref v i)
        sum (cond ((= x 92) 2) ((or (= x 127) (and (< x 32) (/= x 9))) 4) (t 1))))

(defun %obuf-escaped (b v start end)
  (declare (type %octets v) (type fixnum start end))
  (let ((vec (%obuf-reserve b (* 4 (- end start))))
        (p (%obuf-fill b)))
    (declare (type %octets vec) (type fixnum p) (optimize speed))
    (loop for i of-type fixnum from start below end
          do (let ((x (aref v i)))
               (cond ((= x 92) (setf (aref vec p) 92 (aref vec (1+ p)) 92) (incf p 2))
                     ((or (= x 127) (and (< x 32) (/= x 9)))
                      (setf (aref vec p) 92 (aref vec (+ p 1)) 120
                            (aref vec (+ p 2)) (char-code (schar +hex-digits+ (ash x -4)))
                            (aref vec (+ p 3)) (char-code (schar +hex-digits+ (logand x 15))))
                      (incf p 4))
                     (t (setf (aref vec p) x) (incf p)))))
    (setf (%obuf-fill b) p)))

(defun %obuf-base64 (b v start end)
  (let ((s (stack-encoding:encode (subseq v start end) :encoding :base64 :pad t)))
    (if (stringp s)
        (%obuf-ascii b s)
        (%obuf-bytes b s 0 (length s)))))

(defun %type-char (type stream)
  (ecase type
    (:output (ecase stream (:stdout #\o) (:stderr #\e)))
    (:header #\h)
    (:drop #\d)
    (:end #\x)))

(defun %print-meta (object stream)
  "PRIN1 with *PACKAGE* KEYWORD, keywords without their colon (SBCL prints one even
when the keyword package is current), so the line reads back the same way."
  (cond ((keywordp object)
         (let ((token (prin1-to-string object)))
           (write-string token stream :start (if (char= #\: (char token 0)) 1 0))))
        ((consp object)
         (write-char #\( stream)
         (loop for (x . more) on object
               do (%print-meta x stream)
                  (cond ((consp more) (write-char #\Space stream))
                        (more (write-string " . " stream) (%print-meta more stream))))
         (write-char #\) stream))
        (t (prin1 object stream))))

(defun %meta-octets (detail)
  "DETAIL (a plist) printed as the payload of a header / drop / end record (UTF-8)."
  (%utf8-encode (with-standard-io-syntax
                  (let ((*package* (find-package :keyword)))
                    (with-output-to-string (s) (%print-meta detail s))))))

(defun %record-length (line wall mono payload-length)
  (+ (%decimal-length line) (%decimal-length wall) (%decimal-length mono) payload-length 17))

(defun %write-record (b line wall mono tchar cont-p v start end text-p)
  "Append one record line to B: payload V [START, END), as text when TEXT-P."
  (let ((line-start (%obuf-fill b)))
    (%obuf-decimal b line) (%obuf-byte b 32)
    (%obuf-decimal b wall) (%obuf-byte b 32)
    (%obuf-decimal b mono) (%obuf-byte b 32)
    (%obuf-byte b (char-code tchar))
    (%obuf-byte b (if text-p 116 98))
    (%obuf-byte b (if cont-p 43 46))
    (%obuf-byte b 32)
    (if text-p (%obuf-escaped b v start end) (%obuf-base64 b v start end))
    (let ((crc (%crc32 (%obuf-vec b) line-start (%obuf-fill b))))
      (%obuf-byte b 32)
      (loop for k from 28 downto 0 by 4
            do (%obuf-byte b (char-code (schar +hex-digits+ (ldb (byte 4 k) crc))))))
    (%obuf-byte b 10)
    b))

(defun format-log-record (record)
  "RECORD as one segment line: a fresh (unsigned-byte 8) vector ending in LF.
LINE, WALL-TIME, MONOTONIC must be non-negative integers."
  (dolist (x (list (log-record-line record) (log-record-wall-time record) (log-record-monotonic record)))
    (unless (typep x (quote (integer 0)))
      (error (quote type-error) :datum x :expected-type (quote (integer 0)))))
  (let* ((type (log-record-type record))
         (meta (not (eq type :output)))
         (v (if meta
                (%meta-octets (log-record-detail record))
                (or (log-record-octets record) (make-array 0 :element-type '(unsigned-byte 8)))))
         (b (%make-obuf (+ 64 (* 2 (length v))))))
    (multiple-value-bind (text-p) (if meta (values t) (%payload-plan v 0 (length v)))
      (%write-record b (log-record-line record) (log-record-wall-time record)
                     (log-record-monotonic record)
                     (%type-char type (log-record-stream record))
                     (and (not meta) (log-record-continued-p record))
                     v 0 (length v) text-p))
    (%obuf-octets b)))

;;; ------------------------------------------------------------------
;;; Parsing

(defun %hex-value (b)
  (cond ((<= 48 b 57) (- b 48))
        ((<= 97 b 102) (- b 87))
        (t nil)))

(defun %unescape (v start end)
  "Text payload → octets, or NIL when an escape or a raw control byte is bad."
  (declare (type %octets v) (type fixnum start end))
  (let ((out (make-array (- end start) :element-type '(unsigned-byte 8)))
        (n 0) (i start))
    (declare (type fixnum n i))
    (loop while (< i end)
          do (let ((x (aref v i)))
               (cond ((= x 92)
                      (cond ((and (< (1+ i) end) (= (aref v (1+ i)) 92))
                             (setf (aref out n) 92) (incf i 2))
                            ((and (< (+ i 3) end) (= (aref v (1+ i)) 120)
                                  (%hex-value (aref v (+ i 2))) (%hex-value (aref v (+ i 3))))
                             (setf (aref out n) (+ (* 16 (%hex-value (aref v (+ i 2))))
                                                   (%hex-value (aref v (+ i 3)))))
                             (incf i 4))
                            (t (return-from %unescape nil))))
                     ((or (= x 127) (and (< x 32) (/= x 9))) (return-from %unescape nil))
                     (t (setf (aref out n) x) (incf i)))
               (incf n)))
    (if (= n (length out)) out (subseq out 0 n))))

(defun %base64-char-p (x)
  (or (<= 65 x 90) (<= 97 x 122) (<= 48 x 57) (= x 43) (= x 47)))

(defun %unbase64 (v start end)
  (let ((n (- end start)))
    (when (and (zerop (mod n 4))
               (loop for i from start below end
                     for x = (aref v i)
                     always (or (%base64-char-p x)
                                (and (= x 61) (>= i (- end 2))
                                     (loop for j from i below end always (= (aref v j) 61))))))
      (if (zerop n)
          (make-array 0 :element-type '(unsigned-byte 8))
          (handler-case
              (let ((out (stack-encoding:decode (map 'string #'code-char (subseq v start end))
                                                :encoding :base64 :pad t :strict t)))
                (coerce out '%octets))
            (error () nil))))))

(defun %read-meta (octets)
  "The plist a meta payload holds, or NIL when it is not one."
  (handler-case
      (let ((string (%utf8-decode octets 0 (length octets))))
        (with-standard-io-syntax
          (let ((*package* (find-package :keyword))
                (*read-eval* nil))
            (multiple-value-bind (form pos) (read-from-string string)
              (and (listp form)
                   (= pos (length string))
                   (handler-case (evenp (list-length form)) (error () nil))
                   (loop for (k) on form by #'cddr always (keywordp k))
                   (list form))))))
    (error () nil)))

(defun %parse-decimal (v i end)
  "→ (values integer index-after) for the digits at I followed by a space, or NIL."
  (let ((n 0) (j i))
    (loop while (and (< j end) (<= 48 (aref v j) 57))
          do (setf n (+ (* n 10) (- (aref v j) 48))) (incf j))
    (if (and (> j i) (< j end) (= (aref v j) 32))
        (values n (1+ j))
        nil)))

(defun parse-log-record (octets &key (start 0) end)
  "The record whose line is OCTETS [START, END) — END NIL: up to and including the
first LF at or after START. → a LOG-RECORD (SEGMENT and OFFSET NIL), or NIL when
the bytes are not one valid line (no LF, bad fields, bad escape or base64, CRC
mismatch). Second value: the index after the line's LF (or NIL)."
  (multiple-value-bind (v s e) (%as-octets octets start (or end (length octets)))
    (let* ((lf (position 10 v :start s :end e))
           (offset (- start s)))
      (when (or (null lf) (and end (/= lf (1- e))))
        (return-from parse-log-record (values nil nil)))
      (let ((crc-sp (- lf 9)))
        (flet ((fail () (return-from parse-log-record (values nil nil))))
          (when (or (< crc-sp (+ s 10)) (/= (aref v crc-sp) 32)) (fail))
          (let ((crc 0))
            (loop for i from (1+ crc-sp) below lf
                  for h = (%hex-value (aref v i))
                  do (if h (setf crc (+ (* crc 16) h)) (fail)))
            (unless (= crc (%crc32 v s crc-sp)) (fail)))
          (multiple-value-bind (line p) (%parse-decimal v s crc-sp)
            (unless line (fail))
            (multiple-value-bind (wall p) (%parse-decimal v p crc-sp)
              (unless wall (fail))
              (multiple-value-bind (mono p) (%parse-decimal v p crc-sp)
                (unless mono (fail))
                (when (> (+ p 4) crc-sp) (fail))
                (let* ((tc (aref v p)) (ec (aref v (+ p 1))) (cc (aref v (+ p 2)))
                       (type (case tc (111 :output) (101 :output) (104 :header) (100 :drop) (120 :end)))
                       (stream (case tc (111 :stdout) (101 :stderr)))
                       (text-p (case ec (116 t) (98 nil) (t :bad)))
                       (cont-p (case cc (46 nil) (43 t) (t :bad)))
                       (pstart (+ p 4)))
                  (when (or (null type) (eq text-p :bad) (eq cont-p :bad)
                            (/= (aref v (+ p 3)) 32))
                    (fail))
                  (when (and (not (eq type :output)) (or (not text-p) cont-p)) (fail))
                  (let ((payload (if text-p (%unescape v pstart crc-sp) (%unbase64 v pstart crc-sp))))
                    (unless payload (fail))
                    (let ((detail nil))
                      (unless (eq type :output)
                        (let ((m (%read-meta payload)))
                          (unless m (fail))
                          (setf detail (first m))))
                      (values (make-instance 'log-record
                                             :line line :wall-time wall :monotonic mono :type type
                                             :stream stream
                                             :octets (and (eq type :output) payload)
                                             :continued-p (and cont-p t) :detail detail)
                              (+ offset (1+ lf))))))))))))))
