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

;;; ------------------------------------------------------------------
;;; Designators

(declaim (ftype function %forget-log %find-live-log-sink))

(defun %log-ids (directory)
  "(values supervisor-id unit-id) from the directory name <supervisor-id>--<unit-id>."
  (let* ((name (car (last (pathname-directory directory))))
         (cut (and (stringp name) (search "--" name))))
    (if cut
        (values (subseq name 0 cut) (subseq name (+ cut 2)))
        (values nil nil))))

(defun %make-unit-log (directory &key sink)
  (multiple-value-bind (sid uid) (%log-ids directory)
    (make-instance 'unit-log :directory directory
                             :supervisor-id (or (and sink (log-sink-supervisor-id sink)) sid)
                             :unit-id (or (and sink (log-sink-unit-id sink)) uid)
                             :sink sink)))

(defmethod unit-log ((designator unit-log))
  designator)

(defmethod unit-log ((designator log-sink))
  (%make-unit-log (log-sink-directory designator) :sink designator))

(defmethod unit-log ((designator unit))
  (let ((sink (unit-output-sink designator)))
    (if (typep sink 'log-sink)
        (unit-log sink)
        (error 'log-not-found :supervisor (unit-supervisor designator)
                              :message (format nil "unit ~A has no log" (unit-id designator))))))

(defun %directory-log (designator)
  (let ((dir (uiop:ensure-directory-pathname designator)))
    (unless (uiop:directory-exists-p dir)
      (error 'log-not-found :path dir :message "no such directory"))
    (%make-unit-log dir :sink (%find-live-log-sink dir))))

(defmethod unit-log ((designator pathname))
  (%directory-log designator))

(defmethod unit-log ((designator string))
  (%directory-log designator))

(defmethod unit-log ((designator t))
  (error 'log-not-found :message (format nil "not a log designator: ~S" designator)))

(defun %live-sink (log)
  (let ((sink (slot-value log 'sink)))
    (and sink (not (slot-value sink 'log-closed-p)) sink)))

(defun find-unit-logs (state-directory &key supervisor-id unit-id)
  "UNIT-LOGs under <STATE-DIRECTORY>/units/, sorted by directory name, filtered by
SUPERVISOR-ID / UNIT-ID (strings) when given. Includes logs of supervisors that are
gone (crash recovery). A missing directory → ()."
  (let* ((state (uiop:ensure-directory-pathname state-directory))
         (units (merge-pathnames (make-pathname :directory '(:relative "units")) state))
         (entries (and (uiop:directory-exists-p units)
                       (handler-case (pathlib:fs-iterdir pathlib:*filesystem* units)
                         (error () nil))))
         (names (sort (loop for p in entries
                            for name = (and (uiop:directory-pathname-p p)
                                            (car (last (pathname-directory p))))
                            when (and (stringp name) (search "--" name))
                              collect name)
                      #'string<)))
    (loop for name in (remove-duplicates names :test #'string=)
          for cut = (search "--" name)
          for sid = (subseq name 0 cut)
          for uid = (subseq name (+ cut 2))
          when (and (or (null supervisor-id) (string= supervisor-id sid))
                    (or (null unit-id) (string= unit-id uid)))
            collect (let ((dir (unit-log-pathname state sid uid)))
                      (make-instance 'unit-log :directory dir :supervisor-id sid :unit-id uid
                                               :sink (%find-live-log-sink dir))))))

;;; ------------------------------------------------------------------
;;; Scanning a log directory

(defstruct (%rseg (:conc-name %rs-))
  number path idx-path (size 0)
  (records #() :type simple-vector)
  (corrupt '())                        ; lines that invalid bytes came right before
  torn-p header)

(defun %segment-numbers (dir)
  "Numbers of the NNNNNNNN.seg files in DIR, ascending."
  (let ((files (and (uiop:directory-exists-p dir)
                    (handler-case (pathlib:fs-iterdir pathlib:*filesystem* dir)
                      (error () nil)))))
    (sort (loop for p in files
                for name = (pathname-name p)
                when (and (equal "seg" (pathname-type p)) (stringp name) (= 8 (length name))
                          (every #'digit-char-p name))
                  collect (parse-integer name))
          #'<)))

(defun %read-octets (path &optional (start 0))
  "The file's octets (opened and closed in this call), or NIL when it is gone."
  (handler-case (pathlib:fs-read-range pathlib:*filesystem* path start nil)
    (error () nil)))

(defun %read-segment (dir number)
  (let* ((path (%segment-pathname dir number "seg"))
         (v (%read-octets path)))
    (when v
      (let ((records '()) (corrupt '()) (pending nil) (pos 0) (n (length v)) (last-line 0))
        (loop while (< pos n)
              do (let ((lf (position 10 v :start pos)))
                   (unless lf (return))
                   (let ((r (parse-log-record v :start pos :end (1+ lf))))
                     (cond ((and r (> (log-record-line r) last-line))
                            (setf (slot-value r 'segment) number
                                  (slot-value r 'offset) pos
                                  last-line (log-record-line r))
                            (when pending (push last-line corrupt) (setf pending nil))
                            (push r records))
                           ((null r) (setf pending t))))
                   (setf pos (1+ lf))))
        (let ((recs (coerce (nreverse records) 'simple-vector)))
          (make-%rseg :number number :path path
                      :idx-path (%segment-pathname dir number "idx")
                      :size n :records recs :corrupt corrupt
                      :torn-p (or pending (< pos n))
                      :header (and (plusp (length recs))
                                   (eq :header (log-record-type (svref recs 0)))
                                   (log-record-detail (svref recs 0)))))))))

(defun %drops-in (seg)
  (loop for r across (%rs-records seg)
        when (eq :drop (log-record-type r)) collect (log-record-detail r)))

(defun %scan-log (dir)
  "The retained segments of DIR (%RSEGs, oldest first): segments numbered at or
below the highest :DROPPED-THROUGH / :DROP :SEGMENT are ignored (logically dropped)."
  (let* ((segs (loop for n in (%segment-numbers dir)
                     for s = (%read-segment dir n)
                     when s collect s))
         (dropped (reduce #'max
                          (loop for s in segs
                                append (cons (or (getf (%rs-header s) :dropped-through) 0)
                                             (mapcar (lambda (d) (or (getf d :segment) 0))
                                                     (%drops-in s))))
                          :initial-value 0)))
    (remove-if (lambda (s) (<= (%rs-number s) dropped)) segs)))

(defun %segment-first-line (seg)
  (or (getf (%rs-header seg) :first-line)
      (and (plusp (length (%rs-records seg))) (log-record-line (svref (%rs-records seg) 0)))))

(defun %segment-closed-p (seg)
  (some (lambda (r) (eq :end (log-record-type r))) (%rs-records seg)))

;;; Indexes

(defun %u32 (v i) (loop for k below 4 sum (ash (aref v (+ i k)) (* 8 k))))
(defun %u64 (v i) (loop for k below 8 sum (ash (aref v (+ i k)) (* 8 k))))

(defun %index-header-stride (v)
  (and v (>= (length v) 16)
       (= (aref v 0) 83) (= (aref v 1) 80) (= (aref v 2) 76) (= (aref v 3) 88)
       (let ((stride (%u32 v 4))) (and (plusp stride) stride))))

(defun %expected-index (seg stride)
  "The index bytes of SEG with STRIDE: header, then the offsets of the existing lines
first + k × stride (up to the first one that is missing)."
  (let* ((first (%segment-first-line seg))
         (b (%make-obuf 64))
         (by-line (make-hash-table)))
    (loop for r across (%rs-records seg)
          do (setf (gethash (log-record-line r) by-line) (log-record-offset r)))
    (%obuf-ascii b "SPLX")
    (%obuf-u32 b stride)
    (%obuf-u64 b first)
    (loop for line from first by stride
          for off = (gethash line by-line)
          while off do (%obuf-u64 b off))
    (%obuf-octets b)))

(defun %index-valid-p (seg v)
  "V (the index file's octets) has a good header and every full entry points at its line."
  (let ((stride (%index-header-stride v))
        (first (%segment-first-line seg)))
    (and stride first (= first (%u64 v 8))
         (let ((by-offset (make-hash-table)))
           (loop for r across (%rs-records seg)
                 do (setf (gethash (log-record-offset r) by-offset) (log-record-line r)))
           (loop for k from 0 below (floor (- (length v) 16) 8)
                 always (eql (gethash (%u64 v (+ 16 (* 8 k))) by-offset)
                             (+ first (* k stride))))))))

(defun %write-index (seg stride)
  "Rewrite SEG's index atomically. → T when written."
  (when (%segment-first-line seg)
    (handler-case
        (progn
          (pathlib:fs-write-atomic pathlib:*filesystem* (%rs-idx-path seg) (%expected-index seg stride))
          ;; The segment was dropped meanwhile: no orphan index.
          (unless (probe-file (%rs-path seg))
            (ignore-errors (pathlib:fs-unlink pathlib:*filesystem* (%rs-idx-path seg) :missing-ok t)))
          t)
      (error () nil))))

(defun %log-stride (log segs indexes)
  (declare (ignore segs))
  (or (loop for v in indexes thereis (%index-header-stride v))
      (let ((sink (slot-value log 'sink))) (and sink (log-sink-index-stride sink)))
      +log-index-stride+))

(defun %maintain-indexes (log segs)
  "Rebuild missing or bad indexes, except the newest segment of an open log."
  (when segs
    (let* ((indexes (mapcar (lambda (s) (%read-octets (%rs-idx-path s))) segs))
           (stride (%log-stride log segs indexes))
           (newest (car (last segs)))
           (open-p (not (%segment-closed-p newest))))
      (loop for s in segs
            for v in indexes
            unless (or (and open-p (eq s newest)) (%index-valid-p s v))
              do (%write-index s (or (%index-header-stride v) stride))))))

(defun %log-segs (log &key (maintain t))
  (let ((segs (%scan-log (unit-log-directory log))))
    (when maintain (%maintain-indexes log segs))
    segs))

;;; Gaps

(defun %make-gap (first last bytes reasons)
  (make-instance 'log-gap :first-line first :last-line last :records (1+ (- last first))
                          :bytes bytes :reasons reasons))

(defun %prefix-drops (segs)
  "→ (values records bytes reasons) of the dropped prefix the markers and the
headers' cumulative fields describe."
  (let ((best-r 0) (best-b 0) (reasons '()))
    (dolist (s segs)
      (let* ((h (%rs-header s))
             (dt (or (getf h :dropped-through) 0))
             (r (or (getf h :dropped-records) 0))
             (b (or (getf h :dropped-bytes) 0)))
        (dolist (d (%drops-in s))
          (pushnew (getf d :reason) reasons)
          (when (> (or (getf d :segment) 0) dt)
            (incf r (or (getf d :records) 0))
            (incf b (or (getf d :bytes) 0))))
        (when (> r best-r) (setf best-r r best-b b))))
    (values best-r best-b (remove nil (nreverse reasons)))))

(defun %gaps (segs records)
  "Maximal ranges of missing lines before and between RECORDS (a list, line order)."
  (let ((raw '()))
    (when records
      (let ((first (log-record-line (first records))))
        (when (> first 1)
          (multiple-value-bind (dr db reasons) (%prefix-drops segs)
            (cond ((zerop dr) (push (list 1 (1- first) nil '(:missing)) raw))
                  ((>= dr (1- first))
                   (push (list 1 (1- first) db (or reasons '(:rotated))) raw))
                  (t (push (list 1 dr db (or reasons '(:rotated))) raw)
                     (push (list (1+ dr) (1- first) nil '(:missing)) raw)))))
        (let ((by-number (make-hash-table)))
          (dolist (s segs) (setf (gethash (%rs-number s) by-number) s))
          (loop for (p q) on records
                while q
                do (let ((a (log-record-line p)) (b (log-record-line q)))
                     (when (> b (1+ a))
                       (let* ((qs (gethash (log-record-segment q) by-number))
                              (ps (gethash (log-record-segment p) by-number))
                              (corrupt (or (member b (%rs-corrupt qs))
                                           (and (not (eq ps qs)) (%rs-torn-p ps)))))
                         (push (list (1+ a) (1- b) nil (if corrupt '(:corrupt) '(:missing)))
                               raw))))))))
    ;; merge adjacent ranges
    (let ((out '()))
      (dolist (g (nreverse raw))
        (destructuring-bind (f l bytes reasons) g
          (let ((prev (first out)))
            (if (and prev (= f (1+ (second prev))))
                (setf (first out) (list (first prev) l
                                        (and (third prev) bytes (+ (third prev) bytes))
                                        (append (fourth prev)
                                                (remove-if (lambda (r) (member r (fourth prev)))
                                                           reasons))))
                (push (list f l bytes reasons) out)))))
      (mapcar (lambda (g) (apply #'%make-gap g)) (nreverse out)))))

(defun %records-of (segs)
  "Every valid record of SEGS in line order (a line seen twice is kept once)."
  (let ((out '()) (last 0))
    (dolist (s segs)
      (loop for r across (%rs-records s)
            when (> (log-record-line r) last)
              do (push r out) (setf last (log-record-line r))))
    (nreverse out)))

(defun %selected-p (record streams types)
  (and (or (eq types t) (member (log-record-type record) types))
       (or (not (eq :output (log-record-type record)))
           (member (log-record-stream record) streams))))

(defun %read-log (log &key (start 1) end (streams '(:stdout :stderr)) (types '(:output)) limit)
  "→ (values records gaps next-line closed-p records-all)."
  (let* ((segs (%log-segs log))
         (all (%records-of segs))
         (gaps (%gaps segs all))
         (last-line (if all (log-record-line (car (last all))) 0))
         (closed (and segs (%segment-closed-p (car (last segs)))))
         (start (max 1 (or start 1)))
         (out '()) (count 0) (stopped nil))
    (dolist (r all)
      (let ((line (log-record-line r)))
        (when (and end (> line end)) (return))
        (when (>= line start)
          (when (and limit (>= count limit)) (setf stopped t) (return))
          (when (%selected-p r streams types)
            (push r out)
            (incf count)))))
    (setf out (nreverse out))
    (let* ((hi (cond (stopped (log-record-line (car (last out))))
                     (end end)
                     (t nil)))
           (next (cond (stopped (1+ hi))
                       (end (max start (1+ (min end last-line))))
                       (t (max start (1+ last-line)))))
           (touched (remove-if-not (lambda (g) (and (>= (log-gap-last-line g) start)
                                                    (or (null hi) (<= (log-gap-first-line g) hi))))
                                   gaps)))
      (values out touched next closed all))))

;;; ------------------------------------------------------------------
;;; The reader API

(defmethod log-read ((log t) &key (start 1) end (streams '(:stdout :stderr)) (types '(:output)) limit)
  (multiple-value-bind (records gaps next)
      (%read-log (unit-log log) :start start :end end :streams streams :types types :limit limit)
    (values records gaps next)))

(defmethod log-tail ((log t) n &key (streams '(:stdout :stderr)))
  (check-type n (integer 0))
  (multiple-value-bind (records gaps) (%read-log (unit-log log) :streams streams)
    (let* ((count (length records))
           (kept (nthcdr (max 0 (- count n)) records)))
      (values kept
              (if (< count n)
                  (if kept
                      (remove-if (lambda (g) (> (log-gap-first-line g)
                                                (log-record-line (car (last kept)))))
                                 gaps)
                      gaps)
                  (and kept
                       (remove-if-not (lambda (g)
                                        (and (> (log-gap-first-line g) (log-record-line (first kept)))
                                             (< (log-gap-last-line g)
                                                (log-record-line (car (last kept))))))
                                      gaps)))))))

(defmethod log-follow ((log t) &key (from 1) (wait 0) (streams '(:stdout :stderr)) limit)
  (let* ((ul (unit-log log))
         (deadline (+ (%now) (or wait 0))))
    (loop
      (let* ((sink (%live-sink ul))
             (version (and sink (%sink-version sink))))
        (multiple-value-bind (records gaps next closed)
            (%read-log ul :start from :streams streams :limit limit)
          (cond (records (return (values records gaps next nil)))
                (closed (return (values nil gaps next t))))
          (let ((left (- deadline (%now))))
            (when (<= left 0) (return (values nil gaps next nil)))
            (if sink
                (%sink-wait sink version (min left 1))
                (sleep (min 0.1 left)))))))))

(defmethod map-log-records (function (log t) &key (start 1) end (streams '(:stdout :stderr))
                                                  (types '(:output)))
  (dolist (r (%read-log (unit-log log) :start start :end end :streams streams :types types))
    (funcall function r))
  nil)

(defmethod log-segments ((log t))
  (let ((ul (unit-log log)))
    (mapcar (lambda (s)
              (let* ((recs (%rs-records s))
                     (n (length recs)))
                (make-instance 'log-segment-info
                               :number (%rs-number s) :path (%rs-path s) :index-path (%rs-idx-path s)
                               :first-line (%segment-first-line s)
                               :last-line (and (plusp n) (log-record-line (svref recs (1- n))))
                               :records n
                               :bytes (%rs-size s)
                               :output-bytes (loop for r across recs
                                                   when (eq :output (log-record-type r))
                                                     sum (+ (length (log-record-octets r))
                                                            (if (log-record-continued-p r) 0 1)))
                               :first-time (and (plusp n) (log-record-wall-time (svref recs 0)))
                               :last-time (and (plusp n) (log-record-wall-time (svref recs (1- n))))
                               :torn-p (and (%rs-torn-p s) t))))
            (%log-segs ul))))

(defmethod log-last-line ((log t))
  (let ((all (nth-value 4 (%read-log (unit-log log)))))
    (if all (log-record-line (car (last all))) 0)))

(defmethod log-closed-p ((log t))
  (let ((segs (%log-segs (unit-log log) :maintain nil)))
    (and segs (%segment-closed-p (car (last segs))) t)))

(defmethod rebuild-log-index ((log t) &key segment)
  (let* ((ul (unit-log log))
         (segs (%log-segs ul :maintain nil))
         (indexes (mapcar (lambda (s) (%read-octets (%rs-idx-path s))) segs))
         (stride (%log-stride ul segs indexes)))
    (loop for s in segs
          for v in indexes
          count (and (or (null segment) (= segment (%rs-number s)))
                     (%write-index s (or (%index-header-stride v) stride))))))

(defmethod delete-unit-log ((log t))
  (let* ((ul (unit-log log))
         (dir (unit-log-directory ul))
         (sink (or (slot-value ul 'sink) (%find-live-log-sink dir))))
    (when sink (%log-sink-go-memory-only sink))
    (%forget-log dir)
    (when (uiop:directory-exists-p dir)
      (pathlib:rmtree dir :missing-ok t))
    t))
