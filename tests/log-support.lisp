(in-package #:supervisor-protocol/tests)

;;; H8b test support: octets, small log sinks in temporary directories, and
;;; helpers that look at segment files directly (never through the code under
;;; test) or reconstruct a stream's bytes from records.

(defparameter +lf+ 10)

(defun utf8 (string)
  (sb-ext:string-to-octets string :external-format :utf-8))

(defun octets (&rest parts)
  "Concatenate PARTS — strings (UTF-8), octet sequences, or single octets — into a
fresh (unsigned-byte 8) vector."
  (let ((out (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (dolist (p parts)
      (etypecase p
        (string (loop for b across (utf8 p) do (vector-push-extend b out)))
        ((integer 0 255) (vector-push-extend p out))
        (sequence (map nil (lambda (b) (vector-push-extend b out)) p))))
    (coerce out '(simple-array (unsigned-byte 8) (*)))))

(defun line-octets (string) (octets string +lf+))

(defun random-octets (n &key (seed 42))
  (let ((state (sb-ext:seed-random-state seed)))
    (let ((v (make-array n :element-type '(unsigned-byte 8))))
      (dotimes (i n v) (setf (aref v i) (random 256 state))))))

(defun call-with-log-sink (fn dir keys)
  (let ((sink (apply #'make-log-sink dir keys)))
    (unwind-protect (funcall fn sink)
      (ignore-errors (sink-close sink)))))

(defmacro with-log-sink ((var dir &rest keys) &body body)
  "BODY with VAR a standalone LOG-SINK writing into DIR (closed after)."
  `(call-with-log-sink (lambda (,var) ,@body) ,dir (list ,@keys)))

(defun write-lines (sink stream lines)
  "Each of LINES (strings) plus LF, one SINK-WRITE-OCTETS call per line."
  (dolist (l lines) (sink-write-octets sink stream (line-octets l))))

(defun numbered-lines (prefix n &key (width 5) (pad 0))
  "(\"<prefix>00001…\" …): N distinct lines, each PAD extra x characters long."
  (loop for i from 1 to n
        collect (format nil "~A~v,'0D~A" prefix width i (make-string pad :initial-element #\x))))

;;; conditions

(defun no-error-p (fn)
  "T when calling FN signals no ERROR."
  (handler-case (progn (funcall fn) t) (error () nil)))

;;; records

(defun texts (records) (mapcar #'log-record-text records))
(defun lines-of (records) (mapcar #'log-record-line records))
(defun of-type (type records) (remove type records :key #'log-record-type :test-not #'eq))

(defun contiguous-p (numbers)
  (loop for (a b) on numbers while b always (= b (1+ a))))

(defun stream-bytes (records stream)
  "The exact byte stream of STREAM reconstructed from :OUTPUT RECORDS (line order):
each record's octets, plus LF unless it is continued."
  (apply #'octets
         (loop for r in records
               when (and (eq :output (log-record-type r)) (eq stream (log-record-stream r)))
                 collect (log-record-octets r)
                 and unless (log-record-continued-p r) collect +lf+)))

(defun joined-lines (records stream)
  "Output lines of STREAM as strings, continued records joined."
  (let ((lines '()) (cur (make-string-output-stream)) (open nil))
    (dolist (r records)
      (when (and (eq :output (log-record-type r)) (eq stream (log-record-stream r)))
        (write-string (log-record-text r) cur)
        (setf open t)
        (unless (log-record-continued-p r)
          (push (get-output-stream-string cur) lines)
          (setf open nil))))
    (when open (push (get-output-stream-string cur) lines))
    (nreverse lines)))

(defun all-records (log) (log-read log :types t))

(defun gap-records-total (gaps) (reduce #'+ gaps :key #'log-gap-records))
(defun gap-bytes-total (gaps) (reduce #'+ gaps :key (lambda (g) (or (log-gap-bytes g) 0))))

;;; files, looked at directly

(defun seg-path (dir n)
  (merge-pathnames (format nil "~8,'0D.seg" n) (uiop:ensure-directory-pathname dir)))

(defun idx-path (dir n)
  (merge-pathnames (format nil "~8,'0D.idx" n) (uiop:ensure-directory-pathname dir)))

(defun segment-files (dir)
  (sort (directory (merge-pathnames "*.seg" (uiop:ensure-directory-pathname dir)))
        #'string< :key #'namestring))

(defun index-files (dir)
  (sort (directory (merge-pathnames "*.idx" (uiop:ensure-directory-pathname dir)))
        #'string< :key #'namestring))

(defun file-bytes (path)
  (with-open-file (s path :element-type '(unsigned-byte 8)) (file-length s)))

(defun files-bytes (paths) (reduce #'+ paths :key #'file-bytes))

(defun read-file-octets (path)
  (with-open-file (s path :element-type '(unsigned-byte 8))
    (let ((v (make-array (file-length s) :element-type '(unsigned-byte 8))))
      (read-sequence v s)
      v)))

(defun append-file-octets (path octets)
  (with-open-file (s path :direction :output :element-type '(unsigned-byte 8)
                          :if-exists :append)
    (write-sequence octets s)))

(defun write-file-octets (path octets)
  (with-open-file (s path :direction :output :element-type '(unsigned-byte 8)
                          :if-exists :supersede :if-does-not-exist :create)
    (write-sequence octets s)))

(defun physical-lines (path)
  "The LF-terminated lines of PATH as octet vectors (a last line without LF dropped)."
  (let ((v (read-file-octets path)) (out '()) (start 0))
    (loop for i from 0 below (length v)
          when (= (aref v i) +lf+)
            do (push (subseq v start (1+ i)) out) (setf start (1+ i)))
    (nreverse out)))

(defun first-field (line-octets)
  "The decimal line number at the start of a physical line."
  (parse-integer (map 'string #'code-char line-octets) :junk-allowed t))

;;; supervisors with logs

(defun unit-log-dir (sup unit)
  (unit-log-pathname (supervisor-state-directory sup) (supervisor-id sup) (unit-id unit)))

(defun writer-body (lines &key (stream :stdout) then)
  "A thread unit body printing LINES to *STANDARD-OUTPUT* (or *ERROR-OUTPUT*), then
calling THEN (a function of the unit) if given."
  (lambda (u)
    (let ((s (if (eq stream :stdout) *standard-output* *error-output*)))
      (dolist (l lines) (write-line l s))
      (finish-output s))
    (if then (funcall then u) :done)))
