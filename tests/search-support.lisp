(in-package #:supervisor-protocol/tests)

;;; H8c test support. The recorded corpus (tests/fixtures/corpus/*.log) was
;;; captured from real runs (make + cc with a compile error, a Rove run with two
;;; failures, a Node dev server throwing TypeErrors, pytest with two failures),
;;; with local paths replaced by /home/dev/…. Logs are written through
;;; standalone LOG-SINKs (no supervisor), one SINK-WRITE-OCTETS per line, so
;;; the same bytes land on every OS. Expected refs are found by looking the
;;; fixture line up in the log with LOG-READ, never through the index.

(defparameter +corpus+ '("build" "rove" "pytest" "devserver"))

(defun corpus-path (name)
  (asdf:system-relative-pathname "supervisor-protocol"
                                 (format nil "tests/fixtures/corpus/~A.log" name)))

(defun corpus-lines (name)
  "The fixture's lines (UTF-8; a CR before LF dropped, in case a checkout added it)."
  (with-open-file (s (corpus-path name) :external-format :utf-8)
    (loop for l = (read-line s nil) while l
          collect (string-right-trim '(#\Return) l))))

(defun devserver-stream (line)
  "The dev server's own request log went to stdout (timestamped lines); stack
traces and Node's warnings to stderr."
  (if (and (>= (length line) 5) (string= "2026-" line :end2 5)) :stdout :stderr))

(defun corpus-stream (name line)
  (if (equal name "devserver") (devserver-stream line) :stdout))

(defun write-corpus-lines (sink name lines)
  (dolist (l lines)
    (sink-write-octets sink (corpus-stream name l) (line-octets l))))

(defun make-corpus-log (root name &key (close t) (segment-size 1048576) (segments 5)
                                     (max-record 8192) lines)
  "A log directory ROOT/<name>/ holding fixture NAME (or LINES) written through a
standalone sink. → (values directory sink); the sink is closed when CLOSE."
  (let* ((dir (uiop:ensure-directory-pathname (merge-pathnames name root)))
         (sink (make-log-sink dir :supervisor-id "rec" :unit-id name :kind :shell-job
                                  :segment-size segment-size :segments segments
                                  :max-record max-record)))
    (write-corpus-lines sink name (or lines (corpus-lines name)))
    (when close (sink-close sink))
    (values dir sink)))

(defun call-with-corpus-index (fn names &key (persist t))
  (with-temp-dir (root)
    (let ((index (ss:make-log-index :persist persist)))
      (unwind-protect
           (progn
             (dolist (n names)
               (ss:index-add-log index (make-corpus-log root n) :key n))
             (funcall fn index root))
        (ignore-errors (ss:close-log-index index))))))

(defmacro with-corpus-index ((index root &key (names '+corpus+) (persist t)) &body body)
  "BODY with INDEX over the corpus logs NAMES (keys = names) under temp dir ROOT."
  `(call-with-corpus-index (lambda (,index ,root) (declare (ignorable ,root)) ,@body)
                           ,names :persist ,persist))

(defun ref-of-text (log key text &key (nth 0))
  "The ref of the NTH (0-based) output record of LOG whose text is TEXT."
  (let ((matches (remove text (log-read log) :key #'log-record-text :test-not #'string=)))
    (let ((r (nth nth matches)))
      (and r (ss:format-log-ref key (log-record-segment r) (log-record-line r))))))

(defun corpus-ref (index key fixture-line &key (nth 0))
  "The ref of line FIXTURE-LINE (1-based) of fixture KEY in INDEX's log KEY."
  (ref-of-text (ss:index-unit-log index key) key
               (nth (1- fixture-line) (corpus-lines key)) :nth nth))

(defun line-of-fixture (index key fixture-line)
  (nth-value 2 (ss:parse-log-ref (corpus-ref index key fixture-line))))

(defun hit-refs (hits) (mapcar #'ss:hit-ref hits))

(defun top-refs (index query &rest keys)
  (hit-refs (apply #'ss:search-logs index query keys)))

(defun in-top-p (ref index query &rest keys)
  (and ref (member ref (apply #'top-refs index query keys) :test #'equal) t))

(defun sorted-terms (text)
  (sort (remove-duplicates (ss:tokenize text) :test #'string=) #'string<))

(defun esc (&rest parts)
  "PARTS joined, with :ESC as the escape character."
  (with-output-to-string (o)
    (dolist (p parts) (if (eq p :esc) (write-char (code-char 27) o) (princ p o)))))

(defun u (code) (string (code-char code)))

(defun six-files (dir)
  (sort (directory (merge-pathnames "*.six" (uiop:ensure-directory-pathname dir)))
        #'string< :key #'namestring))

(defun six-path (dir n)
  (merge-pathnames (format nil "~8,'0D.six" n) (uiop:ensure-directory-pathname dir)))

(defun stat (index key) (getf (ss:index-stats index) key))

(defclass counting-embedder ()
  ((calls :initform '() :accessor embedder-calls)
   (lock :initform (bt2:make-lock :name "embedder"))))

(defmethod ss:embed-texts ((e counting-embedder) texts)
  (bt2:with-lock-held ((slot-value e 'lock))
    (push (copy-list texts) (embedder-calls e)))
  (mapcar (lambda (s) (make-array 1 :element-type 'single-float
                                    :initial-element (coerce (length s) 'single-float)))
          texts))
