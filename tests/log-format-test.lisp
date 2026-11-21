(in-package #:supervisor-protocol/tests)

;;; H8b record format: golden lines (CRC-32 values computed independently with
;;; zlib.crc32), escapes, base64, and lines that must be rejected.

(defun tab () (string #\Tab))

(defparameter +golden-header-detail+
  '(:format 1 :supervisor-id "4242-0a1b2c3d" :unit-id "shell-job-1" :kind :shell-job
    :segment 1 :first-line 1 :dropped-through 0 :dropped-records 0 :dropped-bytes 0))

(defun golden-line-2 ()
  ;; payload octets: say "hi"<TAB>to C:\Temp\x<CR> naïve
  (octets "2 1700000000001 1235 et+ say \"hi\"" (tab) "to C:\\\\Temp\\\\x\\x0d na" #xc3 #xaf
          "ve 479e83d0" +lf+))

(deftest log-constants-and-conditions
  (ok (= 1 +log-format+))
  (ok (= 1048576 +default-log-segment-size+))
  (ok (= 5 +default-log-segments+))
  (ok (= 268435456 +default-log-total-cap+))
  (ok (= 8192 +default-log-max-record+))
  (ok (= 64 +log-index-stride+))
  (ok (equal +log-record-types+ '(:output :header :drop :end)))
  (ok (equal +log-gap-reasons+ '(:rotated :total-cap :corrupt :missing)))
  (ok (subtypep 'log-write-error 'supervisor-error))
  (ok (subtypep 'log-not-found 'supervisor-error))
  (ok (subtypep 'log-sink 'head-tail-buffer) "the log sink keeps the in-memory head + tail")
  (let ((c (make-condition 'log-write-error :path #p"/x/00000001.seg" :cause "disk full")))
    (ok (search "disk full" (princ-to-string c)))))

(deftest log-format-golden-text-record
  (let* ((r (make-log-record :line 1 :wall-time 1700000000000 :monotonic 1234
                             :type :output :stream :stdout :octets (utf8 "hello world")))
         (bytes (format-log-record r))
         (golden (octets "1 1700000000000 1234 ot. hello world 12fd0add" +lf+)))
    (ok (equalp golden bytes))
    (let ((p (parse-log-record golden)))
      (ok (typep p 'log-record))
      (ok (= 1 (log-record-line p)))
      (ok (= 1700000000000 (log-record-wall-time p)))
      (ok (= 1234 (log-record-monotonic p)))
      (ok (eq :output (log-record-type p)))
      (ok (eq :stdout (log-record-stream p)))
      (ok (not (log-record-continued-p p)))
      (ok (string= "hello world" (log-record-text p))))
    (ok (= (length golden) (nth-value 1 (parse-log-record golden))) "second value: after the LF")))

(deftest log-format-golden-escapes
  (let* ((payload (octets "say \"hi\"" (tab) "to C:\\Temp\\x" 13 " naïve"))
         (r (make-log-record :line 2 :wall-time 1700000000001 :monotonic 1235
                             :stream :stderr :octets payload :continued-p t)))
    (ok (equalp (golden-line-2) (format-log-record r)) "\\ doubled, CR as \\x0d, TAB and UTF-8 raw")
    (let ((p (parse-log-record (golden-line-2))))
      (ok (eq :stderr (log-record-stream p)))
      (ok (log-record-continued-p p))
      (ok (equalp payload (log-record-octets p))))))

(deftest log-format-golden-base64
  (let ((invalid (make-log-record :line 3 :wall-time 1700000000002 :monotonic 1236
                                  :stream :stdout :octets #(255 254 0 65)))
        (controls (make-log-record :line 5 :wall-time 1700000000004 :monotonic 1238
                                   :stream :stderr :octets (make-array 12 :initial-element 0))))
    (ok (equalp (octets "3 1700000000002 1236 ob. //4AQQ== 19ae7754" +lf+)
                (format-log-record invalid))
        "not UTF-8 → base64")
    (ok (equalp (octets "5 1700000000004 1238 eb. AAAAAAAAAAAAAAAA 72a0e5f0" +lf+)
                (format-log-record controls))
        "valid UTF-8 whose escaped form is longer than base64 → base64")
    (ok (equalp #(255 254 0 65)
                (log-record-octets (parse-log-record (format-log-record invalid)))))))

(deftest log-format-golden-empty-and-header
  (ok (equalp (octets "4 1700000000003 1237 ot.  45a0e7c7" +lf+)
              (format-log-record (make-log-record :line 4 :wall-time 1700000000003 :monotonic 1237
                                                  :stream :stdout :octets #())))
      "an empty line")
  (let ((h (make-log-record :line 1 :wall-time 1700000000000 :monotonic 0 :type :header
                            :detail +golden-header-detail+))
        (golden (octets "1 1700000000000 0 ht. (FORMAT 1 SUPERVISOR-ID \"4242-0a1b2c3d\" UNIT-ID \"shell-job-1\" KIND SHELL-JOB SEGMENT 1 FIRST-LINE 1 DROPPED-THROUGH 0 DROPPED-RECORDS 0 DROPPED-BYTES 0) 334a0c98" +lf+)))
    (ok (equalp golden (format-log-record h)))
    (let ((p (parse-log-record golden)))
      (ok (eq :header (log-record-type p)))
      (ok (null (log-record-stream p)))
      (ok (equal +golden-header-detail+ (log-record-detail p))))))

(deftest-parametrize log-format-round-trip
    ((octets stream continued)
     ((utf8 "plain ascii") :stdout nil)
     ((utf8 "") :stderr t)
     ((utf8 "back\\slash and \\x41 literally") :stdout nil)
     ((utf8 "日本語のテキスト") :stderr nil)
     ((octets 0 1 2 31 127 9 13) :stdout t)
     ((random-octets 300 :seed 7) :stdout nil)
     ((octets #xe6 #x97) :stderr t))
  (let* ((r (make-log-record :line 77 :wall-time 1 :monotonic 2 :stream stream
                             :octets octets :continued-p continued))
         (p (parse-log-record (format-log-record r))))
    (ok (equalp octets (log-record-octets p)))
    (ok (eq stream (log-record-stream p)))
    (ok (eq continued (log-record-continued-p p)))
    (ok (= 77 (log-record-line p)))))

(deftest log-format-rejects-bad-lines
  (let ((good (octets "1 1700000000000 1234 ot. hello world 12fd0add" +lf+)))
    (ok (null (parse-log-record (subseq good 0 (1- (length good))))) "no LF: torn")
    (ok (null (parse-log-record (subseq good 0 20))) "cut in the middle")
    (let ((flipped (copy-seq good)))
      (setf (aref flipped 25) (char-code #\H))
      (ok (null (parse-log-record flipped)) "payload changed: CRC mismatch"))
    (ok (null (parse-log-record (make-array 40 :element-type '(unsigned-byte 8)
                                               :initial-element 0)))
        "zero-filled tail")
    (ok (null (parse-log-record (octets "garbage line" +lf+))))
    (ok (null (parse-log-record (octets "1 1 1 ot. bad\\q escape 00000000" +lf+))))
    (let ((two (octets good good)))
      (ok (= 1 (log-record-line (parse-log-record two :start (length good)))) ":start"))))
