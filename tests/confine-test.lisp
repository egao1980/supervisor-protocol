(in-package #:supervisor-protocol/tests)

;;; R4d-0b: confined process units through a compute-protocol confining
;;; backend. The model backend confines nothing (it is compute-protocol's
;;; reference model): these tests check the plumbing — the launch argv and
;;; environment are the confined launch's, the scrub reaches the child, the
;;; private temp directory lives exactly as long as a launch, refusals are
;;; start errors, unit files say what confined the unit. The kernel laws are
;;; compute-protocol's confinement suite, run by each backend.

(defun model-backend (&rest keys)
  (apply #'sc:make-model-confinement-backend keys))

(defun area-confinement (area &rest keys)
  (apply #'sc:make-confinement :writable (list area) :env '(("KEEP" . "kept")) keys))

(defun physical (dir)
  (uiop:native-namestring (uiop:ensure-directory-pathname (truename dir))))

(defparameter +report-form+
  "(progn (format t \"~@{~A~^|~}~%\" (sb-ext:posix-getenv \"STACK_COMPUTE_CONFINEMENT\") (sb-ext:posix-getenv \"KEEP\") (sb-ext:posix-getenv \"EXTRA\") (or (sb-ext:posix-getenv \"SUP_R4D_SECRET\") \"gone\") (sb-ext:posix-getenv \"TMPDIR\") (uiop-free-cwd)) (finish-output))"
  "Prints model|kept|EXTRA|gone|TMPDIR|cwd. (uiop-free-cwd is defined by +cwd-defun+.)")

(defparameter +cwd-defun+
  "(defun uiop-free-cwd () (namestring (truename \".\")))")

(defun report-argv (&key (exit 0))
  (lisp-argv +cwd-defun+ +report-form+ (format nil "(sb-ext:exit :code ~D :abort t)" exit)))

(defun report-fields (unit)
  "The last report line of UNIT's output, split at |."
  (let* ((text (string-right-trim '(#\Newline #\Return) (unit-output unit)))
         (line (subseq text (1+ (or (position #\Newline text :from-end t) -1)))))
    (uiop:split-string line :separator "|")))

(defmacro with-area ((var) &body body)
  `(with-temp-dir (,var :prefix "sup-r4d-area") ,@body))

(defmacro with-confined-supervisor ((sup &key (backend '(model-backend)) confinement) &body body)
  `(with-supervisor (,sup :compute-executor (make-compute-executor :compute-backend ,backend
                                                                   :confinement ,confinement))
     ,@body))

(defun real-failure (e)
  (not (search "R4d-0 pending" (princ-to-string e))))

(defclass wrapping-backend (sc:model-confinement-backend) ())

(defmethod sc:backend-confine-argv ((b wrapping-backend) confinement argv directory tmp)
  (declare (ignore confinement argv directory tmp))
  (values (lisp-argv "(format t \"wrapped|~A|~A~%\" (sb-ext:posix-getenv \"KEEP\") (sb-ext:posix-getenv \"BY\"))"
                     "(finish-output)")
          '(("BY" . "wrapping"))
          nil))

;;; Given code

(deftest compute-executor-construction
  (let ((e (make-compute-executor :compute-backend (model-backend))))
    (ok (typep e 'compute-executor))
    (ok (typep e 'process-executor) "a process executor")
    (ok (typep (compute-executor-compute-backend e) 'sc:model-confinement-backend))
    (ok (null (compute-executor-confinement e)))
    (ok (executor-supports-p e :confined))
    (ok (executor-supports-p e :pid))
    (ok (executor-supports-p e :tree))
    (ok (executor-supports-p e :input))
    (ng (executor-supports-p (make-process-executor) :confined)))
  (ok (signals (make-compute-executor :compute-backend (sc:make-native-process-backend))
               'supervisor-error)
      "the native backend does not confine")
  (ok (signals (make-compute-executor) 'supervisor-error)))

(deftest supervisor-compute-executor-slot
  (with-supervisor (sup)
    (ok (null (supervisor-compute-executor sup))))
  (let ((e (make-compute-executor :compute-backend (model-backend))))
    (with-supervisor (sup :compute-executor e)
      (ok (eq e (supervisor-compute-executor sup))))))

(deftest unconfined-units-unchanged
  (with-supervisor (sup)
    (let ((u (start-unit sup :shell-job :command (exit-argv 0))))
      (wait-unit u :timeout 30)
      (ok (eq :exited (unit-state u)))
      (ok (null (unit-confinement u)))
      (ng (unit-confined-p u))
      (ok (null (unit-confined-launch u))))))

;;; Confined units

(deftest confined-unit-runs
  (with-area (area)
    (let ((old (uiop:getenv "SUP_R4D_SECRET")))
      (setf (uiop:getenv "SUP_R4D_SECRET") "leak")
      (unwind-protect
           (with-confined-supervisor (sup)
             (let* ((c (area-confinement area))
                    (u (start-unit sup :shell-job :command (report-argv) :confinement c
                                                  :env '(("EXTRA" . "extra")))))
               (ok (eq c (unit-confinement u)))
               (ok (unit-confined-p u))
               (ok (eq :exited (wait-unit u :timeout 60)))
               (let ((f (report-fields u)))
                 (ok (equal "model" (first f)) "the backend's extra env")
                 (ok (equal "kept" (second f)) "the confinement's env")
                 (ok (equal "extra" (third f)) "the unit's :env survives the scrub")
                 (ok (equal "gone" (fourth f)) "the image's other variables are scrubbed")
                 (ok (and (fifth f) (search "stack-compute" (string-downcase (fifth f))))
                     "TMPDIR is the launch's private directory")
                 (ok (equal (physical area) (sixth f)) "the default directory is the area"))))
        (setf (uiop:getenv "SUP_R4D_SECRET") (or old ""))))))

(deftest confined-unit-cwd
  (with-area (area)
    (let ((sub (merge-pathnames "sub/" area)))
      (ensure-directories-exist sub)
      (with-confined-supervisor (sup)
        (let ((u (start-unit sup :shell-job :command (report-argv) :cwd sub
                                            :confinement (area-confinement area))))
          (wait-unit u :timeout 60)
          (ok (equal (physical sub) (sixth (report-fields u))) "UNIT-CWD is the launch directory"))))))

(deftest confined-unit-launch-lifetime
  (with-area (area)
    (with-confined-supervisor (sup)
      (let* ((u (start-unit sup :shell-job :command (sleeper-argv 30)
                                           :confinement (area-confinement area)))
             (launch (unit-confined-launch u))
             (tmp (and launch (sc:confined-launch-tmp launch))))
        (ok (sc:confined-launch-p launch) "the current launch")
        (ok (and tmp (uiop:directory-exists-p tmp)) "its private directory exists while it runs")
        (ng (and launch (sc:confined-launch-released-p launch)))
        (ok (eq :killed (stop-unit u :grace 2)))
        (ok (wait-until (lambda () (sc:confined-launch-released-p launch)) :timeout 10)
            "released after the end")
        (ng (and tmp (uiop:directory-exists-p tmp)) "the private directory is gone")))))

(deftest confined-unit-backend-argv
  ;; The process that runs is the launch's argv, with exactly the launch's env.
  (with-area (area)
    (with-confined-supervisor (sup :backend (make-instance 'wrapping-backend))
      (let ((u (start-unit sup :shell-job :command '("this-program-does-not-exist-r4d")
                                          :confinement (area-confinement area))))
        (ok (eq :exited (wait-unit u :timeout 60)))
        (ok (search "wrapped|kept|wrapping" (unit-output u)))))))

(deftest confined-unit-default-confinement
  (with-area (area)
    (let* ((c (area-confinement area))
           (exec (make-compute-executor :compute-backend (model-backend) :confinement c)))
      (with-supervisor (sup)
        (let ((u (start-unit sup :shell-job :command (report-argv) :executor exec)))
          (ok (eq c (unit-confinement u)) "the executor's confinement")
          (wait-unit u :timeout 60)
          (ok (equal "kept" (second (report-fields u)))))))))

(deftest confined-unit-restart-new-launch
  (with-area (area)
    (with-confined-supervisor (sup)
      (let ((u (start-unit sup :rpc :command (report-argv :exit 3) :restart :on-failure
                                    :max-restarts 1 :backoff '(0.1 1 0.1)
                                    :confinement (area-confinement area))))
        (ok (wait-until (lambda () (and (eq :failed (unit-state u)) (unit-finished-p u))) :timeout 60))
        (ok (= 1 (unit-restart-count u)))
        (let* ((lines (remove "" (uiop:split-string (unit-output u) :separator '(#\Newline))
                              :test #'string=))
               (tmps (mapcar (lambda (l) (fifth (uiop:split-string l :separator "|"))) lines)))
          (ok (= 2 (length lines)) "two launches")
          (ok (and (= 2 (length tmps)) (string/= (first tmps) (second tmps)))
              "each launch has its own private directory")
          (ok (notany #'uiop:directory-exists-p (remove nil tmps)) "both released"))))))

(deftest-parametrize confined-start-errors
    ((case)
     :ids ("no-compute-executor" "function" "process-executor" "no-confinement" "not-a-confinement")
     (:no-compute-executor) (:function) (:process-executor) (:no-confinement) (:not-a-confinement))
  (with-area (area)
    (with-supervisor (sup :compute-executor (and (not (eq case :no-compute-executor))
                                                 (make-compute-executor :compute-backend (model-backend))))
      (let ((c (area-confinement area))
            (e nil))
        (handler-case
            (ecase case
              (:no-compute-executor (start-unit sup :shell-job :command (exit-argv 0) :confinement c))
              (:function (start-unit sup :subagent :function (lambda (u) u) :confinement c))
              (:process-executor (start-unit sup :shell-job :command (exit-argv 0) :confinement c
                                                            :executor (make-process-executor)))
              (:no-confinement (start-unit sup :shell-job :command (exit-argv 0)
                                                         :executor (supervisor-compute-executor sup)))
              (:not-a-confinement (start-unit sup :shell-job :command (exit-argv 0) :confinement :yes)))
          (unit-start-error (c) (setf e c))
          (error (c) (when (real-failure c) (setf e c))))
        (ok (typep e 'unit-start-error) (format nil "unit-start-error, got ~a" e))
        (ok (null (supervisor-units sup)) "nothing registered")))))

(deftest confined-start-refused-by-backend
  (with-area (area)
    (with-confined-supervisor (sup :backend (model-backend :features '(:filesystem :network-none)))
      (let ((e (handler-case
                   (progn (start-unit sup :shell-job :command (exit-argv 0)
                                                     :confinement (area-confinement area :cpu-limit 5))
                          nil)
                 (unit-start-error (c) c))))
        (ok e "unit-start-error")
        (ok (and e (typep (unit-start-error-cause e) 'sc:confinement-unsupported))
            "its cause is the compute condition")
        (ok (null (supervisor-units sup)))
        ;; No id was consumed.
        (let ((u (start-unit sup :shell-job :command (exit-argv 0)
                                            :confinement (area-confinement area :deny-read nil))))
          (ok (equal "shell-job-1" (unit-id u))))))))

(deftest confined-unit-file
  (with-temp-dir (state)
    (with-area (area)
      (with-supervisor (sup :state-directory state
                            :compute-executor (make-compute-executor :compute-backend (model-backend)))
        (let ((u (start-unit sup :shell-job :command (sleeper-argv 30)
                                            :confinement (area-confinement area :deny-read nil))))
          (let* ((files (units-dir-files state))
                 (plist (and files (read-unit-file (first files)))))
            (ok (= 1 (length files)))
            (ok (eql +unit-file-format+ (getf plist :format)) "format unchanged")
            (ok (equal "model-confinement-backend" (getf plist :compute-backend)))
            (ok (equal (list (physical area)) (getf (getf plist :confinement) :writable))
                "the resolved confinement's summary")
            (ok (eq :none (getf (getf plist :confinement) :network))))
          (stop-unit u :grace 2)
          (ok (null (units-dir-files state)) "deleted when finished"))))))

(deftest confined-unit-input
  (with-area (area)
    (with-confined-supervisor (sup)
      (let ((u (start-unit sup :rpc :command (echo-server-argv) :confinement (area-confinement area))))
        (send-unit-input u (format nil "ping~%"))
        (ok (wait-until (lambda () (search "ping" (unit-output u))) :timeout 30) "stdin and stdout work")
        (close-unit-input u)
        (ok (eq :exited (wait-unit u :timeout 30)))))))
