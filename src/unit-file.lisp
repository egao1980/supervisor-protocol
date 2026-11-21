(in-package #:supervisor-protocol)

;;; Durable unit files and the orphan sweep (the fallback path that works on
;;; every OS; H8a-1 adds OS-enforced death where the OS has it).
;;;
;;; One file per process unit, <state-directory>/units/<supervisor-id>--<unit-id>.unit,
;;; written atomically (temporary file + rename) after each launch and deleted
;;; when the unit is finished, evicted or shut down. Thread units have none.
;;; Content: one plist, printed WITH-STANDARD-IO-SYNTAX with *PACKAGE* KEYWORD,
;;; read with *READ-EVAL* NIL:
;;;
;;;   (:format 1 :supervisor-id "…" :supervisor-pid n :supervisor-os-start "…"
;;;    :unit-id "…" :kind :shell-job :pid n :pgid n-or-nil :os-start "…"
;;;    :command ("…" …) :cwd "…"-or-nil :inspector :ps-or-:cim)
;;;
;;; SWEEP-ORPHANS, per file:
;;;   unreadable → warn UNIT-FILE-UNREADABLE, delete, action :UNREADABLE.
;;;   owner alive (its pid runs with :SUPERVISOR-OS-START; in this image: the
;;;     supervisor id is one of LIVE-SUPERVISORS) → :LIVE-OWNER, file kept.
;;;   Unix (:pgid): members = processes in that group. The leader (pid = pgid)
;;;     running with another start time → :REUSED (a pid is not reused while
;;;     its group has members, so the old group is gone). Members → TERM the
;;;     group, wait GRACE, KILL what is left → :KILLED. None → :GONE.
;;;   Windows (no pgid): the root (pid) with another start time → :REUSED.
;;;     Otherwise the root (if it runs) and its descendants by ParentProcessId,
;;;     each started no earlier than its parent → taskkill /F each → :KILLED;
;;;     nothing → :GONE.
;;;   Every action but :LIVE-OWNER deletes the file.

(defun unit-file-directory (state-directory)
  "STATE-DIRECTORY/units/."
  (merge-pathnames (make-pathname :directory '(:relative "units"))
                   (uiop:ensure-directory-pathname state-directory)))

(defun %unit-file-pathname (unit)
  (let* ((sup (unit-supervisor unit))
         (dir (and sup (supervisor-state-directory sup))))
    (and dir
         (merge-pathnames (make-pathname :name (format nil "~A--~A" (supervisor-id sup) (unit-id unit))
                                         :type "unit")
                          (unit-file-directory dir)))))

(defun %supervisor-os-start (sup)
  (or (slot-value sup 'os-start)
      (setf (slot-value sup 'os-start) (os-process-start-time (current-pid)))))

(defun write-unit-file (unit)
  "Write (or rewrite) UNIT's file. → its pathname; NIL for a unit that has none."
  (let ((path (and (unit-pid unit) (%unit-file-pathname unit))))
    (when path
      (let* ((sup (unit-supervisor unit))
             (plist (list :format +unit-file-format+
                          :supervisor-id (supervisor-id sup)
                          :supervisor-pid (current-pid)
                          :supervisor-os-start (%supervisor-os-start sup)
                          :unit-id (unit-id unit)
                          :kind (unit-kind unit)
                          :pid (unit-pid unit)
                          :pgid (unit-pgid unit)
                          :os-start (unit-os-start-time unit)
                          :command (unit-command unit)
                          :cwd (uiop:native-namestring (or (unit-cwd unit) (uiop:getcwd)))
                          :inspector (if (uiop:os-windows-p) :cim :ps)))
             (plist (if (unit-confinement unit)
                        (append plist (%unit-file-confinement unit))
                        plist))
             (temp (make-pathname :type "tmp" :defaults path)))
        (ensure-directories-exist path)
        (with-open-file (s temp :direction :output :if-exists :supersede
                                :if-does-not-exist :create :external-format :utf-8)
          (with-standard-io-syntax
            (let ((*package* (find-package :keyword)))
              (prin1 plist s)
              (terpri s))))
        (when (and (uiop:os-windows-p) (probe-file path))
          (delete-file path))
        (rename-file temp path)
        (setf (slot-value unit 'file-path) path)
        path))))

(defun %delete-unit-file (unit)
  "Delete UNIT's file, if it has one. → NIL."
  (let ((path (unit-file-path unit)))
    (when path
      (setf (slot-value unit 'file-path) nil)
      (ignore-errors (uiop:delete-file-if-exists path))))
  nil)

(defun %valid-unit-plist-p (p)
  (and (listp p)
       (let ((n (ignore-errors (list-length p))))
         (and n (evenp n)))
       (loop for k in p by #'cddr always (keywordp k))
       (eql +unit-file-format+ (getf p :format))))

(defun read-unit-file (pathname)
  "→ the plist, or NIL after warning UNIT-FILE-UNREADABLE (bad syntax, #. forms,
not a plist, unknown :FORMAT)."
  (multiple-value-bind (plist cause)
      ;; Anything the reader can signal on a hostile or torn file is "unreadable".
      (handler-case
          (with-open-file (s pathname :external-format :utf-8)
            (with-standard-io-syntax
              (let ((*read-eval* nil)
                    (*package* (find-package :keyword)))
                (let ((p (read s)))
                  (if (%valid-unit-plist-p p)
                      p
                      (values nil "not a format 1 unit plist"))))))
        (error (e) (values nil e)))
    (or plist
        (progn (warn 'unit-file-unreadable :path pathname :cause cause)
               nil))))

(defclass sweep-record ()
  ((path :initarg :path :reader sweep-record-path)
   (unit-id :initarg :unit-id :initform nil :reader sweep-record-unit-id)
   (pid :initarg :pid :initform nil :reader sweep-record-pid)
   (pgid :initarg :pgid :initform nil :reader sweep-record-pgid)
   (action :initarg :action :reader sweep-record-action
           :documentation ":KILLED :GONE :REUSED :LIVE-OWNER :UNREADABLE")
   (killed-pids :initarg :killed-pids :initform nil :reader sweep-record-killed-pids)))

(defmethod print-object ((r sweep-record) stream)
  (print-unreadable-object (r stream :type t)
    (format stream "~A ~S~@[ ~S~]" (sweep-record-unit-id r) (sweep-record-action r)
            (sweep-record-killed-pids r))))

(defun %owner-alive-p (plist inspector)
  (let ((id (getf plist :supervisor-id))
        (pid (getf plist :supervisor-pid))
        (start (getf plist :supervisor-os-start)))
    (cond ((not (integerp pid)) nil)
          ((= pid (current-pid))
           (and (find id (live-supervisors) :key #'supervisor-id :test #'equal) t))
          (t (os-process-alive-p pid :start-time (and (stringp start) start)
                                     :inspector inspector)))))

(defun %sweep-group (pgid os-start grace inspector)
  "Unix rules. → (values action killed-pids)."
  (let* ((procs (list-os-processes :inspector inspector))
         (members (remove pgid procs :key #'os-process-pgid :test-not #'eql))
         (leader (find pgid procs :key #'os-process-pid)))
    (cond ((and leader (stringp os-start) (string/= os-start (os-process-start leader)))
           (values :reused '()))
          ((null members) (values :gone '()))
          (t
           (let ((pids (mapcar #'os-process-pid members)))
             (kill-os-process-group pgid)
             (let ((deadline (+ (%now) grace)))
               (loop while (and (< (%now) deadline)
                                (find pgid (list-os-processes :inspector inspector)
                                      :key #'os-process-pgid))
                     do (sleep 0.05)))
             (kill-os-process-group pgid :force t)
             (values :killed pids))))))

(defun %sweep-tree (pid os-start inspector)
  "Windows (or no pgid) rules. → (values action killed-pids)."
  (let* ((procs (list-os-processes :inspector inspector))
         (root (find pid procs :key #'os-process-pid))
         (ordered (typep inspector 'cim-inspector)))
    (if (and root (stringp os-start) (string/= os-start (os-process-start root)))
        (values :reused '())
        (let ((found (if root (list root) '()))
              (frontier (list (cons pid (if root (os-process-start root) os-start)))))
          (loop while frontier
                do (destructuring-bind (ppid . pstart) (pop frontier)
                     (dolist (p procs)
                       (when (and (eql ppid (os-process-ppid p))
                                  (/= (os-process-pid p) ppid)
                                  (not (member p found))
                                  (or (not ordered) (not (stringp pstart))
                                      (string>= (os-process-start p) pstart)))
                         (push p found)
                         (push (cons (os-process-pid p) (os-process-start p)) frontier)))))
          (if found
              (let ((pids (mapcar #'os-process-pid (reverse found))))
                (kill-os-processes pids :force t :inspector inspector)
                (values :killed pids))
              (values :gone '()))))))

(defun %sweep-file (path grace inspector)
  (let ((plist (read-unit-file path)))
    (flet ((done (action &optional killed)
             (unless (eq action :live-owner)
               (ignore-errors (uiop:delete-file-if-exists path)))
             (make-instance 'sweep-record :path path :action action :killed-pids killed
                                          :unit-id (getf plist :unit-id)
                                          :pid (getf plist :pid) :pgid (getf plist :pgid))))
      (cond ((null plist) (done :unreadable))
            ((%owner-alive-p plist inspector) (done :live-owner))
            (t
             (let ((pid (getf plist :pid))
                   (pgid (getf plist :pgid))
                   (os-start (getf plist :os-start)))
               (multiple-value-bind (action killed)
                   (cond ((and (integerp pgid) (plusp pgid) (typep inspector 'ps-inspector))
                          (%sweep-group pgid os-start grace inspector))
                         ((and (integerp pid) (plusp pid))
                          (%sweep-tree pid os-start inspector))
                         (t (values :gone '())))
                 (done action killed))))))))

(defun sweep-orphans (state-directory &key (grace +default-sweep-grace+)
                                           (inspector (default-os-inspector)))
  "Reap the processes recorded in STATE-DIRECTORY's unit files whose supervisor is
gone. → list of SWEEP-RECORDs, one per file. A missing directory → NIL."
  (let ((dir (unit-file-directory state-directory)))
    (when (uiop:directory-exists-p dir)
      ;; DIRECTORY returns truenames; report paths under DIR as given.
      (loop for found in (directory (merge-pathnames (make-pathname :name :wild :type "unit") dir))
            for path = (merge-pathnames (make-pathname :name (pathname-name found)
                                                       :type (pathname-type found))
                                        dir)
            collect (%sweep-file path grace inspector)))))
