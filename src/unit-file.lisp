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

(defun write-unit-file (unit)
  "Write (or rewrite) UNIT's file. → its pathname; NIL for a unit that has none."
  (declare (ignore unit))
  (%h8a-pending "write-unit-file"))

(defun read-unit-file (pathname)
  "→ the plist, or NIL after warning UNIT-FILE-UNREADABLE (bad syntax, #. forms,
not a plist, unknown :FORMAT)."
  (declare (ignore pathname))
  (%h8a-pending "read-unit-file"))

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

(defun sweep-orphans (state-directory &key (grace +default-sweep-grace+)
                                           (inspector (default-os-inspector)))
  "Reap the processes recorded in STATE-DIRECTORY's unit files whose supervisor is
gone. → list of SWEEP-RECORDs, one per file. A missing directory → NIL."
  (declare (ignore state-directory grace inspector))
  (%h8a-pending "sweep-orphans"))
