(in-package #:supervisor-protocol)

;;; Executors run a unit. The supervisor owns state, events, caps, restarts
;;; and unit files; an executor only launches, stops, waits and feeds input.
;;; H8a: PROCESS-EXECUTOR (process-protocol) and THREAD-EXECUTOR (bt2).
;;; H8a-1 adds pty, tmux and compute-backend executors behind the same GFs.
;;;
;;; The supervisor calls EXECUTOR-WAIT with no timeout on a thread of its own
;;; per launch, and publishes the unit's final state from there, so a state
;;; never stays :RUNNING after the work ended (no caller needs to poll).
;;;
;;; H8b: the process executor launches with :ELEMENT-TYPE :OCTETS (all three
;;; pipes), its pumps hand each chunk to SINK-WRITE-OCTETS unchanged (no
;;; decoding: binary output reaches the sink byte for byte; a chunk is what one
;;; read returned, at most +PUMP-CHUNK+ octets), and EXECUTOR-SEND-INPUT writes
;;; TEXT encoded as UTF-8. The thread executor binds *STANDARD-OUTPUT* and
;;; *ERROR-OUTPUT* to SINK-OUTPUT-STREAMs of the unit's sink (:STDOUT / :STDERR)
;;; around the body and flushes them however the body ends. A LOG-WRITE-ERROR
;;; is signalled on the writing thread (a pump, or the unit's own thread), so a
;;; body's own HANDLER-BIND sees it first (see src/conditions.lisp).

(defclass executor () ())

(defclass process-executor (executor)
  ((backend :initarg :backend :initform nil :reader process-executor-backend
            :documentation "A PROCESS-PROTOCOL backend, or NIL for PROCESS-PROTOCOL:*PROCESS-BACKEND*."))
  (:documentation "UNIT-COMMAND through PROCESS-PROTOCOL:LAUNCH :PROCESS-GROUP T, stdin,
stdout and stderr as octet pipes (output pumped to SINK-WRITE-OCTETS, input UTF-8), :DIRECTORY UNIT-CWD, :ENV UNIT-ENV.
Graceful stop: KILL :TREE T (Unix SIGTERM to the group; Windows the job object,
always forced). Force: KILL :TREE T :FORCE T. On exit (by itself or not) the
rest of the tree is killed too (EXECUTOR-RELEASE), so nothing the unit started
outlives it."))

(defclass thread-executor (executor) ()
  (:documentation "UNIT-FUNCTION, called with the unit on a new bt2 thread, with
*CURRENT-UNIT* bound to the unit and *CURRENT-SCOPE* to its owner. Normal
return → :EXITED (value in UNIT-RESULT); an unhandled ERROR → :FAILED (in
UNIT-CONDITION; the thread does not enter the debugger). Graceful stop sets
UNIT-STOP-REQUESTED-P (cooperative: the body polls it). Force interrupts the
thread (bt2:interrupt-thread) and unwinds it to the unit's top, running its
UNWIND-PROTECT cleanups."))

(defun make-process-executor (&key backend)
  (make-instance 'process-executor :backend backend))

(defun make-thread-executor ()
  (make-instance 'thread-executor))

(defgeneric executor-launch (executor unit)
  (:documentation "Start UNIT (read its command / function / cwd / env; write output to
UNIT-OUTPUT-SINK). → a handle. Cannot start → UNIT-START-ERROR."))

(defgeneric executor-pid (executor handle)
  (:documentation "The OS pid of the direct child, or NIL.")
  (:method ((executor executor) handle) (declare (ignore handle)) nil))

(defgeneric executor-pgid (executor handle)
  (:documentation "The OS process group id (Unix, tree-capable launch), or NIL.")
  (:method ((executor executor) handle) (declare (ignore handle)) nil))

(defgeneric executor-stop (executor unit handle &key force)
  (:documentation "Ask HANDLE to stop: gracefully, or with FORCE so it cannot refuse;
tree-wide where the executor supports :TREE. Returns at once. → T."))

(defgeneric executor-wait (executor unit handle &key timeout)
  (:documentation "Block until HANDLE has ended or TIMEOUT seconds passed (NIL: no limit).
→ (values ended-p exit-code condition result): EXIT-CODE an integer for a
process, NIL for a thread; CONDITION the error that ended a thread; RESULT a
thread's return value."))

(defgeneric executor-alive-p (executor unit handle))

(defgeneric executor-send-input (executor unit handle text)
  (:documentation "Write TEXT to the unit's stdin and flush. → T. Not supported →
EXECUTOR-UNSUPPORTED."))

(defgeneric executor-close-input (executor unit handle)
  (:documentation "H8c: close the unit's stdin, so a program reading it sees end of
file. → T when this call closed it, NIL when it was already closed. Not supported
→ EXECUTOR-UNSUPPORTED (the default method; the thread executor has no stdin)."))

(defmethod executor-close-input ((executor executor) unit handle)
  (declare (ignore unit handle))
  (error 'executor-unsupported :executor executor :operation :close-input))

(defmethod executor-close-input ((executor process-executor) unit handle)
  (declare (ignore unit handle))
  (%h8c-pending "executor-close-input (process executor)"))

(defgeneric executor-release (executor unit handle)
  (:documentation "After the end: kill what is left of the tree, close the pipes, join the
pump threads, close nothing that belongs to the sink. → NIL.")
  (:method ((executor executor) unit handle) (declare (ignore unit handle)) nil))

(defgeneric executor-supports-p (executor capability)
  (:documentation "Capabilities: :PID (EXECUTOR-PID answers), :TREE (stops reach
descendants), :INPUT (EXECUTOR-SEND-INPUT works), :GRACEFUL-STOP (a graceful stop
differs from a forced one). Unknown → NIL.")
  (:method ((executor executor) capability) (declare (ignore capability)) nil)
  (:method ((executor process-executor) capability)
    (case capability
      ((:pid :tree :input) t)
      (:graceful-stop (not (uiop:os-windows-p)))
      (t nil)))
  (:method ((executor thread-executor) capability)
    (eq capability :graceful-stop)))

(defmethod executor-send-input ((executor executor) unit handle text)
  (declare (ignore unit handle text))
  (error 'executor-unsupported :executor executor :operation :input))

;;; Internal: the supervisor tells an executor that the unit is registered (the
;;; thread executor holds the body back until then, so units a thread starts at
;;; once always come after it in start order).

(defgeneric %executor-started (executor unit handle)
  (:method ((executor executor) unit handle) (declare (ignore unit handle)) nil))

;;; process-executor

(defclass process-unit-handle ()
  ((process :initarg :process :reader %ph-process)
   (pid :initarg :pid :reader %ph-pid)
   (lock :initform (bt2:make-lock :name "process unit input"))
   (cv :initform (bt2:make-condition-variable :name "process unit pumps"))
   (pumps-left :initform 0)
   (pumps :initform '())
   (released :initform nil)))

(defparameter +pump-chunk+ 4096)

(defun %read-chunk (stream buf)
  "Block for one octet, then take what is available without blocking (at most
+PUMP-CHUNK+). → the count, or NIL at EOF."
  (declare (type (simple-array (unsigned-byte 8) (*)) buf))
  (let ((b (read-byte stream nil :eof)))
    (if (eq b :eof)
        nil
        (let ((n 1))
          (declare (type fixnum n))
          (setf (aref buf 0) b)
          (loop while (and (< n +pump-chunk+) (listen stream))
                do (let ((b2 (read-byte stream nil :eof)))
                     (when (eq b2 :eof) (return))
                     (setf (aref buf n) b2)
                     (incf n)))
          n))))

(defun %pump (stream sink which)
  "Copy STREAM (octets) into SINK (as WHICH) until EOF. Each chunk is handed to
SINK-WRITE-OCTETS as it arrives, unchanged."
  (let ((buf (make-array +pump-chunk+ :element-type '(unsigned-byte 8))))
    (loop
      ;; The pipe went away (closed under us): the pump ends.
      (let ((n (handler-case (%read-chunk stream buf) (error () nil))))
        (unless n (return))
        ;; A broken custom sink must not take the pump into the debugger.
        (handler-case (sink-write-octets sink which buf :end n)
          (error () (return)))))))

(defun %start-pump (handle stream sink which unit)
  (with-slots (lock cv pumps-left pumps) handle
    (bt2:with-lock-held (lock) (incf pumps-left))
    (push (bt2:make-thread
           (lambda ()
             (unwind-protect (%pump stream sink which)
               (bt2:with-lock-held (lock)
                 (decf pumps-left)
                 (bt2:condition-broadcast cv))))
           :name (format nil "supervisor pump ~A ~(~A~)" (unit-id unit) which))
          pumps)))

(defmethod executor-launch ((executor process-executor) unit)
  (let* ((command (unit-command unit))
         (backend (process-executor-backend executor))
         (process
           (handler-case
               (apply #'pp:launch command
                      :process-group t
                      :input :stream :output :stream :error-output :stream
                      :element-type :octets
                      (append (and (unit-cwd unit) (list :directory (unit-cwd unit)))
                              (and (unit-env unit) (list :env (unit-env unit)))
                              (and backend (list :backend backend))))
             (error (e)
               (error 'unit-start-error :supervisor (unit-supervisor unit)
                                        :kind (unit-kind unit) :command command :cause e))))
         (handle (make-instance 'process-unit-handle :process process :pid (pp:pid process)))
         (sink (unit-output-sink unit)))
    (when (pp:stdout process) (%start-pump handle (pp:stdout process) sink :stdout unit))
    (when (pp:stderr process) (%start-pump handle (pp:stderr process) sink :stderr unit))
    handle))

(defmethod executor-pid ((executor process-executor) handle)
  (%ph-pid handle))

(defmethod executor-pgid ((executor process-executor) handle)
  (and (not (uiop:os-windows-p)) (%ph-pid handle)))

(defun %kill-process-tree (process force)
  "PP:KILL :TREE T; a handle without a group falls back to the direct child."
  (handler-bind ((pp:process-group-error
                   (lambda (c) (let ((r (find-restart 'continue c))) (when r (invoke-restart r))))))
    (pp:kill process :tree t :force force)))

(defmethod executor-stop ((executor process-executor) unit handle &key force)
  (declare (ignore unit))
  (%kill-process-tree (%ph-process handle) force)
  t)

(defmethod executor-wait ((executor process-executor) unit handle &key timeout)
  (declare (ignore unit))
  (let ((process (%ph-process handle)))
    (if timeout
        (handler-case (values t (pp:wait process :timeout timeout) nil nil)
          (pp:process-timeout-error () (values nil)))
        ;; No limit: short bounded waits in a loop (the backend polls the child),
        ;; so the end is seen within a fraction of a second.
        (loop
          (handler-case (return (values t (pp:wait process :timeout 0.5) nil nil))
            (pp:process-timeout-error () nil))))))

(defmethod executor-alive-p ((executor process-executor) unit handle)
  (declare (ignore unit))
  (pp:alive-p (%ph-process handle)))

(defmethod executor-send-input ((executor process-executor) unit handle text)
  (let ((stdin (pp:stdin (%ph-process handle))))
    (unless stdin
      (error 'unit-not-running :supervisor (unit-supervisor unit) :unit unit))
    (handler-case
        (bt2:with-lock-held ((slot-value handle 'lock))
          (write-sequence (%utf8-encode text) stdin)
          (finish-output stdin))
      (stream-error ()
        (error 'unit-not-running :supervisor (unit-supervisor unit) :unit unit
                                 :message "its stdin is closed")))
    t))

(defparameter +pump-join-timeout+ 3
  "Seconds EXECUTOR-RELEASE waits for the pumps to reach EOF after the tree kill.")

(defmethod executor-release ((executor process-executor) unit handle)
  (declare (ignore unit))
  (let ((process (%ph-process handle)))
    (with-slots (lock cv pumps-left pumps released) handle
      (unless (bt2:with-lock-held (lock) (shiftf released t))
        ;; Whatever is left of the tree (grandchildren in the group / job) dies too.
        (ignore-errors (%kill-process-tree process t))
        ;; The pumps see EOF once every holder of the pipes is gone.
        (let ((deadline (+ (%now) +pump-join-timeout+)))
          (bt2:with-lock-held (lock)
            (loop while (and (plusp pumps-left) (< (%now) deadline))
                  do (bt2:condition-wait cv lock :timeout (max 0.01 (- deadline (%now)))))))
        (dolist (s (list (pp:stdin process) (pp:stdout process) (pp:stderr process)))
          (when s (ignore-errors (close s :abort t))))
        (dolist (th pumps)
          (unless (bt2:thread-alive-p th)
            (ignore-errors (bt2:join-thread th)))))))
  nil)

;;; thread-executor

(defclass thread-unit-handle ()
  ((thread :initform nil :accessor %th-thread)
   (lock :initform (bt2:make-lock :name "thread unit"))
   (cv :initform (bt2:make-condition-variable :name "thread unit"))
   (go-p :initform nil)
   (done :initform nil)
   (forced :initform nil)
   (condition :initform nil)
   (result :initform nil)
   (in-body :initform nil)
   (deferred :initform (list nil) :documentation "*DEFERRED-STOP* cell of the thread.")))

(defun %thread-body (handle unit)
  (with-slots (lock cv go-p done forced condition result in-body deferred) handle
    (let* ((sink (unit-output-sink unit))
           (out (and sink (make-sink-output-stream sink :stdout)))
           (err (and sink (make-sink-output-stream sink :stderr)))
           (*standard-output* (or out *standard-output*))
           (*error-output* (or err *error-output*))
           (*current-unit* unit)
           (*current-scope* (unit-owner unit))
           (*deferred-stop* deferred))
      (unwind-protect
           (when (eq :forced
                     (catch handle
                       ;; Wait until the supervisor registered the unit.
                       (bt2:with-lock-held (lock)
                         (loop until go-p do (bt2:condition-wait cv lock :timeout 1)))
                       (setf in-body t)
                       (handler-case (setf result (funcall (unit-function unit) unit))
                         (error (e) (setf condition e)))
                       (setf in-body nil)
                       nil))
             (setf forced t))
        (setf in-body nil)
        ;; Everything the body printed is in the sink before the final state is
        ;; published (the waiter publishes after DONE).
        (dolist (s (list out err))
          (when s (handler-case (finish-output s) (error () nil))))
        (bt2:with-lock-held (lock)
          (setf done t)
          (bt2:condition-broadcast cv))))))

(defmethod executor-launch ((executor thread-executor) unit)
  (unless (functionp (unit-function unit))
    (error 'unit-start-error :supervisor (unit-supervisor unit) :kind (unit-kind unit)
                             :message "the thread executor needs a :function"))
  (let ((handle (make-instance 'thread-unit-handle)))
    (setf (%th-thread handle)
          (bt2:make-thread (lambda () (%thread-body handle unit))
                           :name (format nil "supervisor unit ~A" (unit-id unit))))
    handle))

(defmethod %executor-started ((executor thread-executor) unit handle)
  (declare (ignore unit))
  (bt2:with-lock-held ((slot-value handle 'lock))
    (setf (slot-value handle 'go-p) t)
    (bt2:condition-broadcast (slot-value handle 'cv)))
  nil)

(defmethod executor-stop ((executor thread-executor) unit handle &key force)
  (declare (ignore unit))
  ;; Graceful: cooperative (the supervisor set UNIT-STOP-REQUESTED-P).
  (when force
    (let ((thread (%th-thread handle)))
      (when (and thread (bt2:thread-alive-p thread)
                 (not (slot-value handle 'done)))
        ;; The thread may end between the check and the interrupt.
        (ignore-errors
         (bt2:interrupt-thread
          thread
          (lambda ()
            (when (slot-value handle 'in-body)
              (if *in-critical*
                  (setf (car (slot-value handle 'deferred)) handle)
                  (throw handle :forced)))))))))
  t)

(defmethod executor-wait ((executor thread-executor) unit handle &key timeout)
  (declare (ignore unit))
  (with-slots (lock cv done forced condition result) handle
    (let ((deadline (and timeout (+ (%now) timeout))))
      (bt2:with-lock-held (lock)
        (loop until done
              do (if deadline
                     (let ((left (- deadline (%now))))
                       (when (<= left 0) (return-from executor-wait (values nil)))
                       (bt2:condition-wait cv lock :timeout left))
                     (bt2:condition-wait cv lock))))
      (if forced
          (values t nil nil nil)
          (values t nil condition result)))))

(defmethod executor-alive-p ((executor thread-executor) unit handle)
  (declare (ignore unit))
  (not (slot-value handle 'done)))
