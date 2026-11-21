(in-package #:supervisor-protocol)

;;; H8b contract stub. The implementation replaces every call and deletes it.
(defun %h8b-pending (what)
  (error "supervisor-protocol H8b: ~A is not implemented yet" what))

;;; ------------------------------------------------------------------
;;; Constants

(defparameter +unit-states+ '(:starting :running :exited :killed :failed)
  ":STARTING launch in progress (also a restart's relaunch); :RUNNING launched;
:EXITED ended by itself with success (exit code 0, or a thread returned);
:FAILED ended by itself otherwise (non-zero exit, thread error, relaunch failed);
:KILLED ended after a stop request (STOP-UNIT, CLOSE-SCOPE, eviction, shutdown).")

(defparameter +live-states+ '(:starting :running))
(defparameter +final-states+ '(:exited :killed :failed))

(defparameter +event-types+ '(:state :restart-scheduled :evicted :log-error)
  ":STATE a state change (FROM → TO; FROM is NIL for the first event of a unit);
:RESTART-SCHEDULED a :FAILED unit will relaunch (detail :ATTEMPT :DELAY);
:EVICTED the unit left the registry (FROM its final state, TO NIL);
:LOG-ERROR (H8b) writing the unit's log failed (FROM and TO both the unit's state
at that moment; detail :ACTION :MEMORY-ONLY | :RETRY | :ABORT-UNIT, :PATH the
native namestring, :MESSAGE the cause's report).")

(defparameter +scope-kinds+ '(:supervisor :session :agent :tool-source)
  "Known scope kinds. :SUPERVISOR is the root. Any other keyword is accepted.")

(defconstant +unit-file-format+ 1)
(defconstant +default-grace+ 5 "Seconds between a graceful stop and STOP-TIMEOUT.")
(defconstant +default-force-grace+ 2 "Seconds after a forced stop before STOP-FAILED.")
(defparameter +default-backoff+ '(0.5 2 30) "(initial factor max) seconds; see RESTART-DELAY.")
(defconstant +default-max-restarts+ 3)
(defconstant +default-event-capacity+ 4096 "Events retained for replay.")
(defconstant +default-head-size+ 16384 "Characters kept from the start of a unit's output.")
(defconstant +default-tail-size+ 65536 "Characters kept from the end of a unit's output.")
(defparameter +default-sweep-grace+ 0.5 "Seconds between TERM and KILL in the orphan sweep.")

;;; H8b: rotated, capped unit logs (src/log-format.lisp, log-sink.lisp, log-reader.lisp).
(defconstant +log-format+ 1 "Version of the segment record format and of the index layout.")
(defconstant +default-log-segment-size+ 1048576 "Maximum bytes of one segment file.")
(defconstant +default-log-segments+ 5 "Segments retained per unit; older ones are dropped.")
(defconstant +default-log-total-cap+ 268435456
  "Bytes (segment + index files) of all unit logs of one supervisor.")
(defconstant +default-log-max-record+ 8192
  "Maximum output octets in one record; longer lines are split into continued records.")
(defconstant +log-index-stride+ 64 "One index entry per this many records.")
(defparameter +log-record-types+ '(:output :header :drop :end)
  ":OUTPUT unit output (stream :STDOUT / :STDERR); :HEADER the first record of every
segment; :DROP the marker a dropped segment leaves; :END the log was closed.")
(defparameter +log-gap-reasons+ '(:rotated :total-cap :corrupt :missing)
  ":ROTATED the unit's segment count was exceeded; :TOTAL-CAP the supervisor's total
cap evicted it; :CORRUPT bytes that are not valid records; :MISSING lines absent
with no marker (a segment deleted from outside).")

(defvar *current-scope* nil
  "The default owner of new units and parent of new scopes: bound by WITH-SCOPE and
inside a thread unit (to the unit's owner).")

(defvar *current-unit* nil
  "Bound to the unit inside a thread unit's function.")

;;; ------------------------------------------------------------------
;;; Internal: clock and the supervisor lock

(declaim (inline %now))
(defun %now ()
  "Seconds on the image's monotonic clock (a double)."
  (/ (float (get-internal-real-time) 1d0) internal-time-units-per-second))

(defvar *in-critical* nil
  "True while this thread holds a supervisor lock: a forced stop of a thread unit
is deferred until the lock is released (see %RUN-DEFERRED-STOP).")

(defvar *deferred-stop* nil
  "Inside a thread unit: a cons whose car is the catch tag a deferred forced stop
throws to, or NIL.")

(defun %run-deferred-stop ()
  (when (and (not *in-critical*) *deferred-stop* (car *deferred-stop*))
    (let ((tag (car *deferred-stop*)))
      (setf (car *deferred-stop*) nil)
      (throw tag :forced))))

(defmacro %with-lock ((supervisor) &body body)
  "BODY with SUPERVISOR's lock held. Never call user code inside."
  `(unwind-protect
        (let ((*in-critical* t))
          (bt2:with-lock-held ((slot-value ,supervisor 'lock)) ,@body))
     (%run-deferred-stop)))

(defun %cv-wait (supervisor timeout)
  "Wait on SUPERVISOR's state cv (lock held), at most TIMEOUT seconds (NIL: no limit).
A forced stop of this thread may unwind from here."
  (let ((*in-critical* nil))
    (%run-deferred-stop)
    (bt2:condition-wait (slot-value supervisor 'cv) (slot-value supervisor 'lock)
                        :timeout (and timeout (max timeout 0.001)))))

(defun %broadcast (supervisor)
  (bt2:condition-broadcast (slot-value supervisor 'cv)))

;;; ------------------------------------------------------------------
;;; Restart policy

(defgeneric kind-default-restart (kind)
  (:documentation "The restart policy a unit of KIND gets when START-UNIT has no :RESTART:
:ON-FAILURE for servers (:RPC :LSP), :NEVER for everything else.")
  (:method ((kind t)) :never)
  (:method ((kind (eql :rpc))) :on-failure)
  (:method ((kind (eql :lsp))) :on-failure))

(defun restart-delay (backoff attempt)
  "Seconds before restart ATTEMPT (1, 2, …) under BACKOFF = (initial factor max):
min(max, initial × factor^(attempt − 1))."
  (destructuring-bind (initial factor max) backoff
    (min max (* initial (expt factor (1- attempt))))))

;;; ------------------------------------------------------------------
;;; Supervisor

(defclass supervisor ()
  ((id :initarg :id :reader supervisor-id
       :documentation "\"<pid>-<8 lowercase hex>\", unique per supervisor.")
   (state-directory :initarg :state-directory :initform nil :reader supervisor-state-directory
                    :documentation "Directory pathname holding units/, or NIL (no unit files, no sweep).")
   (root-scope :initform nil :reader supervisor-root-scope)
   (kind-caps :initarg :kind-caps :initform nil :reader supervisor-kind-caps
              :documentation "Plist kind → maximum units of that kind in the registry.")
   (owner-cap :initarg :owner-cap :initform nil :reader supervisor-owner-cap
              :documentation "Maximum units owned directly by one scope, or NIL.")
   (grace :initarg :grace :initform +default-grace+ :reader supervisor-grace)
   (force-grace :initarg :force-grace :initform +default-force-grace+ :reader supervisor-force-grace)
   (event-capacity :initarg :event-capacity :initform +default-event-capacity+
                   :reader supervisor-event-capacity)
   (output-sink-factory :initarg :output-sink-factory :initform 'default-output-sink
                        :reader supervisor-output-sink-factory
                        :documentation "Function of a unit → its OUTPUT-SINK. H8b plugs rotated logs in here.")
   ;; H8b
   (log-p :initarg :log :initform t :reader supervisor-log-p
          :documentation "With a state directory: T → units log to files by default.")
   (log-segment-size :initarg :log-segment-size :initform +default-log-segment-size+
                     :reader supervisor-log-segment-size)
   (log-segments :initarg :log-segments :initform +default-log-segments+
                 :reader supervisor-log-segments)
   (log-total-cap :initarg :log-total-cap :initform +default-log-total-cap+
                  :reader supervisor-log-total-cap)
   (log-max-record :initarg :log-max-record :initform +default-log-max-record+
                   :reader supervisor-log-max-record)
   (log-error-handler :initarg :log-error-handler :initform nil
                      :reader supervisor-log-error-handler
                      :documentation "Function of a LOG-WRITE-ERROR, called when the dynamic handlers declined; may invoke a restart.")
   (sweep-report :initform nil :reader supervisor-sweep-report
                 :documentation "SWEEP-RECORDs of the sweep MAKE-SUPERVISOR ran.")
   (closed-p :initform nil :reader supervisor-closed-p)
   (exit-hook :initform nil :reader supervisor-exit-hook
              :documentation "The function registered as image exit hook, or NIL.")
   (process-executor :initarg :process-executor :initform nil)
   (thread-executor :initarg :thread-executor :initform nil)
   (lock :initform (bt2:make-lock :name "supervisor"))
   ;; internal state, all guarded by LOCK
   (cv :initform (bt2:make-condition-variable :name "supervisor state"))
   (units :initform '() :documentation "Registry, newest first.")
   (by-id :initform (make-hash-table :test 'equal))
   (kind-counters :initform (make-hash-table :test 'eq))
   (tick :initform 0 :documentation "LRU clock, strictly increasing.")
   (seq :initform 0 :documentation "Seq of the newest event.")
   (event-log :initform nil :documentation "Ring of the retained events.")
   (queue-head :initform '() :documentation "Dispatcher FIFO (events and replays).")
   (queue-tail :initform '())
   (subscriptions :initform '())
   (dispatcher :initform nil)
   (dispatch-closing :initform nil)
   (dispatch-done :initform nil)
   (os-start :initform nil :documentation "This image's OS start string (lazy).")))

;;; ------------------------------------------------------------------
;;; Scopes

(defclass scope ()
  ((supervisor :initarg :supervisor :reader scope-supervisor)
   (kind :initarg :kind :reader scope-kind)
   (name :initarg :name :reader scope-name)
   (parent :initarg :parent :initform nil :reader scope-parent)
   (children :initform nil :reader scope-children :documentation "Oldest first.")
   (closed-p :initform nil :reader scope-closed-p)))

(defun scope-path (scope)
  "Names from the root's child down to SCOPE; the root itself → ()."
  (loop for s = scope then (scope-parent s)
        while (and s (scope-parent s))
        collect (scope-name s) into names
        finally (return (nreverse names))))

(defmethod print-object ((scope scope) stream)
  (print-unreadable-object (scope stream :type t)
    (format stream "~(~A~) ~{~A~^ › ~}~:[~; closed~]"
            (scope-kind scope) (scope-path scope) (scope-closed-p scope))))

;;; ------------------------------------------------------------------
;;; Units

(defclass unit ()
  ((id :initarg :id :reader unit-id :documentation "\"<kind>-<n>\", n counted per kind per supervisor.")
   (kind :initarg :kind :reader unit-kind)
   (owner :initarg :owner :reader unit-owner)
   (label-plist :initarg :labels :initform nil :reader unit-labels :documentation "Plist keyword → value.")
   (command :initarg :command :initform nil :reader unit-command :documentation "argv list, or NIL.")
   (body :initarg :function :initform nil :reader unit-function :documentation "Thread body, or NIL.")
   (cwd :initarg :cwd :initform nil :reader unit-cwd)
   (env :initarg :env :initform nil :reader unit-env)
   (pid :initform nil :reader unit-pid)
   (pgid :initform nil :reader unit-pgid)
   (start-time :initform nil :reader unit-start-time :documentation "Universal time of the last launch.")
   (os-start-time :initform nil :reader unit-os-start-time
                  :documentation "The OS start time string of PID (pid-reuse guard), or NIL.")
   (state :initform :starting :reader unit-state)
   (exit-code :initform nil :reader unit-exit-code)
   (stop-signal :initform nil :reader unit-signal
           :documentation ":TERM or :KILL when the supervisor stopped the unit, else NIL.")
   (end-condition :initform nil :reader unit-condition :documentation "The error that ended a thread unit.")
   (result :initform nil :reader unit-result :documentation "A thread unit's return value.")
   (restart-policy :initarg :restart-policy :reader unit-restart-policy)
   (max-restarts :initarg :max-restarts :reader unit-max-restarts)
   (backoff :initarg :backoff :reader unit-backoff)
   (restart-count :initform 0 :reader unit-restart-count)
   (restart-pending :initform nil)
   (last-used :initform 0 :reader unit-last-used :documentation "Supervisor tick of the last use (LRU).")
   (interacting-p :initarg :interacting :initform nil :accessor unit-interacting-p
                  :documentation "True while someone interacts with the unit: never evicted.")
   (stop-requested-p :initform nil :reader unit-stop-requested-p)
   (executor :initarg :executor :reader unit-executor)
   (handle :initform nil)
   (supervisor :initarg :supervisor :reader unit-supervisor)
   (output-sink :initform nil :reader unit-output-sink)
   (log-settings :initarg :log-settings :initform nil :reader unit-log-settings
                 :documentation "H8b, effective: (:ENABLED bool :SEGMENT-SIZE n :SEGMENTS k).")
   (file-path :initform nil :reader unit-file-path
              :documentation "Its unit file while one exists, else NIL.")
   ;; internal
   (gen :initform 0 :documentation "Launch generation: a waiter publishes only for the current one.")
   (abandoned-p :initform nil)))

(defun unit-live-p (unit)
  (and (member (unit-state unit) +live-states+) t))

(defun unit-finished-p (unit)
  "Final state and no restart pending."
  (and (member (unit-state unit) +final-states+)
       (not (slot-value unit 'restart-pending))))

(defmethod print-object ((unit unit) stream)
  (print-unreadable-object (unit stream :type t)
    (format stream "~A ~S" (unit-id unit) (unit-state unit))))

;;; ------------------------------------------------------------------
;;; Events

(defclass unit-event ()
  ((seq :initarg :seq :reader event-seq :documentation "1, 2, … per supervisor, no gaps.")
   (universal-time :initarg :time :reader event-time :documentation "Universal time.")
   (monotonic :initarg :monotonic :reader event-monotonic
              :documentation "Seconds (real) on the image's monotonic clock.")
   (unit-id :initarg :unit-id :reader event-unit-id)
   (unit-kind :initarg :unit-kind :reader event-unit-kind)
   (event-type :initarg :type :reader event-type)
   (from :initarg :from :initform nil :reader event-from)
   (to :initarg :to :initform nil :reader event-to)
   (detail :initarg :detail :initform nil :reader event-detail)))

(defmethod print-object ((event unit-event) stream)
  (print-unreadable-object (event stream :type t)
    (format stream "~D ~A ~(~A~) ~S → ~S~@[ ~S~]" (event-seq event) (event-unit-id event)
            (event-type event) (event-from event) (event-to event) (event-detail event))))
