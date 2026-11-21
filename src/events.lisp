(in-package #:supervisor-protocol)

;;; The state-change event stream. Every change is one UNIT-EVENT with the
;;; next sequence number (1, 2, …, no gaps), appended under the supervisor lock
;;; in the order the changes happen; per unit the order is its transition
;;; order. The last EVENT-CAPACITY events are retained for replay.
;;;
;;; Subscribers are called on one dispatcher thread per supervisor, serially,
;;; in sequence order, never with the lock held. SUBSCRIBE :SINCE N first
;;; replays the retained events with seq > N, then continues live: no gap, no
;;; duplicate. A callback that signals an ERROR is unsubscribed; the condition
;;; stays in SUBSCRIPTION-ERROR and the other subscribers carry on.

(defclass subscription ()
  ((supervisor :initarg :supervisor :reader subscription-supervisor)
   (callback :initarg :function :reader subscription-function)
   (active-p :initform t :reader subscription-active-p)
   (failure :initform nil :reader subscription-error)
   (live-from :initform 1 :documentation "Live events with a smaller seq came through its replay.")))

;;; Internal (lock held unless noted)

(defun %ensure-event-log (sup)
  (or (slot-value sup 'event-log)
      (setf (slot-value sup 'event-log)
            (make-array (max 1 (supervisor-event-capacity sup)) :initial-element nil))))

(defun %enqueue (sup item)
  (let ((cell (list item)))
    (if (slot-value sup 'queue-head)
        (setf (cdr (slot-value sup 'queue-tail)) cell)
        (setf (slot-value sup 'queue-head) cell))
    (setf (slot-value sup 'queue-tail) cell)))

(defun %publish (sup unit type from to &optional detail)
  "Append the next event (lock held) and hand it to the dispatcher. → the event."
  (let* ((seq (incf (slot-value sup 'seq)))
         (event (make-instance 'unit-event :seq seq :time (get-universal-time)
                                           :monotonic (%now)
                                           :unit-id (unit-id unit) :unit-kind (unit-kind unit)
                                           :type type :from from :to to :detail detail))
         (capacity (supervisor-event-capacity sup)))
    (when (plusp capacity)
      (let ((log (%ensure-event-log sup)))
        (setf (svref log (mod (1- seq) (length log))) event)))
    (%enqueue sup event)
    (%broadcast sup)
    event))

(defun %retained-since (sup since)
  "→ (values events complete-p), lock held."
  (let* ((last (slot-value sup 'seq))
         (capacity (supervisor-event-capacity sup))
         (first (if (plusp capacity) (max 1 (- last capacity -1)) (1+ last)))
         (from (max (1+ since) first)))
    (values (if (plusp capacity)
                (let ((log (%ensure-event-log sup)))
                  (loop for s from from to last
                        collect (svref log (mod (1- s) (length log)))))
                '())
            (or (>= (1+ since) first) (>= since last)))))

(defun %deliver (sup sub event)
  "Call SUB's callback with EVENT (lock NOT held); an ERROR drops the subscription."
  (when (subscription-active-p sub)
    (handler-case (funcall (subscription-function sub) event)
      (error (e)
        (%with-lock (sup)
          (setf (slot-value sub 'failure) e
                (slot-value sub 'active-p) nil
                (slot-value sup 'subscriptions)
                (remove sub (slot-value sup 'subscriptions))))))))

(defun %dispatch-loop (sup)
  (loop
    (let ((item nil) (targets '()))
      (%with-lock (sup)
        (loop until (or (slot-value sup 'queue-head) (slot-value sup 'dispatch-closing))
              do (%cv-wait sup nil))
        (cond ((slot-value sup 'queue-head)
               (setf item (pop (slot-value sup 'queue-head)))
               (unless (slot-value sup 'queue-head) (setf (slot-value sup 'queue-tail) nil))
               (if (consp item)
                   (setf targets (list (cadr item)))
                   (setf targets (remove-if-not
                                  (lambda (sub) (<= (slot-value sub 'live-from) (event-seq item)))
                                  (slot-value sup 'subscriptions)))))
              (t
               (setf (slot-value sup 'dispatch-done) t)
               (%broadcast sup)
               (return))))
      (if (consp item)
          (dolist (e (cddr item)) (%deliver sup (first targets) e))
          (dolist (sub targets) (%deliver sup sub item))))))

(defun %start-dispatcher (sup)
  (setf (slot-value sup 'dispatcher)
        (bt2:make-thread (lambda () (%dispatch-loop sup))
                         :name (format nil "supervisor ~A events" (supervisor-id sup)))))

(defun %stop-dispatcher (sup &key (timeout 5))
  "Deliver what is queued, end the dispatcher (bounded wait), deactivate every subscription."
  (let ((self (eq (bt2:current-thread) (slot-value sup 'dispatcher))))
    (%with-lock (sup)
      (setf (slot-value sup 'dispatch-closing) t)
      (%broadcast sup)
      (unless self
        (let ((deadline (+ (%now) timeout)))
          (loop until (or (slot-value sup 'dispatch-done) (>= (%now) deadline))
                do (%cv-wait sup (- deadline (%now))))))
      (dolist (sub (slot-value sup 'subscriptions))
        (setf (slot-value sub 'active-p) nil))
      (setf (slot-value sup 'subscriptions) '()))))

;;; API

(defun supervisor-last-seq (supervisor)
  "The seq of the newest event; 0 before any."
  (%with-lock (supervisor) (slot-value supervisor 'seq)))

(defun events-since (supervisor seq)
  "Retained events with seq > SEQ, oldest first. → (values events complete-p):
COMPLETE-P is NIL when events after SEQ were already dropped (capacity)."
  (check-type seq (integer 0))
  (%with-lock (supervisor) (%retained-since supervisor seq)))

(defun subscribe (supervisor function &key since)
  "Call FUNCTION with each new event; with SINCE (a seq), first with the retained
events after it. → SUBSCRIPTION."
  (check-type since (or null (integer 0)))
  (let ((sub (make-instance 'subscription :supervisor supervisor :function function)))
    (%with-lock (supervisor)
      (let ((last (slot-value supervisor 'seq)))
        (setf (slot-value sub 'live-from) (1+ last))
        (if (slot-value supervisor 'dispatch-closing)
            (setf (slot-value sub 'active-p) nil)
            (progn
              (push sub (slot-value supervisor 'subscriptions))
              (when (and since (< since last))
                (%enqueue supervisor (list* :replay sub (%retained-since supervisor since)))
                (%broadcast supervisor))))))
    sub))

(defun unsubscribe (subscription)
  "No further calls after this returns (a call in progress may finish). Idempotent. → NIL."
  (let ((sup (subscription-supervisor subscription)))
    (%with-lock (sup)
      (setf (slot-value subscription 'active-p) nil
            (slot-value sup 'subscriptions) (remove subscription (slot-value sup 'subscriptions)))))
  nil)
