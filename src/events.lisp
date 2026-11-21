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
   (failure :initform nil :reader subscription-error)))

(defun supervisor-last-seq (supervisor)
  "The seq of the newest event; 0 before any."
  (declare (ignore supervisor))
  (%h8a-pending "supervisor-last-seq"))

(defun events-since (supervisor seq)
  "Retained events with seq > SEQ, oldest first. → (values events complete-p):
COMPLETE-P is NIL when events after SEQ were already dropped (capacity)."
  (declare (ignore supervisor seq))
  (%h8a-pending "events-since"))

(defun subscribe (supervisor function &key since)
  "Call FUNCTION with each new event; with SINCE (a seq), first with the retained
events after it. → SUBSCRIPTION."
  (declare (ignore supervisor function since))
  (%h8a-pending "subscribe"))

(defun unsubscribe (subscription)
  "No further calls after this returns (a call in progress may finish). Idempotent. → NIL."
  (declare (ignore subscription))
  (%h8a-pending "unsubscribe"))
