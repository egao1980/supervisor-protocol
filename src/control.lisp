(in-package #:supervisor-protocol)

;;; H8c control additions (0.3.0): a manual restart and closing a unit's stdin.
;;; Both are what the agent tool source (agent-tools unit_control, shell-tools
;;; foreground commands) needs on top of the 0.2.0 surface.

(defun restart-unit (unit &key grace force-grace)
  "Relaunch UNIT now: same unit, same id, a new run. A live unit is stopped first
(as STOP-UNIT with GRACE / FORCE-GRACE: STOP-TIMEOUT and STOP-FAILED behave as
there). A pending policy restart is cancelled; UNIT-RESTART-COUNT is not
incremented (it counts policy restarts only).
Events: :STATE FINAL → :STARTING with detail (:RESTART :MANUAL), then :STARTING →
:RUNNING. The unit file is rewritten. The unit's log continues: the new run's
records follow the previous ones in one line sequence (a new segment with a
header starts after an end record; LOG-CLOSED-P and LOG-FOLLOW's ENDED-P look at
the last end record only).
→ UNIT in :RUNNING. A unit no longer in its supervisor's registry (evicted) →
SUPERVISOR-ERROR; a shut-down supervisor → SUPERVISOR-CLOSED; a launch failure →
UNIT-START-ERROR with the unit left :FAILED (and its :STATE event)."
  (declare (ignore grace force-grace))
  (check-type unit unit)
  (%h8c-pending "restart-unit"))

(defun close-unit-input (unit)
  "Close UNIT's stdin (EXECUTOR-CLOSE-INPUT), so a program reading it sees end of
file. → T when this call closed it; NIL when it was already closed or the unit is
finished. Later SEND-UNIT-INPUT on this run → UNIT-INPUT-CLOSED. A restart opens a
new stdin. Thread units → EXECUTOR-UNSUPPORTED."
  (check-type unit unit)
  (%h8c-pending "close-unit-input"))
