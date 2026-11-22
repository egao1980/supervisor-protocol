(in-package #:supervisor-protocol/tests)

;;; 0.5.0 channel units (supervisor-protocol#13): START-UNIT :STDIO :CHANNEL hands
;;; stdin and stdout to the caller (UNIT-CHANNEL-STREAMS); only stderr reaches the
;;; unit log. The child is this SBCL: a line-delimited JSON-RPC server that
;;; answers every request line {"id":N,…} with {"jsonrpc":"2.0","id":N,"result":"pong-N"}
;;; and reports on stderr. It reads the id with SEARCH, enough for the requests
;;; the tests send (rpc-backend-stdio's, and hand-written lines).

(defparameter +json-server-form+
  "(progn
     (format *error-output* \"server starting~%\") (finish-output *error-output*)
     (loop for l = (read-line *standard-input* nil) while l
           do (let* ((p (search \"\\\"id\\\"\" l))
                     (d (and p (position-if #'digit-char-p l :start p)))
                     (id (and d (parse-integer l :start d :junk-allowed t))))
                (format *error-output* \"got ~D chars~%\" (length l)) (finish-output *error-output*)
                (format t \"{\\\"jsonrpc\\\":\\\"2.0\\\",\\\"id\\\":~D,\\\"result\\\":\\\"pong-~D\\\"}~%\" id id)
                (finish-output)))
     (format *error-output* \"server done~%\") (finish-output *error-output*))")

(defun json-server-argv () (lisp-argv +json-server-form+))

(defmacro with-channel-unit ((unit sup dir &rest keys) &body body)
  "BODY with UNIT a channel :MCP-SERVER unit running the JSON-RPC child under a
logged supervisor SUP (state directory DIR)."
  `(with-logged-supervisor (,sup ,dir)
     (let ((,unit (start-unit ,sup :mcp-server :command (json-server-argv) :stdio :channel ,@keys)))
       ,@body)))

(defun stderr-texts (unit)
  (texts (remove :stderr (log-read unit) :key #'log-record-stream :test-not #'eq)))

(defun stdout-records (unit)
  (remove :stdout (log-read unit) :key #'log-record-stream :test-not #'eq))

(deftest channel-unit-json-lines-round-trip
  (with-channel-unit (u sup dir)
    (ok (eq :running (unit-state u)))
    (ok (eq :channel (unit-stdio u)))
    (ok (unit-channel-p u))
    (ok (eq :never (unit-restart-policy u)) "channel units run once")
    (multiple-value-bind (in out) (unit-channel-streams u)
      (ok (and (input-stream-p in) (output-stream-p out)))
      (ok (subtypep (stream-element-type in) 'character) "character streams")
      (write-line "{\"jsonrpc\":\"2.0\",\"id\":7,\"method\":\"ping\"}" out)
      (finish-output out)
      (ok (equal "{\"jsonrpc\":\"2.0\",\"id\":7,\"result\":\"pong-7\"}" (read-line in))
          "the parent reads the child's stdout")
      (write-line "{\"jsonrpc\":\"2.0\",\"id\":8,\"method\":\"ping\"}" out)
      (finish-output out)
      (ok (search "pong-8" (read-line in)))
      (ok (wait-until (lambda () (= 3 (length (stderr-texts u)))) :timeout 20)
          "stderr reaches the log while the unit runs")
      ;; The caller closes stdin: the server sees EOF and exits by itself.
      (close out)
      (ok (equal '(:exited 0) (multiple-value-list (wait-unit u :timeout 30))))
      (ok (equal '("server starting" "got 40 chars" "got 40 chars" "server done")
                 (stderr-texts u))
          "every stderr line, in order")
      (ok (null (stdout-records u)) "stdout never reaches the log")
      (ok (not (search "pong" (unit-output u))) "nor the in-memory output")
      (ok (search "server starting" (unit-output u)))
      (ok (wait-until (lambda () (not (open-stream-p in))) :timeout 5)
          "release closes the channel"))))

(deftest channel-unit-through-rpc-backend-stdio
  (with-channel-unit (u sup dir)
    (multiple-value-bind (in out) (unit-channel-streams u)
      (let ((tx (rpc-backend-stdio:make-stdio-rpc-transport :input in :output out)))
        (ok (equal "pong-1" (rpc-protocol:rpc-call "ping" nil :transport tx :id 1 :timeout 30)))
        (ok (equal "pong-2" (rpc-protocol:rpc-call "ping" #(1 2) :transport tx :id 2 :timeout 30)))
        (ok (eq :killed (stop-unit u :grace 2)))
        (ok (signals (rpc-protocol:rpc-call "ping" nil :transport tx :id 3 :timeout 10)
                     'rpc-protocol:rpc-error)
            "a stopped unit: the client sees the peer closed")
        (ignore-errors (rpc-backend-stdio:close-stdio-rpc-transport tx :timeout 2))
        (ok (some (lambda (l) (search "got" l)) (stderr-texts u)))
        (ok (null (stdout-records u)))))))

(deftest channel-unit-utf-8
  ;; Raw octets on stdout: "✓" (E2 9C 93), an invalid byte, "abcd", LF. (SBCL's decoder
  ;; waits for more input when an invalid byte is among the last few available, so
  ;; the invalid byte is not put at the end of the line.)
  (with-logged-supervisor (sup dir)
    (let ((u (start-unit sup :mcp-server :stdio :channel
                         :command (lisp-argv
                                   "(progn (write-sequence (coerce '(226 156 147 32 255 97 98 99 100 10) '(vector (unsigned-byte 8))) sb-sys:*stdout*) (finish-output sb-sys:*stdout*) (write-sequence (coerce '(226 156 147 10) '(vector (unsigned-byte 8))) sb-sys:*stderr*) (finish-output sb-sys:*stderr*) (read-line *standard-input* nil))"))))
      (multiple-value-bind (in out) (unit-channel-streams u)
        (let ((line (read-line in)))
          (ok (char= (code-char #x2713) (char line 0)) "UTF-8 decoded")
          #+sbcl (ok (char= (code-char #xFFFD) (char line 2)) "an invalid byte → U+FFFD"))
        (close out)
        (ok (eq :exited (wait-unit u :timeout 30)))
        (ok (equal (list (string (code-char #x2713))) (stderr-texts u)) "stderr as UTF-8 in the log")))))

(deftest channel-unit-stderr-flood-does-not-block
  ;; 400 KB on stderr before the answer: the pump drains it.
  (with-logged-supervisor (sup dir)
    (let ((u (start-unit sup :mcp-server :stdio :channel
                         :command (lisp-argv
                                   "(progn (dotimes (i 4000) (write-line (make-string 99 :initial-element #\\e) *error-output*)) (finish-output *error-output*) (write-line \"{\\\"ok\\\":true}\") (finish-output) (read-line *standard-input* nil))"))))
      (multiple-value-bind (in out) (unit-channel-streams u)
        (ok (equal "{\"ok\":true}" (read-line in)))
        (close out)
        (ok (eq :exited (wait-unit u :timeout 60)))
        (ok (= 4000 (length (stderr-texts u))))
        (ok (null (stdout-records u)))))))

(deftest channel-unit-refusals
  (with-supervisor (sup)
    (let ((u (start-unit sup :lsp :command (json-server-argv) :stdio :channel))
          (plain (start-unit sup :shell-job :command (sleeper-argv 30))))
      (ok (eq :never (unit-restart-policy u)) ":lsp would default to :on-failure")
      (ok (signals (send-unit-input u "x") 'channel-unit-error))
      (ok (signals (close-unit-input u) 'channel-unit-error))
      (ok (signals (restart-unit u) 'channel-unit-error))
      (ok (signals (unit-channel-streams plain) 'channel-unit-error))
      (ok (eq :log (unit-stdio plain)))
      (ok (eq :running (unit-state u)) "refusals leave the unit alone")
      (ok (signals (start-unit sup :mcp-server :command (json-server-argv) :stdio :channel
                                               :restart :on-failure)
                   'unit-start-error))
      (ok (signals (start-unit sup :mcp-server :function (lambda (u) u) :stdio :channel)
                   'unit-start-error))
      (ok (signals (start-unit sup :mcp-server :command (json-server-argv) :stdio :pty)
                   'unit-start-error))
      (ok (= 2 (length (supervisor-units sup))) "refused starts register nothing")
      (ok (executor-supports-p (make-process-executor) :channel))
      (ok (not (executor-supports-p (make-thread-executor) :channel)))
      (stop-unit plain :grace 1)
      (stop-unit u :grace 1))))

(deftest channel-unit-stop-reaps-and-closes
  (with-channel-unit (u sup dir)
    (let ((pid (unit-pid u)))
      (multiple-value-bind (in out) (unit-channel-streams u)
        (ok (= 1 (length (units-dir-files dir))) "a unit file: the sweep covers it")
        (ok (eq :killed (stop-unit u :grace 2)))
        (ok (all-dead-p (list pid)) "the child is reaped")
        (ok (not (open-stream-p in)))
        (ok (not (open-stream-p out)))
        (ok (null (units-dir-files dir)) "unit file gone after the end")
        (ok (equal '((:state nil :starting) (:state :starting :running) (:state :running :killed))
                   (transitions (events-since sup 0) (unit-id u))))))))

(deftest channel-unit-force-kill
  ;; Graceful stop is a no-op on Windows (always forced); on Unix a TERM-ignoring
  ;; child needs the escalation.
  (with-supervisor (sup)
    (let* ((u (start-unit sup :mcp-server :stdio :channel
                          :command (if (unix-p) (term-ignoring-argv 60) (sleeper-argv 60))))
           (pid (unit-pid u)))
      (ok (eq :killed (handler-bind ((stop-timeout (lambda (c)
                                                     (invoke-restart (find-restart 'force c)))))
                        (stop-unit u :grace 0.5))))
      (ok (all-dead-p (list pid))))))

(deftest channel-unit-tree-kill
  (with-temp-dir (pid-dir)
    (with-supervisor (sup)
      (let ((u (start-unit sup :mcp-server :stdio :channel :command (tree-argv pid-dir 2 60))))
        (let ((pids (wait-pid-files pid-dir 3)))
          (ok (= 3 (length pids)))
          (ok (eq :killed (stop-unit u :grace 2)))
          (ok (all-dead-p pids) "the whole tree dies"))))))

(deftest channel-unit-exit-states
  (with-supervisor (sup)
    (let ((bad (start-unit sup :lsp :stdio :channel :command (exit-argv 3)))
          (good (start-unit sup :mcp-server :stdio :channel :command (exit-argv 0))))
      (ok (eq :failed (wait-unit bad :timeout 30)))
      (ok (= 3 (unit-exit-code bad)))
      (ok (eq :exited (wait-unit good :timeout 30)))
      (ok (= 0 (unit-exit-code good)))
      (sleep 0.3)
      (ok (eq :failed (unit-state bad)) "no policy restart")
      (ok (= 0 (unit-restart-count bad)))
      (let ((events (events-since sup 0)))
        (ok (equal '((:state nil :starting) (:state :starting :running) (:state :running :failed))
                   (transitions events (unit-id bad))))
        (ok (equal '((:state nil :starting) (:state :starting :running) (:state :running :exited))
                   (transitions events (unit-id good))))
        (ok (notany (lambda (e) (eq :restart-scheduled (event-type e))) events))))))
