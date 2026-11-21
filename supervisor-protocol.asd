(defsystem "supervisor-protocol"
  :version "0.1.0"
  :description "Process supervisor core for the cl-stack harness: units, scopes, caps, restarts, events, orphan sweep"
  :author "egao1980"
  :license "MIT"
  :depends-on ((:version "process-protocol" "0.4.1")
               (:version "process-backend-uiop" "0.3.1")
               "bordeaux-threads"
               "uiop")
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "conditions")
               (:file "model")          ; constants, scopes, units, events, the supervisor class
               (:file "output")         ; output sinks: head + tail buffer (H8b attaches files here)
               (:file "os")             ; current pid, OS process inspectors (ps / CIM), OS kills
               (:file "executor")       ; executor GFs: process-protocol and in-image threads
               (:file "events")         ; sequence log, replay, subscriptions
               (:file "unit-file")      ; durable unit files and the orphan sweep
               (:file "supervisor"))    ; make-supervisor, start/stop/wait, caps, restarts, queries
  :in-order-to ((test-op (test-op "supervisor-protocol/tests"))))

(defsystem "supervisor-protocol/tests"
  :depends-on ("supervisor-protocol" "process-protocol" "bordeaux-threads" "rove")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "support")
               (:file "model-test")
               (:file "output-test")
               (:file "unit-test")
               (:file "query-test")
               (:file "scope-test")
               (:file "restart-test")
               (:file "events-test")
               (:file "caps-test")
               (:file "stop-test")
               (:file "unit-file-test")
               (:file "sweep-test")
               (:file "shutdown-test")
               (:file "regression-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
