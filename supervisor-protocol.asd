(defsystem "supervisor-protocol"
  :version "0.2.0"
  :description "Process supervisor for the cl-stack harness: units, scopes, caps, restarts, events, orphan sweep, rotated capped unit logs"
  :author "egao1980"
  :license "MIT"
  :depends-on ((:version "process-protocol" "0.4.1")
               (:version "process-backend-uiop" "0.3.1")
               (:version "cl-stack-pathlib" "0.9.0")
               (:version "encoding-protocol" "0.2.0")
               "trivial-gray-streams"
               "bordeaux-threads"
               "uiop")
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "conditions")
               (:file "model")          ; constants, scopes, units, events, the supervisor class
               (:file "output")         ; output sinks: head + tail buffer, octet writes
               (:file "log-format")     ; H8b: segment record lines, CRC-32, escapes / base64
               (:file "log-sink")       ; H8b: rotated capped segment files + index, thread-unit streams
               (:file "log-reader")     ; H8b: read / tail / follow / segments across rotations
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
               (:file "regression-test")
               (:file "log-support")
               (:file "log-format-test")
               (:file "log-sink-test")
               (:file "log-reader-test")
               (:file "log-supervisor-test")
               (:file "log-error-test")
               (:file "log-crash-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
