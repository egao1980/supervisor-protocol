(defsystem "supervisor-protocol"
  :version "0.1.0"
  :description "Process supervisor core for the cl-stack harness: units, scopes, caps, restarts, events, orphan sweep"
  :author "egao1980"
  :license "MIT"
  :depends-on ("uiop")
  :serial t
  :pathname "src"
  :components ((:file "package")))
