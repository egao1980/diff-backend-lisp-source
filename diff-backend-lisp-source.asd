(defsystem "diff-backend-lisp-source"
  :version "0.0.1"
  :description "diff-protocol engine for Common Lisp source: form-level diff and three-way merge on a lossless CST (comments kept, never evaluates)"
  :author "egao1980"
  :license "MIT"
  :depends-on ((:version "diff-protocol" "0.2.0") "lisp-source-protocol")
  :serial t
  :pathname "src"
  :components ((:file "package")))
