(defsystem "diff-backend-lisp-source"
  :version "0.1.0"
  :description "diff-protocol engine for Common Lisp source: form-level diff and three-way merge on a lossless CST (comments kept, never evaluates)"
  :author "egao1980"
  :license "MIT"
  :depends-on ((:version "diff-protocol" "0.2.0") "lisp-source-protocol")
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "conditions")
               (:file "engine")
               (:file "units")
               (:file "align")
               (:file "apply")
               (:file "merge")
               (:file "render"))
  :in-order-to ((test-op (test-op "diff-backend-lisp-source/tests"))))

(defsystem "diff-backend-lisp-source/tests"
  :depends-on ("diff-backend-lisp-source" "rove")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "units-test")
               (:file "diff-test")
               (:file "apply-test")
               (:file "merge-test")
               (:file "render-test")
               (:file "laws-test")
               (:file "history-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
