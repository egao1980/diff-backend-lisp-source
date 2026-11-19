(defpackage #:diff-backend-lisp-source
  (:use #:cl)
  (:nicknames #:stack-diff-lisp)
  (:documentation "diff-protocol engine for Common Lisp source (plan milestone R3).
Brief: cl-stack/docs/capabilities/diff-backend-lisp-source.md.")
  (:export
   ;; engine (ENGINE-NAME → :LISP, registered for FIND-DIFF-ENGINE)
   #:lisp-source-diff-engine
   #:lisp-engine-detect-renames
   ;; source units: (:key k :anchor prev-key :text unit-text) plists
   #:source-units
   ;; rendering to a patch that applies to the file
   #:unified-patch
   ;; conditions and restarts
   #:lisp-source-unreadable
   #:lisp-source-unreadable-diagnostics
   #:use-text-diff))
