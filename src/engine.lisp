(in-package #:diff-backend-lisp-source)

;;; R3 form engine (brief: cl-stack docs/capabilities/diff-backend-lisp-source.md).
;;; The class and its name. Reading, units and keys are in units.lisp; COMPUTE-DIFF
;;; and STATE-EQUAL in align.lisp; the diff-protocol engine hooks %APPLY-CHANGE /
;;; %PUT-VALUE in apply.lisp, %THREE-WAY / %REBASE in merge.lisp; UNIFIED-PATCH
;;; and the registration under :LISP in render.lisp.

(defclass lisp-source-diff-engine (diff-protocol:diff-engine)
  ((detect-renames :initarg :detect-renames :initform t
                   :reader lisp-engine-detect-renames))
  (:documentation "Form-level engine for Common Lisp source strings. ENGINE-NAME → :LISP.
DETECT-RENAMES NIL turns rename detection (a removed and an added definition that
differ only in the name → one :MOVE) off."))

(defmethod diff-protocol:engine-name ((e lisp-source-diff-engine))
  :lisp)
