(in-package #:diff-backend-lisp-source)

;;; R3 form engine (brief: cl-stack docs/capabilities/diff-backend-lisp-source.md).
;;; CONTRACT SKELETON: the class and stubs only. The implementation (reading
;;; through lisp-source-protocol with the empty feature list, units and keys,
;;; COMPUTE-DIFF at the three levels, STATE-EQUAL, the diff-protocol engine
;;; hooks %APPLY-CHANGE / %PUT-VALUE / %THREE-WAY / %REBASE, registration under
;;; :LISP, SOURCE-UNITS, UNIFIED-PATCH) is team-solve's job.

(defun %not-implemented (what)
  (error "diff-backend-lisp-source: ~A is not implemented yet" what))

(defclass lisp-source-diff-engine (diff-protocol:diff-engine)
  ((detect-renames :initarg :detect-renames :initform t
                   :reader lisp-engine-detect-renames))
  (:documentation "Form-level engine for Common Lisp source strings. ENGINE-NAME → :LISP.
DETECT-RENAMES NIL turns rename detection (a removed and an added definition that
differ only in the name → one :MOVE) off."))

(defmethod diff-protocol:engine-name ((e lisp-source-diff-engine))
  :lisp)

(defmethod diff-protocol:compute-diff ((e lisp-source-diff-engine) a b &key level context)
  (declare (ignore a b level context))
  (%not-implemented 'compute-diff))

(defmethod diff-protocol:state-equal ((e lisp-source-diff-engine) a b)
  (declare (ignore a b))
  (%not-implemented 'state-equal))

(defmethod diff-protocol:three-way-diff ((e lisp-source-diff-engine) base ours theirs)
  (declare (ignore base ours theirs))
  (%not-implemented 'three-way-diff))

(defun source-units (text)
  "→ fresh list of the units of Lisp source TEXT, in order, as plists
(:KEY key :ANCHOR previous-unit-key-or-NIL :TEXT unit-text). The unit texts
concatenate to TEXT. Signals LISP-SOURCE-UNREADABLE when TEXT does not read clean,
UNSUPPORTED-CONTENT when it is not a string."
  (declare (ignore text))
  (%not-implemented 'source-units))

(defun unified-patch (base diff &key path (context 3))
  "→ string: the unified diff from BASE to (APPLY-DIFF BASE DIFF), CONTEXT lines of
context. Without PATH, bare @@ hunks (diff-protocol's :unified text render); with
PATH, preceded by \"--- a/PATH\" and \"+++ b/PATH\" lines. \"\" when nothing changes."
  (declare (ignore base diff path context))
  (%not-implemented 'unified-patch))
