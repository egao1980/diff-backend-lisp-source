(in-package #:diff-backend-lisp-source)

;;; Conditions (brief: "Conditions and restarts"). The diff-protocol tree is
;;; reused: APPLY-CONFLICT and UNSUPPORTED-CONTENT come from diff-protocol and
;;; keep their restarts (APPLY-DIFF: SKIP-CHANGE, USE-VALUE, ABORT).

(define-condition lisp-source-unreadable (diff-protocol:unsupported-content)
  ((diagnostics :initarg :diagnostics :initform nil
                :reader lisp-source-unreadable-diagnostics
                :documentation "lisp-source-protocol DIAGNOSTIC (:error severity) and
IMBALANCE objects of the side that does not read clean."))
  (:report (lambda (c s)
             (format s "Lisp source does not read cleanly (~D problem~:P); the form engine needs a clean read"
                     (length (lisp-source-unreadable-diagnostics c)))))
  (:documentation "A side has reader errors or paren imbalances (lisp-source-protocol
PARSED-SOURCE-CLEAN-P is false). COMPUTE-DIFF and THREE-WAY-DIFF establish
USE-TEXT-DIFF (fall back to the :text engine) and USE-VALUE (return a given result)."))

(defun use-text-diff (&optional condition)
  "Invoke the USE-TEXT-DIFF restart (of CONDITION, if given). Like CL:CONTINUE:
returns NIL when there is none."
  (let ((restart (find-restart 'use-text-diff condition)))
    (when restart
      (invoke-restart restart))))
