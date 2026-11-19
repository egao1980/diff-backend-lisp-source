(in-package #:diff-backend-lisp-source)

;;; UNIFIED-PATCH (brief: "Render") and the registration under :LISP.

(defun unified-patch (base diff &key path (context 3))
  "→ string: the unified diff from BASE to (APPLY-DIFF BASE DIFF), CONTEXT lines of
context. Without PATH, bare @@ hunks (diff-protocol's :unified text render); with
PATH, preceded by \"--- a/PATH\" and \"+++ b/PATH\" lines. \"\" when nothing changes."
  (check-type context (integer 0))
  (let ((target (values (diff-protocol:apply-diff base diff))))
    (if (string= base target)
        ""
        (let ((body (diff-protocol:render-diff
                     (diff-protocol:compute-diff (diff-protocol:find-diff-engine :text)
                                                 base target
                                                 :level :changes :context context)
                     :unified nil)))
          (if path
              (format nil "--- a/~A~%+++ b/~A~%~A" path path body)
              body)))))

;;; The shared instance: (find-diff-engine :lisp). Loaded last so every method exists.
(diff-protocol::%register-engine :lisp (make-instance 'lisp-source-diff-engine))
