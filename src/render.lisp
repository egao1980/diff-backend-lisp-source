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

;;; :LISP files inside a tree diff's :unified render (diff-protocol 0.3.0,
;;; diff-backend-lisp-source#3).

(defun %write-lines (prefix text stream)
  "TEXT's lines, each after PREFIX."
  (let ((start 0) (n (length text)))
    (loop while (< start n)
          do (let ((nl (position #\Newline text :start start)))
               (write-string prefix stream)
               (write-string text stream :start start :end (or nl n))
               (terpri stream)
               (setf start (if nl (1+ nl) n))))))

(defun %change-text (side)
  "Text of a change side: a form-level plist's :TEXT, a leaf's node text, or NIL."
  (cond ((stringp side) side)
        ((and (consp side) (eq (first side) :key)) (getf (cddr side) :text))
        (t nil)))

(defmethod diff-protocol:render-unified-content ((e lisp-source-diff-engine)
                                                  before after changes stream)
  ;; With either side's text: the line hunks of the :text engine, as UNIFIED-PATCH.
  ;; Without (a :CHANGES-level tree diff): one section per change, headed by its
  ;; footprint, with the unit or node texts it replaces; no line numbers, so this
  ;; form is for reading, not for patch (use UNIFIED-PATCH with the base text).
  (cond ((and (or before after)
              (typep before '(or null string))
              (typep after '(or null string)))
         (diff-protocol:render-unified-content (diff-protocol:find-diff-engine :text)
                                               before after nil stream))
        ((and changes (every (lambda (c) (typep c 'diff-protocol:change)) changes))
         (labels ((leaves (cs)
                    (loop for c in cs
                          append (if (diff-protocol:change-children c)
                                     (leaves (diff-protocol:change-children c))
                                     (list c)))))
           (dolist (c (leaves changes) t)
             (format stream "@@ ~{~A~^ ~} @@~%"
                     (or (remove-if-not (lambda (f) (uiop:string-prefix-p "lisp:" f))
                                        (diff-protocol:change-footprint c))
                         (diff-protocol:change-footprint c)))
             (let ((b (%change-text (diff-protocol:change-before c)))
                   (a (%change-text (diff-protocol:change-after c))))
               (when b (%write-lines "-" b stream))
               (when a (%write-lines "+" a stream))))))
        (t nil)))

;;; The shared instance: (find-diff-engine :lisp). Loaded last so every method exists.
(diff-protocol:register-diff-engine :lisp (make-instance 'lisp-source-diff-engine))
