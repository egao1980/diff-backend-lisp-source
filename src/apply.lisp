(in-package #:diff-backend-lisp-source)

;;; Apply (brief: "Apply"). diff-protocol's APPLY-DIFF calls these hooks inside
;;; its per-change RESTART-CASE (SKIP-CHANGE, USE-VALUE, ABORT), so every
;;; condition signalled here, LISP-SOURCE-UNREADABLE of an unclean state
;;; included, gets those restarts. Each change re-reads the current state and
;;; finds its unit by key.

(defun %fail (reason actual c d)
  (error 'diff-protocol:apply-conflict :reason reason :actual actual :change c :diff d))

(defun %change-target (c d)
  "→ (values key indices) of change C's path (:FORM KEY . INDICES)."
  (let ((path (diff-protocol:change-path c)))
    (unless (and (consp path) (eq (first path) :form)
                 (consp (rest path)) (stringp (second path))
                 (every (lambda (i) (typep i '(integer 0))) (cddr path)))
      (%fail :missing nil c d))
    (values (second path) (cddr path))))

(defun %join-units (units)
  (with-output-to-string (s)
    (dolist (u units) (write-string (%u-text u) s))))

(defun %find-unit (units key)
  (and key (find key units :key #'%u-key :test #'string=)))

(defun %insert-after (units anchor-key unit)
  "UNITS with UNIT inserted right after the unit keyed ANCHOR-KEY (NIL → at the start)."
  (if (null anchor-key)
      (cons unit units)
      (loop for (u . rest) on units
            collect u into head
            when (string= (%u-key u) anchor-key)
              return (append head (list unit) rest)
            finally (return (append units (list unit))))))

(defun %side-text (plist)
  (getf plist :text))

(defun %reach (side key indices)
  "The node at INDICES (NODE-FORMS steps) inside the unit KEY of SIDE, or NIL."
  (let* ((u (%side-unit side key))
         (node (and u (%u-head u))))
    (when (and node (lisp-source-protocol:expression-node-p node))
      (dolist (i indices node)
        (let ((forms (lisp-source-protocol:node-forms node)))
          (unless (< i (length forms))
            (return nil))
          (setf node (nth i forms)))))))

(defun %splice (s node text)
  (concatenate 'string
               (subseq s 0 (lisp-source-protocol:node-start node))
               text
               (subseq s (lisp-source-protocol:node-end node))))

(defun %check-value-text (e value c d)
  (unless (stringp value)
    (error 'diff-protocol:unsupported-content :engine e :content value :change c :diff d)))

(defun %apply-form (side c d)
  (let* ((units (coerce (%s-units side) 'list))
         (key (%change-target c d))
         (kind (diff-protocol:change-kind c)))
    (flet ((need (k)
             (let ((u (%find-unit units k)))
               (unless u (%fail :missing nil c d))
               (unless (string= (%u-text u) (%side-text (diff-protocol:change-before c)))
                 (%fail :before-mismatch (%u-text u) c d))
               u))
           (need-anchor (us anchor)
             (when (and anchor (not (%find-unit us anchor)))
               (%fail :missing nil c d)))
           (new-unit (k)
             (%make-unit :key k :text (%side-text (diff-protocol:change-after c)))))
      (%join-units
       (ecase kind
         (:modify
          (let ((u (need key)))
            (substitute (new-unit key) u units :test #'eq)))
         (:remove
          (remove (need key) units :test #'eq))
         (:add
          (let ((there (%find-unit units key))
                (anchor (getf (diff-protocol:change-after c) :anchor)))
            (when there (%fail :exists (%u-text there) c d))
            (need-anchor units anchor)
            (%insert-after units anchor (new-unit key))))
         (:move
          (let* ((from (second (diff-protocol:change-from c)))
                 (u (need from))
                 (rest (remove u units :test #'eq))
                 (anchor (getf (diff-protocol:change-after c) :anchor)))
            (unless (string= from key)
              (let ((there (%find-unit rest key)))
                (when there (%fail :exists (%u-text there) c d))))
            (need-anchor rest anchor)
            (%insert-after rest anchor (new-unit key)))))))))

(defmethod diff-protocol::%apply-change ((e lisp-source-diff-engine) state
                                        (c diff-protocol:opaque-change) d)
  (declare (ignore state))
  (error 'diff-protocol:unsupported-content :engine e :content c :change c :diff d))

(defmethod diff-protocol::%apply-change ((e lisp-source-diff-engine) state
                                        (c diff-protocol:change) d)
  (let ((side (%read-side e state)))
    (multiple-value-bind (key indices) (%change-target c d)
      (if (null indices)
          (%apply-form side c d)
          (let ((node (%reach side key indices)))
            (unless node (%fail :missing nil c d))
            (let ((found (lisp-source-protocol:node-text node)))
              (unless (string= found (diff-protocol:change-before c))
                (%fail :before-mismatch found c d)))
            (%splice (%s-string side) node (diff-protocol:change-after c)))))))

(defmethod diff-protocol::%put-value ((e lisp-source-diff-engine) state
                                     (c diff-protocol:change) value d)
  (let ((side (%read-side e state)))
    (%check-value-text e value c d)
    (multiple-value-bind (key indices) (%change-target c d)
      (if indices
          (let ((node (%reach side key indices)))
            (unless node (%fail :missing nil c d))
            (%splice (%s-string side) node value))
          (let* ((units (coerce (%s-units side) 'list))
                 (unit (%make-unit :key key :text value))
                 (there (%find-unit units key)))
            (%join-units
             (if there
                 (substitute unit there units :test #'eq)
                 (let ((after (diff-protocol:change-after c))
                       (before (diff-protocol:change-before c)))
                   (flet ((placeable-p (plist)
                            (and plist
                                 (let ((a (getf plist :anchor)))
                                   (or (null a) (%find-unit units a))))))
                     (cond ((placeable-p after)
                            (%insert-after units (getf after :anchor) unit))
                           ((placeable-p before)
                            (%insert-after units (getf before :anchor) unit))
                           (t
                            (let ((trailer (position-if #'%u-trailer-p units)))
                              (if trailer
                                  (append (subseq units 0 trailer) (list unit)
                                          (nthcdr trailer units))
                                  (append units (list unit)))))))))))))))
