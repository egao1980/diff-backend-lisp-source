(in-package #:diff-backend-lisp-source)

;;; Alignment of two sides by key (brief: "Changes", "Nested changes", "Levels").
;;; %ALIGN records one %ENTRY per form-level change, already in apply order;
;;; COMPUTE-DIFF projects the entries to diff-protocol changes at a level, and
;;; %THREE-WAY (merge.lisp) classifies the same entries.

(defstruct (%entry (:conc-name %e-) (:constructor %make-entry) (:copier nil))
  "One form-level change. KEY = the path key, FROM = the old key of a :MOVE, BEFORE /
AFTER = %UNITs (or NIL), B-ANCHOR / A-ANCHOR = the before / after anchors, LEAVES =
nested leaves (indices before-text after-text) of a nestable :MODIFY."
  kind key from before after b-anchor a-anchor leaves)

;;; Footprints

(defun %esc (key)
  (with-output-to-string (s)
    (loop for c across key
          do (when (member c '(#\\ #\/)) (write-char #\\ s))
             (write-char c s))))

(defun %fp (key &optional indices)
  (format nil "lisp:/~A~{/~D~}" (%esc key) indices))

;;; Moved keys: Myers (the :text engine) over the common keys as index tokens

(defun %moved-keys (common-a common-b)
  "→ EQUAL hash set of the common keys the :text engine marks removed when diffing
COMMON-A (a's order) against COMMON-B (b's order), one key per line."
  (let ((moved (make-hash-table :test #'equal)))
    (when common-a
      (let ((token (make-hash-table :test #'equal))
            (ka (coerce common-a 'simple-vector)))
        (loop for k across ka for i from 0 do (setf (gethash k token) i))
        (flet ((lines (keys)
                 (format nil "~{~D~%~}" (mapcar (lambda (k) (gethash k token)) keys))))
          (dolist (c (diff-protocol:diff-changes
                      (diff-protocol:compute-diff (diff-protocol:find-diff-engine :text)
                                                  (lines common-a) (lines common-b)
                                                  :level :changes)))
            (dolist (line (diff-protocol:change-before c))
              (setf (gethash (svref ka (parse-integer line :junk-allowed t)) moved) t))))))
    moved))

;;; Nested leaves: same shape

(defun %gaps (node)
  "Texts of NODE not covered by an expression child (delimiters, whitespace, comments)."
  (let* ((text (lisp-source-protocol:node-text node))
         (s0 (lisp-source-protocol:node-start node))
         (pos 0)
         (out '()))
    (dolist (c (lisp-source-protocol:node-forms node))
      (push (subseq text pos (- (lisp-source-protocol:node-start c) s0)) out)
      (setf pos (- (lisp-source-protocol:node-end c) s0)))
    (push (subseq text pos) out)
    (nreverse out)))

(defun %same-shape-p (x y)
  (let ((cx (lisp-source-protocol:node-children x))
        (cy (lisp-source-protocol:node-children y)))
    (and (lisp-source-protocol:expression-node-p x)
         (lisp-source-protocol:expression-node-p y)
         (eq (lisp-source-protocol:node-kind x) (lisp-source-protocol:node-kind y))
         (= (length cx) (length cy))
         (every (lambda (p q)
                  (eq (lisp-source-protocol:expression-node-p p)
                      (lisp-source-protocol:expression-node-p q)))
                cx cy)
         (equal (%gaps x) (%gaps y)))))

(defun %node-leaves (x y path)
  "Leaves (indices before-text after-text) between same-shape nodes X and Y, depth first."
  (loop for cx in (lisp-source-protocol:node-forms x)
        for cy in (lisp-source-protocol:node-forms y)
        for i from 0
        for p = (append path (list i))
        for tx = (lisp-source-protocol:node-text cx)
        for ty = (lisp-source-protocol:node-text cy)
        unless (string= tx ty)
          append (if (%same-shape-p cx cy)
                     (%node-leaves cx cy p)
                     (list (list p tx ty)))))

(defun %unit-leaves (ua ub)
  "Leaves of a :MODIFY from unit UA to UB, or NIL when it stays form level."
  (let ((ha (%u-head ua))
        (hb (%u-head ub)))
    (when (and ha hb
               (lisp-source-protocol:expression-node-p ha)
               (lisp-source-protocol:expression-node-p hb))
      (let* ((ta (%u-text ua)) (tb (%u-text ub))
             (pa (- (lisp-source-protocol:node-start ha) (%u-start ua)))
             (pb (- (lisp-source-protocol:node-start hb) (%u-start ub)))
             (ea (- (lisp-source-protocol:node-end ha) (%u-start ua)))
             (eb (- (lisp-source-protocol:node-end hb) (%u-start ub))))
        (when (and (string= ta tb :end1 pa :end2 pb)
                   (string= ta tb :start1 ea :start2 eb)
                   (%same-shape-p ha hb))
          (%node-leaves ha hb '()))))))

;;; Alignment

(defun %cut-name (u)
  "Unit text with its name node's text cut out."
  (let* ((n (%u-name u))
         (s (- (lisp-source-protocol:node-start n) (%u-start u)))
         (e (- (lisp-source-protocol:node-end n) (%u-start u)))
         (text (%u-text u)))
    (concatenate 'string (subseq text 0 s) (subseq text e))))

(defun %align (e sa sb)
  "→ list of %ENTRYs from side SA to side SB, in apply order: removes (descending in
a), adds and moves (b's order), modifies (b's order)."
  (let* ((ua (%s-units sa))
         (ub (%s-units sb))
         (ia (%s-index sa))
         (ib (%s-index sb))
         (common-a (loop for u across ua
                         when (gethash (%u-key u) ib) collect (%u-key u)))
         (common-b (loop for u across ub
                         when (gethash (%u-key u) ia) collect (%u-key u)))
         (moved (%moved-keys common-a common-b))
         (removed (loop for u across ua
                        unless (gethash (%u-key u) ib) collect u))
         (added (loop for u across ub
                      unless (gethash (%u-key u) ia) collect u))
         (renames '())                  ; (old-unit . new-unit), a's order
         (positioned (make-hash-table :test #'equal)) ; a key → b position
         (entries '()))
    ;; Renames: same text once the name is cut out.
    (when (lisp-engine-detect-renames e)
      (let ((used (make-hash-table :test #'eq))
            (cut-b (mapcar (lambda (u) (and (%u-name u) (%cut-name u))) added)))
        (dolist (r removed)
          (when (%u-name r)
            (let ((cr (%cut-name r)))
              (loop for a in added
                    for ca in cut-b
                    when (and ca (not (gethash a used)) (string= cr ca))
                      do (setf (gethash a used) t)
                         (push (cons r a) renames)
                         (return)))))
        (setf renames (nreverse renames))))
    (let ((renamed-old (make-hash-table :test #'eq))
          (renamed-new (make-hash-table :test #'eq)))
      (dolist (p renames)
        (setf (gethash (car p) renamed-old) t
              (gethash (cdr p) renamed-new) t
              (gethash (%u-key (car p)) positioned) (gethash (%u-key (cdr p)) ib)))
      (maphash (lambda (k v) (declare (ignore v))
                 (setf (gethash k positioned) (gethash k ib)))
               moved)
      (labels ((apos (k) (gethash k ia))
               (bpos (k) (gethash k ib))
               (stable-p (k) (and (gethash k ib) (not (gethash k moved))))
               (move-anchor (old-key new-bpos)
                 (loop for j from (1- (apos old-key)) downto 0
                       for k = (%u-key (svref ua j))
                       do (when (stable-p k) (return k))
                          (let ((p (gethash k positioned)))
                            (when (and p (> p new-bpos)) (return k)))
                       finally (return nil))))
        (let ((removes '()) (positions '()) (modifies '()))
          (dolist (u removed)
            (unless (gethash u renamed-old)
              (push (%make-entry :kind :remove :key (%u-key u) :before u
                                 :b-anchor (%pred-key sa (apos (%u-key u))))
                    removes)))
          (dolist (u added)
            (unless (gethash u renamed-new)
              (push (cons (bpos (%u-key u))
                          (%make-entry :kind :add :key (%u-key u) :after u
                                       :a-anchor (%pred-key sb (bpos (%u-key u)))))
                    positions)))
          (dolist (p renames)
            (let* ((old (car p)) (new (cdr p)) (nb (bpos (%u-key new))))
              (push (cons nb (%make-entry :kind :move :key (%u-key new) :from (%u-key old)
                                          :before old :after new
                                          :b-anchor (move-anchor (%u-key old) nb)
                                          :a-anchor (%pred-key sb nb)))
                    positions)))
          (dolist (k common-a)
            (let ((x (svref ua (apos k)))
                  (y (svref ub (bpos k))))
              (cond ((gethash k moved)
                     (push (cons (bpos k)
                                 (%make-entry :kind :move :key k :from k :before x :after y
                                              :b-anchor (move-anchor k (bpos k))
                                              :a-anchor (%pred-key sb (bpos k))))
                           positions))
                    ((string/= (%u-text x) (%u-text y))
                     (push (cons (bpos k)
                                 (%make-entry :kind :modify :key k :before x :after y
                                              :leaves (%unit-leaves x y)))
                           modifies)))))
          ;; REMOVES was pushed in ascending a order: it is descending now.
          (setf entries
                (append removes
                        (mapcar #'cdr (sort positions #'< :key #'car))
                        (mapcar #'cdr (sort modifies #'< :key #'car)))))))
    entries))

;;; Projection

(defun %plist (u anchor)
  (list :key (%u-key u) :anchor anchor :text (%u-text u)))

(defun %entry-footprint (x)
  (if (and (eq (%e-kind x) :move) (string/= (%e-from x) (%e-key x)))
      (list (%fp (%e-from x)) (%fp (%e-key x)))
      (list (%fp (%e-key x)))))

(defun %leaf-change (key leaf)
  (destructuring-bind (indices before after) leaf
    (diff-protocol:make-change :modify :path (list* :form key indices)
                                       :footprint (list (%fp key indices))
                                       :before before :after after)))

(defun %entry-change (x level)
  "The form-level change of entry X at LEVEL (:SUMMARY lazy, :CONTENT with children)."
  (let* ((kind (%e-kind x))
         (modify-p (eq kind :modify))
         (before (and (%e-before x)
                      (%plist (%e-before x) (if modify-p nil (%e-b-anchor x)))))
         (after (and (%e-after x)
                     (%plist (%e-after x) (if modify-p nil (%e-a-anchor x)))))
         (common (list :path (list :form (%e-key x))
                       :from (and (eq kind :move) (list :form (%e-from x)))
                       :footprint (%entry-footprint x))))
    (if (eq level :summary)
        (apply #'diff-protocol:make-change kind
               (append common
                       (when before (list :before-fn (lambda () before)))
                       (when after (list :after-fn (lambda () after)))))
        (apply #'diff-protocol:make-change kind
               (append common
                       (when before (list :before before))
                       (when after (list :after after))
                       (when (and (eq level :content) (%e-leaves x))
                         (list :children (mapcar (lambda (l) (%leaf-change (%e-key x) l))
                                                 (%e-leaves x)))))))))

(defun %project (e entries level)
  (diff-protocol:make-diff
   (if (eq level :changes)
       (loop for x in entries
             append (if (%e-leaves x)
                        (mapcar (lambda (l) (%leaf-change (%e-key x) l)) (%e-leaves x))
                        (list (%entry-change x :changes))))
       (mapcar (lambda (x) (%entry-change x level)) entries))
   :engine e :level level))

(defun %diff (e a b level)
  "COMPUTE-DIFF without restarts (used inside the merge)."
  (check-type level diff-protocol:diff-level)
  (let ((sa (%read-side e a))
        (sb (%read-side e b)))
    (%project e (%align e sa sb) level)))

(defmethod diff-protocol:compute-diff ((e lisp-source-diff-engine) a b
                                       &key (level :changes) context)
  (declare (ignore context))            ; accepted for the tree engine
  (restart-case (%diff e a b level)
    (use-text-diff ()
      :report "Diff the two states with the :text engine instead."
      (diff-protocol:compute-diff (diff-protocol:find-diff-engine :text) a b :level level))
    (use-value (value)
      :report "Return a given value as the diff."
      :interactive (lambda ()
                     (format *query-io* "Value (evaluated): ")
                     (force-output *query-io*)
                     (list (eval (read *query-io*))))
      value)))

(defmethod diff-protocol:state-equal ((e lisp-source-diff-engine) a b)
  (dolist (x (list a b))
    (unless (stringp x)
      (error 'diff-protocol:unsupported-content :engine e :content x)))
  (string= a b))
