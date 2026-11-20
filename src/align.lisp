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

;;; Content matching of anonymous units (diff-backend-lisp-source#2). Anonymous
;;; units (no name node: setf, eval-when, …) are keyed operator + ordinal, so the
;;; ordinal alone would pair the Nth setf of a with the Nth of b, and inserting one
;;; would show every later one as modified. Before aligning, b's anonymous units are
;;; re-keyed into a's namespace: a unit of b takes the key of the a unit with the
;;; same base and the same text (first unused, in order); the rest pair up in order
;;; between the same matched neighbours (an edit stays a :modify); what is left
;;; gets fresh ordinals after a's. %OPERATIONAL then renames the keys of the
;;; resulting changes to the ordinal keys of the states they apply to.

(defun %groups (side)
  "EQUAL hash base → the anonymous units of SIDE with that base, in file order."
  (let ((h (make-hash-table :test #'equal)))
    (loop for u across (%s-units side)
          when (%anonymous-p u)
            do (push u (gethash (%u-base u) h)))
    (maphash (lambda (k v) (setf (gethash k h) (nreverse v))) h)
    h))

(defun %match-group (ga gb)
  "Pair the units GA (of a) and GB (of b) of one base by content. → (values EQ hash
b-unit → a-unit, the unmatched b units in order)."
  (let ((pair (make-hash-table :test #'eq))
        (used (make-hash-table :test #'eq)))
    (dolist (y gb)
      (let ((x (find-if (lambda (x)
                          (and (not (gethash x used)) (string= (%u-text x) (%u-text y))))
                        ga)))
        (when x
          (setf (gethash x used) t
                (gethash y pair) x))))
    (flet ((segments (units matched)
             ;; key of the last matched unit before (NIL = none) → unmatched units
             (let ((segs (make-hash-table :test #'equal))
                   (seg nil))
               (dolist (u units segs)
                 (let ((m (funcall matched u)))
                   (if m
                       (setf seg m)
                       (setf (gethash seg segs) (append (gethash seg segs) (list u)))))))))
      (let ((segs-a (segments ga (lambda (x) (and (gethash x used) (%u-key x)))))
            (segs-b (segments gb (lambda (y) (let ((x (gethash y pair))) (and x (%u-key x)))))))
        (maphash (lambda (seg ys)
                   (loop for y in ys
                         for x in (gethash seg segs-a)
                         do (setf (gethash y pair) x)))
                 segs-b)))
    (values pair (remove-if (lambda (y) (gethash y pair)) gb))))

(defun %match-keys (sa sb)
  "→ (values EQ hash b-unit → a's key for matched anonymous units, EQUAL hash base →
unmatched anonymous b units, a's groups)."
  (let ((ga (%groups sa))
        (keys (make-hash-table :test #'eq))
        (fresh (make-hash-table :test #'equal)))
    (maphash (lambda (base ys)
               (multiple-value-bind (pair unmatched) (%match-group (gethash base ga) ys)
                 (maphash (lambda (y x) (setf (gethash y keys) (%u-key x))) pair)
                 (when unmatched
                   (setf (gethash base fresh) unmatched))))
             (%groups sb))
    (values keys fresh ga)))

(defun %keyed-side (side keys)
  "SIDE with each unit's key replaced by (GETHASH unit KEYS), when present."
  (let ((units (map 'simple-vector
                    (lambda (u)
                      (let ((k (gethash u keys)))
                        (if (and k (string/= k (%u-key u)))
                            (%make-unit :key k :base (%u-base u) :text (%u-text u)
                                        :start (%u-start u) :end (%u-end u) :head (%u-head u)
                                        :name (%u-name u) :trailer-p (%u-trailer-p u))
                            u)))
                    (%s-units side)))
        (index (make-hash-table :test #'equal)))
    (loop for u across units
          for i from 0
          do (setf (gethash (%u-key u) index) i))
    (%make-side :string (%s-string side) :units units :index index)))

(defun %rekey (sa sb)
  "SB with its anonymous units keyed in SA's namespace (fresh ordinals after a's for
the unmatched)."
  (multiple-value-bind (keys fresh ga) (%match-keys sa sb)
    (maphash (lambda (base ys)
               (loop for y in ys
                     for n from (length (gethash base ga))
                     do (setf (gethash y keys) (%ordinal-key base n nil))))
             fresh)
    (%keyed-side sb keys)))

(defun %rekey-both (sbase so st)
  "→ (values so st) keyed in SBASE's namespace for a merge. Unmatched anonymous units
of both sides share one numbering by text, so the same new form gets the same key
on both sides and different new forms different keys (symmetric in the sides)."
  (multiple-value-bind (ko fo ga) (%match-keys sbase so)
    (multiple-value-bind (kt ft) (%match-keys sbase st)
      (let ((bases (union (loop for b being the hash-keys of fo collect b)
                          (loop for b being the hash-keys of ft collect b)
                          :test #'string=)))
        (dolist (base bases)
          (flet ((tagged (ys)
                   (let ((seen (make-hash-table :test #'equal)))
                     (mapcar (lambda (y)
                               (list (%u-text y) (1- (incf (gethash (%u-text y) seen 0))) y))
                             ys))))
            (let* ((to (tagged (gethash base fo)))
                   (tt (tagged (gethash base ft)))
                   (tags (sort (remove-duplicates (mapcar (lambda (x) (subseq x 0 2)) (append to tt))
                                                  :test #'equal)
                               (lambda (p q)
                                 (or (string< (first p) (first q))
                                     (and (string= (first p) (first q)) (< (second p) (second q)))))))
                   (n0 (length (gethash base ga))))
              (flet ((assign (tagged keys)
                       (dolist (x tagged)
                         (setf (gethash (third x) keys)
                               (%ordinal-key base
                                             (+ n0 (position (subseq x 0 2) tags :test #'equal))
                                             nil)))))
                (assign to ko)
                (assign tt kt)))))
        (values (%keyed-side so ko) (%keyed-side st kt))))))

;;; Operational keys: the changes of a diff apply one by one, each to a re-read
;;; state, so a change must name each unit by the key it has in that state.

(defstruct (%cell (:constructor %cell (key base trailer-p)) (:copier nil))
  key base trailer-p)

(defun %op-key (cells cell)
  "Key CELL has in the state CELLS (its ordinal among cells of the same base)."
  (%ordinal-key (%cell-base cell)
                (loop for c in cells
                      until (eq c cell)
                      count (and (not (%cell-trailer-p c))
                                 (string= (%cell-base c) (%cell-base cell))))
                (%cell-trailer-p cell)))

(defun %operational (sa entries)
  "ENTRIES (in apply order, keys in the diff's namespace) with each key, from and
anchor renamed to the key of that unit in the state the change reads (before key,
from, before anchor) or makes (key, after anchor). Simulates the apply from SA.
Only ordinal keys of duplicated bases can change."
  (let ((cells (loop for u across (%s-units sa)
                     collect (%cell (%u-key u) (%u-base u) (%u-trailer-p u)))))
    (labels ((find-cell (k)
               (find k cells :key #'%cell-key :test #'string=))
             (op (k)
               (and k (let ((c (find-cell k))) (and c (%op-key cells c)))))
             (insert-after (k cell)
               (let ((p (and k (position k cells :key #'%cell-key :test #'string=))))
                 (setf cells (cond ((null k) (cons cell cells))
                                   (p (append (subseq cells 0 (1+ p)) (list cell)
                                              (nthcdr (1+ p) cells)))
                                   (t (append cells (list cell)))))))
             (new-cell (x)
               (%cell (%e-key x) (%u-base (%e-after x)) (%u-trailer-p (%e-after x))))
             (entry (x &key key from b-anchor a-anchor)
               (%make-entry :kind (%e-kind x) :key key :from from
                            :before (%e-before x) :after (%e-after x)
                            :b-anchor b-anchor :a-anchor a-anchor :leaves (%e-leaves x))))
      (loop for x in entries
            collect
            (ecase (%e-kind x)
              (:remove
               (let* ((c (find-cell (%e-key x)))
                      (y (entry x :key (%op-key cells c) :b-anchor (op (%e-b-anchor x)))))
                 (setf cells (remove c cells :test #'eq))
                 y))
              (:add
               (let ((c (new-cell x)))
                 (insert-after (%e-a-anchor x) c)
                 (entry x :key (%op-key cells c) :a-anchor (op (%e-a-anchor x)))))
              (:move
               (let* ((c (find-cell (%e-from x)))
                      (from (%op-key cells c)))
                 ;; The before anchor is the unit right before it in this state, so
                 ;; the inverse puts it back exactly where it was and every later
                 ;; inverse change sees the state the forward one left.
                 (let* ((p (position c cells :test #'eq))
                        (b-anchor (and (plusp p) (%op-key cells (nth (1- p) cells))))
                        (c2 (new-cell x)))
                   (setf cells (remove c cells :test #'eq))
                   (insert-after (%e-a-anchor x) c2)
                   (entry x :key (%op-key cells c2) :from from :b-anchor b-anchor
                            :a-anchor (op (%e-a-anchor x))))))
              (:modify
               (let* ((c (find-cell (%e-key x)))
                      (p (position c cells :test #'eq))
                      (pred (and (plusp p) (%op-key cells (nth (1- p) cells)))))
                 (entry x :key (%op-key cells c) :b-anchor pred :a-anchor pred))))))))

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
                     ;; Anchors (b's predecessor) only serve USE-VALUE on a vanished unit.
                     (push (cons (bpos k)
                                 (%make-entry :kind :modify :key k :before x :after y
                                              :b-anchor (%pred-key sb (bpos k))
                                              :a-anchor (%pred-key sb (bpos k))
                                              :leaves (%unit-leaves x y)))
                           modifies)))))
          ;; REMOVES was pushed in ascending a order: it is descending now.
          (setf entries
                (append removes
                        (mapcar #'cdr (sort positions #'< :key #'car))
                        (mapcar #'cdr (sort modifies #'< :key #'car)))))))
    entries))

;;; Projection

(defun %plist (key u anchor)
  (list :key key :anchor anchor :text (%u-text u)))

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
         (before (and (%e-before x)
                      (%plist (if (eq kind :move) (%e-from x) (%e-key x))
                              (%e-before x) (%e-b-anchor x))))
         (after (and (%e-after x)
                     (%plist (%e-key x) (%e-after x) (%e-a-anchor x))))
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
  (let* ((sa (%read-side e a))
         (sb (%rekey sa (%read-side e b))))
    (%project e (%operational sa (%align e sa sb)) level)))

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
