(in-package #:diff-backend-lisp-source)

;;; Reading, units and form keys (brief: "Reader", "Clean input only", "Units",
;;; "Form keys"). Everything is read through lisp-source-protocol with the empty
;;; feature list: nothing is evaluated (#. is a placeholder; READ-TIME-EVAL-REFUSED
;;; is a plain SIGNAL we decline) and no symbol is interned.

(defstruct (%unit (:conc-name %u-) (:constructor %make-unit) (:copier nil))
  "One unit of a side: KEY (unique in the file), TEXT, START / END (offsets in the
side's string), HEAD (top-level node, NIL for the trailer), NAME (name node for rename
detection, or NIL), TRAILER-P (the text after the last form), BASE (the key without
its ordinal)."
  (key "" :type string)
  (base "" :type string)
  (text "" :type string)
  (start 0 :type fixnum)
  (end 0 :type fixnum)
  (head nil)
  (name nil)
  (trailer-p nil))

(defstruct (%side (:conc-name %s-) (:constructor %make-side) (:copier nil))
  "A read side: the STRING, its UNITS (simple-vector, file order) and INDEX (EQUAL
hash table key → position in UNITS)."
  (string "" :type string)
  (units #() :type simple-vector)
  (index nil :type hash-table))

;;; Reading

(defun %parse (e x)
  "→ PARSED-SOURCE of string X read with the empty feature list. Signals
UNSUPPORTED-CONTENT for a non-string, LISP-SOURCE-UNREADABLE when X does not read clean."
  (unless (stringp x)
    (error 'diff-protocol:unsupported-content :engine e :content x))
  (let ((ps (lisp-source-protocol:read-source x :features '())))
    (unless (lisp-source-protocol:parsed-source-clean-p ps)
      (error 'lisp-source-unreadable
             :engine e :content x
             :diagnostics (append (remove-if-not
                                   (lambda (d)
                                     (eq :error (lisp-source-protocol:diagnostic-severity d)))
                                   (lisp-source-protocol:parsed-source-diagnostics ps))
                                  (copy-list (lisp-source-protocol:parsed-source-imbalances ps)))))
    ps))

;;; Heads and unit boundaries

(defun %head-p (node)
  "Top-level nodes that start a unit: expressions and skipped #+ / #- forms."
  (or (lisp-source-protocol:expression-node-p node)
      (eq (lisp-source-protocol:node-kind node) :feature-skipped)))

(defun %unit-end (s nodes i)
  "Offset where the unit of head (SVREF NODES I) ends: after the head, trailing
spaces / tabs, comments that start on that line, and the line's newline; or where
the next head starts on the same line."
  (declare (type string s) (type simple-vector nodes) (type fixnum i))
  (let ((pos (lisp-source-protocol:node-end (svref nodes i)))
        (n (length s))
        (j (1+ i)))
    (loop
      (loop while (and (< pos n) (member (char s pos) '(#\Space #\Tab)))
            do (incf pos))
      (when (>= pos n)
        (return n))
      (let ((next (and (< j (length nodes)) (svref nodes j))))
        (cond ((and next (= (lisp-source-protocol:node-start next) pos))
               (case (lisp-source-protocol:node-kind next)
                 ;; A ; comment node runs to and includes its newline.
                 (:comment (return (lisp-source-protocol:node-end next)))
                 (:block-comment (setf pos (lisp-source-protocol:node-end next))
                  (incf j))
                 (t (return pos))))
              ((char= (char s pos) #\Newline) (return (1+ pos)))
              ((and (char= (char s pos) #\Return) (< (1+ pos) n)
                    (char= (char s (1+ pos)) #\Newline))
               (return (+ pos 2)))
              (t (return pos)))))))

;;; Keys

(defun %norm (text)
  "lisp-source-protocol's locator normalisation: trimmed, whitespace runs → one
space, downcased."
  (lisp-source-protocol::normalize-space text))

(defun %name-string (node)
  "NAME of a key: a symbol → its name, downcased; a string → as written; else the
normalised text."
  (case (lisp-source-protocol:node-kind node)
    (:symbol
     (let ((v (lisp-source-protocol:node-value node)))
       (if (lisp-source-protocol:symbol-token-p v)
           (string-downcase (lisp-source-protocol:symbol-token-name v))
           (%norm (lisp-source-protocol:node-text node)))))
    (:string
     (let ((v (lisp-source-protocol:node-value node)))
       (if (stringp v) v (%norm (lisp-source-protocol:node-text node)))))
    (t (%norm (lisp-source-protocol:node-text node)))))

(defun %list-key (node)
  (let ((op (lisp-source-protocol:node-head-name node))
        (forms (lisp-source-protocol:node-forms node)))
    (cond ((null op) "list")
          ((string= op "DEFMETHOD")
           (if (second forms)
               (format nil "defmethod ~A~{ ~A~}~@[ ~A~]"
                       (%name-string (second forms))
                       (loop for f in (cddr forms)
                             until (eq (lisp-source-protocol:node-kind f) :list)
                             collect (%norm (lisp-source-protocol:node-text f)))
                       (lisp-source-protocol::method-specializers-string node))
               "defmethod"))
          ((string= op "IN-PACKAGE")
           (if (second forms)
               (format nil "in-package ~A" (%name-string (second forms)))
               "in-package"))
          ((lisp-source-protocol::definition-operator-p op)
           (if (second forms)
               (format nil "~(~A~) ~A" op (%name-string (second forms)))
               (string-downcase op)))
          (t (string-downcase op)))))

(defun %text-key (text)
  "Key of the first form of TEXT, read on its own with the empty feature list."
  (let* ((ps (lisp-source-protocol:read-source text :features '()))
         (head (find-if #'%head-p (lisp-source-protocol:parsed-source-nodes ps))))
    (if head (%head-key head) (%norm text))))

(defun %head-key (node)
  "Base key of a head node (no ordinal)."
  (let ((kind (lisp-source-protocol:node-kind node)))
    (case kind
      ((:feature-skipped :feature-conditional)
       ;; The sign comes from the text: #-(not x) is skipped, #+(not x) is read.
       (let ((text (lisp-source-protocol:node-text node))
             (fx (first (lisp-source-protocol:node-forms node))))
         (if fx
             (format nil "#~C~A ~A" (char text 1)
                     (%norm (lisp-source-protocol:node-text fx))
                     (%text-key (subseq text (- (lisp-source-protocol:node-end fx)
                                                (lisp-source-protocol:node-start node)))))
             (%norm text))))
      (:list (%list-key node))
      (t (string-downcase (symbol-name kind))))))

(defun %name-node (node)
  "Name node of a definition head (also under a read #- guard), for renames; NIL for
IN-PACKAGE, anonymous forms and skipped #+ units."
  (case (lisp-source-protocol:node-kind node)
    (:list
     (let ((op (lisp-source-protocol:node-head-name node)))
       (and op
            (not (string= op "IN-PACKAGE"))
            (or (string= op "DEFMETHOD") (lisp-source-protocol::definition-operator-p op))
            (second (lisp-source-protocol:node-forms node)))))
    (:feature-conditional
     (let ((form (second (lisp-source-protocol:node-forms node))))
       (and form (%name-node form))))
    (t nil)))

;;; Ordinals

(defun %ordinal-key (base n trailer-p)
  "Key of the unit with base key BASE that has N units of the same base before it.
The trailer is always \"eof\"; a form whose base is \"eof\" (a top-level (eof …))
counts from #2, so it never takes the trailer's key."
  (cond (trailer-p "eof")
        ((string= base "eof") (format nil "eof #~D" (+ n 2)))
        ((zerop n) base)
        (t (format nil "~A #~D" base (1+ n)))))

(defun %anonymous-p (u)
  "Units whose identity is only operator + ordinal: a head with no name node (not
in-package, not the trailer). These are matched by content (align.lisp)."
  (and (%u-head u)
       (not (%u-trailer-p u))
       (null (%u-name u))
       (not (uiop:string-prefix-p "in-package" (%u-base u)))))

;;; Cutting

(defun %cut (s ps)
  "→ simple-vector of the units of S (parsed as PS), keys with ordinals."
  (let* ((nodes (coerce (lisp-source-protocol:parsed-source-nodes ps) 'simple-vector))
         (units '())
         (prev 0)
         (n (length s)))
    (loop for i from 0 below (length nodes)
          for node = (svref nodes i)
          when (%head-p node)
            do (let ((end (%unit-end s nodes i))
                     (base (%head-key node)))
                 (push (%make-unit :key base :base base :text (subseq s prev end)
                                   :start prev :end end :head node :name (%name-node node))
                       units)
                 (setf prev end)))
    (when (< prev n)
      (push (%make-unit :key "eof" :base "eof" :text (subseq s prev) :start prev :end n
                        :trailer-p t)
            units))
    (let ((seen (make-hash-table :test #'equal))
          (out (coerce (nreverse units) 'simple-vector)))
      (loop for u across out
            unless (%u-trailer-p u)
              do (setf (%u-key u)
                       (%ordinal-key (%u-base u) (1- (incf (gethash (%u-base u) seen 0))) nil)))
      out)))

(defun %read-side (e x)
  "→ %SIDE of string X (signals as %PARSE)."
  (let* ((ps (%parse e x))
         (units (%cut x ps))
         (index (make-hash-table :test #'equal)))
    (loop for u across units
          for i from 0
          do (setf (gethash (%u-key u) index) i))
    (%make-side :string x :units units :index index)))

(defun %side-unit (side key)
  "The unit of SIDE with KEY, or NIL."
  (let ((i (gethash key (%s-index side))))
    (and i (svref (%s-units side) i))))

(defun %pred-key (side i)
  "Key of the unit before position I of SIDE, or NIL at the start."
  (and (plusp i) (%u-key (svref (%s-units side) (1- i)))))

(defun source-units (text)
  "→ fresh list of the units of Lisp source TEXT, in order, as plists
(:KEY key :ANCHOR previous-unit-key-or-NIL :TEXT unit-text). The unit texts
concatenate to TEXT. Signals LISP-SOURCE-UNREADABLE when TEXT does not read clean,
UNSUPPORTED-CONTENT when it is not a string."
  (let ((side (%read-side (or (diff-protocol:find-diff-engine :lisp)
                              (make-instance 'lisp-source-diff-engine))
                          text)))
    (loop for u across (%s-units side)
          for i from 0
          collect (list :key (copy-seq (%u-key u))
                        :anchor (let ((p (%pred-key side i))) (and p (copy-seq p)))
                        :text (copy-seq (%u-text u))))))
