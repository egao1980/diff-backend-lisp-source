(in-package #:diff-backend-lisp-source)

;;; Apply (brief: "Apply"). diff-protocol's APPLY-DIFF calls these hooks inside
;;; its per-change RESTART-CASE (SKIP-CHANGE, USE-VALUE, ABORT), so every
;;; condition signalled here, LISP-SOURCE-UNREADABLE of an unclean state
;;; included, gets those restarts. Each change reads the current state and finds
;;; its unit by key.
;;;
;;; A state is worked on as its list of units. A state this engine just produced
;;; is not re-cut: its units are the ones the change left (cached by identity, see
;;; %STATE-UNITS). Re-cutting an intermediate state can move text between units:
;;; a unit with no final newline followed by one that starts with a comment re-reads
;;; with that comment on its own line (diff-backend-lisp-source#2), so a later
;;; change of the same diff would not find its unit as the diff saw it.

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

(defun %side-text (plist)
  (getf plist :text))

(defun %check-value-text (e value c d)
  (unless (stringp value)
    (error 'diff-protocol:unsupported-content :engine e :content value :change c :diff d)))

;;; Units of a state being applied to

(defstruct (%au (:constructor %make-au (key base text trailer-p anonymous-p)) (:copier nil))
  "A unit while applying: KEY (recomputed after each change), BASE, TEXT,
TRAILER-P, ANONYMOUS-P (keyed by operator + ordinal only, see %ANONYMOUS-P)."
  key base text trailer-p anonymous-p)

(defun %text-unit (text key)
  "Unit for a unit TEXT put at KEY: base and anonymity from its first head; a text
with no head put at \"eof\" is the trailer."
  (let* ((ps (lisp-source-protocol:read-source text :features '()))
         (head (find-if #'%head-p (lisp-source-protocol:parsed-source-nodes ps))))
    (if head
        (let ((base (%head-key head)))
          (%make-au key base text nil
                    (and (null (%name-node head))
                         (not (uiop:string-prefix-p "in-package" base)))))
        (%make-au key (if (string= key "eof") "eof" (%norm text)) text
                  (string= key "eof") nil))))

(defun %rekey-units (units)
  "UNITS with keys recomputed from their bases (ordinals, the trailer \"eof\")."
  (let ((seen (make-hash-table :test #'equal)))
    (dolist (u units units)
      (setf (%au-key u)
            (if (%au-trailer-p u)
                "eof"
                (%ordinal-key (%au-base u) (1- (incf (gethash (%au-base u) seen 0))) nil))))))

(defvar *produced* '()
  "Recent (state . units) this engine returned, newest first. A state is found by
identity (EQ), so another string with the same text is read afresh.")

(defconstant +produced-max+ 8)

(defun %state-units (e state)
  "Fresh list of %AUs of STATE: the units a change of this engine left when STATE is
what it returned, else STATE cut (signals as %READ-SIDE)."
  (let ((hit (assoc state *produced* :test #'eq)))
    (if hit
        (mapcar (lambda (u)
                  (%make-au (%au-key u) (%au-base u) (%au-text u) (%au-trailer-p u)
                            (%au-anonymous-p u)))
                (cdr hit))
        (loop for u across (%s-units (%read-side e state))
              collect (%make-au (%u-key u) (%u-base u) (%u-text u) (%u-trailer-p u)
                                (%anonymous-p u))))))

(defun %produce (units)
  "→ the state string of UNITS, remembered with them for the next change."
  (let* ((units (%rekey-units units))
         (state (with-output-to-string (s)
                  (dolist (u units) (write-string (%au-text u) s)))))
    (setf *produced* (cons (cons state units)
                           (let ((rest *produced*))
                             (subseq rest 0 (min (length rest) (1- +produced-max+))))))
    state))

(defun %find-au (units key)
  (and key (find key units :key #'%au-key :test #'string=)))

(defun %locate (units key before c d)
  "The unit of KEY whose text is BEFORE. An anonymous unit whose ordinal moved is
found by its text among the units of the same base. Signals APPLY-CONFLICT
:MISSING or :BEFORE-MISMATCH."
  (let ((u (%find-au units key)))
    (cond ((and u (string= (%au-text u) before)) u)
          ((let ((base (if u (%au-base u) (%text-key before))))
             (and (or (null u) (%au-anonymous-p u))
                  (find-if (lambda (v)
                             (and (%au-anonymous-p v)
                                  (string= (%au-base v) base)
                                  (string= (%au-text v) before)))
                           units))))
          (u (%fail :before-mismatch (%au-text u) c d))
          (t (%fail :missing nil c d)))))

(defun %check-absent (units key text c d)
  "Signal :EXISTS when KEY names a unit, unless that unit and TEXT are anonymous
forms of the same base (a new one shifts the ordinals of those after it)."
  (let ((there (%find-au units key)))
    (when (and there
               (not (and (%au-anonymous-p there)
                         (%au-anonymous-p (%text-unit text key))
                         (string= (%au-base there) (%text-key text)))))
      (%fail :exists (%au-text there) c d))))

(defun %insert-after (units anchor unit)
  "UNITS with UNIT right after the unit keyed ANCHOR (NIL → at the start)."
  (if (null anchor)
      (cons unit units)
      (loop for (u . rest) on units
            collect u into head
            when (string= (%au-key u) anchor)
              return (append head (list unit) rest)
            finally (return (append units (list unit))))))

(defun %need-anchor (units anchor c d)
  (when (and anchor (not (%find-au units anchor)))
    (%fail :missing nil c d)))

(defun %apply-form (units c d)
  (let ((key (%change-target c d))
        (before (diff-protocol:change-before c))
        (after (diff-protocol:change-after c)))
    (ecase (diff-protocol:change-kind c)
      (:modify
       (let ((u (%locate units key (%side-text before) c d)))
         (substitute (%text-unit (%side-text after) (%au-key u)) u units :test #'eq)))
      (:remove
       (remove (%locate units key (%side-text before) c d) units :test #'eq))
      (:add
       (%check-absent units key (%side-text after) c d)
       (%need-anchor units (getf after :anchor) c d)
       (%insert-after units (getf after :anchor) (%text-unit (%side-text after) key)))
      (:move
       (let* ((from (second (diff-protocol:change-from c)))
              (rest (%rekey-units
                     (remove (%locate units from (%side-text before) c d) units :test #'eq))))
         (unless (string= from key)
           (%check-absent rest key (%side-text after) c d))
         (%need-anchor rest (getf after :anchor) c d)
         (%insert-after rest (getf after :anchor) (%text-unit (%side-text after) key)))))))

;;; Leaves: a node inside one unit, reached in that unit's text read on its own

(defun %unit-head (u)
  "First head node of unit U's text read on its own (offsets within that text)."
  (find-if #'%head-p (lisp-source-protocol:parsed-source-nodes
                      (lisp-source-protocol:read-source (%au-text u) :features '()))))

(defun %reach (u indices)
  "The node at INDICES (NODE-FORMS steps) inside unit U, or NIL."
  (let ((node (and u (%unit-head u))))
    (when (and node (lisp-source-protocol:expression-node-p node))
      (dolist (i indices node)
        (let ((forms (lisp-source-protocol:node-forms node)))
          (unless (< i (length forms))
            (return nil))
          (setf node (nth i forms)))))))

(defun %splice-unit (units u node text)
  (let ((s (%au-text u)))
    (substitute (%text-unit (concatenate 'string
                                         (subseq s 0 (lisp-source-protocol:node-start node))
                                         text
                                         (subseq s (lisp-source-protocol:node-end node)))
                            (%au-key u))
                u units :test #'eq)))

(defmethod diff-protocol:apply-change ((e lisp-source-diff-engine) state
                                        (c diff-protocol:opaque-change) d)
  (declare (ignore state))
  (error 'diff-protocol:unsupported-content :engine e :content c :change c :diff d))

(defmethod diff-protocol:apply-change ((e lisp-source-diff-engine) state
                                        (c diff-protocol:change) d)
  (unless (stringp state)
    (error 'diff-protocol:unsupported-content :engine e :content state :change c :diff d))
  (let ((units (%state-units e state)))
    (multiple-value-bind (key indices) (%change-target c d)
      (%produce
       (if (null indices)
           (%apply-form units c d)
           (let* ((u (%find-au units key))
                  (node (%reach u indices)))
             (unless node (%fail :missing nil c d))
             (let ((found (lisp-source-protocol:node-text node)))
               (unless (string= found (diff-protocol:change-before c))
                 (%fail :before-mismatch found c d)))
             (%splice-unit units u node (diff-protocol:change-after c))))))))

(defmethod diff-protocol:put-value ((e lisp-source-diff-engine) state
                                     (c diff-protocol:change) value d)
  (unless (stringp state)
    (error 'diff-protocol:unsupported-content :engine e :content state :change c :diff d))
  (let ((units (%state-units e state)))
    (%check-value-text e value c d)
    (multiple-value-bind (key indices) (%change-target c d)
      ;; Not remembered: VALUE may not read clean, and the next change must see that.
      (let ((new
              (if indices
                  (let* ((u (%find-au units key))
                         (node (%reach u indices)))
                    (unless node (%fail :missing nil c d))
                    (%splice-unit units u node value))
                  (let ((unit (%make-au key "" value nil nil))
                        (there (%find-au units key)))
                    (if there
                        (substitute unit there units :test #'eq)
                        (let ((after (diff-protocol:change-after c))
                              (before (diff-protocol:change-before c)))
                          (flet ((placeable-p (plist)
                                   (and plist
                                        (let ((a (getf plist :anchor)))
                                          (or (null a) (%find-au units a))))))
                            (cond ((placeable-p after)
                                   (%insert-after units (getf after :anchor) unit))
                                  ((placeable-p before)
                                   (%insert-after units (getf before :anchor) unit))
                                  (t
                                   (let ((trailer (position-if #'%au-trailer-p units)))
                                     (if trailer
                                         (append (subseq units 0 trailer) (list unit)
                                                 (nthcdr trailer units))
                                         (append units (list unit)))))))))))))
        (with-output-to-string (s)
          (dolist (u new) (write-string (%au-text u) s)))))))
