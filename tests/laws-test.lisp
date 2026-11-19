(in-package #:diff-backend-lisp-source/tests)

;;; diff-protocol laws for the form engine over a deterministic random corpus
;;; (brief: "Laws"). Sources are drawn from a pool of units with variants:
;;; edits inside a form, comment edits, renames, reader conditionals, #., a
;;; package that does not exist, block comments; plus reordering, an optional
;;; header and trailer.

(defparameter *pool*
  (list (list (src "(defun a (x) (+ x 1))") (src "(defun a (x) (+ x 2))") (src "(defun a2 (x) (+ x 1))"))
        (list (src ";; doc for b" "(defun b () 2)") (src ";; doc for b, edited" "(defun b () 2)")
              (src "" ";; doc for b" "(defun b () (list 2 3))"))
        (list (src "(defvar *c* 3) ; trailing") (src "(defvar *c* 4) ; trailing"))
        (list (src "(defmethod d :around ((x c) y) (call-next-method))")
              (src "(defmethod d :around ((x c) y)" "  (print y)" "  (call-next-method))"))
        (list (src "#+sbcl (defun e () :sbcl)") (src "#+sbcl (defun e () :sbcl!)"))
        (list (src "#-sbcl (defun f () (g 1 2))") (src "#-sbcl (defun f () (g 1 3))"))
        (list (src "(setf (gethash :g *h*) 1)") (src "(setf (gethash :g *h*) 2)"))
        (list (src "(defclass h ()" "  ((s :initarg :s))) #| block |#")
              (src "(defclass h ()" "  ((s :initarg :s)" "   (u :initform 0))) #| block |#"))
        (list (src "(defun i () \"doc\" #.(error \"never\") (nope:x 1))")
              (src "(defun i () \"doc, edited\" #.(error \"never\") (nope:x 1))"))))

(defvar *seed* 0)

(defun rnd (n)
  "Deterministic LCG (glibc constants), so the corpus is the same on every implementation."
  (setf *seed* (mod (+ (* *seed* 1103515245) 12345) 2147483648))
  (mod (floor *seed* 65536) n))

(defun random-source ()
  (let* ((items (loop for variants in *pool*
                      when (< (rnd 10) 7) collect (nth (rnd (length variants)) variants)))
         (n (length items)))
    (dotimes (k (rnd 3))
      (when (> n 1)
        (let ((i (rnd n)) (j (rnd n)))
          (rotatef (nth i items) (nth j items)))))
    (format nil "~:[~;;;;; header~%~]~{~A~}~:[~;;; trailer~%~]" (zerop (rnd 2)) items (zerop (rnd 3)))))

(defun corpus (seed n arity)
  (let ((*seed* seed))
    (loop repeat n collect (loop repeat arity collect (random-source)))))

(defparameter *pairs* (corpus 42 60 2))
(defparameter *triples* (corpus 7 40 3))

(deftest-parametrize law-apply-invert-empty
    ((level) (:summary) (:changes) (:content))
  (dolist (p *pairs*)
    (destructuring-bind (a b) p
      (let ((dd (diff a b level)))
        (ok (string= b (d:apply-diff a dd)))
        (ok (string= a (d:apply-diff b (d:invert-diff dd))))
        (ok (d:diff-empty-p (diff a a level)))))))

(deftest law-compose
  (dolist (tr *triples*)
    (destructuring-bind (a b c) tr
      (let ((d1 (diff a b)) (d2 (diff b c)) (d3 (diff c a)))
        (ok (string= c (d:apply-diff a (d:compose-diffs d1 d2))))
        (ok (string= (d:apply-diff a (d:compose-diffs (d:compose-diffs d1 d2) d3))
                     (d:apply-diff a (d:compose-diffs d1 (d:compose-diffs d2 d3)))))))))

(deftest law-three-way
  (dolist (tr *triples*)
    (destructuring-bind (base ours theirs) tr
      (multiple-value-bind (m cs) (d:three-way-diff (engine) base ours theirs)
        (multiple-value-bind (m2 cs2) (d:three-way-diff (engine) base theirs ours)
          (testing "symmetric: same merged state, same conflict footprints"
            (ok (string= (d:apply-diff base m) (d:apply-diff base m2)))
            (ok (equal (conflict-footprints cs) (conflict-footprints cs2))))))
      (testing "one side unchanged, or both equal: the other side, no conflicts"
        (multiple-value-bind (text cs) (merged base base theirs)
          (ok (null cs))
          (ok (string= text theirs)))
        (multiple-value-bind (text cs) (merged base ours ours)
          (ok (null cs))
          (ok (string= text ours))))
      (testing "the rebased diff applies to onto's state"
        (let ((onto-state (d:apply-diff base (diff base theirs))))
          (ok (stringp (d:apply-diff onto-state (d:rebase-diff (diff base ours) (diff base theirs) base)))))))))
