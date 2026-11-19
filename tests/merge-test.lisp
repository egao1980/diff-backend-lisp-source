(in-package #:diff-backend-lisp-source/tests)

;;; three-way-diff and rebase-diff (brief: "Three-way merge", "Rebase").

(defparameter *m-base*
  (src "(in-package #:m)"
       ""
       ";; a"
       "(defun a (x) (+ x 1))"
       ""
       "(defun b (y) (list y))"
       ""
       "(defun c () :c)"))

(defun replace-once (string old new)
  (let ((p (search old string)))
    (assert p () "~S not in fixture" old)
    (concatenate 'string (subseq string 0 p) new (subseq string (+ p (length old))))))

(deftest merge-disjoint-form-edits
  (let ((ours (replace-once *m-base* "(+ x 1)" "(+ x 2)"))
        (theirs (replace-once *m-base* "(list y)" "(vector y)")))
    (multiple-value-bind (text cs) (merged *m-base* ours theirs)
      (ok (null cs))
      (ok (string= text (replace-once ours "(list y)" "(vector y)"))))
    (testing "the merged diff is at :changes and applies to base"
      (let ((m (d:three-way-diff (engine) *m-base* ours theirs)))
        (ok (eq :changes (d:diff-level m)))
        (ok (equal (d:render-diff m :summary nil)
                   (format nil "M lisp:/defun a/3/2~%M lisp:/defun b/3/0~%")))))))

(deftest merge-both-append
  (testing "both sides add a form at the end: no conflict, the run whose first key sorts first goes first"
    (let ((ours (concatenate 'string *m-base* (src "" "(defun zed () :ours)")))
          (theirs (concatenate 'string *m-base* (src "" "(defun alpha () :theirs)"))))
      (multiple-value-bind (text cs) (merged *m-base* ours theirs)
        (ok (null cs))
        (ok (string= text (concatenate 'string *m-base*
                                       (src "" "(defun alpha () :theirs)" "" "(defun zed () :ours)")))))
      (testing "symmetric"
        (ok (string= (merged *m-base* ours theirs) (merged *m-base* theirs ours))))))
  (testing "a side's own run keeps its order"
    (let ((ours (concatenate 'string *m-base* (src "(defun y1 ())" "(defun a1 ())")))
          (theirs (concatenate 'string *m-base* (src "(defun b1 ())"))))
      (ok (string= (merged *m-base* ours theirs)
                   (concatenate 'string *m-base* (src "(defun b1 ())" "(defun y1 ())" "(defun a1 ())")))))))

(deftest merge-same-form-conflicts
  (let ((ours (replace-once *m-base* "(+ x 1)" "(+ x 2)"))
        (theirs (replace-once *m-base* "(defun a (x)" "(defun a (x &optional y)")))
    (multiple-value-bind (text cs) (merged *m-base* ours theirs)
      (ok (string= text *m-base*))
      (ok (equal (conflict-footprints cs) '("lisp:/defun a")))
      (testing "conflict sides are the form-level changes"
        (let ((c (first cs)))
          (ok (eq :modify (d:change-kind (d:conflict-ours c))))
          (ok (equal (d:change-path (d:conflict-ours c)) '(:form "defun a")))
          (ok (equal (getf (d:change-after (d:conflict-ours c)) :text)
                     (src "" ";; a" "(defun a (x) (+ x 2))")))
          (ok (equal (getf (d:change-after (d:conflict-theirs c)) :text)
                     (src "" ";; a" "(defun a (x &optional y) (+ x 1))")))))))
  (testing "other forms still merge next to a conflict"
    (let ((ours (replace-once (replace-once *m-base* "(+ x 1)" "(+ x 2)") ":c" ":ours-c"))
          (theirs (replace-once *m-base* "(+ x 1)" "(+ x 3)")))
      (multiple-value-bind (text cs) (merged *m-base* ours theirs)
        (ok (string= text (replace-once *m-base* ":c" ":ours-c")))
        (ok (equal (conflict-footprints cs) '("lisp:/defun a"))))))
  (testing "a comment edit and a body edit of the same form conflict"
    (let ((ours (replace-once *m-base* ";; a" ";; about a"))
          (theirs (replace-once *m-base* "(+ x 1)" "(+ x 2)")))
      (ok (equal (conflict-footprints (nth-value 1 (merged *m-base* ours theirs))) '("lisp:/defun a"))))))

(deftest merge-identical-changes-once
  (let ((both (replace-once (concatenate 'string *m-base* (src "(defun d () :d)")) "(list y)" "(cons y nil)")))
    (multiple-value-bind (text cs) (merged *m-base* both both)
      (ok (null cs))
      (ok (string= text both)))))

(deftest merge-kind-conflicts
  (testing "modify vs remove"
    (let ((ours (replace-once *m-base* "(list y)" "(list y y)"))
          (theirs (replace-once *m-base* (src "" "(defun b (y) (list y))") "")))
      (multiple-value-bind (text cs) (merged *m-base* ours theirs)
        (ok (string= text *m-base*))
        (ok (equal (conflict-footprints cs) '("lisp:/defun b")))
        (ok (equal (list (d:change-kind (d:conflict-ours (first cs)))
                         (d:change-kind (d:conflict-theirs (first cs))))
                   '(:modify :remove))))))
  (testing "add vs add of the same key with different text"
    (let ((ours (concatenate 'string *m-base* (src "(defun d () 1)")))
          (theirs (concatenate 'string *m-base* (src "(defun d () 2)"))))
      (multiple-value-bind (text cs) (merged *m-base* ours theirs)
        (ok (string= text *m-base*))
        (ok (equal (conflict-footprints cs) '("lisp:/defun d"))))))
  (testing "rename vs an edit of the renamed form"
    (let ((ours (replace-once *m-base* "(defun c ()" "(defun c2 ()"))
          (theirs (replace-once *m-base* ":c" ":cc")))
      (multiple-value-bind (text cs) (merged *m-base* ours theirs)
        (ok (string= text *m-base*))
        (ok (equal (conflict-footprints cs) '("lisp:/defun c lisp:/defun c2"))))))
  (testing "both sides move the same form to different places"
    (flet ((forms (&rest names)
             (format nil "~{~A~}" (mapcar (lambda (n) (src "" (format nil "(defun ~A () :~:*~A)" n))) names))))
      (let ((base (forms "a" "b" "c" "d" "e"))
            (ours (forms "c" "a" "b" "d" "e"))
            (theirs (forms "a" "b" "d" "e" "c")))
        (multiple-value-bind (text cs) (merged base ours theirs)
          (ok (string= text base))
          (ok (equal (conflict-footprints cs) '("lisp:/defun c"))))))))

(deftest merge-reorder-and-modify-combine
  (testing "one side only moves a form, the other edits it: both apply"
    (let ((ours (src "(in-package #:m)" "" "(defun c () :c)" "" ";; a" "(defun a (x) (+ x 1))"
                     "" "(defun b (y) (list y))"))
          (theirs (replace-once *m-base* ":c" ":theirs-c")))
      (multiple-value-bind (text cs) (merged *m-base* ours theirs)
        (ok (null cs))
        (ok (string= text (replace-once ours ":c" ":theirs-c")))))))

(deftest merge-one-side-unchanged
  (let ((other (replace-once (concatenate 'string *m-base* (src "(defun d ())")) ";; a" ";; A")))
    (multiple-value-bind (text cs) (merged *m-base* *m-base* other)
      (ok (null cs))
      (ok (string= text other)))
    (multiple-value-bind (text cs) (merged *m-base* other *m-base*)
      (ok (null cs))
      (ok (string= text other)))))

(deftest merge-removed-anchor
  (testing "theirs adds after a form ours removed: placed after the nearest surviving predecessor"
    (let ((ours (replace-once *m-base* (src "" "(defun b (y) (list y))") ""))
          (theirs (replace-once *m-base* (src "(defun b (y) (list y))")
                                (src "(defun b (y) (list y))" "(defun b2 () :new)"))))
      (multiple-value-bind (text cs) (merged *m-base* ours theirs)
        (ok (null cs))
        (ok (string= text (replace-once ours (src "(defun a (x) (+ x 1))")
                                        (src "(defun a (x) (+ x 1))" "(defun b2 () :new)"))))))))

(deftest merge-errors-and-text-fallback
  (let ((broken "(defun a (x)"))
    (ok (signals (d:three-way-diff (engine) *m-base* broken *m-base*) 'l:lisp-source-unreadable))
    (testing "use-text-diff falls back to the :text engine's diff3"
      (let ((m (handler-bind ((l:lisp-source-unreadable (lambda (c) (l:use-text-diff c))))
                 (d:three-way-diff (engine) *m-base* broken *m-base*))))
        (ok (eq :text (d:engine-name (d:diff-engine m))))
        (ok (string= broken (d:apply-diff *m-base* m)))))
    (testing "use-value returns a given result"
      (ok (eq :given (handler-bind ((l:lisp-source-unreadable (lambda (c) (use-value :given c))))
                       (d:three-way-diff (engine) *m-base* broken *m-base*)))))))

(deftest rebase
  (let* ((ours (replace-once *m-base* "(+ x 1)" "(+ x 2)"))
         (onto-state (concatenate 'string (replace-once *m-base* "(list y)" "(vector y)") (src "(defun d ())")))
         (d (diff *m-base* ours))
         (onto (diff *m-base* onto-state)))
    (testing "disjoint: the rebased diff applies to onto's state"
      (multiple-value-bind (r cs) (d:rebase-diff d onto *m-base*)
        (ok (null cs))
        (ok (string= (apply-d onto-state r) (replace-once onto-state "(+ x 1)" "(+ x 2)")))))
    (testing "conflicts are data, resolved to onto"
      (let* ((onto2-state (replace-once *m-base* "(+ x 1)" "(+ x 9)"))
             (onto2 (diff *m-base* onto2-state)))
        (multiple-value-bind (r cs) (d:rebase-diff d onto2 *m-base*)
          (ok (equal (conflict-footprints cs) '("lisp:/defun a")))
          (ok (string= (apply-d onto2-state r) onto2-state))
          (ok (equal (getf (d:change-after (d:conflict-ours (first cs))) :text)
                     (src "" ";; a" "(defun a (x) (+ x 2))"))))))))
