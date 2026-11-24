(in-package #:diff-backend-lisp-source/tests)

;;; compute-diff: kinds, paths, footprints, anchors, nested leaves, levels
;;; (brief: "Changes", "Nested changes", "Levels", "Change order").

(defparameter *ab* (src "(defun a (x) (+ x 1))" "(defun b () 2)"))
(defparameter *ab2c* (src "(defun a (x) (+ x 2))" "(defun b () 2)" "(defun c () 3)"))

(deftest diff-add-and-nested-modify
  (testing ":changes — the add, then the nested leaf of the modify"
    (ok (equal (specs (diff *ab* *ab2c*))
               (list (list :add '(:form "defun c") nil '("lisp:/defun c")
                           nil (side "defun c" "defun b" (src "(defun c () 3)")) 0)
                     (list :modify '(:form "defun a" 3 2) nil '("lisp:/defun a/3/2") "1" "2" 0)))))
  (testing ":content — the form-level modify carries the leaf as a child"
    (let ((cs (d:diff-changes (diff *ab* *ab2c* :content))))
      (ok (equal (mapcar #'spec cs)
                 (list (list :add '(:form "defun c") nil '("lisp:/defun c")
                             nil (side "defun c" "defun b" (src "(defun c () 3)")) 0)
                       (list :modify '(:form "defun a") nil '("lisp:/defun a")
                             (side "defun a" nil (src "(defun a (x) (+ x 1))"))
                             (side "defun a" nil (src "(defun a (x) (+ x 2))")) 1))))
      (ok (equal (mapcar #'spec (d:change-children (second cs)))
                 (list (list :modify '(:form "defun a" 3 2) nil '("lisp:/defun a/3/2") "1" "2" 0))))))
  (testing ":summary — form-level changes, lazy, no children"
    (let ((cs (d:diff-changes (diff *ab* *ab2c* :summary))))
      (ok (equal (mapcar #'d:change-kind cs) '(:add :modify)))
      (ok (equal (mapcar #'d:change-footprint cs) '(("lisp:/defun c") ("lisp:/defun a"))))
      (ok (notany #'d:change-after-loaded-p cs))
      (ok (notany #'d:change-before-loaded-p cs))
      (ok (every (lambda (c) (null (d:change-children c))) cs))
      (ok (equal (d:change-after (second cs)) (side "defun a" nil (src "(defun a (x) (+ x 2))"))))
      (ok (d:change-after-loaded-p (second cs)))))
  (testing "levels are recorded and the engine is ours"
    (dolist (level '(:summary :changes :content))
      (let ((dd (diff *ab* *ab2c* level)))
        (ok (eq level (d:diff-level dd)))
        (ok (eq (engine) (d:diff-engine dd)))))))

(deftest diff-equal-and-errors
  (dolist (level '(:summary :changes :content))
    (ok (d:diff-empty-p (diff *mixed* *mixed* level))))
  (ok (d:diff-empty-p (diff "" "")))
  (ok (d:state-equal (engine) *ab* (copy-seq *ab*)))
  (ok (not (d:state-equal (engine) *ab* (crlf "(defun a (x) (+ x 1))" "(defun b () 2)"))))
  (ok (signals (diff :not-a-string *ab*) 'd:unsupported-content))
  (ok (signals (d:state-equal (engine) *ab* 7) 'd:unsupported-content)))

(deftest diff-remove
  (ok (equal (specs (diff (src "(defun a () 1)" "(defun b () 2)") (src "(defun b () 2)")))
             (list (list :remove '(:form "defun a") nil '("lisp:/defun a")
                         (side "defun a" nil (src "(defun a () 1)")) nil 0)))))

(deftest diff-reorder-is-a-move
  (ok (equal (specs (diff (src "(defun a () 1)" "(defun b () 2)" "(defun c () 3)")
                          (src "(defun b () 2)" "(defun c () 3)" "(defun a () 1)")))
             (list (list :move '(:form "defun a") '(:form "defun a") '("lisp:/defun a")
                         (side "defun a" nil (src "(defun a () 1)"))
                         (side "defun a" "defun c" (src "(defun a () 1)")) 0))))
  (testing "a moved and edited form is one :move with different texts"
    (ok (equal (specs (diff (src "(defun a () 1)" "(defun b () 2)")
                            (src "(defun b () 2)" "(defun a () 10)")))
               (list (list :move '(:form "defun a") '(:form "defun a") '("lisp:/defun a")
                           (side "defun a" nil (src "(defun a () 1)"))
                           (side "defun a" "defun b" (src "(defun a () 10)")) 0))))))

(deftest diff-rename
  (let ((a (src "(defun old (x) (* x 2))" "(defun z () 0)"))
        (b (src "(defun new (x) (* x 2))" "(defun z () 0)")))
    (ok (equal (specs (diff a b))
               (list (list :move '(:form "defun new") '(:form "defun old")
                           '("lisp:/defun old" "lisp:/defun new")
                           (side "defun old" nil (src "(defun old (x) (* x 2))"))
                           (side "defun new" nil (src "(defun new (x) (* x 2))")) 0))))
    (testing "detect-renames NIL → remove + add"
      (ok (equal (mapcar #'spec (d:diff-changes
                                 (d:compute-diff (make-instance 'l:lisp-source-diff-engine
                                                                :detect-renames nil)
                                                 a b)))
                 (list (list :remove '(:form "defun old") nil '("lisp:/defun old")
                             (side "defun old" nil (src "(defun old (x) (* x 2))")) nil 0)
                       (list :add '(:form "defun new") nil '("lisp:/defun new")
                             nil (side "defun new" nil (src "(defun new (x) (* x 2))")) 0)))))
    (testing "a rename that also changes the body is not a rename"
      (ok (equal (mapcar #'d:change-kind
                         (d:diff-changes (diff (src "(defun old (x) (* x 2))")
                                               (src "(defun new (x) (* x 3))"))))
                 '(:remove :add))))
    (testing "different operators never pair"
      (ok (equal (mapcar #'d:change-kind
                         (d:diff-changes (diff (src "(defun old (x) x)") (src "(defmacro new (x) x)"))))
                 '(:remove :add))))))

(deftest diff-trivia-changes-are-form-level
  (testing "a comment edit before a form: a whole-unit modify, no children"
    (ok (equal (specs (diff (src ";; one" "(defun a () 1)") (src ";; two" "(defun a () 1)") :content))
               (list (list :modify '(:form "defun a") nil '("lisp:/defun a")
                           (side "defun a" nil (src ";; one" "(defun a () 1)"))
                           (side "defun a" nil (src ";; two" "(defun a () 1)")) 0)))))
  (testing "re-indenting a form: whole-unit modify (the gaps differ)"
    (ok (equal (specs (diff (src "(defun a ()" "  1)") (src "(defun a () 1)") :content))
               (list (list :modify '(:form "defun a") nil '("lisp:/defun a")
                           (side "defun a" nil (src "(defun a ()" "  1)"))
                           (side "defun a" nil (src "(defun a () 1)")) 0)))))
  (testing "a comment inside a form changes: the innermost list holding it is the leaf"
    (ok (equal (specs (diff (src "(defun a ()" "  (list 1 ; one" "        2))")
                            (src "(defun a ()" "  (list 1 ; uno" "        2))")))
               (list (list :modify '(:form "defun a" 3) nil '("lisp:/defun a/3")
                           (format nil "(list 1 ; one~%        2)")
                           (format nil "(list 1 ; uno~%        2)") 0)))))
  (testing "adding a body form changes the child count: the whole form"
    (ok (equal (mapcar #'spec (d:diff-changes (diff (src "(defun a () (f) (g))")
                                                    (src "(defun a () (f) (h) (g))"))))
               (list (list :modify '(:form "defun a") nil '("lisp:/defun a")
                           (side "defun a" nil (src "(defun a () (f) (g))"))
                           (side "defun a" nil (src "(defun a () (f) (h) (g))")) 0)))))
  (testing "the trailer after the last form"
    (ok (equal (specs (diff (src "(defun a () 1)") (src "(defun a () 1)" ";; the end")))
               (list (list :add '(:form "eof") nil '("lisp:/eof")
                           nil (side "eof" "defun a" (src ";; the end")) 0))))))

(deftest diff-nested-leaves
  (testing "several leaves in one form, in node order"
    (ok (equal (specs (diff (src "(defun f (x) (let ((y 1)) (+ x y 10)))")
                            (src "(defun f (x) (let ((y 2)) (+ x y 20)))")))
               (list (list :modify '(:form "defun f" 3 1 0 1) nil '("lisp:/defun f/3/1/0/1") "1" "2" 0)
                     (list :modify '(:form "defun f" 3 2 3) nil '("lisp:/defun f/3/2/3") "10" "20" 0)))))
  (testing "inside a #- form (read under the empty feature list)"
    (ok (equal (specs (diff (src "#-sbcl (defun f () (g 1 2))") (src "#-sbcl (defun f () (g 1 3))")))
               (list (list :modify '(:form "#-sbcl defun f" 1 3 2) nil '("lisp:/#-sbcl defun f/1/3/2")
                           "2" "3" 0)))))
  (testing "inside a #+ form the features exclude: its branch is read too (lisp-source-protocol 0.2.5)"
    (ok (equal (specs (diff (src "#+sbcl (defun e () 1)") (src "#+sbcl (defun e () 2)")))
               (list (list :modify '(:form "#+sbcl defun e" 1 3) nil '("lisp:/#+sbcl defun e/1/3")
                           "1" "2" 0)))))
  (testing "CRLF source"
    (ok (equal (specs (diff (crlf "(defun a () 1)" "(defun b () 2)") (crlf "(defun a () 1)" "(defun b () 3)")))
               (list (list :modify '(:form "defun b" 3) nil '("lisp:/defun b/3") "2" "3" 0))))))

(deftest diff-footprint-escaping
  (testing "/ and \\ in a key are escaped, so a name never looks like a path below another form"
    (ok (equal (d:diff-footprint (diff "" (src "(defsystem \"x/tests\")")))
               '("lisp:/defsystem x\\/tests")))
    (ok (equal (d:diff-footprint (diff "" (src "(defun |a\\\\b| ())")))
               '("lisp:/defun a\\\\b")))
    (ok (not (d:footprint-overlap-p "lisp:/defsystem x" "lisp:/defsystem x\\/tests"))))
  (testing "a nested leaf overlaps its form"
    (ok (d:footprint-overlap-p "lisp:/defun a" "lisp:/defun a/3/2"))))

(deftest diff-change-order
  (testing "removes (descending in a), then adds and moves (in b's order), then modifies (in b's order)"
    (ok (equal (mapcar (lambda (c) (list (d:change-kind c) (second (d:change-path c))))
                       (d:diff-changes
                        (diff (src "(defun r1 () 1)" "(defun m () 0)" "(defun k () 1)" "(defun r2 () 2)"
                                   "(defun q () 1)")
                              (src "(defun n1 () :new)" "(defun k () 2)" "(defun q () 2)" "(defun m () 0)"
                                   "(defun n2 () :new)"))))
               '((:remove "defun r2") (:remove "defun r1")
                 (:add "defun n1") (:move "defun m") (:add "defun n2")
                 (:modify "defun k") (:modify "defun q"))))))

(deftest diff-move-anchors
  (testing "a :move's before anchor is its nearest predecessor in a that is already in place
when the inverse moves it back"
    (let* ((a (src "(defun s () 0)" "(defun r () 1)" "(defun m1 () 2)" "(defun m2 () 3)" "(defun t1 () 4)"))
           (b (src "(defun s () 0)" "(defun t1 () 4)" "(defun m2 () 3)" "(defun m1 () 2)"))
           (moves (remove :move (d:diff-changes (diff a b)) :key #'d:change-kind :test-not #'eq)))
      (ok (equal (mapcar (lambda (c) (list (getf (d:change-before c) :key)
                                           (getf (d:change-before c) :anchor)
                                           (getf (d:change-after c) :anchor)))
                         moves)
                 '(("defun m2" "defun m1" "defun t1")
                   ("defun m1" "defun s" "defun m2"))))
      (ok (string= b (apply-d a (diff a b))))
      (ok (string= a (apply-d b (d:invert-diff (diff a b))))))))
