(in-package #:diff-backend-lisp-source/tests)

;;; Edges found after R3 (diff-backend-lisp-source#2): a unit with no final
;;; newline moved in front of a comment, a top-level (eof …) form, use-value on a
;;; :modify whose unit is gone, and anonymous top-level forms matched by content.

(defun round-trips-p (a b)
  "B from A and A from B at every level."
  (every (lambda (level)
           (let ((dd (diff a b level)))
             (and (string= b (apply-d a dd))
                  (string= a (apply-d b (d:invert-diff dd))))))
         '(:summary :changes :content)))

(defun kinds-and-keys (dif)
  (mapcar (lambda (c) (list (d:change-kind c) (second (d:change-path c))))
          (d:diff-changes dif)))

;;; (a) No final newline, moved in front of a unit that starts with a comment

(deftest edge-no-final-newline-before-a-comment
  (testing "the unit without a newline ends up in front of a comment-led unit while applying"
    (let ((a (format nil ";; about a~%(defun a () 1)~%(defun b () 2)"))
          (b (src "(defun b () 2)" ";; about a" "(defun a () 1)")))
      (ok (round-trips-p a b))
      (ok (round-trips-p b a))))
  (testing "a unit that ends before a head on the same line"
    (let ((a (src ";; c" "(defun c () 3)" "(defun p () 1) (defun q () 2)"))
          (b (src "(defun q () 2)" ";; c" "(defun c () 3)" "(defun p () 1) ")))
      (ok (round-trips-p a b))
      (ok (round-trips-p b a))))
  (testing "a later change finds its unit although the comment was cut onto the line before"
    (let ((a (format nil ";; about a~%(defun a () 1)~%(defun b () 2)"))
          (b (src "(defun b () 20)" ";; about a, edited" "(defun a () 10)")))
      (ok (round-trips-p a b)))))

;;; (b) A top-level (eof …) form

(deftest edge-eof-form-keeps-the-trailer-key
  (ok (equal (mapcar (lambda (u) (getf u :key)) (l:source-units (src "(eof 1)" ";; end")))
             '("eof #2" "eof"))
      "the trailer is always \"eof\"")
  (ok (equal (mapcar (lambda (u) (getf u :key)) (l:source-units (src "(eof 1)")))
             '("eof #2"))
      "an (eof …) form keys as eof #2 with or without a trailer")
  (ok (equal (mapcar (lambda (u) (getf u :key))
                     (l:source-units (src "(eof 1)" "(defun f ())" "(eof 2)" ";; end")))
             '("eof #2" "defun f" "eof #3" "eof")))
  (testing "adding a trailer after an (eof …) form adds \"eof\" and changes nothing else"
    (let ((a (src "(eof 1)"))
          (b (src "(eof 1)" ";; end")))
      (ok (equal (specs (diff a b))
                 (list (list :add '(:form "eof") nil '("lisp:/eof")
                             nil (side "eof" "eof #2" (src ";; end")) 0))))
      (ok (round-trips-p a b))))
  (testing "merge: the trailer stays last"
    (multiple-value-bind (text cs)
        (merged (src "(defun f ())" ";; end")
                (src "(defun f ())" "(eof 1)" ";; end")
                (src "(defun f () 2)" ";; end"))
      (ok (null cs))
      (ok (string= text (src "(defun f () 2)" "(eof 1)" ";; end"))))))

;;; (c) use-value on a :modify whose unit is gone

(deftest edge-use-value-on-a-vanished-modify
  (let* ((base (src "(defun a () 1)" "(defun b () 2)" "(defun c () 3)"))
         (dd (diff base (src "(defun a () 1)" "(defun b () 20)" "(defun c () 3)") :summary))
         (c (first (d:diff-changes dd))))
    (testing ":modify sides record anchors"
      (ok (eq :modify (d:change-kind c)))
      (ok (equal "defun a" (getf (d:change-before c) :anchor)))
      (ok (equal "defun a" (getf (d:change-after c) :anchor))))
    (testing "use-value inserts at the unit's old position, not at the start of the file"
      (ok (equal (handler-bind ((d:apply-conflict (lambda (x) (use-value (src "(defun b () :mine)") x))))
                   (apply-d (src "(defun a () 1)" "(defun c () 3)") dd))
                 (src "(defun a () 1)" "(defun b () :mine)" "(defun c () 3)"))))
    (testing "anchor gone too: at the end, before the trailer"
      (ok (equal (handler-bind ((d:apply-conflict (lambda (x) (use-value (src "(defun b () :mine)") x))))
                   (apply-d (src "(defun c () 3)" ";; end") dd))
                 (src "(defun c () 3)" "(defun b () :mine)" ";; end"))))
    (testing "the first unit's modify still anchors at the start"
      (let ((d1 (diff base (src "(defun a () 10)" "(defun b () 2)" "(defun c () 3)") :summary)))
        (ok (equal (handler-bind ((d:apply-conflict (lambda (x) (use-value (src "(defun a () :mine)") x))))
                     (apply-d (src "(defun b () 2)" "(defun c () 3)") d1))
                   (src "(defun a () :mine)" "(defun b () 2)" "(defun c () 3)")))))))

;;; (d) Anonymous top-level forms are matched by content

(defparameter *anon*
  (src "(defun f () 0)" "(setf x 1)" "(setf y 2)" "(eval-when (:execute) (g 1))"
       "(eval-when (:execute) (g 2))"))

(deftest edge-anonymous-insert-is-one-add
  (testing "a setf inserted before the others is one :add; the later ones are not modified"
    (let ((b (src "(defun f () 0)" "(setf w 0)" "(setf x 1)" "(setf y 2)" "(eval-when (:execute) (g 1))"
                  "(eval-when (:execute) (g 2))")))
      (ok (equal (specs (diff *anon* b))
                 (list (list :add '(:form "setf") nil '("lisp:/setf")
                             nil (side "setf" "defun f" (src "(setf w 0)")) 0))))
      (ok (round-trips-p *anon* b))))
  (testing "an eval-when inserted before the others"
    (let ((b (src "(defun f () 0)" "(setf x 1)" "(setf y 2)" "(eval-when (:execute) (g 0))"
                  "(eval-when (:execute) (g 1))" "(eval-when (:execute) (g 2))")))
      (ok (equal (kinds-and-keys (diff *anon* b)) '((:add "eval-when"))))
      (ok (round-trips-p *anon* b))))
  (testing "insert one, edit a later one: an :add and a :modify of the later one"
    (let ((b (src "(defun f () 0)" "(setf w 0)" "(setf x 1)" "(setf y 3)" "(eval-when (:execute) (g 1))"
                  "(eval-when (:execute) (g 2))")))
      (ok (equal (kinds-and-keys (diff *anon* b :content)) '((:add "setf") (:modify "setf #3"))))
      (ok (round-trips-p *anon* b))))
  (testing "remove the first of several"
    (let ((b (src "(defun f () 0)" "(setf y 2)" "(eval-when (:execute) (g 1))"
                  "(eval-when (:execute) (g 2))")))
      (ok (equal (kinds-and-keys (diff *anon* b)) '((:remove "setf"))))
      (ok (round-trips-p *anon* b))))
  (testing "identical duplicates"
    (let ((a (src "(setf x 1)" "(setf x 1)" "(setf y 2)"))
          (b (src "(setf x 1)" "(setf y 2)" "(setf x 1)")))
      (ok (round-trips-p a b))
      (ok (round-trips-p a (src "(setf y 2)")))
      (ok (round-trips-p (src "(setf y 2)") a))))
  (testing "two inserts in a row and a reorder"
    (let ((b (src "(setf v 0)" "(setf w 0)" "(defun f () 0)" "(setf y 2)" "(setf x 1)"
                  "(eval-when (:execute) (g 2))" "(eval-when (:execute) (g 1))")))
      (ok (round-trips-p *anon* b)))))

(deftest edge-anonymous-merge
  (testing "one side inserts a setf before the others, the other edits a later one: no conflict"
    (multiple-value-bind (text cs)
        (merged *anon*
                (src "(defun f () 0)" "(setf w 0)" "(setf x 1)" "(setf y 2)" "(eval-when (:execute) (g 1))"
                     "(eval-when (:execute) (g 2))")
                (src "(defun f () 0)" "(setf x 1)" "(setf y 3)" "(eval-when (:execute) (g 1))"
                     "(eval-when (:execute) (g 2))"))
      (ok (null cs))
      (ok (string= text (src "(defun f () 0)" "(setf w 0)" "(setf x 1)" "(setf y 3)"
                             "(eval-when (:execute) (g 1))" "(eval-when (:execute) (g 2))")))))
  (testing "both sides insert different setfs in different places: both kept"
    (multiple-value-bind (text cs)
        (merged *anon*
                (src "(defun f () 0)" "(setf w 0)" "(setf x 1)" "(setf y 2)" "(eval-when (:execute) (g 1))"
                     "(eval-when (:execute) (g 2))")
                (src "(defun f () 0)" "(setf x 1)" "(setf y 2)" "(setf z 9)" "(eval-when (:execute) (g 1))"
                     "(eval-when (:execute) (g 2))"))
      (ok (null cs))
      (ok (string= text (src "(defun f () 0)" "(setf w 0)" "(setf x 1)" "(setf y 2)" "(setf z 9)"
                             "(eval-when (:execute) (g 1))" "(eval-when (:execute) (g 2))")))))
  (testing "both sides edit the same setf differently: a conflict"
    (multiple-value-bind (text cs)
        (merged *anon*
                (src "(defun f () 0)" "(setf x 1)" "(setf y 20)" "(eval-when (:execute) (g 1))"
                     "(eval-when (:execute) (g 2))")
                (src "(defun f () 0)" "(setf x 1)" "(setf y 30)" "(eval-when (:execute) (g 1))"
                     "(eval-when (:execute) (g 2))"))
      (declare (ignore text))
      (ok (= 1 (length cs))))))

;;; Laws over a corpus of anonymous forms

(defparameter *anon-pool*
  (list (list (src "(setf a 1)") (src "(setf a 2)"))
        (list (src "(setf b 1)") (src ";; b" "(setf b 1)"))
        (list (src "(setf c 1)") (src "(setf c 1)") (src "(setf c 3)"))
        (list (src "(setf a 1)"))
        (list (src "(eval-when (:execute) (g 1))") (src "(eval-when (:execute) (g 10))"))
        (list (src "(eval-when (:execute) (g 2))"))
        (list (src "(defun f () 0)") (src "(defun f () 1)"))
        (list (src "(progn 1)") (src "(progn 1 2)"))
        ;; no final newline: the next unit (maybe comment-led) follows on its line
        (list "(setf d 1)" (src "(setf d 1)") "(defun k () 1)")))

(deftest edge-anonymous-laws
  (let* ((*pool* *anon-pool*)
         (pairs (corpus 5 120 2))
         (triples (corpus 9 80 3)))
    (dolist (p pairs)
      (destructuring-bind (a b) p
        (ok (round-trips-p a b))))
    (dolist (tr triples)
      (destructuring-bind (base ours theirs) tr
        (ok (string= theirs (d:apply-diff base (d:compose-diffs (diff base ours) (diff ours theirs)))))
        (multiple-value-bind (m cs) (d:three-way-diff (engine) base ours theirs)
          (multiple-value-bind (m2 cs2) (d:three-way-diff (engine) base theirs ours)
            (ok (string= (d:apply-diff base m) (d:apply-diff base m2)))
            (ok (equal (conflict-footprints cs) (conflict-footprints cs2)))))
        (ok (string= theirs (merged base base theirs)))
        (let ((onto-state (d:apply-diff base (diff base theirs))))
          (ok (stringp (d:apply-diff onto-state
                                     (d:rebase-diff (diff base ours) (diff base theirs) base)))))))))

(deftest edge-mixed-laws
  ;; Anonymous forms, a unit with no final newline and the named pool together.
  (let ((*pool* (append *anon-pool* *pool*)))
    (dolist (seed '(3 4))
      (dolist (tr (corpus seed 40 3))
        (destructuring-bind (base ours theirs) tr
          (ok (round-trips-p base ours))
          (ok (string= theirs (d:apply-diff base (d:compose-diffs (diff base ours) (diff ours theirs)))))
          (multiple-value-bind (m cs) (d:three-way-diff (engine) base ours theirs)
            (multiple-value-bind (m2 cs2) (d:three-way-diff (engine) base theirs ours)
              (ok (string= (d:apply-diff base m) (d:apply-diff base m2)))
              (ok (equal (conflict-footprints cs) (conflict-footprints cs2))))))))))
