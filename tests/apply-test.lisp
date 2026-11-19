(in-package #:diff-backend-lisp-source/tests)

;;; apply-diff checks and restarts, invert, compose (brief: "Apply").
;;; Handlers sit inside OK (Rove binds its own ERROR handler around the form) and
;;; restarts are found inside the handler.

(defparameter *base* (src "(defun a (x) (+ x 1))" "(defun b () 2)"))

(defun applied (state dif)
  (multiple-value-list (d:apply-diff state dif)))

(deftest apply-by-key-ignores-unrelated-edits
  (testing "a form-level change applies to a state where other forms moved or changed"
    (let ((dd (diff *base* (src "(defun a (x) (+ x 1))" "(defun b () 3)")))
          (other (src ";; new header" "(defun b () 2)" "(defun z () 0)" "(defun a (x) (* x 9))")))
      (ok (string= (apply-d other dd)
                   (src ";; new header" "(defun b () 3)" "(defun z () 0)" "(defun a (x) (* x 9))")))))
  (testing "an add lands right after its anchor"
    (let ((dd (diff *base* (src "(defun a (x) (+ x 1))" "(defun new () t)" "(defun b () 2)")))
          (other (src "(defun b () 2)" "(defun a (x) (+ x 1))")))
      (ok (string= (apply-d other dd)
                   (src "(defun b () 2)" "(defun a (x) (+ x 1))" "(defun new () t)"))))))

(deftest apply-conflict-reasons
  (let ((modify (diff *base* (src "(defun a (x) (+ x 1))" "(defun b () 3)") :summary))
        (leaf (diff *base* (src "(defun a (x) (+ x 2))" "(defun b () 2)")))
        (add (diff *base* (src "(defun a (x) (+ x 1))" "(defun b () 2)" "(defun c () 3)")))
        (remove (diff *base* (src "(defun b () 2)"))))
    (flet ((reason (state dif)
             (handler-case (progn (d:apply-diff state dif) nil)
               (d:apply-conflict (c) (list (d:apply-conflict-reason c) (d:apply-conflict-actual c))))))
      (ok (equal (reason (src "(defun a (x) (+ x 1))") modify) '(:missing nil)))
      (ok (equal (reason (src "(defun a (x) (+ x 1))" "(defun b () 9)") modify)
                 (list :before-mismatch (src "(defun b () 9)"))))
      (ok (equal (reason (src "(defun a (x) (+ x 7))" "(defun b () 2)") leaf)
                 '(:before-mismatch "7")))
      (ok (equal (reason (src "(defun a (x) x)" "(defun b () 2)") leaf) '(:missing nil)))
      (ok (equal (reason (src "(defun a (x) (+ x 1))" "(defun b () 2)" "(defun c () :old)") add)
                 (list :exists (src "(defun c () :old)"))))
      (ok (equal (reason (src "(defun a (x) (+ x 1))") add) '(:missing nil)))
      (ok (equal (reason (src "(defun b () 2)") remove) '(:missing nil))))))

(deftest apply-restarts
  (let ((dd (diff *base* (src "(defun a (x) (+ x 5))" "(defun b () 3)")))
        (state (src "(defun a (x) (+ x 7))" "(defun b () 2)")))
    (testing "skip-change leaves the location and reports the change"
      (ok (equal (handler-bind ((d:apply-conflict (lambda (c) (d:skip-change c))))
                   (let ((r (applied state dd)))
                     (list (first r) (mapcar #'d:change-path (second r)))))
                 (list (src "(defun a (x) (+ x 7))" "(defun b () 3)")
                       '((:form "defun a" 3 2))))))
    (testing "use-value puts a node text at a leaf's path"
      (ok (equal (handler-bind ((d:apply-conflict (lambda (c) (use-value "(* x 9)" c))))
                   (first (applied state dd)))
                 (src "(defun a (x) (+ x (* x 9)))" "(defun b () 3)"))))
    (testing "use-value puts a unit text at a form-level change's key"
      (ok (equal (handler-bind ((d:apply-conflict (lambda (c) (use-value (src "(defun b () :mine)") c))))
                   (first (applied (src "(defun a (x) (+ x 1))" "(defun b () 9)")
                                   (diff *base* (src "(defun a (x) (+ x 1))" "(defun b () 3)") :summary))))
                 (src "(defun a (x) (+ x 1))" "(defun b () :mine)"))))
    (testing "use-value for an add whose anchor is gone: at the end, before the trailer"
      (ok (equal (handler-bind ((d:apply-conflict (lambda (c) (use-value (src "(defun c () 3)") c))))
                   (first (applied (src "(defun b () 2)" ";; end")
                                   (diff *base* (src "(defun a (x) (+ x 1))" "(defun c () 3)" "(defun b () 2)")))))
                 (src "(defun b () 2)" "(defun c () 3)" ";; end"))))
    (testing "abort returns the original state"
      (let ((r (handler-bind ((d:apply-conflict (lambda (c) (abort c))))
                 (applied state dd))))
        (ok (eq state (first r)))
        (ok (eq :aborted (second r)))))
    (testing "unhandled → apply-conflict"
      (ok (signals (d:apply-diff state dd) 'd:apply-conflict)))))

(deftest apply-unreadable-and-foreign-states
  (let ((dd (diff *base* (src "(defun a (x) (+ x 1))" "(defun b () 3)"))))
    (ok (signals (d:apply-diff 42 dd) 'd:unsupported-content))
    (ok (signals (d:apply-diff "(defun b (" dd) 'l:lisp-source-unreadable))
    (testing "apply-diff's restarts cover an unreadable state too"
      (ok (equal (handler-bind ((l:lisp-source-unreadable (lambda (c) (d:skip-change c))))
                   (let ((r (applied "(defun b (" dd)))
                     (list (first r) (length (second r)))))
                 '("(defun b (" 1))))))

(deftest invert-and-compose
  (let* ((a (src ";; hdr" "(defun a () 1)" "(defun b () 2)" "(defun c () 3)"))
         (b (src ";; hdr" "(defun c () 3)" "(defun b () 20)" "(defun new () 0)"))
         (c (src "(defun c () 3)" "(defun new () 1)"))
         (dab (diff a b))
         (dbc (diff b c)))
    (ok (string= a (apply-d b (d:invert-diff dab))))
    (ok (equal (specs dab) (specs (d:invert-diff (d:invert-diff dab)))))
    (ok (string= c (apply-d a (d:compose-diffs dab dbc))))
    (ok (string= a (apply-d c (d:invert-diff (d:compose-diffs dab dbc)))))
    (testing "the inverse of a :summary diff stays lazy"
      (let ((inv (d:invert-diff (diff a b :summary))))
        (ok (notany #'d:change-before-loaded-p (d:diff-changes inv)))
        (ok (string= a (apply-d b inv)))))))
