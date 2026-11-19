(in-package #:diff-backend-lisp-source/tests)

;;; Rendering (brief: "Render") and the tree engine as host (brief: "Under the tree engine").

(defparameter *r-a* (src "(in-package #:r)" "" "(defun a (x)" "  (+ x 1))" "" "(defun b () 2)"))
(defparameter *r-b* (src "(in-package #:r)" "" "(defun a (x)" "  (+ x 2))" "" "(defun b () 2)" ""
                         "(defun c () 3)"))

(deftest render-summary-and-sexp
  (ok (equal (summary *r-a* *r-b*)
             (format nil "A lisp:/defun c~%M lisp:/defun a/3/2~%")))
  (ok (equal (summary *r-a* *r-b* :summary)
             (format nil "A lisp:/defun c~%M lisp:/defun a~%")))
  (testing ":sexp round trip; the parsed diff applies (engine found by name)"
    (dolist (level '(:summary :changes :content))
      (let* ((text (d:render-diff (diff *r-a* *r-b* level) :sexp nil))
             (back (d:parse-diff :sexp text)))
        (ok (string= text (d:render-diff back :sexp nil)))
        (ok (eq (engine) (d:diff-engine back)))
        (ok (string= *r-b* (apply-d *r-a* back)))))))

(deftest unified-patch-text
  (ok (string= (l:unified-patch *r-a* (diff *r-a* *r-b*) :context 1)
               (src "@@ -3,4 +3,6 @@"
                    " (defun a (x)"
                    "-  (+ x 1))"
                    "+  (+ x 2))"
                    " "
                    " (defun b () 2)"
                    "+"
                    "+(defun c () 3)")))
  (ok (string= (l:unified-patch *r-a* (diff *r-a* *r-b*) :path "src/r.lisp" :context 0)
               (src "--- a/src/r.lisp"
                    "+++ b/src/r.lisp"
                    "@@ -4 +4 @@"
                    "-  (+ x 1))"
                    "+  (+ x 2))"
                    "@@ -6,0 +7,2 @@"
                    "+"
                    "+(defun c () 3)")))
  (ok (string= "" (l:unified-patch *r-a* (diff *r-a* *r-a*) :path "src/r.lisp")))
  (testing "every level renders the same patch"
    (ok (string= (l:unified-patch *r-a* (diff *r-a* *r-b* :summary))
                 (l:unified-patch *r-a* (diff *r-a* *r-b* :content)))))
  (testing "the patch parses and applies to the file"
    (let ((bare (d:parse-diff :unified (l:unified-patch *r-a* (diff *r-a* *r-b*)))))
      (ok (string= *r-b* (d:apply-diff *r-a* bare))))
    (let ((tree (d:parse-diff :unified (l:unified-patch *r-a* (diff *r-a* *r-b*) :path "src/r.lisp"))))
      (ok (d:state-equal (d:find-diff-engine :tree)
                         (d:apply-diff (d:make-tree "src" (d:make-tree "r.lisp" (d:make-tree-file *r-a*))) tree)
                         (d:make-tree "src" (d:make-tree "r.lisp" (d:make-tree-file *r-b*))))))))

(defun lisp-tree-engine ()
  (make-instance 'd:tree-diff-engine :leaf-engines '((:text . :text) (:lisp . :lisp))))

(defun lisp-tree (content &optional (notes "n\n"))
  (d:make-tree "src" (d:make-tree "r.lisp" (d:make-tree-file content :type :lisp))
               "notes.txt" (d:make-tree-file notes)))

(deftest tree-engine-leaf
  (let* ((te (lisp-tree-engine))
         (ta (lisp-tree *r-a*))
         (tb (lisp-tree *r-b* (format nil "x~%")))
         (dd (d:compute-diff te ta tb)))
    (testing "leaf changes nest under the file; footprints are spliced"
      (ok (equal (mapcar #'d:change-path (d:diff-changes dd))
                 '(("notes.txt" 0) ("src" "r.lisp" :form "defun c") ("src" "r.lisp" :form "defun a" 3 2))))
      (ok (equal (d:diff-footprint dd)
                 '("lisp:/src/r.lisp/defun a/3/2" "lisp:/src/r.lisp/defun c" "text:/notes.txt/0-1"
                   "tree:/notes.txt" "tree:/src/r.lisp"))))
    (ok (d:state-equal te tb (d:apply-diff ta dd)))
    (ok (d:state-equal te ta (d:apply-diff tb (d:invert-diff dd))))
    (testing "three-way per file goes through the form engine"
      (let ((ours (lisp-tree (replace-once *r-a* "(+ x 1)" "(+ x 2)")))
            (theirs (lisp-tree (concatenate 'string *r-a* (src "(defun d ())")))))
        (multiple-value-bind (m cs) (d:three-way-diff te ta ours theirs)
          (ok (null cs))
          (ok (d:state-equal te (d:apply-diff ta m)
                             (lisp-tree (concatenate 'string (replace-once *r-a* "(+ x 1)" "(+ x 2)")
                                                     (src "(defun d ())")))))))
      (let ((ours (lisp-tree (replace-once *r-a* "(+ x 1)" "(+ x 2)")))
            (theirs (lisp-tree (replace-once *r-a* "(+ x 1)" "(+ x 3)"))))
        (ok (equal (conflict-footprints (nth-value 1 (d:three-way-diff te ta ours theirs)))
                   '("lisp:/src/r.lisp/defun a tree:/src/r.lisp")))))))

(deftest never-evaluates
  (let ((a (src "(defun i () \"doc\" #.(error \"never\") (nope:x 1))" "#.(error \"top\")"))
        (b (src "(defun i () \"doc!\" #.(error \"never\") (nope:x 2))" "#.(error \"top!\")")))
    (ok (equal (summary a b)
               (format nil "M lisp:/defun i/3~%M lisp:/defun i/5/1~%M lisp:/dispatch/0/1~%")))
    (ok (string= b (apply-d a (diff a b))))
    (ok (string= b (merged a a b)))
    (ok (null (find-package "NOPE")))))

(deftest host-features-do-not-matter
  (let* ((a (src "#+sbcl (defun e () 1)" "#-sbcl (defun f () (g 1 2))" "#+win32 (defun w () 0)"))
         (b (src "#+sbcl (defun e () 2)" "#-sbcl (defun f () (g 1 3))" "#+win32 (defun w () 1)"))
         (plain (specs (diff a b :content))))
    (ok (equal plain (let ((*features* (cons :win32 (remove :sbcl *features*))))
                       (specs (diff a b :content)))))
    (ok (equal plain (let ((lisp-source-protocol:*source-features* '(:sbcl :win32)))
                       (specs (diff a b :content)))))))

(deftest compute-diff-unreadable
  (let ((broken (src "(defun a (x)" "  (+ x 1)")))
    (ok (signals (diff *r-a* broken) 'l:lisp-source-unreadable))
    (ok (signals (diff broken *r-a*) 'd:unsupported-content))
    (testing "use-text-diff → the :text engine's diff at the same level"
      (let ((dd (handler-bind ((l:lisp-source-unreadable (lambda (c) (l:use-text-diff c))))
                  (d:compute-diff (engine) *r-a* broken :level :content))))
        (ok (eq :text (d:engine-name (d:diff-engine dd))))
        (ok (eq :content (d:diff-level dd)))
        (ok (string= broken (d:apply-diff *r-a* dd)))))
    (testing "use-value → the given value"
      (ok (eq :given (handler-bind ((l:lisp-source-unreadable (lambda (c) (use-value :given c))))
                       (diff *r-a* broken)))))
    (testing "use-text-diff with no restart around returns NIL"
      (ok (null (l:use-text-diff))))))
