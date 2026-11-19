(in-package #:diff-backend-lisp-source/tests)

;;; Units and form identity (brief: "Units", "Form keys").

(deftest engine-registered
  (ok (typep (engine) 'l:lisp-source-diff-engine))
  (ok (eq :lisp (d:engine-name (engine))))
  (ok (eq t (l:lisp-engine-detect-renames (engine))))
  (ok (null (l:lisp-engine-detect-renames
             (make-instance 'l:lisp-source-diff-engine :detect-renames nil)))))

(defparameter *mixed*
  (src ";;;; header"
       "(in-package #:foo)"
       ""
       ";;; Section"
       "(defun a (x) ; trailing"
       "  (+ x 1))"
       ""
       "#+sbcl (defun b () 1)"
       "#-sbcl (defun b () 2)"
       "#.(error \"boom\") (defmethod m :around ((x c) y) x) #| blk |#"
       "(defvar *v* 1)"
       "(setf x 1) (setf y 2)"
       "(defmethod (setf m) (v (x (eql :k))) v)"
       ";; end"))

(deftest units-of-mixed-source
  (let ((units (l:source-units *mixed*)))
    (ok (equal (mapcar (lambda (u) (getf u :key)) units)
               '("in-package foo" "defun a" "#+sbcl defun b" "#-sbcl defun b" "dispatch"
                 "defmethod m :around (c t)" "defvar *v*" "setf" "setf #2"
                 "defmethod (setf m) (t (eql :k))" "eof")))
    (ok (equal (mapcar (lambda (u) (getf u :anchor)) units)
               '(nil "in-package foo" "defun a" "#+sbcl defun b" "#-sbcl defun b" "dispatch"
                 "defmethod m :around (c t)" "defvar *v*" "setf" "setf #2"
                 "defmethod (setf m) (t (eql :k))")))
    (ok (equal (mapcar (lambda (u) (getf u :text)) units)
               (list (src ";;;; header" "(in-package #:foo)")
                     (src "" ";;; Section" "(defun a (x) ; trailing" "  (+ x 1))")
                     (src "" "#+sbcl (defun b () 1)")
                     (src "#-sbcl (defun b () 2)")
                     "#.(error \"boom\") "
                     (src "(defmethod m :around ((x c) y) x) #| blk |#")
                     (src "(defvar *v* 1)")
                     "(setf x 1) "
                     (src "(setf y 2)")
                     (src "(defmethod (setf m) (v (x (eql :k))) v)")
                     (src ";; end"))))
    (testing "the unit texts concatenate to the source"
      (ok (string= *mixed* (format nil "~{~A~}" (mapcar (lambda (u) (getf u :text)) units)))))))

(deftest-parametrize form-keys
    ((source keys)
     ;; definitions: operator + name, package prefixes dropped, names downcased
     ((src "(defun foo (x) x)") '("defun foo"))
     ((src "(cl:defmacro Foo (x) x)") '("defmacro foo"))
     ((src "(defgeneric frob (x))") '("defgeneric frob"))
     ((src "(defclass widget () ())") '("defclass widget"))
     ((src "(define-condition oops (error) ())") '("define-condition oops"))
     ((src "(defpackage #:my-pkg (:use #:cl))") '("defpackage my-pkg"))
     ((src "(defsystem \"my-sys/tests\" :depends-on ())") '("defsystem my-sys/tests"))
     ((src "(defun (setf foo) (v x) v)") '("defun (setf foo)"))
     ((src "(in-package :my-pkg)") '("in-package my-pkg"))
     ;; methods: qualifiers and specializers
     ((src "(defmethod print-object ((o widget) stream) o)") '("defmethod print-object (widget t)"))
     ((src "(defmethod m :before ((x (eql :k)) &optional y) y)") '("defmethod m :before ((eql :k))"))
     ((src "(defmethod m :around ((a pkg:cls) (b t) c) a)") '("defmethod m :around (pkg:cls t t)"))
     ;; anonymous forms: the operator, then the node kind
     ((src "(eval-when (:compile-toplevel) (foo))") '("eval-when"))
     ((src "((lambda () 1))") '("list"))
     ((src "*x*") '("symbol"))
     ((src "#.(read-time-thing)") '("dispatch"))
     ;; duplicates get an ordinal
     ((src "(setf a 1)" "(setf b 2)" "(setf c 3)") '("setf" "setf #2" "setf #3"))
     ((src "(defun f () 1)" "(defun f () 2)") '("defun f" "defun f #2"))
     ;; reader conditionals: the guard is part of the key, whatever the host features
     ((src "#+sbcl (defun g () 1)" "#-sbcl (defun g () 2)") '("#+sbcl defun g" "#-sbcl defun g"))
     ((src "#+(or sbcl ccl) (defvar *h* 1)") '("#+(or sbcl ccl) defvar *h*"))
     ((src "#+a #-b (defun k ())") '("#+a #-b defun k"))
     ;; trailing trivia after the last form
     ((src "(defun a ())" ";; done") '("defun a" "eof"))
     ((src ";; nothing but a comment") '("eof"))
     ("" '()))
  (ok (equal keys (mapcar (lambda (u) (getf u :key)) (l:source-units source)))))

(deftest unit-boundaries
  (testing "blank lines and comments before a form belong to it; a same-line comment to the form before"
    (ok (equal (mapcar (lambda (u) (getf u :text))
                       (l:source-units (src "(defun a () 1) ; about a" "" "" ";; about b" "(defun b () 2)")))
               (list (src "(defun a () 1) ; about a")
                     (src "" "" ";; about b" "(defun b () 2)")))))
  (testing "two forms on one line split where the second starts"
    (ok (equal (mapcar (lambda (u) (getf u :text)) (l:source-units (src "(a) (b)")))
               (list "(a) " (src "(b)")))))
  (testing "a #! line stays in the first unit; a last line without a newline is kept"
    (ok (equal (l:source-units (format nil "#!/usr/bin/env ros~%(defun main () 1)"))
               (list (side "defun main" nil (format nil "#!/usr/bin/env ros~%(defun main () 1)"))))))
  (testing "a block comment after a form, then a newline"
    (ok (equal (mapcar (lambda (u) (getf u :text))
                       (l:source-units (src "(defun a () 1) #| one |# ; two" "(defun b () 2)")))
               (list (src "(defun a () 1) #| one |# ; two") (src "(defun b () 2)")))))
  (testing "CRLF line ends stay in the unit texts"
    (ok (equal (mapcar (lambda (u) (getf u :text)) (l:source-units (crlf "(defun a () 1)" "(defun b () 2)")))
               (list (crlf "(defun a () 1)") (crlf "(defun b () 2)"))))))

(deftest units-ignore-host-features
  (let ((plain (l:source-units *mixed*)))
    (ok (equal plain (let ((*features* (cons :win32 (remove :sbcl *features*)))
                           (lisp-source-protocol:*source-features* '(:win32)))
                       (l:source-units *mixed*))))
    (ok (equal plain (let ((lisp-source-protocol:*source-features* '(:sbcl :a :b)))
                       (l:source-units *mixed*))))))

(deftest units-errors
  (ok (signals (l:source-units 42) 'd:unsupported-content))
  (ok (signals (l:source-units "(defun broken (x)") 'l:lisp-source-unreadable))
  (ok (signals (l:source-units "(defun extra () 1))") 'l:lisp-source-unreadable))
  (let ((c (handler-case (l:source-units "(defun broken (x)") (error (e) e))))
    (ok (typep c 'd:unsupported-content))
    (ok (plusp (length (l:lisp-source-unreadable-diagnostics c))))))
