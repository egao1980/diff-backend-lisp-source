(defpackage #:diff-backend-lisp-source/tests
  (:use #:cl #:rove)
  (:local-nicknames (#:d #:diff-protocol)
                    (#:l #:diff-backend-lisp-source)))
(in-package #:diff-backend-lisp-source/tests)

;;; Shared helpers. Brief: cl-stack docs/capabilities/diff-backend-lisp-source.md.

(defun engine ()
  "The registered engine (find-diff-engine :lisp)."
  (d:find-diff-engine :lisp))

(defun src (&rest lines)
  "Lines joined, each ended by #\\Newline."
  (format nil "~{~A~%~}" lines))

(defun crlf (&rest lines)
  "Lines joined, each ended by CR LF."
  (format nil "~{~A~C~%~}" (loop for x in lines collect x collect #\Return)))

(defun fixture (name)
  "Contents of tests/fixtures/NAME, read as UTF-8 (bytes kept: fixtures are -text)."
  (uiop:read-file-string
   (asdf:system-relative-pathname "diff-backend-lisp-source"
                                  (concatenate 'string "tests/fixtures/" name))
   :external-format :utf-8))

(defun diff (a b &optional (level :changes))
  (d:compute-diff (engine) a b :level level))

(defun side (key anchor text)
  (list :key key :anchor anchor :text text))

(defun spec (c)
  "A change as plain data: (kind path from footprint before after n-children)."
  (list (d:change-kind c) (d:change-path c) (d:change-from c) (d:change-footprint c)
        (d:change-before c) (d:change-after c) (length (d:change-children c))))

(defun specs (dif)
  (mapcar #'spec (d:diff-changes dif)))

(defun summary (a b &optional (level :changes))
  (d:render-diff (diff a b level) :summary nil))

(defun apply-d (state dif)
  (values (d:apply-diff state dif)))

(defun conflict-footprints (conflicts)
  "Sorted list of each conflict's footprint strings joined by spaces."
  (sort (mapcar (lambda (c) (format nil "~{~A~^ ~}" (d:conflict-footprint c))) conflicts)
        #'string<))

(defun merged (base ours theirs)
  "→ (values merged-text conflicts) through THREE-WAY-DIFF."
  (multiple-value-bind (m cs) (d:three-way-diff (engine) base ours theirs)
    (values (d:apply-diff base m) cs)))
