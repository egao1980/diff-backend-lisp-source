(in-package #:diff-backend-lisp-source/tests)

;;; Real files from the workspace's own history (plan R3 acceptance: "Round-trip
;;; on the workspace's own .lisp history; comments preserved"). Fixtures in
;;; tests/fixtures/history, byte for byte from git:
;;;   indent-0/1/2   lisp-source-protocol src/indent.lisp at caf719c, 3424b55, 3b71bca
;;;   indent-theirs  3b71bca's change (3424b55..3b71bca) applied with patch(1) to indent-0
;;;   edit-0/1/2     lisp-source-protocol src/edit.lisp at caf719c, 31ccd7d, fd1a363
;;;   reader-0/1     lisp-source-protocol src/reader.lisp at 12315c5^, 12315c5
;;;   package-0/1    lisp-source-protocol src/package.lisp at a947591^, a947591
;;;   ops-0/1        diff-protocol src/ops.lisp at 27ae849 (R0), 944df0a (R1)

(defparameter *history-pairs*
  '(("indent-0" "indent-1") ("indent-1" "indent-2") ("indent-0" "indent-theirs")
    ("edit-0" "edit-1") ("edit-1" "edit-2") ("reader-0" "reader-1")
    ("package-0" "package-1") ("ops-0" "ops-1")))

(defun hist (name)
  (fixture (format nil "history/~A.lisp" name)))

(deftest-parametrize history-round-trip
    ((a b) :rows *history-pairs*)
  (let ((ta (hist a)) (tb (hist b)))
    (testing "the units concatenate back to the file"
      (ok (string= ta (format nil "~{~A~}" (mapcar (lambda (u) (getf u :text)) (l:source-units ta))))))
    (dolist (level '(:summary :changes :content))
      (let ((dd (diff ta tb level)))
        (ok (string= tb (d:apply-diff ta dd)))
        (ok (string= ta (d:apply-diff tb (d:invert-diff dd))))
        (ok (string= tb (d:apply-diff ta (d:parse-diff :sexp (d:render-diff dd :sexp nil)))))))
    (testing "the unified patch applies to the file"
      (ok (string= tb (d:apply-diff ta (d:parse-diff :unified (l:unified-patch ta (diff ta tb)))))))
    (testing "every comment of b survives in the applied result"
      (let ((applied (d:apply-diff ta (diff ta tb))))
        (dolist (u (l:source-units tb))
          (ok (search (getf u :text) applied)))))))

(deftest-parametrize history-summaries
    ((a b expected)
     ("indent-0" "indent-1"
      '("A lisp:/defparameter *method-option-operators*"
       "A lisp:/defun bare-operator-name"
       "M lisp:/defun scan-indent-line/4/3/2/1/4/2"))
     ("indent-1" "indent-2"
      '("M lisp:/defvar *indentation*"
       "M lisp:/defparameter +standard-indentation+/2/0"
       "M lisp:/defun line-indent-for/4/2/3/2"))
     ("edit-0" "edit-1"
      '("M lisp:/defun wrap-node/3"
       "M lisp:/defun wrap-node/7/0"
       "M lisp:/defun wrap-node/7/1"
       "M lisp:/defun wrap-node/7/2/3/2/2/1/3/5"))
     ("edit-1" "edit-2"
      '("A lisp:/defun line-start"
       "A lisp:/defun comment-line-p"
       "A lisp:/defun leading-comment-start"
       "M lisp:/defun delete-node/6"
       "M lisp:/defun transpose-nodes/6/4"
       "M lisp:/defun move-node/8/4/1/2/1"))
     ("reader-0" "reader-1"
      '("A lisp:/defun script-header-end"
       "M lisp:/defun read-source/4/1/7/1"))
     ("package-0" "package-1"
      '("M lisp:/defpackage lisp-source-protocol/4"))
     ("ops-0" "ops-1"
      '("A lisp:/defun %range-footprint-p"
       "A lisp:/defun %inverse-footprint"
       "A lisp:/defun %list-step-p"
       "A lisp:/defun %tail-runs"
       "A lisp:/defun %close-runs"
       "A lisp:/defun %reindex-add"
       "A lisp:/defun %reindex-adds"
       "A lisp:/defun %count-removes"
       "A lisp:/defgeneric %three-way"
       "A lisp:/defgeneric %rebase"
       "A lisp:/defun %merge-leaves"
       "A lisp:/defmethod %three-way (diff-engine t t t t)"
       "A lisp:/defmethod %rebase (diff-engine t t t)"
       "A lisp:/defun %rebase-by-merge"
       "M lisp:/defmethod apply-diff (t diff)/3/2/1/2"
       "M lisp:/defmethod %invert-change (change t)"
       "M lisp:/defun %partition/2"
       "M lisp:/defun %partition/3"
       "M lisp:/defun %partition/4"
       "M lisp:/defmethod three-way-diff (diff-engine t t t)/3"
       "M lisp:/defmethod rebase-diff (diff diff t)")))
  (ok (string= (format nil "~{~A~%~}" expected) (summary (hist a) (hist b)))))

(deftest history-form-level-summary
  (ok (string= (summary (hist "edit-1") (hist "edit-2") :summary)
               (format nil "~{~A~%~}" '("A lisp:/defun line-start"
                                        "A lisp:/defun comment-line-p"
                                        "A lisp:/defun leading-comment-start"
                                        "M lisp:/defun delete-node"
                                        "M lisp:/defun transpose-nodes"
                                        "M lisp:/defun move-node")))))

(deftest history-compose-chains
  (dolist (chain '(("indent-0" "indent-1" "indent-2") ("edit-0" "edit-1" "edit-2")))
    (destructuring-bind (a b c) (mapcar #'hist chain)
      (ok (string= c (d:apply-diff a (d:compose-diffs (diff a b) (diff b c)))))
      (ok (string= a (d:apply-diff c (d:invert-diff (d:compose-diffs (diff a b) (diff b c)))))))))

(deftest history-concurrent-merge
  (testing "PR #6 and PR #7 of lisp-source-protocol, both from indent-0: a clean merge equal to the real indent-2"
    (multiple-value-bind (text cs) (merged (hist "indent-0") (hist "indent-1") (hist "indent-theirs"))
      (ok (null cs))
      (ok (string= text (hist "indent-2"))))
    (ok (string= (d:render-diff (d:three-way-diff (engine) (hist "indent-0") (hist "indent-1") (hist "indent-theirs"))
                                :summary nil)
                 (format nil "~{~A~%~}" '("A lisp:/defparameter *method-option-operators*"
                                          "A lisp:/defun bare-operator-name"
                                          "M lisp:/defvar *indentation*"
                                          "M lisp:/defparameter +standard-indentation+/2/0"
                                          "M lisp:/defun line-indent-for/4/2/3/2"
                                          "M lisp:/defun scan-indent-line/4/3/2/1/4/2")))))
  (testing "rebasing PR #7 onto PR #6 gives the same file"
    (let* ((base (hist "indent-0"))
           (onto-state (hist "indent-1"))
           (r (d:rebase-diff (diff base (hist "indent-theirs")) (diff base onto-state) base)))
      (ok (string= (hist "indent-2") (d:apply-diff onto-state r)))))
  (testing "a real edit of wrap-node against a comment added above it: one conflict on that form"
    (let* ((base (hist "edit-0"))
           (theirs (replace-once base "(defun wrap-node" (format nil ";; theirs~%(defun wrap-node"))))
      (multiple-value-bind (text cs) (merged base (hist "edit-1") theirs)
        (ok (string= text base))
        (ok (equal (conflict-footprints cs) '("lisp:/defun wrap-node")))))))
