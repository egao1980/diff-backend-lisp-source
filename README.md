# diff-backend-lisp-source

A [`diff-protocol`](https://github.com/egao1980/diff-protocol) engine (`:lisp`, package `stack-diff-lisp`) for Common Lisp source: top-level forms identified like lisp-mcp locators (`defun foo`, `defmethod m :around (c t)`, `#+sbcl defun f`), form-level changes (add / remove / modify / move, renames), nested structural changes inside a form, and a form-level three-way merge: edits to different forms merge cleanly, two different edits to one form conflict. Built on the lossless CST of [`lisp-source-protocol`](https://github.com/egao1980/lisp-source-protocol): comments, whitespace and reader conditionals are kept byte for byte, and nothing is evaluated (`#.` included).

```lisp
(asdf:load-system "diff-backend-lisp-source")

(let ((e (diff-protocol:find-diff-engine :lisp)))
  (diff-protocol:render-diff
   (diff-protocol:compute-diff e "(defun a (x) (+ x 1))
" "(defun a (x) (+ x 2))
(defun b () t)
")
   :summary nil))
;; => "A lisp:/defun b
;;     M lisp:/defun a/3/2
;;     "
```

Brief: [cl-stack `docs/capabilities/diff-backend-lisp-source.md`](https://github.com/egao1980/cl-stack/blob/main/docs/capabilities/diff-backend-lisp-source.md) · Plan: HARNESS-PLAN Track R, milestone R3.

**Status:** R3 contract — exports, conditions, engine class and `%not-implemented` stubs, plus the Rove suite `diff-backend-lisp-source/tests` (with real files from the workspace's git history as fixtures). The suite fails until the implementation lands through team-solve (`.team-solve/task.md`).

Tests: `cl-repo test`.
