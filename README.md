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

**Status:** 0.2.1 (diff-protocol 0.3.0): `:lisp` files inside a tree diff render as `@@` hunks in `render-diff … :unified` (`render-unified-content`; at `:changes` level, where the tree diff carries no file contents, one section per change headed by its footprint), and the engine registers through the exported `register-diff-engine` / hook API (diff-backend-lisp-source#3).

0.2.0: R3 implemented, plus (diff-backend-lisp-source#2):

- Anonymous top-level forms (`setf`, `eval-when`, … keyed operator + ordinal) are matched by content: inserting one before others is one `:add`, not a run of `:modify`s; a merge where one side inserts and the other edits a later one is clean. Change keys are the ordinal keys of the state each change applies to.
- A top-level `(eof …)` form keys as `eof #2` (`eof #3`, …): `"eof"` is always the trailer.
- `:modify` sides record anchors (the unit's predecessor), so `use-value` on a modify whose unit is gone puts the text at its old position.
- A `:move`'s before anchor is the unit right before it when it moves, so an inverse retraces the forward states exactly.
- `apply-diff` keeps the units a change placed for the next change of the same apply (a unit with no final newline in front of a comment-led unit no longer re-cuts mid-apply).

Tests: `cl-repo test`.
