# diff-backend-lisp-source

A [`diff-protocol`](https://github.com/egao1980/diff-protocol) engine for Common Lisp source: top-level forms identified by kind and name (`defun foo`, `defmethod m :around (a t)`, …), form-level changes (added / removed / modified / moved), nested structural changes inside a form, and a form-level three-way merge. Built on the lossless CST of [`lisp-source-protocol`](https://github.com/egao1980/lisp-source-protocol): comments, whitespace and reader conditionals are kept byte for byte, and nothing is evaluated (`#.` included).

Brief: [cl-stack `docs/capabilities/diff-backend-lisp-source.md`](https://github.com/egao1980/cl-stack/blob/main/docs/capabilities/diff-backend-lisp-source.md) · Plan: HARNESS-PLAN Track R, milestone R3.

Status: skeleton. The contract (exported API stubs + Rove suite) lands first; the implementation follows through team-solve.
