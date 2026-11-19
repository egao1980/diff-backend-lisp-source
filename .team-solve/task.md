---
system: diff-backend-lisp-source
test-system: diff-backend-lisp-source/tests
brief: ../cl-stack/docs/capabilities/diff-backend-lisp-source.md
---

# R3: diff-backend-lisp-source — form-level diff and three-way merge of Lisp source

Implement the `:lisp` diff-protocol engine of the brief (all of "Locked decisions",
"Units", "Form keys", "Changes", "Nested changes", "Levels", "Apply", "Three-way
merge", "Render", "Under the tree engine", "Conditions and restarts") so that
`(asdf:test-system "diff-backend-lisp-source")` passes.

Starting point: `src/package.lisp` (exports, final), `src/conditions.lisp`
(`lisp-source-unreadable`, `use-text-diff`, final), `src/engine.lisp` (the
`lisp-source-diff-engine` class and `%not-implemented` stubs). Add source files as
needed and list them in the `.asd`; keep the exports, the dependency set
(`diff-protocol` >= 0.2.0, `lisp-source-protocol`) and version 0.1.0.

Work items:

1. Reading through `lisp-source-protocol:read-source` with `:features '()`; the
   clean-read check and `lisp-source-unreadable` (diagnostics + imbalances).
2. Units, keys (guards, methods, ordinals, `"eof"`), `source-units`.
3. `compute-diff` at `:summary` / `:changes` / `:content`: adds, removes, modifies,
   reorder moves (Myers on key lines through the diff-protocol `:text` engine),
   renames, anchors, change order, nested leaves (same-shape rule), footprints with
   escaping; `state-equal`; `use-text-diff` / `use-value` restarts.
4. Registration under `:lisp` and the diff-protocol engine hooks
   (`diff-protocol::%apply-change`, `%put-value`, `%three-way`, `%rebase`) so that
   `apply-diff` (with its restarts), `invert-diff`, `compose-diffs`, `rebase-diff`,
   `parse-diff :sexp` and the tree engine work.
5. `three-way-diff` (form-level merge, twins, pure reorder + modify, skeleton and
   runs, conflicts as data) and `rebase-diff` (conflicts resolved to onto).
6. `unified-patch`.

The real-history fixtures in `tests/fixtures/history` are bytes from git; do not
re-save them. The suite was checked against a throwaway prototype before sealing:
every assertion is satisfiable with the rules exactly as the brief states them.
