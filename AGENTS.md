# AGENTS.md

Emacs Lisp package: an Emacs client for the umbriel Wayland compositor.

## Layout

- Library at the root: `umbriel.el` (entry; loads the rest), `-ipc` (socket
  client), `-model`, `-columns` (pure), `-buffer` (UI), `-focus`. `-model`
  and `-columns` must not require the client.
- `test/` ERT suites; `dev/umbriel-test-runner.el` runs them.
- `.elpaignore` keeps non-library files out of package installs.
- `docs/umbriel.md` design and usage notes.

## The compositor socket

The session shell can reach the live umbriel socket. Read-only probes
(queries) are fine. Mutating actions (focus, kill, switch, layout) are the
user's to take. Sandboxed (jailed) workers lack the socket.

## Design history

The decisions behind this code (DEC-016..031, backlog IDE-003) are records in
the author's emacs-config repo. Code and docs here state the reasoning
directly and do not cite them.

## Gate

`just check` (inside `nix develop`) is green: lint (paren check, byte-compile
with warnings as errors, no `.elc` written to the checkout, package-lint,
checkdoc) and tests.

## Elisp edits

1. Never manually balance parentheses across a whole file.
2. After changing any .el file, run `bin/elisp-locate-paren-error FILE`.
3. If it fails: inspect `error.line`, then the first item in `open_stack`;
   restrict repairs to the reported line or `toplevel.start_line..toplevel.end_line`.
4. Re-run until it returns `{"ok":true}`, then byte-compile and test.

## Naming

Public symbols are `umbriel-...`; private ones `umbriel--...` (or
`umbriel-FILE--...`). The package defines no keybindings: keys belong to the
user's config.
