# AGENTS.md

Emacs Lisp package: an Emacs client for the umbriel Wayland compositor.

## Layout

- `umbriel/` library: `umbriel.el` (socket client), `-model`, `-columns` (pure),
  `-buffer` (UI), `-focus`. `-model` and `-columns` must not require the client.
- `umbriel/test/` ERT suites; `dev/umbriel-test-runner.el` runs them.
- `docs/umbriel.md` design and usage notes.

## Gate

`just check` (inside `nix develop`) is green: lint (paren check, byte-compile
with warnings as errors, no `.elc` written to the checkout) and tests.

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
