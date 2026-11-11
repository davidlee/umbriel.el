load_path := "-L . -L test -L dev"

check: lint test

# Paren check, then byte-compile every .el with warnings as errors, one Emacs
# per file. Output goes to a temp dir, never into the checkout.
lint:
    #!/usr/bin/env bash
    set -euo pipefail
    out=$(mktemp -d)
    trap 'rm -rf "$out"' EXIT
    for f in *.el test/*.el dev/*.el; do
        bin/elisp-locate-paren-error "$f" >/dev/null
        emacs -Q --batch {{load_path}} \
          --eval "(setq byte-compile-error-on-warn t)" \
          --eval "(setq byte-compile-dest-file-function (lambda (_) \"$out/out.elc\"))" \
          -f batch-byte-compile "$f"
    done
    just package-lint checkdoc

# The archive rules (headers, prefixes, versions) for the library files.
# It checks Package-Requires against the packages Emacs knows; where -Q
# leaves none known (CI), UMBRIEL_PACKAGE_DIR names a package dir to know.
package-lint:
    emacs -Q --batch -L . \
      --eval "(when-let* ((dir (getenv \"UMBRIEL_PACKAGE_DIR\"))) (setq package-user-dir dir) (package-initialize))" \
      --eval "(require 'package-lint)" \
      --eval '(setq package-lint-main-file "umbriel.el")' \
      -f package-lint-batch-and-exit umbriel*.el

# Docstring and comment style for the library files. checkdoc reports
# without failing, so any `FILE:LINE:` report fails the recipe. The
# experimental imperative-verb check is off: docstrings here describe what
# a function returns ("The face that marks ..."), which it misreads, and
# Emacs 30 and 32 disagree on it.
checkdoc:
    #!/usr/bin/env bash
    set -euo pipefail
    report=$(for f in umbriel*.el; do
        emacs -Q --batch -L . \
          --eval "(setq checkdoc-verb-check-experimental-flag nil)" \
          --eval "(checkdoc-file \"$f\")" 2>&1
    done | grep -E '^[^ ]+\.el:[0-9]+: ' || true)
    if [ -n "$report" ]; then echo "$report"; exit 1; fi

test:
    emacs -Q --batch {{load_path}} \
      -l umbriel-test-runner -f umbriel-test-runner-run-batch-and-exit
