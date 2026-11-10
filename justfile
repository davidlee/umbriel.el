load_path := "-L umbriel -L umbriel/test -L dev"

check: lint test

# Paren check, then byte-compile every .el with warnings as errors, one Emacs
# per file. Output goes to a temp dir, never into the checkout.
lint:
    #!/usr/bin/env bash
    set -euo pipefail
    out=$(mktemp -d)
    trap 'rm -rf "$out"' EXIT
    for f in umbriel/*.el umbriel/test/*.el dev/*.el; do
        bin/elisp-locate-paren-error "$f" >/dev/null
        emacs -Q --batch {{load_path}} \
          --eval "(setq byte-compile-error-on-warn t)" \
          --eval "(setq byte-compile-dest-file-function (lambda (_) \"$out/out.elc\"))" \
          -f batch-byte-compile "$f"
    done

test:
    emacs -Q --batch {{load_path}} \
      -l umbriel-test-runner -f umbriel-test-runner-run-batch-and-exit
