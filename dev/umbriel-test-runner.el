;;; umbriel-test-runner.el --- Run the umbriel ERT suites -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Lee

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Batch runner for `just test':
;;
;;   emacs -Q --batch -L umbriel -L umbriel/test -L dev \
;;     -l umbriel-test-runner -f umbriel-test-runner-run-batch-and-exit
;;
;; Loads every `*-test.el' under umbriel/test/, prints the ERT summary,
;; and exits non-zero on any unexpected result, any load error, or when no
;; test was loaded at all (an empty run must not report green).
;;
;; The suite directory is resolved from this file's location, not from
;; `default-directory'.
;;
;; This feature is deliberately not named `umbriel-test': that is the
;; client suite's own feature, and a runner skipping already-provided
;; features would then silently skip the client suite.

;;; Code:

;; `just check' must never test stale bytecode: `require' prefers an older
;; `.elc' to a newer `.el' while this is nil.  Set it before any `require'.
(setq load-prefer-newer t)

(require 'ert)

(defconst umbriel-test-runner--repo-root
  (file-name-directory
   (directory-file-name
    (file-name-directory
     (or load-file-name buffer-file-name default-directory))))
  "Repository root, resolved from this file's location (dev/).")

(defun umbriel-test-runner--suite-files ()
  "Absolute paths of every `*-test.el' under umbriel/test/."
  (directory-files (expand-file-name "umbriel/test" umbriel-test-runner--repo-root)
                   t "-test\\.el\\'"))

(defun umbriel-test-runner--load-suites ()
  "Load every suite file; return (LOADED . ERRORS), ERRORS being strings.
A suite that an earlier suite already `require'd is not loaded again:
loading twice redefines its tests, which ERT refuses."
  (let ((loaded 0)
        errors)
    (dolist (file (umbriel-test-runner--suite-files))
      (unless (featurep (intern (file-name-base file)))
        (condition-case err
            (progn (load file nil t) (setq loaded (1+ loaded)))
          (error (push (format "%s: %s" (file-name-base file)
                               (error-message-string err))
                       errors)))))
    (cons loaded (nreverse errors))))

(defun umbriel-test-runner-run-batch ()
  "Load and run the suites; return a one-line summary string starting PASS or FAIL."
  (ert-delete-all-tests)
  (pcase-let* ((`(,loaded . ,errors) (umbriel-test-runner--load-suites))
               (stats (ert-run-tests-batch t))
               (total (ert-stats-total stats))
               (unexpected (ert-stats-completed-unexpected stats)))
    (message "umbriel-test-runner: %d suites loaded" loaded)
    (cond
     (errors (format "FAIL load errors: %s" (string-join errors "; ")))
     ((zerop total) "FAIL no tests loaded")
     ((> unexpected 0) (format "FAIL %d unexpected of %d" unexpected total))
     (t (format "PASS %d/%d passed (%d skipped)"
                (ert-stats-completed-expected stats) total
                (ert-stats-skipped stats))))))

(defun umbriel-test-runner-run-batch-and-exit ()
  "Run `umbriel-test-runner-run-batch', print its summary, and exit Emacs.
Exit status is 0 on PASS and 1 otherwise."
  (let ((summary (umbriel-test-runner-run-batch)))
    (message "%s" summary)
    (kill-emacs (if (string-prefix-p "PASS" summary) 0 1))))

(provide 'umbriel-test-runner)
;;; umbriel-test-runner.el ends here
