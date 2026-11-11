;;; umbriel-columns-test.el --- ert tests for umbriel column layout -*- lexical-binding: t; -*-

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

;; Run: `just check', or M-x ert RET umbriel-columns RET.
;;
;; Layout is pure, so nothing here needs a buffer.  How rows land in
;; *umbriel* is tested in umbriel-buffer-test.el.

;;; Code:

(require 'ert)
(require 'umbriel-columns)

(defun umbriel-columns-test--stops (row)
  "The `:align-to' specs on ROW, a string, left to right."
  (let ((pos 0)
        stops)
    (while (< pos (length row))
      (when-let* ((display (get-text-property pos 'display row)))
        (push (plist-get (cdr display) :align-to) stops))
      (setq pos (1+ pos)))
    (nreverse stops)))

(ert-deftest umbriel-columns-fit/cuts-only-limited-columns ()
  "A cell over its limit ends in an ellipsis; others are left alone."
  (should (equal '((a . "xxx…") (b . "xxxxxx") (c . "xx"))
                 (umbriel-columns-fit
                  '((a . "xxxxxx") (b . "xxxxxx") (c . "xx"))
                  '((a . 4) (c . 4))))))

(ert-deftest umbriel-columns-starts/measure-the-widest-cell ()
  "Each column starts a gap past the widest cell of the one before."
  (should (equal `((a . 2)
                   (b . ,(+ 2 3 umbriel-columns-gap))
                   (c . ,(+ 2 3 umbriel-columns-gap
                            1 umbriel-columns-gap)))
                 (umbriel-columns-starts '(((a . "xxx") (b . "") (c . "x"))
                                              ((a . "x") (b . "x") (c . "xxx")))
                                            2))))

(ert-deftest umbriel-columns-starts/count-display-width ()
  "A wide character counts as the columns it occupies."
  (should (= (+ 2 umbriel-columns-gap)
             (alist-get 'b (umbriel-columns-starts
                            '(((a . "漢") (b . "x"))) 0)))))

(ert-deftest umbriel-columns-starts/no-rows-no-columns ()
  "Nothing to measure gives no starts."
  (should-not (umbriel-columns-starts nil 0)))

(ert-deftest umbriel-columns-row/reads-as-a-single-spaced-line ()
  "Without its display properties a row is its cells, one space apart."
  (should (equal "foo bar"
                 (substring-no-properties
                  (umbriel-columns-row '((a . "foo") (b . "bar"))
                                          '((a . 0) (b . 9))
                                          nil)))))

(ert-deftest umbriel-columns-row/stops-count-the-buffer-font ()
  "A gap aligns in the current font's width, not the frame's."
  (should (equal '((9 . width))
                 (umbriel-columns-test--stops
                  (umbriel-columns-row '((a . "foo") (b . "bar"))
                                          '((a . 0) (b . 9))
                                          nil)))))

(ert-deftest umbriel-columns-row/skips-an-empty-cell ()
  "An empty cell takes no gap; the next cell keeps its own stop."
  (let ((row (umbriel-columns-row '((a . "foo") (b . "") (c . "bar"))
                                     '((a . 0) (b . 5) (c . 9))
                                     nil)))
    (should (equal "foo bar" (substring-no-properties row)))
    (should (equal '((9 . width)) (umbriel-columns-test--stops row)))))

(ert-deftest umbriel-columns-row/faces-go-on-the-text-not-the-gap ()
  "A cell's face covers its text; a column with no face has none."
  (let ((row (umbriel-columns-row '((a . "foo") (b . "bar"))
                                     '((a . 0) (b . 9))
                                     '((a . bold)))))
    (should (eq 'bold (get-text-property 0 'font-lock-face row)))
    (should-not (get-text-property 3 'font-lock-face row))
    (should-not (get-text-property 4 'font-lock-face row))))

(ert-deftest umbriel-columns-at-least/moves-a-column-right ()
  "A column short of POSITION moves there; the others stay."
  (should (equal '((a . 0) (b . 9))
                 (umbriel-columns-at-least '((a . 0) (b . 4)) 'b 9))))

(ert-deftest umbriel-columns-at-least/never-moves-a-column-left ()
  "A column already past POSITION stays, so its cells cannot overlap."
  (should (equal '((a . 0) (b . 4))
                 (umbriel-columns-at-least '((a . 0) (b . 4)) 'b 2))))

(ert-deftest umbriel-columns-at-least/no-position-changes-nothing ()
  "A nil POSITION, from a table with no such column, leaves STARTS alone."
  (should (equal '((a . 0) (b . 4))
                 (umbriel-columns-at-least '((a . 0) (b . 4)) 'b nil))))

;;; umbriel-columns-test.el ends here
