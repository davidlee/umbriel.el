;;; umbriel-model-test.el --- ert tests for the pure umbriel model -*- lexical-binding: t; -*-

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

;; Run: `just check', or M-x ert RET umbriel-model/ RET.
;;
;; The model is pure: hand-written alists in the shape `umbriel-ipc' parses
;; in, data or strings out.  No test reaches a real compositor, and every
;; title, app id and name below is invented.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'umbriel-model)

;;;; Fixtures

(defconst umbriel-model-test--unset :unset
  "Override value that removes a field from a fixture entry entirely.")

(defun umbriel-model-test--entry (defaults overrides)
  "Return a copy of DEFAULTS with the plist OVERRIDES applied.
OVERRIDES are keywords (`:id') naming the alist symbols they replace, so
they cannot be confused with a fixture's own values.  A value of
`umbriel-model-test--unset' removes that key instead."
  (let ((entry (copy-tree defaults)))
    (while overrides
      (let ((key (intern (substring (symbol-name (pop overrides)) 1)))
            (value (pop overrides)))
        (setq entry (assq-delete-all key entry))
        (unless (eq value umbriel-model-test--unset)
          (push (cons key value) entry))))
    entry))

(defconst umbriel-model-test--workspace-defaults
  '((id . "DP-1:1") (name . "web") (named . t) (index . 1) (output . "DP-1")
    (active . nil) (focused . nil) (occupied . t) (window_count . 0)
    (focused_window . nil) (urgent . nil) (layout . "dwindle")
    (layout_override . nil))
  "A workspace entry as the compositor snapshot delivers it.")

(defconst umbriel-model-test--window-defaults
  '((id . "w-1") (workspace . "DP-1:1") (scratchpad . "") (app_id . "foot")
    (title . "shell") (floating . nil) (column . 0) (row . 0) (tabbed . nil)
    (tab_index . -1) (active . nil) (focused . nil) (visible . nil)
    (minimized . nil) (fullscreen . nil) (urgent . nil) (xwayland . nil)
    (pid . 4242))
  "A window entry as the compositor snapshot delivers it.")

(defun umbriel-model-test--workspace (&rest overrides)
  "A workspace fixture with OVERRIDES applied."
  (umbriel-model-test--entry umbriel-model-test--workspace-defaults overrides))

(defun umbriel-model-test--window (&rest overrides)
  "A window fixture with OVERRIDES applied."
  (umbriel-model-test--entry umbriel-model-test--window-defaults overrides))

(defun umbriel-model-test--windows (groups)
  "Every window held in GROUPS, flattened."
  (apply #'append (mapcar #'cdr groups)))

(defun umbriel-model-test--ids (windows)
  "The `id' of each of WINDOWS."
  (mapcar (lambda (window) (alist-get 'id window)) windows))

(defun umbriel-model-test--word-p (string word)
  "Non-nil when STRING contains WORD as a whole word."
  (and (stringp string)
       (string-match-p (concat "\\_<" (regexp-quote word) "\\_>") string)))

(defun umbriel-model-test--label (value)
  "Assert VALUE is a display string and return it for further checks.
A non-string fails the test as an assertion instead of erroring below."
  (should (stringp value))
  (if (stringp value) value ""))

;; The purity test's subprocess needs the directory holding the model, found the
;; way `require' found it above, not from `default-directory'.
(defvar umbriel-model-test--model-dir
  (file-name-directory (locate-library "umbriel-model"))
  "The directory holding `umbriel-model'.")

;;;; group

(ert-deftest umbriel-model-window-scratchpad/names-only-a-real-pad ()
  "A non-empty string is a pad; anything else is none."
  (should (equal "signal" (umbriel-model-window-scratchpad
                           (umbriel-model-test--window :scratchpad "signal"))))
  (dolist (value (list "" nil 3 umbriel-model-test--unset))
    (should-not (umbriel-model-window-scratchpad
                 (umbriel-model-test--window :scratchpad value)))))

(ert-deftest umbriel-model-pad-names/names-each-held-pad-once-in-order ()
  "A pad counts once however many windows it holds; a plain window names none."
  (should (equal '("discord" "signal")
                 (umbriel-model-pad-names
                  (list (umbriel-model-test--window :scratchpad "signal")
                        (umbriel-model-test--window :scratchpad "discord")
                        (umbriel-model-test--window :scratchpad "signal")
                        (umbriel-model-test--window))))))

(ert-deftest umbriel-model-group/pads-sit-between-workspaces-and-unassigned ()
  "Pads by name after the workspaces; a shown pad window stays in its pad."
  (let* ((held (umbriel-model-test--workspace))
         (joined (umbriel-model-test--window :id "w-1"))
         (shown (umbriel-model-test--window :id "w-2" :scratchpad "signal"))
         (hidden (umbriel-model-test--window :id "w-3" :workspace ""
                                                :scratchpad "discord"))
         (mate (umbriel-model-test--window :id "w-4" :workspace ""
                                              :scratchpad "signal"))
         (orphan (umbriel-model-test--window :id "w-5" :workspace ""))
         (windows (list orphan mate shown hidden joined))
         (groups (umbriel-model-group (list held) windows)))
    (should (equal '(nil "discord" "signal" nil)
                   (mapcar (lambda (group)
                             (alist-get 'scratchpad (car group)))
                           groups)))
    (should (eq held (car (nth 0 groups))))
    (should (equal '("w-1") (umbriel-model-test--ids (cdr (nth 0 groups)))))
    (should (equal '("w-3") (umbriel-model-test--ids (cdr (nth 1 groups)))))
    (should (seq-set-equal-p '("w-2" "w-4")
                             (umbriel-model-test--ids (cdr (nth 2 groups)))))
    (should (equal '("w-5") (umbriel-model-test--ids (cdr (nth 3 groups)))))
    (should (= 5 (length (umbriel-model-test--windows groups))))))

(ert-deftest umbriel-model-group-cells/gives-a-pad-heading-cells ()
  "A pad's cells have the workspace's columns, layout and marks empty."
  (let ((cells (umbriel-model-group-cells '((scratchpad . "signal")) 2)))
    (should (equal "scratchpad" (alist-get 'id cells)))
    (should (equal "signal" (alist-get 'name cells)))
    (should (equal "2 windows" (alist-get 'windows cells)))
    (should (equal "" (alist-get 'layout cells)))
    (should (equal "" (alist-get 'marks cells)))))

(ert-deftest umbriel-model-group-cells/gives-a-workspace-its-own-cells ()
  "A workspace owner's cells are `umbriel-model-workspace-cells'."
  (let ((workspace (umbriel-model-test--workspace)))
    (should (equal (umbriel-model-workspace-cells workspace 2)
                   (umbriel-model-group-cells workspace 2)))))

(ert-deftest umbriel-model-group/keeps-every-window ()
  "No window is dropped, whatever its `workspace' field says.
Scratchpad (\"\"), nil, absent and unknown ids all land in the final
Unassigned group, which is last."
  (let* ((held (umbriel-model-test--workspace :id "DP-1:1" :name "web" :index 1))
         (empty (umbriel-model-test--workspace :id "HDMI-A-1:3" :name nil :named nil
                                                  :index 3 :output "HDMI-A-1"))
         (joined (umbriel-model-test--window :id "w-1" :app_id "foot"
                                                :title "shell"))
         (scratch (umbriel-model-test--window :id "w-2" :workspace ""
                                                 :app_id "pavucontrol"
                                                 :title "Volume" :floating t))
         (layer (umbriel-model-test--window :id "w-3" :workspace nil
                                               :app_id "swaybg" :title "wallpaper"))
         (absent (umbriel-model-test--window :id "w-4"
                                                :workspace umbriel-model-test--unset
                                                :app_id "emacs" :title "notes.org"))
         (ghost (umbriel-model-test--window :id "w-5" :workspace "DP-9:9"
                                               :app_id "firefox" :title "Docs"))
         (workspaces (list empty held))
         (windows (list scratch joined ghost absent layer))
         (snapshot (copy-tree windows))
         (groups (umbriel-model-group workspaces windows)))
    (should (= 3 (length groups)))
    (should (eq held (car (nth 0 groups))))
    (should (equal (list joined) (cdr (nth 0 groups))))
    (should (eq empty (car (nth 1 groups))))
    (should-not (cdr (nth 1 groups)))
    (should-not (car (car (last groups))))
    (should (equal '("w-5" "w-4" "w-3" "w-2")
                   (umbriel-model-test--ids (cdr (car (last groups))))))
    (dolist (window windows)
      (should (= 1 (seq-count (lambda (other) (eq other window))
                              (umbriel-model-test--windows groups)))))
    (should (= (length windows)
               (length (umbriel-model-test--windows groups))))
    (should (equal snapshot windows))
    (should (equal (list empty held) workspaces))))

(ert-deftest umbriel-model-group/omits-unassigned-when-every-window-joins ()
  "No empty Unassigned group, and it is last when it exists."
  (let* ((held (umbriel-model-test--workspace :id "DP-1:1"))
         (tiled (umbriel-model-test--window :id "w-1" :column 0 :row 0))
         (floating (umbriel-model-test--window :id "w-2" :floating t
                                                  :title "Docs"))
         (groups (umbriel-model-group (list held) (list floating tiled))))
    (should (= 1 (length groups)))
    (should (eq held (car (car groups))))
    (should (equal (list tiled floating) (cdr (car groups))))
    (should-not (seq-find (lambda (pair) (null (car pair))) groups))))

(ert-deftest umbriel-model-group/workspaces-ordered-by-output-then-index ()
  "DP-1:2, HDMI-A-1:3, DP-1:1 in input order come out DP-1:1, DP-1:2, HDMI-A-1:3."
  (let* ((dp2 (umbriel-model-test--workspace :id "DP-1:2" :name "notes"
                                                :index 2 :output "DP-1"))
         (hdmi (umbriel-model-test--workspace :id "HDMI-A-1:3" :name nil
                                                 :named nil :index 3
                                                 :output "HDMI-A-1"))
         (dp1 (umbriel-model-test--workspace :id "DP-1:1" :name "web"
                                                :index 1 :output "DP-1"))
         (groups (umbriel-model-group (list dp2 hdmi dp1) nil)))
    (should (= 3 (length groups)))
    (should (eq dp1 (car (nth 0 groups))))
    (should (eq dp2 (car (nth 1 groups))))
    (should (eq hdmi (car (nth 2 groups))))
    (should-not (umbriel-model-test--windows groups))))

(ert-deftest umbriel-model-group/windows-ordered-by-column-then-row ()
  "Tiled before floating, `-1' after placed columns and rows, floating by title."
  (let* ((held (umbriel-model-test--workspace :id "DP-1:1"))
         (col1 (umbriel-model-test--window :id "w-col1" :column 1 :row 0
                                              :title "second"))
         (bare (umbriel-model-test--window :id "w-bare" :column -1 :row -1
                                              :title "alpha"))
         (row1 (umbriel-model-test--window :id "w-row1" :column 0 :row 1
                                              :title "lower"))
         (col0 (umbriel-model-test--window :id "w-col0" :column 0 :row 0
                                              :title "zulu"))
         (float-z (umbriel-model-test--window :id "w-fz" :floating t
                                                 :column -1 :row -1
                                                 :title "zsh"))
         (float-a (umbriel-model-test--window :id "w-fa" :floating t
                                                 :column -1 :row -1
                                                 :title "firefox"))
         (windows (list float-z col1 bare float-a row1 col0))
         (groups (umbriel-model-group (list held) windows)))
    (should (equal '("w-col0" "w-row1" "w-col1" "w-bare" "w-fa" "w-fz")
                   (umbriel-model-test--ids (cdr (car groups)))))))

(ert-deftest umbriel-model-group/tab-index-sorts-before-title ()
  "Same column and row: `tab_index' decides, `-1' after placed tabs, then title."
  (let* ((held (umbriel-model-test--workspace :id "DP-1:1"))
         (tab0 (umbriel-model-test--window :id "w-tab0" :column 0 :row 0
                                              :tabbed t :tab_index 0
                                              :title "zebra"))
         (tab1 (umbriel-model-test--window :id "w-tab1" :column 0 :row 0
                                              :tabbed t :tab_index 1
                                              :title "apple"))
         (plain (umbriel-model-test--window :id "w-plain" :column 0 :row 0
                                               :tab_index -1 :title "aardvark"))
         (groups (umbriel-model-group (list held) (list plain tab1 tab0))))
    (should (equal '("w-tab0" "w-tab1" "w-plain")
                   (umbriel-model-test--ids (cdr (car groups)))))))

(ert-deftest umbriel-model-group/workspace-order-ignores-the-id-serial ()
  "Two workspaces on one output sort by `index', not by the id's serial."
  (let* ((serial9 (umbriel-model-test--workspace :id "DP-1:9" :name "web"
                                                    :index 2 :output "DP-1"))
         (serial3 (umbriel-model-test--workspace :id "DP-1:3" :name "notes"
                                                    :index 10 :output "DP-1"))
         (hdmi (umbriel-model-test--workspace :id "HDMI-A-1:3" :name nil
                                                 :named nil :index 3
                                                 :output "HDMI-A-1"))
         (groups (umbriel-model-group (list serial3 hdmi serial9) nil)))
    (should (eq serial9 (car (nth 0 groups))))
    (should (eq serial3 (car (nth 1 groups))))
    (should (eq hdmi (car (nth 2 groups))))))

(ert-deftest umbriel-model-group/floating-ordered-by-title-only ()
  "Floating windows ignore column and row and sort by `title' alone."
  (let* ((held (umbriel-model-test--workspace :id "DP-1:1"))
         (zzz (umbriel-model-test--window :id "w-zzz" :floating t
                                             :column 1 :row 0 :title "zzz"))
         (aaa (umbriel-model-test--window :id "w-aaa" :floating t
                                             :column -1 :row -1 :title "aaa"))
         (mmm (umbriel-model-test--window :id "w-mmm" :floating t
                                             :column 0 :row 0 :title "mmm"))
         (groups (umbriel-model-group (list held) (list zzz aaa mmm))))
    (should (equal '("w-aaa" "w-mmm" "w-zzz")
                   (umbriel-model-test--ids (cdr (car groups)))))))

(ert-deftest umbriel-model-group/ties-keep-input-order ()
  "Windows equal on every sort field keep their input order."
  (let* ((held (umbriel-model-test--workspace :id "DP-1:1"))
         (first (umbriel-model-test--window :id "w-first" :column 0 :row 0
                                               :tab_index 0 :title "same"))
         (second (umbriel-model-test--window :id "w-second" :column 0 :row 0
                                                :tab_index 0 :title "same"))
         (third (umbriel-model-test--window :id "w-third" :column 0 :row 0
                                               :tab_index 0 :title "same"))
         (groups (umbriel-model-group (list held)
                                         (list first second third))))
    (should (equal '("w-first" "w-second" "w-third")
                   (umbriel-model-test--ids (cdr (car groups)))))))

(ert-deftest umbriel-model-workspace-selector/named-selects-the-name ()
  "A named workspace is addressed by its name string, digits included."
  (let* ((named (umbriel-model-workspace-selector
                 (umbriel-model-test--workspace :id "DP-1:1" :name "web"
                                                   :named t :index 1
                                                   :output "DP-1")))
         (digits (umbriel-model-workspace-selector
                  (umbriel-model-test--workspace :id "DP-1:7" :name "2"
                                                    :named t :index 7
                                                    :output "DP-1"))))
    (should (equal "web" (alist-get 'workspace named)))
    (should (equal "DP-1" (alist-get 'output named)))
    (should (stringp (alist-get 'workspace digits)))
    (should (equal "2" (alist-get 'workspace digits)))
    (should (equal "DP-1" (alist-get 'output digits)))))

(ert-deftest umbriel-model-workspace-selector/name-is-not-flattened ()
  "A selector carries the raw `name': flattening is a display rule only."
  (let ((selector (umbriel-model-workspace-selector
                   (umbriel-model-test--workspace :id "DP-1:1"
                                                     :name "my\tws" :named t
                                                     :index 1
                                                     :output "DP-1"))))
    (should (equal "my\tws" (alist-get 'workspace selector)))))

(ert-deftest umbriel-model-workspace-selector/unnamed-selects-the-index ()
  "An unnamed workspace is addressed by its index number, with its output."
  (let ((selector (umbriel-model-workspace-selector
                   (umbriel-model-test--workspace :id "HDMI-A-1:4"
                                                     :name nil :named nil
                                                     :index 4
                                                     :output "HDMI-A-1"))))
    (should (equal 4 (alist-get 'workspace selector)))
    (should (integerp (alist-get 'workspace selector)))
    (should (equal "HDMI-A-1" (alist-get 'output selector)))))

(ert-deftest umbriel-model-workspace-selector/output-is-always-present ()
  "A workspace with no `output' field still yields an output entry."
  (let ((selector (umbriel-model-workspace-selector
                   (umbriel-model-test--workspace
                    :output umbriel-model-test--unset))))
    (should (assq 'output selector))
    (should (equal "" (alist-get 'output selector)))))

(ert-deftest umbriel-model-window-workspace/joins-by-id ()
  "A joining window returns its workspace entry itself; others return nil."
  (let* ((held (umbriel-model-test--workspace :id "DP-1:1"))
         (other (umbriel-model-test--workspace :id "HDMI-A-1:3" :index 3
                                                  :output "HDMI-A-1"))
         (joined (umbriel-model-test--window :id "w-1" :workspace "DP-1:1"))
         (elsewhere (umbriel-model-test--window :id "w-2" :workspace "HDMI-A-1:3"))
         (scratch (umbriel-model-test--window :id "w-3" :workspace ""))
         (layer (umbriel-model-test--window :id "w-4" :workspace nil))
         (ghost (umbriel-model-test--window :id "w-5" :workspace "DP-9:9"))
         (workspaces (list other held)))
    (should (eq held (umbriel-model-window-workspace workspaces joined)))
    (should (eq other (umbriel-model-window-workspace workspaces elsewhere)))
    (should-not (umbriel-model-window-workspace workspaces scratch))
    (should-not (umbriel-model-window-workspace workspaces layer))
    (should-not (umbriel-model-window-workspace workspaces ghost))
    (should-not (umbriel-model-window-workspace workspaces nil))))

(ert-deftest umbriel-model-window-workspace/joins-by-exact-id ()
  "The join compares raw ids; a near-miss id must not capture the window."
  (let* ((near (umbriel-model-test--workspace :id "DP-1:1\n"))
         (exact (umbriel-model-test--workspace :id "DP-1:1"))
         (window (umbriel-model-test--window :id "w-1"
                                                :workspace "DP-1:1"))
         ;; Only flattening would make these two ids equal.
         (tabbed (umbriel-model-test--workspace :id "DP-1\t1"))
         (spaced (umbriel-model-test--workspace :id "DP-1 1"))
         (other (umbriel-model-test--window :id "w-2"
                                               :workspace "DP-1 1")))
    (should (eq exact (umbriel-model-window-workspace (list near exact)
                                                         window)))
    (should (eq spaced (umbriel-model-window-workspace (list tabbed spaced)
                                                          other)))))

(ert-deftest umbriel-model-workspace-choices/labels-in-display-order ()
  "`(LABEL . WORKSPACE)' pairs in display order, labels are strings."
  (let* ((dp2 (umbriel-model-test--workspace :id "DP-1:2" :name "notes"
                                                :index 2 :output "DP-1"))
         (hdmi (umbriel-model-test--workspace :id "HDMI-A-1:3" :name nil
                                                 :named nil :index 3
                                                 :output "HDMI-A-1"))
         (dp1 (umbriel-model-test--workspace :id "DP-1:1" :name "web"
                                                :index 1 :output "DP-1"))
         (choices (umbriel-model-workspace-choices (list dp2 hdmi dp1))))
    (should (= 3 (length choices)))
    (should (cl-every #'stringp (mapcar #'car choices)))
    (should (eq dp1 (cdr (nth 0 choices))))
    (should (eq dp2 (cdr (nth 1 choices))))
    (should (eq hdmi (cdr (nth 2 choices))))))

(ert-deftest umbriel-model-workspace-choices/labels-carry-no-count ()
  "A choice label is the count-less label; the wire count never appears."
  (let* ((busy (umbriel-model-test--workspace :id "DP-1:1" :name "web"
                                                :index 1 :output "DP-1"
                                                :window_count 7))
         (choices (umbriel-model-workspace-choices (list busy)))
         (label (car (car choices))))
    (should (= 1 (length choices)))
    (should (equal (umbriel-model-workspace-label busy) label))
    (should-not (umbriel-model-test--word-p label "window"))
    (should-not (string-match-p "7" label))))

;;;; window label

(ert-deftest umbriel-model-window-label/active-and-focused-are-independent ()
  "Seat-global `active' and the workspace's remembered `focused' are separate."
  (let ((active-only (umbriel-model-window-label
                      (umbriel-model-test--window :active t :focused nil)))
        (focused-only (umbriel-model-window-label
                       (umbriel-model-test--window :active nil :focused t)))
        (both (umbriel-model-window-label
               (umbriel-model-test--window :active t :focused t))))
    (should (umbriel-model-test--word-p active-only "active"))
    (should-not (umbriel-model-test--word-p active-only "focused"))
    (should (umbriel-model-test--word-p focused-only "focused"))
    (should-not (umbriel-model-test--word-p focused-only "active"))
    (should (umbriel-model-test--word-p both "active"))
    (should (umbriel-model-test--word-p both "focused"))))

(ert-deftest umbriel-model-window-label/visible-is-not-on-screen ()
  "`visible' is the flag word; \"on screen\" never appears."
  (let ((visible (umbriel-model-window-label
                  (umbriel-model-test--window :visible t)))
        (hidden (umbriel-model-window-label
                 (umbriel-model-test--window :visible nil))))
    (should (umbriel-model-test--word-p visible "visible"))
    (should-not (string-match-p "on screen" visible))
    (should-not (string-match-p "on screen" hidden))
    (should-not (umbriel-model-test--word-p hidden "visible"))))

(ert-deftest umbriel-model-window-label/placement-tokens ()
  "`tile C,R', `float', `tab I', and bare `tile' for column -1."
  (let ((tiled (umbriel-model-test--label
                (umbriel-model-window-label
                 (umbriel-model-test--window :floating nil :tabbed nil
                                                :column 0 :row 1))))
        (floating (umbriel-model-test--label
                   (umbriel-model-window-label
                    (umbriel-model-test--window :floating t :column 3 :row 0))))
        (tabbed (umbriel-model-test--label
                 (umbriel-model-window-label
                  (umbriel-model-test--window :tabbed t :tab_index 2
                                                 :column 0 :row 0))))
        (bare (umbriel-model-test--label
               (umbriel-model-window-label
                (umbriel-model-test--window :column -1 :row -1)))))
    (should (string-match-p "tile 0,1" tiled))
    (should (umbriel-model-test--word-p floating "float"))
    (should-not (umbriel-model-test--word-p floating "tile"))
    (should (string-match-p "tab 2" tabbed))
    (should-not (umbriel-model-test--word-p tabbed "tile"))
    (should (umbriel-model-test--word-p bare "tile"))
    (should-not (string-match-p "tile -1" bare))
    (should-not (string-match-p "," bare))))

(ert-deftest umbriel-model-window-label/flags-follow-the-contract-order ()
  "Flags appear active, focused, visible, minimized, fullscreen, urgent, xwayland."
  (let* ((label (umbriel-model-test--label
                 (umbriel-model-window-label
                  (umbriel-model-test--window :active t :focused t :visible t
                                                 :minimized t :fullscreen t
                                                 :urgent t :xwayland t))))
         (positions (mapcar (lambda (word)
                              (let ((position (string-match
                                               (concat "\\_<" (regexp-quote word)
                                                       "\\_>")
                                               label)))
                                (should position)
                                position))
                            '("active" "focused" "visible" "minimized"
                              "fullscreen" "urgent" "xwayland"))))
    (should (equal positions (sort (copy-sequence positions) #'<)))))

(ert-deftest umbriel-model-window-label/part-order ()
  "The label reads the app id, then the title, then the placement."
  (let* ((label (umbriel-model-test--label
                 (umbriel-model-window-label
                  (umbriel-model-test--window :app_id "kitty"
                                                 :title "Inbox"
                                                 :column 2 :row 3))))
         (positions (mapcar (lambda (part)
                              (let ((position (string-match (regexp-quote part)
                                                            label)))
                                (should position)
                                position))
                            '("kitty" "Inbox" "tile 2,3"))))
    (should (equal positions (sort (copy-sequence positions) #'<)))))

(ert-deftest umbriel-model-window-label/flattens-compositor-strings ()
  "Newlines, carriage returns and tabs from the compositor become spaces."
  (let ((label (umbriel-model-test--label
                (umbriel-model-window-label
                 (umbriel-model-test--window :app_id "fo\not"
                                                :title "a\nb\tc\rd")))))
    (should-not (string-match-p "[\n\t\r]" label))
    (should (string-match-p "fo ot" label))
    (should (string-match-p "a b c d" label))))

(ert-deftest umbriel-model-window-label/missing-fields-default ()
  "An empty or partial entry yields a string instead of signalling."
  (should (stringp (umbriel-model-window-label nil)))
  (should (stringp (umbriel-model-window-label '())))
  (should (stringp (umbriel-model-window-label '((id . "w-1")))))
  (should (stringp (umbriel-model-window-label
                    (umbriel-model-test--window))))
  (should-not (string-match-p "[\n\t]"
                              (umbriel-model-window-label '((id . "w-1"))))))

;;;; workspace label

(ert-deftest umbriel-model-window-cells/columns-in-order ()
  "A window's cells are its label's parts, keyed by column."
  (should (equal '((app . "foot") (title . "shell") (placement . "tile 1,0")
                   (flags . "active urgent"))
                 (umbriel-model-window-cells
                  (umbriel-model-test--window
                   :app_id "foot" :title "shell" :column 1 :row 0
                   :active t :focused nil :visible nil :urgent t)))))

(ert-deftest umbriel-model-workspace-cells/absent-parts-are-empty ()
  "Every column is present; what a workspace lacks is the empty string."
  (should (equal '((id . "DP-1 3") (name . "") (layout . "dwindle")
                   (windows . "") (marks . ""))
                 (umbriel-model-workspace-cells
                  (umbriel-model-test--workspace
                   :output "DP-1" :index 3 :named nil :name nil
                   :layout "dwindle" :active nil :focused nil :urgent nil)))))

(ert-deftest umbriel-model-workspace-cells/count-reads-as-windows ()
  "COUNT fills the `windows' column, singular for one."
  (dolist (case '((1 . "1 window") (0 . "0 windows") (3 . "3 windows")))
    (should (equal (cdr case)
                   (alist-get 'windows
                              (umbriel-model-workspace-cells
                               (umbriel-model-test--workspace)
                               (car case)))))))

(ert-deftest umbriel-model-workspace-label/name-only-when-named ()
  "The name is shown for named workspaces only; output, index and layout show."
  (let ((named (umbriel-model-workspace-label
                (umbriel-model-test--workspace :name "web" :named t
                                                  :index 1 :output "DP-1"
                                                  :layout "dwindle")))
        (unnamed (umbriel-model-workspace-label
                  (umbriel-model-test--workspace :name "hidden" :named nil
                                                    :index 4 :output "HDMI-A-1"
                                                    :layout "master"))))
    (should (umbriel-model-test--word-p named "web"))
    (should (umbriel-model-test--word-p named "DP-1"))
    (should (umbriel-model-test--word-p named "dwindle"))
    (should-not (string-match-p "hidden" unnamed))
    (should (umbriel-model-test--word-p unnamed "4"))
    (should (umbriel-model-test--word-p unnamed "master"))))

(ert-deftest umbriel-model-workspace-label/marks-are-independent ()
  "`active', `focused' and `urgent' each appear only for their own field."
  (let ((active-only (umbriel-model-workspace-label
                      (umbriel-model-test--workspace
                       :active t :focused nil :urgent nil)))
        (focused-only (umbriel-model-workspace-label
                       (umbriel-model-test--workspace
                        :active nil :focused t :urgent nil)))
        (urgent-only (umbriel-model-workspace-label
                      (umbriel-model-test--workspace
                       :active nil :focused nil :urgent t))))
    (should (umbriel-model-test--word-p active-only "active"))
    (should-not (umbriel-model-test--word-p active-only "focused"))
    (should-not (umbriel-model-test--word-p active-only "urgent"))
    (should (umbriel-model-test--word-p focused-only "focused"))
    (should-not (umbriel-model-test--word-p focused-only "active"))
    (should-not (umbriel-model-test--word-p focused-only "urgent"))
    (should (umbriel-model-test--word-p urgent-only "urgent"))
    (should-not (umbriel-model-test--word-p urgent-only "active"))
    (should-not (umbriel-model-test--word-p urgent-only "focused"))))

(ert-deftest umbriel-model-workspace-label/count-comes-from-the-join ()
  "COUNT is rendered (singular for 1); the wire's `window_count' is not read."
  (let* ((workspace (umbriel-model-test--workspace :window_count 99
                                                      :layout "scrolling"))
         (three (umbriel-model-test--label
                 (umbriel-model-workspace-label workspace 3)))
         (one (umbriel-model-test--label
               (umbriel-model-workspace-label workspace 1)))
         (none (umbriel-model-test--label
                (umbriel-model-workspace-label workspace))))
    (should (string-match-p "\\_<3 windows\\_>" three))
    (should (string-match-p "\\_<1 window\\_>" one))
    (should-not (string-match-p "1 windows" one))
    (should-not (umbriel-model-test--word-p none "window"))
    (should-not (umbriel-model-test--word-p none "windows"))
    (should-not (string-match-p "99" three))
    (should-not (string-match-p "99" none))
    (should (umbriel-model-test--word-p three "scrolling"))))

(ert-deftest umbriel-model-workspace-label/part-order ()
  "The label reads output, index, name, layout, count, then the marks."
  (let* ((label (umbriel-model-test--label
                 (umbriel-model-workspace-label
                  (umbriel-model-test--workspace :id "HDMI-A-1:4"
                                                    :output "HDMI-A-1"
                                                    :index 42 :name "webtab"
                                                    :named t :layout "scrolling"
                                                    :active t :focused t
                                                    :urgent t)
                  3)))
         (positions (mapcar (lambda (part)
                              (let ((position (string-match (regexp-quote part)
                                                            label)))
                                (should position)
                                position))
                            '("HDMI-A-1" "42" "webtab" "scrolling"
                              "3 windows" "active" "focused" "urgent"))))
    (should (equal positions (sort (copy-sequence positions) #'<)))))

(ert-deftest umbriel-model-workspace-label/flattens-and-defaults ()
  "Compositor strings are flattened; a partial entry yields a string."
  (let ((label (umbriel-model-test--label
                (umbriel-model-workspace-label
                 (umbriel-model-test--workspace :name "we\nb\tc" :named t
                                                   :output "DP\n-1")))))
    (should-not (string-match-p "[\n\t\r]" label))
    (should (string-match-p "we b c" label))
    (should (string-match-p "DP -1" label)))
  (should (stringp (umbriel-model-workspace-label nil)))
  (should (stringp (umbriel-model-workspace-label '())))
  (should (stringp (umbriel-model-workspace-label '((id . "DP-1:1")))))
  (should-not (string-match-p "[\n\t]"
                              (umbriel-model-workspace-label
                               '((id . "DP-1:1"))))))

;;;; layouts

(ert-deftest umbriel-model/layouts-are-the-contract-list ()
  "`umbriel-model-layouts' is the constant list the actions offer."
  (should (boundp 'umbriel-model-layouts))
  (should (equal umbriel-model-layouts
                 '("scrolling" "dwindle" "master" "stacking" "toggle"))))

;;;; purity

(ert-deftest umbriel-model/loads-without-the-client ()
  "A fresh `emacs -Q --batch' loads the model and leaves `umbriel' unloaded."
  (let* ((process-environment
          (seq-remove (lambda (variable)
                        (string-prefix-p "LD_LIBRARY_PATH=" variable))
                      process-environment))
         (output (with-temp-buffer
                   (let ((status (call-process
                                  (expand-file-name invocation-name
                                                    invocation-directory)
                                  nil t nil
                                  "-Q" "--batch"
                                  "-L" umbriel-model-test--model-dir
                                  ;; A stale .elc beside the source must
                                  ;; not shadow it, nor warn into OUTPUT.
                                  "--eval" "(setq load-prefer-newer t)"
                                  "--eval"
                                  (concat "(progn"
                                          " (unless (require 'umbriel-model nil t)"
                                          " (kill-emacs 3))"
                                          " (princ (format \"%S\""
                                          " (featurep 'umbriel-ipc))))"))))
                     (should (equal 0 status))
                     (buffer-string)))))
    (should (equal "nil" (string-trim output)))))

;;; umbriel-model-test.el ends here
