;;; umbriel-buffer.el --- Draw held umbriel state -*- lexical-binding: t; -*-

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

;; The display half of the umbriel client.  `umbriel' holds the last
;; snapshots the stream was sent; this file draws them as a tree of magit
;; sections -- workspaces and scratchpads, and the windows joined to each --
;; and turns a key on a row into the requests that act on it.  It never connects:
;; `umbriel' is the one entry point that opens the stream, beside the
;; buffer's creation, never from the mode body.
;;
;; Actions are one table and one plan.  The row at point is captured
;; first, its plan is then refused or built, and only then is anything
;; prompted and sent -- so a refused row never prompts, and a prompt
;; never redirects the action it was opened for.  Nothing is refreshed
;; before a reply lands, and no effect is ever written locally: the
;; buffer shows only what the compositor reports.  No prompt runs inside
;; a process callback; the global switch reaches its prompt in a timer.
;;
;; Repainting is decoupled from drawing.  A write to the held state only
;; schedules one repaint, `umbriel-buffer-repaint-delay' out, so a
;; burst of stream events coalesces into a single redraw.  The timer does
;; not redraw a buffer nobody is looking at: it marks the buffer stale,
;; and the buffer redraws itself when a window next displays it.
;;
;; A repaint keeps what the user would notice: every displaying window's
;; point returns to the section it was on, and folds survive through
;; magit's visibility cache.

;;; Code:

(require 'magit-section)
(require 'umbriel-ipc)
(require 'umbriel-columns)
(require 'umbriel-model)

;;;; State of the buffer

(defconst umbriel-buffer-name "*umbriel*"
  "The name of the buffer that displays the held umbriel state.")

(defconst umbriel-buffer-repaint-delay 0.1
  "Seconds a state change waits before the buffer repaints.
A burst of stream events therefore coalesces into one redraw.")

(defvar umbriel-buffer--repaint-timer nil
  "The pending repaint timer, or nil when no repaint is pending.
Global on purpose: one repaint serves the one *umbriel* buffer, however
many changes asked for it.")

(defvar-local umbriel-buffer--stale nil
  "Non-nil when the buffer fell behind the held state while undisplayed.
`window-buffer-change-functions' schedules the repaint that catches up.")

(defun umbriel-buffer--live ()
  "The *umbriel* buffer when it is alive and in `umbriel-buffer-mode'.
A buffer that merely bears the name is somebody else's; never draw into
it."
  (let ((buffer (get-buffer umbriel-buffer-name)))
    (when (and (buffer-live-p buffer)
               (eq (buffer-local-value 'major-mode buffer)
                   'umbriel-buffer-mode))
      buffer)))

;;;; Header line

(defun umbriel-buffer--doubled (text)
  "TEXT with every `%' doubled, as a mode-line construct requires."
  (replace-regexp-in-string "%" "%%" text t t))

(defun umbriel-buffer--status-text (status)
  "One sentence naming the stream STATUS."
  (pcase status
    ('open "umbriel: stream open")
    ('never "umbriel: stream never opened")
    (_ (umbriel-failure-message status))))

(defun umbriel-buffer--skipped-text (skipped)
  "How many stream lines were SKIPPED, or nil when none were."
  (when (> skipped 0)
    (if (= skipped 1)
        "1 line skipped"
      (format "%s lines skipped" skipped))))

(defun umbriel-buffer--header-text (status skipped)
  "The header line for the client STATUS and the SKIPPED line count.
Every `%' is doubled, because the result is a mode-line construct."
  (let ((text (umbriel-buffer--status-text status))
        (skipped-text (umbriel-buffer--skipped-text skipped)))
    (umbriel-buffer--doubled
     (if skipped-text (format "%s · %s" text skipped-text) text))))

;;;; Rendering

(defface umbriel-buffer-active-window
  '((t :inherit isearch :extend t))
  "Face for the row of the window holding the keyboard."
  :group 'umbriel)

(defface umbriel-buffer-focused-window
  '((t :inherit highlight :extend t))
  "Face for the row of a workspace's remembered focus."
  :group 'umbriel)

(defface umbriel-buffer-active-workspace
  '((t :inherit magit-section-heading :extend t))
  "Face for the heading of an active workspace.
A user's config may shade it a step darker than the rows below it."
  :group 'umbriel)

(defface umbriel-buffer-active-workspace-window
  '((t :extend t))
  "Face for the window rows of an active workspace.
A user's config may shade it darker than `default'."
  :group 'umbriel)

(defun umbriel-buffer--window-face (window)
  "The face that marks WINDOW's row, or nil for an unmarked one.
`active' is seat-global, `focused' is per workspace, so `active' wins."
  (cond ((alist-get 'active window) 'umbriel-buffer-active-window)
        ((alist-get 'focused window) 'umbriel-buffer-focused-window)))

(defun umbriel-buffer--overlay-face (start end face)
  "Show START to END in FACE.
An overlay, because magit's point highlight is one and would hide a
text property; the priority outranks it.  The repaint's erase empties
the overlay, which then evaporates."
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'face face)
    (overlay-put overlay 'priority 1)
    (overlay-put overlay 'evaporate t)))

(defface umbriel-buffer-active-workspace-id
  '((t :inherit bold :foreground "white"))
  "Face for the output and index in an active workspace's heading."
  :group 'umbriel)

(defface umbriel-buffer-active-workspace-name
  '((t :inherit success :inverse-video t))
  "Face for the name pill in an active workspace's heading."
  :group 'umbriel)

(defface umbriel-buffer-workspace-name
  '((t :inherit default))
  "Face for the name pill in an inactive workspace's heading.
A user's config may give it a muted shade of the active pill's colour."
  :group 'umbriel)

(defface umbriel-buffer-layout
  '((t))
  "Face every layout pill shares; it sets no foreground, so a layout can.
A user's config may give it a background."
  :group 'umbriel)

(defface umbriel-buffer-layout-scrolling
  '((t :inherit (umbriel-buffer-layout font-lock-comment-face)
       :slant normal))
  "Face for the `scrolling' layout in a workspace's heading."
  :group 'umbriel)

(defface umbriel-buffer-layout-dwindle
  '((t :inherit (umbriel-buffer-layout warning) :weight normal))
  "Face for the `dwindle' layout in a workspace's heading."
  :group 'umbriel)

(defface umbriel-buffer-layout-master
  '((t :inherit (umbriel-buffer-layout success) :weight normal))
  "Face for the `master' layout in a workspace's heading."
  :group 'umbriel)

(defface umbriel-buffer-layout-stacking
  '((t :inherit (umbriel-buffer-layout font-lock-string-face)))
  "Face for the `stacking' layout in a workspace's heading."
  :group 'umbriel)

(defface umbriel-buffer-layout-toggle
  '((t :inherit (umbriel-buffer-layout shadow)))
  "Face for the `toggle' layout in a workspace's heading."
  :group 'umbriel)

(defface umbriel-buffer-window-app
  '((t :inherit font-lock-string-face))
  "Face for the app id column of a window row."
  :group 'umbriel)

(defface umbriel-buffer-window-placement
  '((t :inherit shadow))
  "Face for the placement column of a window row."
  :group 'umbriel)

(defface umbriel-buffer-window-flags
  '((t :inherit font-lock-comment-face))
  "Face for the flags column of a window row."
  :group 'umbriel)

(defconst umbriel-buffer-window-cell-faces
  '((app . umbriel-buffer-window-app)
    (placement . umbriel-buffer-window-placement)
    (visible . umbriel-buffer-window-flags)
    (active . umbriel-buffer-window-flags)
    (focused . umbriel-buffer-window-flags)
    (flags . umbriel-buffer-window-flags))
  "The face of each window row column; a column left out has none.")

(defconst umbriel-buffer-layouts
  '(("scrolling" umbriel-buffer-layout-scrolling "\U000F0E73")
    ("dwindle" umbriel-buffer-layout-dwindle "\U000F1677")
    ("master" umbriel-buffer-layout-master "\U000F0574")
    ("stacking" umbriel-buffer-layout-stacking "\U000F0328")
    ("toggle" umbriel-buffer-layout-toggle "\U000F0521"))
  "The face and Nerd Font icon of each layout.
A layout left out shows in `umbriel-buffer-layout', without an icon.")

(defconst umbriel-buffer-flags
  '((visible "\U000F0208" "V")
    (active "\U000F030C" "A")
    (focused "\U000F04FE" "F")
    (minimized "\U000F05B0" "m")
    (fullscreen "\U000F0293" "f")
    (urgent "\U000F009E" "!")
    (xwayland "\U000F0B1F" "x"))
  "Each flag's Nerd Font icon and its letter where icons cannot show.
In display order.")

(defconst umbriel-buffer-flag-columns '(visible active focused)
  "The flags with a column each, so they line up; the rest share `flags'.")

(defcustom umbriel-buffer-column-limits '((app . 24) (title . 40))
  "The widest each column may grow; a column left out is unlimited.
A window showing the buffer replaces the title's limit with what fits it."
  :type '(alist :key-type (choice (const app) (const title))
                :value-type natnum)
  :group 'umbriel)

(defcustom umbriel-buffer-title-min-width 16
  "The narrowest a window may make the title column."
  :type 'natnum
  :group 'umbriel)

(defconst umbriel-buffer-row-indent "  "
  "What a window row starts with, under its workspace's heading.")

(defun umbriel-buffer--icon (icon fallback)
  "ICON when a font has it, else FALLBACK.
A terminal names no font, so it gets FALLBACK whatever it could show."
  (if (fontp (char-displayable-p (aref icon 0))) icon fallback))

(defun umbriel-buffer--flag-glyph (flag)
  "What stands for FLAG: its icon, or its letter."
  (apply #'umbriel-buffer--icon (alist-get flag umbriel-buffer-flags)))

(defun umbriel-buffer--flag-glyphs (entry flags)
  "The glyphs of those FLAGS that ENTRY has set, joined."
  (string-join (mapcar #'umbriel-buffer--flag-glyph
                       (seq-filter (lambda (flag) (alist-get flag entry)) flags))
               " "))

(defun umbriel-buffer--flag-cells (entry)
  "ENTRY's flags as cells, one for each flag column and `flags' for the rest.
ENTRY is a window or a workspace, so headings and rows share the columns."
  (append (mapcar (lambda (flag)
                    (cons flag (umbriel-buffer--flag-glyphs entry (list flag))))
                  umbriel-buffer-flag-columns)
          (list (cons 'flags
                      (umbriel-buffer--flag-glyphs
                       entry (seq-difference
                              (mapcar #'car umbriel-buffer-flags)
                              umbriel-buffer-flag-columns))))))

(defun umbriel-buffer--window-cells (window limits)
  "WINDOW's cells, cut to fit LIMITS, with its flags as glyphs."
  (umbriel-columns-fit
   (append (assq-delete-all 'flags (umbriel-model-window-cells window))
           (umbriel-buffer--flag-cells window))
   limits))

(defun umbriel-buffer--insert-window (window grid)
  "Insert WINDOW's row at point, placed by GRID, in the face that marks it, if any."
  (let ((start (point)))
    (insert umbriel-buffer-row-indent
            (umbriel-columns-row
             (umbriel-buffer--window-cells window (plist-get grid :limits))
             (plist-get grid :row-starts)
             umbriel-buffer-window-cell-faces)
            "\n")
    (when-let* ((face (umbriel-buffer--window-face window)))
      (umbriel-buffer--overlay-face start (point) face))))

(defun umbriel-buffer--insert-windows (windows grid)
  "Insert each of WINDOWS as a child section placed by GRID, in the given order."
  (dolist (window windows)
    (magit-insert-section (umbriel-window (alist-get 'id window))
      (umbriel-buffer--insert-window window grid))))

(defun umbriel-buffer--pill (text face)
  "TEXT padded, in FACE, as a pill; an empty TEXT stays empty."
  (if (string-empty-p text)
      ""
    (propertize (format " %s " text)
                'font-lock-face face 'umbriel-buffer-pill t)))

(defun umbriel-buffer--layout-pill (layout)
  "The pill for LAYOUT, in its face and behind its icon when it has them."
  (pcase-let ((`(,face ,icon)
               (or (cdr (assoc layout umbriel-buffer-layouts))
                   '(umbriel-buffer-layout))))
    (umbriel-buffer--pill
     (string-join (remq nil (list (and icon (umbriel-buffer--icon icon nil))
                                  layout))
                  " ")
     face)))

(defun umbriel-buffer--group-cells (group)
  "The heading cells of GROUP, a `(OWNER . WINDOWS)' pair.
The count comes before the layout, so the layout can sit over the rows'
placement, the name and layout are pills, and the flags are the rows'."
  (let* ((workspace (car group))
         (cells (umbriel-model-group-cells workspace (length (cdr group)))))
    (append
     (list (assq 'id cells)
           (cons 'name (umbriel-buffer--pill
                        (alist-get 'name cells)
                        (if (alist-get 'active workspace)
                            'umbriel-buffer-active-workspace-name
                          'umbriel-buffer-workspace-name)))
           (assq 'windows cells)
           (cons 'layout (umbriel-buffer--layout-pill
                          (alist-get 'layout cells))))
     (umbriel-buffer--flag-cells workspace))))

(defun umbriel-buffer--flag-column-names ()
  "Every column that holds flags, rows and headings alike."
  (append umbriel-buffer-flag-columns '(flags)))

(defun umbriel-buffer--title-limit (rows heading-cells width)
  "The widest a title can be, for ROWS and HEADING-CELLS to fit WIDTH columns.
A layout shares the placement column and headings share the flag
columns, so each takes the wider of the two."
  (let ((flag-columns (umbriel-buffer--flag-column-names))
        (all (append rows heading-cells)))
    (max umbriel-buffer-title-min-width
         (- width
            (length umbriel-buffer-row-indent)
            (umbriel-columns-width rows 'app)
            (max (umbriel-columns-width rows 'placement)
                 (umbriel-columns-width heading-cells 'layout))
            (apply #'+ (mapcar (lambda (column)
                                 (umbriel-columns-width all column))
                               flag-columns))
            (* (+ 2 (length flag-columns)) umbriel-columns-gap)))))

(defun umbriel-buffer--row-starts (rows heading-cells)
  "Where ROWS' columns start, leaving room for HEADING-CELLS' layouts.
A layout sits over the placement column, so it can widen it."
  (umbriel-columns-starts
   rows (length umbriel-buffer-row-indent)
   `((placement . ,(umbriel-columns-width heading-cells 'layout)))))

(defun umbriel-buffer--heading-starts (heading-cells row-starts)
  "Where HEADING-CELLS' columns start, lined up with ROW-STARTS.
A heading's layout sits over the rows' placement and its flags over
theirs, unless its own cells run past them."
  (seq-reduce (lambda (starts column)
                (umbriel-columns-at-least starts column
                                             (alist-get column row-starts)))
              (umbriel-buffer--flag-column-names)
              (umbriel-columns-at-least
               (umbriel-columns-starts heading-cells 0)
               'layout (alist-get 'placement row-starts))))

(defun umbriel-buffer--grid (groups windows width)
  "How to lay out GROUPS and WINDOWS in WIDTH columns, as a plist.
`:limits' cuts a row's cells, and `:row-starts' and `:heading-starts'
place rows and headings.  A nil WIDTH keeps the default title limit."
  (let* ((heading-cells (mapcar #'umbriel-buffer--group-cells
                                (seq-filter #'car groups)))
         (cells-at (lambda (limits)
                     (mapcar (lambda (window)
                               (umbriel-buffer--window-cells window limits))
                             windows)))
         (limits (if width
                     (cons (cons 'title
                                 (umbriel-buffer--title-limit
                                  (funcall cells-at
                                           umbriel-buffer-column-limits)
                                  heading-cells width))
                           umbriel-buffer-column-limits)
                   umbriel-buffer-column-limits))
         (row-starts (umbriel-buffer--row-starts (funcall cells-at limits)
                                                    heading-cells)))
    (list :limits limits
          :row-starts row-starts
          :heading-starts (umbriel-buffer--heading-starts heading-cells
                                                             row-starts))))

(defun umbriel-buffer--count-end (row-starts)
  "Where a heading's window count ends: the right edge of ROW-STARTS' titles.
Nil when there are no rows to line up with."
  (when-let* ((placement (alist-get 'placement row-starts)))
    (- placement umbriel-columns-gap)))

(defun umbriel-buffer--group-heading (group grid)
  "The heading of GROUP placed by GRID, shaded when its owner is active.
Its window count ends with the title column, if that leaves the count
clear of the cells before it.  A styled cell stops `magit-insert-heading'
applying its face, so the heading sets its own, under the cells.  The
newline is part of the heading, so an active shade spans the line."
  (let* ((cells (umbriel-buffer--group-cells group))
         (active (alist-get 'active (car group)))
         (count-end (umbriel-buffer--count-end
                     (plist-get grid :row-starts)))
         (heading
          (concat (umbriel-columns-row
                   cells
                   (umbriel-columns-at-least
                    (plist-get grid :heading-starts) 'windows
                    (and count-end
                         (- count-end
                            (string-width (alist-get 'windows cells)))))
                   (and active
                        '((id . umbriel-buffer-active-workspace-id))))
                  "\n")))
    (font-lock-append-text-property
     0 (length heading) 'font-lock-face
     (if active 'umbriel-buffer-active-workspace 'magit-section-heading)
     heading)
    heading))

(defun umbriel-buffer--raise-pills (start end)
  "Show each pill from START to END over the point highlight.
Its text property alone would lose its background under point."
  (while (< start end)
    (let ((next (next-single-property-change start 'umbriel-buffer-pill
                                             nil end)))
      (when (get-text-property start 'umbriel-buffer-pill)
        (umbriel-buffer--overlay-face
         start next (car (ensure-list (get-text-property start
                                                         'font-lock-face)))))
      (setq start next))))

(defun umbriel-buffer--legend ()
  "What each flag in use means, aligned to the header's right edge.
Only flags something has set, because all of them would not fit.  An icon
can be wider than the one column it counts for, so each gets one spare."
  (let* ((held (append (umbriel-windows) (umbriel-workspaces)))
         (flags (seq-filter (lambda (flag)
                              (seq-some (lambda (entry) (alist-get flag entry))
                                        held))
                            (mapcar #'car umbriel-buffer-flags)))
         (text (mapconcat (lambda (flag)
                            (format "%s %s" (umbriel-buffer--flag-glyph flag)
                                    flag))
                          flags "  ")))
    (concat (propertize " " 'display
                        `(space :align-to
                                (- right ,(+ (string-width text)
                                             (length flags)))))
            text)))

(defun umbriel-buffer--header-line ()
  "The header: the stream's status, then the legend if it fits beside it."
  (concat (umbriel-buffer--header-text (umbriel-stream-status)
                                          (umbriel-skipped))
          (umbriel-buffer--legend)))

(defun umbriel-buffer--insert-workspace-windows (workspace windows grid)
  "Insert WINDOWS, WORKSPACE's rows placed by GRID, shaded when it is active.
A text property, so magit's point highlight still shows over the shade."
  (let ((start (point)))
    (umbriel-buffer--insert-windows windows grid)
    (when (alist-get 'active workspace)
      (font-lock-append-text-property
       start (point) 'font-lock-face
       'umbriel-buffer-active-workspace-window))))

(defun umbriel-buffer--insert-heading (group grid)
  "Insert the heading of GROUP placed by GRID, its pills raised."
  (let ((start (point)))
    (magit-insert-heading (umbriel-buffer--group-heading group grid))
    (umbriel-buffer--raise-pills start (point))))

(defun umbriel-buffer--insert-group (group grid)
  "Insert GROUP, an `(OWNER . WINDOWS)' pair from the model, placed by GRID.
OWNER is a workspace, a pad, or nil for the model's Unassigned group."
  (let* ((owner (car group))
         (windows (cdr group))
         (pad (alist-get 'scratchpad owner)))
    (cond
     ((null owner)
      (magit-insert-section (umbriel-unassigned)
        (magit-insert-heading "Unassigned")
        (umbriel-buffer--insert-windows windows grid)))
     (pad
      (magit-insert-section (umbriel-scratchpad pad)
        (umbriel-buffer--insert-heading group grid)
        (umbriel-buffer--insert-windows windows grid)))
     (t
      (magit-insert-section (umbriel-workspace (alist-get 'id owner))
        (umbriel-buffer--insert-heading group grid)
        (umbriel-buffer--insert-workspace-windows owner windows grid))))))

(defun umbriel-buffer--width ()
  "The columns of the narrowest window showing this buffer, or nil if none."
  (when-let* ((windows (get-buffer-window-list nil nil t)))
    (seq-min (mapcar #'window-max-chars-per-line windows))))

(defun umbriel-buffer--insert (workspaces windows)
  "Insert the held WORKSPACES and WINDOWS as sections at point.
The caller erases the buffer first; this inserts the whole tree.  Columns
line up across every group and fit the window, so they are measured
before anything is inserted."
  (magit-insert-section (umbriel-root)
    (if (and (null workspaces) (null windows))
        (insert "No windows or workspaces held.\n")
      (let* ((groups (umbriel-model-group workspaces windows))
             (grid (umbriel-buffer--grid groups windows
                                            (umbriel-buffer--width))))
        (dolist (group groups)
          (umbriel-buffer--insert-group group grid))))))

;;;; Keeping point

(defun umbriel-buffer--point-entry ()
  "The section at point with its line and column offset, or nil.
The result is `(SECTION LINE CHAR)'."
  (when-let* ((section (magit-section-at)))
    (cons section (magit-section-get-relative-position section))))

(defun umbriel-buffer--start-entry ()
  "The section at `window-start' with its line offset, or nil."
  (let ((start (window-start)))
    (when-let* ((section (magit-section-at start)))
      (cons section (count-lines (oref section start) start)))))

(defun umbriel-buffer--record-positions ()
  "The point and start of every window that shows the current buffer.
Each entry is `(WINDOW POINT-ENTRY START-ENTRY)', the entries as the
recorders above return them.  When no window shows the buffer, the one
entry has a nil WINDOW and the buffer's own point."
  (let ((buffer (current-buffer)))
    (or (mapcar (lambda (window)
                  (with-selected-window window
                    (with-current-buffer buffer
                      (list window
                            (umbriel-buffer--point-entry)
                            (umbriel-buffer--start-entry)))))
                (get-buffer-window-list buffer nil t))
        (list (list nil (umbriel-buffer--point-entry))))))

(defun umbriel-buffer--goto-position (position)
  "Move point back onto POSITION, a `(SECTION LINE CHAR)' list, or nil."
  (when position
    (pcase-let ((`(,section ,line ,char) position))
      (magit-section-goto-successor section line char))))

(defun umbriel-buffer--restore-start (window entry)
  "Put WINDOW's start back on the section in ENTRY, if not past point."
  (when-let* ((section (car-safe entry))
              (line (cdr-safe entry)))
    (when-let* ((found (magit-get-section (magit-section-ident section)))
                (pos (save-excursion
                       (goto-char (oref found start))
                       (forward-line line)
                       (point))))
      (when (<= pos (window-point window))
        (set-window-start window pos t)))))

(defun umbriel-buffer--restore-positions (entries)
  "Move each window in ENTRIES back onto the sections it was showing."
  (dolist (entry entries)
    (let ((window (nth 0 entry)))
      (if (not window)
          (umbriel-buffer--goto-position (nth 1 entry))
        (with-selected-window window
          (umbriel-buffer--goto-position (nth 1 entry)))
        (umbriel-buffer--restore-start window (nth 2 entry))))))

;;;; Repainting

(defun umbriel-buffer--forget-highlight ()
  "Drop the highlight state that the coming erase invalidates."
  (setq magit-section-pre-command-section nil
        magit-section-highlight-overlays nil
        magit-section-selection-overlays nil
        magit-section-highlighted-sections nil
        magit-section-focused-sections nil))

(defun umbriel-buffer--repaint ()
  "Redraw the *umbriel* buffer from the held state, keeping each point.
Does nothing unless there is a live *umbriel* buffer."
  (when-let* ((buffer (umbriel-buffer--live)))
    (with-current-buffer buffer
      (umbriel-buffer--forget-highlight)
      (let ((positions (umbriel-buffer--record-positions))
            (inhibit-read-only t))
        (erase-buffer)
        (umbriel-buffer--insert (umbriel-workspaces)
                                   (umbriel-windows))
        ;; The new sections know their `hidden' slot; only this gives
        ;; them the invisible overlays that make a fold real.
        (let ((magit-section-cache-visibility nil))
          (magit-section-show magit-root-section))
        (setq umbriel-buffer--stale nil)
        (umbriel-buffer--restore-positions positions))
      (magit-section-update-highlight t)
      (set-buffer-modified-p nil))))

(defun umbriel-buffer--repaint-due ()
  "Repaint *umbriel* when a window shows it, else mark the buffer stale."
  (setq umbriel-buffer--repaint-timer nil)
  (when-let* ((buffer (umbriel-buffer--live)))
    (if (get-buffer-window buffer t)
        (umbriel-buffer--repaint)
      (with-current-buffer buffer
        (setq umbriel-buffer--stale t)))))

(defun umbriel-buffer--schedule-repaint ()
  "Schedule one repaint of the *umbriel* buffer, unless one is pending.
The hook function: it never cancels or re-arms the pending timer, so a
burst of changes shares one repaint."
  (when (and (umbriel-buffer--live)
             (null umbriel-buffer--repaint-timer))
    (setq umbriel-buffer--repaint-timer
          (run-at-time umbriel-buffer-repaint-delay nil
                       #'umbriel-buffer--repaint-due))))

(defun umbriel-buffer--on-resize (_window)
  "Schedule a repaint, so the columns fit a window that changed size."
  (umbriel-buffer--schedule-repaint))

(defun umbriel-buffer--on-display (_window)
  "Schedule a repaint when this newly displayed buffer is stale.
It never edits the buffer: only the timer does.  The hook also runs for
buffers a window stops showing, which only costs a scheduled repaint."
  (when umbriel-buffer--stale
    (umbriel-buffer--schedule-repaint)))

;;;; Commands

(defun umbriel-buffer--on-kill ()
  "Close the stream, then cancel a pending repaint, as *umbriel* dies.
The close announces the new status, which can arm a timer through the
state hook, so the cancel has to come last."
  (umbriel-stream-close)
  (when-let* ((timer umbriel-buffer--repaint-timer))
    (cancel-timer timer)
    (setq umbriel-buffer--repaint-timer nil)))

(defun umbriel-buffer-refresh ()
  "Repaint *umbriel*, reopening the stream when it is not open.
An open stream is asked for nothing: the repaint shows what the stream
already delivered."
  (interactive)
  (if (eq (umbriel-stream-status) 'open)
      (umbriel-buffer--repaint)
    (umbriel-stream-open)))

;;;; Actions

(defun umbriel-buffer--key (command &optional map)
  "The key the user has bound to COMMAND, else `M-x COMMAND'.
MAP names the keymap to search, a symbol; nil searches the active maps.
Hints name keys this way because the package binds no global keys."
  (substitute-command-keys
   (format "%s\\[%s]" (if map (format "\\<%s>" map) "") command)))

;; A row key names an action, and most actions act on what the
;; compositor has focused, not on the row.  Acting on a row is therefore
;; three steps: capture the held entry the row shows, plan the requests
;; that make that entry the right one (or refuse the row), then -- only
;; if the plan stands -- prompt and send.  One table holds all three per
;; row, and one plan builds them.

(eval-and-compile
  (defconst umbriel-buffer-actions
    '((umbriel-focus "window-focus" window)
      (umbriel-toggle-floating "window-toggle-floating" window)
      (umbriel-kill "window-kill" window confirm-kill)
      (umbriel-center "window-center" focused)
      (umbriel-cycle-extent "window-cycle-primary-extent" focused)
      (umbriel-cycle-extent-back "window-cycle-primary-extent-back"
                                    focused)
      (umbriel-column-left "column-move-left" focused)
      (umbriel-column-right "column-move-right" focused)
      (umbriel-move-to-workspace "window-move-to-workspace-silent"
                                    focused workspace)
      (umbriel-switch "workspace-switch" workspace pad-workspace)
      (umbriel-set-layout "workspace-set-layout" active-workspace
                             layout)
      (umbriel-toggle-scratchpad "scratchpad-toggle" scratchpad)
      (umbriel-send-to-pad "window-move-to-scratchpad" pointer-window
                              pad))
    "Row actions: COMMAND, umbriel ACTION, ADDRESSING and a PROMPT.
ADDRESSING is `window' (the action takes the row's window id),
`focused' (focus the row's window first, then act, then restore the
focus the compositor held), `workspace' (always switch to the row's
workspace), `active-workspace' (switch there, unless that is already
the focused workspace, then act: switching to a workspace umbriel holds
active on its output toggles back to the previous one under
`back_and_forth'), `scratchpad' (the action takes the row's pad name;
the one action a pad heading allows) or `pointer-window' (as `focused',
for an action that takes the focus of the workspace under the pointer,
whatever holds the keyboard: only a workspace window on that output can
be the one it acts on).  PROMPT, when given, is `workspace',
`pad-workspace', `layout', `pad' or `confirm-kill'; a prompting
row prompts in its command, never in a reply.  Moving a window goes
silently, so the plan's own focus steps are the only ones seen."))

(defun umbriel-buffer--target ()
  "The held entry the row at point addresses, as a one-pair plist.
A `umbriel-window' section gives `(:window ENTRY)', an
`umbriel-workspace' one `(:workspace ENTRY)', a `umbriel-scratchpad'
one `(:scratchpad NAME)'; a heading, an empty
buffer or an entry no longer held is a `user-error'.  Capturing the
entry here is what keeps a later prompt from redirecting the action."
  (let* ((section (magit-section-at))
         (type (and section (oref section type)))
         (value (and section (oref section value))))
    (pcase type
      ('umbriel-window
       (if-let* ((window (umbriel-window value)))
           (list :window window)
         (user-error "No umbriel row at point")))
      ('umbriel-workspace
       (if-let* ((workspace (seq-find (lambda (held)
                                        (equal value (alist-get 'id held)))
                                      (umbriel-workspaces))))
           (list :workspace workspace)
         (user-error "No umbriel row at point")))
      ('umbriel-scratchpad (list :scratchpad value))
      (_ (user-error "No umbriel row at point")))))

(defun umbriel-buffer--focused-workspace (workspaces)
  "The one workspace in WORKSPACES umbriel marks focused, or nil.
umbriel marks at most one; with none focused, or with more than one,
there is no answer, and nil is how callers read that."
  (let ((focused (seq-filter (lambda (held) (alist-get 'focused held))
                             workspaces)))
    (when (= 1 (length focused))
      (car focused))))

(defun umbriel-buffer--off-pointer-p (workspace workspaces)
  "Non-nil unless WORKSPACE is on the output under the pointer.
That is the output of the `focused' workspace in WORKSPACES.  With
none focused, or with more than one, there is no answer, and refusing
is the conservative reading: a layout sent to the wrong output would
change a workspace the user is not looking at."
  (if-let* ((focused (umbriel-buffer--focused-workspace workspaces)))
      (not (equal (alist-get 'output workspace)
                  (alist-get 'output focused)))
    t))

(defun umbriel-buffer--previous-focus (workspaces)
  "The window id a `focused' plan restores afterwards, or nil.
That is the `focused_window' of the focused workspace in WORKSPACES,
which umbriel sends as \"\" when no window is focused.  A
workspace that is not known, or a `focused_window' that is absent, nil
or empty, leaves the focus unknown."
  (when-let* ((focused (umbriel-buffer--focused-workspace workspaces))
              (id (alist-get 'focused_window focused)))
    (and (stringp id) (not (string-empty-p id)) id)))

(defun umbriel-buffer--focus-act-restore (id act workspaces)
  "The requests that focus window ID, send ACT, then restore the focus.
With no known previous focus in WORKSPACES, or when that is ID itself,
there is nothing to restore."
  (let ((previous (umbriel-buffer--previous-focus workspaces))
        (focus (umbriel-action "window-focus" (list (cons 'window id)))))
    (if (and previous (not (equal previous id)))
        (list focus act
              (umbriel-action
               "window-focus" (list (cons 'window previous))))
      (list focus act))))

(defun umbriel-buffer--plan (spec target fields workspaces)
  "The requests SPEC plans for TARGET, or a refusal message string.
SPEC is a row of `umbriel-buffer-actions', TARGET the plist
`umbriel-buffer--target' returned, FIELDS the prompted action
fields or nil, and WORKSPACES the held workspaces.  Pure: it reads no
other state.  A refusal is a string with no requests in it and
depends on nothing but SPEC, TARGET and WORKSPACES, which is what lets
a command refuse a row before it prompts.  A `focused' window
row whose known previous focus is a different window plans one more
`window-focus' at the end, putting that focus back.  An
`active-workspace' row switches to its workspace unless that is the
focused one, in which case it only acts: switching to a workspace
umbriel already holds active toggles back under `back_and_forth'.  A
`scratchpad' row names the pad of its heading or window; any other
addressing refuses a pad heading, which has no window.  A
`workspace' row on a pad window with no workspace sends the action with
FIELDS, the workspace the user picked.  A `pointer-window' row plans as
a `focused' one, but refuses any row the action would not reach: it
would act on the workspace's focus instead, often this Emacs."
  (let* ((action (nth 1 spec))
         (addressing (nth 2 spec))
         (window (plist-get target :window))
         (its (and window
                   (umbriel-model-window-workspace workspaces window)))
         (workspace (or (plist-get target :workspace) its))
         (focused (umbriel-buffer--focused-workspace workspaces))
         (already (and workspace focused
                       (equal (alist-get 'id workspace)
                              (alist-get 'id focused))))
         (switch (and workspace
                      (umbriel-action
                       "workspace-switch"
                       (umbriel-model-workspace-selector workspace))))
         (act (umbriel-action action fields))
         (heading-pad (plist-get target :scratchpad))
         (pad (or heading-pad
                  (and window (umbriel-model-window-scratchpad window)))))
    (pcase (if (and heading-pad (not (eq addressing 'scratchpad)))
               'pad-heading
             addressing)
      ('pad-heading "Not a window")
      ('scratchpad
       (if pad
           (list (umbriel-action action `((scratchpad . ,pad))))
         "Not in a scratchpad"))
      ('window
       (if window
           (list (umbriel-action
                  action (cons (cons 'window (alist-get 'id window)) fields)))
         "Not a window"))
      ('focused
       (if window
           (umbriel-buffer--focus-act-restore (alist-get 'id window) act
                                                 workspaces)
         (list act)))
      ('pointer-window
       (cond ((not window) "Not a window")
             (pad (format "Already in a scratchpad; take it out with %s first"
                          (umbriel-buffer--key 'umbriel-move-to-workspace
                                               'umbriel-buffer-mode-map)))
             ((not workspace) "Window has no workspace")
             ((umbriel-buffer--off-pointer-p workspace workspaces)
              "Only a window on the output under the pointer can be sent")
             (t (umbriel-buffer--focus-act-restore (alist-get 'id window)
                                                      act workspaces))))
      ('workspace
       (cond (switch (list switch))
             (pad (list act))
             (t "Window has no workspace")))
      ('active-workspace
       (cond ((and pad (not switch))
              (format "A pad window has no layout; use %s"
                      (umbriel-buffer--key 'umbriel-set-active-layout)))
             ((not switch) "Window has no workspace")
             ((umbriel-buffer--off-pointer-p workspace workspaces)
              "The layout acts on the output under the pointer, not here")
             (already (list act))
             (t (list switch act))))
      (_ (error "Unknown addressing: %S" addressing)))))

(defun umbriel-buffer--ordered-table (candidates)
  "A completion table over CANDIDATES that keeps their own order.
vertico prefers a table's `display-sort-function' metadata over
`vertico-sort-function', so declaring identity there keeps a recency
sort (prescient) from reordering the prompt."
  (lambda (string pred action)
    (if (eq action 'metadata)
        '(metadata (display-sort-function . identity)
                   (cycle-sort-function . identity))
      (complete-with-action action candidates string pred))))

(defun umbriel-buffer--workspace-fields ()
  "The selector fields of the held workspace the user picks.
The prompt offers the choices in model order.  Empty input, which
`completing-read' allows even when it requires a match, is a
`user-error': nothing is sent."
  (let* ((choices (umbriel-model-workspace-choices (umbriel-workspaces)))
         (workspace (alist-get (completing-read
                                "Workspace: "
                                (umbriel-buffer--ordered-table choices)
                                nil t)
                               choices nil nil #'equal)))
    (unless workspace
      (user-error "No workspace chosen"))
    (umbriel-model-workspace-selector workspace)))

(defun umbriel-buffer--layout-fields ()
  "The layout field the user picks, for the output under the pointer.
The prompt offers the layouts in model order.  Empty input is a
`user-error', as for a workspace."
  (let ((layout (completing-read
                 "Set the layout of the output under the pointer: "
                 (umbriel-buffer--ordered-table umbriel-model-layouts)
                 nil t)))
    (unless (member layout umbriel-model-layouts)
      (user-error "No layout chosen"))
    (list (cons 'layout layout))))

(defcustom umbriel-buffer-known-pads '("main" "secondary" "tertiary")
  "Pads the send prompt offers even while empty.
The IPC names only pads holding windows.  A name here must also be a
`[[scratchpad]]' in umbriel's config, the only way to create a pad.
The default is umbriel's default set of pads."
  :type '(repeat string)
  :group 'umbriel)

(defun umbriel-buffer--pad-fields ()
  "The scratchpad field the user picks or types.
The prompt offers `umbriel-buffer-known-pads' and the pads holding
windows.  Any name may be typed, and umbriel refuses one it does not
know.  Empty input is a `user-error', as for a workspace."
  (let ((name (completing-read
               "Send to scratchpad: "
               (umbriel-buffer--ordered-table
                (sort (seq-uniq
                       (append umbriel-buffer-known-pads
                               (umbriel-model-pad-names
                                (umbriel-windows)))))))))
    (when (string-empty-p name)
      (user-error "No scratchpad chosen"))
    (list (cons 'scratchpad name))))

(defun umbriel-buffer--confirm-kill (window)
  "Return nil when the user confirms killing WINDOW, `declined' when not.
Emacs's own pid refuses at once, before any prompt: killing it would
take the editor with it.  The prompt reuses
the row's label, so compositor text arrives sanitised to one line; WINDOW was
captured before the prompt, so a repaint cannot redirect the kill."
  (let ((pid (alist-get 'pid window)))
    (when (and (numberp pid) (= pid (emacs-pid)))
      (user-error "That window is this Emacs; use delete-frame instead"))
    (unless (y-or-n-p (format "Kill the process of %s (pid %s)? "
                              (umbriel-model-window-label window)
                              pid))
      'declined)))

(defun umbriel-buffer--unjoined-pad-window-p (window)
  "Non-nil when WINDOW is in a pad and joins no held workspace.
umbriel reports no workspace for a pad window, shown or hidden."
  (and window
       (umbriel-model-window-scratchpad window)
       (not (umbriel-model-window-workspace (umbriel-workspaces)
                                               window))))

(defun umbriel-buffer--fields (kind target)
  "The action fields KIND yields for TARGET, or `declined'.
KIND is a row's PROMPT: `workspace', `layout' and `pad' prompt with
`completing-read', `pad-workspace' does the same only for a pad window
that joins no workspace, `confirm-kill' confirms TARGET's window, and
nil prompts nothing.  Any other kind signals an `error': a mistyped
prompt must never send unconfirmed."
  (cond ((null kind) nil)
        ((eq kind 'workspace) (umbriel-buffer--workspace-fields))
        ((eq kind 'pad-workspace)
         (when (umbriel-buffer--unjoined-pad-window-p
                (plist-get target :window))
           (umbriel-buffer--workspace-fields)))
        ((eq kind 'layout) (umbriel-buffer--layout-fields))
        ((eq kind 'pad) (umbriel-buffer--pad-fields))
        ((eq kind 'confirm-kill)
         (umbriel-buffer--confirm-kill (plist-get target :window)))
        (t (error "Unknown prompt kind: %S" kind))))

(defun umbriel-buffer--completion (result)
  "Act on the reply RESULT, then re-query unless the stream is open.
A failure is echoed and never refreshed; a success is silent.  The
stream status is read when the reply lands, not when the request left:
with the stream open the effect arrives on its own."
  (if (eq (car result) :failure)
      (umbriel-echo-failure result)
    (unless (eq (umbriel-stream-status) 'open)
      (umbriel-refresh))))

(defun umbriel-buffer--run (spec)
  "Send what SPEC plans for the row at point, refusing and prompting.
The target is captured and the plan refused before any prompt,
and the plan is refused again once the prompted fields are in, so a
refusal never reaches `umbriel-send-all'.  The prompts read the
held state; the reply is left to `umbriel-buffer--completion'.  A
declined prompt sends nothing."
  (let* ((target (umbriel-buffer--target))
         (workspaces (umbriel-workspaces))
         (refusal (umbriel-buffer--plan spec target nil workspaces)))
    (when (stringp refusal)
      (user-error "%s" refusal))
    (let ((fields (umbriel-buffer--fields (nth 3 spec) target)))
      (unless (eq fields 'declined)
        (let ((plan (umbriel-buffer--plan spec target fields workspaces)))
          (when (stringp plan)
            (user-error "%s" plan))
          (umbriel-send-all plan #'umbriel-buffer--completion))))))

(defmacro umbriel-buffer--define-actions ()
  "Define one interactive command per row of `umbriel-buffer-actions'.
Each command has a docstring naming its action and addressing, and
delegates to `umbriel-buffer--run' with its own row."
  `(progn
     ,@(mapcar (lambda (row)
                 (let ((command (nth 0 row))
                       (action (nth 1 row))
                       (addressing (nth 2 row)))
                   `(defun ,command ()
                      ,(format "Send the action %s for the row at point.
Addressing: %s." action addressing)
                      (interactive)
                      (umbriel-buffer--run ',row))))
               umbriel-buffer-actions)))

(umbriel-buffer--define-actions)

;;;; Global commands

(defun umbriel-buffer--send-one (action fields)
  "Send the single request ACTION with FIELDS, then complete it.
The global commands act where the pointer is, not on a row."
  (umbriel-send-all (list (umbriel-action action fields))
                       #'umbriel-buffer--completion))

(defun umbriel-buffer--switch ()
  "Send `workspace-switch' to the held workspace the user picks."
  (umbriel-buffer--send-one "workspace-switch"
                               (umbriel-buffer--workspace-fields)))

(defun umbriel-buffer--switch-later ()
  "Switch workspaces from a timer, or say why there is nothing to switch to.
A refresh's reply may not prompt, so it schedules this instead;
an active minibuffer asks for a retry, and so does the reply holding
no workspace at all."
  (cond ((active-minibuffer-window)
         (message "Minibuffer in use; retry with %s"
                  (umbriel-buffer--key 'umbriel-switch-workspace)))
        ((null (umbriel-workspaces))
         (message "No umbriel workspace known; retry with %s"
                  (umbriel-buffer--key 'umbriel-switch-workspace)))
        (t (umbriel-buffer--switch))))

;;;###autoload
(defun umbriel-switch-workspace ()
  "Switch to a workspace chosen from the held state.
The prompt runs now only while the stream is open and workspaces are
held; otherwise the state is refreshed first and the prompt follows
the reply in a timer, so no prompt ever runs inside a process
callback.  A reply that still holds no workspace is not prompted
from -- it says so instead."
  (interactive)
  (if (and (eq (umbriel-stream-status) 'open)
           (umbriel-workspaces))
      (umbriel-buffer--switch)
    (umbriel-refresh
     (lambda (result)
       (if (eq (car result) :failure)
           (umbriel-echo-failure result)
         (run-at-time 0 nil #'umbriel-buffer--switch-later))))))

;;;###autoload
(defun umbriel-set-active-layout ()
  "Set the layout of the workspace on the output under the pointer.
The layout is prompted for at once: the choices need no held state."
  (interactive)
  (umbriel-buffer--send-one "workspace-set-layout"
                               (umbriel-buffer--layout-fields)))

;;;###autoload
(defvar-keymap umbriel-command-map
  :doc "The commands that need no row, for one prefix key of the user's.
For example: (keymap-global-set \"C-c u\" umbriel-command-map)."
  :prefix 'umbriel-command-map
  "u" #'umbriel
  "w" #'umbriel-switch-workspace
  "l" #'umbriel-set-active-layout)

;;;; Mode

(defvar-keymap umbriel-buffer-mode-map
  :doc "Keymap for `umbriel-buffer-mode'."
  "RET" #'umbriel-focus
  "s" #'umbriel-switch
  "m" #'umbriel-move-to-workspace
  "t" #'umbriel-toggle-floating
  "c" #'umbriel-center
  "k" #'umbriel-kill
  "L" #'umbriel-set-layout
  "M-<left>" #'umbriel-column-left
  "<" #'umbriel-column-left
  "M-<right>" #'umbriel-column-right
  ">" #'umbriel-column-right
  "e" #'umbriel-cycle-extent
  "E" #'umbriel-cycle-extent-back
  "v" #'umbriel-toggle-scratchpad
  "P" #'umbriel-send-to-pad
  "g" #'umbriel-buffer-refresh
  "q" #'quit-window)

(define-derived-mode umbriel-buffer-mode magit-section-mode "Umbriel"
  "Major mode for the *umbriel* buffer.

Displays the held state as a section tree and sends the requests the
row keys name.  It never opens the stream: `umbriel' owns the
connection."
  (setq-local header-line-format
              '(:eval (umbriel-buffer--header-line)))
  (add-hook 'window-buffer-change-functions
            #'umbriel-buffer--on-display nil t)
  (add-hook 'window-size-change-functions
            #'umbriel-buffer--on-resize nil t)
  (add-hook 'umbriel-state-changed-hook
            #'umbriel-buffer--schedule-repaint))

(defun umbriel-buffer--create ()
  "Create *umbriel*, open the stream for it, draw it; return the buffer.
Whoever opens the stream registers the close: the kill hook is added
here, beside the open, never in the mode body."
  (with-current-buffer (get-buffer-create umbriel-buffer-name)
    (umbriel-buffer-mode)
    (add-hook 'kill-buffer-hook #'umbriel-buffer--on-kill nil t)
    (umbriel-stream-open)
    (umbriel-buffer--repaint)
    (current-buffer)))

;;;###autoload
(defun umbriel ()
  "Show the *umbriel* buffer, opening the stream when it is new.
An existing buffer is shown as it is: nothing is reopened."
  (interactive)
  (pop-to-buffer (or (umbriel-buffer--live) (umbriel-buffer--create))))

(provide 'umbriel-buffer)
;;; umbriel-buffer.el ends here
