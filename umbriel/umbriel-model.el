;;; umbriel-model.el --- Pure model of a umbriel snapshot -*- lexical-binding: t; -*-

;;; Commentary:

;; The model turns the parsed umbriel snapshots into the data and one-line
;; strings a presentation layer draws: workspaces paired with their windows,
;; the selector that addresses a workspace, and the labels a user reads.
;;
;; Everything here is pure -- parsed alists in, data or strings out -- so it
;; can be tested without a compositor and reused by any front end.  No
;; process, buffer or input is touched.
;;
;; Entries are alists in the shape emacs-config:DEC-013 parses out of the
;; wire: JSON false, null and `[]' all arrive as nil, so every read is
;; `alist-get' with a default (`""' for strings, `-1' for positions and
;; `index', nil for flags).  A field renamed upstream degrades a label
;; instead of signalling, and no entry -- even an empty one -- makes a function here signal.
;;
;; Ordering is expressed as sort keys, one list per entry, and `sort' with
;; `:key' compares them with `value<': lists lexicographically, numbers
;; numerically, strings with `string<'.  Sorting is stable and copies, so a
;; tie keeps input order and no caller's list is mutated.
;;
;; Two addressing fields are not interchangeable.  The join between a window
;; and its workspace uses the workspace's opaque `id'; an action addresses a
;; workspace by its selector, the `name' when `named' (even when it is all
;; digits) else the `index', always qualified by its `output'.  Windows are
;; addressed by their own opaque `id'.  Flattening is a display rule only:
;; the join and the selector read raw values.
;;
;; The wire's `window_count' is never read: a workspace's window count is
;; the number of windows the join placed under it, so label and rows agree.

;;; Code:

(require 'seq)
(require 'subr-x)

;;;; Fields

(defun umbriel-model--text (entry key)
  "ENTRY's KEY flattened into one line.
Newlines, carriage returns and tabs become spaces; a missing value, or a
nil one, becomes the empty string, and a non-string is rendered with
`%s'."
  (let ((value (alist-get key entry)))
    (cond ((stringp value)
           (replace-regexp-in-string "[\n\r\t]" " " value t t))
          ((null value) "")
          (t (format "%s" value)))))

(defun umbriel-model--number (value)
  "VALUE when it is an integer, else -1, the unset position."
  (if (integerp value) value -1))

(defun umbriel-model--position (value)
  "VALUE when it is a placed position, else `most-positive-fixnum'.
Placed means an integer of 0 or more; -1 and anything unreadable sorts
after every placed position."
  (if (and (integerp value) (>= value 0))
      value
    most-positive-fixnum))

(defun umbriel-model--marks (entry fields)
  "The names of FIELDS that ENTRY has set, in the given order, as strings."
  (delq nil (mapcar (lambda (field)
                      (and (alist-get field entry) (symbol-name field)))
                    fields)))

;;;; Sort keys

(defun umbriel-model--workspace-key (workspace)
  "Sort key for WORKSPACE: its `output', then its `index'."
  (list (umbriel-model--text workspace 'output)
        (umbriel-model--number (alist-get 'index workspace -1))))

(defun umbriel-model--window-key (window)
  "Sort key for WINDOW, which also puts tiled windows before floating ones.
Tiled windows sort by `column', then `row', then `tab_index' (each with -1
last), then `title'; floating windows sort by `title' alone."
  (if (alist-get 'floating window)
      (list 1 0 0 0 (umbriel-model--text window 'title))
    (list 0
          (umbriel-model--position (alist-get 'column window -1))
          (umbriel-model--position (alist-get 'row window -1))
          (umbriel-model--position (alist-get 'tab_index window -1))
          (umbriel-model--text window 'title))))

;;;; Grouping

(defun umbriel-model--windows-of (owner pairs)
  "The windows of PAIRS that OWNER takes, in display order.
PAIRS are `(OWNER . WINDOW)' as `umbriel-model-group' computed them,
with a nil owner for the windows that join nothing."
  (sort (mapcar #'cdr (seq-filter (lambda (pair) (eq owner (car pair)))
                                  pairs))
        :key #'umbriel-model--window-key))

(defun umbriel-model-pad-names (windows)
  "The name of each pad among WINDOWS, sorted.
A pad with no windows is in no window, so it is not here."
  (sort (seq-uniq (delq nil (mapcar #'umbriel-model-window-scratchpad
                                    windows)))))

(defun umbriel-model--pads (windows)
  "One `((scratchpad . NAME))' entry per pad among WINDOWS, ordered by name."
  (mapcar (lambda (name) (list (cons 'scratchpad name)))
          (umbriel-model-pad-names windows)))

(defun umbriel-model--owner (window pads workspaces)
  "The group that takes WINDOW: its entry in PADS, else in WORKSPACES, else nil.
A pad comes first because a shown pad's windows also report a workspace."
  (let ((pad (umbriel-model-window-scratchpad window)))
    (if pad
        (seq-find (lambda (entry) (equal pad (alist-get 'scratchpad entry)))
                  pads)
      (umbriel-model-window-workspace workspaces window))))

(defun umbriel-model-group (workspaces windows)
  "Pair each of WORKSPACES and each pad with the WINDOWS that join it.
The result is a list of `(OWNER . WINDOWS)' groups in display order:
workspaces by output and index, kept even with nil windows, then a
`((scratchpad . NAME))' owner per pad by name, then a final
`(nil . WINDOWS)' group for what joins neither, omitted when empty.  A
window joins its pad when it has one (emacs-config:DEC-028), else the
workspace whose `id' equals its `workspace'.  Every window appears exactly
once, entries are returned as given, and neither input is mutated."
  (let* ((ordered (sort workspaces :key #'umbriel-model--workspace-key))
         (pads (umbriel-model--pads windows))
         (pairs (mapcar (lambda (window)
                          (cons (umbriel-model--owner window pads ordered)
                                window))
                        windows))
         (groups (mapcar (lambda (owner)
                           (cons owner
                                 (umbriel-model--windows-of owner pairs)))
                         (append ordered pads)))
         (unassigned (umbriel-model--windows-of nil pairs)))
    (if unassigned
        (append groups (list (cons nil unassigned)))
      groups)))

(defun umbriel-model-window-scratchpad (window)
  "The name of the pad WINDOW belongs to, or nil.
Only a non-empty `scratchpad' string names a pad."
  (let ((name (alist-get 'scratchpad window)))
    (and (stringp name) (not (string-empty-p name)) name)))

(defun umbriel-model-window-workspace (workspaces window)
  "The entry in WORKSPACES that WINDOW joins, or nil.
The join is on the raw opaque `id'; a window whose `workspace' is not a
non-empty string joins nothing."
  (let ((id (alist-get 'workspace window)))
    (when (and (stringp id) (not (string-empty-p id)))
      (seq-find (lambda (workspace)
                  (equal id (alist-get 'id workspace)))
                workspaces))))

;;;; Selectors and choices

(defun umbriel-model-workspace-selector (workspace)
  "The action fields addressing WORKSPACE, as an alist.
`workspace' is the raw `name' string when WORKSPACE is `named' -- even
when it is all digits -- else the `index' number.  `output' is always
present, raw as well."
  (list (cons 'workspace
              (if (alist-get 'named workspace)
                  (or (alist-get 'name workspace) "")
                (umbriel-model--number (alist-get 'index workspace -1))))
        (cons 'output (or (alist-get 'output workspace) ""))))

(defun umbriel-model-workspace-choices (workspaces)
  "`(LABEL . WORKSPACE)' pairs for `completing-read', in display order.
LABEL is `umbriel-model-workspace-label' without a window count."
  (mapcar (lambda (workspace)
            (cons (umbriel-model-workspace-label workspace) workspace))
          (sort workspaces :key #'umbriel-model--workspace-key)))

;;;; Display strings

(defun umbriel-model--line (tokens)
  "TOKENS, the non-empty ones, joined into one display line."
  (string-join (seq-filter (lambda (token) (not (string-empty-p token)))
                           tokens)
               " "))

(defun umbriel-model--cells-line (cells)
  "CELLS, an alist of column to text, as one display line."
  (umbriel-model--line (mapcar #'cdr cells)))

(defun umbriel-model--count-text (count)
  "COUNT windows as display text, empty when COUNT is nil."
  (cond ((null count) "")
        ((equal count 1) "1 window")
        (t (format "%s windows" count))))

(defun umbriel-model-workspace-cells (workspace &optional count)
  "WORKSPACE's display parts as an alist, in column order.
`id' is the output and index, `name' the name of a named workspace,
`layout' the layout, `windows' how many windows the join placed under it
when COUNT is non-nil, and `marks' the `active', `focused' and `urgent'
marks.  A part WORKSPACE lacks is the empty string.  The wire's
`window_count' is never read."
  (list (cons 'id (umbriel-model--line
                   (list (umbriel-model--text workspace 'output)
                         (number-to-string
                          (umbriel-model--number
                           (alist-get 'index workspace -1))))))
        (cons 'name (if (alist-get 'named workspace)
                        (umbriel-model--text workspace 'name)
                      ""))
        (cons 'layout (umbriel-model--text workspace 'layout))
        (cons 'windows (umbriel-model--count-text count))
        (cons 'marks (umbriel-model--line
                      (umbriel-model--marks
                       workspace '(active focused urgent))))))

(defun umbriel-model-group-cells (owner count)
  "The heading cells of OWNER, a workspace or a pad, over COUNT windows.
A pad gives the columns a workspace does, its `layout' and `marks' empty."
  (if-let* ((pad (alist-get 'scratchpad owner)))
      (list (cons 'id "scratchpad")
            (cons 'name pad)
            (cons 'layout "")
            (cons 'windows (umbriel-model--count-text count))
            (cons 'marks ""))
    (umbriel-model-workspace-cells owner count)))

(defun umbriel-model-workspace-label (workspace &optional count)
  "One display line for WORKSPACE, its cells for COUNT windows joined.
See `umbriel-model-workspace-cells'."
  (umbriel-model--cells-line
   (umbriel-model-workspace-cells workspace count)))

(defun umbriel-model--placement (window column row tab)
  "Placement token for WINDOW at COLUMN, ROW and TAB.
`float' for a floating window, `tab I' for a tabbed one, `tile C,R' when
COLUMN is placed, else the bare `tile'."
  (cond ((alist-get 'floating window) "float")
        ((alist-get 'tabbed window) (format "tab %d" tab))
        ((>= column 0) (format "tile %d,%d" column row))
        (t "tile")))

(defun umbriel-model-window-cells (window)
  "WINDOW's display parts as an alist, in column order.
`app' is the app id, `title' the title, `placement' the placement, and
`flags' every flag that is set, in the order `active', `focused',
`visible', `minimized', `fullscreen', `urgent', `xwayland'.  A part
WINDOW lacks is the empty string."
  (list (cons 'app (umbriel-model--text window 'app_id))
        (cons 'title (umbriel-model--text window 'title))
        (cons 'placement
              (umbriel-model--placement
               window
               (umbriel-model--number (alist-get 'column window -1))
               (umbriel-model--number (alist-get 'row window -1))
               (umbriel-model--number (alist-get 'tab_index window -1))))
        (cons 'flags (umbriel-model--line
                      (umbriel-model--marks
                       window '(active focused visible minimized fullscreen
                                urgent xwayland))))))

(defun umbriel-model-window-label (window)
  "One display line for WINDOW, its cells joined.
See `umbriel-model-window-cells'."
  (umbriel-model--cells-line (umbriel-model-window-cells window)))

;;;; Layouts

(defconst umbriel-model-layouts
  '("scrolling" "dwindle" "master" "stacking" "toggle")
  "The layout actions a workspace may be asked to switch to.")

(provide 'umbriel-model)

;;; umbriel-model.el ends here
