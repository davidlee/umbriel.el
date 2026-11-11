;;; umbriel-buffer-test.el --- ert tests for the *umbriel* buffer -*- lexical-binding: t; -*-

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

;; Run: `just check', or M-x ert RET umbriel-buffer/ RET.
;;
;; Contract: the design of emacs-config:SL-016, § The buffer and § Actions, and
;; its phase sheets 04 (C-1..C-12, EX-1..EX-7) and 05 (VT-1..VT-5, EX-1..EX-7,
;; D-1..D-8).
;;
;; Nothing here reaches a compositor.  Every test runs inside
;; `umbriel-buffer-test--with-state', which binds `process-environment' to a
;; socket path that does not exist, stubs the stream open/close pair, and kills
;; *umbriel* on the way out with the stubs still in place.  Fixtures are
;; hand-written alists in the emacs-config:DEC-013 shape; no title, app id or name below is
;; real.
;;
;; One batch-mode caveat: `format-mode-line' returns "" in `emacs --batch'
;; whatever it is given (there is no terminal to render a mode line on), so the
;; `%%' rule is checked where it is observable here: the header construct's
;; text has every `%' doubled, which is exactly what the display would halve.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'eieio)
(require 'magit-section)
(require 'umbriel-ipc)
(require 'umbriel-model)
(require 'umbriel-buffer)

;; Declared for the byte-compiler: the tests `let'-bind and read these, and the
;; implementation owns their values.
(defvar umbriel-buffer--repaint-timer)
(defvar umbriel-buffer--stale)

;;;; Fixtures

(defconst umbriel-buffer-test--unset :unset
  "Override value that removes a field from a fixture entry entirely.")

(defun umbriel-buffer-test--entry (defaults overrides)
  "Return a copy of DEFAULTS with the plist OVERRIDES applied.
OVERRIDES are keywords (`:id') naming the alist symbols they replace.  A
value of `umbriel-buffer-test--unset' removes that key instead."
  (let ((entry (copy-tree defaults)))
    (while overrides
      (let ((key (intern (substring (symbol-name (pop overrides)) 1)))
            (value (pop overrides)))
        (setq entry (assq-delete-all key entry))
        (unless (eq value umbriel-buffer-test--unset)
          (push (cons key value) entry))))
    entry))

(defconst umbriel-buffer-test--workspace-defaults
  '((id . "DP-1:1") (name . "web") (named . t) (index . 1) (output . "DP-1")
    (active . nil) (focused . nil) (layout . "dwindle"))
  "A workspace entry as the compositor snapshot delivers it.")

(defconst umbriel-buffer-test--window-defaults
  '((id . "w-1") (workspace . "DP-1:1") (app_id . "foot") (title . "shell")
    (floating . nil) (column . 0) (row . 0) (tabbed . nil) (tab_index . -1)
    (active . nil) (focused . nil) (visible . nil) (minimized . nil)
    (fullscreen . nil) (urgent . nil) (xwayland . nil) (pid . 4242))
  "A window entry as the compositor snapshot delivers it.")

(defun umbriel-buffer-test--workspace (&rest overrides)
  "A workspace fixture with OVERRIDES applied."
  (umbriel-buffer-test--entry umbriel-buffer-test--workspace-defaults
                                 overrides))

(defun umbriel-buffer-test--window (&rest overrides)
  "A window fixture with OVERRIDES applied."
  (umbriel-buffer-test--entry umbriel-buffer-test--window-defaults
                                 overrides))

(defun umbriel-buffer-test--held (windows workspaces &optional status skipped)
  "A held-state plist: WINDOWS, WORKSPACES, STATUS, SKIPPED.
STATUS is `open' unless given; SKIPPED is 0 unless given."
  (list :windows windows :workspaces workspaces
        :status (or status 'open) :skipped (or skipped 0)))

(defun umbriel-buffer-test--fixture-workspaces ()
  "Two workspaces on two outputs, the second one empty."
  (list (umbriel-buffer-test--workspace :id "DP-1:3" :name "web"
                                           :index 3 :output "DP-1")
        (umbriel-buffer-test--workspace :id "HDMI-A-1:3" :name nil
                                           :named nil :index 3
                                           :output "HDMI-A-1")))

(defun umbriel-buffer-test--fixture-windows ()
  "Windows for the render fixture: two joined, one scratch, one ghost."
  (list (umbriel-buffer-test--window :id "w-abc" :workspace "DP-1:3"
                                        :app_id "foot" :title "shell"
                                        :column 0 :row 0
                                        :active t :focused t)
        (umbriel-buffer-test--window :id "w-def" :workspace "DP-1:3"
                                        :app_id "emacs" :title "notes.org"
                                        :column 1 :row 0)
        (umbriel-buffer-test--window :id "w-scratch" :workspace ""
                                        :app_id "pavucontrol" :title "Volume"
                                        :floating t :minimized t)
        (umbriel-buffer-test--window :id "w-ghost" :workspace "DP-9:9"
                                        :app_id "firefox" :title "Docs"
                                        :column 0 :row 0)))

(defun umbriel-buffer-test--point-workspaces ()
  "One workspace, for the point-keeping fixtures."
  (list (umbriel-buffer-test--workspace :id "DP-1:3" :name "web"
                                           :index 3 :output "DP-1")))

(defun umbriel-buffer-test--order-windows (b-column b-row)
  "Three windows in one workspace, with w-b placed at B-COLUMN and B-ROW.
At 0,0 w-b sorts first; at 3,0 it sorts last."
  (list (umbriel-buffer-test--window :id "w-a" :workspace "DP-1:3"
                                        :app_id "foot" :title "alpha"
                                        :column 1 :row 0)
        (umbriel-buffer-test--window :id "w-b" :workspace "DP-1:3"
                                        :app_id "emacs" :title "bravo"
                                        :column b-column :row b-row)
        (umbriel-buffer-test--window :id "w-c" :workspace "DP-1:3"
                                        :app_id "kitty" :title "charlie"
                                        :column 2 :row 0)))

(defun umbriel-buffer-test--tall-windows (&optional extra)
  "Thirty tiled windows in one workspace, plus EXTRA more.
Enough rows that the rendered tree is taller than a batch window."
  (cl-loop for index from 0 below (+ 30 (or extra 0))
           collect (umbriel-buffer-test--window
                    :id (format "w-tall-%02d" index)
                    :workspace "DP-1:3"
                    :app_id "foot"
                    :title (format "tall %02d" index)
                    :column 0 :row index)))

(defconst umbriel-buffer-test--socket
  "/nonexistent/umbriel-test/umbriel.sock"
  "The `UMBRIEL_SOCKET' every test binds: a path nothing listens on.")

;;;; Isolation (C-12)

(defvar umbriel-buffer-test--stream-log nil
  "Stream calls made by the default stubs, newest first.")

(defvar umbriel-buffer-test--timers nil
  "The timer objects `--with-timers' faked for the repaint throttle.")

(defconst umbriel-buffer-test--helper-buffers
  (list "*umbriel-test-other*" "*umbriel-test-unrelated*")
  "Scratch buffers the tests display or kill, removed on the way out.")

(defmacro umbriel-buffer-test--with-state (state &rest body)
  "Run BODY with STATE held, the stream faked and *umbriel* disposable.
STATE is a held-state plist for `umbriel--state', or nil for the initial
state.  `process-environment' names a socket that does not exist and drops
`WAYLAND_DISPLAY'; `umbriel-stream-open' and `-close' are counting stubs
logged in `umbriel-buffer-test--stream-log';
`umbriel-state-changed-hook' and the repaint timer start nil.  On exit
the buffer named `umbriel-buffer-name' is killed with the stubs still
in place, and the window configuration is restored."
  (declare (indent 1))
  `(let ((umbriel--state ,state)
         (umbriel-state-changed-hook nil)
         (umbriel-buffer--repaint-timer nil)
         (umbriel-buffer-test--stream-log nil)
         (process-environment
          (append (list (concat "UMBRIEL_SOCKET="
                                umbriel-buffer-test--socket))
                  (seq-remove
                   (lambda (variable)
                     (string-match-p
                      "\\`\\(UMBRIEL_SOCKET\\|WAYLAND_DISPLAY\\)=" variable))
                   process-environment))))
     (save-window-excursion
       (unwind-protect
           (cl-letf (((symbol-function 'umbriel-stream-open)
                      (lambda (&rest _)
                        (push 'open umbriel-buffer-test--stream-log)
                        t))
                     ((symbol-function 'umbriel-stream-close)
                      (lambda (&rest _)
                        (push 'close umbriel-buffer-test--stream-log)
                        nil)))
             ,@body)
         (when-let* ((buffer (get-buffer umbriel-buffer-name)))
           (with-current-buffer buffer
             ;; A faked timer is not a timer: clear it, so the kill hook's
             ;; cancel (C-9) cannot signal now that the stubs are gone.
             (when (and umbriel-buffer--repaint-timer
                        (not (timerp umbriel-buffer--repaint-timer)))
               (setq umbriel-buffer--repaint-timer nil)))
           (kill-buffer buffer)))
         ;; Nothing a test displayed or killed may outlive it either.
         (dolist (name umbriel-buffer-test--helper-buffers)
           (when (get-buffer name)
             (kill-buffer name))))))

(defun umbriel-buffer-test--count (call)
  "How often the default stubs logged CALL, a symbol."
  (seq-count (lambda (entry) (eq entry call))
             umbriel-buffer-test--stream-log))

(defmacro umbriel-buffer-test--with-timers (calls &rest body)
  "Run BODY with the timer interface discriminated; CALLS collects ours.
A `run-at-time' whose FUNCTION is `umbriel-buffer--repaint-due' is
recorded in CALLS as `(DELAY REPEAT FUNCTION ARGS)' and answered with a
faked timer; a `run-with-idle-timer' for that function fails the test.
`cancel-timer' and `timer-set-time' fail the test when their TIMER is
one of ours, because the throttle never cancels or re-arms (C-6).
Every other call -- auto-revert and friends in a full init -- goes to
the original function."
  (declare (indent 1))
  `(let ((,calls nil)
         (umbriel-buffer-test--timers nil)
         (orig-at-time (symbol-function 'run-at-time))
         (orig-idle (symbol-function 'run-with-idle-timer))
         (orig-cancel (symbol-function 'cancel-timer))
         (orig-set-time (symbol-function 'timer-set-time)))
     (cl-letf (((symbol-function 'run-at-time)
                (lambda (time repeat function &rest args)
                  (if (eq function #'umbriel-buffer--repaint-due)
                      (let ((timer (list 'fake-timer time repeat)))
                        (push (list time repeat function args) ,calls)
                        (push timer umbriel-buffer-test--timers)
                        timer)
                    (apply orig-at-time time repeat function args))))
               ((symbol-function 'run-with-idle-timer)
                (lambda (time repeat function &rest args)
                  (if (eq function #'umbriel-buffer--repaint-due)
                      (ert-fail "the repaint throttle must not be idle")
                    (apply orig-idle time repeat function args))))
               ((symbol-function 'cancel-timer)
                (lambda (timer)
                  (if (memq timer umbriel-buffer-test--timers)
                      (ert-fail "cancel-timer must not be called")
                    (funcall orig-cancel timer))))
               ((symbol-function 'timer-set-time)
                (lambda (timer &rest args)
                  (if (memq timer umbriel-buffer-test--timers)
                      (ert-fail "timer-set-time must not be called")
                    (apply orig-set-time timer args)))))
       ,@body)))

(defmacro umbriel-buffer-test--with-repaint-count (count &rest body)
  "Run BODY with `umbriel-buffer--repaint' counting into COUNT."
  (declare (indent 1))
  `(let ((,count 0))
     (cl-letf (((symbol-function 'umbriel-buffer--repaint)
                (lambda (&rest _) (cl-incf ,count))))
       ,@body)))

;;;; Buffer and section helpers

(defun umbriel-buffer-test--mode-buffer ()
  "Create *umbriel* in `umbriel-buffer-mode', undisplayed and unrendered."
  (let ((buffer (get-buffer-create umbriel-buffer-name)))
    (with-current-buffer buffer
      (umbriel-buffer-mode))
    buffer))

(defun umbriel-buffer-test--text ()
  "The current buffer's text, without properties."
  (buffer-substring-no-properties (point-min) (point-max)))

(defun umbriel-buffer-test--root ()
  "The root section of the current buffer."
  (or magit-root-section
      (error "The buffer has no root section")))

(defun umbriel-buffer-test--children (&optional section)
  "The child sections of SECTION, default the root."
  (oref (or section (umbriel-buffer-test--root)) children))

(defun umbriel-buffer-test--sections (&optional section)
  "SECTION, default the root, and all its descendants, depth first."
  (let ((section (or section (umbriel-buffer-test--root))))
    (cons section
          (mapcan #'umbriel-buffer-test--sections (oref section children)))))

(defun umbriel-buffer-test--types (sections)
  "The `type' of each of SECTIONS."
  (mapcar (lambda (section) (oref section type)) sections))

(defun umbriel-buffer-test--values (sections)
  "The `value' of each of SECTIONS."
  (mapcar (lambda (section) (oref section value)) sections))

(defun umbriel-buffer-test--find (type value)
  "The first section of TYPE in the current buffer whose value is VALUE."
  (seq-find (lambda (section)
              (and (eq (oref section type) type)
                   (equal (oref section value) value)))
            (umbriel-buffer-test--sections)))

(defun umbriel-buffer-test--heading (section)
  "The heading text of SECTION, from its start to the end of its heading."
  (buffer-substring-no-properties (oref section start)
                                  (or (oref section content)
                                      (oref section end))))

(defun umbriel-buffer-test--percent-pairs-p (string)
  "Non-nil when every `%' in STRING is part of a `%%' pair.
That is the property a mode-line construct needs: the display halves each
pair, so a lone `%' would be read as a construct."
  (let ((index 0)
        (ok t)
        (length (length string)))
    (while (and ok (< index length))
      (if (eq (aref string index) ?%)
          (if (and (< (1+ index) length)
                   (eq (aref string (1+ index)) ?%))
              (setq index (+ index 2))
            (setq ok nil))
        (setq index (1+ index))))
    ok))

(defun umbriel-buffer-test--doubled (string)
  "STRING with every `%' doubled, as a mode-line construct needs."
  (replace-regexp-in-string "%" "%%" string t t))

;;;; prompt and send isolation (used from scratchpads on)

(defvar umbriel-buffer-test--send-all-calls nil
  "Calls to the faked `umbriel-send-all', newest first: (PLAN . CALLBACK).")

(defvar umbriel-buffer-test--refreshes nil
  "Calls to the faked `umbriel-refresh', newest first: CALLBACK.")

(defvar umbriel-buffer-test--prompts nil
  "Prompt calls, newest first: (KIND PROMPT COLLECTION REQUIRE-MATCH).")

(defvar umbriel-buffer-test--prompt-choice nil
  "What the faked `completing-read' answers.")

(defvar umbriel-buffer-test--prompt-confirm t
  "What the faked `y-or-n-p' answers.")

(defvar umbriel-buffer-test--prompt-hook nil
  "Function the faked prompts call before answering, or nil.")

(defvar umbriel-buffer-test--prompt-allowed t
  "When nil, a faked prompt fails the test instead of answering.")

(defvar umbriel-buffer-test--messages nil
  "Messages the faked `message' recorded, newest first.")

(defvar umbriel-buffer-test--fake-timers nil
  "The timers `--with-scheduled' faked for the code under test.")

(defun umbriel-buffer-test--prompting (kind prompt &optional c match)
  "Record a prompt of KIND for PROMPT, C and MATCH; fail when none may run."
  (unless umbriel-buffer-test--prompt-allowed
    (ert-fail (format "%s ran where no prompt may run: %s" kind prompt)))
  (when umbriel-buffer-test--prompt-hook
    (funcall umbriel-buffer-test--prompt-hook))
  (push (list kind prompt c match) umbriel-buffer-test--prompts))

(defmacro umbriel-buffer-test--with-send-all (&rest body)
  "Run BODY with `umbriel-send-all' capturing instead of sending.
The calls land in `--send-all-calls'.  `umbriel-send' and
`make-network-process' fail the test, so no request can leave."
  (declare (indent 0))
  `(let ((umbriel-buffer-test--send-all-calls nil))
     (cl-letf (((symbol-function 'umbriel-send-all)
                (lambda (plan callback)
                  (push (cons plan callback)
                        umbriel-buffer-test--send-all-calls)))
               ((symbol-function 'umbriel-send)
                (lambda (&rest _)
                  (ert-fail "a request bypassed `umbriel-send-all'")))
               ((symbol-function 'make-network-process)
                (lambda (&rest _)
                  (ert-fail "a test opened a socket"))))
       ,@body)))

(defmacro umbriel-buffer-test--with-refresh (&rest body)
  "Run BODY with `umbriel-refresh' recording its callbacks in `--refreshes'."
  (declare (indent 0))
  `(let ((umbriel-buffer-test--refreshes nil))
     (cl-letf (((symbol-function 'umbriel-refresh)
                (lambda (&optional callback)
                  (push callback umbriel-buffer-test--refreshes)
                  nil)))
       ,@body)))

(defmacro umbriel-buffer-test--with-fake-status (variable &rest body)
  "Run BODY with `umbriel-stream-status' answering VARIABLE's value."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'umbriel-stream-status)
              (lambda () ,variable)))
     ,@body))

(defmacro umbriel-buffer-test--with-messages (&rest body)
  "Run BODY with `message' recorded in `--messages' instead of shown."
  (declare (indent 0))
  `(let ((umbriel-buffer-test--messages nil))
     (cl-letf (((symbol-function 'message)
                (lambda (format-string &rest args)
                  (push (condition-case nil
                            (apply #'format format-string args)
                          (error format-string))
                        umbriel-buffer-test--messages)
                  nil)))
       ,@body)))

;; A hint names the user's key, so a test binds its own and expects it.
(defmacro umbriel-buffer-test--with-key (map key command &rest body)
  "Run BODY with KEY bound to COMMAND in a child of MAP's value.
MAP is `global' for the global map, else a keymap variable's symbol."
  (declare (indent 3))
  (let ((saved (gensym "saved")) (child (gensym "child")))
    `(let* ((,saved ,(if (eq map 'global) '(current-global-map) map))
            (,child (make-sparse-keymap)))
       (set-keymap-parent ,child ,saved)
       (keymap-set ,child ,key ,command)
       ,(if (eq map 'global)
            `(unwind-protect (progn (use-global-map ,child) ,@body)
               (use-global-map ,saved))
          `(let ((,map ,child)) ,@body)))))

(defmacro umbriel-buffer-test--with-prompts (&rest body)
  "Run BODY with the prompting functions faked.
`completing-read' answers `--prompt-choice', `y-or-n-p' answers
`--prompt-confirm', and the `read-' family answers \"\".  Each records
its arguments in `--prompts' and calls `--prompt-hook' first.  With
`--prompt-allowed' nil a prompt fails the test instead: that is how a
callback or a timer is checked for never prompting."
  (declare (indent 0))
  `(let ((umbriel-buffer-test--prompts nil)
         (umbriel-buffer-test--prompt-choice nil)
         (umbriel-buffer-test--prompt-confirm t)
         (umbriel-buffer-test--prompt-hook nil)
         (umbriel-buffer-test--prompt-allowed t))
     (cl-letf (((symbol-function 'completing-read)
                (lambda (prompt collection &optional _predicate require-match
                                 &rest _)
                  (umbriel-buffer-test--prompting 'completing-read prompt
                                                     collection require-match)
                  umbriel-buffer-test--prompt-choice))
               ((symbol-function 'y-or-n-p)
                (lambda (prompt)
                  (umbriel-buffer-test--prompting 'y-or-n-p prompt)
                  umbriel-buffer-test--prompt-confirm))
               ((symbol-function 'read-string)
                (lambda (prompt &rest _)
                  (umbriel-buffer-test--prompting 'read-string prompt)
                  ""))
               ((symbol-function 'read-from-minibuffer)
                (lambda (prompt &rest _)
                  (umbriel-buffer-test--prompting 'read-from-minibuffer prompt)
                  ""))
               ((symbol-function 'read-number)
                (lambda (prompt &rest _)
                  (umbriel-buffer-test--prompting 'read-number prompt)
                  0)))
       ,@body)))

;;;; scratchpads (emacs-config:SL-017)

(defconst umbriel-buffer-test--toggle
  (assq 'umbriel-toggle-scratchpad umbriel-buffer-actions)
  "The pad toggle's row of the action table.")

(ert-deftest umbriel-buffer--plan/toggles-the-pad-of-a-heading-or-row ()
  "emacs-config:DEC-029: the name comes from a pad heading or a pad window's row."
  (let ((expected (list (umbriel-action
                         "scratchpad-toggle" '((scratchpad . "signal"))))))
    (should (equal expected
                   (umbriel-buffer--plan umbriel-buffer-test--toggle
                                            '(:scratchpad "signal") nil nil)))
    (should (equal expected
                   (umbriel-buffer--plan
                    umbriel-buffer-test--toggle
                    (list :window (umbriel-buffer-test--window
                                   :scratchpad "signal"))
                    nil nil)))))

(ert-deftest umbriel-buffer--plan/refuses-to-toggle-outside-a-pad ()
  "emacs-config:DEC-029: a plain window row or a workspace row has no pad to toggle."
  (dolist (target (list (list :window (umbriel-buffer-test--window))
                        (list :workspace (umbriel-buffer-test--workspace))))
    (should (equal "Not in a scratchpad"
                   (umbriel-buffer--plan umbriel-buffer-test--toggle
                                            target nil nil)))))

(ert-deftest umbriel-buffer--plan/other-actions-refuse-a-pad-heading ()
  "emacs-config:DEC-030: a pad heading names no window, so nothing else acts on it."
  (dolist (spec umbriel-buffer-actions)
    (unless (eq spec umbriel-buffer-test--toggle)
      (should (stringp (umbriel-buffer--plan
                        spec '(:scratchpad "signal") nil nil))))))

(ert-deftest umbriel-buffer/switch-prompts-from-an-unjoined-pad-window ()
  "A pad window with no workspace asks where to switch, and sends that."
  (let ((workspaces (umbriel-buffer-test--action-workspaces)))
    (umbriel-buffer-test--with-state
        (umbriel-buffer-test--held
         (list (umbriel-buffer-test--window
                :id "w-pad" :workspace "" :scratchpad "signal"))
         workspaces)
      (with-current-buffer (umbriel-buffer-test--mode-buffer)
        (umbriel-buffer--repaint)
        (umbriel-buffer-test--with-send-all
          (umbriel-buffer-test--with-prompts
            (let ((choice (nth 0 (umbriel-model-workspace-choices
                                  workspaces))))
              (setq umbriel-buffer-test--prompt-choice (car choice))
              (umbriel-buffer-test--call-on-row
               'umbriel-switch 'umbriel-window "w-pad")
              (should (= 1 (length umbriel-buffer-test--prompts)))
              (should (equal
                       (list (umbriel-action
                              "workspace-switch"
                              (umbriel-model-workspace-selector (cdr choice))))
                       (caar umbriel-buffer-test--send-all-calls))))))))))

(ert-deftest umbriel-buffer/switch-does-not-prompt-from-a-joined-window ()
  "A window with a workspace, in a pad or not, switches without asking."
  (let* ((workspaces (umbriel-buffer-test--action-workspaces))
         (id (alist-get 'id (car workspaces))))
    (umbriel-buffer-test--with-state
        (umbriel-buffer-test--held
         (list (umbriel-buffer-test--window
                :id "w-pad" :workspace id :scratchpad "signal"))
         workspaces)
      (with-current-buffer (umbriel-buffer-test--mode-buffer)
        (umbriel-buffer--repaint)
        (umbriel-buffer-test--with-send-all
          (umbriel-buffer-test--with-prompts
            (umbriel-buffer-test--call-on-row
             'umbriel-switch 'umbriel-window "w-pad")
            (should-not umbriel-buffer-test--prompts)
            (should umbriel-buffer-test--send-all-calls)))))))

(ert-deftest umbriel-buffer--plan/switch-still-refuses-an-orphan ()
  "A window in no pad and no workspace has nothing to switch from."
  (should (equal "Window has no workspace"
                 (umbriel-buffer--plan
                  (assq 'umbriel-switch umbriel-buffer-actions)
                  (list :window (umbriel-buffer-test--window :workspace ""))
                  nil nil))))

(defun umbriel-buffer-test--pad-window-layout-refusal ()
  "What `L' says on a pad window."
  (umbriel-buffer-test--refused
   (assq 'umbriel-set-layout umbriel-buffer-actions)
   (list :window (umbriel-buffer-test--window
                  :workspace "" :scratchpad "signal"))
   '((layout . "master"))
   (umbriel-buffer-test--action-workspaces)))

(ert-deftest umbriel-buffer--plan/layout-points-a-pad-window-elsewhere ()
  "A pad window has no layout, so `L' names the user's key that sets one."
  (umbriel-buffer-test--with-key global "C-c z l" #'umbriel-set-active-layout
    (should (umbriel-buffer-test--mentions-p
             (umbriel-buffer-test--pad-window-layout-refusal) "C-c z l"))))

(ert-deftest umbriel-buffer--plan/layout-hint-falls-back-to-m-x ()
  "With the command unbound, the hint names it for `M-x'."
  (should (umbriel-buffer-test--mentions-p
           (umbriel-buffer-test--pad-window-layout-refusal)
           "M-x umbriel-set-active-layout")))

(ert-deftest umbriel-buffer--target/reads-a-pad-heading ()
  "emacs-config:DEC-029: point on a pad heading targets that pad."
  (umbriel-buffer-test--with-state
      (umbriel-buffer-test--held
       (list (umbriel-buffer-test--window
              :id "w-signal" :workspace "" :scratchpad "signal"))
       (umbriel-buffer-test--point-workspaces))
    (with-current-buffer (umbriel-buffer-test--mode-buffer)
      (umbriel-buffer--repaint)
      (goto-char (oref (umbriel-buffer-test--find 'umbriel-scratchpad "signal")
                       start))
      (should (equal '(:scratchpad "signal") (umbriel-buffer--target))))))

(ert-deftest umbriel-buffer/v-toggles-the-pad ()
  "emacs-config:DEC-031: `v' is the pad toggle."
  (should (eq 'umbriel-toggle-scratchpad
              (keymap-lookup umbriel-buffer-mode-map "v"))))

;;;; sending a window to a pad (emacs-config:IMP-031)

(defconst umbriel-buffer-test--send-to-pad
  (assq 'umbriel-send-to-pad umbriel-buffer-actions)
  "The send-to-pad row of the action table.")

(defconst umbriel-buffer-test--pad-fields '((scratchpad . "main"))
  "The fields a send to the pad `main' carries.")

(defun umbriel-buffer-test--refused-send (target workspaces)
  "The refusal a send of TARGET to `main' gets among WORKSPACES."
  (umbriel-buffer-test--refused umbriel-buffer-test--send-to-pad target
                                   umbriel-buffer-test--pad-fields workspaces))

(ert-deftest umbriel-buffer--plan/send-to-pad-focuses-acts-and-restores ()
  "The row's window is focused, sent, and the previous focus comes back."
  (should (equal
           (list (umbriel-action "window-focus" '((window . "w-dp1")))
                 (umbriel-action "window-move-to-scratchpad"
                                    umbriel-buffer-test--pad-fields)
                 (umbriel-action "window-focus" '((window . "w-emacs"))))
           (umbriel-buffer--plan
            umbriel-buffer-test--send-to-pad
            (list :window (umbriel-buffer-test--window
                           :id "w-dp1" :workspace "DP-1:1"))
            umbriel-buffer-test--pad-fields
            (umbriel-buffer-test--action-workspaces-focusing "w-emacs")))))

(ert-deftest umbriel-buffer--plan/send-to-pad-refuses-a-pad-window ()
  "The action takes the workspace's focus, so a pad row would send another window."
  (should (umbriel-buffer-test--mentions-p
           (umbriel-buffer-test--refused-send
            (list :window (umbriel-buffer-test--window
                           :workspace "" :scratchpad "signal"))
            (umbriel-buffer-test--action-workspaces))
           "already in a scratchpad")))

(ert-deftest umbriel-buffer--plan/send-to-pad-names-the-move-key ()
  "The way out of a pad is the buffer's move key, whatever it is bound to."
  (umbriel-buffer-test--with-key umbriel-buffer-mode-map "M"
      #'umbriel-move-to-workspace
    (keymap-unset umbriel-buffer-mode-map "m" t)
    (should (string-search
             "with M first"
             (umbriel-buffer-test--refused-send
              (list :window (umbriel-buffer-test--window
                             :workspace "" :scratchpad "signal"))
              (umbriel-buffer-test--action-workspaces))))))

(ert-deftest umbriel-buffer--plan/send-to-pad-refuses-a-workspace-row ()
  "A workspace row must not send whatever happens to be focused."
  (should (equal "Not a window"
                 (umbriel-buffer-test--refused-send
                  (list :workspace (umbriel-buffer-test--workspace))
                  (umbriel-buffer-test--action-workspaces)))))

(ert-deftest umbriel-buffer--plan/send-to-pad-refuses-another-output ()
  "`window-focus' leaves the pointer, and the action follows the pointer."
  (should (umbriel-buffer-test--mentions-p
           (umbriel-buffer-test--refused-send
            (list :window (umbriel-buffer-test--window
                           :id "w-dp2" :workspace "DP-2:1"))
            (umbriel-buffer-test--action-workspaces))
           "under the pointer")))

(ert-deftest umbriel-buffer--plan/send-to-pad-refuses-an-orphan ()
  "A window in no pad and no workspace has no output to judge."
  (should (equal "Window has no workspace"
                 (umbriel-buffer-test--refused-send
                  (list :window (umbriel-buffer-test--window :workspace ""))
                  (umbriel-buffer-test--action-workspaces)))))

(ert-deftest umbriel-buffer/send-to-pad-offers-held-pads-and-takes-any-name ()
  "Held and known pads complete in order, and any other name can be typed."
  (let ((workspaces (umbriel-buffer-test--action-workspaces))
        (umbriel-buffer-known-pads '("main" "signal")))
    (umbriel-buffer-test--with-state
        (umbriel-buffer-test--held
         (list (umbriel-buffer-test--window :id "w-dp1" :workspace "DP-1:1")
               (umbriel-buffer-test--window
                :id "w-signal" :workspace "" :scratchpad "signal")
               (umbriel-buffer-test--window
                :id "w-discord" :workspace "" :scratchpad "discord"))
         workspaces)
      (with-current-buffer (umbriel-buffer-test--mode-buffer)
        (umbriel-buffer--repaint)
        (umbriel-buffer-test--with-send-all
          (umbriel-buffer-test--with-prompts
            (setq umbriel-buffer-test--prompt-choice "main")
            (umbriel-buffer-test--call-on-row
             'umbriel-send-to-pad 'umbriel-window "w-dp1")
            (pcase-let ((`(,_kind ,_prompt ,collection ,match)
                         (car umbriel-buffer-test--prompts)))
              (should (equal '("discord" "main" "signal")
                             (umbriel-buffer-test--offered collection)))
              (should-not match))
            (should (member (umbriel-action
                             "window-move-to-scratchpad"
                             umbriel-buffer-test--pad-fields)
                            (caar umbriel-buffer-test--send-all-calls)))))))))

(ert-deftest umbriel-buffer/send-to-pad-sends-nothing-without-a-name ()
  "Empty input names no pad, and umbriel would reject a bare action."
  (let ((workspaces (umbriel-buffer-test--action-workspaces)))
    (umbriel-buffer-test--with-state
        (umbriel-buffer-test--held
         (list (umbriel-buffer-test--window :id "w-dp1" :workspace "DP-1:1"))
         workspaces)
      (with-current-buffer (umbriel-buffer-test--mode-buffer)
        (umbriel-buffer--repaint)
        (umbriel-buffer-test--with-send-all
          (umbriel-buffer-test--with-prompts
            (setq umbriel-buffer-test--prompt-choice "")
            (should-error (umbriel-buffer-test--call-on-row
                           'umbriel-send-to-pad 'umbriel-window "w-dp1")
                          :type 'user-error)
            (should-not umbriel-buffer-test--send-all-calls)))))))

(ert-deftest umbriel-buffer/P-sends-to-a-pad ()
  "`P' sends the row's window to a pad."
  (should (eq 'umbriel-send-to-pad
              (keymap-lookup umbriel-buffer-mode-map "P"))))

;;;; render (VT-1, EX-1)

(ert-deftest umbriel-buffer/draws-a-section-per-pad ()
  "emacs-config:DEC-028: pads sit between workspaces and Unassigned, with their windows."
  (umbriel-buffer-test--with-state
      (umbriel-buffer-test--held
       (append (umbriel-buffer-test--fixture-windows)
               (list (umbriel-buffer-test--window
                      :id "w-signal" :workspace "" :scratchpad "signal"
                      :app_id "signal" :title "Signal" :floating t)
                     (umbriel-buffer-test--window
                      :id "w-shown" :workspace "DP-1:3" :scratchpad "discord"
                      :app_id "discord" :title "Chat" :floating t)))
       (umbriel-buffer-test--fixture-workspaces))
    (with-current-buffer (umbriel-buffer-test--mode-buffer)
      (umbriel-buffer--repaint)
      (let ((top (umbriel-buffer-test--children)))
        (should (equal '(umbriel-workspace umbriel-workspace
                         umbriel-scratchpad umbriel-scratchpad
                         umbriel-unassigned)
                       (umbriel-buffer-test--types top)))
        (should (equal '("DP-1:3" "HDMI-A-1:3" "discord" "signal" nil)
                       (umbriel-buffer-test--values top)))
        (should (equal '("w-shown")
                       (umbriel-buffer-test--values
                        (umbriel-buffer-test--children (nth 2 top)))))
        (should (string-match-p
                 "scratchpad +signal +1 window"
                 (umbriel-buffer-test--heading (nth 3 top))))))))

(ert-deftest umbriel-buffer/renders-the-held-state ()
  "VT-1/EX-1: section types and id values, windows nested, Unassigned last."
  (umbriel-buffer-test--with-state
      (umbriel-buffer-test--held (umbriel-buffer-test--fixture-windows)
                                    (umbriel-buffer-test--fixture-workspaces))
    (let ((buffer (umbriel-buffer-test--mode-buffer)))
      (with-current-buffer buffer
        (umbriel-buffer--repaint)
        (should (eq (oref (umbriel-buffer-test--root) type) 'umbriel-root))
        (let ((top (umbriel-buffer-test--children)))
          (should (equal '(umbriel-workspace umbriel-workspace
                            umbriel-unassigned)
                         (umbriel-buffer-test--types top)))
          (should (equal '("DP-1:3" "HDMI-A-1:3" nil)
                         (umbriel-buffer-test--values top)))
          ;; Windows nested under the workspace that joined them.
          (should (equal '(umbriel-window umbriel-window)
                         (umbriel-buffer-test--types
                          (umbriel-buffer-test--children (nth 0 top)))))
          (should (equal '("w-abc" "w-def")
                         (umbriel-buffer-test--values
                          (umbriel-buffer-test--children (nth 0 top)))))
          ;; An empty workspace still gets a section, with no windows.
          (should-not (umbriel-buffer-test--children (nth 1 top)))
          ;; The workspace id, not the heading label, is the section value.
          ;; Unassigned holds the unknown- and empty-workspace windows.
          (should (equal '("w-ghost" "w-scratch")
                         (sort (umbriel-buffer-test--values
                                (umbriel-buffer-test--children (nth 2 top)))
                               #'string<))))))))

(ert-deftest umbriel-buffer/workspace-heading-counts-the-join ()
  "VT-1: a workspace heading carries the number of windows the join placed."
  (umbriel-buffer-test--with-state
      (umbriel-buffer-test--held (umbriel-buffer-test--fixture-windows)
                                    (umbriel-buffer-test--fixture-workspaces))
    (let ((buffer (umbriel-buffer-test--mode-buffer)))
      (with-current-buffer buffer
        (umbriel-buffer--repaint)
        (let ((top (umbriel-buffer-test--children)))
          (should (string-match-p "2 windows"
                                  (umbriel-buffer-test--heading (nth 0 top))))
          (should (string-match-p "0 windows"
                                  (umbriel-buffer-test--heading (nth 1 top))))
          (should (string-match-p "Unassigned"
                                  (umbriel-buffer-test--heading (nth 2 top)))))))))

(ert-deftest umbriel-buffer/renders-the-empty-state ()
  "VT-1/EX-1: with nothing held the body is the one empty-state line."
  (umbriel-buffer-test--with-state nil
    (let ((buffer (umbriel-buffer-test--mode-buffer)))
      (with-current-buffer buffer
        (umbriel-buffer--repaint)
        (should (equal "No windows or workspaces held.\n"
                       (umbriel-buffer-test--text)))
        (should (eq (oref (umbriel-buffer-test--root) type) 'umbriel-root))
        (should-not (umbriel-buffer-test--children))))))

;;;; active and focused windows (emacs-config:IMP-030)

(defun umbriel-buffer-test--top-face (pos)
  "The face of the highest-priority overlay at POS that sets one, or nil."
  (seq-some (lambda (overlay)
              (or (overlay-get overlay 'face)
                  (overlay-get overlay 'font-lock-face)))
            (overlays-at pos t)))

(defun umbriel-buffer-test--row-face (id)
  "The top overlay face at the start of the window row ID."
  (umbriel-buffer-test--top-face
   (oref (umbriel-buffer-test--find 'umbriel-window id) start)))

(defmacro umbriel-buffer-test--with-fixture-render (&rest body)
  "Run BODY in a displayed *umbriel* painted from the render fixture.
There w-abc is both `active' and `focused', and w-def is neither."
  (declare (indent 0) (debug t))
  `(umbriel-buffer-test--with-state
       (umbriel-buffer-test--held
        (umbriel-buffer-test--fixture-windows)
        (umbriel-buffer-test--fixture-workspaces))
     (let ((buffer (umbriel-buffer-test--mode-buffer)))
       (set-window-buffer (selected-window) buffer)
       (with-current-buffer buffer
         (umbriel-buffer--repaint)
         ,@body))))

(defun umbriel-buffer-test--move-active-to (id)
  "Make window ID the only `active' one and repaint; `focused' stays put."
  (setq umbriel--state
        (umbriel-buffer-test--held
         (mapcar (lambda (window)
                   (umbriel-buffer-test--entry
                    window (list :active (equal id (alist-get 'id window)))))
                 (umbriel-windows))
         (umbriel-workspaces)))
  (umbriel-buffer--repaint))

(ert-deftest umbriel-buffer/active-window-row-has-the-active-face ()
  "The active row carries its face to the newline, over its `focused' mark."
  (umbriel-buffer-test--with-fixture-render
    (let ((row (umbriel-buffer-test--find 'umbriel-window "w-abc")))
      (dolist (pos (list (oref row start) (1- (oref row end))))
        (should (eq 'umbriel-buffer-active-window
                    (umbriel-buffer-test--top-face pos)))))))

(ert-deftest umbriel-buffer/plain-window-row-has-no-mark ()
  "A row neither `active' nor `focused' gets neither face."
  (umbriel-buffer-test--with-fixture-render
    (should-not (umbriel-buffer-test--row-face "w-def"))))

(ert-deftest umbriel-buffer/row-face-outranks-the-point-highlight ()
  "Point on the row, or on its workspace, leaves the row's face showing."
  (umbriel-buffer-test--with-fixture-render
    (dolist (section (list (umbriel-buffer-test--find 'umbriel-window "w-abc")
                           (umbriel-buffer-test--find 'umbriel-workspace
                                                         "DP-1:3")))
      (goto-char (oref section start))
      (magit-section-update-highlight t)
      (should magit-section-highlight-overlays)
      (should (eq 'umbriel-buffer-active-window
                  (umbriel-buffer-test--row-face "w-abc"))))))

(ert-deftest umbriel-buffer/row-faces-follow-the-keyboard ()
  "Active moving away leaves the old row focused, and one overlay per row."
  (umbriel-buffer-test--with-fixture-render
    (umbriel-buffer-test--move-active-to "w-def")
    (should (eq 'umbriel-buffer-active-window
                (umbriel-buffer-test--row-face "w-def")))
    (should (eq 'umbriel-buffer-focused-window
                (umbriel-buffer-test--row-face "w-abc")))
    (should (= 2 (seq-count (lambda (overlay)
                              (memq (overlay-get overlay 'face)
                                    '(umbriel-buffer-active-window
                                      umbriel-buffer-focused-window)))
                            (overlays-in (point-min) (point-max)))))))

;;;; active workspace (emacs-config:IMP-030)

(defun umbriel-buffer-test--activate-workspace (id)
  "Make workspace ID the only `active' one and repaint."
  (setq umbriel--state
        (umbriel-buffer-test--held
         (umbriel-windows)
         (mapcar (lambda (workspace)
                   (umbriel-buffer-test--entry
                    workspace (list :active (equal id (alist-get 'id workspace)))))
                 (umbriel-workspaces))))
  (umbriel-buffer--repaint))

(defun umbriel-buffer-test--faces-at (pos)
  "The `font-lock-face' value at POS, as a list."
  (ensure-list (get-text-property pos 'font-lock-face)))

(defun umbriel-buffer-test--line-end (section)
  "The position of the newline ending SECTION's first line."
  (save-excursion
    (goto-char (oref section start))
    (line-end-position)))

(defun umbriel-buffer-test--line-has-face-p (section face)
  "Non-nil when FACE runs from SECTION's start to its first line's newline."
  (and (memq face (umbriel-buffer-test--faces-at (oref section start)))
       (memq face (umbriel-buffer-test--faces-at
                   (umbriel-buffer-test--line-end section)))))

(ert-deftest umbriel-buffer/active-workspace-heading-is-shaded ()
  "The active workspace's heading is shaded to its newline; others are not."
  (umbriel-buffer-test--with-fixture-render
    (umbriel-buffer-test--activate-workspace "DP-1:3")
    (should (umbriel-buffer-test--line-has-face-p
             (umbriel-buffer-test--find 'umbriel-workspace "DP-1:3")
             'umbriel-buffer-active-workspace))
    (should (equal '(magit-section-heading)
                   (umbriel-buffer-test--faces-at
                    (oref (umbriel-buffer-test--find 'umbriel-workspace
                                                        "HDMI-A-1:3")
                          start))))))

(ert-deftest umbriel-buffer/active-workspace-rows-are-shaded ()
  "Every row of the active workspace is shaded; rows elsewhere are not."
  (umbriel-buffer-test--with-fixture-render
    (umbriel-buffer-test--activate-workspace "DP-1:3")
    (dolist (id '("w-abc" "w-def"))
      (should (umbriel-buffer-test--line-has-face-p
               (umbriel-buffer-test--find 'umbriel-window id)
               'umbriel-buffer-active-workspace-window)))
    (should-not (umbriel-buffer-test--line-has-face-p
                 (umbriel-buffer-test--find 'umbriel-window "w-ghost")
                 'umbriel-buffer-active-workspace-window))))

(ert-deftest umbriel-buffer/active-workspace-id-stands-out ()
  "The output and index of the active workspace, and only those, are bright."
  (umbriel-buffer-test--with-fixture-render
    (umbriel-buffer-test--activate-workspace "DP-1:3")
    (let ((start (oref (umbriel-buffer-test--find 'umbriel-workspace "DP-1:3")
                       start))
          (other (oref (umbriel-buffer-test--find 'umbriel-workspace
                                                     "HDMI-A-1:3")
                       start)))
      (should (equal '(umbriel-buffer-active-workspace-id
                       umbriel-buffer-active-workspace)
                     (umbriel-buffer-test--faces-at start)))
      (should-not (memq 'umbriel-buffer-active-workspace-id
                        (umbriel-buffer-test--faces-at
                         (+ start (length "DP-1 3")))))
      (should-not (memq 'umbriel-buffer-active-workspace-id
                        (umbriel-buffer-test--faces-at other))))))

(defun umbriel-buffer-test--workspace-start (id)
  "Where the heading of workspace ID starts."
  (oref (umbriel-buffer-test--find 'umbriel-workspace id) start))

(defun umbriel-buffer-test--heading-text-pos (id text)
  "Where TEXT starts in the heading of workspace ID."
  (save-excursion
    (goto-char (umbriel-buffer-test--workspace-start id))
    (search-forward text (line-end-position))
    (match-beginning 0)))

(ert-deftest umbriel-buffer/inactive-heading-keeps-its-face-beside-styled-cells ()
  "A styled cell does not cost the rest of an inactive heading its face."
  (umbriel-buffer-test--with-fixture-render
    (umbriel-buffer-test--activate-workspace "HDMI-A-1:3")
    (should (umbriel-buffer-test--line-has-face-p
             (umbriel-buffer-test--find 'umbriel-workspace "DP-1:3")
             'magit-section-heading))))

(ert-deftest umbriel-buffer/workspace-name-is-a-padded-pill ()
  "A name's face covers its padding, and is muted unless its workspace is active."
  (umbriel-buffer-test--with-fixture-render
    (let ((name-face (lambda ()
                       (car (umbriel-buffer-test--faces-at
                             (umbriel-buffer-test--heading-text-pos
                              "DP-1:3" " web "))))))
      (umbriel-buffer-test--activate-workspace "HDMI-A-1:3")
      (should (eq 'umbriel-buffer-workspace-name (funcall name-face)))
      (umbriel-buffer-test--activate-workspace "DP-1:3")
      (should (eq 'umbriel-buffer-active-workspace-name
                  (funcall name-face))))))

(ert-deftest umbriel-buffer/unnamed-workspace-has-no-pill ()
  "A workspace without a name shows no empty pill."
  (umbriel-buffer-test--with-fixture-render
    (let ((start (umbriel-buffer-test--workspace-start "HDMI-A-1:3")))
      (should-not (text-property-any
                   start (save-excursion (goto-char start) (line-end-position))
                   'font-lock-face 'umbriel-buffer-workspace-name)))))

(ert-deftest umbriel-buffer/each-layout-has-its-own-face ()
  "Every layout the model offers shows in a face no other layout has."
  (umbriel-buffer-test--with-fixture-render
    (let ((faces
           (mapcar
            (lambda (layout)
              (umbriel-buffer-test--render
               nil (list (umbriel-buffer-test--workspace
                          :id "DP-1:3" :layout layout)))
              (car (umbriel-buffer-test--faces-at
                    (umbriel-buffer-test--heading-text-pos
                     "DP-1:3" layout))))
            umbriel-model-layouts)))
      (should-not (memq 'magit-section-heading faces))
      (should (equal faces (seq-uniq faces))))))

(ert-deftest umbriel-buffer/unknown-layout-is-a-plain-pill ()
  "A layout the buffer does not know shows in the base pill, without an icon."
  (umbriel-buffer-test--with-fixture-render
    (umbriel-buffer-test--render
     nil (list (umbriel-buffer-test--workspace :id "DP-1:3"
                                                  :layout "spiral")))
    (should (eq 'umbriel-buffer-layout
                (car (umbriel-buffer-test--faces-at
                      (umbriel-buffer-test--heading-text-pos
                       "DP-1:3" " spiral ")))))))

(ert-deftest umbriel-buffer/each-layout-has-its-own-icon ()
  "Every layout the model offers shows a distinct icon before its name."
  (umbriel-buffer-test--with-fixture-render
    (cl-letf (((symbol-function 'char-displayable-p)
               (lambda (&rest _) (font-spec))))
      (let ((icons
             (mapcar
              (lambda (layout)
                (umbriel-buffer-test--render
                 nil (list (umbriel-buffer-test--workspace
                            :id "DP-1:3" :layout layout)))
                (let ((pos (umbriel-buffer-test--heading-text-pos
                            "DP-1:3" (concat " " layout " "))))
                  (char-before pos)))
              umbriel-model-layouts)))
        (should (seq-every-p (lambda (icon) (>= icon #xe000)) icons))
        (should (equal icons (seq-uniq icons)))))))

(ert-deftest umbriel-buffer/pills-show-over-the-point-highlight ()
  "A pill is an overlay above magit's, so point on its heading leaves it shown."
  (umbriel-buffer-test--with-fixture-render
    (umbriel-buffer-test--activate-workspace "HDMI-A-1:3")
    (dolist (text '(" web " " dwindle "))
      (should (eq (car (umbriel-buffer-test--faces-at
                        (umbriel-buffer-test--heading-text-pos "DP-1:3" text)))
                  (umbriel-buffer-test--top-face
                   (umbriel-buffer-test--heading-text-pos "DP-1:3" text)))))))

(defun umbriel-buffer-test--render-aligned ()
  "Paint a named workspace and an unnamed one, with a window in each."
  (umbriel-buffer-test--render
   (list (umbriel-buffer-test--window :id "w-wide" :workspace "DP-1:3"
                                         :title (make-string 30 ?t))
         (umbriel-buffer-test--window :id "w-two" :workspace "DP-1:3")
         (umbriel-buffer-test--window :id "w-other" :workspace "DP-1:4"))
   (list (umbriel-buffer-test--workspace :id "DP-1:3" :active t)
         (umbriel-buffer-test--workspace :id "DP-1:4" :index 4 :name nil
                                            :named nil))))

(defun umbriel-buffer-test--stop-before (id text)
  "The `:align-to' column of the gap before TEXT in workspace ID's heading."
  (car (plist-get (cdr (get-text-property
                        (1- (umbriel-buffer-test--heading-text-pos id text))
                        'display))
                  :align-to)))

(ert-deftest umbriel-buffer/layout-lines-up-with-placement ()
  "A heading's layout pill starts where the rows' placement does."
  (umbriel-buffer-test--with-fixture-render
    (umbriel-buffer-test--render-aligned)
    (should (= (nth 1 (umbriel-buffer-test--stops
                       (umbriel-buffer-test--find 'umbriel-window "w-wide")))
               (umbriel-buffer-test--stop-before
                "DP-1:3" (umbriel-buffer--layout-pill "dwindle"))))))

(ert-deftest umbriel-buffer/window-count-ends-with-the-title-column ()
  "Window counts are right-justified to the title column's right edge."
  (umbriel-buffer-test--with-fixture-render
    (umbriel-buffer-test--render-aligned)
    (let ((edge (- (nth 1 (umbriel-buffer-test--stops
                           (umbriel-buffer-test--find 'umbriel-window
                                                         "w-wide")))
                   umbriel-columns-gap)))
      (should (= edge (+ (umbriel-buffer-test--stop-before "DP-1:3" "2 windows")
                         (length "2 windows"))))
      (should (= edge (+ (umbriel-buffer-test--stop-before "DP-1:4" "1 window")
                         (length "1 window")))))))

(ert-deftest umbriel-buffer/marks-clear-the-layout-pill ()
  "Flags and marks start past the widest layout pill, not over it."
  (umbriel-buffer-test--with-fixture-render
    (umbriel-buffer-test--render-aligned)
    (let* ((section (umbriel-buffer-test--find 'umbriel-workspace "DP-1:3"))
           (stops (umbriel-buffer-test--stops section))
           (marks (car (last stops)))
           (layout (car (last stops 2)))
           (pill (umbriel-buffer--layout-pill "dwindle")))
      (should (>= marks (+ layout (string-width pill)
                           umbriel-columns-gap))))))

(defun umbriel-buffer-test--render-in-width (width title)
  "Paint one window titled TITLE as if the buffer's window held WIDTH columns."
  (cl-letf (((symbol-function 'window-max-chars-per-line)
             (lambda (&rest _) width)))
    (umbriel-buffer-test--render
     (list (umbriel-buffer-test--window :id "w-long" :workspace "DP-1:3"
                                           :title title :active t))
     (list (umbriel-buffer-test--workspace :id "DP-1:3")))))

(defun umbriel-buffer-test--row-end (id)
  "The column where window ID's row ends, its last cell being one flag."
  (let ((section (umbriel-buffer-test--find 'umbriel-window id)))
    (1+ (car (last (umbriel-buffer-test--stops section))))))

(ert-deftest umbriel-buffer/title-shrinks-so-the-row-fits-the-window ()
  "A long title gives way, so the columns after it stay inside the window."
  (umbriel-buffer-test--with-fixture-render
    (umbriel-buffer-test--render-in-width 60 (make-string 200 ?t))
    (should (<= (umbriel-buffer-test--row-end "w-long") 60))
    (should (string-match-p
             "…" (umbriel-buffer-test--line-text
                  (umbriel-buffer-test--find 'umbriel-window "w-long"))))))

(ert-deftest umbriel-buffer/title-grows-into-a-wide-window ()
  "A wide window shows a title in full, past the limit used without a window."
  (umbriel-buffer-test--with-fixture-render
    (let ((title (make-string 100 ?t)))
      (umbriel-buffer-test--render-in-width 200 title)
      (should (string-match-p
               title (umbriel-buffer-test--line-text
                      (umbriel-buffer-test--find 'umbriel-window
                                                    "w-long")))))))

(ert-deftest umbriel-buffer/title-keeps-its-minimum-in-a-tiny-window ()
  "A window too narrow for the row still gets a readable title."
  (umbriel-buffer-test--with-fixture-render
    (umbriel-buffer-test--render-in-width 10 (make-string 200 ?t))
    (should (string-match-p
             (format "t\\{%d\\}…" (1- (1- umbriel-buffer-title-min-width)))
             (umbriel-buffer-test--line-text
              (umbriel-buffer-test--find 'umbriel-window "w-long"))))))

(ert-deftest umbriel-buffer/title-budget-leaves-room-for-heading-columns ()
  "Marks and layouts wider than the rows' cells take from the title too."
  (let ((rows '(((app . "app") (title . "") (placement . "tile") (visible . "")
                 (active . "") (focused . "") (flags . "")))))
    (should (> (umbriel-buffer--title-limit rows nil 100)
               (umbriel-buffer--title-limit
                rows '(((layout . "wider than tile") (visible . "") (active . "")
                        (focused . "") (flags . "! x")))
                100)))))

(ert-deftest umbriel-buffer/resizing-its-window-schedules-a-repaint ()
  "A resize of a window showing the buffer repaints it to the new width."
  (umbriel-buffer-test--with-fixture-render
    (should (memq #'umbriel-buffer--on-resize window-size-change-functions))
    (unwind-protect
        (progn
          (umbriel-buffer--on-resize (selected-window))
          (should umbriel-buffer--repaint-timer))
      (when umbriel-buffer--repaint-timer
        (cancel-timer umbriel-buffer--repaint-timer)
        (setq umbriel-buffer--repaint-timer nil)))))

(defun umbriel-buffer-test--render-flagged ()
  "Paint rows with different flags under one active, focused workspace."
  (umbriel-buffer-test--render
   (list (umbriel-buffer-test--window :id "w-all" :workspace "DP-1:3"
                                         :title (make-string 40 ?t)
                                         :visible t :active t :focused t)
         (umbriel-buffer-test--window :id "w-focused" :workspace "DP-1:3"
                                         :focused t)
         (umbriel-buffer-test--window :id "w-rare" :workspace "DP-1:3"
                                         :minimized t :urgent t))
   (list (umbriel-buffer-test--workspace :id "DP-1:3" :active t :focused t))))

(ert-deftest umbriel-buffer/flags-read-visible-active-focused ()
  "A row shows its flags in that order, as letters when icons are unavailable."
  (umbriel-buffer-test--with-fixture-render
    (umbriel-buffer-test--render-flagged)
    (should (string-suffix-p
             " V A F" (umbriel-buffer-test--line-text
                       (umbriel-buffer-test--find 'umbriel-window "w-all"))))))

(ert-deftest umbriel-buffer/each-main-flag-keeps-its-column ()
  "A flag sits in the same place whichever flags its row also has."
  (umbriel-buffer-test--with-fixture-render
    (umbriel-buffer-test--render-flagged)
    (let ((stops (lambda (id)
                   (umbriel-buffer-test--stops
                    (umbriel-buffer-test--find 'umbriel-window id)))))
      (should (= (car (last (funcall stops "w-all")))
                 (car (last (funcall stops "w-focused"))))))))

(ert-deftest umbriel-buffer/rarer-flags-share-the-last-column ()
  "Flags without a column of their own follow the main ones, joined."
  (umbriel-buffer-test--with-fixture-render
    (umbriel-buffer-test--render-flagged)
    (should (string-suffix-p
             " m !" (umbriel-buffer-test--line-text
                     (umbriel-buffer-test--find 'umbriel-window "w-rare"))))))

(ert-deftest umbriel-buffer/heading-marks-share-the-rows-flag-columns ()
  "A heading's `focused' sits over its rows' `focused'."
  (umbriel-buffer-test--with-fixture-render
    (umbriel-buffer-test--render-flagged)
    (should (= (car (last (umbriel-buffer-test--stops
                           (umbriel-buffer-test--find 'umbriel-workspace
                                                         "DP-1:3"))))
               (car (last (umbriel-buffer-test--stops
                           (umbriel-buffer-test--find 'umbriel-window
                                                         "w-focused"))))))))

(ert-deftest umbriel-buffer/flags-are-icons-when-a-font-has-them ()
  "With a font for the glyphs a flag is its icon; without one, its letter."
  (cl-letf (((symbol-function 'char-displayable-p)
             (lambda (&rest _) (font-spec))))
    (should (equal "\U000F0208" (umbriel-buffer--flag-glyph 'visible))))
  (cl-letf (((symbol-function 'char-displayable-p) (lambda (&rest _) 'unicode)))
    (should (equal "V" (umbriel-buffer--flag-glyph 'visible)))))

(ert-deftest umbriel-buffer/layout-drops-its-icon-without-a-font ()
  "Without a font for the icon, a layout pill is just its name."
  (cl-letf (((symbol-function 'char-displayable-p) #'ignore))
    (should (equal " dwindle " (umbriel-buffer--layout-pill "dwindle")))))

(ert-deftest umbriel-buffer/legend-names-the-flags-in-use ()
  "The header explains each flag something has set, and no others."
  (umbriel-buffer-test--with-fixture-render
    (umbriel-buffer-test--render-flagged)
    (let ((legend (umbriel-buffer--legend)))
      (dolist (entry '("V visible" "A active" "F focused" "m minimized"
                       "! urgent"))
        (should (string-search entry legend)))
      (should-not (string-search "xwayland" legend)))))

(ert-deftest umbriel-buffer/header-shows-the-status-before-the-legend ()
  "The status keeps its place, and the legend is aligned right after it."
  (umbriel-buffer-test--with-fixture-render
    (umbriel-buffer-test--render-flagged)
    (let* ((header (umbriel-buffer--header-line))
           (legend (umbriel-buffer--legend)))
      (should (string-suffix-p legend header))
      (should (string-prefix-p (umbriel-buffer--header-text
                                (umbriel-stream-status)
                                (umbriel-skipped))
                               header)))))

(ert-deftest umbriel-buffer/row-mark-shows-over-the-workspace-shade ()
  "The active row keeps its own face inside a shaded workspace."
  (umbriel-buffer-test--with-fixture-render
    (umbriel-buffer-test--activate-workspace "DP-1:3")
    (should (eq 'umbriel-buffer-active-window
                (umbriel-buffer-test--row-face "w-abc")))))

;;;; columns (emacs-config:IMP-030)

(defun umbriel-buffer-test--stops (section)
  "The `:align-to' columns on SECTION's first line, left to right."
  (let ((pos (oref section start))
        (end (umbriel-buffer-test--line-end section))
        stops)
    (while (< pos end)
      (when-let* ((display (get-text-property pos 'display)))
        (let ((stop (plist-get (cdr display) :align-to)))
          ;; A bare number counts the frame's font, not the buffer's, whose
          ;; wider glyphs then overrun the stops.
          (should (eq 'width (cdr stop)))
          (push (car stop) stops)))
      (setq pos (1+ pos)))
    (nreverse stops)))

(defun umbriel-buffer-test--render (windows workspaces)
  "Paint *umbriel* from WINDOWS and WORKSPACES in the current buffer."
  (setq umbriel--state
        (umbriel-buffer-test--held windows workspaces))
  (umbriel-buffer--repaint))

(defun umbriel-buffer-test--line-text (section)
  "The text of SECTION's first line, without its newline."
  (buffer-substring-no-properties
   (oref section start) (umbriel-buffer-test--line-end section)))

(ert-deftest umbriel-buffer/window-columns-align-across-workspaces ()
  "Titles start a gap past the widest app id, in every group."
  (umbriel-buffer-test--with-fixture-render
    (let ((title-column (+ 2 (length "pavucontrol") 2)))
      (dolist (id '("w-abc" "w-def" "w-scratch" "w-ghost"))
        (should (= title-column
                   (car (umbriel-buffer-test--stops
                         (umbriel-buffer-test--find 'umbriel-window id)))))))))

(ert-deftest umbriel-buffer/placement-column-aligns ()
  "Placement starts a gap past the widest title, whichever row has it."
  (umbriel-buffer-test--with-fixture-render
    (let ((placement-column (+ 2 (length "pavucontrol") 2 (length "notes.org") 2)))
      (dolist (id '("w-abc" "w-def"))
        (should (= placement-column
                   (cadr (umbriel-buffer-test--stops
                          (umbriel-buffer-test--find 'umbriel-window id)))))))))

(ert-deftest umbriel-buffer/aligned-row-reads-as-its-label ()
  "Alignment is display only: an unflagged row's text is the model's label."
  (umbriel-buffer-test--with-fixture-render
    (let ((window (umbriel-window "w-def")))
      (should (equal (concat "  " (umbriel-model-window-label window))
                     (umbriel-buffer-test--line-text
                      (umbriel-buffer-test--find 'umbriel-window "w-def")))))))

(ert-deftest umbriel-buffer/long-cell-is-cut-to-its-column ()
  "An over-long title ends in an ellipsis, and sets no wider column."
  (umbriel-buffer-test--with-fixture-render
    (cl-letf (((symbol-function 'umbriel-buffer--width) #'ignore))
      (umbriel-buffer-test--render
       (list (umbriel-buffer-test--window :id "w-long" :workspace "DP-1:3"
                                             :app_id "foot"
                                             :title (make-string 200 ?x)))
       (umbriel-workspaces)))
    (let ((row (umbriel-buffer-test--find 'umbriel-window "w-long"))
          (limit (alist-get 'title umbriel-buffer-column-limits)))
      (should (string-match-p "x…" (umbriel-buffer-test--line-text row)))
      (should (= (+ 2 (length "foot") 2 limit 2)
                 (cadr (umbriel-buffer-test--stops row)))))))

(ert-deftest umbriel-buffer/workspace-columns-skip-an-absent-name ()
  "An unnamed workspace's layout still lines up with a named one's."
  (umbriel-buffer-test--with-fixture-render
    (let ((named (umbriel-buffer-test--stops
                  (umbriel-buffer-test--find 'umbriel-workspace "DP-1:3")))
          (unnamed (umbriel-buffer-test--stops
                    (umbriel-buffer-test--find 'umbriel-workspace
                                                  "HDMI-A-1:3"))))
      (should (= (nth 1 named) (nth 0 unnamed))))))

(ert-deftest umbriel-buffer/workspace-marks-line-up-with-window-flags ()
  "A heading's marks start where the window rows' flags do."
  (umbriel-buffer-test--with-fixture-render
    (umbriel-buffer-test--render
     (list (umbriel-buffer-test--window :id "w-wide" :workspace "DP-1:3"
                                           :title (make-string 40 ?t)
                                           :active t))
     (list (umbriel-buffer-test--workspace :id "DP-1:3" :active t)))
    (should (= (car (last (umbriel-buffer-test--stops
                           (umbriel-buffer-test--find 'umbriel-workspace
                                                         "DP-1:3"))))
               (car (last (umbriel-buffer-test--stops
                           (umbriel-buffer-test--find 'umbriel-window
                                                         "w-wide"))))))))

(ert-deftest umbriel-buffer/wide-heading-keeps-its-marks-clear ()
  "Marks stay past the heading's own cells when those outrun the flags."
  (umbriel-buffer-test--with-fixture-render
    (let ((name (make-string 60 ?n)))
      (umbriel-buffer-test--render
       (umbriel-windows)
       (list (umbriel-buffer-test--workspace
              :id "DP-1:3" :name name :index 3 :output "DP-1" :active t)))
      (should (> (car (last (umbriel-buffer-test--stops
                             (umbriel-buffer-test--find 'umbriel-workspace
                                                           "DP-1:3"))))
                 (length name))))))

(ert-deftest umbriel-buffer/window-cells-have-their-column-faces ()
  "App, placement and flags are coloured; the title keeps the default."
  (umbriel-buffer-test--with-fixture-render
    (let* ((row (umbriel-buffer-test--find 'umbriel-window "w-def"))
           (start (oref row start))
           (face-of (lambda (text)
                      (save-excursion
                        (goto-char start)
                        (search-forward text)
                        (umbriel-buffer-test--faces-at (match-beginning 0))))))
      (should (equal '(umbriel-buffer-window-app)
                     (funcall face-of "emacs")))
      (should (equal '(umbriel-buffer-window-placement)
                     (funcall face-of "tile 1,0")))
      (should-not (car (funcall face-of "notes.org"))))
    (let ((row (umbriel-buffer-test--find 'umbriel-window "w-abc")))
      (save-excursion
        (goto-char (oref row start))
        (let ((case-fold-search nil))
          (search-forward "A"))
        (should (equal '(umbriel-buffer-window-flags)
                       (umbriel-buffer-test--faces-at
                        (match-beginning 0))))))))

(ert-deftest umbriel-buffer/shade-keeps-the-cell-faces ()
  "A shaded row keeps its column colours and gains the shade."
  (umbriel-buffer-test--with-fixture-render
    (umbriel-buffer-test--activate-workspace "DP-1:3")
    (should (equal '(umbriel-buffer-window-app
                     umbriel-buffer-active-workspace-window)
                   (umbriel-buffer-test--faces-at
                    (+ 2 (oref (umbriel-buffer-test--find 'umbriel-window
                                                             "w-def")
                               start)))))))

;;;; header (VT-2, EX-2)

(ert-deftest umbriel-buffer/header-names-the-stream ()
  "VT-2/EX-2: open and never get their sentence, each failure its message."
  (should (equal "umbriel: stream open"
                 (umbriel-buffer--header-text 'open 0)))
  (should (equal "umbriel: stream never opened"
                 (umbriel-buffer--header-text 'never 0)))
  (dolist (kind '(no-path absent refused closed rejected malformed))
    (let* ((failure (list :failure kind (format "%s: test detail" kind)))
           (text (umbriel-buffer--header-text failure 0)))
      (should (stringp text))
      (should (string-prefix-p (format "umbriel: %s" kind) text))
      (should (equal (umbriel-failure-message failure) text))
      (should-not (string-match-p "\n" text)))))

(ert-deftest umbriel-buffer/header-shows-skipped-lines ()
  "VT-2: the skipped count appears only when non-zero, singular for one."
  (let ((none (umbriel-buffer--header-text 'open 0))
        (one (umbriel-buffer--header-text 'open 1))
        (three (umbriel-buffer--header-text 'open 3)))
    (should-not (string-match-p "skipped" none))
    (should (string-prefix-p "umbriel: stream open" one))
    (should (string-match-p "1 line skipped" one))
    (should-not (string-match-p "1 lines skipped" one))
    (should (string-prefix-p "umbriel: stream open" three))
    (should (string-match-p "3 lines skipped" three))))

(ert-deftest umbriel-buffer/header-doubles-percent ()
  "VT-2/EX-2: a compositor `%' is doubled, so the display shows one."
  (let* ((failure (list :failure 'closed "50% of the docs"))
         (text (umbriel-buffer--header-text failure 0)))
    (should (stringp text))
    (should (string-match-p "50%%" text))
    (should (umbriel-buffer-test--percent-pairs-p text))
    (should (equal (umbriel-buffer-test--doubled
                    (umbriel-failure-message failure))
                   text)))
  (umbriel-buffer-test--with-state
      (umbriel-buffer-test--held nil nil '(:failure closed "50% of the docs") 0)
    (let ((buffer (umbriel-buffer-test--mode-buffer)))
      (with-current-buffer buffer
        ;; The mode installs `:eval' so the header reads live client state.
        (should (consp header-line-format))
        (should (eq (car header-line-format) :eval))
        (let ((rendered (eval (nth 1 header-line-format) t)))
          (should (stringp rendered))
          (should (string-match-p "50%%" rendered))
          (should-not (string-match-p "50%[^%]" rendered))
          (should (umbriel-buffer-test--percent-pairs-p rendered)))))))

;;;; throttle (VT-3, EX-3)

(ert-deftest umbriel-buffer/throttle-coalesces-state-changes ()
  "VT-3/EX-3/F-11: five changes ride one `run-at-time'; a later one adds one."
  (umbriel-buffer-test--with-state
      (umbriel-buffer-test--held (umbriel-buffer-test--fixture-windows)
                                    (umbriel-buffer-test--fixture-workspaces))
    (let ((buffer (umbriel-buffer-test--mode-buffer)))
      (umbriel-buffer-test--with-timers calls
        (umbriel-buffer-test--with-repaint-count repaints
          (dotimes (_ 5)
            (umbriel-buffer--schedule-repaint))
          (should (= 1 (length calls)))
          (let ((call (car calls)))
            (should (equal umbriel-buffer-repaint-delay (nth 0 call)))
            (should (equal 0.1 (nth 0 call)))
            (should-not (nth 1 call))
            (should (eq (nth 2 call) #'umbriel-buffer--repaint-due)))
          ;; Nothing repaints until the timer fires.
          (should (= 0 repaints))
          ;; Displayed: the pending repaint repaints exactly once.
          (set-window-buffer (selected-window) buffer)
          (funcall (nth 2 (car calls)))
          (should (= 1 repaints))
          (should-not umbriel-buffer--repaint-timer)
          ;; A change after the fire schedules exactly one more.
          (umbriel-buffer--schedule-repaint)
          (should (= 2 (length calls)))
          (funcall (nth 2 (car (cdr calls))))
          (should (= 2 repaints)))
        ;; Leave no faked timer behind for the kill hook.
        (setq umbriel-buffer--repaint-timer nil)))))

(ert-deftest umbriel-buffer/throttle-hooks-the-state-changed-hook ()
  "VT-3: the mode adds one hook entry, and one hook run schedules one timer."
  (umbriel-buffer-test--with-state
      (umbriel-buffer-test--held (umbriel-buffer-test--fixture-windows)
                                    (umbriel-buffer-test--fixture-workspaces))
    (umbriel-buffer-test--mode-buffer)
    ;; Entering the mode again does not add a second entry.
    (umbriel-buffer-test--mode-buffer)
    (should (= 1 (seq-count (lambda (function)
                              (eq function #'umbriel-buffer--schedule-repaint))
                            umbriel-state-changed-hook)))
    (umbriel-buffer-test--with-timers calls
      (dotimes (_ 3)
        (run-hooks 'umbriel-state-changed-hook))
      (should (= 1 (length calls)))
      (should (equal umbriel-buffer-repaint-delay (nth 0 (car calls))))
      (setq umbriel-buffer--repaint-timer nil))))

;;;; point, folds and stale (VT-4, EX-4)

(ert-deftest umbriel-buffer/repaint-keeps-point-across-a-reorder ()
  "VT-4a/EX-4: point stays on the same window section when the row moves."
  (umbriel-buffer-test--with-state
      (umbriel-buffer-test--held (umbriel-buffer-test--order-windows 0 0)
                                    (umbriel-buffer-test--point-workspaces))
    (let ((buffer (umbriel-buffer-test--mode-buffer)))
      (set-window-buffer (selected-window) buffer)
      (with-current-buffer buffer
        (umbriel-buffer--repaint)
        (goto-char (oref (umbriel-buffer-test--find 'umbriel-window "w-b")
                         start))
        (should (equal "w-b" (oref (magit-current-section) value)))
        (let ((before (line-number-at-pos)))
          ;; w-b moves from the first row to the last.
          (setq umbriel--state
                (umbriel-buffer-test--held
                 (umbriel-buffer-test--order-windows 3 0)
                 (umbriel-buffer-test--point-workspaces)))
          (umbriel-buffer--repaint)
          (should (equal "w-b" (oref (magit-current-section) value)))
          (should (/= before (line-number-at-pos))))))))

(ert-deftest umbriel-buffer/repaint-keeps-a-non-selected-windows-point ()
  "VT-4b/C-5: a window that does not have the buffer selected keeps its point."
  (umbriel-buffer-test--with-state
      (umbriel-buffer-test--held (umbriel-buffer-test--order-windows 0 0)
                                    (umbriel-buffer-test--point-workspaces))
    (let* ((buffer (umbriel-buffer-test--mode-buffer))
           (other (split-window (selected-window))))
      (set-window-buffer other buffer)
      (with-current-buffer buffer
        (umbriel-buffer--repaint))
      (set-window-point other
                        (with-current-buffer buffer
                          (oref (umbriel-buffer-test--find 'umbriel-window "w-b")
                                start)))
      ;; Keep the selected window on another buffer.
      (set-window-buffer (selected-window)
                         (get-buffer-create "*umbriel-test-other*"))
      (should-not (eq (selected-window) other))
      (should (eq (window-buffer other) buffer))
      (setq umbriel--state
            (umbriel-buffer-test--held
             (umbriel-buffer-test--order-windows 3 0)
             (umbriel-buffer-test--point-workspaces)))
      (umbriel-buffer--repaint)
      (let* ((section (with-current-buffer buffer
                        (umbriel-buffer-test--find 'umbriel-window "w-b")))
             (point (window-point other)))
        (should (>= point (oref section start)))
        (should (< point (oref section end)))
        (should (= point (oref section start)))))))

(ert-deftest umbriel-buffer/repaint-keeps-a-folded-workspace-folded ()
  "VT-4c/EX-4: hiding a workspace survives a repaint, through magit's cache."
  (umbriel-buffer-test--with-state
      (umbriel-buffer-test--held (umbriel-buffer-test--fixture-windows)
                                    (umbriel-buffer-test--fixture-workspaces))
    (let ((buffer (umbriel-buffer-test--mode-buffer)))
      (with-current-buffer buffer
        (umbriel-buffer--repaint)
        (let ((before (umbriel-buffer-test--find 'umbriel-workspace "DP-1:3")))
          (magit-section-hide before)
          (should (oref before hidden))
          (umbriel-buffer--repaint)
          (let ((after (umbriel-buffer-test--find 'umbriel-workspace "DP-1:3")))
            (should after)
            (should-not (eq after before))
            (should (oref after hidden))
            ;; R-1: a real fold, not just the slot: the body is invisible
            ;; and the heading is not.
            (let ((child (car (umbriel-buffer-test--children after))))
              (should child)
              (should (invisible-p (oref child start)))
              (should (invisible-p (oref after content)))
              (should-not (invisible-p (oref after start))))))))))

(ert-deftest umbriel-buffer/undisplayed-buffer-goes-stale ()
  "VT-4d/C-6: a repaint due while nothing shows the buffer only marks it."
  (umbriel-buffer-test--with-state
      (umbriel-buffer-test--held (umbriel-buffer-test--fixture-windows)
                                    (umbriel-buffer-test--fixture-workspaces))
    (let ((buffer (umbriel-buffer-test--mode-buffer)))
      (should-not (get-buffer-window buffer t))
      (let ((text (with-current-buffer buffer
                    (umbriel-buffer-test--text))))
        (umbriel-buffer-test--with-timers calls
          (umbriel-buffer-test--with-repaint-count repaints
            (umbriel-buffer--repaint-due)
            (should (= 0 repaints))
            (should (buffer-local-value 'umbriel-buffer--stale buffer))
            (should (local-variable-p 'umbriel-buffer--stale buffer))
            ;; Marking stale does not edit the buffer.
            (should (equal text (with-current-buffer buffer
                                  (umbriel-buffer-test--text)))))
          (should-not calls)
          (setq umbriel-buffer--repaint-timer nil))))))

(ert-deftest umbriel-buffer/display-hook-schedules-without-editing ()
  "VT-4d/C-7: showing a stale buffer schedules; only the timer repaints."
  (umbriel-buffer-test--with-state
      (umbriel-buffer-test--held (umbriel-buffer-test--fixture-windows)
                                    (umbriel-buffer-test--fixture-workspaces))
    (let ((buffer (umbriel-buffer-test--mode-buffer)))
      (with-current-buffer buffer
        (umbriel-buffer--repaint-due))
      (should (buffer-local-value 'umbriel-buffer--stale buffer))
      (let ((text (with-current-buffer buffer
                    (umbriel-buffer-test--text))))
        (umbriel-buffer-test--with-timers calls
          (let ((window (selected-window)))
            (set-window-buffer window buffer)
            (should (local-variable-p 'window-buffer-change-functions buffer))
            (let ((entry (seq-find
                          (lambda (function)
                            (eq function #'umbriel-buffer--on-display))
                          (buffer-local-value 'window-buffer-change-functions
                                              buffer))))
              (should entry)
              ;; The display engine runs the hook with the buffer current.
              (umbriel-buffer-test--with-repaint-count repaints
                (with-current-buffer buffer
                  (funcall entry window))
                (should (= 1 (length calls)))
                (should (= 0 repaints))
                (should (equal text (with-current-buffer buffer
                                      (umbriel-buffer-test--text)))))))
          ;; Firing the scheduled repaint renders once and clears stale.
          (funcall (nth 2 (car calls)))
          (with-current-buffer buffer
            ;; Rendered now: the row is a section whose value is the id.
            (should-not (equal "" (umbriel-buffer-test--text)))
            (should (umbriel-buffer-test--find 'umbriel-window "w-abc"))
            (should-not (buffer-local-value 'umbriel-buffer--stale buffer)))
          (setq umbriel-buffer--repaint-timer nil))))))

;;;; no connection (VT-5, EX-5, F-14)

(ert-deftest umbriel-buffer/mode-and-render-never-connect ()
  "VT-5/EX-5: the mode, the render path and the header open no stream."
  (umbriel-buffer-test--with-state
      (umbriel-buffer-test--held (umbriel-buffer-test--fixture-windows)
                                    (umbriel-buffer-test--fixture-workspaces))
    (should (equal umbriel-buffer-test--socket (getenv "UMBRIEL_SOCKET")))
    (should-not (getenv "WAYLAND_DISPLAY"))
    (let ((forbidden (lambda (&rest _)
                       (ert-fail "the display path opened a stream")))
          (buffer-nil (get-buffer-create umbriel-buffer-name)))
      (unwind-protect
          (cl-letf (((symbol-function 'umbriel-stream-open) forbidden)
                    ((symbol-function 'make-network-process) forbidden))
            (with-current-buffer buffer-nil
              (umbriel-buffer-mode)
              (let ((inhibit-read-only t))
                (erase-buffer)
                (umbriel-buffer--insert (umbriel-workspaces)
                                           (umbriel-windows)))
              (umbriel-buffer--repaint)
              ;; The header pass, over every status the client can hold.
              (dolist (status (list 'open 'never
                                    (list :failure 'rejected "nope")))
                (setq umbriel--state
                      (umbriel-buffer-test--held
                       (umbriel-buffer-test--fixture-windows)
                       (umbriel-buffer-test--fixture-workspaces)
                       status 2))
                (should (stringp (umbriel-buffer--header-text
                                  (umbriel-stream-status)
                                  (umbriel-skipped))))
                (should (stringp (eval (nth 1 header-line-format) t))))))
        (when (buffer-live-p buffer-nil)
          (kill-buffer buffer-nil))))
    ;; The counting stubs saw nothing either.
    (should-not umbriel-buffer-test--stream-log)))

;;;; lifecycle (VT-6, EX-7, F-2)

(ert-deftest umbriel-buffer/umbriel-opens-once-and-kill-closes-once ()
  "VT-6/EX-7: creating opens once; bury, quit and re-invoke keep it."
  (umbriel-buffer-test--with-state nil
    (umbriel-buffer-test--with-timers calls
      (umbriel)
      (should (= 1 (umbriel-buffer-test--count 'open)))
      (let ((buffer (get-buffer umbriel-buffer-name)))
        (should buffer)
        (should (eq (buffer-local-value 'major-mode buffer)
                    'umbriel-buffer-mode))
        (should (get-buffer-window buffer t))
        ;; C-9/D-1: the kill hook is buffer-local, never a global default.
        (should (memq #'umbriel-buffer--on-kill
                      (buffer-local-value 'kill-buffer-hook buffer)))
        (should-not (memq #'umbriel-buffer--on-kill
                          (default-value 'kill-buffer-hook)))
        ;; Re-invoking opens nothing more.
        (umbriel)
        (should (= 1 (umbriel-buffer-test--count 'open)))
        ;; Burying keeps the connection.
        (bury-buffer buffer)
        (should (buffer-live-p buffer))
        (should (= 0 (umbriel-buffer-test--count 'close)))
        ;; Quitting its window keeps it too.
        (umbriel)
        (quit-window)
        (should (= 0 (umbriel-buffer-test--count 'close)))
        (should (= 1 (umbriel-buffer-test--count 'open)))
        ;; Killing an unrelated buffer is not our business.
        (kill-buffer (get-buffer-create "*umbriel-test-unrelated*"))
        (should (= 0 (umbriel-buffer-test--count 'close)))
        ;; Killing *umbriel* closes exactly once.
        (kill-buffer buffer)
        (should (= 1 (umbriel-buffer-test--count 'close))))
      (should-not calls))))

(ert-deftest umbriel-buffer/refresh-reconnects-only-when-shut ()
  "VT-6/C-10: `g' repaints when the stream is open, reconnects when not."
  (umbriel-buffer-test--with-state (umbriel-buffer-test--held nil nil 'open 0)
    (umbriel-buffer-test--mode-buffer)
    (umbriel-buffer-test--with-timers calls
      (umbriel-buffer-test--with-repaint-count repaints
        (cl-letf (((symbol-function 'umbriel-send)
                   (lambda (&rest _)
                     (ert-fail "refresh on an open stream sent a request"))))
          (umbriel-buffer-refresh))
        (should (= 1 repaints))
        (should (= 0 (umbriel-buffer-test--count 'open))))
      (setq umbriel-buffer--repaint-timer nil)))
  (dolist (status (list 'never (list :failure 'closed "x")))
    (umbriel-buffer-test--with-state
        (umbriel-buffer-test--held nil nil status 0)
      (umbriel-buffer-test--mode-buffer)
      (umbriel-buffer-test--with-timers calls
        (umbriel-buffer-refresh)
        (should (= 1 (umbriel-buffer-test--count 'open)))
        (should (= 0 (umbriel-buffer-test--count 'close)))
        (setq umbriel-buffer--repaint-timer nil)))))

(ert-deftest umbriel-buffer/g-refreshes ()
  "VT-6/C-11: `g' in the buffer map is `umbriel-buffer-refresh'."
  (should (eq (lookup-key umbriel-buffer-mode-map "g")
              #'umbriel-buffer-refresh)))

(ert-deftest umbriel-buffer/mode-and-constants ()
  "C-1/C-2: the buffer name, the repaint delay and the mode's parent."
  (umbriel-buffer-test--with-state nil
    (should (equal "*umbriel*" umbriel-buffer-name))
    (should (equal 0.1 umbriel-buffer-repaint-delay))
    (let ((buffer (umbriel-buffer-test--mode-buffer)))
      (with-current-buffer buffer
        (should (eq major-mode 'umbriel-buffer-mode))
        (should (derived-mode-p 'magit-section-mode))
        (should (local-variable-p 'window-buffer-change-functions))
        (should (eq (car-safe header-line-format) :eval))))))

;;;; pending timers and a dead buffer (C-6, C-9)

(ert-deftest umbriel-buffer/kill-cancels-a-pending-repaint ()
  "C-9: killing *umbriel* cancels its pending repaint, exactly once."
  (umbriel-buffer-test--with-state nil
    (let ((fake (list 'fake-timer))
          (cancelled nil))
      (umbriel)
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (&rest _) fake))
                ((symbol-function 'cancel-timer)
                 (lambda (timer) (push timer cancelled))))
        (umbriel-buffer--schedule-repaint)
        (should (eq fake umbriel-buffer--repaint-timer))
        (kill-buffer (get-buffer umbriel-buffer-name))
        (should (equal (list fake) cancelled))
        (should-not umbriel-buffer--repaint-timer)
        (should (= 1 (umbriel-buffer-test--count 'close)))))))

(ert-deftest umbriel-buffer/repaint-due-after-kill-is-harmless ()
  "C-6: a repaint due after the buffer died does nothing and signals nothing."
  (umbriel-buffer-test--with-state nil
    (let ((fake (list 'fake-timer))
          (due nil))
      (umbriel)
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (_time _repeat function &rest _)
                   (setq due function)
                   fake))
                ((symbol-function 'cancel-timer)
                 (lambda (&rest _) nil)))
        (umbriel-buffer--schedule-repaint)
        (should due)
        (kill-buffer (get-buffer umbriel-buffer-name))
        (should-not (get-buffer umbriel-buffer-name))
        (umbriel-buffer-test--with-repaint-count repaints
          (funcall due)
          (should (= 0 repaints))
          (should-not umbriel-buffer--repaint-timer))))))

(ert-deftest umbriel-buffer/no-buffer-schedules-nothing ()
  "C-6: with no *umbriel* buffer live, scheduling does nothing."
  (umbriel-buffer-test--with-state nil
    (should-not (get-buffer umbriel-buffer-name))
    (let ((calls 0))
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (&rest _)
                   (cl-incf calls)
                   (list 'fake-timer))))
        (umbriel-buffer--schedule-repaint)
        (should (= 0 calls))
        (should-not umbriel-buffer--repaint-timer)))))

;;;; review fixes (T-3..T-6)

(ert-deftest umbriel-buffer/kill-cancels-after-the-close-announces ()
  "T-3/R-4: the close may run the state hook; the kill still cancels last.
A real `umbriel-stream-close' announces, so a kill that closed first
would see the hook arm a fresh timer; cancelling afterwards leaves none."
  (umbriel-buffer-test--with-state nil
    (let ((fake (list 'fake-timer))
          (events nil))
      (cl-letf (((symbol-function 'umbriel-stream-close)
                 (lambda (&rest _)
                   (push 'close umbriel-buffer-test--stream-log)
                   (push 'close events)
                   (run-hooks 'umbriel-state-changed-hook)))
                ((symbol-function 'run-at-time)
                 (lambda (&rest _)
                   (push 'arm events)
                   fake))
                ((symbol-function 'cancel-timer)
                 (lambda (&rest _) (push 'cancel events))))
        (umbriel)
        (umbriel-buffer--schedule-repaint)
        (should (eq fake umbriel-buffer--repaint-timer))
        (setq events nil)
        (kill-buffer (get-buffer umbriel-buffer-name))
        (setq events (nreverse events))
        (should (memq 'close events))
        (should (memq 'cancel events))
        (should-not (memq 'arm (cdr (memq 'cancel events))))
        (should-not umbriel-buffer--repaint-timer)
        (should (= 1 (umbriel-buffer-test--count 'close)))))))

(ert-deftest umbriel-buffer/repaint-unmodifies-and-highlights ()
  "T-4/R-2/R-3: a repaint leaves no modification and re-highlights point."
  (umbriel-buffer-test--with-state
      (umbriel-buffer-test--held (umbriel-buffer-test--fixture-windows)
                                    (umbriel-buffer-test--fixture-workspaces))
    (let ((buffer (umbriel-buffer-test--mode-buffer)))
      (set-window-buffer (selected-window) buffer)
      (with-current-buffer buffer
        (umbriel-buffer--repaint)
        (goto-char (oref (umbriel-buffer-test--find 'umbriel-window "w-abc")
                         start))
        (umbriel-buffer--repaint)
        (should (equal "w-abc" (oref (magit-current-section) value)))
        (should-not (buffer-modified-p))
        ;; The erase threw the old highlight away; the repaint puts it back.
        (should magit-section-highlight-overlays)
        (should (seq-some (lambda (overlay)
                            (and (eq (overlay-get overlay 'font-lock-face)
                                     'magit-section-highlight)
                                 (<= (overlay-start overlay) (point))
                                 (<= (point) (overlay-end overlay))))
                          magit-section-highlight-overlays))))))

(ert-deftest umbriel-buffer/repaint-keeps-the-window-start ()
  "T-5/R-5: a repaint leaves the window showing the same top section."
  (umbriel-buffer-test--with-state
      (umbriel-buffer-test--held (umbriel-buffer-test--tall-windows)
                                    (umbriel-buffer-test--point-workspaces))
    (let ((buffer (umbriel-buffer-test--mode-buffer)))
      (set-window-buffer (selected-window) buffer)
      (with-current-buffer buffer
        (umbriel-buffer--repaint)
        (let* ((rows (umbriel-buffer-test--children
                      (umbriel-buffer-test--find 'umbriel-workspace "DP-1:3")))
               (top (nth 5 rows))
               (value (oref top value)))
          (should (> (length rows) (window-height)))
          (set-window-start (selected-window) (oref top start))
          (should (= (window-start) (oref top start)))
          (goto-char (oref (nth 7 rows) start))
          (should (> (window-point) (window-start)))
          ;; Grow the tree below the window start: the top row is unchanged.
          (setq umbriel--state
                (umbriel-buffer-test--held
                 (umbriel-buffer-test--tall-windows 3)
                 (umbriel-buffer-test--point-workspaces)))
          (umbriel-buffer--repaint)
          (let ((after (umbriel-buffer-test--find 'umbriel-window value)))
            (should after)
            (should (= (window-start) (oref after start)))
            (should (equal value (oref (magit-section-at (window-start))
                                       value)))))))))

(ert-deftest umbriel-buffer/leaves-a-foreign-buffer-alone ()
  "T-6/R-6: a buffer that merely bears the name is never drawn into."
  (umbriel-buffer-test--with-state nil
    (let ((foreign (get-buffer-create umbriel-buffer-name)))
      (with-current-buffer foreign
        (insert "not umbriel\n"))
      (umbriel-buffer--repaint)
      (with-current-buffer foreign
        (should (equal "not umbriel\n" (umbriel-buffer-test--text)))
        (should-not (eq major-mode 'umbriel-buffer-mode)))
      (umbriel-buffer-test--with-timers calls
        (umbriel-buffer--schedule-repaint)
        (should-not calls)))))

;;;; actions (PHASE-05)
;;
;; Contract: emacs-config:SL-016 phase sheet 05 (Contract, Gates, D-1..D-8) and
;; its design sections Actions and Keys.
;;
;; Nothing here reaches a socket either.  Where a test only wants the plan a
;; command built, `umbriel-send-all' is faked; where a test wants the real
;; sequencer, `umbriel-send' is faked in its place.  `--with-state' already
;; points UMBRIEL_SOCKET at a path nothing listens on.

;;;; actions: fixtures

(defun umbriel-buffer-test--action-workspaces ()
  "Three workspaces on two outputs, the DP-1 one the focused one."
  (list (umbriel-buffer-test--workspace :id "DP-1:1" :index 1 :name "web"
                                           :output "DP-1" :focused t)
        (umbriel-buffer-test--workspace :id "DP-1:2" :index 2 :name nil
                                           :named nil :output "DP-1")
        (umbriel-buffer-test--workspace :id "DP-2:1" :index 1 :name "docs"
                                           :output "DP-2")))

(defun umbriel-buffer-test--action-windows ()
  "One window per output, and one whose workspace joins nothing."
  (list (umbriel-buffer-test--window :id "w-dp1" :workspace "DP-1:1"
                                        :app_id "foot" :title "shell")
        (umbriel-buffer-test--window :id "w-dp2" :workspace "DP-2:1"
                                        :app_id "emacs" :title "notes")
        (umbriel-buffer-test--window :id "w-loose" :workspace "DP-9:9"
                                        :app_id "firefox" :title "Docs")))

(defun umbriel-buffer-test--action-workspaces-focusing (id)
  "The action workspaces, the focused one's `focused_window' set to ID.
ID is stored as given: a window id, \"\" or nil.  The plain action
workspaces carry no `focused_window' at all, which emacs-config:DEC-025 reads as
unknown."
  (mapcar (lambda (workspace)
            (if (alist-get 'focused workspace)
                (cons (cons 'focused_window id)
                      (assq-delete-all 'focused_window workspace))
              workspace))
          (umbriel-buffer-test--action-workspaces)))

(defconst umbriel-buffer-test--action-table
  '((umbriel-focus "window-focus" window)
    (umbriel-toggle-floating "window-toggle-floating" window)
    (umbriel-kill "window-kill" window confirm-kill)
    (umbriel-center "window-center" focused)
    (umbriel-cycle-extent "window-cycle-primary-extent" focused)
    (umbriel-cycle-extent-back "window-cycle-primary-extent-back" focused)
    (umbriel-column-left "column-move-left" focused)
    (umbriel-column-right "column-move-right" focused)
    (umbriel-move-to-workspace "window-move-to-workspace-silent" focused
                                  workspace)
    (umbriel-switch "workspace-switch" workspace pad-workspace)
    (umbriel-set-layout "workspace-set-layout" active-workspace layout)
    (umbriel-toggle-scratchpad "scratchpad-toggle" scratchpad)
    (umbriel-send-to-pad "window-move-to-scratchpad" pointer-window pad))
  "The design's action table, as this file expects it to be held.
emacs-config:DEC-025: moving a window to a workspace goes silently, so the
plan's own focus steps are the only ones the user sees.")

;;;; actions: helpers

(defun umbriel-buffer-test--hold (windows workspaces &optional status skipped)
  "Replace the held state with WINDOWS, WORKSPACES, STATUS and SKIPPED."
  (setq umbriel--state
        (umbriel-buffer-test--held windows workspaces status skipped)))

(defun umbriel-buffer-test--spec (addressing action &optional prompt)
  "A table row for ACTION with ADDRESSING, prompting with PROMPT if given."
  (append (list 'test-command action addressing)
          (when prompt (list prompt))))

(defun umbriel-buffer-test--mentions-p (text needle)
  "Non-nil when TEXT mentions NEEDLE, ignoring case."
  (and (stringp text)
       (let ((case-fold-search t))
         (string-match-p (regexp-quote needle) text))))

(defun umbriel-buffer-test--refused (spec target fields workspaces)
  "The refusal `umbriel-buffer--plan' gives SPEC for TARGET.
Fails unless the answer is a string and the same string whether FIELDS
is given or nil: a refusal may not depend on FIELDS (D-3)."
  (let ((with (umbriel-buffer--plan spec target fields workspaces))
        (without (umbriel-buffer--plan spec target nil workspaces)))
    (should (stringp with))
    (should (equal with without))
    with))

(defun umbriel-buffer-test--offered (collection)
  "The strings COLLECTION offers, whether it holds strings or pairs."
  (all-completions "" collection))

(defun umbriel-buffer-test--metadata (collection key)
  "What COLLECTION's completion metadata says under KEY.
COLLECTION must be a completion-table function: vertico reads its
`metadata' action and prefers it over `vertico-sort-function', which an
alist or a plain list of strings cannot answer at all."
  (unless (functionp collection)
    (ert-fail (format "the prompt's collection is not a completion table: %S"
                      collection)))
  (alist-get key (funcall collection "" nil 'metadata)))

(defun umbriel-buffer-test--same-set-p (one other)
  "Non-nil when lists ONE and OTHER hold the same elements, in any order."
  (and (null (seq-difference one other))
       (null (seq-difference other one))))

(defun umbriel-buffer-test--switches-p (result)
  "Non-nil when RESULT is a request list holding a `workspace-switch'."
  (and (listp result)
       (seq-some (lambda (request)
                   (equal (alist-get 'action request) "workspace-switch"))
                 result)))

(defun umbriel-buffer-test--call-on-row (command type value)
  "Call COMMAND with point on the TYPE section whose value is VALUE."
  (let ((section (umbriel-buffer-test--find type value)))
    (unless section
      (ert-fail (format "no %s section whose value is %S" type value)))
    (goto-char (oref section start))
    (call-interactively command)))

(defmacro umbriel-buffer-test--with-scheduled (calls &rest body)
  "Run BODY with CALLS collecting the timers the code under test schedules.
A `run-at-time' with a delay of 0 and no repeat -- how a reply leaves a
process callback -- is recorded in CALLS as `(TIME REPEAT FUNCTION
ARGS)' and answered with a faked timer.  Every other call, the repaint
throttle included, goes to the original function; `cancel-timer' forgets
our timers and passes foreign ones through."
  (declare (indent 1))
  `(let ((,calls nil)
         (umbriel-buffer-test--fake-timers nil)
         (orig-at-time (symbol-function 'run-at-time))
         (orig-cancel (symbol-function 'cancel-timer)))
     (cl-letf (((symbol-function 'run-at-time)
                (lambda (time repeat function &rest args)
                  (if (and (null repeat) (equal time 0))
                      (let ((timer (list 'fake-timer time repeat)))
                        (push (list time repeat function args) ,calls)
                        (push timer umbriel-buffer-test--fake-timers)
                        timer)
                    (apply orig-at-time time repeat function args))))
               ((symbol-function 'cancel-timer)
                (lambda (timer)
                  (if (memq timer umbriel-buffer-test--fake-timers)
                      nil
                    (funcall orig-cancel timer)))))
       ,@body)))

;;;; actions: the plan (VT-1, EX-1, D-1..D-3)

(ert-deftest umbriel-buffer/plan-window-addressing ()
  "VT-1/EX-1: `window' acts on the row's id and refuses a workspace row."
  (let* ((workspaces (umbriel-buffer-test--action-workspaces))
         (windows (umbriel-buffer-test--action-windows))
         (window (nth 0 windows))
         (loose (nth 2 windows))
         (id (alist-get 'id window))
         (spec (umbriel-buffer-test--spec 'window "window-toggle-floating"))
         (fields '((quiet . t)))
         (expected (lambda (window-id extra)
                     (list (umbriel-action
                            "window-toggle-floating"
                            (append (list (cons 'window window-id)) extra))))))
    (should (equal (funcall expected id fields)
                   (umbriel-buffer--plan spec (list :window window) fields
                                            workspaces)))
    (should (equal (funcall expected id nil)
                   (umbriel-buffer--plan spec (list :window window) nil
                                            workspaces)))
    ;; An Unassigned window is still a window: the id addresses it.
    (should (equal (funcall expected (alist-get 'id loose) nil)
                   (umbriel-buffer--plan spec (list :window loose) nil
                                            workspaces)))
    ;; `umbriel-focus' on a window row is exactly one request.
    (should (equal (list (umbriel-action "window-focus"
                                            (list (cons 'window id))))
                   (umbriel-buffer--plan
                    (umbriel-buffer-test--spec 'window "window-focus")
                    (list :window window) nil workspaces)))
    ;; A workspace row is not a window.
    (let ((refusal (umbriel-buffer-test--refused
                    spec (list :workspace (nth 0 workspaces)) fields workspaces)))
      (should (umbriel-buffer-test--mentions-p refusal "not a window")))))

(ert-deftest umbriel-buffer/plan-focused-addressing ()
  "VT-1/EX-1: `focused' focuses the row's window first; a workspace row acts."
  (let* ((workspaces (umbriel-buffer-test--action-workspaces))
         (windows (umbriel-buffer-test--action-windows))
         (window (nth 0 windows))
         (loose (nth 2 windows))
         (spec (umbriel-buffer-test--spec 'focused "window-center"))
         (fields '((step . 1))))
    (should (equal (list (umbriel-action "window-focus"
                                            (list (cons 'window (alist-get 'id window))))
                         (umbriel-action "window-center" fields))
                   (umbriel-buffer--plan spec (list :window window) fields
                                            workspaces)))
    (should (equal (list (umbriel-action
                          "window-focus"
                          (list (cons 'window (alist-get 'id loose))))
                         (umbriel-action "window-center" nil))
                   (umbriel-buffer--plan spec (list :window loose) nil
                                            workspaces)))
    (should (equal (list (umbriel-action "window-center" fields))
                   (umbriel-buffer--plan spec
                                            (list :workspace (nth 0 workspaces))
                                            fields workspaces)))
    (should (equal (list (umbriel-action "window-center" nil))
                   (umbriel-buffer--plan spec
                                            (list :workspace (nth 0 workspaces))
                                            nil workspaces)))))

(ert-deftest umbriel-buffer/plan-workspace-addressing ()
  "VT-1/EX-1: `workspace' switches by selector; an Unassigned window refuses."
  (let* ((workspaces (umbriel-buffer-test--action-workspaces))
         (windows (umbriel-buffer-test--action-windows))
         (window (nth 0 windows))
         (loose (nth 2 windows))
         (spec (umbriel-buffer-test--spec 'workspace "workspace-switch"))
         (its (umbriel-model-window-workspace workspaces window)))
    (should its)
    (should (equal (list (umbriel-action
                          "workspace-switch"
                          (umbriel-model-workspace-selector its)))
                   (umbriel-buffer--plan spec (list :window window) nil
                                            workspaces)))
    (should (equal (list (umbriel-action
                          "workspace-switch"
                          (umbriel-model-workspace-selector (nth 1 workspaces))))
                   (umbriel-buffer--plan spec
                                            (list :workspace (nth 1 workspaces))
                                            nil workspaces)))
    ;; The switch request carries the selector and nothing else.
    (should (equal (list (umbriel-action
                          "workspace-switch"
                          (umbriel-model-workspace-selector its)))
                   (umbriel-buffer--plan spec (list :window window)
                                            '((layout . "master")) workspaces)))
    (let ((refusal (umbriel-buffer-test--refused
                    spec (list :window loose) nil workspaces)))
      (should (umbriel-buffer-test--mentions-p refusal "no workspace")))))

(ert-deftest umbriel-buffer/plan-active-workspace-addressing ()
  "VH-1 Fix 1: `active-workspace' switches there, unless that is the
focused workspace, in which case it only acts.  Switching to a workspace
umbriel already holds active on its output toggles back to the previous
one when the user sets `back_and_forth', so the layout would land on the
wrong workspace."
  (let* ((workspaces (umbriel-buffer-test--action-workspaces))
         (windows (umbriel-buffer-test--action-windows))
         (focused (nth 0 workspaces))
         (other (nth 1 workspaces))
         (window (nth 0 windows))
         (its (umbriel-model-window-workspace workspaces window))
         (spec (umbriel-buffer-test--spec 'active-workspace
                                             "workspace-set-layout" 'layout))
         (fields '((layout . "master"))))
    (should its)
    ;; Window 0 sits on the focused DP-1:1: act, do not switch.
    (should (equal (alist-get 'id focused) (alist-get 'id its)))
    (should (equal (list (umbriel-action "workspace-set-layout" fields))
                   (umbriel-buffer--plan spec (list :window window) fields
                                            workspaces)))
    ;; A workspace row on another workspace of the same output switches.
    (should (equal (list (umbriel-action
                          "workspace-switch"
                          (umbriel-model-workspace-selector other))
                         (umbriel-action "workspace-set-layout" fields))
                   (umbriel-buffer--plan spec (list :workspace other)
                                            fields workspaces)))
    (let ((refusal (umbriel-buffer-test--refused
                    spec (list :window (nth 2 windows)) fields workspaces)))
      (should (umbriel-buffer-test--mentions-p refusal "no workspace")))))

(ert-deftest umbriel-buffer/plan-active-workspace-on-the-focused-row ()
  "VH-1 Fix 1: a `:workspace' row of the focused workspace plans [ACT] alone."
  (let* ((workspaces (umbriel-buffer-test--action-workspaces))
         (focused (umbriel-buffer--focused-workspace workspaces))
         (spec (umbriel-buffer-test--spec 'active-workspace
                                             "workspace-set-layout" 'layout))
         (fields '((layout . "master"))))
    (should focused)
    (dolist (given (list fields nil))
      (let ((plan (umbriel-buffer--plan spec (list :workspace focused) given
                                           workspaces)))
        (should (equal (list (umbriel-action "workspace-set-layout" given))
                       plan))
        (should-not (umbriel-buffer-test--switches-p plan))))))

(ert-deftest umbriel-buffer/plan-workspace-addressing-switches-the-focused-row ()
  "VH-1 Fix 1: `workspace' addressing is unchanged; it always switches,
the focused workspace included (user ruling)."
  (let* ((workspaces (umbriel-buffer-test--action-workspaces))
         (windows (umbriel-buffer-test--action-windows))
         (focused (nth 0 workspaces))
         (spec (umbriel-buffer-test--spec 'workspace "workspace-switch")))
    (dolist (target (list (list :workspace focused)
                          (list :window (nth 0 windows))))
      (let ((plan (umbriel-buffer--plan spec target nil workspaces)))
        (should (equal (list (umbriel-action
                              "workspace-switch"
                              (umbriel-model-workspace-selector focused)))
                       plan))
        (should (umbriel-buffer-test--switches-p plan))))))

(ert-deftest umbriel-buffer/plan-layout-refuses-a-foreign-output ()
  "VT-2/EX-2/emacs-config:RV-018 F-6: a layout off the pointer's output is refused.
The refusal is a string with no requests in it -- never a partial plan."
  (let* ((workspaces (umbriel-buffer-test--action-workspaces))
         (windows (umbriel-buffer-test--action-windows))
         (dp2 (nth 2 workspaces))
         (spec (umbriel-buffer-test--spec 'active-workspace
                                             "workspace-set-layout" 'layout))
         (fields '((layout . "master"))))
    (dolist (target (list (list :workspace dp2)
                          (list :window (nth 1 windows))))
      (let ((result (umbriel-buffer-test--refused
                     spec target fields workspaces)))
        (should (umbriel-buffer-test--mentions-p
                 result "layout acts on the output under the pointer"))
        (should-not (umbriel-buffer-test--switches-p result))))
    ;; The same output is fine: switch first, then set the layout.
    (should (equal (list (umbriel-action
                          "workspace-switch"
                          (umbriel-model-workspace-selector (nth 1 workspaces)))
                         (umbriel-action "workspace-set-layout" fields))
                   (umbriel-buffer--plan spec
                                            (list :workspace (nth 1 workspaces))
                                            fields workspaces)))
    ;; D-2: nothing is marked focused, so the output cannot be checked.
    (let* ((unfocused
            (list (umbriel-buffer-test--workspace :id "DP-1:1" :index 1
                                                     :output "DP-1")
                  (umbriel-buffer-test--workspace :id "DP-1:2" :index 2
                                                     :output "DP-1")))
           (result (umbriel-buffer-test--refused
                    spec (list :workspace (nth 0 unfocused))
                    fields unfocused)))
      (should (umbriel-buffer-test--mentions-p
               result "layout acts on the output under the pointer"))
      (should-not (umbriel-buffer-test--switches-p result)))))

(ert-deftest umbriel-buffer/plan-restores-focus-after-acting ()
  "emacs-config:DEC-025: a `focused' plan on a window row ends by restoring PREVIOUS.
PREVIOUS is the focused workspace's `focused_window'; FIELDS belong to
the action, never to the restore, and the loose window restores too."
  (let* ((workspaces (umbriel-buffer-test--action-workspaces-focusing
                      "w-dp2"))
         (windows (umbriel-buffer-test--action-windows))
         (spec (umbriel-buffer-test--spec 'focused "window-center"))
         (fields '((step . 1)))
         (focus (lambda (id)
                  (umbriel-action "window-focus"
                                     (list (cons 'window id))))))
    (dolist (given (list fields nil))
      (should (equal (list (funcall focus "w-dp1")
                           (umbriel-action "window-center" given)
                           (funcall focus "w-dp2"))
                     (umbriel-buffer--plan spec (list :window (nth 0 windows))
                                              given workspaces))))
    (dolist (given (list fields nil))
      (should (equal (list (funcall focus "w-loose")
                           (umbriel-action "window-center" given)
                           (funcall focus "w-dp2"))
                     (umbriel-buffer--plan spec (list :window (nth 2 windows))
                                              given workspaces))))))

(ert-deftest umbriel-buffer/plan-no-restore-when-already-focused ()
  "emacs-config:DEC-025: PREVIOUS equal to the row's window id plans no restore step."
  (let* ((workspaces (umbriel-buffer-test--action-workspaces-focusing
                      "w-dp1"))
         (windows (umbriel-buffer-test--action-windows))
         (spec (umbriel-buffer-test--spec 'focused "window-center"))
         (fields '((step . 1))))
    (should (equal (list (umbriel-action "window-focus"
                                            '((window . "w-dp1")))
                         (umbriel-action "window-center" fields))
                   (umbriel-buffer--plan spec (list :window (nth 0 windows))
                                            fields workspaces)))
    (should (equal (list (umbriel-action "window-focus" '((window . "w-dp1")))
                         (umbriel-action "window-center" nil))
                   (umbriel-buffer--plan spec (list :window (nth 0 windows))
                                            nil workspaces)))))

(ert-deftest umbriel-buffer/plan-no-restore-when-previous-unknown ()
  "emacs-config:DEC-025: an unknown PREVIOUS plans focus-then-act and no restore.
Unknown is: `focused_window' \"\", nil or absent, no focused workspace,
and more than one focused workspace."
  (let* ((windows (umbriel-buffer-test--action-windows))
         (spec (umbriel-buffer-test--spec 'focused "window-center"))
         (fields '((step . 1)))
         (unfocused
          (mapcar (lambda (workspace)
                    (setf (alist-get 'focused workspace) nil)
                    workspace)
                  (umbriel-buffer-test--action-workspaces-focusing "w-dp2")))
         (two-focused
          (let ((both (umbriel-buffer-test--action-workspaces-focusing
                       "w-dp2")))
            (setf (alist-get 'focused (nth 1 both)) t)
            both))
         (cases (list (umbriel-buffer-test--action-workspaces-focusing "")
                      (umbriel-buffer-test--action-workspaces-focusing nil)
                      (umbriel-buffer-test--action-workspaces)
                      unfocused
                      two-focused)))
    (should (= 2 (seq-count (lambda (held) (alist-get 'focused held))
                            two-focused)))
    (should-not (seq-some (lambda (held) (alist-get 'focused held)) unfocused))
    (dolist (workspaces cases)
      (dolist (target (list (list :window (nth 0 windows))
                            (list :window (nth 2 windows))))
        (let* ((id (alist-get 'id (plist-get target :window)))
               (restore (umbriel-action "window-focus"
                                           (list (cons 'window "w-dp2")))))
          (let ((plan (umbriel-buffer--plan spec target fields workspaces)))
            (should (equal (list (umbriel-action "window-focus"
                                                    (list (cons 'window id)))
                                 (umbriel-action "window-center" fields))
                           plan))
            (should-not (member restore plan))))))))

(ert-deftest umbriel-buffer/plan-no-restore-for-a-workspace-row ()
  "emacs-config:DEC-025: `focused' on a workspace row is [ACTION], restore or not."
  (let* ((workspaces (umbriel-buffer-test--action-workspaces-focusing
                      "w-dp2"))
         (spec (umbriel-buffer-test--spec 'focused "window-center"))
         (fields '((step . 1))))
    (dolist (workspace workspaces)
      (dolist (given (list nil fields))
        (should (equal (list (umbriel-action "window-center" given))
                       (umbriel-buffer--plan spec (list :workspace workspace)
                                                given workspaces)))))))

(ert-deftest umbriel-buffer/plan-other-addressing-never-restores ()
  "emacs-config:DEC-025: `window', `workspace' and `active-workspace' plan as before.
A known PREVIOUS adds no `window-focus' step to any of them."
  (let* ((plain (umbriel-buffer-test--action-workspaces))
         (restoring (umbriel-buffer-test--action-workspaces-focusing
                     "w-dp2"))
         (windows (umbriel-buffer-test--action-windows))
         (targets (list (list :window (nth 0 windows))
                        (list :workspace (nth 0 plain))
                        (list :workspace (nth 1 plain))))
         (fields '((step . 1)))
         (restores-p (lambda (plan)
                       (seq-some (lambda (request)
                                   (and (equal "window-focus"
                                               (alist-get 'action request))
                                        (equal "w-dp2"
                                               (alist-get 'window request))))
                                 plan))))
    (dolist (addressing '(window workspace active-workspace))
      (let ((spec (umbriel-buffer-test--spec addressing
                                                "window-center")))
        (dolist (target targets)
          (dolist (given (list nil fields))
            (let ((with (umbriel-buffer--plan spec target given restoring))
                  (without (umbriel-buffer--plan spec target given plain)))
              (should (equal without with))
              ;; A refusal is a string, and no step of it restores anything.
              (when (listp with)
                (should-not (funcall restores-p with))))))))))

;;;; actions: kill (VT-2, EX-3, F-19, D-4)

(ert-deftest umbriel-buffer/kill-refuses-emacs-own-process ()
  "VT-2/EX-3/F-19: killing Emacs's own pid refuses; nothing asked or sent."
  (let ((windows (list (umbriel-buffer-test--window
                        :id "w-self" :workspace "DP-1:1"
                        :app_id "emacs" :title "notes.org"
                        :pid (emacs-pid)))))
    (umbriel-buffer-test--with-state
        (umbriel-buffer-test--held
         windows (umbriel-buffer-test--action-workspaces))
      (let ((buffer (umbriel-buffer-test--mode-buffer)))
        (with-current-buffer buffer
          (umbriel-buffer--repaint)
          (umbriel-buffer-test--with-send-all
            (umbriel-buffer-test--with-refresh
              (umbriel-buffer-test--with-prompts
                (let ((signalled
                       (should-error
                        (umbriel-buffer-test--call-on-row
                         'umbriel-kill 'umbriel-window "w-self")
                        :type 'user-error)))
                  (should (umbriel-buffer-test--mentions-p
                           (error-message-string signalled) "use delete-frame")))
                (should-not umbriel-buffer-test--prompts)
                (should-not umbriel-buffer-test--send-all-calls)
                (should-not umbriel-buffer-test--refreshes)))))))))

(ert-deftest umbriel-buffer/kill-prompt-names-the-window ()
  "VT-2/EX-3: the confirmation names app id, title and pid; -1 as well."
  (dolist (pid (list 4242 -1))
    (let* ((id (format "w-pid-%d" pid))
           (windows (list (umbriel-buffer-test--window
                           :id id :workspace "DP-1:1" :app_id "foot"
                           :title "shell" :pid pid))))
      (umbriel-buffer-test--with-state
          (umbriel-buffer-test--held
           windows (umbriel-buffer-test--action-workspaces))
        (let ((buffer (umbriel-buffer-test--mode-buffer)))
          (with-current-buffer buffer
            (umbriel-buffer--repaint)
            (umbriel-buffer-test--with-send-all
              (umbriel-buffer-test--with-prompts
                (setq umbriel-buffer-test--prompt-confirm t)
                (umbriel-buffer-test--call-on-row 'umbriel-kill
                                                     'umbriel-window id)
                (let ((call (car umbriel-buffer-test--prompts)))
                  (should call)
                  (should (eq 'y-or-n-p (car call)))
                  (dolist (needle (list "foot" "shell" (number-to-string pid)))
                    (should (umbriel-buffer-test--mentions-p
                             (nth 1 call) needle))))
                (should (equal (list (umbriel-action "window-kill"
                                                        (list (cons 'window id))))
                               (car (car umbriel-buffer-test--send-all-calls))))))))))))

(ert-deftest umbriel-buffer/kill-confirms-the-captured-window ()
  "VT-2/EX-3: point and held state moving under the prompt redirect nothing."
  (let ((workspaces (umbriel-buffer-test--action-workspaces))
        (windows (umbriel-buffer-test--action-windows)))
    (umbriel-buffer-test--with-state
        (umbriel-buffer-test--held windows workspaces)
      (let ((buffer (umbriel-buffer-test--mode-buffer)))
        (with-current-buffer buffer
          (umbriel-buffer--repaint)
          (umbriel-buffer-test--with-send-all
            (umbriel-buffer-test--with-prompts
              (setq umbriel-buffer-test--prompt-confirm t)
              (setq umbriel-buffer-test--prompt-hook
                    (lambda ()
                      ;; The row at point and the held state both move on.
                      (goto-char (oref (umbriel-buffer-test--find
                                        'umbriel-window "w-dp2")
                                       start))
                      (umbriel-buffer-test--hold
                       (seq-remove (lambda (window)
                                     (equal "w-dp1" (alist-get 'id window)))
                                   (umbriel-buffer-test--action-windows))
                       workspaces)
                      (umbriel-buffer--repaint)))
              (umbriel-buffer-test--call-on-row 'umbriel-kill
                                                   'umbriel-window "w-dp1")
              ;; The prompt really did move both out from under it.
              (should (equal "w-dp2" (oref (magit-current-section) value)))
              (should-not (umbriel-window "w-dp1"))
              (should (equal (list (umbriel-action "window-kill"
                                                      '((window . "w-dp1"))))
                             (car (car umbriel-buffer-test--send-all-calls)))))))))))

(ert-deftest umbriel-buffer/kill-declined-sends-nothing ()
  "VT-2/D-4: answering no sends nothing, refreshes nothing, says nothing."
  (let ((workspaces (umbriel-buffer-test--action-workspaces))
        (windows (umbriel-buffer-test--action-windows)))
    (umbriel-buffer-test--with-state
        (umbriel-buffer-test--held windows workspaces)
      (let ((buffer (umbriel-buffer-test--mode-buffer)))
        (with-current-buffer buffer
          (umbriel-buffer--repaint)
          (umbriel-buffer-test--with-send-all
            (umbriel-buffer-test--with-refresh
              (umbriel-buffer-test--with-messages
                (umbriel-buffer-test--with-prompts
                  (setq umbriel-buffer-test--prompt-confirm nil)
                  (umbriel-buffer-test--call-on-row 'umbriel-kill
                                                       'umbriel-window "w-dp1")
                  (should (= 1 (length umbriel-buffer-test--prompts)))
                  (should-not umbriel-buffer-test--send-all-calls)
                  (should-not umbriel-buffer-test--refreshes)
                  (should-not umbriel-buffer-test--messages))))))))))

;;;; actions: prompts (D-3)

(ert-deftest umbriel-buffer/workspace-prompt-offers-the-held-workspaces ()
  "Prompts: `m' completes over the model choices and sends the selector."
  (let ((workspaces (umbriel-buffer-test--action-workspaces))
        (windows (umbriel-buffer-test--action-windows)))
    (umbriel-buffer-test--with-state
        (umbriel-buffer-test--held windows workspaces)
      (let ((buffer (umbriel-buffer-test--mode-buffer)))
        (with-current-buffer buffer
          (umbriel-buffer--repaint)
          (umbriel-buffer-test--with-send-all
            (umbriel-buffer-test--with-prompts
              (let* ((choices (umbriel-model-workspace-choices
                               (umbriel-workspaces)))
                     (label (car (nth 1 choices)))
                     (chosen (cdr (nth 1 choices))))
                (setq umbriel-buffer-test--prompt-choice label)
                (umbriel-buffer-test--call-on-row
                 'umbriel-move-to-workspace 'umbriel-window "w-dp1")
                (let ((call (car umbriel-buffer-test--prompts)))
                  (should (eq 'completing-read (car call)))
                  (should (nth 3 call))
                  (should (umbriel-buffer-test--same-set-p
                           (umbriel-buffer-test--offered (nth 2 call))
                           (mapcar #'car choices))))
                (should (equal (list (umbriel-action "window-focus"
                                                        '((window . "w-dp1")))
                                     (umbriel-action
                                      "window-move-to-workspace-silent"
                                      (umbriel-model-workspace-selector chosen)))
                               (car (car umbriel-buffer-test--send-all-calls))))))))))))

(ert-deftest umbriel-buffer/layout-prompt-offers-the-model-layouts ()
  "Prompts: `L' completes over the layouts and sends `(layout . CHOICE)'.
`toggle' goes without a `cycle' field: the request holds the layout only."
  (let* ((workspaces (umbriel-buffer-test--action-workspaces))
         (windows (umbriel-buffer-test--action-windows)))
    (umbriel-buffer-test--with-state
        (umbriel-buffer-test--held windows workspaces)
      (let ((buffer (umbriel-buffer-test--mode-buffer)))
        (with-current-buffer buffer
          (umbriel-buffer--repaint)
          (umbriel-buffer-test--with-send-all
            (umbriel-buffer-test--with-prompts
              (dolist (choice umbriel-model-layouts)
                (setq umbriel-buffer-test--prompts nil)
                (setq umbriel-buffer-test--send-all-calls nil)
                (setq umbriel-buffer-test--prompt-choice choice)
                (umbriel-buffer-test--call-on-row 'umbriel-set-layout
                                                     'umbriel-window "w-dp1")
                (let ((call (car umbriel-buffer-test--prompts)))
                  (should (eq 'completing-read (car call)))
                  (should (nth 3 call))
                  (should (umbriel-buffer-test--same-set-p
                           (umbriel-buffer-test--offered (nth 2 call))
                           umbriel-model-layouts)))
                ;; The window's workspace is the focused one: act, no switch.
                (should (equal (list (umbriel-action
                                      "workspace-set-layout"
                                      (list (cons 'layout choice))))
                               (car (car umbriel-buffer-test--send-all-calls))))))))))))

(ert-deftest umbriel-buffer/prompts-keep-the-model-order ()
  "VH-1 Fix 2: both prompt tables pin identity sorts and offer model order.
vertico prefers a table's `display-sort-function' metadata over
`vertico-sort-function', so prescient's recency sort would reorder a
prompt unless the table declares identity for display and cycle."
  (let ((workspaces (umbriel-buffer-test--action-workspaces))
        (windows (umbriel-buffer-test--action-windows)))
    (umbriel-buffer-test--with-state
        (umbriel-buffer-test--held windows workspaces)
      (let ((buffer (umbriel-buffer-test--mode-buffer))
            (cases (list (list 'umbriel-move-to-workspace
                               (mapcar #'car
                                       (umbriel-model-workspace-choices
                                        workspaces)))
                         (list 'umbriel-set-layout
                               umbriel-model-layouts))))
        (with-current-buffer buffer
          (umbriel-buffer--repaint)
          (umbriel-buffer-test--with-send-all
            (umbriel-buffer-test--with-prompts
              (dolist (case cases)
                (let ((command (nth 0 case))
                      (expected (nth 1 case)))
                  (setq umbriel-buffer-test--prompts nil)
                  (setq umbriel-buffer-test--send-all-calls nil)
                  (setq umbriel-buffer-test--prompt-choice (car expected))
                  (umbriel-buffer-test--call-on-row command 'umbriel-window
                                                       "w-dp1")
                  (let ((call (car umbriel-buffer-test--prompts)))
                    (should (eq 'completing-read (car call)))
                    (should (nth 3 call))
                    (dolist (key '(display-sort-function cycle-sort-function))
                      (should (eq 'identity
                                  (umbriel-buffer-test--metadata
                                   (nth 2 call) key))))
                    (should (equal expected
                                   (umbriel-buffer-test--offered
                                    (nth 2 call))))))))))))))

(ert-deftest umbriel-buffer/refused-row-never-prompts ()
  "D-3: a refused row errors before prompting and before sending anything."
  (let ((workspaces (umbriel-buffer-test--action-workspaces))
        (windows (umbriel-buffer-test--action-windows)))
    (umbriel-buffer-test--with-state
        (umbriel-buffer-test--held windows workspaces)
      (let ((buffer (umbriel-buffer-test--mode-buffer)))
        (with-current-buffer buffer
          (umbriel-buffer--repaint)
          (umbriel-buffer-test--with-send-all
            (umbriel-buffer-test--with-refresh
              (umbriel-buffer-test--with-prompts
                ;; `L' on an Unassigned window has no workspace to switch to.
                (let ((signalled
                       (should-error
                        (umbriel-buffer-test--call-on-row
                         'umbriel-set-layout 'umbriel-window "w-loose")
                        :type 'user-error)))
                  (should (umbriel-buffer-test--mentions-p
                           (error-message-string signalled) "workspace")))
                (should-not umbriel-buffer-test--prompts)
                (should-not umbriel-buffer-test--send-all-calls)
                (should-not umbriel-buffer-test--refreshes)))))))))

(ert-deftest umbriel-buffer/empty-prompt-input-sends-nothing ()
  "`completing-read' allows empty input under REQUIRE-MATCH: nothing is sent."
  (let ((workspaces (umbriel-buffer-test--action-workspaces))
        (windows (umbriel-buffer-test--action-windows)))
    (umbriel-buffer-test--with-state
        (umbriel-buffer-test--held windows workspaces)
      (let ((buffer (umbriel-buffer-test--mode-buffer)))
        (with-current-buffer buffer
          (umbriel-buffer--repaint)
          (umbriel-buffer-test--with-send-all
            (umbriel-buffer-test--with-prompts
              (setq umbriel-buffer-test--prompt-choice "")
              (dolist (command '(umbriel-move-to-workspace
                                 umbriel-set-layout))
                (should-error (umbriel-buffer-test--call-on-row
                               command 'umbriel-window "w-dp1")
                              :type 'user-error))
              (should-error (umbriel-set-active-layout) :type 'user-error)
              (should-not umbriel-buffer-test--send-all-calls))))))))

(ert-deftest umbriel-buffer/no-row-at-point-is-a-user-error ()
  "Target: a heading, or an empty buffer, is not an umbriel row."
  (let ((workspaces (umbriel-buffer-test--action-workspaces))
        (windows (umbriel-buffer-test--action-windows)))
    (umbriel-buffer-test--with-state
        (umbriel-buffer-test--held windows workspaces)
      (umbriel-buffer-test--with-send-all
        (let ((buffer (umbriel-buffer-test--mode-buffer)))
          (with-current-buffer buffer
            ;; Empty: there is not even a root section.
            (let ((signalled (should-error (call-interactively 'umbriel-focus)
                                           :type 'user-error)))
              (should (umbriel-buffer-test--mentions-p
                       (error-message-string signalled)
                       "No umbriel row at point")))
            (umbriel-buffer--repaint)
            ;; The Unassigned heading is a heading, not a row.
            (let ((signalled
                   (should-error
                    (umbriel-buffer-test--call-on-row
                     'umbriel-focus 'umbriel-unassigned nil)
                    :type 'user-error)))
              (should (umbriel-buffer-test--mentions-p
                       (error-message-string signalled)
                       "No umbriel row at point"))))
        (should-not umbriel-buffer-test--send-all-calls))))))

;;;; actions: completion (VT-5, EX-7, emacs-config:DEC-021, D-6)

(ert-deftest umbriel-buffer/completion-refreshes-only-when-shut ()
  "VT-5/EX-7: one refresh, after the reply, and only with the stream shut."
  (let ((workspaces (umbriel-buffer-test--action-workspaces))
        (windows (umbriel-buffer-test--action-windows))
        (status 'open))
    (umbriel-buffer-test--with-state
        (umbriel-buffer-test--held windows workspaces)
      (let ((buffer (umbriel-buffer-test--mode-buffer)))
        (with-current-buffer buffer
          (umbriel-buffer--repaint)
          (umbriel-buffer-test--with-fake-status status
            (umbriel-buffer-test--with-send-all
              (umbriel-buffer-test--with-refresh
                (umbriel-buffer-test--with-messages
                  ;; Open: the reply is silent and refreshes nothing.
                  (umbriel-buffer-test--call-on-row
                   'umbriel-toggle-floating 'umbriel-window "w-dp1")
                  (should (= 1 (length umbriel-buffer-test--send-all-calls)))
                  (should-not umbriel-buffer-test--refreshes)
                  (funcall (cdr (car umbriel-buffer-test--send-all-calls))
                           '(:ok nil))
                  (should-not umbriel-buffer-test--refreshes)
                  (should-not umbriel-buffer-test--messages)
                  ;; Shut: exactly one refresh, once the reply has landed.
                  (dolist (shut (list 'never (list :failure 'closed "gone")))
                    (setq umbriel-buffer-test--send-all-calls nil)
                    (setq umbriel-buffer-test--refreshes nil)
                    (setq status shut)
                    (umbriel-buffer-test--call-on-row
                     'umbriel-toggle-floating 'umbriel-window "w-dp1")
                    (should-not umbriel-buffer-test--refreshes)
                    (funcall (cdr (car umbriel-buffer-test--send-all-calls))
                             '(:ok nil))
                    (should (= 1 (length umbriel-buffer-test--refreshes)))
                    ;; No argument: the refresh fetches both families.
                    (should-not (car umbriel-buffer-test--refreshes))
                    (should-not umbriel-buffer-test--messages)))))))))))

(ert-deftest umbriel-buffer/completion-reads-the-stream-status-late ()
  "VT-5/emacs-config:DEC-021: the status is read as the reply lands, not when it is sent."
  (let ((workspaces (umbriel-buffer-test--action-workspaces))
        (windows (umbriel-buffer-test--action-windows))
        (status 'open))
    (umbriel-buffer-test--with-state
        (umbriel-buffer-test--held windows workspaces)
      (let ((buffer (umbriel-buffer-test--mode-buffer)))
        (with-current-buffer buffer
          (umbriel-buffer--repaint)
          (umbriel-buffer-test--with-fake-status status
            (umbriel-buffer-test--with-send-all
              (umbriel-buffer-test--with-refresh
                (umbriel-buffer-test--call-on-row
                 'umbriel-toggle-floating 'umbriel-window "w-dp1")
                (setq status 'never)
                (funcall (cdr (car umbriel-buffer-test--send-all-calls))
                         '(:ok nil))
                (should (= 1 (length umbriel-buffer-test--refreshes)))
                ;; And the other way round: open by then, no refresh.
                (setq umbriel-buffer-test--send-all-calls nil)
                (setq umbriel-buffer-test--refreshes nil)
                (umbriel-buffer-test--call-on-row
                 'umbriel-toggle-floating 'umbriel-window "w-dp1")
                (setq status 'open)
                (funcall (cdr (car umbriel-buffer-test--send-all-calls))
                         '(:ok nil))
                (should-not umbriel-buffer-test--refreshes)))))))))

(ert-deftest umbriel-buffer/completion-echoes-a-failure ()
  "VT-5/EX-7: a failure reaches the echo as its message; no refresh."
  (let ((workspaces (umbriel-buffer-test--action-workspaces))
        (windows (umbriel-buffer-test--action-windows))
        (failure '(:failure rejected "no such window")))
    (umbriel-buffer-test--with-state
        (umbriel-buffer-test--held windows workspaces)
      (let ((buffer (umbriel-buffer-test--mode-buffer)))
        (with-current-buffer buffer
          (umbriel-buffer--repaint)
          (umbriel-buffer-test--with-send-all
            (umbriel-buffer-test--with-refresh
              (umbriel-buffer-test--call-on-row
               'umbriel-toggle-floating 'umbriel-window "w-dp1")
              (umbriel-buffer-test--with-messages
                (funcall (cdr (car umbriel-buffer-test--send-all-calls))
                         failure)
                (should (equal (list (umbriel-failure-message failure))
                               umbriel-buffer-test--messages)))
              (should-not umbriel-buffer-test--refreshes))))))))

(ert-deftest umbriel-buffer/completion-stops-at-the-first-failure ()
  "VT-5/EX-7: the real sequencer stops; step two is never sent."
  (let ((workspaces (umbriel-buffer-test--action-workspaces))
        (windows (umbriel-buffer-test--action-windows))
        (sent nil))
    (umbriel-buffer-test--with-state
        (umbriel-buffer-test--held windows workspaces)
      (let ((buffer (umbriel-buffer-test--mode-buffer)))
        (with-current-buffer buffer
          (umbriel-buffer--repaint)
          (umbriel-buffer-test--with-prompts
            (setq umbriel-buffer-test--prompt-choice "master")
            (cl-letf (((symbol-function 'umbriel-send)
                       (lambda (request callback)
                         (push request sent)
                         (funcall callback '(:failure rejected "no such window")))))
              (umbriel-buffer-test--with-refresh
                (umbriel-buffer-test--with-messages
                  ;; `L' on a workspace row of another workspace plans two
                  ;; steps: switch there, then act.
                  (umbriel-buffer-test--call-on-row
                   'umbriel-set-layout 'umbriel-workspace "DP-1:2")
                  (should (= 1 (length sent)))
                  (should (equal "workspace-switch"
                                 (alist-get 'action (car sent))))
                  (should (equal (list (umbriel-failure-message
                                        '(:failure rejected "no such window")))
                                 umbriel-buffer-test--messages))
                  (should-not umbriel-buffer-test--refreshes))))))))))

(ert-deftest umbriel-buffer/completion-sends-the-restore-last ()
  "emacs-config:DEC-025/EX-7: focus, act, restore -- three requests, in that order.
Each one leaves only after the previous replied successfully."
  (let ((workspaces (umbriel-buffer-test--action-workspaces-focusing "w-dp2"))
        (windows (umbriel-buffer-test--action-windows))
        (sent nil))
    (umbriel-buffer-test--with-state
        (umbriel-buffer-test--held windows workspaces)
      (let ((buffer (umbriel-buffer-test--mode-buffer)))
        (with-current-buffer buffer
          (umbriel-buffer--repaint)
          (cl-letf (((symbol-function 'umbriel-send)
                     (lambda (request callback)
                       (push request sent)
                       (funcall callback '(:ok nil)))))
            (umbriel-buffer-test--with-refresh
              (umbriel-buffer-test--with-messages
                (umbriel-buffer-test--call-on-row 'umbriel-center
                                                     'umbriel-window "w-dp1")
                (should (equal (list (umbriel-action
                                      "window-focus" '((window . "w-dp1")))
                                     (umbriel-action "window-center" nil)
                                     (umbriel-action
                                      "window-focus" '((window . "w-dp2"))))
                               (reverse sent)))
                (should-not umbriel-buffer-test--messages)
                (should-not umbriel-buffer-test--refreshes)))))))))

(ert-deftest umbriel-buffer/completion-a-failed-action-skips-the-restore ()
  "emacs-config:DEC-025/EX-7: a failed action stops the plan; the restore is never sent."
  (let ((workspaces (umbriel-buffer-test--action-workspaces-focusing "w-dp2"))
        (windows (umbriel-buffer-test--action-windows))
        (failure '(:failure rejected "no such window"))
        (sent nil))
    (umbriel-buffer-test--with-state
        (umbriel-buffer-test--held windows workspaces)
      (let ((buffer (umbriel-buffer-test--mode-buffer)))
        (with-current-buffer buffer
          (umbriel-buffer--repaint)
          (cl-letf (((symbol-function 'umbriel-send)
                     (lambda (request callback)
                       (push request sent)
                       (funcall callback (if (= 2 (length sent))
                                             failure
                                           '(:ok nil))))))
            (umbriel-buffer-test--with-refresh
              (umbriel-buffer-test--with-messages
                (umbriel-buffer-test--call-on-row 'umbriel-center
                                                     'umbriel-window "w-dp1")
                (should (equal (list (umbriel-action
                                      "window-focus" '((window . "w-dp1")))
                                     (umbriel-action "window-center" nil))
                               (reverse sent)))
                (should (equal (list (umbriel-failure-message failure))
                               umbriel-buffer-test--messages))
                (should-not umbriel-buffer-test--refreshes)))))))))

(ert-deftest umbriel-buffer/completion-echoes-a-failed-restore ()
  "emacs-config:DEC-025/EX-7: the restore is sent last; its failure echoes like any other."
  (let ((workspaces (umbriel-buffer-test--action-workspaces-focusing "w-dp2"))
        (windows (umbriel-buffer-test--action-windows))
        (failure '(:failure closed "umbriel went away"))
        (sent nil))
    (umbriel-buffer-test--with-state
        (umbriel-buffer-test--held windows workspaces)
      (let ((buffer (umbriel-buffer-test--mode-buffer)))
        (with-current-buffer buffer
          (umbriel-buffer--repaint)
          (cl-letf (((symbol-function 'umbriel-send)
                     (lambda (request callback)
                       (push request sent)
                       (funcall callback (if (= 3 (length sent))
                                             failure
                                           '(:ok nil))))))
            (umbriel-buffer-test--with-refresh
              (umbriel-buffer-test--with-messages
                (umbriel-buffer-test--call-on-row 'umbriel-center
                                                     'umbriel-window "w-dp1")
                (should (equal (list (umbriel-action
                                      "window-focus" '((window . "w-dp1")))
                                     (umbriel-action "window-center" nil)
                                     (umbriel-action
                                      "window-focus" '((window . "w-dp2"))))
                               (reverse sent)))
                (should (equal (list (umbriel-failure-message failure))
                               umbriel-buffer-test--messages))
                (should-not umbriel-buffer-test--refreshes)))))))))

(ert-deftest umbriel-buffer/row-action-writes-no-held-state ()
  "VT-5/emacs-config:DEC-021: neither the command nor its callback writes held state."
  (let ((workspaces (umbriel-buffer-test--action-workspaces))
        (windows (umbriel-buffer-test--action-windows)))
    (umbriel-buffer-test--with-state
        (umbriel-buffer-test--held windows workspaces)
      (let ((buffer (umbriel-buffer-test--mode-buffer)))
        (with-current-buffer buffer
          (umbriel-buffer--repaint)
          (let ((before (umbriel-state)))
            (cl-letf (((symbol-function 'umbriel--set-family)
                       (lambda (&rest _)
                         (ert-fail "the command wrote the held snapshots")))
                      ((symbol-function 'umbriel--set-status)
                       (lambda (&rest _)
                         (ert-fail "the command wrote the stream status"))))
              (umbriel-buffer-test--with-send-all
                (umbriel-buffer-test--with-refresh
                  (umbriel-buffer-test--with-prompts
                    (setq umbriel-buffer-test--prompt-choice
                          (car (nth 0 (umbriel-model-workspace-choices
                                       workspaces))))
                    (umbriel-buffer-test--call-on-row
                     'umbriel-move-to-workspace 'umbriel-window "w-dp1")
                    (should (= 1 (length umbriel-buffer-test--send-all-calls)))
                    (funcall (cdr (car umbriel-buffer-test--send-all-calls))
                             '(:ok nil)))))
            (should (equal before (umbriel-state))))))))))

;;;; actions: the global commands (VT-3, EX-4, D-5, D-7)

(ert-deftest umbriel-buffer/global-switch-refreshes-then-schedules ()
  "VT-3/EX-4/D-5: a shut stream refreshes; the prompt waits for a timer."
  (let ((status 'never)
        (workspaces (umbriel-buffer-test--action-workspaces))
        (choices nil))
    (setq choices (umbriel-model-workspace-choices workspaces))
    (umbriel-buffer-test--with-state
        (umbriel-buffer-test--held nil workspaces)
      (umbriel-buffer-test--with-fake-status status
        (umbriel-buffer-test--with-send-all
          (umbriel-buffer-test--with-refresh
            (umbriel-buffer-test--with-prompts
              (setq umbriel-buffer-test--prompt-allowed nil)
              (umbriel-buffer-test--with-scheduled calls
                ;; The command only refreshes; it prompts nowhere yet.
                (umbriel-switch-workspace)
                (should (= 1 (length umbriel-buffer-test--refreshes)))
                (should (car umbriel-buffer-test--refreshes))
                (should-not umbriel-buffer-test--prompts)
                (should-not calls)
                (should-not umbriel-buffer-test--send-all-calls)
                ;; Nor may the reply prompt: it schedules.
                (funcall (car umbriel-buffer-test--refreshes) '(:ok nil))
                (should-not umbriel-buffer-test--prompts)
                (should (= 1 (length calls)))
                (should (equal 0 (nth 0 (car calls))))
                (should-not (nth 1 (car calls)))
                (should (functionp (nth 2 (car calls))))
                (should-not umbriel-buffer-test--send-all-calls)
                ;; The timer's function is where the prompt belongs.
                (setq umbriel-buffer-test--prompt-allowed t)
                (setq umbriel-buffer-test--prompt-choice
                      (car (nth 1 choices)))
                (funcall (nth 2 (car calls)))
                (should (= 1 (length umbriel-buffer-test--prompts)))
                (should (equal (list (umbriel-action
                                      "workspace-switch"
                                      (umbriel-model-workspace-selector
                                       (cdr (nth 1 choices)))))
                               (car (car umbriel-buffer-test--send-all-calls))))))))))))

(ert-deftest umbriel-buffer/global-switch-messages-inside-a-minibuffer ()
  "VT-3/EX-4: a minibuffer already active when the timer runs is respected."
  (let ((status 'never)
        (workspaces (umbriel-buffer-test--action-workspaces)))
    (umbriel-buffer-test--with-state
        (umbriel-buffer-test--held nil workspaces)
      (umbriel-buffer-test--with-fake-status status
        (umbriel-buffer-test--with-send-all
          (umbriel-buffer-test--with-refresh
            (umbriel-buffer-test--with-messages
              (umbriel-buffer-test--with-prompts
                (setq umbriel-buffer-test--prompt-allowed nil)
                (umbriel-buffer-test--with-scheduled calls
                  (umbriel-switch-workspace)
                  (funcall (car umbriel-buffer-test--refreshes) '(:ok nil))
                  (should (= 1 (length calls)))
                  (cl-letf (((symbol-function 'active-minibuffer-window)
                             (lambda () (selected-window))))
                    (umbriel-buffer-test--with-key global "C-c z w"
                        #'umbriel-switch-workspace
                      (funcall (nth 2 (car calls)))))
                  (should-not umbriel-buffer-test--prompts)
                  (should-not umbriel-buffer-test--send-all-calls)
                  (should (= 1 (length umbriel-buffer-test--messages)))
                  (should (umbriel-buffer-test--mentions-p
                           (car umbriel-buffer-test--messages) "retry"))
                  (should (umbriel-buffer-test--mentions-p
                           (car umbriel-buffer-test--messages)
                           "C-c z w")))))))))))

(ert-deftest umbriel-buffer/global-switch-echoes-a-refresh-failure ()
  "VT-3/EX-4: a failed refresh is echoed and schedules nothing."
  (let ((status 'never)
        (workspaces (umbriel-buffer-test--action-workspaces))
        (failure '(:failure closed "stream closed")))
    (umbriel-buffer-test--with-state
        (umbriel-buffer-test--held nil workspaces)
      (umbriel-buffer-test--with-fake-status status
        (umbriel-buffer-test--with-send-all
          (umbriel-buffer-test--with-refresh
            (umbriel-buffer-test--with-prompts
              (setq umbriel-buffer-test--prompt-allowed nil)
              (umbriel-buffer-test--with-scheduled calls
                (umbriel-switch-workspace)
                (umbriel-buffer-test--with-messages
                  (funcall (car umbriel-buffer-test--refreshes) failure)
                  (should (equal (list (umbriel-failure-message failure))
                                 umbriel-buffer-test--messages)))
                (should-not calls)
                (should-not umbriel-buffer-test--prompts)
                (should-not umbriel-buffer-test--send-all-calls)))))))))

(ert-deftest umbriel-buffer/global-switch-prompts-at-once-when-open ()
  "D-5/VT-3: an open stream prompts in the command, refreshing nothing first."
  (let ((status 'open)
        (workspaces (umbriel-buffer-test--action-workspaces)))
    (umbriel-buffer-test--with-state
        (umbriel-buffer-test--held nil workspaces)
      (umbriel-buffer-test--with-fake-status status
        (umbriel-buffer-test--with-send-all
          (umbriel-buffer-test--with-refresh
            (umbriel-buffer-test--with-prompts
              (let* ((choices (umbriel-model-workspace-choices
                               (umbriel-workspaces)))
                     (chosen (cdr (nth 0 choices))))
                (setq umbriel-buffer-test--prompt-choice
                      (car (nth 0 choices)))
                (umbriel-switch-workspace)
                (should-not umbriel-buffer-test--refreshes)
                (should (= 1 (length umbriel-buffer-test--prompts)))
                (should (equal (list (umbriel-action
                                      "workspace-switch"
                                      (umbriel-model-workspace-selector chosen)))
                               (car (car umbriel-buffer-test--send-all-calls))))))))))))

(ert-deftest umbriel-buffer/no-row-callback-prompts ()
  "VT-5/EX-7: no row command's send-all callback opens any prompt.
The row sits in a pad, so that every action in the table has a target;
a send to a pad, which refuses a pad window, gets a plain row instead."
  (let ((workspaces (umbriel-buffer-test--action-workspaces))
        (windows (cons (umbriel-buffer-test--window :id "w-plain"
                                                       :workspace "DP-1:1")
                       (mapcar (lambda (window)
                                 (cons '(scratchpad . "signal") window))
                               (umbriel-buffer-test--action-windows)))))
    (umbriel-buffer-test--with-state
        (umbriel-buffer-test--held windows workspaces)
      (let ((buffer (umbriel-buffer-test--mode-buffer)))
        (with-current-buffer buffer
          (umbriel-buffer--repaint)
          (umbriel-buffer-test--with-send-all
            (umbriel-buffer-test--with-prompts
              (dolist (spec umbriel-buffer-test--action-table)
                (setq umbriel-buffer-test--send-all-calls nil)
                (setq umbriel-buffer-test--prompts nil)
                (setq umbriel-buffer-test--prompt-choice
                      (if (eq (nth 3 spec) 'layout)
                          "master"
                        (car (nth 0 (umbriel-model-workspace-choices
                                     workspaces)))))
                (setq umbriel-buffer-test--prompt-confirm t)
                (umbriel-buffer-test--call-on-row
                 (nth 0 spec) 'umbriel-window
                 (if (eq (nth 2 spec) 'pointer-window) "w-plain" "w-dp1"))
                (let ((plan (caar umbriel-buffer-test--send-all-calls))
                      (callback (cdar umbriel-buffer-test--send-all-calls))
                      (asked (length umbriel-buffer-test--prompts)))
                  (should plan)
                  (should callback)
                  (setq umbriel-buffer-test--prompt-allowed nil)
                  (funcall callback '(:ok nil))
                  (should (= asked (length umbriel-buffer-test--prompts)))
                  (should (= 1 (length umbriel-buffer-test--send-all-calls)))
                  (setq umbriel-buffer-test--prompt-allowed t))))))))))

(ert-deftest umbriel-buffer/set-active-layout-prompts-the-pointer ()
  "D-7: the global layout command names the pointer and sends the layout."
  (let ((status 'open))
    (umbriel-buffer-test--with-state nil
      (umbriel-buffer-test--with-fake-status status
        (umbriel-buffer-test--with-send-all
          (umbriel-buffer-test--with-refresh
            (umbriel-buffer-test--with-prompts
              (dolist (choice (list "master" "toggle"))
                (setq umbriel-buffer-test--send-all-calls nil)
                (setq umbriel-buffer-test--prompts nil)
                (setq umbriel-buffer-test--prompt-choice choice)
                (umbriel-set-active-layout)
                (let ((call (car umbriel-buffer-test--prompts)))
                  (should (eq 'completing-read (car call)))
                  (should (nth 3 call))
                  (should (umbriel-buffer-test--mentions-p (nth 1 call) "pointer"))
                  (should (umbriel-buffer-test--mentions-p (nth 1 call) "output"))
                  (should (umbriel-buffer-test--same-set-p
                           (umbriel-buffer-test--offered (nth 2 call))
                           umbriel-model-layouts)))
                (should (equal (list (umbriel-action
                                      "workspace-set-layout"
                                      (list (cons 'layout choice))))
                               (car (car umbriel-buffer-test--send-all-calls)))))
              ;; Completion as a row command's: one refresh when shut.
              (setq status 'never)
              (setq umbriel-buffer-test--send-all-calls nil)
              (setq umbriel-buffer-test--refreshes nil)
              (setq umbriel-buffer-test--prompt-choice "master")
              (umbriel-set-active-layout)
              (should-not umbriel-buffer-test--refreshes)
              (funcall (cdr (car umbriel-buffer-test--send-all-calls))
                       '(:ok nil))
              (should (= 1 (length umbriel-buffer-test--refreshes))))))))))

;;;; actions: keys and commands (VT-4, § Macro)

(ert-deftest umbriel-buffer/row-keys-bind-the-actions ()
  "VT-4: every row key in the sheet's table, and its twin, is bound."
  (dolist (binding '(("RET" . umbriel-focus)
                     ("s" . umbriel-switch)
                     ("m" . umbriel-move-to-workspace)
                     ("t" . umbriel-toggle-floating)
                     ("c" . umbriel-center)
                     ("k" . umbriel-kill)
                     ("L" . umbriel-set-layout)
                     ("M-<left>" . umbriel-column-left)
                     ("<" . umbriel-column-left)
                     ("M-<right>" . umbriel-column-right)
                     (">" . umbriel-column-right)
                     ("e" . umbriel-cycle-extent)
                     ("E" . umbriel-cycle-extent-back)
                     ("g" . umbriel-buffer-refresh)
                     ("?" . describe-mode)
                     ("q" . quit-window)))
    (should (eq (keymap-lookup umbriel-buffer-mode-map (car binding))
                (cdr binding)))))

(ert-deftest umbriel-buffer/mode-help-lists-the-row-keys ()
  "`?' shows the mode's help, and the help lists the row keys."
  (should (string-search "umbriel-toggle-floating"
                         (substitute-command-keys
                          (documentation 'umbriel-buffer-mode)))))

(ert-deftest umbriel-buffer/command-map-binds-the-global-commands ()
  "The prefix map a user binds once holds the commands that need no row."
  (dolist (binding '(("u" . umbriel)
                     ("w" . umbriel-switch-workspace)
                     ("l" . umbriel-set-active-layout)))
    (should (eq (keymap-lookup umbriel-command-map (car binding))
                (cdr binding)))))

(ert-deftest umbriel-buffer/command-map-is-autoloaded ()
  "An init file can bind the prefix map before the library loads."
  (let* ((dir (file-name-directory (locate-library "umbriel-buffer")))
         ;; A fresh directory: an existing output file newer than the
         ;; sources counts as up to date, and nothing is written to it.
         (tmp (make-temp-file "umbriel-loaddefs" t))
         (loaddefs (expand-file-name "loaddefs.el" tmp)))
    (unwind-protect
        (progn
          (let ((inhibit-message t))
            (loaddefs-generate dir loaddefs))
          (with-temp-buffer
            (should (equal 0 (call-process
                              (expand-file-name invocation-name
                                                invocation-directory)
                              nil t nil "-Q" "--batch" "-l" loaddefs
                              "--eval"
                              (concat "(princ (list (keymap-lookup"
                                      " umbriel-command-map \"w\")"
                                      " (featurep 'umbriel-buffer)))"))))
            (should (string-suffix-p "(umbriel-switch-workspace nil)"
                                     (string-trim (buffer-string))))))
      (delete-directory tmp t))))

(ert-deftest umbriel-buffer/tunables-are-user-options ()
  "What a user may want to change is customizable, so `setopt' works."
  (dolist (option '(umbriel-buffer-known-pads
                    umbriel-buffer-column-limits
                    umbriel-buffer-title-min-width))
    (should (custom-variable-p option))))

(ert-deftest umbriel-buffer/scroll-keys-are-gone ()
  "emacs-config:DEC-026: the buffer keeps column moves and leaves scrolling to umbriel.
No `C-<left>'/`[' or `C-<right>'/`]' row key, and no scroll commands."
  (dolist (key '("C-<left>" "[" "C-<right>" "]"))
    (should-not (keymap-lookup umbriel-buffer-mode-map key)))
  (should-not (fboundp 'umbriel-scroll-left))
  (should-not (fboundp 'umbriel-scroll-right)))

(ert-deftest umbriel-buffer/action-rows-are-commands ()
  "VT-4/§Macro: every table row is an interactive command with a docstring."
  (dolist (row umbriel-buffer-test--action-table)
    (let ((command (nth 0 row)))
      (should (fboundp command))
      (should (commandp command))
      (let ((doc (documentation command)))
        (should (stringp doc))
        (should-not (string-empty-p doc))
        (dolist (line (split-string doc "\n"))
          (should (<= (length line) 80)))))))

(ert-deftest umbriel-buffer/action-table-is-the-designs ()
  "§Macro: the table holds the design's rows, verbatim, in order."
  (should (equal umbriel-buffer-test--action-table
                 umbriel-buffer-actions)))

;;;; actions: review fixes
;;
;; Contract: the review of emacs-config:SL-016 phase 05, R-1..R-7.
;; T-1..T-6 pin behaviour the review asked for and the implementation did not
;; have yet; T-7 and T-8 pin behaviour it already had.

(ert-deftest umbriel-buffer/layout-refuses-two-focused-workspaces ()
  "T-1/R-1/F-6: two `focused' workspaces is no answer, so refuse."
  (let* ((workspaces
          (list (umbriel-buffer-test--workspace :id "DP-1:1" :index 1
                                                   :output "DP-1" :focused t)
                (umbriel-buffer-test--workspace :id "DP-2:1" :index 1
                                                   :output "DP-2" :focused t)))
         (windows (list (umbriel-buffer-test--window :id "w-dp1"
                                                        :workspace "DP-1:1")
                        (umbriel-buffer-test--window :id "w-dp2"
                                                        :workspace "DP-2:1")))
         (spec (umbriel-buffer-test--spec 'active-workspace
                                             "workspace-set-layout" 'layout))
         (fields '((layout . "master"))))
    (dolist (target (list (list :workspace (nth 0 workspaces))
                          (list :workspace (nth 1 workspaces))
                          (list :window (nth 0 windows))
                          (list :window (nth 1 windows))))
      (let ((result (umbriel-buffer-test--refused
                     spec target fields workspaces)))
        (should-not (umbriel-buffer-test--switches-p result))))))

(ert-deftest umbriel-buffer/unknown-prompt-kind-refuses-to-send ()
  "T-2/R-2: an unknown PROMPT kind fails closed: it signals, sends nothing."
  (let ((workspaces (umbriel-buffer-test--action-workspaces))
        (windows (umbriel-buffer-test--action-windows)))
    (umbriel-buffer-test--with-state
        (umbriel-buffer-test--held windows workspaces)
      (let ((buffer (umbriel-buffer-test--mode-buffer)))
        (with-current-buffer buffer
          (umbriel-buffer--repaint)
          (umbriel-buffer-test--with-send-all
            (umbriel-buffer-test--with-prompts
              ;; A typo'd `confirm-kill' must never confirm, and never send.
              (goto-char (oref (umbriel-buffer-test--find
                                'umbriel-window "w-dp1")
                               start))
              (should-error
               (umbriel-buffer--run
                (umbriel-buffer-test--spec 'window "window-kill"
                                              'confirm-killl))
               :type 'error)
              (should-not umbriel-buffer-test--prompts)
              (should-not umbriel-buffer-test--send-all-calls))))))))

(ert-deftest umbriel-buffer/kill-prompt-keeps-a-newline-out ()
  "T-3/R-3: the confirmation stays one line and still names id, title, pid."
  (let ((windows (list (umbriel-buffer-test--window
                        :id "w-multiline" :workspace "DP-1:1"
                        :app_id "foot" :title "notes\nsecret" :pid 4242))))
    (umbriel-buffer-test--with-state
        (umbriel-buffer-test--held
         windows (umbriel-buffer-test--action-workspaces))
      (let ((buffer (umbriel-buffer-test--mode-buffer)))
        (with-current-buffer buffer
          (umbriel-buffer--repaint)
          (umbriel-buffer-test--with-send-all
            (umbriel-buffer-test--with-prompts
              (setq umbriel-buffer-test--prompt-confirm nil)
              (umbriel-buffer-test--call-on-row
               'umbriel-kill 'umbriel-window "w-multiline")
              (let ((call (car umbriel-buffer-test--prompts)))
                (should (eq 'y-or-n-p (car call)))
                (should-not (string-match-p "\n" (nth 1 call)))
                (dolist (needle (list "foot" "notes" "secret" "4242"))
                  (should (umbriel-buffer-test--mentions-p
                           (nth 1 call) needle))))
              (should-not umbriel-buffer-test--send-all-calls))))))))

(ert-deftest umbriel-buffer/kill-compares-the-pid-numerically ()
  "T-4/R-4: a float pid names the same process; a string pid prompts."
  (let ((windows (list (umbriel-buffer-test--window
                        :id "w-float" :workspace "DP-1:1"
                        :app_id "emacs" :title "notes"
                        :pid (float (emacs-pid)))
                       (umbriel-buffer-test--window
                        :id "w-string" :workspace "DP-1:1"
                        :app_id "foot" :title "shell" :pid "4242"))))
    (umbriel-buffer-test--with-state
        (umbriel-buffer-test--held
         windows (umbriel-buffer-test--action-workspaces))
      (let ((buffer (umbriel-buffer-test--mode-buffer)))
        (with-current-buffer buffer
          (umbriel-buffer--repaint)
          (umbriel-buffer-test--with-send-all
            (umbriel-buffer-test--with-prompts
              (setq umbriel-buffer-test--prompt-confirm t)
              ;; The compositor's 4242.0 is this Emacs just as 4242 is.
              (let ((signalled
                     (should-error
                      (umbriel-buffer-test--call-on-row
                       'umbriel-kill 'umbriel-window "w-float")
                      :type 'user-error)))
                (should (umbriel-buffer-test--mentions-p
                         (error-message-string signalled) "use delete-frame")))
              (should-not umbriel-buffer-test--prompts)
              (should-not umbriel-buffer-test--send-all-calls)
              ;; A pid that arrived as a string takes the normal path.
              (setq umbriel-buffer-test--prompt-confirm nil)
              (umbriel-buffer-test--call-on-row
               'umbriel-kill 'umbriel-window "w-string")
              (let ((call (car umbriel-buffer-test--prompts)))
                (should (eq 'y-or-n-p (car call)))
                (should (umbriel-buffer-test--mentions-p
                         (nth 1 call) "4242")))
              (should-not umbriel-buffer-test--send-all-calls))))))))

(ert-deftest umbriel-buffer/unknown-addressing-signals ()
  "T-5/R-6: a plan for an addressing the table does not know signals."
  (let ((workspaces (umbriel-buffer-test--action-workspaces))
        (windows (umbriel-buffer-test--action-windows))
        (spec (umbriel-buffer-test--spec 'sideways "window-focus")))
    (dolist (target (list (list :window (nth 0 windows))
                          (list :workspace (nth 0 workspaces))))
      (should-error (umbriel-buffer--plan spec target nil workspaces)
                    :type 'error))))

(ert-deftest umbriel-buffer/global-switch-with-no-workspaces-held ()
  "T-6/R-7: nothing held still refreshes, then the timer messages."
  (umbriel-buffer-test--with-state (umbriel-buffer-test--held nil nil 'open 0)
    (umbriel-buffer-test--with-send-all
      (umbriel-buffer-test--with-refresh
        (umbriel-buffer-test--with-messages
          (umbriel-buffer-test--with-prompts
            (setq umbriel-buffer-test--prompt-allowed nil)
            (umbriel-buffer-test--with-scheduled calls
              ;; Open, but nothing held: refresh rather than prompt.
              (umbriel-switch-workspace)
              (should (= 1 (length umbriel-buffer-test--refreshes)))
              (should-not umbriel-buffer-test--prompts)
              (should-not calls)
              ;; The reply may not prompt either: it schedules.
              (funcall (car umbriel-buffer-test--refreshes) '(:ok nil))
              (should-not umbriel-buffer-test--prompts)
              (should (= 1 (length calls)))
              (should (equal 0 (nth 0 (car calls))))
              ;; The timer runs with the workspaces still empty.
              (funcall (nth 2 (car calls)))
              (should-not umbriel-buffer-test--prompts)
              (should-not umbriel-buffer-test--send-all-calls)
              (should (= 1 (length umbriel-buffer-test--messages)))
              (should (umbriel-buffer-test--mentions-p
                       (car umbriel-buffer-test--messages)
                       "workspace")))))))))

(ert-deftest umbriel-buffer/the-prompt-does-not-re-read-the-held-workspaces ()
  "T-7: the workspaces captured before the prompt are planned with."
  (let* ((workspaces (umbriel-buffer-test--action-workspaces))
         (windows (umbriel-buffer-test--action-windows))
         (target (nth 1 workspaces))
         (moved (list (umbriel-buffer-test--workspace :id "DP-2:1" :index 1
                                                         :name "docs"
                                                         :output "DP-2"
                                                         :focused t)
                      (umbriel-buffer-test--workspace :id "DP-1:2" :index 2
                                                         :name nil :named nil
                                                         :output "DP-1"))))
    (umbriel-buffer-test--with-state
        (umbriel-buffer-test--held windows workspaces)
      (let ((buffer (umbriel-buffer-test--mode-buffer)))
        (with-current-buffer buffer
          (umbriel-buffer--repaint)
          (umbriel-buffer-test--with-send-all
            (umbriel-buffer-test--with-prompts
              (setq umbriel-buffer-test--prompt-choice "master")
              (setq umbriel-buffer-test--prompt-hook
                    (lambda ()
                      ;; Focus moves to the other output under the prompt.
                      (umbriel-buffer-test--hold windows moved)))
              (umbriel-buffer-test--call-on-row
               'umbriel-set-layout 'umbriel-workspace "DP-1:2")
              ;; The held state really did move off the target's output.
              (should (equal "DP-2"
                             (alist-get 'output
                                        (seq-find (lambda (held)
                                                    (alist-get 'focused held))
                                                  (umbriel-workspaces)))))
              (should (equal (list (umbriel-action
                                    "workspace-switch"
                                    (umbriel-model-workspace-selector target))
                                   (umbriel-action "workspace-set-layout"
                                                      '((layout . "master"))))
                             (car (car umbriel-buffer-test--send-all-calls)))))))))))

(ert-deftest umbriel-buffer/docstrings-name-their-action ()
  "T-8/§Macro: each generated docstring names its umbriel action string."
  (dolist (row umbriel-buffer-test--action-table)
    (let ((doc (documentation (nth 0 row))))
      (should (stringp doc))
      (should (umbriel-buffer-test--mentions-p doc (nth 1 row))))))

;;; umbriel-buffer-test.el ends here
