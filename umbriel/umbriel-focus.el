;;; umbriel-focus.el --- Focus server frames under umbriel -*- lexical-binding: t; -*-

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

;; Wayland gives focus only to a client holding an activation token, and
;; emacsclient brings none: when it reuses a frame, the server's
;; `select-frame-set-input-focus' leaves that frame behind the terminal.
;; Ask umbriel, the compositor, to focus the frame's window instead: the
;; window this process owns that bears the frame's title.
;;
;; Titles are not unique — two frames showing one buffer share one — so
;; the first match wins.

;;; Code:

(require 'seq)
(require 'umbriel)

(defun umbriel-window-id (windows pid title)
  "Id of the window in WINDOWS that PID owns and that bears TITLE.
WINDOWS is the reply to umbriel's `windows' query."
  (alist-get 'id (seq-find (lambda (window)
                             (and (eql (alist-get 'pid window) pid)
                                  (equal (alist-get 'title window) title)))
                           windows)))

(defun umbriel-focus-frame (&optional frame)
  "Ask umbriel to focus FRAME's window; FRAME defaults to the selected one."
  (interactive)
  (let ((frame (or frame (selected-frame))))
    (when (eq (framep frame) 'pgtk)
      (let ((title (frame-parameter frame 'name)))
        (umbriel-send
         (umbriel-query "windows")
         (lambda (result)
           (pcase result
             (`(:ok ,windows)
              (when-let* ((id (umbriel-window-id windows (emacs-pid) title)))
                (umbriel-send (umbriel-action "window-focus" `((window . ,id)))
                                 #'umbriel-echo-failure)))
             (_ (umbriel-echo-failure result)))))))))

;;;###autoload
(define-minor-mode umbriel-focus-mode
  "Focus the frame an emacsclient reuses, by asking umbriel.
Runs `umbriel-focus-frame' on `server-switch-hook', which the server
runs once it has selected the frame showing the client's file."
  :global t
  :group 'umbriel
  (if umbriel-focus-mode
      (add-hook 'server-switch-hook #'umbriel-focus-frame)
    (remove-hook 'server-switch-hook #'umbriel-focus-frame)))

(provide 'umbriel-focus)
;;; umbriel-focus.el ends here
