;;; umbriel-focus.el --- Focus server frames under umbriel -*- lexical-binding: t; -*-

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

;; Runs once the server has selected the frame showing the client's file.
(add-hook 'server-switch-hook #'umbriel-focus-frame)

(provide 'umbriel-focus)
;;; umbriel-focus.el ends here
