;;; umbriel-focus-test.el --- ert tests for umbriel frame focus -*- lexical-binding: t; -*-

;;; Commentary:

;; Run: `just check', or M-x ert RET umbriel-focus/ RET.
;;
;; `umbriel-send' is faked with scripted replies: no test reaches a compositor.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'umbriel-focus)

(defconst umbriel-focus-test--windows
  '(((id . "theirs") (pid . 7) (title . "notes.org"))
    ((id . "other") (pid . 42) (title . "inbox"))
    ((id . "ours") (pid . 42) (title . "notes.org")))
  "Windows as umbriel's `windows' query lists them, trimmed to the keys used.")

;;;; `umbriel-window-id'

(ert-deftest umbriel-focus/window-id-matches-pid-and-title ()
  "Another process's window with the same title is not ours."
  (should (equal (umbriel-window-id umbriel-focus-test--windows 42 "notes.org")
                 "ours")))

(ert-deftest umbriel-focus/window-id-nil-without-match ()
  (should-not (umbriel-window-id umbriel-focus-test--windows 42 "absent")))

;;;; `umbriel-focus-frame'

(defun umbriel-focus-test--run (frame-type title replies)
  "Focus a FRAME-TYPE frame bearing TITLE; this process is pid 42.
`umbriel-send' answers REPLIES in order.  Return (REQUESTS MESSAGES)."
  (let (requests messages)
    (cl-letf (((symbol-function 'framep) (lambda (_) frame-type))
              ((symbol-function 'frame-parameter)
               (lambda (_ param) (and (eq param 'name) title)))
              ((symbol-function 'emacs-pid) (lambda () 42))
              ((symbol-function 'umbriel-send)
               (lambda (request callback)
                 (push request requests)
                 (funcall callback (pop replies))))
              ((symbol-function 'message)
               (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
      (umbriel-focus-frame 'a-frame))
    (list (reverse requests) (reverse messages))))

(ert-deftest umbriel-focus/focuses-its-window ()
  (should (equal (umbriel-focus-test--run
                  'pgtk "notes.org" `((:ok ,umbriel-focus-test--windows) (:ok nil)))
                 (list (list (umbriel-query "windows")
                             (umbriel-action "window-focus" '((window . "ours"))))
                       nil))))

(ert-deftest umbriel-focus/no-match-sends-nothing-more ()
  (should (equal (umbriel-focus-test--run
                  'pgtk "absent" `((:ok ,umbriel-focus-test--windows)))
                 (list (list (umbriel-query "windows")) nil))))

(ert-deftest umbriel-focus/skips-non-pgtk-frames ()
  (should (equal (umbriel-focus-test--run 'x "notes.org" nil) '(nil nil))))

(ert-deftest umbriel-focus/echoes-a-failure ()
  (let ((failure '(:failure absent "/run/u/umbriel-wayland-1.sock")))
    (should (equal (cadr (umbriel-focus-test--run 'pgtk "notes.org" (list failure)))
                   (list (umbriel-failure-message failure))))))

(provide 'umbriel-focus-test)
;;; umbriel-focus-test.el ends here
