;;; umbriel-test.el --- ert tests for the umbriel socket client -*- lexical-binding: t; -*-

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

;; Run: `just check', or M-x ert RET umbriel/ RET.
;;
;; No test reaches a real compositor: path tests bind `process-environment',
;; and transport tests fake the network with `umbriel-test--with-net'.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'umbriel)

;;;; Fake network

(cl-defstruct (umbriel-test--net (:constructor umbriel-test--net-create))
  "The faked network: what was opened, written and deleted.
ARGS are the latest connection's; PROCESSES maps each handle to its
args, newest first; DEAD lists the deleted handles."
  args sent (connections 0) (deleted 0) connect-error write-error
  processes dead)

(defconst umbriel-test--socket "/run/test/umbriel-wayland-9.sock")

(defmacro umbriel-test--with-net (net &rest body)
  "Run BODY with the network faked and the socket path resolvable.
NET is bound to a `umbriel-test--net'.  Each connection gets its own
handle.  Like Emacs, the fake `delete-process' runs that connection's
sentinel synchronously with \"deleted\\n\" (once), and writing to a
deleted connection signals."
  (declare (indent 1))
  `(let ((,net (umbriel-test--net-create))
         (process-environment (list (concat "UMBRIEL_SOCKET=" umbriel-test--socket))))
     (cl-letf (((symbol-function 'make-network-process)
                (lambda (&rest args)
                  (when-let* ((err (umbriel-test--net-connect-error ,net)))
                    (signal (car err) (cdr err)))
                  (let ((process (cons 'fake-process
                                       (cl-incf (umbriel-test--net-connections ,net)))))
                    (push (cons process args) (umbriel-test--net-processes ,net))
                    (setf (umbriel-test--net-args ,net) args)
                    process)))
               ((symbol-function 'process-send-string)
                (lambda (process string)
                  (when (member process (umbriel-test--net-dead ,net))
                    (error "Process %S not running" process))
                  (when-let* ((err (umbriel-test--net-write-error ,net)))
                    (signal (car err) (cdr err)))
                  (push string (umbriel-test--net-sent ,net))))
               ((symbol-function 'delete-process)
                (lambda (process)
                  (unless (member process (umbriel-test--net-dead ,net))
                    (cl-incf (umbriel-test--net-deleted ,net))
                    (push process (umbriel-test--net-dead ,net))
                    (funcall (plist-get (alist-get process (umbriel-test--net-processes ,net)
                                                   nil nil #'equal)
                                        :sentinel)
                             process "deleted\n")))))
       ,@body)))

(defun umbriel-test--latest (net)
  "NET's newest connection handle."
  (car (car (umbriel-test--net-processes net))))

(defun umbriel-test--feed (net chunk)
  "Deliver CHUNK to the filter of NET's newest connection."
  (funcall (plist-get (umbriel-test--net-args net) :filter)
           (umbriel-test--latest net) chunk))

(defun umbriel-test--close (net event)
  "Deliver EVENT to the sentinel of NET's newest connection."
  (funcall (plist-get (umbriel-test--net-args net) :sentinel)
           (umbriel-test--latest net) event))

(defmacro umbriel-test--results (form)
  "Evaluate FORM with `deliver' bound to a recording callback.
Return a function answering the results recorded so far, oldest first."
  (declare (indent 0))
  `(let ((results nil))
     (let ((deliver (lambda (result) (push result results))))
       ,form)
     (lambda () (reverse results))))

;;;; Socket path

(ert-deftest umbriel/socket-path-explicit-wins ()
  (let ((process-environment '("UMBRIEL_SOCKET=/s.sock" "XDG_RUNTIME_DIR=/run/u"
                               "WAYLAND_DISPLAY=wayland-1")))
    (should (equal (umbriel-socket-path) "/s.sock"))))

(ert-deftest umbriel/socket-path-composes-runtime-and-display ()
  (let ((process-environment '("XDG_RUNTIME_DIR=/run/u" "WAYLAND_DISPLAY=wayland-1")))
    (should (equal (umbriel-socket-path) "/run/u/umbriel-wayland-1.sock"))))

(ert-deftest umbriel/socket-path-empty-counts-as-unset ()
  (let ((process-environment '("UMBRIEL_SOCKET=" "XDG_RUNTIME_DIR=/run/u"
                               "WAYLAND_DISPLAY=wayland-1")))
    (should (equal (umbriel-socket-path) "/run/u/umbriel-wayland-1.sock")))
  (let ((process-environment '("XDG_RUNTIME_DIR=/run/u" "WAYLAND_DISPLAY=")))
    (should-not (umbriel-socket-path))))

(ert-deftest umbriel/no-path-names-the-unset-variables ()
  (let ((process-environment '("XDG_RUNTIME_DIR=/run/u")))
    (cl-letf (((symbol-function 'make-network-process)
               (lambda (&rest _) (ert-fail "connected without a socket path"))))
      (let ((results (umbriel-test--results
                       (umbriel-send (umbriel-query "windows") deliver))))
        (pcase (funcall results)
          (`((:failure no-path ,detail))
           (should (string-match-p "UMBRIEL_SOCKET" detail))
           (should (string-match-p "WAYLAND_DISPLAY" detail))
           (should-not (string-match-p "XDG_RUNTIME_DIR" detail)))
          (other (ert-fail (list 'unexpected other))))))))

;;;; Request objects

(ert-deftest umbriel/query-request ()
  (should (equal (umbriel-query "windows") '((cmd . "windows")))))

(ert-deftest umbriel/action-drops-nil-fields ()
  (should (equal (umbriel-action "window-focus" '((window . "w-1") (output)))
                 '((cmd . "action") (action . "window-focus") (window . "w-1"))))
  (should (equal (umbriel-action "layout-scroll-left")
                 '((cmd . "action") (action . "layout-scroll-left")))))

(ert-deftest umbriel/action-keeps-index-number-and-digit-name-string ()
  "A workspace position is a JSON number; a workspace named \"2\" is a string."
  (should (equal (json-serialize (umbriel-action "workspace-switch"
                                                    '((workspace . 2) (output . "DP-1"))))
                 "{\"cmd\":\"action\",\"action\":\"workspace-switch\",\"workspace\":2,\"output\":\"DP-1\"}"))
  (should (string-match-p "\"workspace\":\"2\""
                          (json-serialize (umbriel-action "workspace-switch"
                                                             '((workspace . "2")))))))

(ert-deftest umbriel/subscribe-events-serialise-as-array ()
  (should (equal (json-serialize umbriel-subscribe-request)
                 "{\"cmd\":\"subscribe\",\"events\":[\"windows\",\"workspaces\"]}")))

;;;; Parsing and framing

(ert-deftest umbriel/parse-false-and-null-are-nil ()
  (should (equal (umbriel-parse "{\"floating\":false,\"urgent\":null,\"focused\":true}")
                 '((floating) (urgent) (focused . t)))))

(ert-deftest umbriel/take-lines-joins-a-line-split-across-chunks ()
  (should (equal (umbriel--take-lines "{\"ok\"" ":null}\n") '(("{\"ok\":null}") . ""))))

(ert-deftest umbriel/take-lines-splits-several-lines ()
  (should (equal (umbriel--take-lines "" "a\nb\nc") '(("a" "b") . "c"))))

(ert-deftest umbriel/take-lines-keeps-an-unterminated-remainder ()
  (should (equal (umbriel--take-lines "a" "b") '(nil . "ab"))))

(ert-deftest umbriel/reply-result-kinds ()
  (should (equal (umbriel--reply-result "{\"ok\":null}") '(:ok nil)))
  (should (equal (umbriel--reply-result "{\"ok\":[{\"id\":\"w\"}]}") '(:ok (((id . "w"))))))
  (should (equal (umbriel--reply-result "{\"err\":\"no such window\"}")
                 '(:failure rejected "no such window")))
  (should (eq (cadr (umbriel--reply-result "[1,2]")) 'malformed))
  (should (eq (cadr (umbriel--reply-result "not json")) 'malformed)))

;;;; Transport

(ert-deftest umbriel/send-writes-the-request-on-connect ()
  (umbriel-test--with-net net
    (umbriel-send (umbriel-query "windows") #'ignore)
    (should (equal (umbriel-test--net-sent net) '("{\"cmd\":\"windows\"}\n")))
    (let ((args (umbriel-test--net-args net)))
      (should (eq (plist-get args :family) 'local))
      (should (equal (plist-get args :service) umbriel-test--socket))
      (should (eq (plist-get args :coding) 'utf-8-unix))
      (should (eq (plist-get args :noquery) t)))))

(ert-deftest umbriel/send-delivers-one-reply-once ()
  "emacs-config:RV-018 F-9: the synchronous sentinel run by `delete-process' delivers nothing."
  (umbriel-test--with-net net
    (let ((results (umbriel-test--results
                     (umbriel-send (umbriel-query "windows") deliver))))
      (umbriel-test--feed net "{\"ok\":")
      (should-not (funcall results))
      (umbriel-test--feed net "[]}\n")
      (should (equal (funcall results) '((:ok nil))))
      (should (= (umbriel-test--net-deleted net) 1))
      (umbriel-test--close net "connection broken by remote peer\n")
      (should (equal (funcall results) '((:ok nil)))))))

(ert-deftest umbriel/send-signalling-write-is-closed ()
  "emacs-config:RV-018 F-8: a write on a connection the server closed reports closed once."
  (umbriel-test--with-net net
    (setf (umbriel-test--net-write-error net) '(error "Process umbriel not running"))
    (let ((results (umbriel-test--results
                     (umbriel-send (umbriel-query "windows") deliver))))
      (pcase (funcall results)
        (`((:failure closed ,_)) t)
        (other (ert-fail (list 'unexpected other)))))))

(ert-deftest umbriel/send-close-after-partial-line-is-closed ()
  "emacs-config:RV-018 F-8: text without a newline at close is not a reply."
  (umbriel-test--with-net net
    (let ((results (umbriel-test--results
                     (umbriel-send (umbriel-query "windows") deliver))))
      (umbriel-test--feed net "{\"ok\":[")
      (umbriel-test--close net "connection broken by remote peer\n")
      (should (equal (funcall results)
                     '((:failure closed "connection broken by remote peer")))))))

(ert-deftest umbriel/send-missing-socket-is-absent ()
  (umbriel-test--with-net net
    (setf (umbriel-test--net-connect-error net)
          '(file-missing "make client process failed" "No such file or directory"))
    (let ((results (umbriel-test--results
                     (umbriel-send (umbriel-query "windows") deliver))))
      (should (equal (funcall results) `((:failure absent ,umbriel-test--socket)))))))

(ert-deftest umbriel/send-refused-socket-is-refused ()
  (umbriel-test--with-net net
    (setf (umbriel-test--net-connect-error net)
          '(file-error "make client process failed" "Connection refused"))
    (let ((results (umbriel-test--results
                     (umbriel-send (umbriel-query "windows") deliver))))
      (pcase (funcall results)
        (`((:failure refused ,detail)) (should (string-match-p "Connection refused" detail)))
        (other (ert-fail (list 'unexpected other)))))))

;;;; Sequencing

(defmacro umbriel-test--with-replies (replies sent &rest body)
  "Run BODY with `umbriel-send' answering REPLIES in order.
SENT is bound to a function answering the requests sent so far, oldest first."
  (declare (indent 2))
  `(let ((queue ,replies) (requests nil))
     (cl-letf (((symbol-function 'umbriel-send)
                (lambda (request callback)
                  (push request requests)
                  (funcall callback (pop queue)))))
       (let ((,sent (lambda () (reverse requests))))
         ,@body))))

(ert-deftest umbriel/send-all-reports-the-last-success ()
  (umbriel-test--with-replies '((:ok nil) (:ok 2)) sent
    (let ((results (umbriel-test--results
                     (umbriel-send-all (list (umbriel-query "a") (umbriel-query "b"))
                                          deliver))))
      (should (equal (funcall results) '((:ok 2))))
      (should (= (length (funcall sent)) 2)))))

(ert-deftest umbriel/send-all-stops-at-the-first-failure ()
  (umbriel-test--with-replies '((:failure rejected "no") (:ok nil)) sent
    (let ((results (umbriel-test--results
                     (umbriel-send-all (list (umbriel-query "a") (umbriel-query "b"))
                                          deliver))))
      (should (equal (funcall results) '((:failure rejected "no"))))
      (should (equal (funcall sent) (list (umbriel-query "a")))))))

;;;; Reporting failures

(ert-deftest umbriel/failure-message-names-each-kind ()
  (dolist (kind '(no-path absent refused closed rejected malformed))
    (should (string-prefix-p (format "umbriel: %s" kind)
                             (umbriel-failure-message (list :failure kind "detail"))))))

(ert-deftest umbriel/echo-failure-is-silent-on-success ()
  (let (messages)
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
      (umbriel-echo-failure '(:ok nil))
      (should-not messages)
      (umbriel-echo-failure '(:failure absent "/s.sock"))
      (should (equal messages (list (umbriel-failure-message '(:failure absent "/s.sock"))))))))

;;;; Held state

(defmacro umbriel-test--with-state (&rest body)
  "Run BODY with the client's held state fresh and isolated.
Binds the private state record, the waiter list and the stream handle
(all `defvar's in `umbriel') to fresh values, and
`umbriel-state-changed-hook' to nil, so a test opts in to the hook and
no test inherits an `open' stream or a leftover waiter from another."
  (declare (indent 0))
  `(let ((umbriel--state nil)
         (umbriel--waiters nil)
         (umbriel--stream nil)
         (umbriel-state-changed-hook nil))
     ,@body))

(defun umbriel-test--holds-window-p (state id)
  "Non-nil when STATE holds a window whose opaque id is ID."
  (and (cl-some (lambda (window) (equal (alist-get 'id window) id))
                (plist-get state :windows))
       t))

(ert-deftest umbriel/set-family-replaces-wholesale-and-is-readable ()
  "EX-1: a family is replaced, never merged, and the readers see it."
  (umbriel-test--with-state
    (umbriel--set-family 'windows '(((id . "w-1")) ((id . "w-2"))))
    (should (equal (umbriel-windows) '(((id . "w-1")) ((id . "w-2")))))
    ;; wholesale: the earlier element is gone, not merged
    (umbriel--set-family 'windows '(((id . "w-3"))))
    (should (equal (umbriel-windows) '(((id . "w-3")))))
    ;; one family never disturbs another
    (umbriel--set-family 'workspaces '(((id . "1"))))
    (should (equal (umbriel-windows) '(((id . "w-3")))))
    (should (equal (umbriel-workspaces) '(((id . "1")))))))

(ert-deftest umbriel/set-family-fires-the-change-hook ()
  "The no-argument hook runs after every write, seeing the new value."
  (umbriel-test--with-state
    (let ((runs 0) (last-windows nil) (last-workspaces nil))
      ;; the state is mutated before `run-hooks'; a hook that reads it sees
      ;; the snapshot that has just landed, not the previous one
      (let ((umbriel-state-changed-hook
             (list (lambda ()
                     (cl-incf runs)
                     (setq last-windows (umbriel-windows)
                           last-workspaces (umbriel-workspaces))))))
        (umbriel--set-family 'windows '(((id . "w-1"))))
        (should (= runs 1))
        (should (equal last-windows '(((id . "w-1")))))
        (should-not last-workspaces)
        (umbriel--set-family 'workspaces '(((id . "1"))))
        (should (= runs 2))
        (should (equal last-workspaces '(((id . "1")))))
        (should (equal last-windows '(((id . "w-1")))))))))

(ert-deftest umbriel/state-is-a-plist-of-four-parts ()
  "`umbriel-state' is the plist waiters receive: all four parts, no more."
  (umbriel-test--with-state
    (umbriel--set-family 'windows '(((id . "w-1"))))
    (umbriel--set-family 'workspaces '(((id . "1"))))
    (let ((state (umbriel-state)))
      (should (equal (plist-get state :windows) '(((id . "w-1")))))
      (should (equal (plist-get state :workspaces) '(((id . "1")))))
      (should (eq (plist-get state :status) 'never))
      (should (= (plist-get state :skipped) 0))
      (should (equal (sort (cl-loop for (key _value) on state by #'cddr
                                    collect key)
                           (lambda (a b) (string< (symbol-name a) (symbol-name b))))
                     '(:skipped :status :windows :workspaces))))))

(ert-deftest umbriel/window-lookup-by-id-and-unknown-id ()
  "A window's opaque `id' is its key; an absent id reads as nil."
  (umbriel-test--with-state
    (should-not (umbriel-window "w-1"))
    (umbriel--set-family 'windows '(((id . "w-1") (title . "shell"))
                                       ((id . "w-2") (title . "editor"))))
    (should (equal (umbriel-window "w-2") '((id . "w-2") (title . "editor"))))
    (should-not (umbriel-window "w-9"))
    ;; replacement, not accumulation: an id dropped by a new snapshot is gone
    (umbriel--set-family 'windows '(((id . "w-2") (title . "editor"))))
    (should-not (umbriel-window "w-1"))))

;;;; Stream lines

(ert-deftest umbriel/stream-line-windows-replace-held-state-and-run-hook ()
  "A windows event is a full snapshot: set through the one setter, hook run."
  (umbriel-test--with-state
    (umbriel--set-family 'windows '(((id . "old"))))
    (let ((runs 0) (seen nil))
      (let ((umbriel-state-changed-hook
             (list (lambda () (cl-incf runs) (push (umbriel-windows) seen)))))
        (umbriel--stream-line
         "{\"event\":\"windows\",\"data\":[{\"id\":\"w-1\",\"title\":\"a\"}]}")
        (should (equal (umbriel-windows) '(((id . "w-1") (title . "a")))))
        (should (= runs 1))
        ;; the hook saw the new snapshot, once
        (should (equal (car seen) '(((id . "w-1") (title . "a")))))
        (should (= (length seen) 1))
        (should-not (umbriel-workspaces))
        (should (= (umbriel-skipped) 0))
        (should (eq (umbriel-stream-status) 'never))))))

(ert-deftest umbriel/stream-line-workspaces-replace-held-state ()
  (umbriel-test--with-state
    (umbriel--set-family 'workspaces '(((id . "old"))))
    (umbriel--stream-line
     "{\"event\":\"workspaces\",\"data\":[{\"id\":\"1\",\"name\":\"one\"}]}")
    (should (equal (umbriel-workspaces) '(((id . "1") (name . "one")))))
    (should-not (umbriel-windows))
    (should (= (umbriel-skipped) 0))))

(ert-deftest umbriel/stream-line-unrequested-family-bumps-skipped ()
  "A family we did not subscribe to is counted, never fatal (VT-1)."
  (umbriel-test--with-state
    (let ((runs 0))
      (let ((umbriel-state-changed-hook (list (lambda () (cl-incf runs)))))
        (umbriel--stream-line "{\"event\":\"theme\",\"data\":{\"name\":\"dark\"}}")
        (should (= (umbriel-skipped) 1))
        ;; the skipped count is a write: the hook runs for it too
        (should (= runs 1))
        (should-not (umbriel-windows))
        (should-not (umbriel-workspaces))
        (should (eq (umbriel-stream-status) 'never))))))

(ert-deftest umbriel/stream-line-malformed-bumps-skipped-without-error ()
  "A line that is not what we expect is counted and skipped; a signal would
fail this test, and C-4 puts a reply-shaped object in the same bucket."
  (umbriel-test--with-state
    (umbriel--stream-line "not json at all")
    (should (= (umbriel-skipped) 1))
    (umbriel--stream-line "[1,2]")
    (should (= (umbriel-skipped) 2))
    (umbriel--stream-line "{\"ok\":null}")
    (should (= (umbriel-skipped) 3))
    (should (eq (umbriel-stream-status) 'never))
    (should-not (umbriel-windows))))

(ert-deftest umbriel/stream-line-err-line-is-rejected ()
  "C-1: the err line sets `rejected' *before* closing, so the close's own
sentinel cannot overwrite it with `closed'."
  (umbriel-test--with-state
    (umbriel-test--with-net net
      (umbriel-stream-open)
      (umbriel-test--feed net "{\"err\":\"unknown subscription event: theme\"}\n")
      (should (equal (umbriel-stream-status)
                     '(:failure rejected "unknown subscription event: theme")))
      (should (>= (umbriel-test--net-deleted net) 1))
      ;; the fake sentinel ran with "deleted\n" during that delete
      (should (eq (car (umbriel-stream-status)) :failure))
      (should (eq (cadr (umbriel-stream-status)) 'rejected)))))

;;;; Stream lifecycle

(ert-deftest umbriel/stream-status-is-never-before-open ()
  (umbriel-test--with-state
    (umbriel-test--with-net net
      (should (eq (umbriel-stream-status) 'never))
      (should (= (umbriel-test--net-connections net) 0)))))

(ert-deftest umbriel/stream-open-subscribes-once-and-is-open ()
  "The one subscription is written on the one connection, then snapshots flow."
  (umbriel-test--with-state
    (umbriel-test--with-net net
      (umbriel-stream-open)
      (should (eq (umbriel-stream-status) 'open))
      (should (= (umbriel-test--net-connections net) 1))
      (should (equal (umbriel-test--net-sent net)
                     '("{\"cmd\":\"subscribe\",\"events\":[\"windows\",\"workspaces\"]}\n")))
      (let ((args (umbriel-test--net-args net)))
        (should (eq (plist-get args :family) 'local))
        (should (equal (plist-get args :service) umbriel-test--socket))
        (should (eq (plist-get args :coding) 'utf-8-unix))
        (should (eq (plist-get args :noquery) t)))
      ;; no {"ok"} ack: the first lines are the snapshots themselves
      (umbriel-test--feed net "{\"event\":\"windows\",\"data\":[{\"id\":\"w-1\"}]}\n")
      (should (equal (umbriel-windows) '(((id . "w-1")))))
      (should (eq (umbriel-stream-status) 'open)))))

(ert-deftest umbriel/stream-open-twice-makes-one-connection ()
  "emacs-config:RV-019 F-2: opening an open stream is a no-op, not a second subscriber."
  (umbriel-test--with-state
    (umbriel-test--with-net net
      (umbriel-stream-open)
      (umbriel-stream-open)
      (should (= (umbriel-test--net-connections net) 1))
      (should (= (length (umbriel-test--net-sent net)) 1))
      (should (eq (umbriel-stream-status) 'open)))))

(ert-deftest umbriel/stream-sentinel-closes-and-keeps-snapshots ()
  (umbriel-test--with-state
    (umbriel-test--with-net net
      (umbriel-stream-open)
      (umbriel-test--feed net "{\"event\":\"windows\",\"data\":[{\"id\":\"w-1\"}]}\n")
      (umbriel-test--close net "connection broken by remote peer\n")
      (should (equal (umbriel-stream-status)
                     '(:failure closed "connection broken by remote peer")))
      ;; the buffer still shows them, under a header that says why they are stale
      (should (equal (umbriel-windows) '(((id . "w-1"))))))))

(ert-deftest umbriel/stream-close-returns-to-never ()
  "Nothing went wrong, so the close lands in `never', not in `failed'."
  (umbriel-test--with-state
    (umbriel-test--with-net net
      (umbriel-stream-open)
      (umbriel-test--feed net "{\"event\":\"windows\",\"data\":[{\"id\":\"w-1\"}]}\n")
      (umbriel-stream-close)
      (should (= (umbriel-test--net-deleted net) 1))
      ;; the fake `delete-process' has already run the sentinel with "deleted\n"
      (should (eq (umbriel-stream-status) 'never))
      (should (equal (umbriel-windows) '(((id . "w-1"))))))))

(ert-deftest umbriel/stream-close-after-a-failure-returns-to-never ()
  "Design: failed --> never on stream-close, though no process is left."
  (umbriel-test--with-state
    (umbriel-test--with-net net
      (umbriel-stream-open)
      (umbriel-test--close net "connection broken by remote peer\n")
      (umbriel-stream-close)
      (should (eq (umbriel-stream-status) 'never))
      (should (= (umbriel-test--net-connections net) 1)))))

(defun umbriel-test--reconnect-on-failure ()
  "Register a waiter whose failure callback reopens the stream, as `g' would."
  (umbriel-await #'ignore (lambda (_result) (umbriel-stream-open))))

(ert-deftest umbriel/stream-close-detaches-before-announcing ()
  "A reconnect from inside the close's announcement keeps its connection."
  (umbriel-test--with-state
    (umbriel-test--with-net net
      (umbriel-stream-open)
      (umbriel-test--reconnect-on-failure)
      (umbriel-stream-close)
      (should (eq (umbriel-stream-status) 'open))
      (should (equal umbriel--stream (umbriel-test--latest net)))
      (should-not (member umbriel--stream (umbriel-test--net-dead net))))))

(ert-deftest umbriel/stream-rejected-detaches-before-announcing ()
  "A reconnect from inside the rejection's announcement keeps its connection."
  (umbriel-test--with-state
    (umbriel-test--with-net net
      (umbriel-stream-open)
      (umbriel-test--reconnect-on-failure)
      (umbriel-test--feed net "{\"err\":\"no\"}\n")
      (should (eq (umbriel-stream-status) 'open))
      (should (= (umbriel-test--net-connections net) 2))
      (should (equal umbriel--stream (umbriel-test--latest net)))
      (should-not (member umbriel--stream (umbriel-test--net-dead net))))))

(ert-deftest umbriel/stream-open-subscribes-before-announcing ()
  "A close from inside the open's announcement wins: the stream is `never'."
  (umbriel-test--with-state
    (umbriel-test--with-net net
      (let ((umbriel-state-changed-hook
             (list (lambda ()
                     (when (eq (umbriel-stream-status) 'open)
                       (umbriel-stream-close))))))
        (umbriel-stream-open)
        (should (eq (umbriel-stream-status) 'never))
        (should (= (length (umbriel-test--net-sent net)) 1))
        (should-not umbriel--stream)))))

(ert-deftest umbriel/stream-line-event-without-data-is-skipped ()
  "Design: a format change is counted, not silently taken as an empty desktop."
  (umbriel-test--with-state
    (umbriel--set-family 'windows '(((id . "w-1"))))
    (umbriel--stream-line "{\"event\":\"windows\",\"payload\":[]}")
    (should (= (umbriel-skipped) 1))
    (should (equal (umbriel-windows) '(((id . "w-1")))))))

(ert-deftest umbriel/stream-open-resets-skipped ()
  (umbriel-test--with-state
    (umbriel-test--with-net net
      (umbriel-stream-open)
      (umbriel-test--feed net "{\"event\":\"theme\",\"data\":{}}\n")
      (umbriel-test--feed net "nonsense\n")
      (should (= (umbriel-skipped) 2))
      (umbriel-stream-close)
      (umbriel-stream-open)
      (should (= (umbriel-skipped) 0))
      (should (eq (umbriel-stream-status) 'open)))))

(ert-deftest umbriel/stream-open-failure-reports-absent-without-a-socket ()
  (umbriel-test--with-state
    (umbriel-test--with-net net
      (setf (umbriel-test--net-connect-error net)
            '(file-missing "make client process failed" "No such file or directory"))
      (umbriel-stream-open)
      (should (equal (umbriel-stream-status)
                     `(:failure absent ,umbriel-test--socket)))
      ;; nothing was sent: there was no connection to send on
      (should-not (umbriel-test--net-sent net)))))

(ert-deftest umbriel/no-auto-reconnect-after-a-serverside-close ()
  "emacs-config:RV-019 F-2 / emacs-config:DEC-018: nothing reconnects by itself; `g' is the reconnect."
  (umbriel-test--with-state
    (umbriel-test--with-net net
      (umbriel-stream-open)
      (umbriel-test--close net "connection broken by remote peer\n")
      (should (equal (umbriel-stream-status)
                     '(:failure closed "connection broken by remote peer")))
      (should (= (umbriel-test--net-connections net) 1))
      (should (= (length (umbriel-test--net-sent net)) 1))
      ;; a later change does not sneak a subscription in either
      (umbriel--set-family 'windows '(((id . "w-1"))))
      (should (= (umbriel-test--net-connections net) 1)))))

;;;; Waiters

(ert-deftest umbriel/await-fires-at-registration-when-the-predicate-holds ()
  (umbriel-test--with-state
    (umbriel-test--with-net net
      (umbriel-stream-open)
      (umbriel--set-family 'windows '(((id . "w-1"))))
      (let ((results (umbriel-test--results
                       (umbriel-await (lambda (state)
                                           (umbriel-test--holds-window-p state "w-1"))
                                         deliver))))
        (pcase (funcall results)
          (`((:ok ,state))
           (should (equal (plist-get state :windows) '(((id . "w-1"))))))
          (other (ert-fail (list 'unexpected other))))))))

(ert-deftest umbriel/await-fires-once-on-the-change-that-makes-it-hold ()
  "One-shot: the waiter is dropped when it fires, a later change adds nothing."
  (umbriel-test--with-state
    (umbriel-test--with-net net
      (umbriel-stream-open)
      (let ((results (umbriel-test--results
                       (umbriel-await (lambda (state)
                                           (umbriel-test--holds-window-p state "w-1"))
                                         deliver))))
        (should-not (funcall results))
        (umbriel-test--feed net "{\"event\":\"windows\",\"data\":[{\"id\":\"w-1\"}]}\n")
        (pcase (funcall results)
          (`((:ok ,state))
           (should (equal (plist-get state :windows) '(((id . "w-1"))))))
          (other (ert-fail (list 'unexpected other))))
        (umbriel-test--feed net (concat "{\"event\":\"windows\",\"data\":"
                                           "[{\"id\":\"w-1\"},{\"id\":\"w-2\"}]}\n"))
        (should (= (length (funcall results)) 1))))))

(ert-deftest umbriel/await-fails-when-the-stream-closes ()
  "No waiter outlives the stream, and it fires only the once."
  (umbriel-test--with-state
    (umbriel-test--with-net net
      (umbriel-stream-open)
      (let ((results (umbriel-test--results
                       (umbriel-await (lambda (state)
                                           (umbriel-test--holds-window-p state "w-1"))
                                         deliver))))
        (should-not (funcall results))
        (umbriel-test--close net "connection broken by remote peer\n")
        (pcase (funcall results)
          (`((:failure closed ,_)) t)
          (other (ert-fail (list 'unexpected other))))
        (umbriel--set-family 'windows '(((id . "w-1"))))
        (should (= (length (funcall results)) 1))))))

(ert-deftest umbriel/await-fails-at-once-when-the-stream-is-not-open ()
  "C-3: registering while not open fails at once, without running the
predicate; a failed stream hands over the failure it already has."
  (umbriel-test--with-state
    (umbriel-test--with-net net
      (let ((predicate-called nil))
        (let ((results (umbriel-test--results
                         (umbriel-await (lambda (_state) (setq predicate-called t))
                                           deliver))))
          (should-not predicate-called)
          (pcase (funcall results)
            (`((:failure closed ,detail)) (should (string-match-p "not open" detail)))
            (other (ert-fail (list 'unexpected other))))))))
  (umbriel-test--with-state
    (umbriel-test--with-net net
      (umbriel-stream-open)
      (umbriel-test--close net "connection broken by remote peer\n")
      (let ((results (umbriel-test--results
                       (umbriel-await (lambda (_state) t) deliver))))
        (pcase (funcall results)
          (`((:failure closed "connection broken by remote peer")) t)
          (other (ert-fail (list 'unexpected other))))))))

(ert-deftest umbriel/waiter-signalling-predicate-drops-only-its-own-waiter ()
  "A predicate that signals drops its own waiter, at registration and on a
later change; the hook and the other waiter are untouched."
  (umbriel-test--with-state
    (umbriel-test--with-net net
      (umbriel-stream-open)
      (let ((at-registration 0) (on-change 0) (fired nil) (runs 0))
        (let ((umbriel-state-changed-hook (list (lambda () (cl-incf runs)))))
          ;; signals while it is evaluated at registration
          (umbriel-await (lambda (_state)
                              (cl-incf at-registration)
                              (error "predicate boom"))
                            #'ignore)
          ;; nil for now, signals on the change that gives it a window: this is
          ;; the notify path, not the registration path
          (umbriel-await (lambda (state)
                              (when (plist-get state :windows)
                                (cl-incf on-change)
                                (error "late predicate boom")))
                            #'ignore)
          ;; a well-behaved waiter on that same change
          (umbriel-await (lambda (state)
                              (umbriel-test--holds-window-p state "w-1"))
                            (lambda (result) (push result fired)))
          (should (= at-registration 1))
          (should (= on-change 0))
          (umbriel-test--feed net "{\"event\":\"windows\",\"data\":[{\"id\":\"w-1\"}]}\n")
          (should (= at-registration 1)) ; dropped at registration
          (should (= on-change 1))       ; consulted, signalled, dropped
          (should (pcase fired (`((:ok ,_)) t) (_ nil)))
          (should (= runs 1))
          ;; dropped, so the next change must not consult it again (and nothing
          ;; may signal out of the filter)
          (umbriel-test--feed net (concat "{\"event\":\"windows\",\"data\":"
                                             "[{\"id\":\"w-1\"},{\"id\":\"w-2\"}]}\n"))
          (should (= on-change 1))
          (should (= runs 2)))))))

(ert-deftest umbriel/waiter-signalling-callback-drops-only-its-own-waiter ()
  "A callback that signals drops its own waiter *before* being called -- at
registration and on a later change -- so it cannot fire twice and cannot
stop the hook or a waiter that becomes true on the same change."
  (umbriel-test--with-state
    (umbriel-test--with-net net
      (umbriel-stream-open)
      (umbriel--set-family 'windows '(((id . "w-1"))))
      (let ((bad-calls 0) (late-calls 0) (other-calls 0) (runs 0))
        ;; this one's predicate already holds: it fires during registration,
        ;; writes once more from inside the callback, then signals
        (umbriel-await (lambda (state) (umbriel-test--holds-window-p state "w-1"))
                          (lambda (_result)
                            (cl-incf bad-calls)
                            (when (= bad-calls 1)
                              (umbriel--set-family 'windows '(((id . "w-1")))))
                            (error "callback boom")))
        ;; these two only become true on a later change and signal from their
        ;; callback there; the well-behaved one sits between them, so a
        ;; signalling callback comes before it in one notification order and
        ;; after it in the other
        (umbriel-await (lambda (state) (umbriel-test--holds-window-p state "w-3"))
                          (lambda (_result) (cl-incf late-calls) (error "late boom")))
        (umbriel-await (lambda (state) (umbriel-test--holds-window-p state "w-2"))
                          (lambda (_result) (cl-incf other-calls)))
        (umbriel-await (lambda (state) (umbriel-test--holds-window-p state "w-4"))
                          (lambda (_result) (cl-incf late-calls) (error "late boom")))
        ;; dropped before the call: the write it made did not fire it again
        (should (= bad-calls 1))
        (should (= late-calls 0))
        (should (= other-calls 0))
        (let ((umbriel-state-changed-hook (list (lambda () (cl-incf runs)))))
          (umbriel-test--feed net (concat "{\"event\":\"windows\",\"data\":"
                                             "[{\"id\":\"w-1\"},{\"id\":\"w-2\"},"
                                             "{\"id\":\"w-3\"},{\"id\":\"w-4\"}]}\n"))
          (should (= bad-calls 1))   ; gone for good
          (should (= late-calls 2))  ; both signalling callbacks ran, once each
          (should (= other-calls 1)) ; neither of them stopped this one
          (should (= runs 1))        ; nor the hook
          ;; a further change does not reach any of the dropped callbacks
          (umbriel-test--feed net (concat "{\"event\":\"windows\",\"data\":"
                                             "[{\"id\":\"w-1\"},{\"id\":\"w-2\"},"
                                             "{\"id\":\"w-3\"},{\"id\":\"w-4\"}]}\n"))
          (should (= bad-calls 1))
          (should (= late-calls 2))
          (should (= other-calls 1))
          (should (= runs 2)))))))

(ert-deftest umbriel/waiters-run-after-the-hook ()
  "The buffer repaints from the hook before any waiter's callback runs."
  (umbriel-test--with-state
    (umbriel-test--with-net net
      (umbriel-stream-open)
      (let ((order nil))
        (let ((umbriel-state-changed-hook (list (lambda () (push 'hook order)))))
          (umbriel-await (lambda (state)
                              (umbriel-test--holds-window-p state "w-1"))
                            (lambda (_result) (push 'waiter order)))
          (umbriel-test--feed net "{\"event\":\"windows\",\"data\":[{\"id\":\"w-1\"}]}\n")
          (should (equal (reverse order) '(hook waiter))))))))

;;;; Refresh

(ert-deftest umbriel/refresh-writes-both-families-through-the-setter ()
  "VT-4: two queries, both `(:ok DATA)' replies routed through the setter."
  (umbriel-test--with-state
    (let ((runs 0))
      (let ((umbriel-state-changed-hook (list (lambda () (cl-incf runs)))))
        (umbriel-test--with-replies '((:ok (((id . "w-1") (title . "a"))))
                                         (:ok (((id . "1") (name . "one")))))
          sent
          (let ((results (umbriel-test--results (umbriel-refresh deliver))))
            (should (equal (funcall sent) (list (umbriel-query "windows")
                                                (umbriel-query "workspaces"))))
            (should (equal (umbriel-windows) '(((id . "w-1") (title . "a")))))
            (should (equal (umbriel-workspaces) '(((id . "1") (name . "one")))))
            (should (= runs 2))
            ;; refresh never speaks for the stream
            (should (eq (umbriel-stream-status) 'never))
            (pcase (funcall results)
              (`((:ok ,_)) t)
              (other (ert-fail (list 'unexpected other))))))))))

(ert-deftest umbriel/refresh-reports-the-first-failure-without-touching-state ()
  "A failure leaves the held snapshots alone and stops the pair of queries."
  (umbriel-test--with-state
    (umbriel--set-family 'windows '(((id . "old"))))
    (let ((runs 0))
      (let ((umbriel-state-changed-hook (list (lambda () (cl-incf runs)))))
        (umbriel-test--with-replies '((:failure rejected "no")
                                         (:ok (((id . "1") (name . "one")))))
          sent
          (let ((results (umbriel-test--results (umbriel-refresh deliver))))
            (should (equal (funcall results) '((:failure rejected "no"))))
            (should (equal (umbriel-windows) '(((id . "old")))))
            (should-not (umbriel-workspaces))
            (should (= runs 0))
            (should (equal (funcall sent) (list (umbriel-query "windows"))))))))))

(provide 'umbriel-test)
;;; umbriel-test.el ends here
