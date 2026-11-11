;;; umbriel-ipc.el --- Talk to umbriel over its socket -*- lexical-binding: t; -*-

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

;; Umbriel, the Wayland compositor, answers JSON requests on a UNIX
;; socket: one object per line, then `{"ok":…}' or `{"err":…}' back
;; before the server closes the connection.  This file is the only
;; place that talks to it, and it executes no program: no `umbriel'
;; binary to be missing, no dev-shell library path to break.
;;
;; Every request reports one shape, `(:ok VALUE)' or
;; `(:failure KIND DETAIL)', where KIND is one of:
;;
;;   no-path    the socket path cannot be resolved from the environment
;;   absent     no socket at that path
;;   refused    a socket, but nothing accepting (a stale socket)
;;   closed     the connection ended before a reply
;;   rejected   the compositor answered `{"err":…}'
;;   malformed  the reply was not a JSON object
;;
;; Everything is asynchronous: callers pass a callback, nothing blocks.
;;
;; Besides one-off requests there is one long-lived subscribe
;; connection, the stream: the compositor answers a subscription with a
;; snapshot of each requested family and then sends every change.  This
;; file holds the last snapshot it was sent, in one state record with
;; one writer per part, and announces each write through
;; `umbriel-state-changed-hook' and to `umbriel-await' waiters.
;; Only the stream speaks for the stream: nothing reconnects on its own,
;; so a closed stream stays visible instead of going silently stale.
;; See docs/umbriel.md.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

;;;; Socket path

(defun umbriel--present-p (value)
  "Non-nil when VALUE, an environment value, is set and not empty."
  (and value (not (string-empty-p value))))

(defun umbriel-socket-path ()
  "Umbriel's socket path from the environment, or nil.
Mirrors the umbriel CLI: UMBRIEL_SOCKET, else
$XDG_RUNTIME_DIR/umbriel-$WAYLAND_DISPLAY.sock."
  (let ((explicit (getenv "UMBRIEL_SOCKET"))
        (runtime (getenv "XDG_RUNTIME_DIR"))
        (display (getenv "WAYLAND_DISPLAY")))
    (cond ((umbriel--present-p explicit) explicit)
          ((and (umbriel--present-p runtime) (umbriel--present-p display))
           (format "%s/umbriel-%s.sock" runtime display)))))

(defun umbriel--no-path-failure ()
  "The failure for an unresolvable socket path, naming the unset variables."
  (list :failure 'no-path
        (concat (string-join (seq-remove (lambda (name) (umbriel--present-p (getenv name)))
                                         '("UMBRIEL_SOCKET" "XDG_RUNTIME_DIR" "WAYLAND_DISPLAY"))
                             ", ")
                " unset")))

;;;; Request objects

(defun umbriel-query (cmd)
  "The request object for the query CMD, e.g. \"windows\"."
  `((cmd . ,cmd)))

(defun umbriel-action (action &optional fields)
  "The request object for ACTION with FIELDS, an alist of action fields.
Fields whose value is nil are dropped: the compositor rejects empty ones."
  `((cmd . "action") (action . ,action) ,@(seq-filter #'cdr fields)))

(defconst umbriel-subscribe-request
  '((cmd . "subscribe") (events . ["windows" "workspaces"]))
  "The request that opens the windows and workspaces event stream.")

;;;; Parsing and framing

(defun umbriel-parse (line)
  "Parse LINE, one JSON value from umbriel.
Objects become alists, arrays lists, and false and null nil."
  (json-parse-string line :object-type 'alist :array-type 'list
                     :false-object nil :null-object nil))

(defun umbriel--parse-object (line)
  "LINE parsed when it is a JSON object, else nil."
  (let ((value (condition-case nil (umbriel-parse line) (json-error nil))))
    (and (consp value) (consp (car value)) (symbolp (caar value)) value)))

(defun umbriel--malformed (line)
  "The failure for LINE, which is not what umbriel should send."
  (list :failure 'malformed (truncate-string-to-width line 80 nil nil "…")))

(defun umbriel--take-lines (pending chunk)
  "Split PENDING text plus CHUNK into (LINES . REST).
LINES are the complete lines; REST is the unterminated remainder."
  (let ((parts (split-string (concat pending chunk) "\n")))
    (cons (butlast parts) (car (last parts)))))

(defun umbriel--reply-result (line)
  "The result of the reply LINE."
  (let ((reply (umbriel--parse-object line)))
    (cond ((assq 'ok reply) (list :ok (alist-get 'ok reply)))
          ((assq 'err reply) (list :failure 'rejected (format "%s" (alist-get 'err reply))))
          (t (umbriel--malformed line)))))

;;;; Transport

(defun umbriel--connect-failure (err path)
  "The failure for ERR, signalled connecting to PATH."
  (if (eq (car err) 'file-missing)
      (list :failure 'absent path)
    (list :failure 'refused (error-message-string err))))

(defun umbriel--connect (path filter sentinel)
  "Open a connection to the socket at PATH with FILTER and SENTINEL."
  (make-network-process :name "umbriel" :family 'local :service path
                        :coding 'utf-8-unix :noquery t
                        :filter filter :sentinel sentinel))

(defun umbriel-send (request callback)
  "Send REQUEST to umbriel on a fresh connection.
CALLBACK receives the result exactly once, when the connection ends."
  (if-let* ((path (umbriel-socket-path)))
      (let ((pending "") (delivered nil) (process nil))
        (cl-flet ((deliver (result)
                    ;; Flag first: `delete-process' runs the sentinel at once.
                    (unless delivered
                      (setq delivered t)
                      (unwind-protect (funcall callback result)
                        (when process (delete-process process))))))
          (condition-case err
              (setq process
                    (umbriel--connect
                     path
                     (lambda (_process chunk)
                       (pcase-let ((`(,lines . ,rest) (umbriel--take-lines pending chunk)))
                         (setq pending rest)
                         (when lines (deliver (umbriel--reply-result (car lines))))))
                     (lambda (_process event)
                       (deliver (list :failure 'closed (string-trim event))))))
            (file-error (deliver (umbriel--connect-failure err path))))
          (when process
            (condition-case err
                (process-send-string process (concat (json-serialize request) "\n"))
              (error (deliver (list :failure 'closed (error-message-string err))))))))
    (funcall callback (umbriel--no-path-failure))))

(defun umbriel-send-all (requests callback)
  "Send REQUESTS in order, each after the previous one succeeded.
CALLBACK receives the first failure, or the last success."
  (umbriel-send (car requests)
                   (lambda (result)
                     (if (or (eq (car result) :failure) (null (cdr requests)))
                         (funcall callback result)
                       (umbriel-send-all (cdr requests) callback)))))

;;;; Held state

;; One record, written only by the three functions below.  Kept as a
;; plain plist so it can be `let'-bound; nil reads as the initial state,
;; which is what a fresh Emacs starts from.

(defgroup umbriel nil
  "Emacs client for the umbriel Wayland compositor."
  :group 'applications
  :prefix "umbriel-"
  :link '(url-link "https://github.com/davidlee/umbriel.el"))

(defvar umbriel--state nil
  "The held state: a plist of :windows, :workspaces, :status, :skipped.
Nil reads as the initial state: no snapshots, status `never', nothing
skipped.  Only `umbriel--set-family', `umbriel--set-status' and
`umbriel--note-skipped' write it.")

(defvar umbriel--stream nil
  "The one stream connection, or nil when the stream is not open.")

(defvar umbriel--waiters nil
  "Pending one-shot waiters, each a cons of PREDICATE and CALLBACK.")

(defvar umbriel-state-changed-hook nil
  "Hook run after every write to the held state, with no arguments.
Internal, not an option: the buffer listens here to repaint itself.
Waiters are settled after the hook, so the repaint has already happened
by the time a waiter's callback runs.")

(defun umbriel-windows ()
  "The held windows snapshot, as the compositor sent it, or nil."
  (plist-get umbriel--state :windows))

(defun umbriel-workspaces ()
  "The held workspaces snapshot, as the compositor sent it, or nil."
  (plist-get umbriel--state :workspaces))

(defun umbriel-stream-status ()
  "The stream status: `never', `open', or a `(:failure KIND DETAIL)'."
  (or (plist-get umbriel--state :status) 'never))

(defun umbriel-skipped ()
  "How many stream lines were skipped on the current connection."
  (or (plist-get umbriel--state :skipped) 0))

(defun umbriel-window (id)
  "The held window whose opaque `id' is ID, or nil when none has it."
  (seq-find (lambda (window) (equal (alist-get 'id window) id))
            (umbriel-windows)))

(defun umbriel-state ()
  "The held state as a plist of :windows, :workspaces, :status, :skipped.
This is the STATE a waiter's predicate and callback receive."
  (list :windows (umbriel-windows)
        :workspaces (umbriel-workspaces)
        :status (umbriel-stream-status)
        :skipped (umbriel-skipped)))

;;;; Announcing change

(defun umbriel--announce ()
  "Run the change hook, then settle the waiters against the new state.
The state has been written before this is called, so a hook entry that
reads it sees the write that just landed."
  (run-hooks 'umbriel-state-changed-hook)
  (umbriel--notify-waiters))

(defun umbriel--set-family (family data)
  "Replace the held FAMILY snapshot with DATA, then announce the change.
FAMILY is `windows' or `workspaces'.  Stream events and refresh replies
are full snapshots, so nothing is merged; this is the only writer of the
snapshots."
  (setq umbriel--state (plist-put umbriel--state
                                     (intern (concat ":" (symbol-name family)))
                                     data))
  (umbriel--announce))

(defun umbriel--set-status (status)
  "Record STATUS as the stream status, then announce it.
The only writer of the status.  A new status starts or ends the stream,
so pending waiters are settled here rather than consulted: a failure
hands them that failure, `never' the closing one, and `open' -- the only
status a waiter may outlive -- consults their predicates."
  (setq umbriel--state (plist-put umbriel--state :status status))
  (run-hooks 'umbriel-state-changed-hook)
  (pcase status
    ('open (umbriel--notify-waiters))
    ('never (umbriel--fail-waiters (list :failure 'closed "stream closed")))
    (_ (umbriel--fail-waiters status))))

(defun umbriel--note-skipped (&optional count)
  "Write COUNT as the number of skipped stream lines, then announce it.
COUNT nil counts one more line; `umbriel-stream-open' passes 0 to
start a connection with nothing skipped.  The only writer of the counter."
  (setq umbriel--state (plist-put umbriel--state :skipped
                                     (or count (1+ (umbriel-skipped)))))
  (umbriel--announce))

;;;; Waiters

(defun umbriel--quietly (function &rest args)
  "Call FUNCTION with ARGS, demoting a signal to nil.
Answers (t . VALUE) for the call's value, and nil when it signalled: a
waiter that fails must not stop the hook or the other waiters."
  (condition-case err
      (cons t (apply function args))
    (error (message "umbriel: waiter signalled: %s" (error-message-string err))
           nil)))

(defun umbriel--notify-waiters ()
  "Settle every pending waiter against the current state.
A waiter whose predicate holds, or signals, is dropped; one whose
predicate holds also gets CALLBACK with `(:ok STATE)'.  The list is
walked over a snapshot, and a waiter is dropped before its callback
runs, so a callback that writes state or registers a waiter can neither
make a waiter fire twice nor corrupt the walk."
  (let ((state (umbriel-state)))
    (dolist (waiter (copy-sequence umbriel--waiters))
      (when (memq waiter umbriel--waiters)
        (let ((outcome (umbriel--quietly (car waiter) state)))
          (cond ((null outcome)         ; the predicate signalled
                 (setq umbriel--waiters (delq waiter umbriel--waiters)))
                ((cdr outcome)
                 (setq umbriel--waiters (delq waiter umbriel--waiters))
                 (umbriel--quietly (cdr waiter) (list :ok state)))))))))

(defun umbriel--fail-waiters (failure)
  "Hand FAILURE to every pending waiter and forget them all.
The list is cleared before any callback runs, so a callback that
registers a waiter does not resurrect a dead stream's waiters."
  (let ((waiters umbriel--waiters))
    (setq umbriel--waiters nil)
    (dolist (waiter waiters)
      (umbriel--quietly (cdr waiter) failure))))

(defun umbriel--not-open-failure ()
  "The failure for a waiter registered while the stream is not open."
  (let ((status (umbriel-stream-status)))
    (if (eq (car-safe status) :failure)
        status
      (list :failure 'closed "stream not open"))))

(defun umbriel-await (predicate callback)
  "Call CALLBACK once, when PREDICATE holds of the held state.
PREDICATE is called with the state (`umbriel-state') at registration
and after each change; when it holds, CALLBACK gets `(:ok STATE)' and
the waiter is dropped.  Registering while the stream is not open calls
CALLBACK at once with the current failure, without consulting PREDICATE:
the stream is what keeps a waiter alive, so no waiter outlives it.  A
predicate or a callback that signals drops its own waiter."
  (if (eq (umbriel-stream-status) 'open)
      (progn
        (push (cons predicate callback) umbriel--waiters)
        (umbriel--notify-waiters))
    (umbriel--quietly callback (umbriel--not-open-failure))))

;;;; Stream lines

(defun umbriel--stream-line (line)
  "Handle one complete LINE from the stream; never signal.
A `windows' or `workspaces' event carrying `data' replaces that family
through the one setter.  An `err' line is the compositor refusing the
subscription: answer `(:failure rejected MSG)' for the filter to record
once the connection is gone.  Anything else -- an unrequested family,
an event without `data', a reply-shaped object, or no JSON object at
all -- is counted and skipped.  No I/O here: the filter owns the process."
  (let* ((event (umbriel--parse-object line))
         (family (alist-get 'event event)))
    (cond
     ((and (member family '("windows" "workspaces")) (assq 'data event))
      (umbriel--set-family (intern family) (alist-get 'data event))
      nil)
     ((assq 'err event)
      (list :failure 'rejected (format "%s" (alist-get 'err event))))
     (t
      (umbriel--note-skipped)
      nil))))

;;;; The stream connection

(defun umbriel--forget-process ()
  "Forget the stream handle, then delete the process.
Detaching first keeps the delete's own sentinel event from being taken
for a close by the server.  Callers detach before they announce the new
status, so a hook or waiter that reconnects keeps its new connection."
  (when-let* ((process umbriel--stream))
    (setq umbriel--stream nil)
    (delete-process process)))

(defun umbriel--subscribe (process)
  "Make PROCESS the stream and write the subscription on it.
Answer non-nil on success; on a failed write, forget PROCESS and record
the failure."
  (setq umbriel--stream process)
  (condition-case err
      (progn (process-send-string
              process (concat (json-serialize umbriel-subscribe-request) "\n"))
             t)
    (error (umbriel--forget-process)
           (umbriel--set-status (list :failure 'closed (error-message-string err)))
           nil)))

(defun umbriel-stream-open ()
  "Open the one stream connection, unless the stream is already open.
The subscription is written as soon as the connection exists; the
compositor then sends a snapshot of each requested family, without an
acknowledgement.  A connect that fails leaves the status at `(:failure
KIND DETAIL)'; nothing retries on its own -- `g' reconnects."
  (unless (eq (umbriel-stream-status) 'open)
    (if-let* ((path (umbriel-socket-path)))
        (let ((pending "") (process nil))
          (condition-case err
              (setq process
                    (umbriel--connect
                     path
                     (lambda (_process chunk)
                       (pcase-let ((`(,lines . ,rest)
                                    (umbriel--take-lines pending chunk)))
                         (setq pending rest)
                         (catch 'closed
                           (dolist (line lines)
                             (when-let* ((failure (umbriel--stream-line line)))
                               (umbriel--forget-process)
                               (umbriel--set-status failure)
                               (throw 'closed nil))))))
                     (lambda (process event)
                       (when (eq process umbriel--stream)
                         (setq umbriel--stream nil)
                         (umbriel--set-status
                          (list :failure 'closed (string-trim event)))))))
            (file-error
             (umbriel--set-status (umbriel--connect-failure err path))))
          (when (and process (umbriel--subscribe process))
            (umbriel--note-skipped 0)
            (umbriel--set-status 'open)))
      (umbriel--set-status (umbriel--no-path-failure)))))

(defun umbriel-stream-close ()
  "Close the stream on request: status back to `never', snapshots kept.
Nothing went wrong, so this does not land in `failed'; and because a
waiter never outlives the stream, the pending ones get the closing
failure."
  (umbriel--forget-process)
  (unless (eq (umbriel-stream-status) 'never)
    (umbriel--set-status 'never)))

;;;; Refresh

(defun umbriel--refresh-families (families callback)
  "Fetch each of FAMILIES in turn, writing each reply through the setter.
CALLBACK receives the first failure, or `(:ok nil)' after the last one."
  (if (null families)
      (funcall callback (list :ok nil))
    (umbriel-send
     (umbriel-query (symbol-name (car families)))
     (lambda (result)
       (if (eq (car result) :failure)
           (funcall callback result)
         (umbriel--set-family (car families) (cadr result))
         (umbriel--refresh-families (cdr families) callback))))))

(defun umbriel-refresh (&optional callback)
  "Fetch the windows and workspaces snapshots, one query after the other.
Each `(:ok DATA)' reply is written through `umbriel--set-family', so
the hook sees it exactly as it sees a stream event.  The first failure
stops the pair (a family already fetched stays written) and reaches
CALLBACK, or is echoed when CALLBACK is nil.  The stream status is left
alone: only the stream speaks for the stream."
  (umbriel--refresh-families '(windows workspaces)
                                (or callback #'umbriel-echo-failure)))

;;;; Reporting failures

(defconst umbriel--remedies
  '((no-path . "set UMBRIEL_SOCKET")
    (absent . "is umbriel running in this session?")
    (refused . "restart umbriel")
    (closed . "retry; g in *umbriel* reconnects")
    (rejected . "the compositor refused the request")
    (malformed . "unexpected reply; report upstream"))
  "What to do about each failure kind.")

(defun umbriel-failure-message (failure)
  "One sentence describing FAILURE, beginning with its kind."
  (pcase-let ((`(:failure ,kind ,detail) failure))
    (format "umbriel: %s — %s (%s)" kind detail (alist-get kind umbriel--remedies))))

(defun umbriel-echo-failure (result)
  "Echo RESULT when it is a failure; a success is silent."
  (when (eq (car result) :failure)
    (message "%s" (umbriel-failure-message result))))

(provide 'umbriel-ipc)
;;; umbriel-ipc.el ends here
