;;; umbriel.el --- Client for the umbriel Wayland compositor -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Lee

;; Author: David Lee <dav@davlee.com>
;; Maintainer: David Lee <dav@davlee.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1") (magit-section "4.0"))
;; Keywords: comm, tools
;; URL: https://github.com/davidlee/umbriel.el

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

;; Talk to umbriel, a Wayland compositor, over its JSON socket.
;;
;; `M-x umbriel' shows workspaces, scratchpads and windows in the
;; *umbriel* buffer, kept current by the compositor's event stream; keys
;; on a row act on it (see `umbriel-buffer-mode').
;; `umbriel-focus-mode' focuses the frame an emacsclient reuses, which
;; Wayland otherwise leaves behind.
;;
;; The package binds no global keys.  This file only loads the parts:
;;
;;   umbriel-ipc      the socket client and held state
;;   umbriel-model    pure model of a snapshot
;;   umbriel-columns  pure column layout
;;   umbriel-buffer   the *umbriel* buffer and its commands
;;   umbriel-focus    `umbriel-focus-mode'

;;; Code:

(require 'umbriel-buffer)
(require 'umbriel-focus)

(provide 'umbriel)
;;; umbriel.el ends here
