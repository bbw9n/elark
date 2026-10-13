;;; elark.el --- Emacs interface to Lark/Feishu via lark-cli -*- lexical-binding: t; -*-

;; Copyright (C) 2026 bbw9n

;; Author: bbw9n <bbw9nio@gmail.com>
;; Assisted-by: Claude:claude-opus-5
;; SPDX-License-Identifier: GPL-3.0-or-later
;; URL: https://github.com/bbw9n/elark
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (org-lark "0.2") (shell-maker "0.97.5"))
;; Keywords: comm, tools, agent

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; elark provides an Emacs interface to the Lark/Feishu platform by
;; wrapping the lark-cli command-line tool.  It covers 11 business
;; domains including Calendar, Messenger, Docs, Drive, Base, Sheets,
;; Tasks, Mail, Contacts, Wiki, and Meetings.
;;
;; The package uses transient menus for command dispatch.
;; All CLI interactions are async by default to keep Emacs responsive.
;;
;; Quick start:
;;   M-x elark-dispatch
;;
;; Key bindings:
;;   elark does not bind any global keys on load.  To get the
;;   `elark-prefix-map' commands under a prefix key, add this to your
;;   init file:
;;
;;     (elark-setup-prefix-key)          ; binds C-c l
;;     (elark-setup-prefix-key "C-c L")  ; or any other prefix
;;
;;   after which `C-c l l' opens the main menu.
;;
;; Prerequisites:
;;   - lark-cli installed and on PATH (https://github.com/larksuite/cli)
;;   - Emacs 29.1+
;;
;; Dependencies:
;;   - org-lark (https://github.com/bbw9n/org-lark) — Org rendering and
;;     export of fetched documents.
;;   - shell-maker (https://github.com/xenodium/shell-maker) — the AI
;;     shell interface, including markdown-overlays rendering.

;;; Code:

;;;; Load path setup for subdirectories

(defconst elark--directory
  (file-name-directory
   (or load-file-name
       (bound-and-true-p byte-compile-current-file)
       buffer-file-name))
  "Directory where elark.el is installed.")

(dolist (sub '("core" "ui" "domain" "ai"))
  (add-to-list 'load-path (expand-file-name sub elark--directory)))

(require 'elark-core)
(require 'elark-ui)
(require 'elark-auth)
(require 'elark-transient)

;;;; Customization

(defgroup elark nil
  "Emacs interface to Lark/Feishu via lark-cli."
  :group 'comm
  :prefix "elark-")

(defcustom elark-cli-executable "lark-cli"
  "Path to the lark-cli executable."
  :type 'string
  :group 'elark)

(defcustom elark-default-format "json"
  "Default output format for lark-cli commands.
One of \"json\", \"table\", \"csv\", \"ndjson\", or \"pretty\"."
  :type '(choice (const "json")
          (const "table")
          (const "csv")
          (const "ndjson")
          (const "pretty"))
  :group 'elark)

(defcustom elark-default-identity "user"
  "Default identity for lark-cli commands.
Either \"user\" or \"bot\"."
  :type '(choice (const "user")
          (const "bot"))
  :group 'elark)

(defcustom elark-dry-run nil
  "When non-nil, pass --dry-run to all destructive lark-cli commands."
  :type 'boolean
  :group 'elark)

;;;; Prefix keymap

(autoload 'elark-calendar-agenda "elark-calendar" nil t)
(autoload 'elark-tasks-list "elark-tasks" nil t)
(autoload 'elark-im-dispatch "elark-im" nil t)
(autoload 'elark-mail-inbox "elark-mail" nil t)
(autoload 'elark-contacts-dispatch "elark-contact" nil t)
(autoload 'elark-docs-dispatch "elark-docs" nil t)
(autoload 'elark-docs-search "elark-docs" nil t)
(autoload 'elark-drive-dispatch "elark-drive" nil t)
(autoload 'elark-drive-list "elark-drive" nil t)
(autoload 'elark-ai-ask "elark-ai" nil t)
(autoload 'elark-ai-shell "elark-ai-shell" nil t)
(autoload 'elark-ai-act "elark-ai" nil t)
(autoload 'elark-ai-brief-on "elark-ai" nil t)

(defvar elark-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "l") #'elark-dispatch)
    (define-key map (kbd "a") #'elark-calendar-agenda)
    (define-key map (kbd "m") #'elark-im-dispatch)
    (define-key map (kbd "t") #'elark-tasks-list)
    (define-key map (kbd "e") #'elark-mail-inbox)
    (define-key map (kbd "d") #'elark-docs-dispatch)
    (define-key map (kbd "f") #'elark-drive-list)
    (define-key map (kbd "k") #'elark-contacts-dispatch)
    (define-key map (kbd "s") #'elark-auth-status)
    (define-key map (kbd "i") #'elark-ai-ask)
    (define-key map (kbd ".") #'elark-ai-act)
    (define-key map (kbd "b") #'elark-ai-brief-on)
    map)
  "Keymap for Lark commands, intended to be bound under a prefix key.
See `elark-setup-prefix-key'.")

;;;###autoload
(defun elark-setup-prefix-key (&optional key)
  "Bind `elark-prefix-map' to KEY in `global-map'.
KEY defaults to `C-c l'."
  (interactive)
  (global-set-key (kbd (or key "C-c l")) elark-prefix-map))

(provide 'elark)
;;; elark.el ends here
