;;; elark-transient.el --- Transient dispatch menus for elark -*- lexical-binding: t; -*-

;; Copyright (C) 2026 bbw9n

;; Author: bbw9n <bbw9nio@gmail.com>
;; Assisted-by: Claude:claude-opus-5
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Provides the top-level transient dispatch menu and shared transient
;; infrastructure for all elark domain modules.

;;; Code:

(require 'transient)
(require 'elark-auth)

;; Autoloads for domain modules
(autoload 'elark-calendar-dispatch "elark-calendar" nil t)
(autoload 'elark-tasks-dispatch "elark-tasks" nil t)
(autoload 'elark-im-dispatch "elark-im" nil t)
(autoload 'elark-mail-dispatch "elark-mail" nil t)
(autoload 'elark-contacts-dispatch "elark-contact" nil t)
(autoload 'elark-docs-dispatch "elark-docs" nil t)
(autoload 'elark-drive-dispatch "elark-drive" nil t)
(autoload 'elark-sheets-dispatch "elark-sheets" nil t)
(autoload 'elark-meetings-dispatch "elark-meetings" nil t)
(autoload 'elark-whiteboard-dispatch "elark-whiteboard" nil t)
(autoload 'elark-wiki-dispatch "elark-wiki" nil t)

;; Autoloads for AI layer
(autoload 'elark-ai-ask "elark-ai" nil t)
(autoload 'elark-ai-act "elark-ai" nil t)
(autoload 'elark-ai-workflow-standup "elark-ai" nil t)
(autoload 'elark-ai-workflow-meeting-summary "elark-ai" nil t)
(autoload 'elark-ai-workflow-schedule "elark-ai" nil t)
(autoload 'elark-ai-workflow-reply "elark-ai" nil t)

;;;; Top-level dispatch

;;;###autoload (autoload 'elark-dispatch "elark-transient" nil t)
(transient-define-prefix elark-dispatch ()
  "Lark/Feishu command dispatch."
  ["Navigation"
   ("c" "Calendar"   elark-calendar-dispatch)
   ("m" "Messenger"  elark-im-dispatch)
   ("d" "Docs"       elark-docs-dispatch)
   ("f" "Drive"      elark-drive-dispatch)
   ("b" "Base"       elark-base-dispatch)
   ("s" "Sheets"     elark-sheets-dispatch)
   ("t" "Tasks"      elark-tasks-dispatch)
   ("e" "Mail"       elark-mail-dispatch)
   ("w" "Wiki"       elark-wiki-dispatch)
   ("k" "Contacts"   elark-contacts-dispatch)
   ("v" "Meetings"   elark-meetings-dispatch)
   ("W" "Whiteboard" elark-whiteboard-dispatch)]
  ["AI"
   ("i" "Ask AI"       elark-ai-ask)
   ("." "AI act"       elark-ai-act)]
  ["AI Workflows"
   ("1" "Standup report"    elark-ai-workflow-standup)
   ("2" "Meeting summary"   elark-ai-workflow-meeting-summary)
   ("3" "Schedule meeting"  elark-ai-workflow-schedule)
   ("4" "Smart reply"       elark-ai-workflow-reply)]
  ["Auth & Config"
   ("a" "Auth status"  elark-auth-status)
   ("L" "Login"        elark-auth-login)
   ("O" "Logout"       elark-auth-logout)
   ("X" "Identity"     elark-auth-switch-identity)
   ("S" "Schema"       elark-schema-lookup)]
  ["Options"
   ("-d" "Dry run"  "--dry-run")
   ("-f" "Format"   "--format=" :choices ("json" "table" "csv" "pretty"))])

;;;; Schema lookup

;;;###autoload
(defun elark-schema-lookup (method)
  "Look up the schema for a lark-cli METHOD."
  (interactive "sMethod (e.g., calendar.events.get): ")
  (elark--run-command
   (list "schema" method)
   (lambda (result)
     (let ((buf (get-buffer-create (format "*Lark Schema: %s*" method))))
       (with-current-buffer buf
         (let ((inhibit-read-only t))
           (erase-buffer)
           (insert (format "Schema: %s\n" method)
                   (make-string 60 ?─)
                   "\n\n"
                   (pp-to-string result)))
         (special-mode)
         (goto-char (point-min)))
       (pop-to-buffer buf)))
   nil
   :format "json"))

;;;; Placeholder dispatchers for domains not yet implemented
;; These will be replaced as each domain module is built.

(defmacro elark--define-stub-dispatch (name domain)
  "Define a stub transient dispatch for DOMAIN with NAME."
  `(progn
     ;;;###autoload (autoload ',name ,(format "elark-transient") nil t)
     (transient-define-prefix ,name ()
       ,(format "Lark %s commands (not yet implemented)." domain)
       [,(format "%s (coming soon)" domain)
        ("q" "Quit" transient-quit-all)])))

;; elark-docs-dispatch is now provided by elark-docs.el
;; elark-drive-dispatch is now provided by elark-drive.el
(elark--define-stub-dispatch elark-base-dispatch "Base")
;; elark-sheets-dispatch is now provided by elark-sheets.el
;; elark-wiki-dispatch is now provided by elark-wiki.el
;; elark-contacts-dispatch is now provided by elark-contact.el
;; elark-meetings-dispatch is now provided by elark-meetings.el

(provide 'elark-transient)
;;; elark-transient.el ends here
