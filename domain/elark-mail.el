;;; elark-mail.el --- Lark Mail integration -*- lexical-binding: t; -*-

;; Copyright (C) 2026 bbw9n

;; Author: bbw9n <bbw9nio@gmail.com>
;; Assisted-by: Claude:claude-opus-5
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Provides Mail domain commands for elark: inbox listing, mail
;; reading, compose/send, reply, forward, draft management, and
;; search.
;;
;; CLI command mapping:
;;   Inbox/search: mail +triage [--query X] [--filter JSON] [--max N]
;;   Read message: mail +message --message-id X
;;   Send:         mail +send --to X --subject X --body X [--confirm-send]  (NO --format)
;;   Reply:        mail +reply --message-id X --body X [--confirm-send]     (NO --format)
;;   Forward:      mail +forward --message-id X --to X [--body X]           (NO --format)
;;   Drafts:       mail +draft-create --to X --subject X --body X
;;   List drafts:  mail user_mailbox.drafts list

;;; Code:

(require 'elark-core)
(require 'elark-ui)
(require 'json)
(require 'transient)
(require 'shr)

;;;; Customization

(defgroup elark-mail nil
  "Lark Mail settings."
  :group 'elark
  :prefix "elark-mail-")

(defcustom elark-mail-page-size 20
  "Number of emails to fetch per page."
  :type 'integer
  :group 'elark-mail)

;;;; Buffer-local variables

(defvar-local elark-mail--items nil
  "Cached mail items for the current buffer.")

(defvar-local elark-mail--folder nil
  "Current folder/label for the mail buffer.")

(defvar-local elark-mail--mail-id nil
  "Mail ID for the current detail buffer.")

;;;; Mail parsing

(defun elark-mail--mail-id (mail)
  "Extract the mail ID from MAIL."
  (or (alist-get 'mail_id mail)
      (alist-get 'message_id mail)
      (alist-get 'id mail)))

(defun elark-mail--mail-subject (mail)
  "Extract the subject from MAIL."
  (or (alist-get 'subject mail)
      (alist-get 'title mail)
      "(no subject)"))

(defun elark-mail--mail-from (mail)
  "Extract the sender from MAIL."
  (let ((from (or (alist-get 'head_from mail)
                  (alist-get 'from mail)
                  (alist-get 'sender mail))))
    (cond
     ((stringp from) from)
     ((listp from)
      (let ((name (or (alist-get 'name from) ""))
            (addr (or (alist-get 'mail_address from)
                      (alist-get 'address from) "")))
        (cond
         ((and (not (string-empty-p name)) (not (string-empty-p addr)))
          (format "%s <%s>" name addr))
         ((not (string-empty-p name)) name)
         ((not (string-empty-p addr)) addr)
         (t ""))))
     (t ""))))

(defun elark-mail--mail-date (mail)
  "Extract the date from MAIL."
  (let ((date (or (alist-get 'date_formatted mail)
                  (alist-get 'date mail)
                  (alist-get 'send_time mail)
                  (alist-get 'internal_date mail)
                  (alist-get 'timestamp mail))))
    (cond
     ((numberp date) (or (elark--format-timestamp date) ""))
     ((stringp date) (substring date 0 (min 16 (length date))))
     (t ""))))

(defun elark-mail--mail-read-p (mail)
  "Return whether MAIL has been read."
  (let ((read (or (alist-get 'is_read mail)
                  (alist-get 'read mail))))
    (and read (not (eq read :false)) (not (equal read 0)))))

(defun elark-mail--mail-has-attachment-p (mail)
  "Return whether MAIL has attachments."
  (let ((att (or (alist-get 'has_attachment mail)
                 (alist-get 'attachments mail))))
    (cond
     ((eq att t) t)
     ((and (listp att) att) t)
     (t nil))))

(defun elark-mail--extract-mails (data)
  "Extract mail list from lark-cli response DATA.
Null-safe: empty collections arrive as JSON null (`:null')."
  (cond
   ((elark--list-field data 'items))
   ((elark--list-field data 'mails))
   ((elark--list-field data 'messages))
   ((elark--list-field data 'data)
    (let ((inner (elark--list-field data 'data)))
      (or (elark--list-field inner 'items)
          (elark--list-field inner 'mails)
          (elark--list-field inner 'messages)
          (and (elark--record-list-p inner) inner))))
   ((and (elark--record-list-p data)
         (or (alist-get 'mail_id (car data))
             (alist-get 'message_id (car data))
             (alist-get 'subject (car data))))
    data)
   (t nil)))

;;;; Mail list mode (mu4e-like)

(defvar elark-mail-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'elark-mail-read)
    (define-key map (kbd "g")   #'elark-mail-refresh)
    (define-key map (kbd "c")   #'elark-mail-compose)
    (define-key map (kbd "r")   #'elark-mail-reply)
    (define-key map (kbd "f")   #'elark-mail-forward)
    (define-key map (kbd "d")   #'elark-mail-delete)
    (define-key map (kbd "/")   #'elark-mail-search)
    (define-key map (kbd "y")   #'elark-mail-copy-id)
    (define-key map (kbd "n")   #'elark-mail--next)
    (define-key map (kbd "p")   #'elark-mail--prev)
    (define-key map (kbd "?")   #'elark-mail-dispatch)
    map)
  "Keymap for `elark-mail-mode'.")

(define-derived-mode elark-mail-mode special-mode
  "Lark Mail"
  "Major mode for browsing Lark mail, styled after mu4e.

\\{elark-mail-mode-map}")

;;;; AI context provider

(defun elark-mail--ai-context ()
  "Return the AI context plist for a mail buffer."
  (let ((items elark-mail--items)
        (mail-id elark-mail--mail-id)
        (folder elark-mail--folder))
    (list :domain "mail"
          :buffer-type (if mail-id "mail-detail" "inbox")
          :item (when mail-id (list :mail-id mail-id))
          :summary (format "Mail %s%s"
                           (or folder "inbox")
                           (if mail-id
                               (format ", viewing mail %s" mail-id)
                             (format " with %d items" (length items)))))))

(put 'elark-mail-mode 'elark-ai-context-provider #'elark-mail--ai-context)

(defun elark-mail--id-at-point ()
  "Return the mail ID at point, or nil."
  (get-text-property (point) 'elark-mail-id))

(defun elark-mail--next ()
  "Move to the next mail entry."
  (interactive)
  (let ((current (elark-mail--id-at-point))
        (pos (point)))
    (when current
      (while (and (not (eobp))
                  (equal (elark-mail--id-at-point) current))
        (forward-char)))
    (while (and (not (eobp))
                (not (elark-mail--id-at-point)))
      (forward-char))
    (when (eobp) (goto-char pos))
    (beginning-of-line)))

(defun elark-mail--prev ()
  "Move to the previous mail entry."
  (interactive)
  (let ((current (elark-mail--id-at-point))
        (pos (point)))
    (when current
      (while (and (not (bobp))
                  (equal (elark-mail--id-at-point) current))
        (backward-char)))
    (while (and (not (bobp))
                (not (elark-mail--id-at-point)))
      (backward-char))
    (if (elark-mail--id-at-point)
        (beginning-of-line)
      (goto-char pos))))

(defun elark-mail--insert-entry (mail)
  "Insert a single-line mu4e-style entry for MAIL."
  (let* ((id (elark-mail--mail-id mail))
         (unread (not (elark-mail--mail-read-p mail)))
         (flags (concat (if unread "N" " ")
                        (if (elark-mail--mail-has-attachment-p mail) "a" " ")))
         (date (elark-mail--mail-date mail))
         (from (elark-mail--mail-from mail))
         (subject (elark-mail--mail-subject mail))
         (face (if unread 'bold 'default))
         (beg (point)))
    (insert (propertize flags 'face 'font-lock-type-face)
            "  "
            (propertize (format "%-16s" date) 'face 'font-lock-comment-face)
            "  "
            (propertize (format "%-40s"
                                (truncate-string-to-width from 40 nil nil t))
                        'face face)
            "  "
            (propertize subject 'face face)
            "\n")
    (put-text-property beg (point) 'elark-mail-id id)))

;;;; Inbox
;; CLI: mail +triage [--query X] [--max N] [--format json]

;;;###autoload
(defun elark-mail-inbox ()
  "Show the Lark mail inbox."
  (interactive)
  (message "Lark: fetching inbox...")
  (elark--run-command
   (list "mail" "+triage"
         "--max" (number-to-string elark-mail-page-size))
   (lambda (data) (elark-mail--display-list data "Inbox"))
   nil
   :format "json"))

(defun elark-mail-refresh ()
  "Refresh the current mail list buffer."
  (interactive)
  (elark-mail-inbox))

(defun elark-mail--display-list (data folder)
  "Display mail list DATA for FOLDER in mu4e-like format."
  (let* ((mails (elark-mail--extract-mails data))
         (buf (get-buffer-create (format "*Lark Mail: %s*" folder))))
    (with-current-buffer buf
      (elark-mail-mode)
      (setq elark-mail--items mails
            elark-mail--folder folder)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (if (null mails)
            (insert "  (no messages)\n")
          (dolist (mail mails)
            (elark-mail--insert-entry mail))))
      (goto-char (point-min))
      (setq header-line-format
            (format " Lark Mail: %s — %d message(s)  [N]=new [a]=attach"
                    folder (length mails))))
    (pop-to-buffer buf)))

;;;; Read mail
;; CLI: mail +message --message-id X


(defun elark-mail-read ()
  "Read the mail at point."
  (interactive)
  (let ((id (elark-mail--id-at-point)))
    (unless id (user-error "No mail at point"))
    (message "Lark: fetching mail...")
    (elark--run-command
     (list "mail" "+message" "--message-id" id)
     (lambda (data)
       (elark-mail--display-detail data id)))))

(defun elark-mail--header-field (label value)
  "Insert a mail header LABEL: VALUE line if VALUE is non-empty."
  (elark-ui-insert-field label value 10 ""))

(defun elark-mail--format-address (addr)
  "Format a single address ADDR (string or alist) for display."
  (cond
   ((stringp addr) addr)
   ((listp addr)
    (let ((name (or (alist-get 'name addr) ""))
          (address (or (alist-get 'address addr)
                       (alist-get 'mail_address addr) "")))
      (cond
       ((and (not (string-empty-p name)) (not (string-empty-p address)))
        (format "%s <%s>" name address))
       ((not (string-empty-p name)) name)
       ((not (string-empty-p address)) address)
       (t ""))))
   (t (format "%s" addr))))

(defun elark-mail--format-address-list (addresses)
  "Format ADDRESSES (string, alist, or list) for display."
  (cond
   ((null addresses) "")
   ((stringp addresses) addresses)
   ;; Single address alist (not a list of addresses)
   ((and (listp addresses)
         (or (alist-get 'address addresses)
             (alist-get 'mail_address addresses)
             (alist-get 'name addresses)))
    (elark-mail--format-address addresses))
   ((listp addresses)
    (mapconcat #'elark-mail--format-address addresses ", "))
   (t (format "%s" addresses))))

(defun elark-mail--extract-body (mail)
  "Extract the best body from MAIL as (TYPE . CONTENT).
TYPE is `plain' or `html'.  Prefers HTML (rendered via shr); falls back
to plain text."
  (let ((html (alist-get 'body_html mail))
        (plain (alist-get 'body_plain_text mail))
        (body (or (alist-get 'body mail)
                  (alist-get 'text_body mail)
                  (alist-get 'content mail)
                  (alist-get 'plain_text mail)))
        (preview (alist-get 'body_preview mail)))
    (cond
     ((and (stringp html) (not (string-empty-p html)))
      (cons 'html html))
     ((and (stringp plain) (not (string-empty-p plain)))
      (cons 'plain plain))
     ((and (stringp body) (not (string-empty-p body)))
      (cons 'plain body))
     ((and (listp body)
           (let ((text (or (alist-get 'plain_text body)
                           (alist-get 'text body)
                           (alist-get 'content body))))
             (and (stringp text) (not (string-empty-p text))
                  (cons 'plain text)))))
     ((and (stringp preview) (not (string-empty-p preview)))
      (cons 'plain preview))
     (t (cons 'plain "")))))

(defun elark-mail--display-detail (data mail-id)
  "Display mail detail DATA for MAIL-ID."
  (let* ((mail (or (alist-get 'data data) data))
         (subject (elark-mail--mail-subject mail))
         (buf (get-buffer-create (format "*Lark Mail: %s*" subject))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (setq elark-mail--mail-id mail-id)
        ;; Subject line
        (elark-ui-insert-title subject 1 72)
        ;; Headers
        (elark-mail--header-field "From"
          (elark-mail--format-address-list (or (alist-get 'head_from mail)
                                              (alist-get 'from mail)
                                              (alist-get 'sender mail))))
        (elark-mail--header-field "To"
          (elark-mail--format-address-list (alist-get 'to mail)))
        (elark-mail--header-field "Cc"
          (elark-mail--format-address-list (alist-get 'cc mail)))
        (elark-mail--header-field "Bcc"
          (elark-mail--format-address-list (alist-get 'bcc mail)))
        (elark-mail--header-field "Reply-To"
          (elark-mail--format-address-list (alist-get 'reply_to mail)))
        (elark-mail--header-field "Date" (elark-mail--mail-date mail))
        (elark-mail--header-field "Mail-ID" (or mail-id ""))
        ;; Folder / labels / state
        (elark-mail--header-field "Folder"
          (or (alist-get 'folder_id mail) ""))
        (let ((labels (or (alist-get 'labels mail)
                          (alist-get 'label_ids mail))))
          (when labels
            (elark-mail--header-field "Labels"
              (cond
               ((stringp labels) labels)
               ((listp labels) (mapconcat (lambda (l) (format "%s" l)) labels ", "))
               (t "")))))
        (elark-mail--header-field "State"
          (or (alist-get 'message_state_text mail) ""))
        (elark-mail--header-field "Thread"
          (or (alist-get 'thread_id mail) ""))
        ;; Attachments
        (let ((attachments (or (alist-get 'attachments mail)
                               (alist-get 'files mail))))
          (when (and (listp attachments) attachments)
            (insert "\n" (propertize "Attachments" 'face 'bold) "\n")
            (dolist (att attachments)
              (let* ((name (or (alist-get 'file_name att)
                               (alist-get 'name att)
                               (alist-get 'filename att) "unknown"))
                     (size (or (alist-get 'size att)
                               (alist-get 'file_size att)))
                     (mime (or (alist-get 'mime_type att)
                               (alist-get 'content_type att) "")))
                (insert "  " (propertize name 'face 'link))
                (when size
                  (insert (propertize (format "  (%s)" (elark-mail--format-size size))
                                      'face 'font-lock-comment-face)))
                (when (and mime (not (string-empty-p mime)))
                  (insert (propertize (format "  [%s]" mime)
                                      'face 'font-lock-comment-face)))
                (insert "\n")))))
        ;; Body
        (insert "\n" (elark-ui-separator 72) "\n\n")
        (let* ((body-pair (elark-mail--extract-body mail))
               (body-type (car body-pair))
               (body-content (cdr body-pair)))
          (cond
           ((string-empty-p body-content)
            (insert (propertize "(no body)" 'face 'font-lock-comment-face) "\n"))
           ((eq body-type 'html)
            (let ((shr-use-fonts nil)
                  (shr-width (min 72 (window-width))))
              (shr-insert-document
               (with-temp-buffer
                 (insert body-content)
                 (libxml-parse-html-region (point-min) (point-max))))))
           (t
            (insert body-content "\n")))))
      (special-mode)
      (visual-line-mode 1)
      (goto-char (point-min)))
    (pop-to-buffer buf)))

(defun elark-mail--format-size (size)
  "Format byte SIZE as a human-readable string."
  (cond
   ((not (numberp size)) (format "%s" size))
   ((< size 1024) (format "%d B" size))
   ((< size (* 1024 1024)) (format "%.1f KB" (/ size 1024.0)))
   (t (format "%.1f MB" (/ size (* 1024.0 1024.0))))))

(defun elark-mail--format-recipients (recipients)
  "Format RECIPIENTS list to a display string."
  (cond
   ((null recipients) "")
   ((stringp recipients) recipients)
   ((listp recipients)
    (mapconcat
     (lambda (r)
       (if (stringp r) r
         (or (alist-get 'name r)
             (alist-get 'address r)
             (format "%s" r))))
     recipients ", "))
   (t (format "%s" recipients))))

;;;; Compose buffer — a dedicated panel, magit-commit style.
;; CLI: mail +send --to X --subject X --body X [--cc X] [--confirm-send]
;;      mail +reply --message-id X --body X [--confirm-send]
;;      mail +forward --message-id X --to X [--body X] [--confirm-send]
;;
;; New mail, reply and forward all open *Lark Mail Compose* in a
;; window below the current one.  Editable headers sit above a
;; separator line (message-mode convention), the body below it.
;; C-c C-c sends, C-c C-d saves a draft (new mail), C-c C-k aborts;
;; the prior window configuration is restored afterwards.

(defconst elark-mail-compose--buffer-name "*Lark Mail Compose*")

(defconst elark-mail-compose--separator "--text follows this line--"
  "Line separating editable headers from the mail body.")

(defvar-local elark-mail-compose--kind nil
  "What this compose buffer produces: `new', `reply' or `forward'.")

(defvar-local elark-mail-compose--message-id nil
  "Message id being replied to / forwarded, when applicable.")

(defvar-local elark-mail-compose--window-config nil
  "Window configuration to restore after send/abort.")

(defvar elark-mail-compose-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'elark-mail-compose-send)
    (define-key map (kbd "C-c C-d") #'elark-mail-compose-save-draft)
    (define-key map (kbd "C-c C-k") #'elark-mail-compose-abort)
    map)
  "Keymap for `elark-mail-compose-mode'.")

(define-derived-mode elark-mail-compose-mode text-mode "Lark Compose"
  "Major mode for composing Lark mail in a dedicated buffer.

\\{elark-mail-compose-mode-map}")

(defun elark-mail-compose--open (kind message-id headers banner)
  "Open the compose panel for KIND (`new'/`reply'/`forward').
MESSAGE-ID is the message acted on (nil for new mail), HEADERS an
alist of editable (NAME . INITIAL-VALUE) header lines, BANNER the
header-line description."
  (let ((config (current-window-configuration))
        (buf (get-buffer-create elark-mail-compose--buffer-name)))
    (when (and (buffer-modified-p buf)
               (not (y-or-n-p "Discard the unsent mail being composed? ")))
      (user-error "Kept the existing compose buffer"))
    (with-current-buffer buf
      (let ((inhibit-read-only t)) (erase-buffer))
      (elark-mail-compose-mode)
      (setq elark-mail-compose--kind kind
            elark-mail-compose--message-id message-id
            elark-mail-compose--window-config config)
      (setq header-line-format
            (format " %s — C-c C-c send%s · C-c C-k abort"
                    banner
                    (if (eq kind 'new) " · C-c C-d save draft" "")))
      (dolist (h headers)
        (insert (propertize (concat (car h) ":")
                            'face 'font-lock-keyword-face)
                " " (or (cdr h) "") "\n"))
      (when headers
        (insert (propertize elark-mail-compose--separator
                            'face 'font-lock-comment-face)
                "\n"))
      ;; Point: end of the first header's value, or the body.
      (goto-char (point-min))
      (when headers (end-of-line))
      (set-buffer-modified-p nil))
    (pop-to-buffer buf '((display-buffer-below-selected)
                         (window-height . 0.4)))
    buf))

(defun elark-mail-compose--parse ()
  "Parse the compose buffer into (:headers ALIST :body STRING).
Header names are downcased; the body is everything after the
separator (or the whole buffer when there are no headers)."
  (let* ((text (buffer-substring-no-properties (point-min) (point-max)))
         (sep-re (concat "^" (regexp-quote elark-mail-compose--separator) "\n?"))
         (head (when (string-match sep-re text)
                 (substring text 0 (match-beginning 0))))
         (body (if head (substring text (match-end 0)) text))
         (headers
          (when head
            (delq nil
                  (mapcar (lambda (line)
                            (when (string-match
                                   "^\\([A-Za-z-]+\\):[ \t]*\\(.*\\)$" line)
                              (cons (downcase (match-string 1 line))
                                    (string-trim (match-string 2 line)))))
                          (split-string head "\n" t))))))
    (list :headers headers :body (string-trim body))))

(defun elark-mail-compose--header (headers name)
  "Return non-empty header NAME from HEADERS, or nil."
  (let ((v (alist-get name headers nil nil #'equal)))
    (and v (not (string-empty-p v)) v)))

(defun elark-mail-compose--finish (args success-msg)
  "Fire the CLI call for ARGS, close the panel, report SUCCESS-MSG."
  (let ((buf (current-buffer))
        (config elark-mail-compose--window-config))
    (message "Lark: sending...")
    (elark--run-command args (lambda (_data) (message "Lark: %s" success-msg)))
    (set-buffer-modified-p nil)
    (kill-buffer buf)
    (when config (set-window-configuration config))))

(defun elark-mail-compose-send ()
  "Send the mail being composed (C-c C-c)."
  (interactive)
  (let* ((parsed (elark-mail-compose--parse))
         (headers (plist-get parsed :headers))
         (body (plist-get parsed :body)))
    (pcase elark-mail-compose--kind
      ('reply
       (when (string-empty-p body) (user-error "Empty reply"))
       (when (y-or-n-p "Send this reply? ")
         (elark-mail-compose--finish
          (list "mail" "+reply" "--message-id" elark-mail-compose--message-id
                "--body" body "--confirm-send")
          "reply sent")))
      ('forward
       (let ((to (or (elark-mail-compose--header headers "to")
                     (user-error "Forward needs a To: address"))))
         (when (y-or-n-p (format "Forward to %s? " to))
           (elark-mail-compose--finish
            (append (list "mail" "+forward"
                          "--message-id" elark-mail-compose--message-id
                          "--to" to "--confirm-send")
                    (unless (string-empty-p body) (list "--body" body)))
            "mail forwarded"))))
      ('new
       (let ((to (or (elark-mail-compose--header headers "to")
                     (user-error "Mail needs a To: address")))
             (subject (or (elark-mail-compose--header headers "subject")
                          (user-error "Mail needs a Subject:")))
             (cc (elark-mail-compose--header headers "cc")))
         (when (string-empty-p body) (user-error "Empty body"))
         (when (y-or-n-p (format "Send to %s? " to))
           (elark-mail-compose--finish
            (append (list "mail" "+send" "--to" to "--subject" subject
                          "--body" body "--confirm-send")
                    (when cc (list "--cc" cc)))
            "mail sent"))))
      (_ (user-error "Not a Lark compose buffer")))))

(defun elark-mail-compose-save-draft ()
  "Save the mail being composed as a draft (new mail only)."
  (interactive)
  (unless (eq elark-mail-compose--kind 'new)
    (user-error "Drafts are only supported for new mail"))
  (let* ((parsed (elark-mail-compose--parse))
         (headers (plist-get parsed :headers))
         (body (plist-get parsed :body))
         (to (elark-mail-compose--header headers "to"))
         (subject (elark-mail-compose--header headers "subject")))
    (elark-mail-compose--finish
     (append '("mail" "+draft-create")
             (when to (list "--to" to))
             (when subject (list "--subject" subject))
             (unless (string-empty-p body) (list "--body" body)))
     "draft saved")))

(defun elark-mail-compose-abort ()
  "Abort composing: kill the panel and restore the windows (C-c C-k)."
  (interactive)
  (when (or (not (buffer-modified-p))
            (y-or-n-p "Discard this unsent mail? "))
    (let ((config elark-mail-compose--window-config))
      (set-buffer-modified-p nil)
      (kill-buffer)
      (when config (set-window-configuration config)))))

;;;; Compose / Reply / Forward entry points

;;;###autoload
(defun elark-mail-compose ()
  "Compose a new Lark mail in a dedicated panel."
  (interactive)
  (elark-mail-compose--open
   'new nil '(("To" . "") ("Cc" . "") ("Subject" . "")) "New mail"))

(defun elark-mail-reply ()
  "Reply to the mail at point in a dedicated compose panel."
  (interactive)
  (let ((id (or (elark-mail--id-at-point) elark-mail--mail-id)))
    (unless id (user-error "No mail selected"))
    (elark-mail-compose--open 'reply id nil "Reply")))

(defun elark-mail-forward ()
  "Forward the mail at point via a dedicated compose panel.
The body, if any, is sent as the forward note."
  (interactive)
  (let ((id (or (elark-mail--id-at-point) elark-mail--mail-id)))
    (unless id (user-error "No mail selected"))
    (elark-mail-compose--open 'forward id '(("To" . "")) "Forward")))

;;;; Delete
;; CLI: mail user_mailbox.messages delete --params '{"user_mailbox_id":"me","message_id":"X"}'


(defun elark-mail-delete ()
  "Delete the mail at point."
  (interactive)
  (let ((id (elark-mail--id-at-point)))
    (unless id (user-error "No mail at point"))
    (when (yes-or-no-p (format "Delete mail %s? " id))
      (message "Lark: deleting mail...")
      (let ((params (json-encode `((user_mailbox_id . "me")
                                   (message_id . ,id)))))
        (elark--run-command
         (list "mail" "user_mailbox.messages" "delete" "--params" params)
         (lambda (_data)
           (message "Lark: mail deleted")
           (elark-mail-refresh)))))))

;;;; Search
;; CLI: mail +triage --query X [--max N] [--format json]


;;;###autoload
(defun elark-mail-search (query)
  "Search Lark mail for QUERY."
  (interactive "sSearch mail: ")
  (message "Lark: searching mail...")
  (elark--run-command
   (list "mail" "+triage" "--query" query
         "--max" (number-to-string elark-mail-page-size))
   (lambda (data)
     (elark-mail--display-list data (format "Search: %s" query)))
   nil
   :format "json"))

;;;; Copy mail ID

(defun elark-mail-copy-id ()
  "Copy the mail ID at point to the kill ring."
  (interactive)
  (let ((id (or (elark-mail--id-at-point) elark-mail--mail-id)))
    (unless id (user-error "No mail ID"))
    (kill-new id)
    (message "Copied: %s" id)))

;;;; Drafts
;; CLI: mail user_mailbox.drafts list


;;;###autoload
(defun elark-mail-drafts ()
  "List Lark mail drafts."
  (interactive)
  (message "Lark: fetching drafts...")
  (elark--run-command
   '("mail" "user_mailbox.drafts" "list")
   (lambda (data)
     (elark-mail--display-list data "Drafts"))))

;;;; Transient dispatch

;;;###autoload (autoload 'elark-mail-dispatch "elark-mail" nil t)
(transient-define-prefix elark-mail-dispatch ()
  "Lark Mail commands."
  ["Browse"
   ("i" "Inbox"       elark-mail-inbox)
   ("d" "Drafts"      elark-mail-drafts)
   ("/" "Search"      elark-mail-search)]
  ["Compose"
   ("c" "Compose"     elark-mail-compose)
   ("r" "Reply"       elark-mail-reply)
   ("f" "Forward"     elark-mail-forward)]
  ["At Point"
   ("RET" "Read"      elark-mail-read)
   ("x"   "Delete"    elark-mail-delete)])

(provide 'elark-mail)
;;; elark-mail.el ends here
