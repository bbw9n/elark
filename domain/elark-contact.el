;;; elark-contact.el --- Lark Contact integration -*- lexical-binding: t; -*-

;; Copyright (C) 2026 bbw9n

;; Author: bbw9n <bbw9nio@gmail.com>
;; Assisted-by: Claude:claude-opus-5
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Provides Contact domain commands for elark: user lookup, user
;; search, and a shared user-name resolution cache used by other
;; modules (IM, Calendar, Tasks, etc.).
;;
;; CLI command mapping:
;;   Get user:    contact +get-user [--user-id X] [--user-id-type Y]
;;   Search user: contact +search-user --query X

;;; Code:

(require 'elark-core)
(require 'elark-ui)
(require 'transient)

;;;; Customization

(defgroup elark-contact nil
  "Lark Contact settings."
  :group 'elark
  :prefix "elark-contact-")

;;;; User cache
;;
;; Resolved names (open_id → display name) are kept in memory AND
;; persisted to disk, so a fresh Emacs session doesn't re-resolve the
;; same people over the network.  Entries carry a timestamp; stale
;; ones (people do get renamed) are dropped at load time and
;; re-resolve transparently on next use.

(defcustom elark-contact-cache-file
  (expand-file-name "elark/contact-names.eld"
                    (or (getenv "XDG_CACHE_HOME") "~/.cache"))
  "File persisting resolved contact names across sessions.
Set to nil to keep names in memory only."
  :type '(choice (const :tag "In-memory only" nil) file)
  :group 'elark-contact)

(defcustom elark-contact-cache-ttl (* 30 24 60 60)
  "Seconds before a persisted contact name is considered stale.
Stale entries are dropped when the cache file is loaded, so the
name re-resolves on next use.  nil = entries never expire."
  :type '(choice (const :tag "Never expires" nil) integer)
  :group 'elark-contact)

(defvar elark-contact--user-cache (make-hash-table :test 'equal)
  "Hash table mapping \"user_id:id_type\" to (NAME . TIMESTAMP).
Populated lazily by resolution and from `elark-contact-cache-file'.")

(defvar elark-contact--cache-loaded nil
  "Non-nil once the persisted cache has been read this session.")

(defvar elark-contact--cache-save-timer nil
  "Pending idle-timer that flushes the cache to disk, or nil.")

(defun elark-contact--cache-key (user-id id-type)
  "Build a cache key from USER-ID and ID-TYPE."
  (format "%s:%s" user-id (or id-type "open_id")))

(defun elark-contact--cache-load ()
  "Populate the in-memory cache from disk, once per session.
Entries older than `elark-contact-cache-ttl' are skipped."
  (unless elark-contact--cache-loaded
    (setq elark-contact--cache-loaded t)
    (when (and elark-contact-cache-file
               (file-readable-p elark-contact-cache-file))
      (condition-case nil
          (let ((now (float-time)))
            (dolist (entry (with-temp-buffer
                             (insert-file-contents elark-contact-cache-file)
                             (read (current-buffer))))
              (pcase entry
                (`(,key ,name . ,time)
                 (when (and (stringp key) (stringp name) (numberp time)
                            (or (null elark-contact-cache-ttl)
                                (< (- now time) elark-contact-cache-ttl))
                            ;; In-session resolutions win over disk.
                            (not (gethash key elark-contact--user-cache)))
                   (puthash key (cons name time)
                            elark-contact--user-cache))))))
        (error nil)))))

(defun elark-contact--cache-get (user-id id-type)
  "Return cached display name for USER-ID / ID-TYPE, or nil."
  (elark-contact--cache-load)
  (let ((v (gethash (elark-contact--cache-key user-id id-type)
                    elark-contact--user-cache)))
    (if (consp v) (car v) v)))

(defun elark-contact--cache-put (user-id id-type name)
  "Store NAME in cache for USER-ID / ID-TYPE and schedule a disk flush."
  (elark-contact--cache-load)
  (puthash (elark-contact--cache-key user-id id-type)
           (cons name (float-time))
           elark-contact--user-cache)
  (elark-contact--cache-schedule-save)
  name)

(defun elark-contact--cache-schedule-save ()
  "Flush the cache to disk on the next idle moment (debounced)."
  (when (and elark-contact-cache-file
             (null elark-contact--cache-save-timer))
    (setq elark-contact--cache-save-timer
          (run-with-idle-timer 2 nil #'elark-contact--cache-save))))

(defun elark-contact--cache-save ()
  "Write the in-memory name cache to `elark-contact-cache-file'."
  (setq elark-contact--cache-save-timer nil)
  (when elark-contact-cache-file
    (condition-case nil
        (let (entries)
          (maphash (lambda (k v)
                     (push (cons k (if (consp v) v (cons v (float-time))))
                           entries))
                   elark-contact--user-cache)
          (make-directory (file-name-directory elark-contact-cache-file) t)
          (with-temp-file elark-contact-cache-file
            (insert ";; elark contact name cache — safe to delete.\n")
            (prin1 entries (current-buffer))
            (insert "\n")))
      (error nil))))

;;;; Core: get-user (sync, used as fundamental utility)

(defun elark-contact--extract-user (data)
  "Extract the user alist from lark-cli response DATA."
  (or (elark--get-nested data 'data 'user)
      (alist-get 'user data)
      (alist-get 'data data)
      data))

(defun elark-contact--user-display-name (user)
  "Extract a display name from USER alist."
  (or (alist-get 'name user)
      (alist-get 'display_name user)
      (alist-get 'en_name user)
      ""))

(defun elark-contact-get-user-sync (user-id &optional id-type)
  "Fetch user info for USER-ID synchronously, return the user alist.
ID-TYPE defaults to \"open_id\".  Also accepts \"user_id\", \"union_id\"."
  (let ((id-type (or id-type "open_id")))
    (elark--run-command-sync
     (list "contact" "+get-user"
           "--user-id" user-id
           "--user-id-type" id-type))))

(defun elark-contact-resolve-name (user-id &optional id-type)
  "Return the display name for USER-ID (sync, cached).
ID-TYPE defaults to \"open_id\".  Returns USER-ID as fallback."
  (let ((id-type (or id-type "open_id")))
    (or (elark-contact--cache-get user-id id-type)
        (condition-case nil
            (let* ((data (elark-contact-get-user-sync user-id id-type))
                   (user (elark-contact--extract-user data))
                   (name (elark-contact--user-display-name user)))
              (when (and name (not (string-empty-p name)))
                (elark-contact--cache-put user-id id-type name)
                name))
          (error nil))
        user-id)))

;;;; Non-blocking name annotation
;;
;; Rendering a listing must never run a synchronous network RPC per
;; user id (that froze Emacs for seconds).  The pattern: insert the
;; cached name, or the raw id tagged with a `elark-contact-ref' text
;; property; after the buffer is displayed, call
;; `elark-contact-resolve-buffer-async' — it looks up each distinct
;; unresolved id once and patches every occurrence in place.

(defun elark-contact-annotate (user-id &optional id-type)
  "Return a display string for USER-ID of ID-TYPE without blocking.
The cached display name when known; otherwise USER-ID itself,
propertized with `elark-contact-ref' so a later
`elark-contact-resolve-buffer-async' pass can patch it in place."
  (let ((id-type (or id-type "open_id")))
    (or (elark-contact--cache-get user-id id-type)
        (propertize user-id 'elark-contact-ref (cons user-id id-type)))))

(defun elark-contact--unresolved-refs (buf)
  "Collect distinct `elark-contact-ref' values still shown as raw ids in BUF."
  (let (refs)
    (with-current-buffer buf
      (save-excursion
        (let ((pos (point-min)))
          (while (setq pos (text-property-not-all
                            pos (point-max) 'elark-contact-ref nil))
            (let ((end (or (next-single-property-change pos 'elark-contact-ref)
                           (point-max)))
                  (ref (get-text-property pos 'elark-contact-ref)))
              (when (equal (buffer-substring-no-properties pos end) (car ref))
                (push ref refs))
              (setq pos end))))))
    (delete-dups (nreverse refs))))

(defun elark-contact-resolve-buffer-async (buf)
  "Resolve user ids displayed raw in BUF and patch them in place.
One async lookup per distinct id; results land in the name cache
so later renders resolve synchronously.  Ids whose lookup returns
no name (restricted profiles, bots) are left as-is."
  (dolist (ref (elark-contact--unresolved-refs buf))
    (let ((ref ref))
      (elark--run-command
       (list "contact" "+get-user"
             "--user-id" (car ref)
             "--user-id-type" (cdr ref))
       (lambda (data)
         (let ((name (elark-contact--user-display-name
                      (elark-contact--extract-user data))))
           (when (and name (not (string-empty-p name)))
             (elark-contact--cache-put (car ref) (cdr ref) name)
             (elark-contact--patch-ref buf ref name))))
       nil :no-error t))))

(defun elark-contact--patch-ref (buf ref name)
  "Replace every raw display of REF in BUF with NAME.
The replaced text's properties are carried over so section-wide
properties (item ids, faces) stay intact."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (save-excursion
        (let ((inhibit-read-only t)
              (pos (point-min)))
          (while (setq pos (text-property-not-all
                            pos (point-max) 'elark-contact-ref nil))
            (let ((end (or (next-single-property-change pos 'elark-contact-ref)
                           (point-max)))
                  (this (get-text-property pos 'elark-contact-ref)))
              (if (and (equal this ref)
                       (equal (buffer-substring-no-properties pos end)
                              (car ref)))
                  (let ((props (text-properties-at pos)))
                    (goto-char pos)
                    (delete-region pos end)
                    (insert (apply #'propertize name props))
                    (setq pos (point)))
                (setq pos end)))))))))

;;;; Async get-user

;;;###autoload
(defun elark-contact-get-user (user-id &optional id-type callback)
  "Fetch user info for USER-ID asynchronously.
ID-TYPE defaults to \"open_id\".
CALLBACK, if non-nil, is called with the user alist.
When called interactively, displays the result in a detail buffer."
  (interactive
   (list (read-string "User ID: ")
         (completing-read "ID type: "
                          '("open_id" "user_id" "union_id") nil t nil nil "open_id")))
  (let ((id-type (or id-type "open_id")))
    (elark--run-command
     (list "contact" "+get-user"
           "--user-id" user-id
           "--user-id-type" id-type)
     (lambda (data)
       (let* ((user (elark-contact--extract-user data))
              (name (elark-contact--user-display-name user)))
         (when (and name (not (string-empty-p name)))
           (elark-contact--cache-put user-id id-type name))
         (if callback
             (funcall callback user)
           (elark-contact--display-user user user-id)))))))

;;;; User search

;;;###autoload
(defun elark-contact-search (query)
  "Search Lark contacts by QUERY."
  (interactive "sSearch contacts: ")
  (when (string-empty-p query)
    (user-error "Search query is required"))
  (message "Lark: searching contacts...")
  (elark--run-command
   (list "contact" "+search-user" "--query" query)
   #'elark-contact--display-search-results))

;;;; Display: user detail

(defun elark-contact--display-user (user user-id)
  "Display USER detail in a buffer.  USER-ID is used for the buffer name."
  (let* ((name (elark-contact--user-display-name user))
         (buf-name (format "*Lark User: %s*" (if (string-empty-p name) user-id name)))
         (buf (get-buffer-create buf-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize (if (string-empty-p name) user-id name) 'face 'bold) "\n"
                (elark-ui-separator 60) "\n\n")
        (elark-contact--insert-field "Name" name)
        (elark-contact--insert-field "EN Name" (or (alist-get 'en_name user) ""))
        (elark-contact--insert-field "Email" (or (alist-get 'email user) ""))
        (elark-contact--insert-field "Mobile" (or (alist-get 'mobile user) ""))
        (elark-contact--insert-field "Open ID" (or (alist-get 'open_id user) ""))
        (elark-contact--insert-field "User ID" (or (alist-get 'user_id user) ""))
        (elark-contact--insert-field "Union ID" (or (alist-get 'union_id user) ""))
        (elark-contact--insert-field "Status"
                                    (let ((s (alist-get 'status user)))
                                      (cond
                                       ((stringp s) s)
                                       ((listp s) (if (eq (alist-get 'is_activated s) t)
                                                      "activated" "inactive"))
                                       (t ""))))
        (elark-contact--insert-field "Department"
                                    (let ((ids (alist-get 'department_ids user)))
                                      (if (and ids (listp ids))
                                          (mapconcat (lambda (x) (format "%s" x)) ids ", ")
                                        ""))))
      (special-mode)
      (goto-char (point-min)))
    (pop-to-buffer buf)))

(defun elark-contact--insert-field (label value)
  "Insert a LABEL: VALUE line if VALUE is non-empty."
  (elark-ui-insert-field label value))

;;;; Display: search results

(defvar elark-contact-search-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'elark-contact-open-at-point)
    (define-key map (kbd "y")   #'elark-contact-copy-id)
    (define-key map (kbd "?")   #'elark-contacts-dispatch)
    map)
  "Keymap for `elark-contact-search-mode'.")

(define-derived-mode elark-contact-search-mode tabulated-list-mode
  "Lark Contacts"
  "Major mode for browsing Lark contact search results."
  (setq tabulated-list-format
        [("Name" 24 t)
         ("EN Name" 20 t)
         ("Email" 30 t)
         ("Open ID" 28 t)])
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header))

(defvar-local elark-contact--users nil
  "Cached user list for the current buffer.")

;;;; AI context provider

(defun elark-contact--ai-context ()
  "Return the AI context plist for a contact search buffer."
  (let ((users elark-contact--users))
    (list :domain "contacts"
          :buffer-type "contact-search"
          :item nil
          :summary (format "Contact search results: %d users"
                           (length users)))))

(put 'elark-contact-search-mode 'elark-ai-context-provider
     #'elark-contact--ai-context)

(defun elark-contact--extract-users (data)
  "Extract user list from lark-cli response DATA."
  (or (elark--get-nested data 'data 'items)
      (elark--get-nested data 'data 'users)
      (alist-get 'items data)
      (alist-get 'users data)
      (and (listp data) (listp (car data))
           (alist-get 'open_id (car data))
           data)))

(defun elark-contact--make-user-entries (users)
  "Convert USERS to `tabulated-list-entries' format."
  (mapcar
   (lambda (user)
     (let ((id (or (alist-get 'open_id user) (alist-get 'user_id user) ""))
           (name (or (alist-get 'name user) (alist-get 'display_name user) ""))
           (en-name (or (alist-get 'en_name user) ""))
           (email (or (alist-get 'email user) "")))
       (list id (vector name en-name email id))))
   users))

(defun elark-contact--display-search-results (data)
  "Display contact search results DATA."
  (let* ((users (elark-contact--extract-users data))
         (buf (get-buffer-create "*Lark Contacts*")))
    (with-current-buffer buf
      (elark-contact-search-mode)
      (setq elark-contact--users users
            tabulated-list-entries (elark-contact--make-user-entries users))
      (tabulated-list-print t)
      (setq header-line-format
            (format " Lark Contacts — %d result(s)" (length users))))
    (pop-to-buffer buf)))

(defun elark-contact-open-at-point ()
  "Open user detail for the entry at point."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (unless id (user-error "No contact at point"))
    (elark-contact-get-user id "open_id")))

(defun elark-contact-copy-id ()
  "Copy the open_id of the contact at point."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (unless id (user-error "No contact at point"))
    (kill-new id)
    (message "Copied: %s" id)))

;;;; Cache management

(defun elark-contact-clear-cache ()
  "Clear the user name cache."
  (interactive)
  (clrhash elark-contact--user-cache)
  (message "Lark: contact cache cleared"))

;;;; Transient dispatch

;;;###autoload (autoload 'elark-contacts-dispatch "elark-contact" nil t)
(transient-define-prefix elark-contacts-dispatch ()
  "Lark Contacts commands."
  ["Contacts"
   ("g" "Get user"      elark-contact-get-user)
   ("s" "Search"        elark-contact-search)
   ("C" "Clear cache"   elark-contact-clear-cache)])

(provide 'elark-contact)
;;; elark-contact.el ends here
