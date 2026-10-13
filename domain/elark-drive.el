;;; elark-drive.el --- Lark Drive integration -*- lexical-binding: t; -*-

;; Copyright (C) 2026 bbw9n

;; Author: bbw9n <bbw9nio@gmail.com>
;; Assisted-by: Claude:claude-opus-5
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Provides a dired-like file browser for Lark Drive, plus upload,
;; download, copy, move, import, and export operations.
;;
;; CLI command mapping:
;;   List folder:    drive files list --params '{"folder_token":"X"}'
;;                     --page-all
;;   Create folder:  drive files create_folder
;;                     --data '{"name":"X","folder_token":"Y"}'
;;   Copy file:      drive files copy --params '{"file_token":"X"}'
;;                     --data '{"name":"Y","type":"Z","folder_token":"W"}'
;;   Upload:         drive +upload --file X [--folder-token Y] [--name Z]
;;   Download:       drive +download --file-token X [--output Y]
;;   Move:           drive +move --file-token X --folder-token Y [--type Z]
;;   Import:         drive +import --file X [--folder-token Y] [--type Z]
;;   Export:         drive +export --token X --doc-type Y --file-extension Z
;;                     [--output-dir W]

;;; Code:

(require 'elark-core)
(require 'elark-ui)
(require 'elark-contact)
(require 'json)
(require 'transient)
(require 'elark-docs)
(require 'elark-sheets)


;;;; Customization

(defgroup elark-drive nil
  "Lark Drive settings."
  :group 'elark
  :prefix "elark-drive-")

;;;; Buffer-local variables

(defvar-local elark-drive--folder-token nil
  "Current folder token.  Empty string or nil means root.")

(defvar-local elark-drive--folder-name nil
  "Current folder display name.")

(defvar-local elark-drive--parent-token nil
  "Parent folder token for \"go up\" navigation.")

(defvar-local elark-drive--files nil
  "Cached file list for the current folder.")

(defvar-local elark-drive--folder-history nil
  "Stack of (token . name) for breadcrumb navigation.")

;;;; File field extractors

(defun elark-drive--file-token (file)
  "Extract the token from FILE."
  (or (alist-get 'token file) (alist-get 'file_token file) ""))

(defun elark-drive--file-name (file)
  "Extract the name from FILE."
  (or (alist-get 'name file) (alist-get 'title file) "(unnamed)"))

(defun elark-drive--file-type (file)
  "Extract the type from FILE."
  (or (alist-get 'type file) "file"))

(defun elark-drive--file-url (file)
  "Extract the URL from FILE."
  (or (alist-get 'url file) ""))

(defun elark-drive--file-owner-id (file)
  "Extract the owner ID from FILE."
  (or (alist-get 'owner_id file) ""))

(defun elark-drive--file-modified-time (file)
  "Extract and format the modified time from FILE."
  (let ((ts (alist-get 'modified_time file)))
    (cond
     ((and (stringp ts) (not (string-empty-p ts)))
      (or (elark--format-timestamp ts) ts))
     ((numberp ts) (or (elark--format-timestamp ts) ""))
     (t ""))))

(defun elark-drive--file-created-time (file)
  "Extract and format the created time from FILE."
  (let ((ts (alist-get 'created_time file)))
    (cond
     ((and (stringp ts) (not (string-empty-p ts)))
      (or (elark--format-timestamp ts) ts))
     ((numberp ts) (or (elark--format-timestamp ts) ""))
     (t ""))))

(defun elark-drive--file-parent (file)
  "Extract the parent token from FILE."
  (or (alist-get 'parent_token file) ""))

(defun elark-drive--folder-p (file)
  "Return non-nil if FILE is a folder."
  (equal (elark-drive--file-type file) "folder"))

(defun elark-drive--type-indicator (type)
  "Return a display indicator for file TYPE."
  (pcase type
    ("folder"   "/")
    ("doc"      " [doc]")
    ("docx"     " [docx]")
    ("sheet"    " [sheet]")
    ("bitable"  " [bitable]")
    ("mindnote" " [mindnote]")
    ("slides"   " [slides]")
    (_          "")))

;;;; Response extraction

(defun elark-drive--extract-files (data)
  "Extract the file list from lark-cli response DATA."
  (or (elark--get-nested data 'data 'files)
      (elark--list-field data 'files)
      (elark--get-nested data 'data 'items)
      (elark--list-field data 'items)))

;;;; Dired-like mode

(defvar elark-drive-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'elark-drive-open)
    (define-key map (kbd "^")   #'elark-drive-up)
    (define-key map (kbd "g")   #'elark-drive-refresh)
    (define-key map (kbd "+")   #'elark-drive-mkdir)
    (define-key map (kbd "C")   #'elark-drive-copy)
    (define-key map (kbd "R")   #'elark-drive-move)
    (define-key map (kbd "U")   #'elark-drive-upload)
    (define-key map (kbd "D")   #'elark-drive-download)
    (define-key map (kbd "I")   #'elark-drive-import)
    (define-key map (kbd "E")   #'elark-drive-export)
    (define-key map (kbd "w")   #'elark-drive-copy-token)
    (define-key map (kbd "W")   #'elark-drive-copy-url)
    (define-key map (kbd "v")   #'elark-drive-view-content)
    (define-key map (kbd "n")   #'elark-drive--next-file)
    (define-key map (kbd "p")   #'elark-drive--prev-file)
    (define-key map (kbd "?")   #'elark-drive-dispatch)
    map)
  "Keymap for `elark-drive-mode'.
Keybindings follow Dired conventions where possible.")

(define-derived-mode elark-drive-mode special-mode
  "Lark Drive"
  "Major mode for browsing Lark Drive, modelled after Dired.

\\{elark-drive-mode-map}")

;;;; AI context provider

(defun elark-drive--ai-context ()
  "Return the AI context plist for a drive buffer."
  (let ((folder elark-drive--folder-token)
        (files elark-drive--files)
        (file-token (get-text-property (point) 'elark-file-token)))
    (list :domain "drive"
          :buffer-type "file-list"
          :item (when file-token (list :file-token file-token))
          :summary (format "Drive folder %s, %d files"
                           (or folder "root") (length files)))))

(put 'elark-drive-mode 'elark-ai-context-provider #'elark-drive--ai-context)

;;;; Text properties & navigation

(defun elark-drive--file-at-point ()
  "Return the file alist at point, or nil."
  (get-text-property (point) 'elark-drive-file))

(defun elark-drive--token-at-point ()
  "Return the file token at point, or nil."
  (let ((f (elark-drive--file-at-point)))
    (when f (elark-drive--file-token f))))

(defun elark-drive--next-file ()
  "Move to the next file entry."
  (interactive)
  (let ((current (elark-drive--token-at-point))
        (pos (point)))
    (when current
      (while (and (not (eobp))
                  (equal (elark-drive--token-at-point) current))
        (forward-char)))
    (while (and (not (eobp))
                (not (elark-drive--file-at-point)))
      (forward-char))
    (when (eobp) (goto-char pos))))

(defun elark-drive--prev-file ()
  "Move to the previous file entry."
  (interactive)
  (let ((current (elark-drive--token-at-point))
        (pos (point)))
    (when current
      (while (and (not (bobp))
                  (equal (elark-drive--token-at-point) current))
        (backward-char)))
    (while (and (not (bobp))
                (not (elark-drive--file-at-point)))
      (backward-char))
    (let ((target (elark-drive--token-at-point)))
      (if target
          (while (and (not (bobp))
                      (equal (elark-drive--token-at-point) target))
            (backward-char))
        (goto-char pos)))
    ;; Move forward onto the entry start
    (unless (elark-drive--file-at-point)
      (forward-char))))

;;;; Rendering

(defun elark-drive--insert-file-entry (file)
  "Insert a single-line entry for FILE, like a Dired listing."
  (let* ((name (elark-drive--file-name file))
         (type (elark-drive--file-type file))
         (modified (elark-drive--file-modified-time file))
         (owner-id (elark-drive--file-owner-id file))
         ;; Non-blocking: cached name or a tagged raw id that the
         ;; post-render `elark-contact-resolve-buffer-async' pass patches.
         (owner-name (if (string-empty-p owner-id) ""
                       (elark-contact-annotate owner-id "open_id")))
         (folder-p (elark-drive--folder-p file))
         (indicator (elark-drive--type-indicator type))
         (beg (point)))
    (insert "  "
            (propertize (format "%-8s" type)
                        'face 'font-lock-type-face)
            "  "
            (propertize (format "%-16s" modified)
                        'face 'font-lock-comment-face)
            "  "
            (propertize (format "%-12s" owner-name)
                        'face 'font-lock-string-face)
            "  "
            (propertize (concat name indicator)
                        'face (if folder-p 'bold 'default))
            "\n")
    (put-text-property beg (point) 'elark-drive-file file)))

(defun elark-drive--render (files folder-token folder-name)
  "Render FILES into the drive buffer for FOLDER-TOKEN / FOLDER-NAME."
  (let ((buf (get-buffer-create "*Lark Drive*"))
        ;; Sort: folders first, then by name
        (sorted (sort (copy-sequence files)
                      (lambda (a b)
                        (let ((fa (elark-drive--folder-p a))
                              (fb (elark-drive--folder-p b)))
                          (cond
                           ((and fa (not fb)) t)
                           ((and (not fa) fb) nil)
                           (t (string< (downcase (elark-drive--file-name a))
                                       (downcase (elark-drive--file-name b))))))))))
    (with-current-buffer buf
      (elark-drive-mode)
      (setq elark-drive--files files
            elark-drive--folder-token folder-token
            elark-drive--folder-name (or folder-name "My Drive"))
      ;; Derive parent from first file's parent if we're not at root
      (setq elark-drive--parent-token
            (when (and files (not (string-empty-p (or folder-token ""))))
              (elark-drive--file-parent (car files))))
      (let ((inhibit-read-only t))
        (erase-buffer)
        ;; Header / breadcrumb
        (insert (propertize (format "  %s" (or folder-name "My Drive")) 'face 'bold))
        (when (and folder-token (not (string-empty-p folder-token)))
          (insert (propertize (format "  [%s]" folder-token) 'face 'font-lock-comment-face)))
        (insert "\n"
                (elark-ui-separator 60) "\n")
        ;; ".." entry for going up
        (when (and folder-token (not (string-empty-p folder-token)))
          (let ((beg (point)))
            (insert "  "
                    (propertize "folder  " 'face 'font-lock-type-face)
                    "  "
                    (propertize (format "%-16s" "") 'face 'font-lock-comment-face)
                    "  "
                    (propertize ".." 'face 'bold)
                    "\n")
            (put-text-property beg (point) 'elark-drive-file '((name . "..") (type . "parent")))))
        ;; File entries
        (if (null sorted)
            (insert "  (empty folder)\n")
          (dolist (file sorted)
            (elark-drive--insert-file-entry file))))
      (goto-char (point-min))
      ;; Move to first file entry
      (while (and (not (eobp)) (not (elark-drive--file-at-point)))
        (forward-line 1))
      (setq header-line-format
            (format " Lark Drive: %s — %d item(s)"
                    (or folder-name "My Drive") (length files))))
    (pop-to-buffer buf)
    ;; Owner names resolve in the background — rendering never blocks.
    (elark-contact-resolve-buffer-async buf)))

;;;; List folder / browse

;;;###autoload
(defun elark-drive-list (&optional folder-token folder-name)
  "List files in FOLDER-TOKEN (nil/empty for root).
FOLDER-NAME is used for display."
  (interactive)
  (message "Lark: listing drive folder...")
  (let ((token (or folder-token ""))
        (name (or folder-name "My Drive")))
    (elark--run-command
     (list "drive" "files" "list"
           "--params" (json-encode `((folder_token . ,token))))
     (lambda (data)
       (let ((files (elark-drive--extract-files data)))
         (elark-drive--render files token name))))))

(defun elark-drive-refresh ()
  "Refresh the current folder listing."
  (interactive)
  (elark-drive-list elark-drive--folder-token elark-drive--folder-name))

(defun elark-drive-open ()
  "Open the item at point.
Folders descend into them; files show metadata."
  (interactive)
  (let ((file (elark-drive--file-at-point)))
    (unless file (user-error "No file at point"))
    (let ((type (elark-drive--file-type file))
          (token (elark-drive--file-token file))
          (name (elark-drive--file-name file)))
      (cond
       ;; ".." — go up
       ((equal type "parent")
        (elark-drive-up))
       ;; Folder — descend
       ((equal type "folder")
        ;; Push current location to history
        (push (cons elark-drive--folder-token elark-drive--folder-name)
              elark-drive--folder-history)
        (elark-drive-list token name))
       ;; File — show detail
       (t
        (elark-drive--show-detail file))))))

(defun elark-drive-up ()
  "Go up to the parent folder."
  (interactive)
  (if elark-drive--folder-history
      ;; Use history stack for accurate back-navigation
      (let ((prev (pop elark-drive--folder-history)))
        (elark-drive-list (car prev) (cdr prev)))
    ;; Fallback: go to root
    (elark-drive-list "" "My Drive")))

;;;; File detail view

(defvar-local elark-drive--detail-file nil
  "The file alist displayed in this detail buffer.")

(defvar elark-drive-detail-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'elark-drive-detail-view-content)
    (define-key map (kbd "?")   #'elark-drive-dispatch)
    map)
  "Keymap for `elark-drive-detail-mode'.")

(define-derived-mode elark-drive-detail-mode special-mode
  "Lark Drive Detail"
  "Major mode for viewing Lark Drive file detail.
Press RET to view document content (doc/docx/sheet).

\\{elark-drive-detail-mode-map}")

(defun elark-drive-detail-view-content ()
  "View the content of the file shown in this detail buffer."
  (interactive)
  (unless elark-drive--detail-file
    (user-error "No file in this buffer"))
  (let ((type (elark-drive--file-type elark-drive--detail-file))
        (url (elark-drive--file-url elark-drive--detail-file))
        (token (elark-drive--file-token elark-drive--detail-file)))
    (let ((ref (if (and url (not (string-empty-p url))) url token)))
      (when (string-empty-p ref)
        (user-error "No URL or token available for this file"))
      (pcase type
        ((or "doc" "docx")
         (elark-docs-fetch ref))
        ("sheet"
         (elark-sheets-info ref))
        (_
         (user-error "Viewing content is not supported for type \"%s\"" type))))))

(defun elark-drive--show-detail (file)
  "Display detail for FILE in a buffer."
  (let* ((name (elark-drive--file-name file))
         (type (elark-drive--file-type file))
         (owner-id (elark-drive--file-owner-id file))
         (owner-name (if (string-empty-p owner-id) ""
                       (elark-contact-annotate owner-id "open_id")))
         (viewable (member type '("doc" "docx" "sheet")))
         (buf (get-buffer-create (format "*Lark Drive: %s*" name))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (elark-ui-insert-title name)
        (elark-drive--detail-field "Type" type)
        (elark-drive--detail-field "Token" (elark-drive--file-token file))
        (elark-drive--detail-field "Modified" (elark-drive--file-modified-time file))
        (elark-drive--detail-field "Created" (elark-drive--file-created-time file))
        (elark-drive--detail-field "Owner" owner-name)
        (elark-drive--detail-field "URL" (elark-drive--file-url file))
        (when viewable
          (insert "\n" (propertize "Press RET to view content" 'face 'font-lock-comment-face) "\n")))
      (elark-drive-detail-mode)
      (setq elark-drive--detail-file file)
      (goto-char (point-min)))
    (pop-to-buffer buf)
    (elark-contact-resolve-buffer-async buf)))

(defun elark-drive--detail-field (label value)
  "Insert LABEL: VALUE if VALUE is non-empty."
  (elark-ui-insert-field label value 12))

;;;; View document content

(defun elark-drive-view-content ()
  "Fetch and view the content of the doc/docx/sheet at point.
Uses the file URL to dispatch to `elark-docs-fetch' or `elark-sheets-info'."
  (interactive)
  (let ((file (elark-drive--file-at-point)))
    (unless file (user-error "No file at point"))
    (let ((type (elark-drive--file-type file))
          (url (elark-drive--file-url file))
          (token (elark-drive--file-token file)))
      (when (elark-drive--folder-p file)
        (user-error "Cannot view content of a folder"))
      (let ((ref (if (and url (not (string-empty-p url))) url token)))
        (when (string-empty-p ref)
          (user-error "No URL or token available for this file"))
        (pcase type
          ((or "doc" "docx")
           (elark-docs-fetch ref))
          ("sheet"
           (elark-sheets-info ref))
          (_
           (user-error "Viewing content is not supported for type \"%s\"" type)))))))

;;;; Create folder (dired "+")

(defun elark-drive-mkdir (name)
  "Create a new folder named NAME in the current folder."
  (interactive "sNew folder name: ")
  (when (string-empty-p name)
    (user-error "Folder name is required"))
  (message "Lark: creating folder...")
  (let ((folder-token (or elark-drive--folder-token "")))
    (elark--run-command
     (list "drive" "files" "create_folder"
           "--data" (json-encode `((name . ,name)
                                   (folder_token . ,folder-token))))
     (lambda (_data)
       (message "Lark: folder \"%s\" created" name)
       (elark-drive-refresh)))))

;;;; Copy file (dired "C")

(defun elark-drive-copy ()
  "Copy the file at point to the current folder with a new name."
  (interactive)
  (let ((file (elark-drive--file-at-point)))
    (unless file (user-error "No file at point"))
    (let* ((token (elark-drive--file-token file))
           (type (elark-drive--file-type file))
           (old-name (elark-drive--file-name file))
           (new-name (read-string (format "Copy \"%s\" as: " old-name) old-name))
           (target-folder (or elark-drive--folder-token "")))
      (when (string-empty-p new-name)
        (user-error "Name is required"))
      (message "Lark: copying file...")
      (elark--run-command
       (list "drive" "files" "copy"
             "--params" (json-encode `((file_token . ,token)))
             "--data" (json-encode `((name . ,new-name)
                                     (type . ,type)
                                     (folder_token . ,target-folder))))
       (lambda (_data)
         (message "Lark: file copied")
         (elark-drive-refresh))))))

;;;; Move file (dired "R" for rename/move)

(defun elark-drive-move ()
  "Move the file at point to another folder."
  (interactive)
  (let ((file (elark-drive--file-at-point)))
    (unless file (user-error "No file at point"))
    (let* ((token (elark-drive--file-token file))
           (type (elark-drive--file-type file))
           (name (elark-drive--file-name file))
           (target (read-string (format "Move \"%s\" to folder token: " name))))
      (when (string-empty-p target)
        (user-error "Target folder token is required"))
      (message "Lark: moving file...")
      (elark--run-command
       (list "drive" "+move"
             "--file-token" token
             "--folder-token" target
             "--type" type)
       (lambda (_data)
         (message "Lark: file moved")
         (elark-drive-refresh))))))

;;;; Upload (dired-like)

(defun elark-drive-upload (file)
  "Upload local FILE to the current folder."
  (interactive "fUpload file: ")
  (message "Lark: uploading %s..." (file-name-nondirectory file))
  (let ((args (list "drive" "+upload"
                    "--file" (expand-file-name file))))
    (when (and elark-drive--folder-token
               (not (string-empty-p elark-drive--folder-token)))
      (setq args (append args (list "--folder-token" elark-drive--folder-token))))
    (elark--run-command
     args
     (lambda (data)
       (let ((token (or (elark--get-nested data 'data 'file_token)
                        (alist-get 'file_token data))))
         (message "Lark: uploaded%s" (if token (format " (token: %s)" token) ""))
         (when (derived-mode-p 'elark-drive-mode)
           (elark-drive-refresh)))))))

;;;; Download

(defun elark-drive-download ()
  "Download the file at point to a local directory."
  (interactive)
  (let ((file (elark-drive--file-at-point)))
    (unless file (user-error "No file at point"))
    (when (elark-drive--folder-p file)
      (user-error "Cannot download a folder"))
    (let* ((token (elark-drive--file-token file))
           (name (elark-drive--file-name file))
           (dir (read-directory-name (format "Save \"%s\" to: " name)))
           (output (expand-file-name name dir)))
      (message "Lark: downloading %s..." name)
      (elark--run-command
       (list "drive" "+download"
             "--file-token" token
             "--output" output
             "--overwrite")
       (lambda (_data)
         (message "Lark: downloaded to %s" output))))))

;;;; Import local file to Drive

(defun elark-drive-import (file type)
  "Import local FILE to Drive as cloud document TYPE (docx, sheet, bitable)."
  (interactive
   (list (read-file-name "Import file: ")
         (completing-read "Import as: " '("docx" "sheet" "bitable") nil t)))
  (message "Lark: importing %s..." (file-name-nondirectory file))
  (let ((args (list "drive" "+import"
                    "--file" (expand-file-name file)
                    "--type" type)))
    (when (and elark-drive--folder-token
               (not (string-empty-p elark-drive--folder-token)))
      (setq args (append args (list "--folder-token" elark-drive--folder-token))))
    (elark--run-command
     args
     (lambda (_data)
       (message "Lark: import complete")
       (when (derived-mode-p 'elark-drive-mode)
         (elark-drive-refresh))))))

;;;; Export cloud doc to local

(defun elark-drive-export ()
  "Export the file at point to a local directory."
  (interactive)
  (let ((file (elark-drive--file-at-point)))
    (unless file (user-error "No file at point"))
    (when (elark-drive--folder-p file)
      (user-error "Cannot export a folder"))
    (let* ((token (elark-drive--file-token file))
           (type (elark-drive--file-type file))
           (name (elark-drive--file-name file))
           (ext (completing-read
                 (format "Export \"%s\" as: " name)
                 '("docx" "pdf" "xlsx" "csv" "markdown") nil t))
           (dir (read-directory-name "Save to: "))
           (default-directory (expand-file-name dir)))
      (message "Lark: exporting %s as %s..." name ext)
      (elark--run-command
       (list "drive" "+export"
             "--token" token
             "--doc-type" type
             "--file-extension" ext
             "--output-dir" "."
             "--overwrite")
       (lambda (data)
         (let ((path (or (alist-get 'path data)
                         (elark--get-nested data 'data 'path))))
           (message "Lark: exported%s"
                    (if path (format " → %s" (expand-file-name path dir)) ""))))))))

;;;; Copy token / URL

(defun elark-drive-copy-token ()
  "Copy the file token at point to the kill ring."
  (interactive)
  (let ((token (elark-drive--token-at-point)))
    (unless token (user-error "No file at point"))
    (kill-new token)
    (message "Copied token: %s" token)))

(defun elark-drive-copy-url ()
  "Copy the file URL at point to the kill ring."
  (interactive)
  (let ((file (elark-drive--file-at-point)))
    (unless file (user-error "No file at point"))
    (let ((url (elark-drive--file-url file)))
      (if (string-empty-p url)
          (user-error "No URL for this file")
        (kill-new url)
        (message "Copied URL: %s" url)))))

;;;; Transient dispatch

;;;###autoload (autoload 'elark-drive-dispatch "elark-drive" nil t)
(transient-define-prefix elark-drive-dispatch ()
  "Lark Drive commands."
  ["Browse"
   ("l" "List root"       elark-drive-list)
   ("g" "Refresh"         elark-drive-refresh)
   ("v" "View content"    elark-drive-view-content)]
  ["File Operations"
   ("U" "Upload"          elark-drive-upload)
   ("D" "Download"        elark-drive-download)
   ("+" "Create folder"   elark-drive-mkdir)
   ("C" "Copy"            elark-drive-copy)
   ("R" "Move/Rename"     elark-drive-move)]
  ["Import / Export"
   ("I" "Import"          elark-drive-import)
   ("E" "Export"           elark-drive-export)])

(provide 'elark-drive)
;;; elark-drive.el ends here
