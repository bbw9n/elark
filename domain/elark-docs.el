;;; elark-docs.el --- Lark Docs integration -*- lexical-binding: t; -*-

;; Copyright (C) 2026 bbw9n

;; Author: bbw9n <bbw9nio@gmail.com>
;; Assisted-by: Claude:claude-opus-5
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Provides Docs domain commands for elark: document creation,
;; fetching, updating, searching, and media operations.
;;
;; CLI command mapping:
;;   Create:           docs +create --title X [--markdown X] [--folder-token X]
;;                                  [--wiki-space X] [--wiki-node X]
;;   Fetch:            docs +fetch --doc X
;;   Update:           docs +update --doc X --markdown X --mode X
;;                                  [--new-title X] [--selection-by-title X]
;;                                  [--selection-with-ellipsis X]
;;   Search:           docs +search --query X [--page-size N]
;;   Media download:   docs +media-download --token X [--output X] [--type X]
;;   Media insert:     docs +media-insert --doc X --file X [--type X]
;;                                        [--align X] [--caption X]
;;   Media preview:    docs +media-preview --token X [--output X]
;;   Whiteboard:       docs +whiteboard-update --whiteboard-token X --source X
;;                                            [--input_format X] [--overwrite]

;;; Code:

(require 'elark-core)
(require 'elark-ui)
(require 'json)
(require 'transient)

;; org-lark (https://github.com/bbw9n/org-lark) provides Lark Markdown
;; → Org conversion; it is a hard dependency (see Package-Requires).
(require 'org-lark)

;;;; Customization

(defgroup elark-docs nil
  "Lark Docs settings."
  :group 'elark
  :prefix "elark-docs-")

(defcustom elark-docs-search-page-size "15"
  "Default page size for document search."
  :type 'string
  :group 'elark-docs)

(defcustom elark-docs-cache-directory nil
  "Directory for caching fetched documents and their media assets.
When nil, uses a PERSISTENT per-user cache under XDG cache home
\(usually ~/.cache/elark/elark-docs/).  Keeping it out of
variable `temporary-file-directory' means images survive reboots and macOS
temp cleanup instead of re-downloading every session — and doc
buffers (whose `default-directory' points here so relative image
links resolve) don't end up with a deleted working directory."
  :type '(choice (const :tag "XDG cache (persistent)" nil)
                 directory)
  :group 'elark-docs)

(defcustom elark-docs-render-mode 'org
  "How to render fetched document content.
`org' renders via org-lark (Lark tags, code blocks, tables
converted).
`markdown' displays raw markdown in `markdown-mode' or `special-mode'."
  :type '(choice (const :tag "Org (via org-lark)" org)
                 (const :tag "Markdown" markdown))
  :group 'elark-docs)

;;;; Buffer-local variables

(defvar-local elark-docs--doc-token nil
  "Document token for the current buffer.")

(defvar-local elark-docs--search-results nil
  "Cached search results for the current buffer.")

(defvar-local elark-docs--search-query nil
  "Last search query used.")

;;;; Result extraction

(defun elark-docs--extract-doc (data)
  "Extract document info from lark-cli response DATA."
  (or (alist-get 'data data)
      data))

(defun elark-docs--extract-search-results (data)
  "Extract search result list from lark-cli response DATA.
Handles the response shape: {data: {results: [...], has_more, total}}."
  (or (elark--get-nested data 'data 'results)
      (alist-get 'results data)
      (elark--get-nested data 'data 'items)
      (alist-get 'items data)))

(defun elark-docs--result-meta (doc)
  "Return the result_meta sub-alist from DOC, or DOC itself.
Search results nest fields under result_meta; other responses don't."
  (or (alist-get 'result_meta doc) doc))

(defun elark-docs--strip-highlight (str)
  "Remove <h>...</h> highlight tags from STR."
  (if (and str (stringp str))
      (replace-regexp-in-string "</?h>" "" str)
    str))

(defun elark-docs--doc-title (doc)
  "Extract the title from DOC."
  (or (elark-docs--strip-highlight (alist-get 'title_highlighted doc))
      (alist-get 'title doc)
      (alist-get 'title (elark-docs--result-meta doc))
      (alist-get 'name doc)
      "(untitled)"))

(defun elark-docs--doc-token (doc)
  "Extract the document token from DOC."
  (let ((meta (elark-docs--result-meta doc)))
    (or (alist-get 'document_id doc)
        (alist-get 'doc_token doc)
        (alist-get 'token meta)
        (alist-get 'obj_token doc)
        (alist-get 'id doc))))

(defun elark-docs--doc-type (doc)
  "Extract the document type from DOC."
  (let ((meta (elark-docs--result-meta doc)))
    (or (alist-get 'entity_type doc)
        (alist-get 'doc_types meta)
        (alist-get 'doc_type doc)
        (alist-get 'type doc)
        "")))

(defun elark-docs--doc-url (doc)
  "Extract the URL from DOC."
  (let ((meta (elark-docs--result-meta doc)))
    (or (alist-get 'url meta)
        (alist-get 'url doc)
        (alist-get 'doc_url doc)
        "")))

(defun elark-docs--doc-owner (doc)
  "Extract the owner from DOC."
  (let ((meta (elark-docs--result-meta doc)))
    (or (alist-get 'owner_name meta)
        (alist-get 'owner_name doc)
        (elark--get-nested doc 'owner 'name)
        "")))

(defun elark-docs--doc-create-time (doc)
  "Extract create time from DOC."
  (let* ((meta (elark-docs--result-meta doc))
         (iso (or (alist-get 'create_time_iso meta) ""))
         (ts (or (alist-get 'create_time meta)
                 (alist-get 'create_time doc)
                 (alist-get 'created_time doc))))
    (cond
     ((and (stringp iso) (not (string-empty-p iso)))
      (substring iso 0 (min 16 (length iso))))
     ((and (stringp ts) (not (string-empty-p ts)))
      (substring ts 0 (min 16 (length ts))))
     ((numberp ts) (or (elark--format-timestamp ts) ""))
     (t ""))))

(defun elark-docs--doc-update-time (doc)
  "Extract update time from DOC."
  (let* ((meta (elark-docs--result-meta doc))
         (iso (or (alist-get 'update_time_iso meta) ""))
         (ts (or (alist-get 'update_time meta)
                 (alist-get 'update_time doc)
                 (alist-get 'edit_time doc)
                 (alist-get 'updated_time doc))))
    (cond
     ((and (stringp iso) (not (string-empty-p iso)))
      (substring iso 0 (min 16 (length iso))))
     ((and (stringp ts) (not (string-empty-p ts)))
      (substring ts 0 (min 16 (length ts))))
     ((numberp ts) (or (elark--format-timestamp ts) ""))
     (t ""))))

;;;; Search
;; CLI: docs +search --query X [--page-size N]

(defvar elark-docs-search-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'elark-docs-open-at-point)
    (define-key map (kbd "O")   #'elark-docs-open-as-org-at-point)
    (define-key map (kbd "E")   #'elark-docs-export-org-at-point)
    (define-key map (kbd "g")   #'elark-docs-search-refresh)
    (define-key map (kbd "n")   #'elark-docs--next-result)
    (define-key map (kbd "p")   #'elark-docs--prev-result)
    (define-key map (kbd "y")   #'elark-docs-copy-token)
    (define-key map (kbd "?")   #'elark-docs-dispatch)
    map)
  "Keymap for `elark-docs-search-mode'.")

(define-derived-mode elark-docs-search-mode special-mode
  "Lark Docs"
  "Major mode for browsing Lark document search results.
Each result is displayed as a multi-line section.")

;;;; AI context provider
;;
;; Two entry points:
;;   - search-results buffer (`elark-docs-search-mode') — registered
;;     via the symbol property below.
;;   - doc-detail buffer (any of org-mode/markdown-mode/special-mode,
;;     identified by buffer name "*Lark Doc: ...*") — looked up by
;;     `elark-ai-context' as a `fboundp'-checked soft fallback.

(defun elark-docs--ai-context ()
  "Return the AI context plist for a docs buffer.
Branches on whether `elark-docs--doc-token' is bound (detail
buffer) vs not (search-results buffer)."
  (let ((doc-token (and (boundp 'elark-docs--doc-token)
                        elark-docs--doc-token))
        (results (and (boundp 'elark-docs--search-results)
                      elark-docs--search-results)))
    (if doc-token
        ;; Detail buffer — include content so the LLM doesn't re-fetch.
        (list :domain "docs"
              :buffer-type "doc-detail"
              :item (list :doc-token doc-token)
              :content (buffer-substring-no-properties
                        (point-min) (point-max))
              :summary (format "Viewing document %s" doc-token))
      (list :domain "docs"
            :buffer-type "search-results"
            :item nil
            :summary (format "Doc search results: %d items"
                             (length results))))))

(put 'elark-docs-search-mode 'elark-ai-context-provider
     #'elark-docs--ai-context)

(defun elark-docs--doc-token-at-point ()
  "Return the document token at point, or nil."
  (get-text-property (point) 'elark-doc-token))

(defun elark-docs--doc-title-at-point ()
  "Return the document title at point, or nil."
  (get-text-property (point) 'elark-doc-title))

(defun elark-docs--insert-field (label value)
  "Insert a LABEL: VALUE line if VALUE is non-empty."
  (elark-ui-insert-field label value))

(defun elark-docs--doc-summary (doc)
  "Extract and clean the summary from search result DOC."
  (elark-docs--strip-highlight
   (or (alist-get 'summary_highlighted doc)
       (alist-get 'summary doc)
       "")))

(defun elark-docs--insert-result (doc)
  "Insert a multi-line section for search result DOC."
  (let ((token (elark-docs--doc-token doc))
        (title (elark-docs--doc-title doc))
        (type (elark-docs--doc-type doc))
        (url (elark-docs--doc-url doc))
        (owner (elark-docs--doc-owner doc))
        (created (elark-docs--doc-create-time doc))
        (updated (elark-docs--doc-update-time doc))
        (summary (elark-docs--doc-summary doc))
        (beg (point)))
    (insert (propertize title 'face 'bold) "\n")
    (elark-docs--insert-field "Type" type)
    (elark-docs--insert-field "Token" token)
    (elark-docs--insert-field "Owner" owner)
    (elark-docs--insert-field "Created" created)
    (elark-docs--insert-field "Updated" updated)
    (elark-docs--insert-field "URL" url)
    (when (and summary (not (string-empty-p summary)))
      (insert (propertize (format "  %s" summary) 'face 'font-lock-comment-face) "\n"))
    (insert "\n")
    (put-text-property beg (point) 'elark-doc-token token)
    (put-text-property beg (point) 'elark-doc-title title)))

(defun elark-docs--next-result ()
  "Move to the next result section."
  (interactive)
  (elark-ui-next-section 'elark-doc-token))

(defun elark-docs--prev-result ()
  "Move to the previous result section."
  (interactive)
  (elark-ui-prev-section 'elark-doc-token))

;;;###autoload
(defun elark-docs-search (query)
  "Search Lark documents by QUERY."
  (interactive "sSearch docs: ")
  (when (string-empty-p query)
    (user-error "Search query is required"))
  (message "Lark: searching docs...")
  (let ((q query))
    (elark--run-command
     (list "docs" "+search" "--query" query
           "--page-size" elark-docs-search-page-size)
     (lambda (data) (elark-docs--display-search data q)))))

(defun elark-docs-search-refresh ()
  "Refresh the search results with the previous query."
  (interactive)
  (elark-docs-search (or elark-docs--search-query
                        (read-string "Search docs: "))))

(defun elark-docs--display-search (data &optional query)
  "Display search results DATA.  QUERY is stored for refresh."
  (let* ((results (elark-docs--extract-search-results data))
         (buf (get-buffer-create "*Lark Docs*")))
    (with-current-buffer buf
      (elark-docs-search-mode)
      (setq elark-docs--search-results results
            elark-docs--search-query query)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (if (null results)
            (insert "(no results)\n")
          (dolist (doc results)
            (elark-docs--insert-result doc))))
      (goto-char (point-min))
      (setq header-line-format
            (format " Lark Docs — %d result(s)" (length results))))
    (pop-to-buffer buf)))

(defun elark-docs-open-at-point ()
  "Fetch and display the document at point."
  (interactive)
  (let ((token (elark-docs--doc-token-at-point)))
    (unless token (user-error "No document at point"))
    (elark-docs-fetch token)))

(defun elark-docs-open-as-org-at-point ()
  "Fetch the document at point and display as Org."
  (interactive)
  (let ((token (elark-docs--doc-token-at-point)))
    (unless token (user-error "No document at point"))
    (elark-docs-fetch-as-org token)))

(defun elark-docs-export-org-at-point ()
  "Export the document at point to an Org file."
  (interactive)
  (let ((token (elark-docs--doc-token-at-point))
        (title (elark-docs--doc-title-at-point)))
    (unless token (user-error "No document at point"))
    (let ((output (read-file-name "Write Org file: " nil nil nil
                                  (concat (or title "elark-export") ".org"))))
      (elark-docs-export-org token output))))

(defun elark-docs-copy-token ()
  "Copy the document token at point to the kill ring."
  (interactive)
  (let ((token (or (elark-docs--doc-token-at-point) elark-docs--doc-token)))
    (unless token (user-error "No document token"))
    (kill-new token)
    (message "Copied: %s" token)))

;;;; Fetch
;; CLI: docs +fetch --doc X

;;;###autoload
(defun elark-docs-fetch (doc)
  "Fetch and display a Lark document by DOC (URL or token).
When `elark-docs-render-mode' is `org', the content is converted to
Org format via org-lark (Lark tags, code blocks, tables handled).
Otherwise raw markdown is shown."
  (interactive "sDocument URL or token: ")
  (when (string-empty-p doc)
    (user-error "Document URL or token is required"))
  (if (elark-docs--use-org-p)
      (elark-docs-fetch-as-org doc)
    (message "Lark: fetching document...")
    (elark--run-command
     (list "docs" "+fetch" "--doc" doc)
     (lambda (data) (elark-docs--display-document-markdown-from-data data doc)))))

(defun elark-docs--extract-content (doc)
  "Extract the markdown content string from a parsed DOC alist."
  (or (alist-get 'content doc)
      (alist-get 'markdown doc)
      (alist-get 'body doc)
      (elark--get-nested doc 'document 'content)
      ""))

(defun elark-docs--display-document-markdown-from-data (data doc-ref)
  "Display fetched document DATA as markdown.  DOC-REF is the URL or token."
  (let* ((doc (elark-docs--extract-doc data))
         (title (or (elark-docs--doc-title doc) doc-ref))
         (token (or (elark-docs--doc-token doc) doc-ref))
         (content (elark-docs--extract-content doc)))
    (elark-docs--display-document-markdown title token doc content)))

(defun elark-docs--display-document-markdown (title token doc content)
  "Display document as markdown.  TITLE, TOKEN, DOC, CONTENT as expected."
  (let ((buf (get-buffer-create (format "*Lark Doc: %s*" title))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (elark-ui-insert-title title)
        (elark-docs--insert-field "Token" token)
        (elark-docs--insert-field "Type" (elark-docs--doc-type doc))
        (elark-docs--insert-field "URL" (elark-docs--doc-url doc))
        (insert "\n")
        (if (string-empty-p content)
            (insert (propertize "(no content)" 'face 'font-lock-comment-face) "\n")
          (insert content "\n")))
      (if (fboundp 'markdown-mode) (markdown-mode) (special-mode))
      (setq elark-docs--doc-token token)
      (goto-char (point-min))
      (setq header-line-format (format " Lark Doc: %s" title)))
    (pop-to-buffer buf)))

;;;; Create
;; CLI: docs +create --title X [--markdown X] [--folder-token X]
;;                   [--wiki-space X] [--wiki-node X]

;;;###autoload (autoload 'elark-docs-create "elark-docs" nil t)
(transient-define-prefix elark-docs-create ()
  "Create a new Lark document."
  ["Document"
   ("t" "Title"        "--title=" :prompt "Title: ")
   ("m" "Markdown"     "--markdown=" :prompt "Markdown content (or @file): ")]
  ["Location"
   ("f" "Folder token" "--folder-token=" :prompt "Folder token: ")
   ("w" "Wiki space"   "--wiki-space=" :prompt "Wiki space ID: ")
   ("n" "Wiki node"    "--wiki-node=" :prompt "Wiki node token: ")]
  ["Actions"
   ("RET" "Create" elark-docs--do-create)
   ("q"   "Cancel" transient-quit-all)])

(defun elark-docs--do-create (&rest _args)
  "Execute document creation with transient arguments."
  (interactive)
  (let ((args (transient-args 'elark-docs-create)))
    (unless args
      (user-error "No document details provided"))
    (message "Lark: creating document...")
    (elark--run-command
     (append '("docs" "+create") args)
     (lambda (data)
       (let* ((doc (elark-docs--extract-doc data))
              (token (elark-docs--doc-token doc))
              (url (elark-docs--doc-url doc)))
         (message "Lark: document created%s%s"
                  (if token (format " (token: %s)" token) "")
                  (if (and url (not (string-empty-p url)))
                      (format " — %s" url) "")))))))

;;;; Update
;; CLI: docs +update --doc X --markdown X --mode X [--new-title X]
;;                   [--selection-by-title X] [--selection-with-ellipsis X]

;;;###autoload (autoload 'elark-docs-update "elark-docs" nil t)
(transient-define-prefix elark-docs-update ()
  "Update a Lark document."
  ["Target"
   ("d" "Document"     "--doc=" :prompt "Document URL or token: ")
   ("T" "New title"    "--new-title=" :prompt "New title: ")]
  ["Content"
   ("m" "Markdown"     "--markdown=" :prompt "Markdown content (or @file): ")
   ("M" "Mode"         "--mode=" :prompt "Mode: "
    :choices ("append" "overwrite" "replace_range" "replace_all"
              "insert_before" "insert_after" "delete_range"))]
  ["Selection"
   ("s" "By title"     "--selection-by-title=" :prompt "Title locator (e.g. ## Section): ")
   ("e" "By ellipsis"  "--selection-with-ellipsis=" :prompt "Content locator (e.g. start...end): ")]
  ["Actions"
   ("RET" "Update" elark-docs--do-update)
   ("q"   "Cancel" transient-quit-all)])

(defun elark-docs--do-update (&rest _args)
  "Execute document update with transient arguments."
  (interactive)
  (let ((args (transient-args 'elark-docs-update)))
    (unless args
      (user-error "No update details provided"))
    (message "Lark: updating document...")
    (elark--run-command
     (append '("docs" "+update") args)
     (lambda (_data)
       (message "Lark: document updated")))))

;;;; Media download
;; CLI: docs +media-download --token X [--output X] [--type X]

;;;###autoload
(defun elark-docs-media-download (token &optional output type)
  "Download media by TOKEN.
Optional OUTPUT is the local save path.
Optional TYPE is \"media\" (default) or \"whiteboard\"."
  (interactive
   (list (read-string "Media token: ")
         (read-file-name "Save to (empty for auto): " nil nil nil)
         (completing-read "Type: " '("media" "whiteboard") nil t nil nil "media")))
  (when (string-empty-p token)
    (user-error "Media token is required"))
  (message "Lark: downloading media...")
  (let ((args (list "docs" "+media-download" "--token" token)))
    (when (and output (not (string-empty-p output)))
      (setq args (append args (list "--output" (expand-file-name output)))))
    (when (and type (not (string-empty-p type)) (not (equal type "media")))
      (setq args (append args (list "--type" type))))
    (elark--run-command
     args
     (lambda (data)
       (let ((path (or (alist-get 'path data)
                       (alist-get 'output data)
                       (elark--get-nested data 'data 'path))))
         (message "Lark: media downloaded%s"
                  (if path (format " → %s" path) "")))))))

;;;; Media insert
;; CLI: docs +media-insert --doc X --file X [--type X] [--align X] [--caption X]

;;;###autoload
(defun elark-docs-media-insert (doc file &optional type align caption)
  "Insert media FILE into document DOC.
TYPE is \"image\" (default) or \"file\".
ALIGN is \"left\", \"center\", or \"right\".
CAPTION is an optional image caption."
  (interactive
   (list (read-string "Document URL or token: "
                      (when elark-docs--doc-token elark-docs--doc-token))
         (read-file-name "File: ")
         (completing-read "Type: " '("image" "file") nil t nil nil "image")
         (completing-read "Align: " '("" "left" "center" "right") nil nil)
         (read-string "Caption (optional): ")))
  (when (string-empty-p doc)
    (user-error "Document URL or token is required"))
  (when (string-empty-p file)
    (user-error "File path is required"))
  (message "Lark: inserting media...")
  (let ((args (list "docs" "+media-insert"
                    "--doc" doc
                    "--file" (expand-file-name file))))
    (when (and type (not (string-empty-p type)))
      (setq args (append args (list "--type" type))))
    (when (and align (not (string-empty-p align)))
      (setq args (append args (list "--align" align))))
    (when (and caption (not (string-empty-p caption)))
      (setq args (append args (list "--caption" caption))))
    (elark--run-command
     args
     (lambda (_data)
       (message "Lark: media inserted into document")))))

;;;; Media preview
;; CLI: docs +media-preview --token X [--output X]

;;;###autoload
(defun elark-docs-media-preview (token &optional output)
  "Preview media file by TOKEN.
Optional OUTPUT is the local save path."
  (interactive
   (list (read-string "Media token: ")
         (read-file-name "Save to (empty for auto): " nil nil nil)))
  (when (string-empty-p token)
    (user-error "Media token is required"))
  (message "Lark: previewing media...")
  (let ((args (list "docs" "+media-preview" "--token" token)))
    (when (and output (not (string-empty-p output)))
      (setq args (append args (list "--output" (expand-file-name output)))))
    (elark--run-command
     args
     (lambda (data)
       (let ((path (or (alist-get 'path data)
                       (alist-get 'output data)
                       (elark--get-nested data 'data 'path))))
         (if path
             (progn
               (message "Lark: media saved to %s" path)
               (find-file path))
           (message "Lark: media preview complete")))))))

;;;; Whiteboard update
;; CLI: docs +whiteboard-update --whiteboard-token X --source X
;;                              [--input_format X] [--overwrite]

;;;###autoload (autoload 'elark-docs-whiteboard-update "elark-docs" nil t)
(transient-define-prefix elark-docs-whiteboard-update ()
  "Update a whiteboard in a Lark document."
  ["Whiteboard"
   ("w" "Whiteboard token" "--whiteboard-token=" :prompt "Whiteboard token: ")
   ("s" "Source"           "--source=" :prompt "Source data (or @file): ")
   ("f" "Input format"    "--input_format=" :prompt "Format: "
    :choices ("raw" "plantuml" "mermaid"))
   ("o" "Overwrite"       "--overwrite")]
  ["Actions"
   ("RET" "Update" elark-docs--do-whiteboard-update)
   ("q"   "Cancel" transient-quit-all)])

(defun elark-docs--do-whiteboard-update (&rest _args)
  "Execute whiteboard update with transient arguments."
  (interactive)
  (let ((args (transient-args 'elark-docs-whiteboard-update)))
    (unless args
      (user-error "No whiteboard details provided"))
    (message "Lark: updating whiteboard...")
    (elark--run-command
     (append '("docs" "+whiteboard-update") args)
     (lambda (_data)
       (message "Lark: whiteboard updated")))))

;;;; org-lark integration

(defun elark-docs--use-org-p ()
  "Return non-nil if documents should be rendered as Org."
  (eq elark-docs-render-mode 'org))

(defun elark-docs--safe-dirname (s)
  "Sanitize S for use as a directory name."
  (replace-regexp-in-string "[^A-Za-z0-9._-]" "_" (or s "doc")))

(defun elark-docs--cache-dir (doc-id)
  "Return the cache directory for DOC-ID's rendered doc and assets.
Under `elark-docs-cache-directory' when set, else the persistent
per-user cache (see `elark--cache-dir')."
  (expand-file-name
   (elark-docs--safe-dirname doc-id)
   (or (and elark-docs-cache-directory
            (file-name-as-directory
             (expand-file-name "elark-docs" elark-docs-cache-directory)))
       (elark--cache-dir "elark-docs"))))

(defun elark-docs--sync-org-lark-config ()
  "Sync elark config into org-lark variables."
  (setq org-lark-cli-program elark-cli-executable))

;;;###autoload
(defun elark-docs-fetch-as-org (doc)
  "Fetch a Lark document DOC and display it in `org-mode'.
Runs asynchronously: fetch, pandoc conversion and media downloads
all happen in background processes so Emacs stays responsive."
  (interactive "sDocument URL or token: ")
  (when (string-empty-p doc)
    (user-error "Document URL or token is required"))
  (elark-docs--sync-org-lark-config)
    (message "Lark: fetching document as org...")
    (org-lark-fetch-async
     doc
     (lambda (err fetched)
       (cond
        (err (message "Lark: fetch failed: %s" err))
        (t
         (let ((markdown (alist-get 'markdown fetched))
               (title (or (alist-get 'title fetched) doc))
               (doc-id (or (alist-get 'doc_id fetched) doc)))
           (if (or (null markdown) (string-empty-p markdown))
               (message "Lark: document has no content")
             (let* ((cache-dir (elark-docs--cache-dir doc-id))
                    (st (make-org-lark--state
                         :output-file (expand-file-name "doc.org" cache-dir)
                         :asset-dir (expand-file-name "assets/" cache-dir))))
               (message "Lark: converting to org...")
               (org-lark--pipeline-async
                markdown fetched doc st
                (lambda (err2 org-content)
                  (cond
                   (err2 (message "Lark: conversion failed: %s" err2))
                   (t (elark-docs--display-org-buffer
                       org-content title doc-id cache-dir)))))))))))))

(defun elark-docs--display-org-buffer (org-content title token &optional base-dir)
  "Display ORG-CONTENT in an `org-mode' buffer named after TITLE.
TOKEN is stored as the doc token.  BASE-DIR, when non-nil, is set
as `default-directory' so relative file links (images) resolve,
and the buffer visits BASE-DIR/doc.org — so it can be edited in
place (by hand or with `gptel-rewrite') and pushed back with
`org-lark-publish-buffer' (the #+lark_doc_id header targets the
same remote doc)."
  (let* ((file (and base-dir (expand-file-name "doc.org" base-dir)))
         (name (format "*Lark Doc: %s*" title))
         (buf (if file (find-file-noselect file) (get-buffer-create name))))
    (with-current-buffer buf
      ;; Re-fetching must not silently discard unpublished edits.
      (when (and file (buffer-modified-p)
                 (not (yes-or-no-p
                       (format "%s has unsaved edits; discard them? " name))))
        (user-error "Lark: re-fetch cancelled; local edits kept"))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert org-content))
      (when file
        (make-directory base-dir t)
        (save-buffer)
        (rename-buffer name t))
      (org-mode)
      (when base-dir
        (setq-local default-directory (file-name-as-directory base-dir)))
      (setq-local elark-docs--doc-token token)
      (goto-char (point-min))
      (setq header-line-format
            (format " Lark Doc (org): %s%s" title
                    (if file "  —  M-x org-lark-publish-buffer to push edits" "")))
      ;; Display inline images if any were downloaded
      (when (fboundp 'org-display-inline-images)
        (org-display-inline-images)))
    (pop-to-buffer buf)))

;;;###autoload
(defun elark-docs-export-org (doc output-file)
  "Export Lark document DOC to OUTPUT-FILE as Org.
Delegates to `org-lark-export-async' which handles fetch, pandoc
conversion, and media download in background processes."
  (interactive
   (list (read-string "Document URL or token: "
                      (when elark-docs--doc-token elark-docs--doc-token))
         (read-file-name "Write Org file: " nil nil nil "elark-export.org")))
  (elark-docs--sync-org-lark-config)
  (message "Lark: exporting document...")
  (org-lark-export-async
   doc output-file
   (lambda (err path)
     (cond
      (err (message "Lark: export failed: %s" err))
      (t (message "Lark: exported to %s" path))))))

(defun elark-docs-toggle-render-mode ()
  "Toggle between org and markdown render modes."
  (interactive)
  (setq elark-docs-render-mode
        (if (eq elark-docs-render-mode 'markdown) 'org 'markdown))
  (message "Lark docs render mode: %s" elark-docs-render-mode))

;;;; Transient dispatch

;;;###autoload (autoload 'elark-docs-dispatch "elark-docs" nil t)
(transient-define-prefix elark-docs-dispatch ()
  "Lark Docs commands."
  ["Read"
   ("s" "Search"           elark-docs-search)
   ("f" "Fetch document"   elark-docs-fetch)
   ("O" "Fetch as Org"     elark-docs-fetch-as-org)]
  ["Write"
   ("c" "Create document"  elark-docs-create)
   ("u" "Update document"  elark-docs-update)]
  ["Org Export"
   ("E" "Export to Org file" elark-docs-export-org)
   ("M" "Toggle render mode" elark-docs-toggle-render-mode)]
  ["Media"
   ("d" "Download media"   elark-docs-media-download)
   ("i" "Insert media"     elark-docs-media-insert)
   ("p" "Preview media"    elark-docs-media-preview)]
  ["Whiteboard"
   ("w" "Update whiteboard" elark-docs-whiteboard-update)])

(provide 'elark-docs)
;;; elark-docs.el ends here
