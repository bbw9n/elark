;;; elark-wiki.el --- Lark Wiki (knowledge base) integration -*- lexical-binding: t; -*-

;; Copyright (C) 2026 bbw9n

;; Author: bbw9n <bbw9nio@gmail.com>
;; Assisted-by: Claude:claude-opus-5
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Provides Wiki domain commands for elark: browsing knowledge
;; spaces and their node trees, viewing node content, and node
;; create/delete/copy/move operations.
;;
;; Wiki is two nested trees: spaces contain nodes, and nodes nest into
;; sub-nodes.  Navigation mirrors `elark-drive': descend into a node
;; with RET, go up with `^', refresh with `g'.  A leaf node is backed
;; by a cloud object (doc/sheet/...); opening it hands off to
;; `elark-docs-fetch' or `elark-sheets-info'.
;;
;; CLI command mapping:
;;   Spaces:   wiki +space-list
;;   Nodes:    wiki +node-list --space-id X [--parent-node-token Y]
;;   Detail:   wiki +node-get --node-token X
;;   Create:   wiki +node-create --space-id X [--parent-node-token Y]
;;                               --title T [--obj-type docx]
;;   Delete:   wiki +node-delete --node-token X --yes
;;   Copy:     wiki +node-copy --node-token X --space-id S
;;                             (--target-space-id | --target-parent-node-token)
;;   Move:     wiki +move --node-token X [--source-space-id S]
;;                        (--target-space-id | --target-parent-token)

;;; Code:

(require 'elark-core)
(require 'elark-ui)
(require 'elark-contact)
(require 'json)
(require 'transient)
(require 'elark-docs)
(require 'elark-sheets)

;; Leaf-node content handoff loads these on demand.

;;;; Customization

(defgroup elark-wiki nil
  "Lark Wiki settings."
  :group 'elark
  :prefix "elark-wiki-")

(defcustom elark-wiki-default-obj-type "docx"
  "Default object type for newly created wiki nodes."
  :type '(choice (const "docx") (const "sheet") (const "bitable")
                 (const "mindnote") (const "slides"))
  :group 'elark-wiki)

;;;; Buffer-local variables

(defvar-local elark-wiki--space-id nil
  "Wiki space ID for the current node-browser buffer.")

(defvar-local elark-wiki--space-name nil
  "Display name of the current wiki space.")

(defvar-local elark-wiki--parent-token nil
  "Parent node token for the current level.  Nil means the space root.")

(defvar-local elark-wiki--level-name nil
  "Display name of the current level (space or parent node title).")

(defvar-local elark-wiki--nodes nil
  "Cached node list for the current level.")

(defvar-local elark-wiki--spaces nil
  "Cached space list for a spaces buffer.")

(defvar-local elark-wiki--history nil
  "Stack of (PARENT-TOKEN . LEVEL-NAME) for breadcrumb navigation.")

(defvar-local elark-wiki--detail-node nil
  "The node alist displayed in a detail buffer.")

;;;; Space field extractors

(defun elark-wiki--space-id-of (space)
  "Extract the space ID from SPACE."
  (or (alist-get 'space_id space) ""))

(defun elark-wiki--space-name (space)
  "Extract the display name from SPACE."
  (or (alist-get 'name space) "(unnamed space)"))

(defun elark-wiki--space-description (space)
  "Extract the description from SPACE."
  (or (alist-get 'description space) ""))

(defun elark-wiki--space-type (space)
  "Extract the space type (team/person) from SPACE."
  (or (alist-get 'space_type space) ""))

;;;; Node field extractors

(defun elark-wiki--node-token (node)
  "Extract the wiki node_token from NODE."
  (or (alist-get 'node_token node) ""))

(defun elark-wiki--obj-token (node)
  "Extract the backing object token from NODE."
  (or (alist-get 'obj_token node) ""))

(defun elark-wiki--obj-type (node)
  "Extract the backing object type from NODE."
  (or (alist-get 'obj_type node) ""))

(defun elark-wiki--node-title (node)
  "Extract the title from NODE."
  (let ((title (alist-get 'title node)))
    (if (and title (stringp title) (not (string-empty-p title)))
        title
      "(untitled)")))

(defun elark-wiki--node-space-id (node)
  "Extract the space ID from NODE."
  (or (alist-get 'space_id node) ""))

(defun elark-wiki--node-parent-token (node)
  "Extract the parent node token from NODE."
  (or (alist-get 'parent_node_token node) ""))

(defun elark-wiki--node-type (node)
  "Extract the node type (origin/shortcut) from NODE."
  (or (alist-get 'node_type node) "origin"))

(defun elark-wiki--node-url (node)
  "Extract the node URL from NODE, or empty string."
  (or (alist-get 'url node) ""))

(defun elark-wiki--has-child-p (node)
  "Return non-nil when NODE has child nodes."
  (let ((hc (alist-get 'has_child node)))
    (and hc (not (eq hc :false)) (not (eq hc :json-false)))))

(defun elark-wiki--node-time (node key)
  "Extract and format timestamp KEY from NODE."
  (let ((ts (alist-get key node)))
    (cond
     ((and (stringp ts) (not (string-empty-p ts)))
      (or (elark--format-timestamp ts) ts))
     ((numberp ts) (or (elark--format-timestamp ts) ""))
     (t ""))))

;;;; Response extraction

(defun elark-wiki--extract-spaces (data)
  "Extract the space list from a `+space-list' response DATA.
The CLI returns the list under `data.spaces' (both single-page and
`--page-all' aggregated)."
  (or (elark--get-nested data 'data 'spaces)
      (alist-get 'spaces data)
      ;; Defensive fallbacks for alternate shapes.
      (elark--get-nested data 'data 'items)
      (alist-get 'items data)))

(defun elark-wiki--extract-nodes (data)
  "Extract the node list from a `+node-list' response DATA.
The CLI returns the list under `data.nodes'."
  (or (elark--get-nested data 'data 'nodes)
      (alist-get 'nodes data)
      ;; Defensive fallbacks for alternate shapes.
      (elark--get-nested data 'data 'items)
      (alist-get 'items data)))

;;;; Spaces list mode

(defvar elark-wiki-spaces-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'elark-wiki-space-open)
    (define-key map (kbd "g")   #'elark-wiki-spaces-refresh)
    (define-key map (kbd "w")   #'elark-wiki-copy-space-id)
    (define-key map (kbd "n")   #'elark-wiki--next-space)
    (define-key map (kbd "p")   #'elark-wiki--prev-space)
    (define-key map (kbd "?")   #'elark-wiki-dispatch)
    map)
  "Keymap for `elark-wiki-spaces-mode'.")

(define-derived-mode elark-wiki-spaces-mode special-mode
  "Lark Wiki Spaces"
  "Major mode for browsing Lark wiki spaces.

\\{elark-wiki-spaces-mode-map}")

(defun elark-wiki--spaces-ai-context ()
  "Return the AI context plist for a wiki spaces buffer."
  (let ((spaces elark-wiki--spaces)
        (space-id (get-text-property (point) 'elark-wiki-space-id)))
    (list :domain "wiki"
          :buffer-type "space-list"
          :item (when space-id (list :space-id space-id))
          :summary (format "Wiki space list, %d space(s)" (length spaces)))))

(put 'elark-wiki-spaces-mode 'elark-ai-context-provider
     #'elark-wiki--spaces-ai-context)

;;;; Node browser mode

(defvar elark-wiki-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'elark-wiki-open)
    (define-key map (kbd "^")   #'elark-wiki-up)
    (define-key map (kbd "g")   #'elark-wiki-refresh)
    (define-key map (kbd "v")   #'elark-wiki-view-content)
    (define-key map (kbd "d")   #'elark-wiki-node-detail)
    (define-key map (kbd "+")   #'elark-wiki-node-create)
    (define-key map (kbd "C")   #'elark-wiki-node-copy)
    (define-key map (kbd "R")   #'elark-wiki-move)
    (define-key map (kbd "D")   #'elark-wiki-node-delete)
    (define-key map (kbd "w")   #'elark-wiki-copy-token)
    (define-key map (kbd "W")   #'elark-wiki-copy-url)
    (define-key map (kbd "n")   #'elark-wiki--next-node)
    (define-key map (kbd "p")   #'elark-wiki--prev-node)
    (define-key map (kbd "?")   #'elark-wiki-dispatch)
    map)
  "Keymap for `elark-wiki-mode'.")

(define-derived-mode elark-wiki-mode special-mode
  "Lark Wiki"
  "Major mode for browsing a Lark wiki space's node tree.

\\{elark-wiki-mode-map}")

(defun elark-wiki--ai-context ()
  "Return the AI context plist for a wiki node-browser buffer."
  (let ((nodes elark-wiki--nodes)
        (node-token (get-text-property (point) 'elark-wiki-node-token)))
    (list :domain "wiki"
          :buffer-type "node-list"
          :item (when node-token (list :node-token node-token
                                       :space-id elark-wiki--space-id))
          :summary (format "Wiki space %s%s, %d node(s)"
                           (or elark-wiki--space-name elark-wiki--space-id "?")
                           (if elark-wiki--parent-token
                               (format " / %s" (or elark-wiki--level-name "subtree"))
                             "")
                           (length nodes)))))

(put 'elark-wiki-mode 'elark-ai-context-provider #'elark-wiki--ai-context)

;;;; Navigation helpers

(defun elark-wiki--space-at-point ()
  "Return the space alist at point, or nil."
  (get-text-property (point) 'elark-wiki-space))

(defun elark-wiki--node-at-point ()
  "Return the node alist at point, or nil."
  (get-text-property (point) 'elark-wiki-node))

(defun elark-wiki--next-space ()
  "Move to the next space entry."
  (interactive)
  (elark-ui-next-section 'elark-wiki-space-id))

(defun elark-wiki--prev-space ()
  "Move to the previous space entry."
  (interactive)
  (elark-ui-prev-section 'elark-wiki-space-id))

(defun elark-wiki--next-node ()
  "Move to the next node entry."
  (interactive)
  (elark-ui-next-section 'elark-wiki-node-token))

(defun elark-wiki--prev-node ()
  "Move to the previous node entry."
  (interactive)
  (elark-ui-prev-section 'elark-wiki-node-token))

;;;; Spaces: list / open
;; CLI: wiki +space-list

;;;###autoload
(defun elark-wiki-spaces ()
  "List the wiki spaces accessible to the current user.
The personal document library (my_library) is prepended, since the
API never returns it in the space list."
  (interactive)
  (message "Lark: listing wiki spaces...")
  (elark--run-command
   (list "wiki" "+space-list" "--as" "user" "--page-all")
   (lambda (data)
     (elark-wiki--render-spaces (elark-wiki--extract-spaces data)))
   nil :no-error t))

(defun elark-wiki-spaces-refresh ()
  "Refresh the wiki spaces list."
  (interactive)
  (elark-wiki-spaces))

(defun elark-wiki--my-library-space ()
  "Return a synthetic space entry for the personal document library."
  '((space_id . "my_library")
    (name . "My Document Library")
    (space_type . "person")
    (description . "Personal wiki library")))

(defun elark-wiki--render-spaces (spaces)
  "Render SPACES into the wiki spaces buffer."
  (let ((buf (get-buffer-create "*Lark Wiki Spaces*"))
        (all (cons (elark-wiki--my-library-space) (or spaces '()))))
    (with-current-buffer buf
      (elark-wiki-spaces-mode)
      (setq elark-wiki--spaces all)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize "Wiki Spaces" 'face 'bold) "\n"
                (elark-ui-separator 60) "\n\n")
        (dolist (space all)
          (elark-wiki--insert-space space)))
      (goto-char (point-min))
      (elark-ui-next-section 'elark-wiki-space-id)
      (setq header-line-format
            (format " Lark Wiki — %d space(s)" (length all))))
    (pop-to-buffer buf)))

(defun elark-wiki--insert-space (space)
  "Insert a one-line entry for SPACE."
  (let ((id (elark-wiki--space-id-of space))
        (name (elark-wiki--space-name space))
        (type (elark-wiki--space-type space))
        (desc (elark-wiki--space-description space))
        (beg (point)))
    (insert "  "
            (propertize (format "%-8s" (if (string-empty-p type) "space" type))
                        'face 'font-lock-type-face)
            "  "
            (propertize name 'face 'bold))
    (unless (string-empty-p desc)
      (insert (propertize (format "  — %s" desc) 'face 'font-lock-comment-face)))
    (insert "\n")
    (put-text-property beg (point) 'elark-wiki-space-id id)
    (put-text-property beg (point) 'elark-wiki-space space)))

(defun elark-wiki-space-open ()
  "Open the wiki space at point, listing its root nodes."
  (interactive)
  (let ((space (elark-wiki--space-at-point)))
    (unless space (user-error "No space at point"))
    (elark-wiki-nodes (elark-wiki--space-id-of space)
                     (elark-wiki--space-name space))))

;;;; Nodes: list / navigate
;; CLI: wiki +node-list --space-id X [--parent-node-token Y]

;;;###autoload
(defun elark-wiki-nodes (space-id &optional space-name parent-token level-name reset-history)
  "List wiki nodes in SPACE-ID, optionally under PARENT-TOKEN.
SPACE-NAME and LEVEL-NAME are display labels.  When RESET-HISTORY is
non-nil, the breadcrumb history is cleared (a fresh space entry)."
  (interactive "sSpace ID (or my_library): ")
  (when (string-empty-p space-id)
    (user-error "Space ID is required"))
  (message "Lark: listing wiki nodes...")
  (let ((args (list "wiki" "+node-list" "--as" "user"
                    "--space-id" space-id "--page-all")))
    (when (and parent-token (not (string-empty-p parent-token)))
      (setq args (append args (list "--parent-node-token" parent-token))))
    (let ((history (unless reset-history
                     (when (get-buffer "*Lark Wiki*")
                       (buffer-local-value 'elark-wiki--history
                                           (get-buffer "*Lark Wiki*"))))))
      (elark--run-command
       args
       (lambda (data)
         (elark-wiki--render-nodes (elark-wiki--extract-nodes data)
                                  space-id space-name parent-token level-name
                                  (if reset-history nil history)))
       nil :no-error t))))

(defun elark-wiki--render-nodes (nodes space-id space-name parent-token level-name history)
  "Render NODES for SPACE-ID under PARENT-TOKEN into the wiki buffer.
SPACE-NAME, LEVEL-NAME label the header; HISTORY is the breadcrumb stack."
  (let ((buf (get-buffer-create "*Lark Wiki*")))
    (with-current-buffer buf
      (elark-wiki-mode)
      (setq elark-wiki--nodes nodes
            elark-wiki--space-id space-id
            elark-wiki--space-name (or space-name space-id)
            elark-wiki--parent-token parent-token
            elark-wiki--level-name level-name
            elark-wiki--history history)
      (let ((inhibit-read-only t))
        (erase-buffer)
        ;; Breadcrumb header
        (insert (propertize (format "  %s" (or space-name space-id)) 'face 'bold))
        (when (and parent-token (not (string-empty-p parent-token)))
          (insert (propertize (format "  / %s" (or level-name "subtree"))
                              'face 'font-lock-function-name-face)))
        (insert "\n" (elark-ui-separator 60) "\n")
        ;; ".." up-entry when not at the space root
        (when (and parent-token (not (string-empty-p parent-token)))
          (let ((beg (point)))
            (insert "  "
                    (propertize "up      " 'face 'font-lock-type-face)
                    "  "
                    (propertize ".." 'face 'bold)
                    "\n")
            (put-text-property beg (point) 'elark-wiki-node
                               '((node_token . "..") (obj_type . "parent")))
            (put-text-property beg (point) 'elark-wiki-node-token "..")))
        (if (null nodes)
            (insert "  (no nodes)\n")
          (dolist (node nodes)
            (elark-wiki--insert-node node))))
      (goto-char (point-min))
      (elark-ui-next-section 'elark-wiki-node-token)
      (setq header-line-format
            (format " Lark Wiki: %s — %d node(s)"
                    (or space-name space-id) (length nodes))))
    (pop-to-buffer buf)))

(defun elark-wiki--insert-node (node)
  "Insert a one-line entry for NODE."
  (let* ((token (elark-wiki--node-token node))
         (title (elark-wiki--node-title node))
         (obj-type (elark-wiki--obj-type node))
         (has-child (elark-wiki--has-child-p node))
         (shortcut (equal (elark-wiki--node-type node) "shortcut"))
         (beg (point)))
    (insert "  "
            (propertize (format "%-8s" (if (string-empty-p obj-type) "node" obj-type))
                        'face 'font-lock-type-face)
            "  "
            (propertize title 'face (if has-child 'bold 'default)))
    (when has-child
      (insert (propertize " ▸" 'face 'font-lock-keyword-face)))
    (when shortcut
      (insert (propertize " ↗" 'face 'font-lock-comment-face)))
    (insert "\n")
    (put-text-property beg (point) 'elark-wiki-node-token token)
    (put-text-property beg (point) 'elark-wiki-node node)))

(defun elark-wiki-open ()
  "Open the node at point.
Nodes with children are descended into; leaf nodes hand off to the
backing document viewer (see `elark-wiki-view-content')."
  (interactive)
  (let ((node (elark-wiki--node-at-point)))
    (unless node (user-error "No node at point"))
    (cond
     ;; ".." up-entry
     ((equal (elark-wiki--obj-type node) "parent")
      (elark-wiki-up))
     ;; Has children — descend, pushing the current level onto history
     ((elark-wiki--has-child-p node)
      (push (cons elark-wiki--parent-token elark-wiki--level-name)
            elark-wiki--history)
      (let ((hist elark-wiki--history)
            (sid elark-wiki--space-id)
            (sname elark-wiki--space-name))
        (elark-wiki-nodes sid sname
                         (elark-wiki--node-token node)
                         (elark-wiki--node-title node))
        ;; Preserve the pushed history across the async re-render.
        (with-current-buffer (get-buffer "*Lark Wiki*")
          (setq elark-wiki--history hist))))
     ;; Leaf — view the backing object
     (t (elark-wiki--view-node node)))))

(defun elark-wiki-up ()
  "Go up one level in the node tree."
  (interactive)
  (if elark-wiki--history
      (let ((prev (pop elark-wiki--history))
            (hist elark-wiki--history)
            (sid elark-wiki--space-id)
            (sname elark-wiki--space-name))
        (elark-wiki-nodes sid sname (car prev) (cdr prev))
        (with-current-buffer (get-buffer "*Lark Wiki*")
          (setq elark-wiki--history hist)))
    ;; At the space root — go back to the space list.
    (elark-wiki-spaces)))

(defun elark-wiki-refresh ()
  "Refresh the current node level."
  (interactive)
  (unless elark-wiki--space-id
    (user-error "Not in a wiki node buffer"))
  (let ((sid elark-wiki--space-id)
        (sname elark-wiki--space-name)
        (ptok elark-wiki--parent-token)
        (lname elark-wiki--level-name)
        (hist elark-wiki--history))
    (elark-wiki-nodes sid sname ptok lname)
    (with-current-buffer (get-buffer "*Lark Wiki*")
      (setq elark-wiki--history hist))))

;;;; View node content (leaf handoff)

(defun elark-wiki--view-node (node)
  "Hand off NODE's backing object to the appropriate content viewer."
  (let* ((obj-type (elark-wiki--obj-type node))
         (obj-token (elark-wiki--obj-token node))
         (url (elark-wiki--node-url node))
         (ref (if (and url (not (string-empty-p url))) url obj-token)))
    (when (string-empty-p ref)
      (user-error "No object token or URL for this node"))
    (pcase obj-type
      ((or "doc" "docx")
       (elark-docs-fetch ref))
      ("sheet"
       (elark-sheets-info ref))
      (_
       (message "Lark: node type \"%s\" is not viewable; showing details" obj-type)
       (elark-wiki--show-node-detail node)))))

(defun elark-wiki-view-content ()
  "View the backing document of the node at point."
  (interactive)
  (let ((node (elark-wiki--node-at-point)))
    (unless node (user-error "No node at point"))
    (when (equal (elark-wiki--obj-type node) "parent")
      (user-error "Not a content node"))
    (elark-wiki--view-node node)))

;;;; Node detail
;; CLI: wiki +node-get --node-token X

(defvar elark-wiki-detail-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'elark-wiki-detail-view-content)
    (define-key map (kbd "w")   #'elark-wiki-copy-token)
    (define-key map (kbd "?")   #'elark-wiki-dispatch)
    map)
  "Keymap for `elark-wiki-detail-mode'.")

(define-derived-mode elark-wiki-detail-mode special-mode
  "Lark Wiki Node"
  "Major mode for viewing a Lark wiki node's details.

\\{elark-wiki-detail-mode-map}")

(defun elark-wiki--detail-ai-context ()
  "Return the AI context plist for a wiki node detail buffer."
  (let* ((node elark-wiki--detail-node)
         (token (and node (elark-wiki--node-token node)))
         (title (and node (elark-wiki--node-title node))))
    (list :domain "wiki"
          :buffer-type "node-detail"
          :item (list :node-token token
                      :obj-token (and node (elark-wiki--obj-token node))
                      :obj-type (and node (elark-wiki--obj-type node)))
          :summary (format "Wiki node detail: %s (%s)" title token))))

(put 'elark-wiki-detail-mode 'elark-ai-context-provider
     #'elark-wiki--detail-ai-context)

(defun elark-wiki-node-detail ()
  "Show full details for the node at point.
Renders the node alist already obtained from `+node-list', which
carries every field the API exposes for a node.  (The `+node-get'
shortcut is not used: its `--node-token' heuristic only recognises
`wik'-prefixed tokens and misresolves the node tokens that
`+node-list' returns.)"
  (interactive)
  (let ((node (elark-wiki--node-at-point)))
    (unless node (user-error "No node at point"))
    (when (equal (elark-wiki--obj-type node) "parent")
      (user-error "Not a content node"))
    (elark-wiki--show-node-detail node)))

(defun elark-wiki--detail-field (label value)
  "Insert LABEL: VALUE if VALUE is non-empty."
  (elark-ui-insert-field label value 14))

(defun elark-wiki--show-node-detail (node)
  "Display NODE details in a detail buffer."
  (let* ((title (elark-wiki--node-title node))
         (obj-type (elark-wiki--obj-type node))
         (viewable (member obj-type '("doc" "docx" "sheet")))
         (buf (get-buffer-create (format "*Lark Wiki Node: %s*" title))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (elark-ui-insert-title title)
        (elark-wiki--detail-field "Node token" (elark-wiki--node-token node))
        (elark-wiki--detail-field "Object token" (elark-wiki--obj-token node))
        (elark-wiki--detail-field "Object type" obj-type)
        (elark-wiki--detail-field "Node type" (elark-wiki--node-type node))
        (elark-wiki--detail-field "Space ID" (elark-wiki--node-space-id node))
        (elark-wiki--detail-field "Parent" (elark-wiki--node-parent-token node))
        (elark-wiki--detail-field "Created" (elark-wiki--node-time node 'obj_create_time))
        (elark-wiki--detail-field "Edited" (elark-wiki--node-time node 'obj_edit_time))
        (elark-wiki--detail-field "URL" (elark-wiki--node-url node))
        (when viewable
          (insert "\n" (propertize "Press RET to view content"
                                   'face 'font-lock-comment-face) "\n")))
      (elark-wiki-detail-mode)
      (setq elark-wiki--detail-node node)
      (goto-char (point-min)))
    (pop-to-buffer buf)))

(defun elark-wiki-detail-view-content ()
  "View the backing document of the node in this detail buffer."
  (interactive)
  (unless elark-wiki--detail-node
    (user-error "No node in this buffer"))
  (elark-wiki--view-node elark-wiki--detail-node))

;;;; Create node
;; CLI: wiki +node-create --space-id X [--parent-node-token Y] --title T --obj-type Z

(defun elark-wiki-node-create (title obj-type)
  "Create a new wiki node titled TITLE of OBJ-TYPE at the current level.
Created in the current buffer's space, under the current parent node."
  (interactive
   (list (read-string "New node title: ")
         (completing-read "Object type: "
                          '("docx" "sheet" "bitable" "mindnote" "slides")
                          nil t nil nil elark-wiki-default-obj-type)))
  (unless elark-wiki--space-id
    (user-error "Not in a wiki node buffer"))
  (when (string-empty-p title)
    (user-error "Title is required"))
  (message "Lark: creating wiki node...")
  (let ((args (list "wiki" "+node-create" "--as" "user"
                    "--space-id" elark-wiki--space-id
                    "--title" title
                    "--obj-type" obj-type)))
    (when (and elark-wiki--parent-token
               (not (string-empty-p elark-wiki--parent-token)))
      (setq args (append args (list "--parent-node-token"
                                    elark-wiki--parent-token))))
    (elark--run-command
     args
     (lambda (_data)
       (message "Lark: node \"%s\" created" title)
       (elark-wiki-refresh)))))

;;;; Delete node
;; CLI: wiki +node-delete --node-token X --yes

(defun elark-wiki-node-delete ()
  "Delete the wiki node at point after confirmation."
  (interactive)
  (let ((node (elark-wiki--node-at-point)))
    (unless node (user-error "No node at point"))
    (when (equal (elark-wiki--obj-type node) "parent")
      (user-error "Not a deletable node"))
    (let ((token (elark-wiki--node-token node))
          (title (elark-wiki--node-title node)))
      (unless (yes-or-no-p
               (format "Delete wiki node \"%s\" and its subtree? " title))
        (user-error "Aborted"))
      (message "Lark: deleting wiki node...")
      (elark--run-command
       ;; --obj-type wiki declares that --node-token is a wiki node
       ;; token; the CLI requires it because these tokens are not
       ;; `wik'-prefixed and would otherwise be read as obj_tokens.
       (list "wiki" "+node-delete" "--as" "user"
             "--node-token" token
             "--space-id" elark-wiki--space-id
             "--obj-type" "wiki"
             "--yes")
       (lambda (_data)
         (message "Lark: node \"%s\" deleted" title)
         (elark-wiki-refresh))
       nil :no-error t))))

;;;; Copy node
;; CLI: wiki +node-copy --node-token X --space-id S (--target-space-id | --target-parent-node-token)

(defun elark-wiki-node-copy (target-parent new-title)
  "Copy the node at point under TARGET-PARENT, optionally renaming to NEW-TITLE.
TARGET-PARENT is a node token in the same space; empty copies to the
space root."
  (interactive
   (list (read-string "Target parent node token (empty = space root): ")
         (read-string "New title (empty = keep original): ")))
  (let ((node (elark-wiki--node-at-point)))
    (unless node (user-error "No node at point"))
    (when (equal (elark-wiki--obj-type node) "parent")
      (user-error "Not a copyable node"))
    (let ((args (list "wiki" "+node-copy" "--as" "user"
                      "--node-token" (elark-wiki--node-token node)
                      "--space-id" elark-wiki--space-id
                      "--yes")))
      (if (and target-parent (not (string-empty-p target-parent)))
          (setq args (append args (list "--target-parent-node-token" target-parent)))
        (setq args (append args (list "--target-space-id" elark-wiki--space-id))))
      (when (and new-title (not (string-empty-p new-title)))
        (setq args (append args (list "--title" new-title))))
      (message "Lark: copying wiki node...")
      (elark--run-command
       args
       (lambda (_data)
         (message "Lark: node copied")
         (elark-wiki-refresh))
       nil :no-error t))))

;;;; Move node
;; CLI: wiki +move --node-token X [--source-space-id S] (--target-space-id | --target-parent-token)

(defun elark-wiki-move (target-parent target-space)
  "Move the node at point under TARGET-PARENT, or into TARGET-SPACE root.
Provide TARGET-PARENT (a node token) or TARGET-SPACE (a space ID).
If both are empty, the move is aborted."
  (interactive
   (list (read-string "Target parent node token (empty to use space): ")
         (read-string "Target space ID (empty = current space): ")))
  (let ((node (elark-wiki--node-at-point)))
    (unless node (user-error "No node at point"))
    (when (equal (elark-wiki--obj-type node) "parent")
      (user-error "Not a movable node"))
    (when (and (string-empty-p target-parent) (string-empty-p target-space))
      (setq target-space elark-wiki--space-id))
    (let ((args (list "wiki" "+move" "--as" "user"
                      "--node-token" (elark-wiki--node-token node)
                      "--source-space-id" elark-wiki--space-id)))
      (when (and target-space (not (string-empty-p target-space)))
        (setq args (append args (list "--target-space-id" target-space))))
      (when (and target-parent (not (string-empty-p target-parent)))
        (setq args (append args (list "--target-parent-token" target-parent))))
      (message "Lark: moving wiki node...")
      (elark--run-command
       args
       (lambda (_data)
         (message "Lark: node moved")
         (elark-wiki-refresh))
       nil :no-error t))))

;;;; Copy helpers

(defun elark-wiki-copy-token ()
  "Copy the node token at point (or in a detail buffer) to the kill ring."
  (interactive)
  (let ((token (or (get-text-property (point) 'elark-wiki-node-token)
                   (and elark-wiki--detail-node
                        (elark-wiki--node-token elark-wiki--detail-node)))))
    (when (or (null token) (string-empty-p token) (equal token ".."))
      (user-error "No node at point"))
    (kill-new token)
    (message "Copied token: %s" token)))

(defun elark-wiki-copy-url ()
  "Copy the node URL at point to the kill ring."
  (interactive)
  (let ((node (elark-wiki--node-at-point)))
    (unless node (user-error "No node at point"))
    (let ((url (elark-wiki--node-url node)))
      (if (string-empty-p url)
          (user-error "No URL for this node (use w to copy the token)")
        (kill-new url)
        (message "Copied URL: %s" url)))))

(defun elark-wiki-copy-space-id ()
  "Copy the space ID at point to the kill ring."
  (interactive)
  (let ((id (get-text-property (point) 'elark-wiki-space-id)))
    (when (or (null id) (string-empty-p id))
      (user-error "No space at point"))
    (kill-new id)
    (message "Copied space ID: %s" id)))

;;;; Transient dispatch

;;;###autoload (autoload 'elark-wiki-dispatch "elark-wiki" nil t)
(transient-define-prefix elark-wiki-dispatch ()
  "Lark Wiki commands."
  ["Browse"
   ("l" "List spaces"   elark-wiki-spaces)
   ("g" "Refresh"       elark-wiki-refresh)
   ("^" "Up"            elark-wiki-up)
   ("v" "View content"  elark-wiki-view-content)]
  ["At Point"
   ("RET" "Open node"   elark-wiki-open)
   ("d"   "Node detail" elark-wiki-node-detail)
   ("w"   "Copy token"  elark-wiki-copy-token)
   ("W"   "Copy URL"    elark-wiki-copy-url)]
  ["Write"
   ("+" "Create node"   elark-wiki-node-create)
   ("C" "Copy node"     elark-wiki-node-copy)
   ("R" "Move node"     elark-wiki-move)
   ("D" "Delete node"   elark-wiki-node-delete)])

(provide 'elark-wiki)
;;; elark-wiki.el ends here
