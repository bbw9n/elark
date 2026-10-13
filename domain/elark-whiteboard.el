;;; elark-whiteboard.el --- Lark Whiteboard integration -*- lexical-binding: t; -*-

;; Copyright (C) 2026 bbw9n

;; Author: bbw9n <bbw9nio@gmail.com>
;; Assisted-by: Claude:claude-opus-5
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Provides Whiteboard domain commands for elark: querying whiteboard
;; content (as image, code, or raw nodes) and updating whiteboards with
;; mermaid, plantuml, or raw DSL source.
;;
;; CLI command mapping:
;;   Query:  whiteboard +query --whiteboard-token X [--output_as image|code|raw]
;;                             [--output X] [--overwrite]
;;   Update: whiteboard +update --whiteboard-token X --source X
;;                              [--input_format raw|plantuml|mermaid]
;;                              [--overwrite] [--yes]

;;; Code:

(require 'elark-core)
(require 'elark-ui)
(require 'transient)

;;;; Customization

(defgroup elark-whiteboard nil
  "Lark Whiteboard settings."
  :group 'elark
  :prefix "elark-whiteboard-")

;;;; Query
;; CLI: whiteboard +query --whiteboard-token X [--output_as image|code|raw]
;;                        [--output X] [--overwrite]

;;;###autoload
(defun elark-whiteboard-query (token &optional output-as output-dir)
  "Query whiteboard by TOKEN.
OUTPUT-AS is \"image\", \"code\", or \"raw\" (default \"raw\").
OUTPUT-DIR is the directory to save image output."
  (interactive
   (list (read-string "Whiteboard token: ")
         (completing-read "Output as: " '("raw" "code" "image") nil t nil nil "raw")
         nil))
  (when (string-empty-p token)
    (user-error "Whiteboard token is required"))
  (let* ((output-as (or output-as "raw"))
         (need-dir (equal output-as "image")))
    (when (and need-dir (not output-dir))
      (setq output-dir (read-directory-name "Save image to: ")))
    (message "Lark: querying whiteboard...")
    (let ((args (list "whiteboard" "+query"
                      "--whiteboard-token" token
                      "--output_as" output-as)))
      (when (and output-dir (not (string-empty-p output-dir)))
        (setq args (append args (list "--output" "." "--overwrite"))))
      (if (equal output-as "image")
          ;; Run from the target directory so the relative "--output ."
          ;; lands in the right place.
          (let ((default-directory (or (and output-dir
                                           (file-name-as-directory
                                            (expand-file-name output-dir)))
                                      default-directory)))
            (elark--run-command
             args
             (lambda (data)
               (let ((file (or (alist-get 'path data)
                               (alist-get 'output data)
                               (elark--get-nested data 'data 'path))))
                 (if file
                     (let ((full (expand-file-name file default-directory)))
                       (message "Lark: whiteboard image saved to %s" full)
                       (find-file full))
                   (message "Lark: whiteboard image exported"))))))
        (elark--run-command
         args
         (lambda (data)
           (elark-whiteboard--display-query data token output-as)))))))

(defun elark-whiteboard--display-query (data token output-as)
  "Display whiteboard query DATA for TOKEN in OUTPUT-AS format."
  (let* ((content (or (alist-get 'data data)
                      (alist-get 'content data)
                      (alist-get 'nodes data)
                      data))
         (buf-name (format "*Lark Whiteboard: %s*" token))
         (buf (get-buffer-create buf-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize (format "Whiteboard: %s" token) 'face 'bold) "\n"
                (propertize (format "Format: %s" output-as) 'face 'font-lock-comment-face) "\n"
                (elark-ui-separator 60) "\n\n")
        (cond
         ((null content)
          (insert "(no content)\n"))
         ((stringp content)
          (insert content "\n"))
         ((listp content)
          (insert (pp-to-string content) "\n"))
         (t
          (insert "(no content)\n"))))
      (special-mode)
      (goto-char (point-min))
      (setq header-line-format (format " Whiteboard: %s [%s]" token output-as)))
    (pop-to-buffer buf)))

;;;; Query shortcuts

;;;###autoload
(defun elark-whiteboard-query-raw (token)
  "Query whiteboard TOKEN as raw node structure."
  (interactive "sWhiteboard token: ")
  (elark-whiteboard-query token "raw"))

;;;###autoload
(defun elark-whiteboard-query-code (token)
  "Query whiteboard TOKEN as code (mermaid/plantuml)."
  (interactive "sWhiteboard token: ")
  (elark-whiteboard-query token "code"))

;;;###autoload
(defun elark-whiteboard-query-image (token)
  "Query whiteboard TOKEN and export as image."
  (interactive "sWhiteboard token: ")
  (elark-whiteboard-query token "image"))

;;;; Update
;; CLI: whiteboard +update --whiteboard-token X --source X
;;                         [--input_format raw|plantuml|mermaid]
;;                         [--overwrite] [--yes]

;;;###autoload (autoload 'elark-whiteboard-update "elark-whiteboard" nil t)
(transient-define-prefix elark-whiteboard-update ()
  "Update a Lark whiteboard."
  ["Whiteboard"
   ("w" "Token"        "--whiteboard-token=" :prompt "Whiteboard token: ")
   ("s" "Source"       "--source=" :prompt "Source data (or @file): ")
   ("f" "Input format" "--input_format=" :prompt "Format: "
    :choices ("raw" "plantuml" "mermaid"))
   ("o" "Overwrite"    "--overwrite")
   ("y" "Confirm"      "--yes")]
  ["Actions"
   ("RET" "Update" elark-whiteboard--do-update)
   ("q"   "Cancel" transient-quit-all)])

(defun elark-whiteboard--do-update (&rest _args)
  "Execute whiteboard update with transient arguments."
  (interactive)
  (let ((args (transient-args 'elark-whiteboard-update)))
    (unless args
      (user-error "No whiteboard details provided"))
    (message "Lark: updating whiteboard...")
    (elark--run-command
     (append '("whiteboard" "+update") args)
     (lambda (_data)
       (message "Lark: whiteboard updated")))))

;;;; Update from buffer

;;;###autoload
(defun elark-whiteboard-update-from-buffer (token input-format &optional overwrite)
  "Update whiteboard TOKEN with the current buffer content.
INPUT-FORMAT is \"raw\", \"plantuml\", or \"mermaid\".
If OVERWRITE is non-nil, replace all existing content."
  (interactive
   (list (read-string "Whiteboard token: ")
         (completing-read "Input format: " '("raw" "plantuml" "mermaid") nil t)
         (y-or-n-p "Overwrite existing content? ")))
  (when (string-empty-p token)
    (user-error "Whiteboard token is required"))
  (let ((source (buffer-substring-no-properties (point-min) (point-max))))
    (when (string-empty-p source)
      (user-error "Buffer is empty"))
    (message "Lark: updating whiteboard from buffer...")
    (let ((args (list "whiteboard" "+update"
                      "--whiteboard-token" token
                      "--source" source
                      "--input_format" input-format
                      "--yes")))
      (when overwrite
        (setq args (append args '("--overwrite"))))
      (elark--run-command
       args
       (lambda (_data)
         (message "Lark: whiteboard updated from buffer"))))))

;;;; Transient dispatch

;;;###autoload (autoload 'elark-whiteboard-dispatch "elark-whiteboard" nil t)
(transient-define-prefix elark-whiteboard-dispatch ()
  "Lark Whiteboard commands."
  ["Query"
   ("q" "Query"          elark-whiteboard-query)
   ("r" "Query raw"      elark-whiteboard-query-raw)
   ("c" "Query code"     elark-whiteboard-query-code)
   ("i" "Query image"    elark-whiteboard-query-image)]
  ["Update"
   ("u" "Update"                   elark-whiteboard-update)
   ("b" "Update from buffer"       elark-whiteboard-update-from-buffer)])

(provide 'elark-whiteboard)
;;; elark-whiteboard.el ends here
