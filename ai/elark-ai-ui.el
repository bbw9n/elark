;;; elark-ai-ui.el --- Fragment-based UI for the Lark AI buffer -*- lexical-binding: t; -*-

;; Copyright (C) 2026 bbw9n

;; Author: bbw9n <bbw9nio@gmail.com>
;; Assisted-by: Claude:claude-opus-5
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Provides a fragment-based UI model for the *Lark AI* buffer.
;; Inspired by agent-shell-ui.el, each section (prompt, skills, log,
;; plan, output) is a named fragment that can be inserted, updated,
;; or appended to independently — no full-buffer rerenders.
;;
;; Key concepts:
;;   - Fragment: a named region in the buffer with a type and body
;;   - Each fragment is identified by a `elark-ai-ui-id' text property
;;   - Bodies live in a buffer-local hash table (not text properties)
;;   - Markdown highlighting is applied incrementally on append

;;; Code:

(require 'cl-lib)
(require 'text-property-search) ; text-property-search-{forward,backward}
(require 'elark-core)             ; elark--log
(require 'elark-ui)               ; elark-ui-fontify-block + elark-ui-block-bg-face

;;;; Fragment data model

(defvar-local elark-ai-ui--content-store nil
  "Hash table mapping fragment-id to body string.")

(defun elark-ai-ui--ensure-store ()
  "Ensure the content store exists."
  (unless elark-ai-ui--content-store
    (setq elark-ai-ui--content-store
          (make-hash-table :test 'equal))))

;;;; Fragment types
;;
;; Each fragment has:
;;   :id    — unique string identifier
;;   :type  — symbol: header, prompt, skills, log, plan, output
;;   :label — display label (optional)

;;;; Buffer setup

(defvar elark-ai-ui-mode-map (make-sparse-keymap)
  "Keymap for `elark-ai-ui-mode'.
Intentionally minimal: per-fragment behaviour (TAB → toggle, plan
keys, etc.) is attached as `keymap' text properties on the fragment
itself, so the input area at the bottom can stay free of those
bindings without needing to shadow them.")

(defvar elark-ai-ui-fragment-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") #'elark-ai-ui-toggle-section)
    (define-key map (kbd "n")   #'elark-ai-ui-next-tool-call)
    (define-key map (kbd "p")   #'elark-ai-ui-prev-tool-call)
    map)
  "Keymap attached to every fragment via the `keymap' text property.
By living on the fragment instead of the buffer mode-map, the keys
only fire when point is actually on a fragment — leaving the input
area below free for plain self-insertion.  TAB folds the body of the
section at point; n/p jump between tool-call cards.")

(define-derived-mode elark-ai-ui-mode fundamental-mode "Lark AI"
  "Major mode for the Lark AI fragment-based UI buffer.
Read-only protection is handled per-fragment via text properties,
not `buffer-read-only', so the input area remains editable."
  (setq-local elark-ai-ui--content-store
              (make-hash-table :test 'equal))
  (setq buffer-read-only nil)
  (setq truncate-lines nil)
  (setq word-wrap t))

;;;; Low-level fragment operations

(defun elark-ai-ui-find-fragment (id)
  "Find the start and end positions of fragment ID.
Returns (BEG . END) or nil."
  (save-excursion
    (goto-char (point-min))
    (let ((match (text-property-search-forward
                  'elark-ai-ui-id id #'equal)))
      (when match
        (let ((beg (prop-match-beginning match)))
          ;; Find end: next fragment or end of buffer.  The predicate
          ;; receives (VALUE PROP-AT-POS) — match on the position's
          ;; property, not the constant VALUE argument.
          (goto-char (prop-match-end match))
          (let ((next (text-property-search-forward
                       'elark-ai-ui-id nil
                       (lambda (_ val) (and val (not (equal val id)))))))
            (cons beg (if next
                         (prop-match-beginning next)
                       (point-max)))))))))

(defun elark-ai-ui-insert-fragment (id type label body)
  "Insert a new fragment at point.
ID is a unique string, TYPE is a symbol, LABEL is the
header text, BODY is the content string."
  (elark-ai-ui--ensure-store)
  (puthash id (or body "") elark-ai-ui--content-store)
  (let ((inhibit-read-only t)
        (frag-beg (point)))
    ;; Label line
    (when label
      (insert (propertize label
                          'face (elark-ai-ui--label-face type)
                          'elark-ai-ui-id id
                          'elark-ai-ui-type type
                          'elark-ai-ui-section 'label)
              "\n"))
    ;; Body
    (when (and body (not (string-empty-p body)))
      (let ((body-beg (point)))
        (elark-ai-ui--insert-body id type body)
        (put-text-property body-beg (point)
                           'elark-ai-ui-id id)
        (put-text-property body-beg (point)
                           'elark-ai-ui-section 'body)))
    ;; Spacing — carries the fragment id so even a label-less,
    ;; empty-body fragment (e.g. the log placeholder) is findable by
    ;; `elark-ai-ui-find-fragment' and terminates the previous
    ;; fragment's region instead of being absorbed into it.
    (insert (propertize "\n" 'elark-ai-ui-id id))
    ;; Mark entire fragment as read-only and attach the fragment
    ;; keymap (TAB → toggle).  Putting it as a text property here
    ;; keeps it off the buffer mode-map, so the input area at the
    ;; bottom doesn't need to shadow TAB.
    (put-text-property frag-beg (point) 'read-only t)
    (put-text-property frag-beg (point)
                       'rear-nonsticky '(read-only))
    (put-text-property frag-beg (point) 'keymap elark-ai-ui-fragment-map)))

(defun elark-ai-ui--insert-body (_id type body)
  "Insert BODY for fragment of TYPE with appropriate formatting."
  (pcase type
    ('output    (elark-ai-ui--insert-markdown body))
    ('plan      (insert body))
    ('log       (insert (propertize body 'face 'font-lock-comment-face)))
    ('tool-call (elark-ai-ui--insert-tool-call-body body))
    (_          (insert body))))

(defun elark-ai-ui--insert-tool-call-body (body)
  "Insert a tool-call BODY as a sh-fontified, block-bg-tinted pane.
BODY is the rendered shell command text.  The pane inherits the user's
theme via `elark-ui-block-bg-face' — so a code-block-like surface
appears even in a `fundamental-mode'-derived chat buffer — and
`sh-mode' supplies the syntax highlighting on top."
  (let ((start (point)))
    (insert body)
    (unless (eq (char-before) ?\n) (insert "\n"))
    (elark-ui-fontify-block 'sh-mode start (point))
    (font-lock-append-text-property
     start (point) 'font-lock-face (elark-ui-block-bg-face))))

(defun elark-ai-ui-update-fragment (id &optional label body)
  "Replace the content of fragment ID.
If LABEL is non-nil, update the label.  BODY replaces the body.
Must be called with a `elark-ai-ui-mode' buffer current; in any
other buffer this is a no-op — async callbacks can fire with an
arbitrary buffer current, and the insert-at-end fallback must
never write into a user's buffer."
  (if (not (derived-mode-p 'elark-ai-ui-mode))
      (elark--log "AI UI: dropped fragment %S update — current buffer %s is not an AI buffer"
                 id (buffer-name))
    (elark-ai-ui--ensure-store)
    (let ((region (elark-ai-ui-find-fragment id)))
      (if region
          (let ((inhibit-read-only t))
            (when body
              (puthash id body elark-ai-ui--content-store))
            (delete-region (car region) (cdr region))
            (goto-char (car region))
            (elark-ai-ui-insert-fragment
             id
             (or (get-text-property (car region) 'elark-ai-ui-type)
                 'default)
             label
             (or body (gethash id elark-ai-ui--content-store ""))))
        ;; Fragment doesn't exist yet — insert at end
        (goto-char (point-max))
        (when body
          (puthash id (or body "") elark-ai-ui--content-store))
        (elark-ai-ui-insert-fragment
         id 'default label
         (or body ""))))))

(defun elark-ai-ui-update-label (id label)
  "Replace just the LABEL line of fragment ID, preserving the body.
Unlike `elark-ai-ui-update-fragment', the body region is left untouched
— so a user-folded section stays folded when the caller updates only
the header (e.g. a tool-call status icon transitioning ⟳ → ✓).

Replaces only the label *text* and leaves the existing separator
\"\\n\" in place: that newline is the carrier of the body's folded
\(`invisible') state, so re-creating it would silently un-fold a card
the user had collapsed."
  (let ((region (elark-ai-ui-find-fragment id)))
    (when region
      (let* ((label-end
              (or (next-single-property-change
                   (car region) 'elark-ai-ui-section nil (cdr region))
                  (cdr region)))
             (type (get-text-property (car region) 'elark-ai-ui-type))
             (inhibit-read-only t))
        (save-excursion
          (delete-region (car region) label-end)
          (goto-char (car region))
          ;; No trailing "\n" — the original separator survives ahead.
          (insert (propertize label
                              'face (elark-ai-ui--label-face type)
                              'elark-ai-ui-id id
                              'elark-ai-ui-type type
                              'elark-ai-ui-section 'label))
          (let ((new-end (point)))
            (put-text-property (car region) new-end 'read-only t)
            (put-text-property (car region) new-end
                               'rear-nonsticky '(read-only))
            (put-text-property (car region) new-end
                               'keymap elark-ai-ui-fragment-map)))))))

(defun elark-ai-ui-append-fragment (id text)
  "Append TEXT to the body of fragment ID.
Applies incremental markdown highlighting to the new text."
  (elark-ai-ui--ensure-store)
  (let* ((old (gethash id elark-ai-ui--content-store ""))
         (new (concat old text)))
    (puthash id new elark-ai-ui--content-store)
    ;; Find the fragment's body end and append there
    (let ((region (elark-ai-ui-find-fragment id)))
      (when region
        (let ((inhibit-read-only t))
          (save-excursion
            (goto-char (cdr region))
            ;; Back up past the trailing newline
            (when (and (not (bobp))
                       (eq (char-before) ?\n))
              (backward-char))
            (let ((ins-start (point)))
              (insert (propertize text 'elark-ai-ui-id id
                                  'elark-ai-ui-section 'body))
              ;; Incremental markdown highlighting
              (elark-ai-ui--highlight-region ins-start (point))
              ;; Match other fragment insertions: appended text is
              ;; read-only too (with rear-nonsticky so typing right
              ;; after it is still allowed) and carries the fragment
              ;; keymap so TAB toggles consistently.
              (put-text-property ins-start (point) 'read-only t)
              (put-text-property ins-start (point)
                                 'rear-nonsticky '(read-only))
              (put-text-property ins-start (point)
                                 'keymap elark-ai-ui-fragment-map))))))))

(defun elark-ai-ui-clear ()
  "Clear all fragments from the buffer."
  (let ((inhibit-read-only t))
    (erase-buffer))
  (setq elark-ai-ui--content-store
        (make-hash-table :test 'equal)))

;;;; Collapse / expand

(defun elark-ai-ui-toggle-section ()
  "Toggle collapse of the fragment at point."
  (interactive)
  (let ((id (get-text-property (point) 'elark-ai-ui-id)))
    (when id
      (let ((region (elark-ai-ui-find-fragment id)))
        (when region
          (save-excursion
            (goto-char (car region))
            ;; Find where label ends and body begins
            (let ((label-end
                   (next-single-property-change
                    (car region) 'elark-ai-ui-section
                    nil (cdr region))))
              (when (and label-end (< label-end (cdr region)))
                (let ((inhibit-read-only t)
                      (currently-invisible
                       (get-text-property label-end 'invisible)))
                  (put-text-property label-end (cdr region)
                                     'invisible
                                     (not currently-invisible)))))))))))

;;;; Tool-call navigation

(defun elark-ai-ui-next-tool-call ()
  "Move point to the start of the next tool-call fragment.
Bound to n on every fragment so the user can walk through the agent's
actions like Org headings — independent of the surrounding section."
  (interactive)
  (let ((target nil))
    (save-excursion
      ;; If point is already inside a tool-call fragment, advance past
      ;; it first so we land on the *next* one rather than the same one.
      (when (eq (get-text-property (point) 'elark-ai-ui-type) 'tool-call)
        (when-let* ((next-change (next-single-property-change
                                 (point) 'elark-ai-ui-type)))
          (goto-char next-change)))
      (when-let* ((match (text-property-search-forward
                         'elark-ai-ui-type 'tool-call #'eq)))
        (setq target (prop-match-beginning match))))
    (if target
        (goto-char target)
      (user-error "No next tool-call"))))

(defun elark-ai-ui-prev-tool-call ()
  "Move point to the start of the previous tool-call fragment."
  (interactive)
  (let ((target nil))
    (save-excursion
      ;; If point is inside a tool-call fragment, step back to before
      ;; its start so we find the previous one.
      (when (eq (get-text-property (point) 'elark-ai-ui-type) 'tool-call)
        (when-let* ((prev-change (previous-single-property-change
                                 (point) 'elark-ai-ui-type)))
          (goto-char prev-change)))
      (when-let* ((match (text-property-search-backward
                         'elark-ai-ui-type 'tool-call #'eq)))
        (setq target (prop-match-beginning match))))
    (if target
        (goto-char target)
      (user-error "No previous tool-call"))))

;;;; Markdown highlighting

(defun elark-ai-ui--insert-markdown (text)
  "Insert TEXT with markdown highlighting applied."
  (let ((start (point)))
    (insert text)
    (elark-ai-ui--highlight-region start (point))))

(defun elark-ai-ui--highlight-region (beg end)
  "Apply markdown highlighting to the region BEG..END."
  (save-excursion
    ;; Headers: # ## ### etc.
    (goto-char beg)
    (while (re-search-forward
            "^\\(#{1,4}\\) +\\(.+\\)$" end t)
      (put-text-property
       (match-beginning 0) (match-end 0) 'face
       (pcase (length (match-string 1))
         (1 'info-title-1) (2 'info-title-2)
         (3 'info-title-3) (_ 'info-title-4))))
    ;; Bold: **text**
    (goto-char beg)
    (while (re-search-forward
            "\\*\\*\\([^*]+\\)\\*\\*" end t)
      (put-text-property (match-beginning 0) (match-end 0)
                         'face 'bold))
    ;; Inline code: `text`
    (goto-char beg)
    (while (re-search-forward "`\\([^`\n]+\\)`" end t)
      (put-text-property (match-beginning 0) (match-end 0)
                         'face 'font-lock-constant-face))
    ;; Code blocks: ```...```
    (goto-char beg)
    (while (re-search-forward
            "^```[a-z]*\n\\(\\(?:.\\|\n\\)*?\\)\n```$" end t)
      (put-text-property (match-beginning 0) (match-end 0)
                         'face 'font-lock-string-face))
    ;; List items: - item or * item
    (goto-char beg)
    (while (re-search-forward "^\\([ ]*\\)[-*] " end t)
      (put-text-property (match-beginning 0) (match-end 0)
                         'face 'font-lock-keyword-face))))

;;;; Face helpers

(defun elark-ai-ui--label-face (type)
  "Return the face for a fragment label of TYPE."
  (pcase type
    ('header    'bold)
    ('prompt    'font-lock-keyword-face)
    ('skills    'font-lock-comment-face)
    ('plan      'font-lock-keyword-face)
    ('output    'bold)
    ('log       'font-lock-comment-face)
    ('separator 'font-lock-comment-face)
    ;; `tool-call' label embeds its own propertised glyphs (status icon +
    ;; command head) so a flat face here would only fight the per-segment
    ;; faces the renderer already chose.
    ('tool-call 'default)
    (_          'default)))

;;;; Separator helper

(defun elark-ai-ui-insert-separator (id)
  "Insert a visual separator with ID."
  (let ((inhibit-read-only t)
        (beg (point)))
    (insert (propertize (make-string 50 ?─)
                        'face 'font-lock-comment-face
                        'elark-ai-ui-id id
                        'elark-ai-ui-type 'separator)
            "\n")
    (put-text-property beg (point) 'read-only t)
    (put-text-property beg (point)
                       'rear-nonsticky '(read-only))
    (put-text-property beg (point) 'keymap elark-ai-ui-fragment-map)))

(provide 'elark-ai-ui)
;;; elark-ai-ui.el ends here
