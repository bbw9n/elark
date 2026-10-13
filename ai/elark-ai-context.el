;;; elark-ai-context.el --- Buffer context extraction for elark AI layer -*- lexical-binding: t; -*-

;; Copyright (C) 2026 bbw9n

;; Author: bbw9n <bbw9nio@gmail.com>
;; Assisted-by: Claude:claude-opus-5
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Extracts structured context from the current Emacs buffer to inform
;; AI actions.  Each elark major mode registers a provider that
;; returns a plist describing the domain, buffer type, and item at
;; point.  This lets `elark-ai-act' be context-aware without the user
;; having to re-state what they're looking at.
;;
;; Registration is via the `elark-ai-context-provider' symbol property
;; on a major-mode symbol — see `elark-ai-context-register'.  This file
;; has no knowledge of any specific domain's internals; each domain
;; module ships its own provider next to its mode definition.
;;
;; A small soft-reference fallback handles the docs detail buffer,
;; which uses arbitrary major modes (`org-mode', `markdown-mode',
;; `special-mode') and is identified by buffer name instead.

;;; Code:

(require 'cl-lib)
(require 'elark-core)

;;;; Customization

(defcustom elark-ai-context-content-max-chars nil
  "Maximum characters of buffer `:content' fed into the LLM prompt.
nil (the default) means no limit — the whole captured buffer content is
sent, so the model sees everything that is on screen.  Set a positive
integer to clip very large buffers.  When clipping, a provider may set
`:content-keep' to `tail' (keep the most recent end — e.g. the newest
chat messages at the bottom) or `head' (keep the beginning, the default
\— e.g. the start of a document)."
  :type '(choice (const :tag "No limit (whole content)" nil)
                 (integer :tag "Max characters"))
  :group 'elark-ai)

;;;; Provider registration

(defun elark-ai-context-register (mode provider)
  "Register PROVIDER for major MODE.
PROVIDER is a 0-arg function called with the current buffer set
to the buffer in question; it returns a plist:
  :domain      - string like \"calendar\", \"im\", \"tasks\", etc.
  :buffer-type - string like \"agenda\", \"chat\", \"inbox\", etc.
  :item        - plist describing the item at point (if any)
  :summary     - brief text summary for the LLM
  :content     - optional, full document text when applicable

Domains call this near their mode definition.  No registration
order constraints — the property persists on the symbol."
  (put mode 'elark-ai-context-provider provider))

(defun elark-ai-context--lookup-provider ()
  "Walk the current buffer's mode parent chain for a registered provider.
Returns the provider function, or nil."
  (let ((mode major-mode)
        provider)
    (while (and mode (not provider))
      (setq provider (get mode 'elark-ai-context-provider)
            mode (get mode 'derived-mode-parent)))
    provider))

;;;; Generic context extractor

(defun elark-ai-context ()
  "Extract structured context from the current buffer.
Returns a plist with:
  :domain      - string like \"calendar\", \"im\", \"tasks\", etc.
  :buffer-type - string like \"agenda\", \"chat\", \"inbox\", etc.
  :item        - plist describing the item at point (if any)
  :summary     - brief text summary for the LLM
  :content     - optional, full text content (e.g. doc body)"
  (or
   ;; Mode-based providers (registered via `elark-ai-context-register').
   (when-let* ((provider (elark-ai-context--lookup-provider)))
     (funcall provider))
   ;; Buffer-name special case for doc detail buffers, which can be in
   ;; org-mode / markdown-mode / special-mode rather than a elark mode.
   (when (and (string-match-p "\\`\\*Lark Doc: " (buffer-name))
              (fboundp 'elark-docs--ai-context))
     (elark-docs--ai-context))
   ;; Default — unknown buffer.
   (list :domain nil
         :buffer-type "other"
         :item nil
         :summary (format "Buffer: %s (mode: %s)" (buffer-name) major-mode))))

;;;; Context formatting for LLM

(defun elark-ai-context--clip (content keep)
  "Clip CONTENT to `elark-ai-context-content-max-chars'.
Returns CONTENT unchanged when the limit is nil or not exceeded.
Otherwise keeps the last chars when KEEP is `tail' (marking the cut with
a leading \"…\") and the first chars otherwise (trailing \"…\")."
  (let ((max elark-ai-context-content-max-chars))
    (if (or (null max) (<= (length content) max))
        content
      (if (eq keep 'tail)
          (concat "…" (substring content (- (length content) max)))
        (concat (substring content 0 max) "…")))))

(defun elark-ai-context-format ()
  "Format current buffer context as a string for the LLM prompt."
  (let ((ctx (elark-ai-context)))
    (if (plist-get ctx :domain)
        (let ((base (format "Current context: %s (%s)\n%s"
                            (plist-get ctx :domain)
                            (plist-get ctx :buffer-type)
                            (plist-get ctx :summary)))
              (content (plist-get ctx :content)))
          (if content
              (format "%s\n\nThe document content is already available (no need to fetch it):\n\n%s"
                      base
                      (elark-ai-context--clip content (plist-get ctx :content-keep)))
            base))
      "")))

;;;###autoload
(defun elark-ai-context-debug ()
  "Display the AI context extracted from the current buffer."
  (interactive)
  (let ((ctx (elark-ai-context)))
    (message "AI context: domain=%s type=%s item=%S\n  summary: %s"
             (plist-get ctx :domain)
             (plist-get ctx :buffer-type)
             (plist-get ctx :item)
             (plist-get ctx :summary))))

(provide 'elark-ai-context)
;;; elark-ai-context.el ends here
