;;; elark-ai-shell.el --- shell-maker front-end for the elark AI session -*- lexical-binding: t; -*-

;; Copyright (C) 2026 bbw9n

;; Author: bbw9n <bbw9nio@gmail.com>
;; Assisted-by: Claude:claude-opus-5
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; A comint-style conversational front-end for the elark AI engine,
;; built on `shell-maker' (the engine under chatgpt-shell/agent-shell).
;; You get native shell ergonomics for free: prompt, input history
;; (M-p/M-n), history search, transcript saving, busy state.
;;
;; The engine is untouched: skills routing, the agent loop, the ACP
;; backend and session/history semantics are exactly those of
;; `elark-ai-ask'.  The loop's UI callbacks are redirected into the
;; shell via the `elark-ai--frontend' dispatch seam:
;; progress lines render dimmed, finished tool calls render as
;; one-line ✓/✗ entries, and the final answer is written as the
;; command output.
;;
;; One turn may be in flight at a time (across shell and classic UI
;; alike — the engine is a singleton today).  C-c C-k aborts.

;;; Code:

(require 'map)
(require 'shell-maker)
(require 'markdown-overlays)          ; ships with shell-maker
(require 'elark-ai-protocol)          ; session struct, `elark-ai--frontend', `elark-ai--ui'
(require 'elark-ai-skills)            ; `elark-ai-skills-routing-context'
(require 'elark-ai-context)           ; `elark-ai-context-format'
(require 'elark-ai-context-graph)     ; brief gather + prompts
(require 'elark-ai-llm)               ; `elark-ai--select-skills', streaming
(require 'elark-ai-agent)             ; `elark-ai-agent--run'

(defconst elark-ai-shell--buffer-name "*Lark AI Shell*")

(defcustom elark-ai-shell-display-action
  '((display-buffer-reuse-window display-buffer-in-side-window)
    (side . right)
    (window-width . 0.42))
  "`display-buffer' action for showing the Lark AI shell.
The default opens it as a vertical side panel on the right at 42%
of the frame width, reusing an existing window showing it.
shell-maker's own display (`shell-maker-display-function', which
defaults to taking over the current window) is bypassed so this
stays local to the elark shell.  Set to nil to fall back to plain
`pop-to-buffer' behavior."
  :type 'sexp
  :group 'elark-ai)

(defvar-local elark-ai-shell--session nil
  "The `elark-ai-session' for this shell buffer.")

(defvar-local elark-ai-shell--context nil
  "Originating-buffer context captured when the shell was opened.")

;;;; Entry point

;;;###autoload
(defun elark-ai-shell ()
  "Open the Lark AI session as a comint-style shell.
Context is captured from the buffer this command is invoked in,
exactly like `elark-ai-ask' — open the shell from a doc or chat
buffer and the turn sees that content."
  (interactive)
  (let* ((context (elark-ai-context-format))
         (config (make-shell-maker-config
                  :name "elark-ai"
                  :prompt "Lark AI> "
                  :prompt-regexp "^Lark AI> "
                  :execute-command #'elark-ai-shell--execute))
         ;; NO-FOCUS: skip shell-maker's own display (it takes over the
         ;; current window); we place the buffer ourselves below.
         (buf (shell-maker-start
               ;; Defining the mode first gives it a keymap under
               ;; `shell-maker-mode-map', where RET is remapped to
               ;; `shell-maker-submit'; without it RET never sends.
               (progn (shell-maker-define-major-mode config) config)
               t
               (lambda (_config)
                 (propertize
                  (format "Lark AI shell — backend: %s.  /brief <topic> builds a topic brief.  C-c C-k aborts a turn.\n"
                          elark-ai-backend)
                  'font-lock-face 'shadow))
               nil elark-ai-shell--buffer-name "Lark AI")))
    (with-current-buffer buf
      ;; Refresh context on every (re)entry so "open shell from a doc"
      ;; always reflects the doc the user just came from.
      (when (and context (not (string-empty-p context)))
        (setq-local elark-ai-shell--context context))
      (unless elark-ai-shell--session
        (setq-local elark-ai-shell--session (make-elark-ai-session)))
      (local-set-key (kbd "C-c C-k") #'elark-ai-shell-abort))
    ;; Display as a side panel (see `elark-ai-shell-display-action').
    ;; Batch sessions (tests) just make it current.
    (if noninteractive
        (set-buffer buf)
      (pop-to-buffer buf elark-ai-shell-display-action))
    buf))

;;;###autoload
(defun elark-ai-shell-ask (prompt)
  "Open the Lark AI shell and submit PROMPT as a turn.
Context is captured from the invoking buffer, exactly like
`elark-ai-ask' — this is what `elark-ai-ask' routes to when
`elark-ai-interface' is `shell'."
  (interactive "sLark AI: ")
  (let ((buf (elark-ai-shell)))
    (with-current-buffer buf
      (shell-maker-submit :input prompt))
    buf))

;;;###autoload
(defun elark-ai-shell-brief (topic)
  "Open the Lark AI shell and build a cross-domain brief on TOPIC.
Shell counterpart of `elark-ai-brief-on' (which routes here when
`elark-ai-interface' is `shell'); the turn appears in the
transcript as \"/brief TOPIC\"."
  (interactive "sBrief on topic: ")
  (let ((topic (string-trim (or topic ""))))
    (when (string-empty-p topic)
      (user-error "Empty topic"))
    (let ((buf (elark-ai-shell)))
      (with-current-buffer buf
        (shell-maker-submit :input (concat "/brief " topic)))
      buf)))

;;;; Turn execution

(defun elark-ai-shell--execute (input shell)
  "Run INPUT through the elark agent loop (shell-maker executor).
SHELL is the callback alist shell-maker hands to executors.
An input of \"/brief <topic>\" runs the cross-domain topic brief
\(`elark-ai-brief-on') instead of the agent loop."
  (let* ((buf (map-elt shell :buffer))
         (session (with-current-buffer buf
                    (or elark-ai-shell--session
                        (setq-local elark-ai-shell--session
                                    (make-elark-ai-session)))))
         (context (or (buffer-local-value 'elark-ai-shell--context buf) ""))
         ;; Snapshot BEFORE pushing the current prompt, mirroring
         ;; `elark-ai-ask' — the user message is built from prior turns.
         (history (elark-ai-session-history session)))
    (cond
     (elark-ai--frontend
      (funcall (map-elt shell :write-output)
               "A turn is already in flight (C-c C-k aborts it).\n")
      (funcall (map-elt shell :finish-output) nil))
     ;; /brief <topic> — cross-domain topic brief.
     ((string-match "\\`/brief\\(?:[ \t]+\\(.*\\)\\)?\\'"
                    (string-trim input))
      (let ((topic (string-trim (or (match-string 1 (string-trim input)) ""))))
        (if (string-empty-p topic)
            (progn
              (funcall (map-elt shell :write-output)
                       "Usage: /brief <topic>\n")
              (funcall (map-elt shell :finish-output) nil))
          (elark-ai-shell--execute-brief topic shell session))))
     (t
      (push (cons "user" input) (elark-ai-session-history session))
      (setq elark-ai--frontend (elark-ai-shell--make-frontend shell session))
      (elark-ai--select-skills
       input (or (elark-ai-skills-routing-context context) "")
       (lambda (skills)
         (elark-ai-agent--run input context history session skills)))))))

(defun elark-ai-shell--execute-brief (topic shell session)
  "Run the cross-domain TOPIC brief as a turn in SHELL.
Mirrors `elark-ai--brief-on-classic': context-graph gather, then
one synthesis call — whose chunks stream live into the shell.
The gathered brief lands in SESSION history, so follow-up turns
\(\"draft a status update from this\") reuse it."
  (let ((write (map-elt shell :write-output))
        (finish (map-elt shell :finish-output)))
    (push (cons "user" (format "Brief on: %s" topic))
          (elark-ai-session-history session))
    (setf (elark-ai-session-phase session) 'executing)
    (setq elark-ai--frontend (elark-ai-shell--make-frontend shell session))
    (elark-ai--ui :progress-log "Gathering context for \"%s\"…" topic)
    (elark-ai-context-graph-gather
     topic
     (lambda (context-text)
       (if (not (eq (elark-ai-session-phase session) 'executing))
           ;; Aborted between hops — release the turn quietly.
           (progn (setq elark-ai--frontend nil)
                  (funcall finish nil))
         (elark-ai--ui :progress-log "Gathered %d chars; calling LLM…"
           (length (or context-text "")))
         (funcall write "\n")
         (elark-ai--call-llm-stream
          elark-ai-brief-on-prompt
          (elark-ai--brief-user-message topic context-text)
          (lambda (text)
            (push (cons "assistant" text)
                  (elark-ai-session-history session))
            (setf (elark-ai-session-phase session) 'done)
            (setq elark-ai--frontend nil)
            (funcall write "\n")
            (funcall finish t)
            (elark-ai-shell--fontify (map-elt shell :buffer)))
          ;; Stream synthesis chunks straight into the shell.
          (lambda (chunk) (funcall write chunk))))))))

(defun elark-ai-shell--make-frontend (shell session)
  "Build the `elark-ai--frontend' callback plist writing into SHELL.
SESSION receives the history bookkeeping the classic UI would do."
  (let ((write (map-elt shell :write-output))
        (finish (map-elt shell :finish-output)))
    (list
     :progress-log
     (lambda (fmt &rest args)
       (funcall write
                (propertize (format "· %s\n" (apply #'format fmt args))
                            'font-lock-face 'shadow)))
     :render-tool-call
     (lambda (_iter cmd status)
       ;; One line per finished action; the running state is already
       ;; narrated by the progress log.
       (unless (eq status 'running)
         (funcall write
                  (concat
                   (propertize (pcase status ('done "✓") ('error "✗") (_ "–"))
                               'font-lock-face
                               (pcase status ('done 'success)
                                      ('error 'error) (_ 'shadow)))
                   " "
                   (propertize
                    (concat "lark-cli "
                            (string-join (elark-ai-agent-abbreviate-cmd cmd) " "))
                    'font-lock-face 'font-lock-function-name-face)
                   "\n"))))
     ;; The raw action JSON stream is noise in a shell; the busy state
     ;; plus progress lines carry liveness.  Must be non-nil — a nil
     ;; handler would make the LLM layer stream into the classic buffer.
     :stream-preview (lambda () #'ignore)
     :clear-waiting #'ignore
     :present
     (lambda (content skip-history)
       (unless skip-history
         (push (cons "assistant" content)
               (elark-ai-session-history session)))
       (setf (elark-ai-session-phase session) 'done)
       (setq elark-ai--frontend nil)
       (funcall write (concat "\n" content "\n"))
       (funcall finish t)
       (elark-ai-shell--fontify (map-elt shell :buffer))))))

(defface elark-ai-shell-table-zebra
  '((t :inherit hl-line :extend t))
  "Face for alternating markdown-table rows in the Lark AI shell.
Inherits the theme's subtle current-line background instead of
markdown-overlays' default `lazy-highlight', which is loud in
most themes."
  :group 'elark-ai)

(defun elark-ai-shell--fontify (buf)
  "Render markdown markup in BUF via overlays.
`markdown-overlays' ships with shell-maker (the same renderer
agent-shell/chatgpt-shell use): headers, bold, code fences and
tables display styled, with the raw markup hidden.
The table zebra face is softened buffer-locally (see
`elark-ai-shell-table-zebra') so other shell-maker shells keep
their own styling."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (setq-local markdown-overlays--table-zebra-face
                  'elark-ai-shell-table-zebra)
      (markdown-overlays-put))))

;;;; Abort

(defun elark-ai-shell-abort ()
  "Abort the in-flight shell turn and release the engine."
  (interactive)
  (elark-ai-acp-abort)
  (when elark-ai-shell--session
    (setf (elark-ai-session-phase elark-ai-shell--session) 'idle))
  (setq elark-ai--frontend nil)
  (call-interactively #'shell-maker-interrupt)
  (message "Lark AI shell: aborted"))

(provide 'elark-ai-shell)
;;; elark-ai-shell.el ends here
