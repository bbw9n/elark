;;; elark-ai.el --- AI-native orchestration layer for elark -*- lexical-binding: t; -*-

;; Copyright (C) 2026 bbw9n

;; Author: bbw9n <bbw9nio@gmail.com>
;; Assisted-by: Claude:claude-opus-5
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Provides the AI-native layer for elark: natural-language prompts
;; are turned into multi-step lark-cli execution plans by an LLM,
;; reviewed by the user, executed, and the results are synthesized
;; into readable output.
;;
;; The LLM's knowledge comes from lark-cli SKILL.md files (loaded by
;; elark-ai-skills.el).  Buffer context (from elark-ai-context.el)
;; makes actions aware of what the user is looking at.
;;
;; Two entry points:
;;   elark-ai-ask   — natural-language prompt (minibuffer)
;;   elark-ai-act   — context-aware action at point
;;
;; LLM backends: gptel (preferred), or raw HTTP fallback.

;;; Code:

(require 'cl-lib)
(require 'elark-core)
(require 'elark-ui)            ; shared spinner engine + `elark-spinner' face
(require 'elark-ai-skills)
(require 'elark-ai-context)
(require 'elark-ai-protocol)   ; debug log + plan parse + interpolate + user-message
(require 'elark-ai-llm)        ; backend dispatch (gptel/http) + streaming
(require 'elark-ai-runner)     ; plan executor
(require 'elark-ai-agent)      ; agentic loop executor (default; see `elark-ai-strategy')
(require 'elark-ai-context-graph) ; cross-domain topic gatherer (elark-ai-brief-on)

(require 'elark-ai-shell)      ; shell-maker front-end (`elark-ai-interface')

;; Forward declarations for IM module

;;;; Session state
;;
;; A single buffer-local struct holds all conversation state for the
;; AI buffer.  Replaces the previous ~9 separate buffer-locals so that
;; (a) the state machine is inspectable in one place, (b) `elark-ai-reset'
;; can clear it atomically, and (c) abort can find the in-flight gptel
;; request handle.  The `cl-defstruct' itself lives in
;; `elark-ai-protocol' so the runner can use the auto-generated
;; accessors at byte-compile time without depending on this module.

(defvar-local elark-ai--session nil
  "The `elark-ai-session' for this AI buffer.")

(defun elark-ai--session ()
  "Return the session for the AI buffer, creating it on first use.
Always operates against the AI buffer, not the current buffer, so
callers in user buffers (e.g. `elark-ai-ask' invoked from a doc)
don't have to switch first."
  (with-current-buffer (elark-ai--get-buffer)
    (or elark-ai--session
        (setq elark-ai--session (make-elark-ai-session)))))

;;;; Fragment-based AI buffer
;;
;; Uses elark-ai-ui.el for a fragment-based rendering model.
;; Each conversation turn gets its own set of fragments with a
;; turn-number prefix.  Follow-up questions append new turns.
;; An editable input area at the bottom allows inline follow-ups.

(require 'elark-ai-ui)

(defvar elark-ai-plan-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map elark-ai-ui-mode-map)
    (define-key map (kbd "C-c C-c") #'elark-ai-send-followup)
    (define-key map (kbd "C-c C-k") #'elark-ai-abort)
    (define-key map (kbd "C-c C-d") #'elark-ai-toggle-debug)
    (define-key map (kbd "C-c C-l") #'elark-ai-reset)
    map)
  "Keymap for the Lark AI buffer.
Intentionally minimal — only the prefix-key bindings live here
so that plain typing in the follow-up input area below isn't
shadowed.  TAB → toggle-section is on `elark-ai-ui-fragment-map'
\(applied as a text property to each fragment), and the plan-review
keys (RET/q/x) are on `elark-ai-plan-keys-map' (applied to the plan
fragment only).")

(defvar elark-ai-plan-keys-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map elark-ai-ui-fragment-map)
    (define-key map (kbd "RET") #'elark-ai-plan-execute)
    (define-key map (kbd "q")   #'elark-ai-plan-cancel)
    (define-key map (kbd "x")   #'elark-ai-plan-remove-step)
    map)
  "Plan-review bindings for the plan fragment.
Attached as a `keymap' text property by
`elark-ai--refresh-plan-fragment'.  Inherits TAB from
`elark-ai-ui-fragment-map' so toggling still works on the
plan, and adds RET/q/x for review actions.  Scoped to the plan so
plain typing in the follow-up input area below isn't intercepted.")

(define-derived-mode elark-ai-plan-mode elark-ai-ui-mode "Lark AI"
  "Major mode for the Lark AI buffer.")

(defun elark-ai--get-buffer ()
  "Get or create the AI buffer."
  (let ((buf (get-buffer-create elark-ai--buf-name)))
    (with-current-buffer buf
      ;; The buffer inherits `default-directory' from whichever buffer
      ;; was current at creation — possibly a doc buffer pointing at a
      ;; purged temp cache dir.  Backends spawn processes from this
      ;; buffer (gptel's curl, the ACP agent), which errors in a dead
      ;; directory, so sanitize it here.
      (setq-local default-directory (elark--safe-default-directory))
      (unless (derived-mode-p 'elark-ai-plan-mode)
        (elark-ai-plan-mode)))
    buf))

;;; Turn-scoped fragment IDs

(defun elark-ai--frag (suffix)
  "Return fragment ID for SUFFIX in the current turn."
  (format "t%d-%s" (elark-ai-session-turn (elark-ai--session)) suffix))

;;; Progress log — appends to the current turn's log fragment

(defun elark-ai--progress-log (fmt &rest args)
  "Add a timestamped entry, FMT formatted with ARGS, to the log fragment.
Routed to the active alternative front-end when one is installed
\(see `elark-ai--frontend')."
  (if-let* ((fn (plist-get elark-ai--frontend :progress-log)))
      (apply fn fmt args)
    (let ((entry (format "[%s] %s\n"
                         (format-time-string "%H:%M:%S")
                         (apply #'format fmt args))))
      (when-let* ((buf (get-buffer elark-ai--buf-name)))
        (with-current-buffer buf
          (elark-ai-ui-append-fragment
           (elark-ai--frag "log") entry))))))

;;; Header progress bar
;;
;; An indeterminate "working…" bar shown in the AI buffer's header line
;; while a turn is loading or executing.  Unlike the in-buffer streaming
;; preview, the header line is pinned to the top of the window, so the
;; user can tell the turn is running even when the loading zone has
;; scrolled out of view (e.g. on follow-ups).

(defvar-local elark-ai--bar-frame nil
  "Current header progress-bar frame string, or nil when idle.")
(defvar-local elark-ai--bar-timer nil
  "Timer animating the header progress bar, or nil.")

(defun elark-ai--ai-header-line ()
  "Return the AI buffer's header-line string while a turn is busy.
The bar itself is the shared `elark-ui-bar-frames' component."
  (concat " Lark AI  "
          (or elark-ai--bar-frame (aref elark-ui-bar-frames 0))
          "  working…"))

(defun elark-ai--stop-bar ()
  "Stop the header progress bar and remove the header line."
  (when-let* ((buf (get-buffer elark-ai--buf-name)))
    (with-current-buffer buf
      (when elark-ai--bar-timer
        (elark-spinner-stop elark-ai--bar-timer))
      (setq elark-ai--bar-timer nil
            elark-ai--bar-frame nil
            header-line-format nil)
      (force-mode-line-update))))

(defun elark-ai--start-bar ()
  "Show and animate the header progress bar while the AI buffer is busy.
Self-stops once the session phase is no longer `loading' or `executing'
\(the render function then returns nil and ON-STOP clears the bar)."
  (when-let* ((buf (get-buffer elark-ai--buf-name)))
    (with-current-buffer buf
      (when elark-ai--bar-timer
        (elark-spinner-stop elark-ai--bar-timer))
      (setq header-line-format '(:eval (elark-ai--ai-header-line)))
      (setq elark-ai--bar-timer
            (elark-spinner-start
             (lambda (frame)
               (when (and (buffer-live-p buf)
                          (memq (and (buffer-local-value 'elark-ai--session buf)
                                     (elark-ai-session-phase
                                      (buffer-local-value 'elark-ai--session buf)))
                                '(loading executing)))
                 (with-current-buffer buf
                   (setq elark-ai--bar-frame frame)
                   (force-mode-line-update))
                 t))
             0.12 elark-ui-bar-frames 36000
             (lambda () (elark-ai--stop-bar)))))))

;;; Session lifecycle commands

(declare-function gptel-abort "ext:gptel" (&optional buf))

;;;###autoload
(defun elark-ai-abort ()
  "Abort any in-flight LLM request and reset the session phase.
Marks any running step as skipped so the plan fragment reflects
reality.  Bound to \\[elark-ai-abort] in `elark-ai-plan-mode-map'."
  (interactive)
  (when (fboundp 'gptel-abort)
    (gptel-abort (elark-ai--get-buffer)))
  (elark-ai-acp-abort)
  (elark-ai--stop-bar)
  (let ((session (elark-ai--session)))
    (setf (elark-ai-session-phase session) 'idle
          (elark-ai-session-callback session) nil)
    ;; Mark anything still 'running as 'skipped so the plan view
    ;; doesn't sit on a stale spinner.
    (setf (elark-ai-session-step-status session)
          (mapcar (lambda (kv)
                    (cons (car kv)
                          (if (eq (cdr kv) 'running) 'skipped (cdr kv))))
                  (elark-ai-session-step-status session))))
  (elark-ai--progress-log "Aborted by user.")
  (elark-ai--refresh-plan-fragment)
  (elark-ai--ensure-input-area)
  (message "Lark AI: aborted"))

;;;###autoload
(defun elark-ai-reset ()
  "Clear the AI buffer and start a fresh conversation.
Aborts any in-flight request and discards all session state
\(history, context, skills, plan)."
  (interactive)
  (when-let* ((buf (get-buffer elark-ai--buf-name)))
    (when (fboundp 'gptel-abort)
      (gptel-abort buf))
    (elark-ai-acp-abort)
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer))
      (setq elark-ai--session (make-elark-ai-session)))
    (message "Lark AI: session reset")))

;;; Input area — editable region at the bottom
;;
;; Boundary tracking is done with two markers stored on the session
;; (`input-region-start' and `input-start').  This replaces the older
;; approach of identifying the input via `elark-ai-ui-id' text properties
;; on a body-sentinel "\n", which required a delicate front-sticky /
;; rear-nonsticky dance to make typed characters inherit the right
;; properties.  Markers are unambiguous and shrink/grow with edits.

(defun elark-ai--clear-input-area ()
  "Delete the existing input region (separator + label + body)."
  (let* ((session (elark-ai--session))
         (m (elark-ai-session-input-region-start session)))
    (when (and m (marker-position m))
      (let ((inhibit-read-only t))
        (delete-region m (point-max))))
    (setf (elark-ai-session-input-region-start session) nil
          (elark-ai-session-input-start session) nil)))

(defun elark-ai--ensure-input-area ()
  "Insert the editable follow-up area at the bottom of the AI buffer.
Records two markers on the session: `input-region-start' (start of
the separator, used to delete the whole region) and `input-start'
\(start of editable text, used by `get-input-text')."
  (let* ((buf (elark-ai--get-buffer))
         (session (elark-ai--session)))
    (with-current-buffer buf
      (elark-ai--clear-input-area)
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert "\n")
        ;; Mark where the input region begins (everything from here
        ;; to point-max is recreated each turn).
        (setf (elark-ai-session-input-region-start session)
              (copy-marker (point) nil))
        (elark-ai-ui-insert-separator "ai-input-sep")
        ;; Read-only label.
        (let ((label-beg (point)))
          (insert (propertize
                   "Follow-up (C-c C-c send · C-c C-k abort · C-c C-l reset · C-c C-d debug):\n"
                   'face 'font-lock-keyword-face))
          (put-text-property label-beg (point) 'read-only t)
          ;; The label is rear-nonsticky for `read-only' only — so the
          ;; typed text just below isn't read-only, but other label
          ;; properties (face/keymap) don't leak either way because we
          ;; don't depend on inheritance for input identification.
          (put-text-property label-beg (point)
                             'rear-nonsticky '(read-only)))
        ;; Mark where typed text starts.  Insertion-type nil keeps the
        ;; marker at the same position when text is inserted at it,
        ;; so it always points at the *start* of the user's input.
        (setf (elark-ai-session-input-start session)
              (copy-marker (point) nil))
        (goto-char (elark-ai-session-input-start session))))))

(defun elark-ai--get-input-text ()
  "Return the text typed in the input area, trimmed."
  (when-let* ((session (elark-ai--session))
              (m (elark-ai-session-input-start session))
              (pos (marker-position m)))
    (string-trim
     (buffer-substring-no-properties pos (point-max)))))

(defun elark-ai-send-followup ()
  "Send the text in the input area as a follow-up question."
  (interactive)
  (let ((input (elark-ai--get-input-text)))
    (when (or (null input) (string-empty-p input))
      (user-error "Empty input"))
    (elark-ai--clear-input-area)
    (elark-ai-ask input)))

;;; Build a new turn's layout

(defun elark-ai--show-loading (prompt skill-names)
  "Start a new turn for PROMPT in the AI buffer, listing SKILL-NAMES."
  (let* ((buf (elark-ai--get-buffer))
         (session (elark-ai--session)))
    (with-current-buffer buf
      ;; Drop the previous input region, advance turn, reset per-turn
      ;; plan state on the session.
      (elark-ai--clear-input-area)
      (cl-incf (elark-ai-session-turn session))
      (setf (elark-ai-session-steps session) nil
            (elark-ai-session-callback session) nil
            (elark-ai-session-phase session) 'loading
            (elark-ai-session-step-status session) nil)
      ;; On first turn, add header
      (when (= (elark-ai-session-turn session) 1)
        (elark-ai-ui-clear)
        (elark-ai-ui-insert-fragment "ai-header" 'header "Lark AI" nil)
        (elark-ai-ui-insert-separator "ai-sep-header"))
      ;; Turn separator (after first turn)
      (when (> (elark-ai-session-turn session) 1)
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (insert "\n")
          (elark-ai-ui-insert-separator
           (elark-ai--frag "turn-sep"))))
      ;; Prompt
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (elark-ai-ui-insert-fragment
         (elark-ai--frag "prompt") 'prompt
         (concat "▶ " prompt) nil)
        ;; Skills
        (elark-ai-ui-insert-fragment
         (elark-ai--frag "skills") 'skills
         (concat "Skills: " (string-join skill-names " · "))
         nil)
        ;; Log
        (elark-ai-ui-insert-fragment
         (elark-ai--frag "log") 'log nil "")
        ;; Plan placeholder
        (elark-ai-ui-insert-fragment
         (elark-ai--frag "plan") 'plan nil
         (propertize "Waiting for LLM response...\n"
                     'face 'font-lock-comment-face)))
      ;; Track conversation
      (push (cons "user" prompt) (elark-ai-session-history session)))
    ;; Make the new turn's loading zone visible.  Follow-up turns are
    ;; appended at the bottom, so without scrolling there the
    ;; "Waiting for LLM response…" preview would be off-screen.
    (let ((win (display-buffer buf)))
      (when (window-live-p win)
        (when-let* ((region (with-current-buffer buf
                             (elark-ai-ui-find-fragment (elark-ai--frag "prompt")))))
          (with-selected-window win
            (goto-char (car region))
            (recenter 0)))))
    ;; Animate the header "working…" bar for the duration of the turn.
    (elark-ai--start-bar)))

;;; Live stream preview
;; A small rolling tail under "Waiting for LLM response…" so a slow
;; call visibly progresses instead of looking hung.

(defcustom elark-ai-stream-tail-lines 4
  "How many recent lines of the live LLM stream to preview while waiting.
Shown under \"Waiting for LLM response…\" in the plan fragment as a
small rolling tail, so the user can see the request is in progress
rather than hung.  Set to 0 to disable the preview."
  :type 'integer
  :group 'elark-ai)

(defun elark-ai--stream-tail (text n)
  "Return a bounded tail of TEXT for the loading preview.
Roughly the last N lines; newline-poor streams (e.g. one-line JSON) are
clipped to the last N*80 characters so the preview stays a few lines
tall rather than one ever-growing line.  The stream is usually a JSON
action object, so common string escapes (\\n, \\t, \\\") are decoded
first — display-only and best-effort — so streamed content shows real
line breaks instead of literal backslash sequences."
  (when (and text (> n 0) (not (string-empty-p text)))
    (let* ((decoded (replace-regexp-in-string
                     "\\\\[nt\"]"
                     (lambda (m)
                       (pcase m ("\\n" "\n") ("\\t" "\t") ("\\\"" "\"")))
                     text t t))
           (maxchars (* n 80))
           (clip (if (> (length decoded) maxchars)
                     (concat "…" (substring decoded (- (length decoded) maxchars)))
                   decoded))
           (lines (seq-remove #'string-empty-p (split-string clip "\n"))))
      (string-join (last lines n) "\n"))))

(defun elark-ai--stream-preview-handler ()
  "Return a streaming chunk-handler that previews the live LLM output.
The returned closure accumulates chunks and updates the plan fragment
to \"Waiting for LLM response…\" plus a rolling tail of the last
`elark-ai-stream-tail-lines' lines.  Use it as the CHUNK-HANDLER arg of
`elark-ai--call-llm-stream'."
  (if elark-ai--frontend
      (when-let* ((fn (plist-get elark-ai--frontend :stream-preview)))
        (funcall fn))
    (let ((acc ""))
      (lambda (chunk)
        (setq acc (concat acc chunk))
        (when-let* ((buf (get-buffer elark-ai--buf-name)))
          (with-current-buffer buf
            (when (elark-ai-ui-find-fragment (elark-ai--frag "plan"))
              (let ((tail (elark-ai--stream-tail acc elark-ai-stream-tail-lines)))
                (elark-ai-ui-update-fragment
                 (elark-ai--frag "plan") nil
                 (concat
                  (propertize "Waiting for LLM response…\n"
                              'face 'font-lock-comment-face)
                  (and tail (propertize tail 'face 'shadow))))))))))))

(defun elark-ai--clear-waiting ()
  "Clear the loading preview in the current turn's plan fragment."
  (if-let* ((fn (plist-get elark-ai--frontend :clear-waiting)))
      (funcall fn)
    (when-let* ((buf (get-buffer elark-ai--buf-name)))
      (with-current-buffer buf
        (when (elark-ai-ui-find-fragment (elark-ai--frag "plan"))
          (elark-ai-ui-update-fragment (elark-ai--frag "plan") nil ""))))))

;;; Tool-call cards — per-action panes in the agent loop.
;;
;; Each dispatched lark-cli command gets its own collapsible fragment
;; sh-fontified and tinted with the theme's code-block background
;; (`elark-ui-block-bg-face').  The card is inserted in the running
;; state and its label is then updated as the command completes —
;; preserving body fold state across the status transition.

(defun elark-ai--tool-call-label (cmd status)
  "Build the label line for a tool-call card.
CMD is the lark-cli argument list; STATUS is `running', `done',
`error', or `skipped'."
  (let* ((head (mapconcat #'identity
                          (seq-take (or cmd nil) 2)
                          " "))
         (icon (pcase status
                 ('running (propertize "⟳" 'face 'warning))
                 ('done    (propertize "✓" 'face 'success))
                 ('error   (propertize "✗" 'face 'error))
                 ('skipped (propertize "⊘" 'face 'shadow))
                 (_        "·"))))
    (concat "  " icon " "
            (propertize (concat "lark-cli " head)
                        'face 'font-lock-function-name-face))))

(defun elark-ai--shell-quote (arg)
  "Return ARG with minimal shell quoting for display.
Args that consist only of typical lark-cli token characters
\(alphanumerics plus . / + - _ : = , @ % # *) pass through verbatim
— so flags like `+chats-list' or `--page-size=20' read naturally —
and only arguments containing whitespace or shell metacharacters get
the heavier `shell-quote-argument' treatment.  This is purely cosmetic;
the rendered string is never re-executed by a shell.

ARG values that carry the `elark-ai-preview' text property (produced
by `elark-ai-agent--preview-string') are wrapped in display quotes
without escaping — the ellipsis and `[N chars]' tail would otherwise
get mangled by shell-quoting."
  (cond
   ((and (> (length arg) 0)
         (get-text-property 0 'elark-ai-preview arg))
    (concat "\"" (substring-no-properties arg) "\""))
   ((string-match-p "\\`[A-Za-z0-9_./+:=,@%#*-]+\\'" arg)
    arg)
   (t (shell-quote-argument arg))))

(defun elark-ai--format-cmd-body (cmd)
  "Pretty-print CMD onto multiple lines for a tool-call card body.
Leading positional args stay on the first line; each --flag and its
value follow on continuation lines with a two-space indent and a
trailing backslash — the same shape a person would type at a shell.

Content-flag values (--content, --markdown, --body, --text, --message)
are clipped to a one-line preview via `elark-ai-agent-abbreviate-cmd'
so a multi-KB doc body doesn't flood the card.  The CLI invocation
still receives the original, unabbreviated command."
  (let ((cmd (elark-ai-agent-abbreviate-cmd cmd))
        (head nil) (groups nil) (cur nil))
    (dolist (arg cmd)
      (if (string-prefix-p "--" arg)
          (progn
            (cond ((null head) (setq head (nreverse cur)))
                  (cur         (push (nreverse cur) groups)))
            (setq cur (list arg)))
        (push arg cur)))
    (cond ((null head) (setq head (nreverse cur)))
          (cur         (push (nreverse cur) groups)))
    (let ((head-line (concat "lark-cli "
                             (mapconcat #'elark-ai--shell-quote head " ")))
          (group-lines (mapcar (lambda (g)
                                 (concat "  "
                                         (mapconcat #'elark-ai--shell-quote
                                                    g " ")))
                               (nreverse groups))))
      (if group-lines
          (concat head-line " \\\n"
                  (mapconcat #'identity group-lines " \\\n"))
        head-line))))

(defun elark-ai--render-tool-call (iter cmd status)
  "Insert or update the tool-call card for CMD at agent iteration ITER.
On first call (STATUS=`running'), inserts a fragment just above the
plan fragment containing the sh-fontified command body.  On subsequent
calls, updates only the label — so a user-folded body stays folded
across the running → done/error transition.
Routed to the active alternative front-end when one is installed."
  (if-let* ((fn (plist-get elark-ai--frontend :render-tool-call)))
      (funcall fn iter cmd status)
    (when-let* ((buf (get-buffer elark-ai--buf-name)))
      (with-current-buffer buf
        (let ((id (elark-ai--frag (format "tool-%d" iter)))
              (label (elark-ai--tool-call-label cmd status)))
          (cond
           ((elark-ai-ui-find-fragment id)
            (elark-ai-ui-update-label id label))
           (t
            (let ((plan-region (elark-ai-ui-find-fragment
                                (elark-ai--frag "plan"))))
              (save-excursion
                (goto-char (if plan-region (car plan-region) (point-max)))
                (elark-ai-ui-insert-fragment
                 id 'tool-call label
                 (elark-ai--format-cmd-body cmd)))))))))))

;;; Plan display — update the plan fragment

(defun elark-ai--display-plan (steps callback)
  "Show a plan for review.
The plan is STEPS; CALLBACK is called with the confirmed steps."
  (let* ((buf (elark-ai--get-buffer))
         (session (elark-ai--session)))
    (with-current-buffer buf
      (setf (elark-ai-session-steps session) steps
            (elark-ai-session-callback session) callback
            (elark-ai-session-phase session) 'review
            (elark-ai-session-step-status session)
            (mapcar (lambda (s) (cons (plist-get s :index) 'pending))
                    steps))
      (elark-ai--refresh-plan-fragment))
    (pop-to-buffer buf)))

(defun elark-ai--refresh-plan-fragment ()
  "Rebuild the plan fragment from current state."
  (let ((body (elark-ai--format-plan-body)))
    (elark-ai-ui-update-fragment
     (elark-ai--frag "plan")
     (elark-ai--plan-label)
     body)
    ;; Scope plan-review keys (RET/q/x) to the plan fragment via a
    ;; `keymap' text property — keeps them off the buffer mode-map
    ;; so plain typing in the input area isn't intercepted.
    (let ((region (elark-ai-ui-find-fragment (elark-ai--frag "plan"))))
      (when region
        (let ((inhibit-read-only t))
          (put-text-property (car region) (cdr region)
                             'keymap elark-ai-plan-keys-map))))))

(defun elark-ai--plan-label ()
  "Return the plan section label for the current phase."
  (pcase (elark-ai-session-phase (elark-ai--session))
    ('review    "Plan — confirm to execute")
    ('executing "Executing")
    ('done      "Complete")
    (_          "Plan")))

(defun elark-ai--format-plan-body ()
  "Format the plan as a string for the plan fragment.
Each step's line carries a `elark-ai-step-index' text property so
`elark-ai-plan-remove-step' can pick the step under point without
relying on line-number arithmetic."
  (let ((session (elark-ai--session)))
    (with-temp-buffer
      (dolist (step (elark-ai-session-steps session))
        (let* ((idx (plist-get step :index))
               (desc (plist-get step :description))
               (cmd (plist-get step :command))
               (side-effect (plist-get step :side-effect))
               (synthesize (plist-get step :synthesize))
               (pg (plist-get step :parallel-group))
               (status (alist-get idx (elark-ai-session-step-status session)))
               (indicator
                (pcase status
                  ('done    (propertize "✓" 'face 'success))
                  ('running (propertize "⟳" 'face 'warning))
                  ('error   (propertize "✗" 'face 'error))
                  ('skipped (propertize "✗" 'face
                                        'font-lock-comment-face))
                  (_        (propertize "○" 'face
                                        'font-lock-comment-face)))))
          (let ((line-beg (point)))
            (insert "  " indicator " "
                    (propertize desc 'face
                                (pcase status
                                  ('done 'font-lock-comment-face)
                                  (_ (if side-effect 'warning
                                       'default))))
                    (if (and side-effect (not (eq status 'done)))
                        (propertize " ⚠ writes" 'face 'error)
                      "")
                    (if synthesize
                        (propertize " (synthesis)"
                                    'face 'font-lock-comment-face)
                      "")
                    (if pg (format " [group %d]" pg) "")
                    "\n")
            (when cmd
              (insert "    "
                      (propertize (string-join cmd " ")
                                  'face 'font-lock-string-face)
                      "\n"))
            ;; Tag both lines (description + optional command) with
            ;; the step's index so remove-step can dispatch by point.
            (put-text-property line-beg (point)
                               'elark-ai-step-index idx))))
      ;; Footer
      (pcase (elark-ai-session-phase session)
        ('review
         (insert "\n"
                 (propertize "RET" 'face 'bold) " Execute  "
                 (propertize "x" 'face 'bold) " Remove step  "
                 (propertize "q" 'face 'bold) " Cancel\n"))
        ('executing
         (insert "\n"
                 (propertize "Running..."
                             'face 'font-lock-comment-face)
                 "\n")))
      (buffer-string))))

;;; Step status updates — only refresh the plan fragment

(defun elark-ai--update-step-status (index status)
  "Update step INDEX to STATUS and refresh plan."
  (when-let* ((buf (get-buffer elark-ai--buf-name)))
    (with-current-buffer buf
      (setf (alist-get index (elark-ai-session-step-status (elark-ai--session)))
            status)
      (elark-ai--refresh-plan-fragment))))

(defun elark-ai--mark-all-done ()
  "Mark execution as done and refresh plan."
  (when-let* ((buf (get-buffer elark-ai--buf-name)))
    (with-current-buffer buf
      (setf (elark-ai-session-phase (elark-ai--session)) 'done)
      (elark-ai--refresh-plan-fragment))))


;;; Plan actions

(defun elark-ai-plan-execute ()
  "Execute the plan."
  (interactive)
  (let ((session (elark-ai--session)))
    (unless (eq (elark-ai-session-phase session) 'review)
      (user-error "No plan to execute"))
    (let ((steps (elark-ai-session-steps session))
          (callback (elark-ai-session-callback session)))
      (setf (elark-ai-session-phase session) 'executing
            (elark-ai-session-callback session) nil)
      (elark-ai--refresh-plan-fragment)
      ;; Re-arm the header bar (it stopped while the plan sat in review).
      (elark-ai--start-bar)
      (when callback
        (funcall callback steps)))))

(defun elark-ai-plan-cancel ()
  "Cancel the plan."
  (interactive)
  (let ((session (elark-ai--session)))
    (setf (elark-ai-session-phase session) 'done
          (elark-ai-session-callback session) nil))
  (elark-ai--refresh-plan-fragment)
  (elark-ai--progress-log "Cancelled."))

(defun elark-ai-plan-remove-step ()
  "Remove the plan step at point.
Reads the `elark-ai-step-index' text property attached by
`elark-ai--format-plan-body', so the cursor can be anywhere on the
step's description or command line."
  (interactive)
  (let ((session (elark-ai--session)))
    (unless (eq (elark-ai-session-phase session) 'review)
      (user-error "Can only remove steps during review"))
    (let ((idx (get-text-property (point) 'elark-ai-step-index)))
      (unless idx
        (user-error "Place point on a plan step"))
      (setf (elark-ai-session-steps session)
            (seq-remove (lambda (s) (= (plist-get s :index) idx))
                        (elark-ai-session-steps session)))
      (setf (elark-ai-session-step-status session)
            (mapcar (lambda (s) (cons (plist-get s :index) 'pending))
                    (elark-ai-session-steps session)))
      (elark-ai--refresh-plan-fragment))))

;; Plan execution, $step-N interpolation, LLM dispatch and streaming
;; have moved to `elark-ai-runner.el', `elark-ai-protocol.el', and
;; `elark-ai-llm.el' respectively (required at the top of this file).

;;;; Synthesis (second LLM pass)

(defun elark-ai--results-text (results plan)
  "Format step RESULTS as a labelled text block for an LLM prompt.
PLAN supplies each step's description.  The `:synthesize' sentinel — an
unproduced (deferred) synthesis step — is skipped.  String results
\(e.g. an inline producer's output) are inserted verbatim; other results
are JSON-encoded.  Steps are ordered by index."
  (mapconcat
   (lambda (pair)
     (let ((idx (car pair))
           (data (cdr pair)))
       (unless (eq data :synthesize)
         (let ((step (seq-find (lambda (s) (= (plist-get s :index) idx)) plan)))
           (format "### Step %d: %s\n```json\n%s\n```\n"
                   idx
                   (or (and step (plist-get step :description)) "")
                   (cond ((null data) "(no data)")
                         ((stringp data) data)
                         (t (json-encode data))))))))
   (sort (copy-sequence results)
         (lambda (a b) (< (car a) (car b))))
   "\n"))

(defun elark-ai--synthesis-instruction (results plan)
  "Choose the instruction for the terminal (final-answer) synthesis of PLAN.
Prefer a synthesis step that is still deferred — its result in RESULTS
is the `:synthesize' sentinel.  A synthesis step already consumed inline
as a producer (its result is real text) should NOT drive the final
answer with its content-generation instruction, so it is ignored here.
Falls back to a generic summary instruction."
  (let ((deferred (seq-find
                   (lambda (s)
                     (and (plist-get s :synthesize)
                          (eq (alist-get (plist-get s :index) results)
                              :synthesize)))
                   plan)))
    (or (and deferred (plist-get deferred :synthesis-instruction))
        "Summarize the results clearly and concisely.")))

(defun elark-ai--synthesize (results plan system-prompt callback)
  "Send execution RESULTS back to the LLM for synthesis.
PLAN is the original plan.  SYSTEM-PROMPT must be the synthesis
prompt (see `elark-ai-skills-build-synthesis-prompt') — passing
the planning prompt here makes the model emit another JSON plan
instead of the prose answer.  CALLBACK receives the synthesis text."
  (let* ((instruction (elark-ai--synthesis-instruction results plan))
         (results-text (elark-ai--results-text results plan))
         (user-msg (format "Here are the execution results:\n\n%s\n\nInstruction: %s\n\nRespond with plain text (not JSON). Use markdown formatting."
                           results-text instruction)))
    (elark-ai--call-llm system-prompt user-msg callback)))

(defun elark-ai--synthesize-stream (results plan system-prompt)
  "Like `elark-ai--synthesize' but streams the output for RESULTS of PLAN.
SYSTEM-PROMPT must be the synthesis prompt (no JSON mandate)."
  (let* ((instruction (elark-ai--synthesis-instruction results plan))
         (results-text (elark-ai--results-text results plan))
         (user-msg (format "Here are the execution results:\n\n%s\n\nInstruction: %s\n\nRespond with plain text (not JSON). Use markdown formatting."
                           results-text instruction)))
    (elark-ai--call-llm-stream
     system-prompt user-msg
     (lambda (text)
       (when-let* ((buf (get-buffer elark-ai--buf-name)))
         (with-current-buffer buf
           (push (cons "assistant" text)
                 (elark-ai-session-history (elark-ai--session)))
           (elark-ai--ensure-input-area)))))))

(defun elark-ai--synthesize-inline (step done-fn)
  "Run a producer synthesis STEP now and pass its text to DONE-FN.
Called by the runner when a later step interpolates this step's output
via $step-N.  Unlike the terminal synthesis pass this is non-streaming
\(the full text must exist before the dependent step runs) and uses the
producer system prompt (bare artifact, no user-facing framing).  The
source material is the originating-buffer context plus any prior step
results — the chat/document content the plan was told to work from lives
in the context, not in a CLI result."
  (let* ((session (elark-ai--session))
         (instruction (or (plist-get step :synthesis-instruction)
                          "Produce the requested content."))
         (context (elark-ai-session-context session))
         (results (elark-ai-session-step-results session))
         (plan (elark-ai-session-steps session))
         (system-prompt (elark-ai-skills-build-producer-prompt
                         (elark-ai-session-skills session)))
         (results-text (elark-ai--results-text results plan))
         (user-msg
          (concat
           (when (and context (not (string-empty-p context)))
             (concat "## Source context\n" context "\n\n"))
           (unless (string-empty-p results-text)
             (concat "## Prior step results\n" results-text "\n\n"))
           "## Instruction\n" instruction
           "\n\nOutput only the content described above, ready to be used"
           " verbatim as a command argument.")))
    (elark-ai--call-llm system-prompt user-msg done-fn)))

;;;; Output rendering

(defun elark-ai--ensure-output-fragment ()
  "Ensure the output fragment exists for the current turn."
  (when-let* ((buf (get-buffer elark-ai--buf-name)))
    (with-current-buffer buf
      (let ((out-id (elark-ai--frag "output")))
        (unless (elark-ai-ui-find-fragment out-id)
          (let ((inhibit-read-only t))
            (goto-char (point-max))
            (elark-ai-ui-insert-separator (elark-ai--frag "sep"))
            (elark-ai-ui-insert-fragment
             out-id 'output "Output" "")))))))

(defun elark-ai--present (content &optional skip-history)
  "Display CONTENT in the output fragment, then add input area.
When SKIP-HISTORY is non-nil, do not push CONTENT into the
conversation history — used for error fallbacks like
parse-plan failure where the raw text would otherwise pollute
future turns.
Routed to the active alternative front-end when one is installed —
the front-end then owns history bookkeeping for its session."
  (if-let* ((fn (plist-get elark-ai--frontend :present)))
      (funcall fn content skip-history)
    (elark-ai--present-classic content skip-history)))

(defun elark-ai--present-classic (content skip-history)
  "Classic *Lark AI* buffer implementation of `elark-ai--present'.
Show CONTENT; record it in history unless SKIP-HISTORY."
  (let ((buf (elark-ai--get-buffer)))
    (with-current-buffer buf
      (elark-ai--ensure-output-fragment)
      (elark-ai-ui-update-fragment
       (elark-ai--frag "output") "Output" content)
      (unless skip-history
        (push (cons "assistant" content)
              (elark-ai-session-history (elark-ai--session))))
      (elark-ai--ensure-input-area))
    (pop-to-buffer buf)
    (elark-ai--scroll-to-output)))

(defun elark-ai--scroll-to-output ()
  "Scroll the AI buffer's window to the output fragment.
Runs from async callbacks, so the selected window may show some
other buffer by the time we get here — `recenter' errors unless
it operates on the window actually displaying the AI buffer.
No-op when the buffer isn't displayed anywhere."
  (when-let* ((buf (get-buffer elark-ai--buf-name))
              (win (get-buffer-window buf t)))
    (with-selected-window win
      (when-let* ((region (elark-ai-ui-find-fragment
                          (elark-ai--frag "output"))))
        (goto-char (car region))
        (forward-line 2)
        (recenter 0)))))

;;;; Entry points
;;
;; `elark-ai-history-truncate-chars' and `elark-ai--build-user-message'
;; live in `elark-ai-protocol.el'.

(defcustom elark-ai-interface 'shell
  "Which UI hosts the AI session for `elark-ai-ask' (and `elark-ai-act').
- `shell'   — the comint-style shell built on shell-maker
              \(`elark-ai-shell').
- `classic' — the fragment-based *Lark AI* buffer."
  :type '(choice (const :tag "shell-maker shell" shell)
                 (const :tag "Classic fragment buffer" classic))
  :group 'elark-ai)

(defun elark-ai--use-shell-p ()
  "Return non-nil when the shell-maker UI should host each turn."
  (eq elark-ai-interface 'shell))

;;;###autoload
(defun elark-ai-ask (prompt)
  "Ask the Lark AI assistant to execute a natural-language PROMPT.
Identifies relevant lark-cli skills (via the LLM by default — see
`elark-ai-skill-routing'), runs the agent loop, and presents results
in the UI selected by `elark-ai-interface'."
  (interactive "sLark AI: ")
  ;; Follow-ups typed inside the classic buffer stay classic, so an
  ;; in-flight classic conversation never jumps UIs mid-stream.
  (if (and (elark-ai--use-shell-p)
           (not (eq (current-buffer) (get-buffer elark-ai--buf-name))))
      (elark-ai-shell-ask prompt)
    (elark-ai--ask-classic prompt)))

(defun elark-ai--ask-classic (prompt)
  "Run PROMPT through the classic fragment-based *Lark AI* buffer."
  (let* ((ai-buf (elark-ai--get-buffer))
         (in-ai-buf (eq (current-buffer) ai-buf))
         (session (elark-ai--session))
         ;; On a follow-up (we are in the AI buffer), reuse the
         ;; originating context — the AI buffer itself has no domain,
         ;; so re-extracting it would lose what the user was viewing
         ;; when they started the conversation.  On a fresh ask from
         ;; a real buffer, capture context from there.
         (context (if in-ai-buf
                      (or (elark-ai-session-context session) "")
                    (elark-ai-context-format)))
         (history (elark-ai-session-history session))
         (prev-skills (elark-ai-session-skills session))
         ;; Build the skill match-text from the originating buffer
         ;; context plus the last plan's command heads + descriptions.
         ;; This lets a non-specific follow-up (\"tell me more\") still
         ;; match domain keywords carried over from the prior turn.
         (last-plan-text
          (when-let* ((plan (elark-ai-session-last-plan session)))
            (mapconcat
             (lambda (s)
               (concat (or (when-let* ((cmd (plist-get s :command)))
                             (string-join cmd " "))
                           "")
                       " "
                       (or (plist-get s :description) "")))
             plan "\n")))
         ;; Routing sees only the head of the context (header lines:
         ;; buffer type, doc title) — a full document body here makes
         ;; the keyword fallback over-match and bloats the LLM router
         ;; call.  The planning/agent calls still get the full context.
         (match-text (mapconcat #'identity
                                (delq nil (list (and (not (string-empty-p context))
                                                     (elark-ai-skills-routing-context
                                                      context))
                                                last-plan-text))
                                "\n")))
    ;; Persist originating context now (fresh ask only) so a follow-up
    ;; doesn't overwrite it with the empty AI-buffer context.
    (unless in-ai-buf
      (setf (elark-ai-session-context session) context))
    ;; Open the turn UI immediately; the skills line is filled in once
    ;; routing resolves (it may require an async LLM call).
    (elark-ai--show-loading prompt nil)
    ;; Step 1: identify which skills the request needs, then plan.
    (elark-ai--select-skills
     prompt match-text
     (lambda (matched-skills)
       ;; Reuse the previous turn's skills as the base set on follow-ups
       ;; so domain continuity is preserved even when this turn picks
       ;; nothing new.  Fresh asks (from a real buffer) start clean.
       (let ((skill-names (delete-dups
                           (if (and in-ai-buf prev-skills)
                               (append prev-skills matched-skills)
                             matched-skills))))
         (elark-ai--debug-log
          "SKILLS" "prev=%S matched=%S → final=%S"
          prev-skills matched-skills skill-names)
         ;; The routing callback may fire with the triggering buffer
         ;; current — fragment updates must target the AI buffer.
         (with-current-buffer (elark-ai--get-buffer)
           (elark-ai-ui-update-fragment
            (elark-ai--frag "skills")
            (concat "Skills: " (string-join skill-names " · "))))
         (elark-ai--run-planning prompt context history session skill-names))))))

(defun elark-ai--run-planning (prompt context history session skill-names)
  "Build the planning prompt from SKILL-NAMES and run plan→execute.
PROMPT, CONTEXT, HISTORY and SESSION carry the turn state.  This is
the second half of `elark-ai-ask', invoked once skills are selected.

When `elark-ai-strategy' is `agent', delegate to the iterative agent
loop instead of the upfront-plan executor."
  (if (eq elark-ai-strategy 'agent)
      (elark-ai-agent--run prompt context history session skill-names)
    (elark-ai--run-planning-1 prompt context history session skill-names)))

(defun elark-ai--run-planning-1 (prompt context history session skill-names)
  "Upfront-plan strategy: plan via the LLM, review, execute, synthesize.
PROMPT, CONTEXT, HISTORY, SESSION and SKILL-NAMES are the turn state."
  (let* ((system-prompt (elark-ai-skills-build-system-prompt skill-names))
         ;; Synthesis pass uses a different system prompt (no JSON
         ;; mandate) so the model produces prose.  Built once here
         ;; from the same skill set so both passes share context.
         (synth-prompt (elark-ai-skills-build-synthesis-prompt skill-names))
         (user-msg (elark-ai--build-user-message prompt context history)))
    (setf (elark-ai-session-system-prompt session) system-prompt
          (elark-ai-session-skills session) skill-names)
    ;; Stream the planning call into the *log* fragment so the user
    ;; sees tokens land instead of staring at "Waiting for LLM
    ;; response...".  HTTP backend silently falls through to a
    ;; non-streaming call inside `elark-ai--call-llm-stream'.
    (elark-ai--progress-log "← LLM planning (streaming)…")
    (elark-ai--call-llm-stream
     system-prompt user-msg
     (lambda (response)
       (elark-ai--progress-log "← LLM planning done (%d chars)"
                              (length response))
       (let ((plan (elark-ai--parse-plan response)))
         (elark-ai--debug-log
          "ASK BRANCH"
          "plan=%s steps=%d all-synthesize=%s"
          (if plan "parsed" "null")
          (length plan)
          (and plan (seq-every-p (lambda (s) (plist-get s :synthesize)) plan)))
         (if (null plan)
             (progn
               ;; Plan parse failed.  Surface a clear error to the
               ;; user (raw response is still in *Lark AI Debug* if
               ;; debug is on) and DO NOT push the raw text into
               ;; history — partial JSON pollutes future turns.
               (elark-ai--progress-log
                "LLM did not return a parseable plan; see *Lark AI Debug*.")
               (elark-ai--mark-all-done)
               (elark-ai--present
                (concat
                 (propertize "Could not parse a plan from the LLM response.\n"
                             'face 'error)
                 "Toggle `M-x elark-ai-toggle-debug' and re-ask to see the raw\
 response, or rephrase the request.")
                'skip-history))
           (setf (elark-ai-session-last-plan session) plan)
           (if (seq-every-p (lambda (s) (plist-get s :synthesize)) plan)
               (progn
                 (elark-ai--progress-log "Pure synthesis — streaming")
                 (elark-ai--mark-all-done)
                 ;; Synth-prompt (no JSON mandate) — otherwise the
                 ;; model would emit another plan JSON instead of the
                 ;; user-facing prose answer.
                 (elark-ai--call-llm-stream
                  synth-prompt
                  (elark-ai--build-user-message
                   (concat prompt
                           "\nRespond with plain text using markdown.")
                   context history)
                  (lambda (text)
                    (when-let* ((buf (get-buffer elark-ai--buf-name)))
                      (with-current-buffer buf
                        (push (cons "assistant" text)
                              (elark-ai-session-history (elark-ai--session)))
                        (elark-ai--ensure-input-area))))))
             (progn
               (elark-ai--display-plan
                plan
                (lambda (confirmed-steps)
                  (elark-ai--progress-log "Executing plan...")
                  (elark-ai--execute-plan
                   confirmed-steps
                   (lambda (results)
                     (if (seq-find (lambda (s) (plist-get s :synthesize))
                                   confirmed-steps)
                         (progn
                           (elark-ai--progress-log "Synthesizing results...")
                           (elark-ai--mark-all-done)
                           ;; Synth-prompt here too — same reason.
                           (elark-ai--synthesize-stream
                            results confirmed-steps synth-prompt))
                       (elark-ai--mark-all-done)
                       (elark-ai--present
                        (mapconcat
                         (lambda (pair)
                           (format "## Step %d\n```\n%s\n```"
                                   (car pair)
                                   (if (cdr pair) (pp-to-string (cdr pair)) "(no data)")))
                         (sort (copy-sequence results)
                               (lambda (a b) (< (car a) (car b))))
                         "\n\n")))))))
               ;; Auto-run past the review gate unless the user wants to
               ;; review the plan first.
               (unless (eq elark-ai-execute-mode 'review)
                 (elark-ai-plan-execute)))))))
     ;; Chunk handler — preview a rolling tail of the streamed plan JSON
     ;; under "Waiting for LLM response…" so the user sees progress.
     (elark-ai--stream-preview-handler))))

;;;; Topic brief — cross-domain context provider
;;
;; `elark-ai-brief-on' is the first workflow built on the context-graph
;; substrate.  Given a TOPIC, it asks every registered provider (v1:
;; docs + messages) for matching snippets, then streams a synthesised
;; brief into the AI chat — so a follow-up like "draft a status update
;; from this" reuses the same gathered context for free.

;;;###autoload
(defun elark-ai-brief-on (topic)
  "Build a cross-domain context brief for TOPIC and synthesise it.
Fans out to every provider in `elark-ai-context-graph-providers' (v1:
docs + messages), aggregates their results, and streams a structured
brief into the AI chat buffer.  Because the gathered context is then
in the chat's conversation history, a follow-up question (\"draft a
status update from this\", \"who owns this work?\") reuses it without
re-fetching."
  (interactive "sBrief on topic: ")
  (let ((topic (string-trim (or topic ""))))
    (when (string-empty-p topic)
      (user-error "Empty topic"))
    (if (and (elark-ai--use-shell-p)
             (not (eq (current-buffer) (get-buffer elark-ai--buf-name))))
        (elark-ai-shell-brief topic)
      (elark-ai--brief-on-classic topic))))

(defun elark-ai--brief-on-classic (topic)
  "Run the TOPIC brief in the classic fragment-based AI buffer."
  (let ((topic (string-trim (or topic ""))))
    (let ((buf (elark-ai--get-buffer))
          (session (elark-ai--session)))
      (elark-ai--show-loading
       (format "Brief on: %s" topic) '("lark-shared"))
      (setf (elark-ai-session-phase session) 'executing)
      (elark-ai--progress-log "Gathering context for \"%s\"…" topic)
      (elark-ai-context-graph-gather
       topic
       (lambda (context-text)
         ;; Cancellation guard: the user may have aborted between hops.
         (let* ((alive (buffer-live-p buf))
                (s (and alive
                        (buffer-local-value 'elark-ai--session buf))))
           (when (and s (eq (elark-ai-session-phase s) 'executing))
             (elark-ai--brief-on-synthesize topic context-text))))))))

(defun elark-ai--brief-on-synthesize (topic context-text)
  "Stream a brief for TOPIC built from CONTEXT-TEXT.
Chunks land directly in the output fragment (classic UI path)."
  (elark-ai--progress-log
   "Gathered %d chars; calling LLM…" (length (or context-text "")))
  (elark-ai--clear-waiting)
  (let ((user-msg (elark-ai--brief-user-message topic context-text)))
    (elark-ai--call-llm-stream
     elark-ai-brief-on-prompt user-msg
     (lambda (text)
       (when-let* ((buf (get-buffer elark-ai--buf-name)))
         (with-current-buffer buf
           (push (cons "assistant" text)
                 (elark-ai-session-history (elark-ai--session)))
           (elark-ai--mark-all-done)
           (elark-ai--ensure-input-area)))))))

;;;###autoload
(defun elark-ai-act ()
  "Perform a context-aware AI action on the item at point.
Offers a menu of actions appropriate to the current domain and item,
then executes the chosen action via `elark-ai-ask'."
  (interactive)
  (let* ((ctx (elark-ai-context))
         (domain (plist-get ctx :domain))
         (item (plist-get ctx :item))
         (summary (plist-get ctx :summary)))
    (unless domain
      (user-error "Not in a Lark buffer"))
    (let* ((actions (elark-ai-act--actions-for domain item))
           (choice (completing-read
                    (format "Action (%s): " summary)
                    (mapcar #'car actions)
                    nil nil nil nil (caar actions)))
           (template (alist-get choice actions nil nil #'equal))
           (prompt (if template
                       (elark-ai-act--expand-template template ctx)
                     choice)))
      (elark-ai-ask prompt))))

(defun elark-ai-act--actions-for (domain item)
  "Return alist of (LABEL . PROMPT-TEMPLATE) for DOMAIN and ITEM.
Templates can use {domain}, {summary}, {id}, and {custom} prompt."
  (append
     ;; Domain-specific actions
     (pcase domain
       ("calendar"
        (append
         '(("Show today's agenda" . "Show my agenda for today")
           ("Check free slots" .
            "Check my free time slots for today")
           ("Suggest meeting time" .
            "Suggest available meeting times this week"))
         (when (plist-get item :event-id)
           `(("Reschedule this event" .
              ,(format "Reschedule event %s. Ask me for the new time."
                       (plist-get item :event-id)))
             ("Add attendee" .
              ,(format "Add an attendee to event %s. Ask me who to add."
                       (plist-get item :event-id)))
             ("Cancel this event" .
              ,(format "Cancel event %s."
                       (plist-get item :event-id)))))))
       ("im"
        (append
         '(("Search messages" .
            "Search messages. Ask me for the keyword."))
         (when (plist-get item :chat-id)
           `(("Summarize this chat" .
              ,(format "Summarize the recent messages in chat %s."
                       (or (plist-get item :chat-name)
                           (plist-get item :chat-id))))
             ("Send a message" .
              ,(format "Send a message to chat %s. Ask me what to say."
                       (or (plist-get item :chat-name)
                           (plist-get item :chat-id))))))
         (when (plist-get item :message-id)
           `(("Reply to this message" .
              ,(format "Draft a reply to message %s in chat %s."
                       (plist-get item :message-id)
                       (or (plist-get item :chat-name)
                           (plist-get item :chat-id))))))))
       ("tasks"
        (append
         '(("Show my tasks" . "List my open tasks")
           ("Create a task" .
            "Create a new task. Ask me for the details."))
         (when (plist-get item :task-id)
           `(("Complete this task" .
              ,(format "Mark task %s as completed."
                       (plist-get item :task-id)))
             ("Set due date" .
              ,(format "Set a due date for task %s. Ask me for the date."
                       (plist-get item :task-id)))
             ("Add subtask" .
              ,(format "Add a subtask to task %s. Ask me for the title."
                       (plist-get item :task-id)))))))
       ("mail"
        (append
         '(("Check inbox" . "Show my recent emails"))
         (when (plist-get item :mail-id)
           `(("Reply to this email" .
              ,(format "Draft a reply to email %s."
                       (plist-get item :mail-id)))
             ("Forward this email" .
              ,(format "Forward email %s. Ask me who to forward to."
                       (plist-get item :mail-id)))))))
       ("docs"
        (append
         (when (plist-get item :doc-token)
           `(("Summarize this document" .
              ,(format "Summarize document %s."
                       (plist-get item :doc-token)))
             ("Extract action items" .
              ,(format "Extract all action items and TODOs from document %s."
                       (plist-get item :doc-token)))
             ("Find related docs" .
              ,(format "Search for documents related to %s."
                       (plist-get item :doc-token)))))
         '(("Search docs" .
            "Search documents. Ask me for the query."))))
       ("drive"
        (when (plist-get item :file-token)
          `(("Download this file" .
             ,(format "Download file %s."
                      (plist-get item :file-token)))
            ("Share this file" .
             ,(format "Share file %s. Ask me who to share with."
                      (plist-get item :file-token))))))
       ("meetings"
        (append
         (when (plist-get item :has-transcript)
           `(("Summarize transcript into a Lark doc" .
              ,(elark-ai--meeting-transcript-summary-prompt item))))
         (when (plist-get item :meeting-id)
           `(("Get meeting notes" .
              ,(format "Fetch meeting notes for meeting %s."
                       (plist-get item :meeting-id)))
             ("Summarize this meeting" .
              ,(format "Summarize meeting %s."
                       (plist-get item :meeting-id)))))))
       ("sheets"
        `(("Read sheet data" .
           ,(format "Read data from spreadsheet %s."
                    (or (plist-get item :token) "this")))
          ("Analyze sheet" .
           ,(format "Analyze the data in spreadsheet %s."
                    (or (plist-get item :token) "this")))))
       ("wiki"
        (append
         (when (plist-get item :space-id)
           `(("List nodes in this space" .
              ,(format "List the wiki nodes in space %s."
                       (plist-get item :space-id)))))
         (when (plist-get item :node-token)
           `(("Summarize this wiki node" .
              ,(format "Summarize the wiki node %s."
                       (plist-get item :node-token)))
             ("Create a node here" .
              ,(format "Create a new wiki node under %s. Ask me for the title."
                       (plist-get item :node-token)))))))
       (_ nil))
     ;; Always available: free-form input
     '(("Other..." . nil))))

(defun elark-ai-act--expand-template (template ctx)
  "Expand TEMPLATE string with context CTX.
If TEMPLATE is nil, prompt for free-form input."
  (if (null template)
      (read-string (format "Action (%s): "
                           (plist-get ctx :summary)))
    template))

;;;; Built-in workflow commands

;;;###autoload
(defun elark-ai-workflow-standup ()
  "Generate a standup report: today's agenda + open tasks."
  (interactive)
  (elark-ai-ask "Generate a standup report for today: show my calendar agenda and open tasks, detect any scheduling conflicts, and list free time slots."))

(defun elark-ai--meeting-transcript-summary-prompt (item)
  "Build the transcript-summarisation prompt for a meeting detail ITEM.
ITEM is the :item plist from the meeting-detail AI context."
  (format "Summarize this meeting's transcript (already provided in the context — do not fetch it) into structured meeting minutes: key topics discussed, decisions made, and action items with owners where identifiable. Start the summary with the meeting's basic meta info from the context (title, meeting ID, time, duration). Write the entire summary in the dominant language of the transcript — the language most of the spoken text is in (e.g. Chinese if the discussion is mostly Chinese), regardless of the language of this instruction. Then create a new Lark doc containing that summary, and give me the doc link. The doc title should also be in that language: \"Meeting Summary: %s\" or its equivalent (e.g. \"会议纪要：%s\" for Chinese)."
          (or (plist-get item :title)
              (plist-get item :meeting-id)
              "Untitled")
          (or (plist-get item :title)
              (plist-get item :meeting-id)
              "Untitled")))

;;;###autoload
(defun elark-ai-workflow-meeting-summary (&optional days)
  "Summarise meetings from the past DAYS days (default 7).
When invoked from a meeting detail buffer whose transcript has
already been fetched, instead summarise that transcript together
with the meeting's basic meta info and create a Lark doc holding
the summary."
  (interactive "P")
  (let* ((ctx (elark-ai-context))
         (item (plist-get ctx :item)))
    (if (and (equal (plist-get ctx :buffer-type) "meeting-detail")
             (plist-get item :has-transcript))
        (elark-ai-ask (elark-ai--meeting-transcript-summary-prompt item))
      (let ((n (or days 7)))
        (elark-ai-ask (format "Summarise my meetings from the past %d days. For each meeting, include the title, time, and any meeting notes available." n))))))

;;;###autoload
(defun elark-ai-workflow-schedule (description)
  "Schedule a meeting described by DESCRIPTION using AI."
  (interactive "sDescribe the meeting: ")
  (elark-ai-ask (format "Schedule a meeting: %s. Find suitable times, check room availability, and create the event." description)))

;;;; UI callbacks for the executors (see `elark-ai--ui')

(setq elark-ai--ui-functions
      (list :session                #'elark-ai--session
            :get-buffer             #'elark-ai--get-buffer
            :frag                   #'elark-ai--frag
            :progress-log           #'elark-ai--progress-log
            :ensure-output-fragment #'elark-ai--ensure-output-fragment
            :present                #'elark-ai--present
            :clear-waiting          #'elark-ai--clear-waiting
            :stream-preview-handler #'elark-ai--stream-preview-handler
            :render-tool-call       #'elark-ai--render-tool-call
            :update-step-status     #'elark-ai--update-step-status
            :synthesize-inline      #'elark-ai--synthesize-inline))

(provide 'elark-ai)
;;; elark-ai.el ends here
