;;; elark-ai-protocol.el --- Pure helpers for the elark AI layer -*- lexical-binding: t; -*-

;; Copyright (C) 2026 bbw9n

;; Author: bbw9n <bbw9nio@gmail.com>
;; Assisted-by: Claude:claude-opus-5
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Pure (UI-free, session-free) helpers used by the AI layer:
;;
;;   - debug logging          — `elark-ai-debug', `elark-ai--debug-log',
;;                              `elark-ai-toggle-debug', `elark-ai-show-debug'
;;   - plan JSON parsing      — `elark-ai--parse-plan', `elark-ai--extract-json'
;;   - $step-N interpolation  — `elark-ai--interpolate-cmd' and helpers
;;   - user-message assembly  — `elark-ai--build-user-message'
;;   - UI callback table      — `elark-ai--ui', `elark-ai--ui-functions'
;;
;; This module has no UI dependency and no knowledge of the AI
;; session struct.  It is the testable foundation under
;; `elark-ai-llm.el', `elark-ai-runner.el', and `elark-ai.el'.

;;; Code:

(require 'cl-lib)
(require 'json)

;;;; UI callbacks
;;
;; The executors (agent, runner, llm, acp, context-graph) report
;; progress and results through `elark-ai--ui' instead of calling
;; `elark-ai' directly: `elark-ai' requires them, so direct calls back
;; would make a circular dependency.  `elark-ai' fills the table when
;; it loads.

(defvar elark-ai--frontend nil
  "UI callbacks overriding the classic *Lark AI* buffer for a turn.
nil means the classic fragment-based UI.  An alternative front-end
\(e.g. `elark-ai-shell') sets this for the duration of its turn to a
plist of functions:
  :progress-log     (FMT &rest ARGS)
  :render-tool-call (ITER CMD STATUS)
  :stream-preview   () → chunk handler (or nil)
  :clear-waiting    ()
  :present          (CONTENT SKIP-HISTORY)
Only one turn can be in flight at a time, which the owning
front-end is responsible for enforcing.")

(defconst elark-ai--buf-name "*Lark AI*"
  "Name of the AI buffer.")

(defvar elark-ai--ui-functions nil
  "Plist mapping UI operations to the functions implementing them.
Set by `elark-ai'.  Keys and the arguments they take:
  :session                ()
  :get-buffer             ()
  :frag                   (SUFFIX)
  :progress-log           (FMT &rest ARGS)
  :ensure-output-fragment ()
  :present                (CONTENT &optional SKIP-HISTORY)
  :clear-waiting          ()
  :stream-preview-handler ()
  :render-tool-call       (ITER CMD STATUS)
  :update-step-status     (INDEX STATUS)
  :synthesize-inline      (STEP DONE-FN)")

(defun elark-ai--ui (op &rest args)
  "Call the UI function registered for OP with ARGS.
Return its value, or nil when nothing is registered for OP."
  (declare (indent 1))
  (when-let* ((fn (plist-get elark-ai--ui-functions op)))
    (apply fn args)))

;;;; Debug logging

(defcustom elark-ai-debug nil
  "When non-nil, log LLM requests, responses, and plan parsing.
Entries go to the `*Lark AI Debug*' buffer.  Toggle interactively with
`elark-ai-toggle-debug'."
  :type 'boolean
  :group 'elark-ai)

(defconst elark-ai--debug-buf-name "*Lark AI Debug*"
  "Name of the debug buffer.")

(define-derived-mode elark-ai-debug-mode special-mode "Lark AI Debug"
  "Major mode for the Lark AI debug log.")

(defun elark-ai--debug-buffer ()
  "Get or create the debug buffer."
  (let ((buf (get-buffer-create elark-ai--debug-buf-name)))
    (with-current-buffer buf
      (unless (derived-mode-p 'elark-ai-debug-mode)
        (elark-ai-debug-mode)))
    buf))

(defun elark-ai--debug-log (label fmt &rest args)
  "Append a labelled entry to the debug buffer.
LABEL is a short tag (e.g. \"REQUEST\", \"RESPONSE\").  FMT and
ARGS are passed to `format'.  No-op when `elark-ai-debug' is nil."
  (when elark-ai-debug
    (let ((entry (apply #'format fmt args)))
      (with-current-buffer (elark-ai--debug-buffer)
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (insert (format "\n=== [%s] %s ===\n%s\n"
                          (format-time-string "%H:%M:%S.%3N")
                          label entry)))))))

(defun elark-ai--format-cli-result (result)
  "Render a lark-cli step RESULT for the debug log.
RESULT is parsed JSON (alist/list) or a raw string.  JSON is
pretty-printed; strings pass through; nil renders as a placeholder."
  (cond
   ((null result) "(no result)")
   ((stringp result) result)
   (t (condition-case nil
          (with-temp-buffer
            (insert (json-encode result))
            (json-pretty-print-buffer)
            (buffer-string))
        (error (format "%S" result))))))

;;;###autoload
(defun elark-ai-toggle-debug ()
  "Toggle Lark AI debug logging.
When on, every LLM request/response and plan parse is logged to
`*Lark AI Debug*'."
  (interactive)
  (setq elark-ai-debug (not elark-ai-debug))
  (message "Lark AI debug: %s" (if elark-ai-debug "ON" "OFF"))
  (when elark-ai-debug
    (display-buffer (elark-ai--debug-buffer))))

;;;###autoload
(defun elark-ai-show-debug ()
  "Pop up the Lark AI debug buffer."
  (interactive)
  (display-buffer (elark-ai--debug-buffer)))

;;;; Plan data structure
;;
;; A plan is a list of step plists with these keys:
;;   :index                - integer (0-based, assigned during parse)
;;   :command              - list of strings (lark-cli args), or nil for synthesis
;;   :description          - string
;;   :side-effect          - boolean
;;   :parallel-group       - integer or nil
;;   :synthesize           - boolean
;;   :synthesis-instruction - string or nil

(defun elark-ai--parse-plan (json-string)
  "Parse a plan from the LLM's JSON-STRING response.
Returns a list of step plists, or nil on parse failure.  Pure:
emits debug entries via `elark-ai--debug-log' but does not touch
any UI buffer."
  (elark-ai--debug-log "PARSE-PLAN INPUT" "%s"
                      (if (stringp json-string) json-string
                        (format "%S" json-string)))
  (condition-case err
      (let* ((response (if (stringp json-string)
                           (elark-ai--extract-json json-string)
                         json-string))
             (plan-data (alist-get 'plan response))
             (steps
              (when (and plan-data (listp plan-data))
                (seq-map-indexed
                 (lambda (step idx)
                   (list :index idx
                         :command (let ((cmd (alist-get 'command step)))
                                    (when (and cmd (not (eq cmd :null)))
                                      (if (listp cmd) cmd (list cmd))))
                         :description (or (alist-get 'description step) "")
                         :side-effect (eq (alist-get 'side_effect step) t)
                         :parallel-group (let ((pg (alist-get 'parallel_group step)))
                                           (when (numberp pg) pg))
                         :synthesize (eq (alist-get 'synthesize step) t)
                         :synthesis-instruction (alist-get 'synthesis_instruction step)))
                 plan-data))))
        (elark-ai--debug-log
         "PARSE-PLAN RESULT"
         "extracted-keys=%S plan-key-type=%s steps=%d"
         (and (listp response) (mapcar #'car response))
         (cond ((null plan-data) "missing")
               ((listp plan-data) "list")
               (t (format "%s" (type-of plan-data))))
         (length steps))
        steps)
    (error
     (elark-ai--debug-log "PARSE-PLAN ERROR" "%s" (error-message-string err))
     nil)))

(defun elark-ai--extract-json (text)
  "Extract a JSON object from TEXT which may contain markdown fences.
Return nil when TEXT is not a string or holds no valid JSON — LLM
output routinely isn't JSON, so that is not an error."
  (when (stringp text)
    (let ((json-str text))
    ;; Strip markdown code fences (handle no newline after lang tag)
    (when (string-match "```[a-z]*[\n ]?\\(\\(?:.\\|\n\\)*?\\)[\n ]?```" json-str)
      (setq json-str (match-string 1 json-str)))
    ;; Find the outermost { ... }
    (when (string-match "\\({\\(?:.\\|\n\\)*}\\)" json-str)
      (setq json-str (match-string 1 json-str)))
      (condition-case nil
          (json-parse-string json-str :object-type 'alist :array-type 'list)
        (json-parse-error nil)))))

;;;; $step-N interpolation

(defun elark-ai--interpolate-cmd (cmd-args results)
  "Replace $step-N references in CMD-ARGS using RESULTS.
RESULTS is an alist of (index . parsed-json).
Supports:
  $step-0           — full result as JSON string
  $step-0.field     — top-level field
  $step-0.a.b       — nested field access
  $step-0.items[0].id — array index + field
  $step-0.items[*].id — collect from all elements"
  (mapcar #'elark-ai--interpolate-arg
          (mapcar (lambda (arg) (cons arg results)) cmd-args)))

(defun elark-ai--interpolate-arg (arg-and-results)
  "Interpolate a single ARG-AND-RESULTS (ARG . RESULTS) pair."
  (let ((arg (car arg-and-results))
        (results (cdr arg-and-results)))
    (if (not (string-match-p "\\$step-[0-9]" arg))
        arg
      ;; Process all $step-N references in this arg
      (let ((pos 0)
            (out ""))
        (while (string-match
                "\\$step-\\([0-9]+\\)\\(\\(?:[.[][^[:space:]]*\\)?\\)"
                arg pos)
          (let* ((match-beg (match-beginning 0))
                 (match-end (match-end 0))
                 (idx (string-to-number (match-string 1 arg)))
                 (path (match-string 2 arg))
                 (data (alist-get idx results))
                 (replacement
                  (if (null data)
                      (substring arg match-beg match-end)
                    (if (or (null path) (string-empty-p path))
                        (if (stringp data) data
                          (json-encode data))
                      (let ((val (elark-ai--resolve-path
                                  data (substring path 1))))
                        (cond
                         ((null val) "")
                         ((stringp val) val)
                         ((numberp val) (number-to-string val))
                         (t (json-encode val))))))))
            (setq out (concat out
                              (substring arg pos match-beg)
                              replacement)
                  pos match-end)))
        (concat out (substring arg pos))))))

(defun elark-ai--resolve-path (data path)
  "Resolve a dot/bracket PATH against DATA.
PATH examples: \"field\", \"a.b\", \"items[0].id\",
\"items[*].id\"."
  (let ((segments (elark-ai--parse-path path))
        (current data))
    (catch 'done
      (dolist (seg segments)
        (when (null current) (throw 'done nil))
        (pcase seg
          (`(index ,n)
           (if (and (listp current) (<= 0 n) (< n (length current)))
               (setq current (nth n current))
             (throw 'done nil)))
          (`(wildcard ,field)
           ;; Collect field from every element in the list
           (throw 'done
                  (if (listp current)
                      (mapconcat
                       (lambda (item)
                         (let ((v (elark-ai--resolve-path item field)))
                           (cond
                            ((null v) "")
                            ((stringp v) v)
                            ((numberp v) (number-to-string v))
                            (t (json-encode v)))))
                       current ",")
                    nil)))
          (`(field ,name)
           (setq current
                 (if (listp current)
                     (alist-get (intern name) current)
                   nil)))))
      current)))

;;;; Step-reference detection

(defun elark-ai--step-referenced-p (idx steps)
  "Return non-nil when some plan step interpolates $step-IDX.
STEPS is the plan's step list.
Scans each step's `:command' args for a $step-IDX reference.  Used to
decide whether a synthesis step must produce its text inline (because a
later step consumes it) or can be deferred to the final answer pass.
Matches IDX exactly: $step-1 does not match $step-10."
  (let ((re (format "\\$step-%d\\b" idx)))
    (seq-some
     (lambda (step)
       (seq-some (lambda (arg)
                   (and (stringp arg) (string-match-p re arg)))
                 (plist-get step :command)))
     steps)))

(defun elark-ai--parse-path (path)
  "Parse PATH into a list of segments.
Returns list of (field NAME), (index N), or (wildcard REST)."
  (let ((segments nil)
        (remaining path))
    (while (and remaining (not (string-empty-p remaining)))
      (cond
       ;; [*].rest — wildcard collect
       ((string-match "^\\[\\*\\]\\.?" remaining)
        (let ((rest (substring remaining (match-end 0))))
          (push (list 'wildcard (if (string-empty-p rest) nil rest))
                segments)
          (setq remaining nil)))
       ;; [N] — array index
       ((string-match "^\\[\\([0-9]+\\)\\]\\.?" remaining)
        (push (list 'index (string-to-number (match-string 1 remaining)))
              segments)
        (setq remaining (substring remaining (match-end 0))))
       ;; field name (up to next . or [)
       ((string-match "^\\([^.[]+\\)\\.?" remaining)
        (push (list 'field (match-string 1 remaining)) segments)
        (setq remaining (substring remaining (match-end 0))))
       (t (setq remaining nil))))
    (nreverse segments)))

;;;; Session struct
;;
;; Defined here (rather than in `elark-ai.el') so that
;; `elark-ai-runner.el' can use the auto-generated accessors and
;; `setf' forms at byte-compile time without depending on the UI
;; module.  The struct is the union of conversation state, plan
;; state and UI state — fields are buffer-local-friendly (e.g.
;; markers) but the definition itself is pure data.

(cl-defstruct elark-ai-session
  (turn 0)                ; current conversation turn (1-indexed when active)
  (phase 'idle)           ; idle | loading | review | executing | done
  (history nil)           ; ((ROLE . CONTENT) ...) — newest first
  (context "")            ; originating-buffer context string
  (skills nil)            ; skill names selected for this conversation
  (system-prompt nil)     ; full system prompt last sent
  (last-plan nil)         ; last parsed plan (for skill match-text)
  (steps nil)             ; current plan steps
  (callback nil)          ; pending callback for plan-execute
  (step-status nil)       ; alist (INDEX . STATUS) — pending/running/done/skipped
  (step-results nil)      ; alist (INDEX . RESULT) from execution
  (agent-steps nil)       ; agent loop transcript: list of (ACTION-alist . OBSERVATION)
  (input-region-start nil) ; marker at start of the input region (separator)
  (input-start nil))      ; marker at start of editable input body

;;;; User message assembly

(defcustom elark-ai-history-truncate-chars 800
  "Maximum characters retained per prior assistant message.
Applies to the follow-up prompt.  Long plan JSON or full document dumps from
earlier turns are clipped to this length so they don't flood the
LLM and bias it into re-running the previous plan."
  :type 'integer
  :group 'elark-ai)

(defun elark-ai--build-user-message (prompt context history)
  "Build the LLM user message for PROMPT with originating CONTEXT.
Uses a labelled, sectioned structure so the new question is the
focus — prior turns and originating-buffer context are framed as
background.  HISTORY is the session history (newest first); long
assistant entries are truncated per `elark-ai-history-truncate-chars'
to keep the previous plan JSON from biasing the LLM into repeating
itself."
  (let* ((clip
          (lambda (text)
            (if (> (length text) elark-ai-history-truncate-chars)
                (concat (substring text 0 elark-ai-history-truncate-chars)
                        "\n…[truncated]")
              text)))
         (history-block
          (when history
            (mapconcat
             (lambda (entry)
               (let ((role (car entry))
                     (text (cdr entry)))
                 (format "- %s: %s"
                         (capitalize role)
                         ;; User prompts are typically short — only
                         ;; clip assistant content.
                         (if (equal role "assistant")
                             (funcall clip text)
                           text))))
             (reverse history)
             "\n")))
         (sections nil))
    (when history-block
      (push (concat "## Prior turns (background only)\n" history-block)
            sections))
    (when (and context (not (string-empty-p context)))
      (push (concat "## Originating buffer context\n" context)
            sections))
    (push (concat "## Current request\n" prompt
                  (when history
                    "\n\nAddress only the request above.  Do not\
 repeat prior plan steps unless the user explicitly asks; if the\
 question is a clarification or refinement of earlier results,\
 answer it directly with a synthesis step instead of re-fetching."))
          sections)
    (mapconcat #'identity (nreverse sections) "\n\n")))

(provide 'elark-ai-protocol)
;;; elark-ai-protocol.el ends here
