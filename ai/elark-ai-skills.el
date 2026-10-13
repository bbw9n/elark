;;; elark-ai-skills.el --- Skill registry and loader for elark AI layer -*- lexical-binding: t; -*-

;; Copyright (C) 2026 bbw9n

;; Author: bbw9n <bbw9nio@gmail.com>
;; Assisted-by: Claude:claude-opus-5
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Manages the lark-cli SKILL.md corpus that powers the LLM's
;; knowledge.  Parses SKILL.md frontmatter, builds a keyword index,
;; selects relevant skills for a given user prompt, and assembles the
;; system prompt sent to the LLM backend.
;;
;; Skills are loaded from `elark-ai-skills-directory', which defaults
;; to ~/.agents/skills/ (where `npx skills add' installs them).

;;; Code:

(require 'elark-core)

;;;; Customization

(defgroup elark-ai nil
  "Lark AI integration settings."
  :group 'elark
  :prefix "elark-ai-")

(defcustom elark-ai-skills-directory
  (expand-file-name "~/.agents/skills/")
  "Directory containing lark-cli SKILL.md files.
Each skill lives in a subdirectory (e.g., elark-calendar/SKILL.md)."
  :type 'directory
  :group 'elark-ai)

(defcustom elark-ai-skill-routing 'llm
  "How `elark-ai-ask' decides which skills to load for a request.
- `llm'     — ask the LLM to pick relevant skills from the catalogue
              of skill names + descriptions, then load only those.
              Falls back to keyword routing if the LLM returns nothing
              usable.
- `keyword' — match the prompt against the hardcoded
              `elark-ai-skills--routing-table' (the legacy behaviour)."
  :type '(choice (const :tag "LLM router" llm)
                 (const :tag "Keyword routing" keyword))
  :group 'elark-ai)

(defcustom elark-ai-skill-routing-context-chars 600
  "Max characters of buffer context used for skill ROUTING.
Routing (LLM router and keyword fallback alike) only needs the
request plus a hint of where it came from — the context header
lines (buffer type, document title).  Feeding the full context in
bloats the router call and makes keyword routing over-match: a
long document body incidentally mentioning tasks, docs, meetings,
or wikis pulls in every matching domain skill.  The head is kept
because `elark-ai-context-format' puts the discriminative lines
first.  Only routing is clipped — the planning/agent calls still
receive the full context.  nil disables clipping."
  :type '(choice (const :tag "No limit" nil) integer)
  :group 'elark-ai)

(defun elark-ai-skills-routing-context (context)
  "Return CONTEXT clipped to `elark-ai-skill-routing-context-chars'.
Keeps the head (see the defcustom); marks a cut with a trailing
ellipsis.  Returns CONTEXT unchanged when under the limit, the
limit is nil, or CONTEXT is nil/empty."
  (let ((max elark-ai-skill-routing-context-chars))
    (if (or (null max) (null context) (<= (length context) max))
        context
      (concat (substring context 0 max) "…"))))

(defcustom elark-ai-skills-log-lines 10
  "How many leading lines of each skill to show in the *Lark AI Debug* log.
Display-only: the FULL skill body is always sent to the LLM; this just
keeps the debug log readable by abbreviating each `## Skill:' section to
its top N lines.  Set to nil to log skill bodies in full."
  :type '(choice (const :tag "Full body" nil)
                 (integer :tag "Top N lines"))
  :group 'elark-ai)

;;;; Internal state

(defvar elark-ai-skills--index nil
  "Alist of (NAME . (:description DESC :dir DIR :keywords (WORD ...))).")

(defvar elark-ai-skills--cache (make-hash-table :test 'equal)
  "Hash table mapping skill name → cached SKILL.md content string.")

;;;; Frontmatter parsing

(defun elark-ai-skills--parse-frontmatter (text)
  "Parse YAML frontmatter from TEXT.
Returns an alist of (key . value) pairs from the --- delimited block."
  (when (string-match "\\`---\n\\(\\(?:.*\n\\)*?\\)---" text)
    (let ((yaml (match-string 1 text))
          result)
      (dolist (line (split-string yaml "\n" t))
        (when (string-match "^\\([a-z_]+\\):[[:space:]]*\"?\\([^\"]*?\\)\"?[[:space:]]*$" line)
          (push (cons (match-string 1 line) (match-string 2 line)) result)))
      (nreverse result))))

(defun elark-ai-skills--extract-keywords (description)
  "Extract keyword tokens from a skill DESCRIPTION string.
Returns a list of downcased words useful for matching."
  (when description
    (let ((words (split-string (downcase description) "[^a-z0-9\u4e00-\u9fff]+" t)))
      (seq-remove (lambda (w) (< (length w) 2)) words))))

;;;; Index building

(defun elark-ai-skills-refresh ()
  "Scan `elark-ai-skills-directory' and rebuild the skill index."
  (interactive)
  (setq elark-ai-skills--index nil)
  (clrhash elark-ai-skills--cache)
  (let ((dir (file-name-as-directory elark-ai-skills-directory)))
    (when (file-directory-p dir)
      (dolist (entry (directory-files dir t "^elark-"))
        (when (file-directory-p entry)
          (let ((skill-file (expand-file-name "SKILL.md" entry)))
            (when (file-exists-p skill-file)
              (let* ((text (with-temp-buffer
                             (insert-file-contents skill-file)
                             (buffer-string)))
                     (meta (elark-ai-skills--parse-frontmatter text))
                     (name (or (cdr (assoc "name" meta))
                               (file-name-nondirectory entry)))
                     (desc (or (cdr (assoc "description" meta)) "")))
                (push (cons name (list :description desc
                                       :dir entry
                                       :keywords (elark-ai-skills--extract-keywords desc)))
                      elark-ai-skills--index))))))))
  (elark--log "AI skills: indexed %d skills" (length elark-ai-skills--index))
  elark-ai-skills--index)

(defun elark-ai-skills--ensure-index ()
  "Ensure the skill index is built; build it on first use."
  (unless elark-ai-skills--index
    (elark-ai-skills-refresh)))

;;;; Skill loading

(defun elark-ai-skills-load (name)
  "Load and return the SKILL.md content for skill NAME as a string.
Results are cached in `elark-ai-skills--cache'."
  (elark-ai-skills--ensure-index)
  (or (gethash name elark-ai-skills--cache)
      (let ((entry (cdr (assoc name elark-ai-skills--index))))
        (when entry
          (let* ((dir (plist-get entry :dir))
                 (file (expand-file-name "SKILL.md" dir))
                 (text (when (file-exists-p file)
                         (with-temp-buffer
                           (insert-file-contents file)
                           (buffer-string)))))
            (when text
              (puthash name text elark-ai-skills--cache))
            text)))))

(defun elark-ai-skills-list ()
  "Return list of all available skill names."
  (elark-ai-skills--ensure-index)
  (mapcar #'car elark-ai-skills--index))

;;;; Skill selection

(defconst elark-ai-skills--routing-table
  '(("calendar\\|agenda\\|meeting\\|schedule\\|event\\|freebusy\\|rsvp\\|日[历程]"
     "lark-calendar")
    ("message\\|chat\\|send\\|im\\|群\\|消息\\|聊天"
     "lark-im")
    ("task\\|todo\\|待办\\|任务"
     "lark-task")
    ("standup\\|daily\\|开工\\|今天.*安排\\|what.*plate"
     "lark-task" "lark-calendar" "lark-workflow-standup-report")
    ("meeting.*\\(note\\|summar\\|minute\\|纪要\\|总结\\)"
     "lark-vc" "lark-workflow-meeting-summary" "lark-doc")
    ("doc\\|document\\|文档"
     "lark-doc" "lark-drive")
    ("mail\\|email\\|inbox\\|邮件\\|邮箱"
     "lark-mail")
    ("sheet\\|spreadsheet\\|表格"
     "lark-sheets")
    ("base\\|bitable\\|多维表"
     "lark-base")
    ("drive\\|file\\|upload\\|download\\|云空间"
     "lark-drive")
    ("wiki\\|知识库"
     "lark-wiki")
    ("contact\\|user\\|search.*people\\|通讯录\\|联系人"
     "lark-contact")
    ("whiteboard\\|画板"
     "lark-whiteboard")
    ("approval\\|审批"
     "lark-approval")
    ("video.*conference\\|vc\\|录制\\|recording"
     "lark-vc"))
  "Routing table: (REGEXP . SKILL-NAMES).
Used to select relevant skills based on user prompt keywords.")

(defun elark-ai-skills-select (prompt &optional context)
  "Select relevant skill names for the user PROMPT.
CONTEXT is optional extra text (originating buffer summary, last
plan commands, etc.) appended to the prompt before matching, so
follow-ups stay on-topic when the new prompt itself is non-specific
\(\"tell me more\", \"summarize that\").
Always includes lark-shared.  Never falls back to loading every
domain skill — if no rule matches, only lark-shared is returned;
callers that want continuity (e.g. follow-ups) should union the
result with the previous turn's selection.
Returns a deduplicated list of skill names."
  (elark-ai-skills--ensure-index)
  (let* ((match-text (downcase
                      (concat prompt
                              (when (and context
                                         (not (string-empty-p context)))
                                (concat "\n" context)))))
         (selected (list "lark-shared"))
         (available (mapcar #'car elark-ai-skills--index)))
    (dolist (rule elark-ai-skills--routing-table)
      (when (string-match-p (car rule) match-text)
        (dolist (skill (cdr rule))
          (when (member skill available)
            (push skill selected)))))
    (delete-dups (nreverse selected))))

;;;; LLM-based skill selection
;;
;; Primary router (`elark-ai-skill-routing' = `llm'): instead of matching
;; the prompt against the hardcoded `elark-ai-skills--routing-table', show
;; the LLM the catalogue of available skills (name + description) and let
;; it pick the relevant ones.  These helpers are pure — the actual async
;; LLM call and the keyword fallback live in `elark-ai.el'.

(defun elark-ai-skills--catalog ()
  "Return a text catalogue of available skills for the LLM router.
One line per indexed skill: \"- NAME: DESCRIPTION\"."
  (elark-ai-skills--ensure-index)
  (mapconcat
   (lambda (entry)
     (format "- %s: %s"
             (car entry)
             (or (plist-get (cdr entry) :description) "")))
   elark-ai-skills--index
   "\n"))

(defun elark-ai-skills-build-router-prompt ()
  "Build the system prompt that asks the LLM to pick relevant skills.
The model sees the skill catalogue and must reply with a JSON object
of the form {\"skills\": [NAME, ...]}."
  (concat
   "You are a skill router for a Lark/Feishu assistant integrated into Emacs.\n"
   "Given the user's request (and any context), decide which skills are\n"
   "needed to fulfil it.  Choose only from the catalogue below, by exact\n"
   "name.  Prefer the smallest set that covers the request — include a\n"
   "skill only when it is clearly relevant.  Returning an empty list is\n"
   "fine when no domain skill applies.\n\n"
   "## Available skills\n"
   (elark-ai-skills--catalog)
   "\n\n## Response format\n"
   "Respond with ONLY a JSON object, no prose, no markdown fences:\n"
   "{\"skills\": [\"lark-calendar\", \"lark-task\"]}"))

(defun elark-ai-skills--validate-selection (names)
  "Filter NAMES to skill names present in the index.
NAMES is a list of strings (e.g. the LLM router's picks).  Returns a
deduplicated list preserving order, with unknown names (hallucinated
or misspelled) dropped.  Returns nil when nothing valid remains."
  (elark-ai-skills--ensure-index)
  (let ((available (mapcar #'car elark-ai-skills--index)))
    (delete-dups
     (seq-filter (lambda (n) (and (stringp n) (member n available)))
                 (if (listp names) names (list names))))))

;;;; System prompt assembly

(defun elark-ai-skills--identity-preamble ()
  "Return the identity, time, and persona shared by both prompt passes.
Excludes any output-format rules so it is safe to reuse on the
synthesis pass, where the model must produce prose, not JSON."
  (format "You are a Lark/Feishu assistant integrated into Emacs.
You help the user by planning and executing lark-cli commands.

Current date/time: %s
Timezone: %s
Default identity: %s
"
          (format-time-string "%Y-%m-%dT%H:%M:%S%z")
          (format-time-string "%Z")
          (or elark-default-identity "user")))

(defun elark-ai-skills--planning-rules ()
  "Output-format rules that mandate JSON plans.
Only included in the planning system prompt — never in the
synthesis prompt, since that pass must emit markdown prose."
  "## Response Format

You MUST respond with a JSON object containing a plan — a list of steps to execute.
Each step is an object with these fields:
- \"command\": array of strings (lark-cli arguments), or null for synthesis steps
- \"description\": human-readable description of what this step does
- \"side_effect\": boolean, true if this step writes/modifies data
- \"parallel_group\": optional integer, steps with the same group run in parallel
- \"synthesize\": boolean, true if this step is an AI synthesis/summary step
- \"synthesis_instruction\": string, instruction for the synthesis (only when synthesize=true)

Example response:
{
  \"plan\": [
    {\"command\": [\"calendar\", \"+agenda\"], \"description\": \"Fetch today's agenda\", \"side_effect\": false, \"parallel_group\": 1},
    {\"command\": [\"task\", \"+get-my-tasks\"], \"description\": \"Fetch open tasks\", \"side_effect\": false, \"parallel_group\": 1},
    {\"command\": null, \"description\": \"Summarize schedule and tasks\", \"side_effect\": false, \"synthesize\": true, \"synthesis_instruction\": \"Combine the agenda and tasks into a standup summary. Detect time conflicts. List free slots.\"}
  ]
}

Rules:
- Only use lark-cli commands documented in the skills below.
- Mark any create/update/delete/send operation as side_effect: true.
- Use parallel_group to indicate steps that can run concurrently.
- Later steps can reference earlier results using $step-N placeholders:
  - $step-0 — the full JSON result of step 0 (as a string)
  - $step-0.field — a top-level field from step 0's result
  - $step-0.field.subfield — nested field access
  - $step-0.items[0].id — array index then field
  - $step-0.items[*].id — collect field from all array elements, joined by comma
  Example: {\"command\": [\"calendar\", \"+create\", \"--attendee-ids\", \"$step-0.items[*].open_id\"]}
- If the user's request is a simple question that needs no CLI calls, return a plan with a single synthesize step.
- If document content is already provided in the context, do NOT add a fetch step — use the provided content directly in your synthesis.
")

(defun elark-ai-skills--synthesis-rules ()
  "Output-format rules for the synthesis pass.
Tells the model to answer in markdown prose, explicitly
overriding any planning instructions it may have seen earlier in
the conversation."
  "## Response Format

Respond with markdown prose — NOT JSON, NOT a plan.  The planning
phase is finished; you are now writing the user-facing answer.
Use the skills below as reference for facts and capabilities, but
do not emit a JSON plan and do not propose further steps.
")

(defun elark-ai-skills--producer-rules ()
  "Output-format rules for a synthesis step that PRODUCES content.
Used when a synthesis step's result is interpolated into a later
command via $step-N — the output is fed verbatim as a command
argument, so it must be the bare artifact, not a user-facing answer."
  "## Response Format

You are producing content that will be passed verbatim as an argument to a
later command — this is NOT a user-facing answer.  Output ONLY the
requested content artifact: no preamble, no commentary, no explanation,
and no surrounding markdown code fences.  Produce exactly the content in
the format the instruction specifies.
")

(defun elark-ai-skills--agent-rules ()
  "Output-format rules for the iterative agent loop.
The model emits ONE JSON action per turn and is shown each command's
output before deciding the next action.  Crucially, content the model
must generate (a document body, a message) is written directly into the
command arguments — there is no $step-N variable substitution here, so
producer-then-consumer dependencies are handled inside one action."
  "## Response Format

You operate as an agent in a loop.  Each turn, respond with EXACTLY ONE
JSON object — no prose, no markdown fences — describing your next action.

To run a lark-cli command:
{\"thought\": \"why this command\", \"action\": \"command\", \"command\": [\"docs\", \"+create\", \"--api-version\", \"v2\", \"--content\", \"<the literal content>\"], \"side_effect\": true}

To finish and answer the user:
{\"thought\": \"why I'm done\", \"action\": \"final\", \"answer\": \"<markdown answer for the user>\"}

Rules:
- ONE action per turn.  After a \"command\" you will be shown its output as an
  observation, then asked for the next action.  Keep going until the request
  is fully handled, then emit \"final\".
- \"command\" is an array of lark-cli argument strings.  There is NO variable
  substitution: put real, literal values directly in the arguments.  If a
  command needs content you must generate (a document body, a summary, a
  message), WRITE that content yourself and place it directly in the argument.
- Set \"side_effect\": true for any create/update/delete/send command.
- Only use commands documented in the skills below.
- If document or chat content is already provided in the context, use it
  directly — do not fetch it again.
- Do not stop after a single command if more steps are needed to finish.
")

(defun elark-ai-skills--preamble ()
  "Backwards-compatible alias for the planning preamble.
Kept so older callers continue to work; new code should call
`elark-ai-skills-build-system-prompt' or
`elark-ai-skills-build-synthesis-prompt' directly."
  (concat (elark-ai-skills--identity-preamble)
          "\n"
          (elark-ai-skills--planning-rules)))

(defun elark-ai-skills--head-lines (text n)
  "Return the first N lines of TEXT.  When N is nil, return TEXT unchanged."
  (if (null n)
      text
    (mapconcat #'identity
               (seq-take (split-string text "\n") n)
               "\n")))

(defun elark-ai-skills--assemble (preamble skill-names)
  "Concatenate PREAMBLE with the full content of each skill in SKILL-NAMES."
  (let ((parts (list preamble)))
    (dolist (name skill-names)
      (let ((content (elark-ai-skills-load name)))
        (when content
          (push (format "\n---\n## Skill: %s\n\n%s" name content) parts))))
    (mapconcat #'identity (nreverse parts) "\n")))

(defun elark-ai-skills-abbreviate-for-log (text)
  "Abbreviate each `## Skill:' section of TEXT for debug-log display.
Keeps each skill's header plus its first `elark-ai-skills-log-lines' lines
and drops the rest behind a marker.  The preamble and any non-skill text
are left intact.  Returns TEXT unchanged when `elark-ai-skills-log-lines'
is nil.  This is display-only — the full bodies are still sent to the LLM."
  (if (null elark-ai-skills-log-lines)
      text
    (let ((n elark-ai-skills-log-lines)
          (in-skill nil)
          (kept 0)
          (truncated nil)
          out)
      (dolist (line (split-string text "\n"))
        (cond
         ((string-prefix-p "## Skill: " line)
          (setq in-skill t kept 0 truncated nil)
          (push line out))
         ((not in-skill)
          (push line out))
         ((< kept n)
          (setq kept (1+ kept))
          (push line out))
         ((not truncated)
          (setq truncated t)
          (push "  …(skill body truncated in log; full text sent to LLM)…" out))
         (t nil)))
      (mapconcat #'identity (nreverse out) "\n"))))

(defun elark-ai-skills-build-system-prompt (skill-names)
  "Build the planning system prompt from SKILL-NAMES.
Includes the JSON-plan response-format mandate."
  (elark-ai-skills--assemble
   (concat (elark-ai-skills--identity-preamble)
           "\n"
           (elark-ai-skills--planning-rules))
   skill-names))

(defun elark-ai-skills-build-synthesis-prompt (skill-names)
  "Build the synthesis system prompt from SKILL-NAMES.
Identity + skills only — no JSON-plan mandate, so the model is
free to answer in markdown prose during the synthesis pass."
  (elark-ai-skills--assemble
   (concat (elark-ai-skills--identity-preamble)
           "\n"
           (elark-ai-skills--synthesis-rules))
   skill-names))

(defun elark-ai-skills-build-producer-prompt (skill-names)
  "Build the producer system prompt from SKILL-NAMES.
Identity + skills + a rule mandating bare-artifact output, for a
synthesis step whose result feeds a later command via $step-N."
  (elark-ai-skills--assemble
   (concat (elark-ai-skills--identity-preamble)
           "\n"
           (elark-ai-skills--producer-rules))
   skill-names))

(defun elark-ai-skills-build-agent-prompt (skill-names)
  "Build the agent-loop system prompt from SKILL-NAMES.
Identity + skills + the one-action-per-turn ReAct protocol."
  (elark-ai-skills--assemble
   (concat (elark-ai-skills--identity-preamble)
           "\n"
           (elark-ai-skills--agent-rules))
   skill-names))

(provide 'elark-ai-skills)
;;; elark-ai-skills.el ends here
