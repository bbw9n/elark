;;; elark-ai-context-graph.el --- Cross-domain topic context provider -*- lexical-binding: t; -*-

;; Copyright (C) 2026 bbw9n

;; Author: bbw9n <bbw9nio@gmail.com>
;; Assisted-by: Claude:claude-opus-5
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The substrate for "connect the dots" workflows: given a TOPIC string,
;; fan out to every registered provider (one per Lark domain), collect
;; their results concurrently, and hand a single markdown blob back to
;; the caller for LLM synthesis.
;;
;; v1 ships with two providers — Docs (`lark-cli docs +search') and
;; Messages (`lark-cli im +messages-search') — but the registry
;; (`elark-ai-context-graph-providers') is open: any function with
;; signature (TOPIC BUDGET DONE-FN) can be added, and the gatherer
;; will divide the budget proportionally and stitch the section into
;; the output in registration order.
;;
;; The gatherer never errors out a brief because of one bad provider:
;; a failing CLI call is logged and silently contributes nothing.
;; That lets new providers be experimental without breaking the rest.

;;; Code:

(require 'cl-lib)
(require 'elark-core)
(require 'elark-ai-protocol)   ; `elark-ai--ui'

;;;; Customization

(defcustom elark-ai-context-graph-budget-chars 100000
  "Soft upper bound (chars) for the gathered context across all providers.
Divided proportionally across enabled providers; each provider may
clip its own section to stay under its share.  Set higher to feed
the LLM more material at the cost of slower synthesis and more
tokens; set lower to keep briefs lean."
  :type 'integer
  :group 'elark-ai)

(defcustom elark-ai-context-graph-result-cap 8
  "Maximum results requested per provider.
Passed through as `--page-size' to the underlying lark-cli search.
The provider may still clip further to respect its budget share."
  :type 'integer
  :group 'elark-ai)

(defcustom elark-ai-context-graph-summary-snippet-chars 240
  "How much of each result's summary/content to keep in the brief.
Per-item clipping is independent of the per-provider budget — small
snippets let more results fit; larger snippets give the LLM more to
work with on each match."
  :type 'integer
  :group 'elark-ai)

(defvar elark-ai-context-graph-providers
  '(("Docs"     . elark-ai-context-graph--docs-provider)
    ("Messages" . elark-ai-context-graph--im-provider))
  "Alist of (SECTION-LABEL . PROVIDER-FN) used by the gatherer.

Each PROVIDER-FN has signature (TOPIC BUDGET DONE-FN):
- TOPIC    — the user-supplied search string
- BUDGET   — soft chars cap for this provider's section
- DONE-FN  — invoked exactly once with the formatted markdown text,
             or nil to skip the section entirely.

Order is preserved in the rendered brief.  Add new providers by
`add-to-list' or `setq' — providers that error out are skipped
silently so experimental additions can't break the workflow.")

;;;; HTML / text helpers

(defun elark-ai-context-graph--strip-html (s)
  "Strip <h>...</h> highlight tags and decode HTML entities from S.
Handles both the common named entities (`&amp;', `&quot;', …) and any
numeric entity (`&#NN;') — Lark search results contain a mix of both,
and a missed entity reads as garbage to a human (and confuses the LLM
about whether it's prose or markup)."
  (when (stringp s)
    (let ((out (replace-regexp-in-string "</?h>" "" s)))
      (dolist (pair '(("&amp;" . "&") ("&lt;" . "<") ("&gt;" . ">")
                      ("&quot;" . "\"") ("&apos;" . "'")
                      ("&#39;" . "'") ("&nbsp;" . " ")))
        (setq out (replace-regexp-in-string
                   (regexp-quote (car pair)) (cdr pair) out t t)))
      ;; Generic numeric entities (`&#34;', `&#8217;', …).
      (setq out (replace-regexp-in-string
                 "&#\\([0-9]+\\);"
                 (lambda (m)
                   (let ((code (string-to-number (match-string 1 m))))
                     (if (and (> code 0) (< code 1114112))
                         (char-to-string code)
                       m)))
                 out t))
      out)))

(defun elark-ai-context-graph--truncate (s n)
  "Return S clipped to at most N chars, appending `…' when cut."
  (cond
   ((not (stringp s)) "")
   ((> (length s) n)
    (concat (substring s 0 (max 0 (1- n))) "…"))
   (t s)))

(defun elark-ai-context-graph--squish-whitespace (s)
  "Collapse each stretch of internal whitespace in S to one space.
Lark messages can carry embedded newlines and tab runs (card layouts,
multi-line text); for a brief snippet a one-line, single-spaced form
reads much better.  Multi-line breaks become `· ' bullets, and any
leading/trailing bullet from a literal newline at either end gets
trimmed so the snippet doesn't start with ` · `."
  (when (stringp s)
    (let ((out (replace-regexp-in-string
                "[ \t]+" " "
                (replace-regexp-in-string "[\r\n]+" " · " s))))
      ;; Trim a leading/trailing ` · ` left over when the original S
      ;; began or ended with a newline.
      (setq out (replace-regexp-in-string "\\`[ \t·]+" "" out))
      (setq out (replace-regexp-in-string "[ \t·]+\\'" "" out))
      out)))

;;;; Aggregation

(defun elark-ai-context-graph--combine (providers results)
  "Stitch per-provider RESULTS into a single markdown blob.
PROVIDERS gives the order to render in; results that are nil / empty
after trim are skipped so a `Messages' section with no hits doesn't
add a lone heading."
  (let (sections)
    (dolist (entry providers)
      (let* ((label (car entry))
             (text (gethash label results "")))
        (when (and text (not (string-empty-p (string-trim text))))
          (push (format "## %s\n\n%s" label text) sections))))
    (string-join (nreverse sections) "\n\n")))

;;;; Top-level fan-out

(defun elark-ai-context-graph-gather (topic done-fn)
  "Gather context for TOPIC across all providers; call DONE-FN with the result.
Each provider runs concurrently and contributes a section of the
combined markdown blob.  When TOPIC is empty or no providers are
registered, DONE-FN is invoked immediately with the empty string.
Provider errors are logged via `elark-ai--ui' :progress-log and contribute
nothing to the output (the brief still ships)."
  (cond
   ((or (null topic) (string-empty-p (string-trim (or topic ""))))
    (funcall done-fn ""))
   ((null elark-ai-context-graph-providers)
    (funcall done-fn ""))
   (t
    (let* ((providers elark-ai-context-graph-providers)
           (count (length providers))
           (per-budget (max 1500
                            (floor elark-ai-context-graph-budget-chars
                                   (max 1 count))))
           (received 0)
           (results (make-hash-table :test 'equal)))
      (dolist (entry providers)
        (let* ((label (car entry))
               (fn (cdr entry))
               (finish (lambda (text)
                         (puthash label (or text "") results)
                         (setq received (1+ received))
                         (when (= received count)
                           (funcall done-fn
                                    (elark-ai-context-graph--combine
                                     providers results))))))
          (condition-case err
              (funcall fn topic per-budget finish)
            (error
             (elark-ai--ui :progress-log
               "Provider %s errored: %s"
               label (error-message-string err))
             (funcall finish nil)))))))))

;;;; Docs provider

(defun elark-ai-context-graph--docs-provider (topic budget done-fn)
  "Search Lark docs/wiki/sheets for TOPIC; format markdown, call DONE-FN.
BUDGET caps the formatted output size.
Backed by `lark-cli docs +search --query …'.  On CLI failure (timeout,
non-zero exit, or unparseable JSON), DONE-FN is called with nil so the
gatherer skips this section."
  (let ((cap (number-to-string
              (max 1 (min 20 elark-ai-context-graph-result-cap)))))
    (elark--run-command
     (list "docs" "+search" "--query" topic "--page-size" cap)
     (lambda (result)
       (funcall done-fn
                (elark-ai-context-graph--format-docs result budget)))
     nil
     :no-error t
     :timeout 30
     :on-error
     (lambda (exit-code msg)
       (elark-ai--ui :progress-log
         "Docs search failed (exit=%s): %s" exit-code (or msg ""))
       (funcall done-fn nil)))))

(defun elark-ai-context-graph--format-docs (result budget)
  "Render a `docs +search' RESULT envelope as a markdown bullet list.
BUDGET caps the cumulative item-line length; once exceeded, remaining
results are dropped (the LLM has plenty already).  Returns nil when
RESULT carries no usable items so the section is skipped entirely."
  (let* ((data (and (consp result) (alist-get 'data result)))
         (items (and data (alist-get 'results data)))
         (list (and items (append items nil)))
         (total (and data (alist-get 'total data)))
         (snip-cap elark-ai-context-graph-summary-snippet-chars)
         (acc nil)
         (used 0))
    (when list
      (catch 'budget
        (dolist (item list)
          (let* ((meta (alist-get 'result_meta item))
                 (title (or (elark-ai-context-graph--strip-html
                             (alist-get 'title_highlighted item))
                            (alist-get 'title meta)
                            "(untitled)"))
                 (url (or (alist-get 'url meta) ""))
                 (author (alist-get 'edit_user_name meta))
                 (date (let ((s (alist-get 'update_time_iso meta)))
                         (and (stringp s) (>= (length s) 10)
                              (substring s 0 10))))
                 (doctype (or (alist-get 'doc_types meta)
                              (alist-get 'entity_type item)
                              "DOC"))
                 (snippet (elark-ai-context-graph--truncate
                           (elark-ai-context-graph--strip-html
                            (alist-get 'summary_highlighted item))
                           snip-cap))
                 (line (format
                        "- **[%s](%s)** — %s%s%s\n  %s"
                        title url doctype
                        (if author (format " · %s" author) "")
                        (if date (format " · %s" date) "")
                        (or snippet ""))))
            ;; Push first so the section always carries at least one
            ;; result even when a single line exceeds the budget; only
            ;; *subsequent* lines get cut once the cap is breached.
            (push line acc)
            (setq used (+ used (length line)))
            (when (> used budget) (throw 'budget t)))))
      (when acc
        (concat
         (format "_%d of %s match%s shown_\n\n"
                 (length acc) (or total "?")
                 (if (eq total 1) "" "es"))
         (string-join (nreverse acc) "\n\n"))))))

;;;; IM provider

(defun elark-ai-context-graph--im-provider (topic budget done-fn)
  "Search messages for TOPIC; format markdown, call DONE-FN.
BUDGET caps the formatted output size.
Backed by `lark-cli im +messages-search --query … --no-reactions'.
On CLI failure DONE-FN is called with nil so the gatherer skips this
section instead of poisoning the brief."
  (let ((cap (number-to-string
              (max 1 (min 50 elark-ai-context-graph-result-cap)))))
    (elark--run-command
     (list "im" "+messages-search" "--query" topic
           "--page-size" cap "--no-reactions")
     (lambda (result)
       (funcall done-fn
                (elark-ai-context-graph--format-im result budget)))
     nil
     :no-error t
     :timeout 30
     :on-error
     (lambda (exit-code msg)
       (elark-ai--ui :progress-log
         "Messages search failed (exit=%s): %s" exit-code (or msg ""))
       (funcall done-fn nil)))))

(defun elark-ai-context-graph--message-text (msg)
  "Extract a brief, single-line snippet from a messages-search MSG result.
Strips Lark `<card>' wrappers (cards account for most of the verbose
content in interactive messages) and collapses internal whitespace."
  (let ((c (or (alist-get 'content msg) "")))
    (when (stringp c)
      (setq c (replace-regexp-in-string "<card[^>]*>" "" c))
      (setq c (replace-regexp-in-string "</card>" "" c))
      (elark-ai-context-graph--squish-whitespace c))))

(defun elark-ai-context-graph--message-sender (msg)
  "Return a display string for MSG's sender.
Falls back to a `[sender-type]' tag (e.g. `[app]', `[bot]') for app /
bot messages that don't carry a human name, instead of `(unknown)' —
release-pipeline bot floods otherwise looked identical in the brief."
  (or (elark--get-nested msg 'sender 'name)
      (let ((type (elark--get-nested msg 'sender 'sender_type)))
        (and (stringp type)
             (not (string-empty-p type))
             (format "[%s]" type)))
      "(unknown)"))

(defun elark-ai-context-graph--message-chat (msg)
  "Return a display string for MSG's chat / conversation.
Group chats use the human-readable `chat_name'.  P2P chats only carry
the partner's open_id, which is opaque to a brief reader; we surface
those as `(direct)' and let the LLM rely on sender + content to give
the message meaning."
  (or (alist-get 'chat_name msg)
      (and (alist-get 'chat_partner msg) "(direct)")
      "(unknown)"))

(defun elark-ai-context-graph--format-im (result budget)
  "Render an `im +messages-search' RESULT envelope as markdown bullets.
See `elark-ai-context-graph--format-docs' for the BUDGET / nil-skip
contract."
  (let* ((data (and (consp result) (alist-get 'data result)))
         (items (and data (alist-get 'messages data)))
         (list (and items (append items nil)))
         (total (and data (alist-get 'total data)))
         (snip-cap elark-ai-context-graph-summary-snippet-chars)
         (acc nil)
         (used 0))
    (when list
      (catch 'budget
        (dolist (msg list)
          (let* ((sender (elark-ai-context-graph--message-sender msg))
                 (chat-name (elark-ai-context-graph--message-chat msg))
                 (time (alist-get 'create_time msg))
                 (content (elark-ai-context-graph--message-text msg))
                 (snippet (elark-ai-context-graph--truncate content snip-cap))
                 (link (alist-get 'message_app_link msg))
                 (line (format "- **%s** in *%s*%s\n  %s%s"
                               sender chat-name
                               (if time (format " · %s" time) "")
                               (or snippet "")
                               (if (and link (not (string-empty-p link)))
                                   (format "\n  [open](%s)" link)
                                 ""))))
            ;; Push first so the section always carries at least one
            ;; result even when a single line exceeds the budget; only
            ;; *subsequent* lines get cut once the cap is breached.
            (push line acc)
            (setq used (+ used (length line)))
            (when (> used budget) (throw 'budget t)))))
      (when acc
        (concat
         (format "_%d of %s match%s shown_\n\n"
                 (length acc) (or total "?")
                 (if (eq total 1) "" "es"))
         (string-join (nreverse acc) "\n\n"))))))

;;;; Topic brief prompts
;; Shared by the classic (`elark-ai-brief-on') and shell brief paths.

(defcustom elark-ai-brief-on-prompt
  (concat
   "You are a productivity assistant building a topic brief from"
   " retrieved snippets across the user's docs and chat history.\n\n"
   "Rules:\n"
   "- Be terse — the reader is busy.\n"
   "- Keep the existing section headings (`## Docs', `## Messages', …)"
   " and write a short synthesis under each one.\n"
   "- Always preserve source links from the snippets so the reader"
   " can click through.\n"
   "- Highlight what's actionable, recent, or decided; downplay"
   " incidental matches.\n"
   "- If a section has no useful signal, omit it.\n"
   "- Output markdown only.")
  "System prompt for `elark-ai-brief-on'.
The synthesised brief is rendered straight into the AI buffer, so
any structural conventions (headings, link style) the prompt asks
for shape what the user sees."
  :type 'string
  :group 'elark-ai)

(defun elark-ai--brief-user-message (topic context-text)
  "Build the synthesis user message for a TOPIC brief over CONTEXT-TEXT.
Shared by the classic and shell brief paths.  Empty CONTEXT-TEXT
yields a short \"no results\" instruction rather than firing the
LLM at nothing."
  (if (string-empty-p (string-trim (or context-text "")))
      (format
       "## Topic\n%s\n\nNo results were retrieved across the\
 enabled providers. Tell the user no relevant docs or messages were\
 found, in one sentence, and suggest broadening the topic." topic)
    (format
     "## Topic\n%s\n\n## Retrieved snippets\n%s\n\nWrite the\
 brief now."
     topic context-text)))

(provide 'elark-ai-context-graph)
;;; elark-ai-context-graph.el ends here
