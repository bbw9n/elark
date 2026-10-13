;;; elark-ai-llm.el --- LLM backend dispatch for the elark AI layer -*- lexical-binding: t; -*-

;; Copyright (C) 2026 bbw9n

;; Author: bbw9n <bbw9nio@gmail.com>
;; Assisted-by: Claude:claude-opus-5
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Backend dispatch for LLM calls.  Three backends are supported:
;;
;;   `acp'   — a local ACP agent (Claude Code, Gemini CLI, …) via
;;             acp.el, the way agent-shell drives them; no API key,
;;             reuses the agent's own login.  The default.  See
;;             `elark-ai-acp.el'.
;;   `gptel' — uses the gptel package (multi-provider, streaming).
;;   `http'  — raw `url-retrieve' against an OpenAI-compatible endpoint.
;;
;; Public entry points (used by elark-ai-runner / elark-ai):
;;
;;   `elark-ai--call-llm'        — non-streaming request.
;;   `elark-ai--call-llm-stream' — streaming request with optional
;;                                chunk handler.
;;
;; Tests rebind these functions via `cl-letf' to inject fake responses
;; without involving the UI or any real network call.
;;
;; Streaming destination: when invoked without a CHUNK-HANDLER, the
;; default streams chunks into the AI buffer's output fragment (used
;; by the synthesis pass).  When invoked with a CHUNK-HANDLER, the
;; handler receives each chunk and the output fragment is left
;; untouched (used by the planning pass to stream raw JSON into the
;; log fragment).  Both paths invoke CALLBACK with the full
;; accumulated text on stream end.

;;; Code:

(require 'elark-ai-protocol)
(require 'elark-ai-skills)   ; `elark-ai-skills-abbreviate-for-log' for debug output
(require 'elark-ai-acp)      ; `acp' backend (acp.el itself is soft-required there)
(require 'elark-core)
(require 'json)

(require 'elark-ai-ui)       ; `elark-ai-ui-append-fragment'

;; gptel forward declarations
(declare-function gptel-request "gptel")
(defvar gptel-model)
(defvar gptel-backend)
(defvar gptel-log-level)

;; url forward declarations
(defvar url-http-end-of-headers)
(defvar url-request-method)
(defvar url-request-extra-headers)
(defvar url-request-data)

;;;; Customization

(defcustom elark-ai-backend 'acp
  "LLM backend for AI features.
`acp' (the default) drives a local Agent Client Protocol agent
\(Claude Code, Gemini CLI, …) via acp.el — no API key needed, the
agent's own login is reused; see `elark-ai-acp-command'.
`gptel' uses the gptel package (supports many providers).
`http' uses `url-retrieve' against an OpenAI-compatible endpoint."
  :type '(choice (const acp) (const gptel) (const http))
  :group 'elark-ai)

(defcustom elark-ai-model nil
  "Model identifier for the LLM backend.
When nil, uses the backend's default (e.g., gptel's `gptel-model')."
  :type '(choice (const nil) string)
  :group 'elark-ai)

(defcustom elark-ai-http-endpoint nil
  "HTTP endpoint for the `http' backend.
An OpenAI-compatible chat completions URL, e.g.,
\"https://api.openai.com/v1/chat/completions\"."
  :type '(choice (const nil) string)
  :group 'elark-ai)

(defcustom elark-ai-http-api-key nil
  "API key for the `http' backend.
If nil, reads from environment variable ELARK_AI_API_KEY."
  :type '(choice (const nil) string)
  :group 'elark-ai)

;;;; Non-streaming

(defun elark-ai--call-llm (system-prompt user-message callback)
  "Send SYSTEM-PROMPT and USER-MESSAGE to the LLM.
Call CALLBACK with the response text."
  (elark-ai--debug-log
   "REQUEST" "backend=%s\n--- system ---\n%s\n--- user ---\n%s"
   elark-ai-backend (elark-ai-skills-abbreviate-for-log system-prompt) user-message)
  (pcase elark-ai-backend
    ('gptel (elark-ai--call-gptel system-prompt user-message callback))
    ('http  (elark-ai--call-http system-prompt user-message callback))
    ('acp   (elark-ai-acp-call system-prompt user-message callback))
    (_      (user-error "Unknown elark-ai-backend: %s" elark-ai-backend))))

(defun elark-ai--call-gptel (system-prompt user-message callback)
  "Call the LLM via gptel with SYSTEM-PROMPT and USER-MESSAGE.
CALLBACK receives the response text.
Runs `gptel-request' inside the AI buffer so `gptel-abort'
against that buffer can find and cancel the in-flight request
\(or the current buffer when the AI UI isn't loaded)."
  (unless (require 'gptel nil t)
    (user-error "Package gptel is not installed; install it or set `elark-ai-backend' to `acp'"))
  (with-current-buffer (or (elark-ai--ui :get-buffer) (current-buffer))
    (let ((gptel-log-level nil)
          (inhibit-message t))
      (gptel-request user-message
                     :system system-prompt
                     :callback (lambda (response info)
                                 (cond
                                  ((stringp response)
                                   (elark-ai--debug-log
                                    "RESPONSE (gptel)" "%s" response)
                                   (funcall callback response))
                                  (t
                                   (elark-ai--debug-log
                                    "ERROR (gptel)"
                                    "response=%S info=%S" response info)
                                   (elark-ai--ui :progress-log
                                     "LLM error: %S" info))))))))

(defun elark-ai--call-http (system-prompt user-message callback)
  "Call the LLM via raw HTTP to an OpenAI-compatible endpoint.
Send SYSTEM-PROMPT and USER-MESSAGE; CALLBACK receives the response text."
  (let* ((url (or elark-ai-http-endpoint
                  (user-error "Set `elark-ai-http-endpoint' for the http backend")))
         (key (or elark-ai-http-api-key
                  (getenv "ELARK_AI_API_KEY")
                  (user-error "Set `elark-ai-http-api-key' or ELARK_AI_API_KEY env var")))
         (model (or elark-ai-model "claude-sonnet-4-20250514"))
         (payload (json-encode
                   `((model . ,model)
                     (messages . [((role . "system") (content . ,system-prompt))
                                  ((role . "user") (content . ,user-message))])
                     (max_tokens . 4096))))
         (url-request-method "POST")
         (url-request-extra-headers
          `(("Content-Type" . "application/json")
            ("Authorization" . ,(format "Bearer %s" key))))
         (url-request-data payload))
    (url-retrieve
     url
     (lambda (status)
       (if (plist-get status :error)
           (progn
             (elark-ai--debug-log
              "ERROR (http)" "%S" (plist-get status :error))
             (elark-ai--ui :progress-log
               "HTTP error: %S" (plist-get status :error)))
         (goto-char url-http-end-of-headers)
         (let* ((json-response (json-parse-buffer :object-type 'alist))
                (content (elark--get-nested json-response
                                            'choices 0 'message 'content)))
           (elark-ai--debug-log "RESPONSE (http)" "%s" (or content ""))
           (funcall callback (or content "")))))
     nil t t)))

;;;; Streaming

(defun elark-ai--call-llm-stream (system-prompt user-message callback
                                                &optional chunk-handler)
  "Send to LLM with streaming, calling CALLBACK with full text when done.
SYSTEM-PROMPT and USER-MESSAGE form the request.
If CHUNK-HANDLER is non-nil, it is called as (CHUNK-HANDLER CHUNK)
on each streamed chunk and the default behaviour of appending
chunks to the output fragment is skipped — used by the planning
call to stream raw JSON into the log fragment instead.
HTTP backend has no streaming, so the fallback is a single
non-streaming call (CHUNK-HANDLER is not invoked)."
  (pcase elark-ai-backend
    ('gptel
     (elark-ai--call-gptel-stream system-prompt user-message
                                 callback chunk-handler))
    ('acp
     (elark-ai--call-acp-stream system-prompt user-message
                               callback chunk-handler))
    (_
     ;; Fallback: non-streaming
     (elark-ai--call-llm system-prompt user-message callback))))

(defun elark-ai--call-gptel-stream (system-prompt user-message callback
                                                  &optional chunk-handler)
  "Call LLM via gptel with streaming.
SYSTEM-PROMPT and USER-MESSAGE form the request; CALLBACK receives
the full text when done.
When CHUNK-HANDLER is nil, chunks are appended to the output
fragment (synthesis path).  When non-nil, CHUNK-HANDLER is
invoked with each chunk and the output fragment is left
untouched — the planning path uses this to stream raw JSON into
the log fragment.
Runs `gptel-request' inside the AI buffer so `gptel-abort' can
find and cancel the in-flight stream."
  (unless (require 'gptel nil t)
    (user-error "Package gptel is not installed"))
  ;; Only create the output fragment for the default (synthesis) path.
  (unless chunk-handler
    (elark-ai--ui :ensure-output-fragment))
  (elark-ai--debug-log
   "STREAM REQUEST" "--- system ---\n%s\n--- user ---\n%s"
   (elark-ai-skills-abbreviate-for-log system-prompt) user-message)
  (let ((gptel-log-level nil)
        (inhibit-message t)
        (accumulated "")
        (chunks 0))
    (with-current-buffer (or (elark-ai--ui :get-buffer) (current-buffer))
      (gptel-request user-message
                     :system system-prompt
                     :stream t
                     :callback
                     (lambda (response info)
                       (cond
                        ((stringp response)
                         (setq accumulated
                               (concat accumulated response)
                               chunks (1+ chunks))
                         (elark-ai--debug-log
                          "STREAM CHUNK"
                          "#%d (%d chars): %s"
                          chunks (length response) response)
                         (elark-ai--route-chunk response chunk-handler))
                        ((eq response t)
                         (elark-ai--debug-log
                          "STREAM DONE"
                          "%d chunks, %d total chars\n--- accumulated ---\n%s"
                          chunks (length accumulated) accumulated)
                         (funcall callback accumulated))
                        ((and (consp response)
                              (eq (car response) 'reasoning))
                         nil)
                        (t
                         (elark-ai--debug-log
                          "STREAM ERROR" "response=%S info=%S" response info)
                         (elark-ai--ui :progress-log
                           "LLM stream error"))))))))

(defun elark-ai--call-acp-stream (system-prompt user-message callback
                                                &optional chunk-handler)
  "Call the LLM via a local ACP agent with streaming.
SYSTEM-PROMPT and USER-MESSAGE form the request; CALLBACK receives
the full text when done.
Same chunk-destination rules as `elark-ai--call-gptel-stream':
without CHUNK-HANDLER, chunks append to the output fragment;
with one, the handler receives each chunk instead."
  (unless chunk-handler
    (elark-ai--ui :ensure-output-fragment))
  (elark-ai--debug-log
   "STREAM REQUEST (acp)" "--- system ---\n%s\n--- user ---\n%s"
   (elark-ai-skills-abbreviate-for-log system-prompt) user-message)
  (elark-ai-acp-call
   system-prompt user-message callback
   (lambda (chunk) (elark-ai--route-chunk chunk chunk-handler))))

;;;; Skill routing

(defun elark-ai--select-skills (prompt context callback)
  "Identify relevant skills for PROMPT, then call CALLBACK with the list.
CONTEXT is extra match text (originating buffer + last plan).  When
`elark-ai-skill-routing' is `llm', ask the LLM to pick skills from the
catalogue and fall back to keyword routing if it returns nothing
usable; otherwise use keyword routing directly.  CALLBACK always
receives a deduplicated list that includes lark-shared."
  (if (eq elark-ai-skill-routing 'keyword)
      (funcall callback (elark-ai-skills-select prompt context))
    (elark-ai--ui :progress-log "Identifying relevant skills…")
    (let ((router-prompt (elark-ai-skills-build-router-prompt))
          (user-msg (concat "User request:\n" prompt
                            (when (and context (not (string-empty-p context)))
                              (concat "\n\nContext:\n" context)))))
      (elark-ai--call-llm
       router-prompt user-msg
       (lambda (response)
         (let* ((data (elark-ai--extract-json response))
                (raw (and (listp data) (alist-get 'skills data)))
                (names (cond ((listp raw) raw)
                             ((vectorp raw) (append raw nil))
                             (t nil)))
                (valid (elark-ai-skills--validate-selection names)))
           (elark-ai--debug-log "SKILL ROUTER" "llm=%S → valid=%S" names valid)
           (funcall callback
                    (if valid
                        (delete-dups (cons "lark-shared" valid))
                      ;; LLM gave nothing usable — keyword fallback.
                      (elark-ai-skills-select prompt context)))))))))

(defun elark-ai--route-chunk (chunk chunk-handler)
  "Send a streamed CHUNK to CHUNK-HANDLER, or else the output fragment.
CHUNK-HANDLER runs even without a `*Lark AI*' buffer: the AI shell
streams briefs through it and never creates that buffer."
  (let ((buf (get-buffer elark-ai--buf-name)))
    (cond (chunk-handler
           (with-current-buffer (or buf (current-buffer))
             (funcall chunk-handler chunk)))
          (buf
           (with-current-buffer buf
             (elark-ai-ui-append-fragment (elark-ai--ui :frag "output") chunk))))))

(provide 'elark-ai-llm)
;;; elark-ai-llm.el ends here
