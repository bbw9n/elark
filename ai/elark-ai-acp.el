;;; elark-ai-acp.el --- ACP agent backend for the elark AI layer -*- lexical-binding: t; -*-

;; Copyright (C) 2026 bbw9n

;; Author: bbw9n <bbw9nio@gmail.com>
;; Assisted-by: Claude:claude-opus-5
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The `acp' backend for `elark-ai-backend' — drives a locally
;; installed ACP (Agent Client Protocol) agent such as Claude Code
;; (via the claude-code-acp adapter) or Gemini CLI as the text engine
;; behind the `elark-ai--call-llm' / `elark-ai--call-llm-stream' seams.
;; This is the agent-shell approach (acp.el under the hood), but
;; headless: no shell UI, and no API key — the agent's own login is
;; reused.
;;
;; One agent process (ACP client) is kept alive across calls; each
;; LLM call opens a FRESH ACP session so the stateless "system + user
;; in, text out" contract of the other backends holds.  Lark's agent
;; loop re-sends its own transcript every iteration, so a persistent
;; session would duplicate context on the agent side.
;;
;; The agent's own agentic abilities are suppressed: file-system
;; capabilities are not advertised at initialize time, any
;; session/request_permission is auto-declined, and (under the
;; default `session' placement, see
;; `elark-ai-acp-system-prompt-placement') elark's system prompt
;; replaces the agent's own via the session/new `_meta'
;; `systemPrompt' extension, so the agent answers in text rather
;; than running its own tools or falling back to its assistant
;; persona.  (A future strategy
;; that delegates the whole loop to the agent — mapping ACP tool
;; calls onto elark's tool-call cards and permission requests onto the
;; confirm-writes gate — can reuse this client plumbing.)
;;
;; In-flight requests are tracked per session id, so a background
;; brief and an interactive turn can safely overlap on the one
;; client.
;;
;; acp.el is a soft dependency, mirroring how the `gptel' backend
;; treats gptel: required at call time with a `user-error' fallback.

;;; Code:

(require 'map)
(require 'seq)
(require 'elark-ai-protocol)   ; `elark-ai--debug-log'
(require 'elark-core)          ; `elark--safe-default-directory'

;; acp.el forward declarations — soft-required in
;; `elark-ai-acp--ensure-client'.
(declare-function acp-make-client "ext:acp")
(declare-function acp-send-request "ext:acp")
(declare-function acp-send-response "ext:acp")
(declare-function acp-send-notification "ext:acp")
(declare-function acp-shutdown "ext:acp")
(declare-function acp-subscribe-to-notifications "ext:acp")
(declare-function acp-subscribe-to-requests "ext:acp")
(declare-function acp-subscribe-to-errors "ext:acp")
(declare-function acp-make-initialize-request "ext:acp")
(declare-function acp-make-session-new-request "ext:acp")
(declare-function acp-make-session-prompt-request "ext:acp")
(declare-function acp-make-session-cancel-notification "ext:acp")
(declare-function acp-make-session-request-permission-response "ext:acp")
(declare-function acp-make-error "ext:acp")

;;;; Customization

(defcustom elark-ai-acp-command "claude-code-acp"
  "Program that speaks ACP on stdio for the `acp' backend.
Any Agent Client Protocol agent works, e.g.:
- \"claude-code-acp\" — Claude Code (npm i -g @zed-industries/claude-code-acp)
- \"gemini\"          — Gemini CLI (set `elark-ai-acp-command-params'
                        to \\='(\"--experimental-acp\"))"
  :type 'string
  :group 'elark-ai)

(defcustom elark-ai-acp-command-params nil
  "Extra command-line arguments passed to `elark-ai-acp-command'."
  :type '(repeat string)
  :group 'elark-ai)

(defcustom elark-ai-acp-environment-variables nil
  "Environment entries (\"VAR=value\" strings) for the agent process."
  :type '(repeat string)
  :group 'elark-ai)

(defcustom elark-ai-acp-cwd "~"
  "Working directory handed to each new ACP session."
  :type 'directory
  :group 'elark-ai)

(defcustom elark-ai-acp-system-prompt-placement 'session
  "Where each call's system prompt goes.
`session' (the default) sends it in the session/new request's
`_meta' `systemPrompt' extension, REPLACING the agent's own system
prompt — honored by claude-code-acp, and the reliable way to make
the agent follow elark's one-JSON-action-per-turn contract instead
of its own assistant persona (which otherwise sometimes answers in
prose and wastes a correction round-trip).
`inline' prepends it to the prompt text in a <system-instructions>
block — weaker, since to the agent it is just user content, but it
works with agents that ignore the `_meta' extension (e.g. Gemini
CLI); set this when `elark-ai-acp-command' is not claude-code-acp."
  :type '(choice (const session) (const inline))
  :group 'elark-ai)

;;;; State

(defvar elark-ai-acp--client nil
  "The live ACP client (agent process), shared across calls.")

(defvar elark-ai-acp--initialized nil
  "Non-nil once the current client answered the initialize request.")

(defvar elark-ai-acp--requests nil
  "Alist of (SESSION-ID . PLIST) for in-flight prompt requests.
PLIST keys: `:accumulated' (streamed text so far), `:on-chunk'
\(per-chunk callback or nil), `:callback' (final-text callback).")

;;;; Pure message helpers (unit-tested without acp.el)

(defun elark-ai-acp--notification-chunk (notification)
  "Return (SESSION-ID . TEXT) when NOTIFICATION is an agent message chunk.
NOTIFICATION is a decoded JSON-RPC object.  Returns nil for every
other notification kind (thought chunks, tool-call updates, …)."
  (when (equal (map-elt notification 'method) "session/update")
    (let ((params (map-elt notification 'params)))
      (when (equal (map-nested-elt params '(update sessionUpdate))
                   "agent_message_chunk")
        (cons (map-elt params 'sessionId)
              (or (map-nested-elt params '(update content text)) ""))))))

(defun elark-ai-acp--reject-option-id (options)
  "Return the optionId of a rejecting entry in OPTIONS, or nil.
OPTIONS is the sequence from a session/request_permission request;
prefers `reject_once' over `reject_always'."
  (let ((by-kind (lambda (kind)
                   (map-elt (seq-find (lambda (opt)
                                        (equal (map-elt opt 'kind) kind))
                                      options)
                            'optionId))))
    (or (funcall by-kind "reject_once")
        (funcall by-kind "reject_always"))))

(defun elark-ai-acp--session-system-prompt (system-prompt)
  "Return SYSTEM-PROMPT when it should ride the session/new meta, else nil.
Non-nil only under the `session' placement (see
`elark-ai-acp-system-prompt-placement') and when SYSTEM-PROMPT has
content; otherwise the inline path handles it."
  (and (eq elark-ai-acp-system-prompt-placement 'session)
       system-prompt
       (not (string-empty-p system-prompt))
       system-prompt))

(defun elark-ai-acp--prompt-text (system-prompt user-message)
  "Combine SYSTEM-PROMPT and USER-MESSAGE into one prompt string.
Used by the `inline' placement (and when SYSTEM-PROMPT is nil
because it already went out on the session): ACP prompts have no
system-message slot, so the system prompt is prepended in a tagged
block the agent treats as instructions."
  (if (and system-prompt (not (string-empty-p system-prompt)))
      (concat "<system-instructions>\n" system-prompt
              "\n</system-instructions>\n\n" user-message)
    user-message))

;;;; Client lifecycle

(defun elark-ai-acp--reset-state ()
  "Forget the client and all in-flight bookkeeping."
  (setq elark-ai-acp--client nil
        elark-ai-acp--initialized nil
        elark-ai-acp--requests nil))

(defun elark-ai-acp--ensure-client ()
  "Return a live ACP client, creating and subscribing it if needed."
  (unless (require 'acp nil t)
    (user-error "Package acp.el is not installed (MELPA: `acp'); install it or set `elark-ai-backend' to `gptel'"))
  ;; A dead agent process invalidates the whole client state.
  (when-let* ((proc (and elark-ai-acp--client
                        (map-elt elark-ai-acp--client :process))))
    (unless (process-live-p proc)
      (elark-ai-acp--reset-state)))
  (unless elark-ai-acp--client
    (setq elark-ai-acp--client
          (acp-make-client
           :command elark-ai-acp-command
           :command-params elark-ai-acp-command-params
           :environment-variables elark-ai-acp-environment-variables))
    (acp-subscribe-to-notifications
     :client elark-ai-acp--client
     :on-notification #'elark-ai-acp--on-notification)
    (acp-subscribe-to-requests
     :client elark-ai-acp--client
     :on-request #'elark-ai-acp--on-request)
    (acp-subscribe-to-errors
     :client elark-ai-acp--client
     :on-error #'elark-ai-acp--on-error))
  elark-ai-acp--client)

(defun elark-ai-acp--with-initialized (client then)
  "Run THEN once CLIENT has completed the ACP initialize handshake.
File-system capabilities are deliberately not advertised so the
agent doesn't route file access through us."
  (if elark-ai-acp--initialized
      (funcall then)
    (acp-send-request
     :client client
     :request (acp-make-initialize-request
               :protocol-version 1
               :client-info '((name . "elark")
                              (title . "Lark AI")
                              (version . "0.1.0")))
     :on-success (lambda (_response)
                   (setq elark-ai-acp--initialized t)
                   (funcall then))
     :on-failure #'elark-ai-acp--report-failure)))

;;;; Incoming traffic

(defun elark-ai-acp--on-notification (notification)
  "Accumulate agent message chunks from NOTIFICATION into their request."
  (pcase (elark-ai-acp--notification-chunk notification)
    (`(,session-id . ,text)
     (when-let* ((req (assoc session-id elark-ai-acp--requests)))
       (setcdr req (plist-put (cdr req) :accumulated
                              (concat (plist-get (cdr req) :accumulated) text)))
       (when-let* ((on-chunk (plist-get (cdr req) :on-chunk)))
         (funcall on-chunk text))))))

(defun elark-ai-acp--on-request (request)
  "Decline agent-initiated REQUEST — this backend wants text only.
Permission requests pick the reject option (or cancel); anything
else gets a method-not-found error so the agent falls back to
answering directly."
  (let ((method (map-elt request 'method))
        (id (map-elt request 'id)))
    (pcase method
      ("session/request_permission"
       (elark-ai--debug-log
        "ACP PERMISSION" "auto-declining: %s"
        (or (map-nested-elt request '(params toolCall title)) method))
       (let ((option-id (elark-ai-acp--reject-option-id
                         (map-nested-elt request '(params options)))))
         (acp-send-response
          :client elark-ai-acp--client
          :response (if option-id
                        (acp-make-session-request-permission-response
                         :request-id id :option-id option-id)
                      (acp-make-session-request-permission-response
                       :request-id id :cancelled t)))))
      (_
       (elark-ai--debug-log "ACP REQUEST" "declining unsupported: %s" method)
       (acp-send-response
        :client elark-ai-acp--client
        :response (list (cons :request-id id)
                        (cons :error (acp-make-error
                                      :code -32601
                                      :message (format "%s is not available"
                                                       method)))))))))

(defun elark-ai-acp--on-error (error)
  "Log agent process ERROR output (stderr noise included) to the debug log."
  (elark-ai--debug-log "ACP STDERR" "%S" error))

(defun elark-ai-acp--report-failure (error)
  "Surface a failed ACP request ERROR in the progress log."
  (let ((msg (or (map-elt error 'message) (format "%S" error))))
    (elark-ai--debug-log "ERROR (acp)" "%S" error)
    (elark-ai--ui :progress-log "ACP error: %s" msg)
    (when (string-match-p "auth" (downcase msg))
      (elark-ai--ui :progress-log
        "Hint: the agent may need a login — run its CLI once (e.g. `claude') and retry."))))

;;;; Prompt round-trip

(defun elark-ai-acp-call (system-prompt user-message callback &optional on-chunk)
  "Send SYSTEM-PROMPT and USER-MESSAGE to the ACP agent.
Call CALLBACK with the full response text when the turn ends.
When ON-CHUNK is non-nil, it also receives each streamed text
chunk as it arrives.  Opens a fresh ACP session per call (see
Commentary).

`default-directory' is rebound to a safe existing directory for
the dynamic extent of the call: the first request spawns the
agent process, and spawning from a buffer whose directory was
purged (e.g. a doc buffer's temp cache dir) fails with
\"Setting current directory: No such file or directory\"."
  (let* ((default-directory (elark--safe-default-directory))
         (client (elark-ai-acp--ensure-client))
         ;; Under the `session' placement the system prompt rides the
         ;; session/new meta and the prompt text carries only the user
         ;; message; under `inline' it is nil here and prepended to the
         ;; prompt text instead.
         (session-sys (elark-ai-acp--session-system-prompt system-prompt))
         (inline-sys (unless session-sys system-prompt)))
    (elark-ai-acp--with-initialized
     client
     (lambda ()
       (acp-send-request
        :client client
        :request (acp-make-session-new-request
                  :cwd elark-ai-acp-cwd
                  :meta (when session-sys
                          `((systemPrompt . ,session-sys))))
        :on-success
        (lambda (response)
          (let ((session-id (map-elt response 'sessionId)))
            (push (cons session-id (list :accumulated ""
                                         :on-chunk on-chunk
                                         :callback callback))
                  elark-ai-acp--requests)
            (elark-ai-acp--prompt client session-id inline-sys user-message)))
        :on-failure #'elark-ai-acp--report-failure)))))

(defun elark-ai-acp--prompt (client session-id system-prompt user-message)
  "Send the session/prompt for SESSION-ID on CLIENT.
SYSTEM-PROMPT and USER-MESSAGE are combined by
`elark-ai-acp--prompt-text'; the response text arrives via
notifications and is finalized in `elark-ai-acp--finish'."
  (acp-send-request
   :client client
   :request (acp-make-session-prompt-request
             :session-id session-id
             :prompt `(((type . "text")
                        (text . ,(elark-ai-acp--prompt-text
                                  system-prompt user-message)))))
   :on-success (lambda (response)
                 (elark-ai-acp--finish session-id
                                      (map-elt response 'stopReason)))
   :on-failure (lambda (error)
                 (setq elark-ai-acp--requests
                       (assoc-delete-all session-id elark-ai-acp--requests))
                 (elark-ai-acp--report-failure error))))

(defun elark-ai-acp--finish (session-id stop-reason)
  "Complete the request for SESSION-ID that ended with STOP-REASON.
Invokes the stored callback with the accumulated text — except on
\"cancelled\", where the abort path already reset the session and
a late callback would resurrect a dead turn."
  (when-let* ((req (assoc session-id elark-ai-acp--requests)))
    (setq elark-ai-acp--requests
          (assoc-delete-all session-id elark-ai-acp--requests))
    (let ((text (plist-get (cdr req) :accumulated)))
      (elark-ai--debug-log "RESPONSE (acp)" "stop=%s\n%s" stop-reason text)
      (unless (equal stop-reason "cancelled")
        (funcall (plist-get (cdr req) :callback) text)))))

;;;; Abort / shutdown

(defun elark-ai-acp-abort ()
  "Cancel every in-flight ACP prompt and drop its callback."
  ;; A dead agent has nothing to cancel, and sending to it would make
  ;; acp.el restart the process just to deliver the notification.
  (when (and elark-ai-acp--client
             (process-live-p (map-elt elark-ai-acp--client :process)))
    (dolist (req elark-ai-acp--requests)
      (acp-send-notification
       :client elark-ai-acp--client
       :notification (acp-make-session-cancel-notification
                      :session-id (car req)))))
  (setq elark-ai-acp--requests nil))

(defun elark-ai-acp-shutdown ()
  "Kill the ACP agent process and reset the backend state.
The next `acp' backend call starts a fresh agent."
  (interactive)
  (when elark-ai-acp--client
    (acp-shutdown :client elark-ai-acp--client))
  (elark-ai-acp--reset-state))

(provide 'elark-ai-acp)
;;; elark-ai-acp.el ends here
