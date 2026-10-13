;;; elark-auth.el --- Authentication for elark -*- lexical-binding: t; -*-

;; Copyright (C) 2026 bbw9n

;; Author: bbw9n <bbw9nio@gmail.com>
;; Assisted-by: Claude:claude-opus-5
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Handles the OAuth device-code authentication flow for lark-cli,
;; auth status checking, and identity switching (user/bot).
;;
;; Note: lark-cli auth commands use their own flags (e.g., --json)
;; rather than the global --format/--as flags.  All auth commands use
;; :literal t to bypass elark--build-command.

;;; Code:

(require 'elark-core)
(require 'browse-url)

;;;; Variables

(defvar elark-auth--authenticated-p nil
  "Cached authentication status.  nil means unknown.")

(defvar elark-auth--polling-timer nil
  "Timer for polling device-code authentication.")

;;;; Auth status

(defun elark-auth--user-from-alist (obj)
  "Return the first user-name-like field in alist OBJ, or nil."
  (when (listp obj)
    (or (alist-get 'userName obj)
        (alist-get 'user_name obj)
        (alist-get 'name obj)
        (alist-get 'displayName obj)
        (alist-get 'display_name obj))))

(defun elark-auth--active-identity (result)
  "Return the active identity alist from a lark-cli `auth status' RESULT.
The CLI wraps each identity (user/bot) under `identities', with the
active one named by the top-level `identity' field.  Returns nil when
the response doesn't use this shape (e.g. a login-poll response)."
  (when (listp result)
    (let ((identity (alist-get 'identity result))
          (identities (alist-get 'identities result)))
      (when (listp identities)
        (or (and (stringp identity)
                 (alist-get (intern identity) identities))
            ;; Defensive: no explicit `identity' field — credit whichever
            ;; sub-identity actually carries a userName.
            (let ((u (alist-get 'user identities)))
              (and (elark-auth--user-from-alist u) u))
            (let ((b (alist-get 'bot identities)))
              (and (elark-auth--user-from-alist b) b)))))))

(defun elark-auth--extract-user (result)
  "Return the authenticated user name from RESULT, or nil.
Primary shape — what `lark-cli auth status' actually returns:

  ((identity . \"user\")
   (identities . ((user . ((userName . \"…\") (tokenStatus . \"valid\") …))
                  (bot  . ((status . \"ready\") …))))
   …)

Falls back to simpler shapes used by other endpoints (top level, or
nested under `data', `user', `data.user') so the same extractor works
for login-poll responses too."
  (or (elark-auth--user-from-alist (elark-auth--active-identity result))
      (elark-auth--user-from-alist result)
      (elark-auth--user-from-alist (alist-get 'data result))
      (elark-auth--user-from-alist (alist-get 'user result))
      (elark-auth--user-from-alist
       (alist-get 'user (alist-get 'data result)))))

(defun elark-auth--extract-field (result key)
  "Look up KEY in RESULT — top level, the active identity, or under `data'.
The active identity is where `auth status' puts `tokenStatus' and other
per-identity fields, so consult it before falling back."
  (or (alist-get key result)
      (alist-get key (elark-auth--active-identity result))
      (and (listp (alist-get 'data result))
           (alist-get key (alist-get 'data result)))))

;;;###autoload
(defun elark-auth-status ()
  "Display lark-cli authentication status."
  (interactive)
  ;; `auth status' outputs JSON by default, no --json flag needed.
  (elark--run-command
   '("auth" "status")
   (lambda (result)
     (elark-auth--log "status response: %S" result)
     (let ((user (elark-auth--extract-user result)))
       (cond
        ((null result)
         (setq elark-auth--authenticated-p nil)
         (message "Lark: not authenticated (no config — see *elark-log*)"))
        (user
         (setq elark-auth--authenticated-p t)
         (message "Lark: authenticated as %s (identity: %s, token: %s)"
                  user
                  (or (elark-auth--extract-field result 'identity) "?")
                  (or (elark-auth--extract-field result 'tokenStatus)
                      (elark-auth--extract-field result 'token_status) "?")))
        (t
         (setq elark-auth--authenticated-p nil)
         (message
          "Lark: not authenticated (response shape unrecognised — see *elark-log*)")))))
   nil :literal t :no-error t
   :on-error
   (lambda (exit-code msg)
     (elark-auth--log "status error: exit %s: %s" exit-code msg)
     (setq elark-auth--authenticated-p nil)
     (message "Lark: auth status failed (exit %s) — see *elark-log*" exit-code))))

(defun elark-auth--check-sync ()
  "Check auth status synchronously.  Return non-nil if authenticated."
  (condition-case nil
      (let ((result (elark--run-command-sync
                     '("auth" "status") nil :literal t :no-error t)))
        (elark-auth--log "status (sync) response: %S" result)
        (setq elark-auth--authenticated-p
              (and (elark-auth--extract-user result) t)))
    (error
     (setq elark-auth--authenticated-p nil))))

;;;; Auth ensure

(defun elark-auth-ensure ()
  "Ensure the user is authenticated, prompting login if needed.
Returns t when authentication is already in place.  Signals a
`user-error' otherwise — both when the user declines the login
prompt and when login was just kicked off (the device-code flow
completes asynchronously, so the in-flight command can't proceed
yet).  Callers therefore never need to check the return value:
either control flows past the call (authed) or unwinds with a
clear message."
  (unless elark-auth--authenticated-p
    (elark-auth--check-sync))
  (unless elark-auth--authenticated-p
    (if (yes-or-no-p "Not authenticated with Lark.  Login now? ")
        (progn
          (elark-auth-login)
          (user-error "Lark: complete the login flow in your browser, then re-run the command"))
      (user-error "Lark authentication required")))
  elark-auth--authenticated-p)

;;;; Login (device-code flow)

;;;###autoload
(defun elark-auth-login (&optional domains)
  "Initiate lark-cli OAuth login via the device-code flow.
DOMAINS is a comma-separated string of scopes to request
\(e.g., \"calendar,task,im\").  Defaults to \"all\".

Opens the authorization URL in a browser and polls for completion."
  (interactive
   (list (read-string "Domains (comma-separated, default all): " nil nil "all")))
  (let ((domain (or domains "all")))
    (message "Lark: initiating login (domains: %s)..." domain)
    (elark--run-command
     (list "auth" "login" "--no-wait" "--json" "--domain" domain)
     (lambda (result)
       (let ((ok (alist-get 'ok result))
             (url (alist-get 'verification_url result))
             (device-code (alist-get 'device_code result))
             (err (alist-get 'error result)))
         (cond
          ;; Error response: {ok: false, error: {message: ...}}
          ((and err (not (eq ok :false)))
           (user-error "Lark login error: %s"
                       (or (alist-get 'message err) (format "%S" err))))
          ((eq ok :false)
           (user-error "Lark login error: %s"
                       (or (and err (alist-get 'message err)) "unknown")))
          ;; Missing expected fields
          ((not (and url device-code))
           (user-error "Lark: unexpected login response: %S" result))
          ;; Success — open browser and poll
          (t
           (message "Lark: opening browser for authorization...")
           (browse-url url)
           (elark-auth--start-polling device-code)))))
     nil
     :literal t :no-error t)))

(defun elark-auth--log (fmt &rest args)
  "Append a labelled auth entry, FMT formatted with ARGS, to `*elark-log*'.
Always on (auth runs rarely and silent failures are hard to diagnose
without a trace).  Lands in the same focused activity log as the CLI
REQUEST/RESPONSE entries, labelled \"AUTH\" so it's easy to scan."
  (apply #'elark--cli-log "AUTH" fmt args))

(defun elark-auth--login-success-p (result)
  "Return non-nil when RESULT from a device-code poll indicates success.
Accepts any of: explicit `ok: t' at top level, a recognised user-name
field via `elark-auth--extract-user', or an access token at the top
level or under `data' — the polling endpoint's success shape isn't
fully pinned down, so this errs on the side of recognising more."
  (when (listp result)
    (or (eq (alist-get 'ok result) t)
        (elark-auth--extract-user result)
        (elark-auth--extract-field result 'access_token)
        (elark-auth--extract-field result 'accessToken))))

(defun elark-auth--stop-polling ()
  "Cancel the device-code polling timer if one is running."
  (when (timerp elark-auth--polling-timer)
    (cancel-timer elark-auth--polling-timer))
  (setq elark-auth--polling-timer nil))

(defun elark-auth--finish-login (result)
  "Mark login complete with RESULT, stop polling, and notify the user.
Idempotent — extra polling responses arriving after success are no-ops."
  (when elark-auth--polling-timer
    (elark-auth--stop-polling)
    (setq elark-auth--authenticated-p t)
    (let ((user (elark-auth--extract-user result)))
      (elark-auth--log "login complete; user=%S" user)
      (message "Lark: login successful%s"
               (if user (format " as %s" user) "!")))))

(defun elark-auth--check-status-async ()
  "Query `auth status' asynchronously; finish login if it reports a user.
Used as a fallback when the device-code poll's response shape doesn't
match `elark-auth--login-success-p' — auth status is the authoritative
source, so consulting it covers any CLI response we don't recognise."
  (elark--run-command
   '("auth" "status")
   (lambda (result)
     (elark-auth--log "status check: %S" result)
     (when (elark-auth--extract-user result)
       (elark-auth--finish-login result)))
   nil :literal t :no-error t))

(defun elark-auth--poll-once (device-code)
  "Run one poll iteration for DEVICE-CODE."
  (elark--run-command
   (list "auth" "login" "--device-code" device-code "--json")
   (lambda (result)
     (elark-auth--log "poll response: %S" result)
     (if (elark-auth--login-success-p result)
         (elark-auth--finish-login result)
       ;; Response didn't look like success — cross-check with
       ;; `auth status' in case the CLI returned a shape we don't
       ;; recognise but did actually authenticate.
       (elark-auth--check-status-async)))
   nil :literal t :no-error t
   ;; Without :on-error, non-zero exits during the pending phase are
   ;; silently dropped (the success callback never fires), so failures
   ;; would be invisible.  Log and still consult `auth status'.
   :on-error
   (lambda (exit-code msg)
     (elark-auth--log "poll error: exit %s: %s" exit-code msg)
     (elark-auth--check-status-async))))

(defun elark-auth--start-polling (device-code)
  "Poll for device-code auth completion.
DEVICE-CODE is the code returned by the initial `auth login --no-wait'.
Each tick fires `elark-auth--poll-once', which both drives the CLI's
device-code exchange and cross-checks `auth status' so we don't miss
completion if the polling response shape changes."
  (elark-auth--stop-polling)
  (let ((attempts 0)
        (max-attempts 120)              ; 10 minutes at 5s intervals
        (interval 5))
    (elark-auth--log "polling started (device-code=%s)" device-code)
    (setq elark-auth--polling-timer
          (run-with-timer
           interval interval
           (lambda ()
             (setq attempts (1+ attempts))
             (cond
              ((null elark-auth--polling-timer) nil)   ; finished elsewhere
              ((>= attempts max-attempts)
               (elark-auth--stop-polling)
               (elark-auth--log "polling timed out after %d attempts" attempts)
               (message "Lark: login timed out — run `M-x elark-auth-login' to retry"))
              (t (elark-auth--poll-once device-code))))))))

;;;; Logout

;;;###autoload
(defun elark-auth-logout ()
  "Log out from lark-cli."
  (interactive)
  (when (yes-or-no-p "Log out from Lark? ")
    (elark--run-command
     '("auth" "logout")
     (lambda (_result)
       (setq elark-auth--authenticated-p nil)
       (message "Lark: logged out"))
     nil
     :literal t :no-error t)))

;;;; Identity switching

;;;###autoload
(defun elark-auth-switch-identity ()
  "Switch between user and bot identity for lark-cli commands."
  (interactive)
  (let ((identity (completing-read "Identity: " '("user" "bot") nil t)))
    (customize-set-variable 'elark-default-identity identity)
    (message "Lark: identity set to %s" identity)))

;;;; List logged-in users

;;;###autoload
(defun elark-auth-list ()
  "List all logged-in lark-cli users."
  (interactive)
  (elark--run-command
   '("auth" "list")
   (lambda (result)
     (let ((buf (get-buffer-create "*Lark Auth*")))
       (with-current-buffer buf
         (let ((inhibit-read-only t))
           (erase-buffer)
           (insert (propertize "Lark Authenticated Users\n" 'face 'bold)
                   (make-string 60 ?─) "\n\n"
                   (if result (pp-to-string result) "(none)\n")))
         (special-mode)
         (goto-char (point-min)))
       (pop-to-buffer buf)))
   nil
   :literal t :no-error t))

(provide 'elark-auth)
;;; elark-auth.el ends here
