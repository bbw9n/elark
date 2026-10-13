;;; elark-core.el --- Core process management for elark -*- lexical-binding: t; -*-

;; Copyright (C) 2026 bbw9n

;; Author: bbw9n <bbw9nio@gmail.com>
;; Assisted-by: Claude:claude-opus-5
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This module provides the engine that all elark domain modules use
;; to interact with the lark-cli process.  It handles async/sync
;; execution, JSON parsing, error handling, pagination, and argument
;; formatting.

;;; Code:

(require 'json)

;;;; Forward declarations for customization variables defined in elark.el
;; These provide defaults when elark-core is loaded without elark.el.

(defvar elark-cli-executable "lark-cli"
  "Path to the lark-cli executable.")
(defvar elark-default-format "json"
  "Default output format for lark-cli commands.")
(defvar elark-default-identity nil
  "Default identity (\"user\" or \"bot\") for lark-cli commands.")
(defvar elark-dry-run nil
  "When non-nil, pass --dry-run to destructive commands.")

;;;; Variables

(defvar elark--debug nil
  "When non-nil, log lark-cli invocations to *elark-log* buffer.")

(defvar elark--process-output-alist nil
  "Alist mapping process objects to accumulated output strings.")

(defconst elark--log-buffer-name "*elark-log*"
  "Name of the Lark activity log buffer.
This is the focused, always-on record of lark-cli traffic (CLI
requests/responses/errors, auth events).  The AI-internals trace
\(`*Lark AI Debug*' — LLM streams, plan parsing, agent step
orchestration) is a separate buffer with a separate toggle.")

;;;; Executable resolution

(defun elark--executable ()
  "Return the lark-cli executable path.
Signals an error if the executable is not found."
  (let ((exe (or elark-cli-executable "lark-cli")))
    (or (executable-find exe)
        (user-error "Cannot find `%s' executable; install lark-cli or set `elark-cli-executable'" exe))))

;;;; Argument formatting

(defun elark--format-args (plist)
  "Convert PLIST to a flat list of CLI arguments.
Keys are converted from :keyword to --keyword form.
Nil values are omitted.  t values produce a flag with no argument.
List values produce repeated flags.

Example:
  (elark--format-args \\='(:page-size 20 :verbose t :fields (\"a\" \"b\")))
  => (\"--page-size\" \"20\" \"--verbose\" \"--fields\" \"a\" \"--fields\" \"b\")"
  (let (args)
    (while plist
      (let* ((key (pop plist))
             (val (pop plist))
             (flag (concat "--" (substring (symbol-name key) 1))))
        (cond
         ((null val))                   ; skip nil values
         ((eq val t)                    ; boolean flag
          (push flag args))
         ((listp val)                   ; repeated flag
          (dolist (v val)
            (push flag args)
            (push (format "%s" v) args)))
         (t                             ; key-value pair
          (push flag args)
          (push (format "%s" val) args)))))
    (nreverse args)))

(defun elark--build-command (cmd-args &optional extra-args format)
  "Build the full argument list for a lark-cli invocation.
CMD-ARGS is a list of positional arguments (e.g., (\"calendar\" \"+agenda\")).
EXTRA-ARGS is a plist of keyword arguments (converted via `elark--format-args').
FORMAT, when non-nil, adds --format FLAG.  Not added by default since
lark-cli already defaults to json and not all subcommands support it."
  (let ((dry-run elark-dry-run))
    (append cmd-args
            (elark--format-args extra-args)
            (when format (list "--format" format))
            (when dry-run (list "--dry-run")))))

(defun elark--cache-dir (subdir)
  "Return a persistent per-user cache directory for SUBDIR, creating it.
Lives under XDG cache home (usually ~/.cache/elark/SUBDIR/) so
cached media survives reboots and OS temp-directory cleanup —
unlike variable `temporary-file-directory', which macOS purges periodically,
forcing every image to re-download."
  (let ((dir (file-name-as-directory
              (expand-file-name
               (concat "elark/" subdir)
               (or (getenv "XDG_CACHE_HOME") "~/.cache")))))
    (make-directory dir t)
    dir))

(defun elark--safe-default-directory ()
  "Return `default-directory' if it still exists, else a safe fallback.
Some elark buffers point `default-directory' at a temp cache dir
\(e.g. a doc buffer's image dir under variable `temporary-file-directory')
that the OS may purge.  Spawning a process from such a buffer
fails with \"Setting current directory: No such file or
directory\", so every spawn site binds `default-directory' to
this value first."
  (if (and default-directory (file-directory-p default-directory))
      default-directory
    (expand-file-name "~/")))

;;;; Logging

(defun elark--log (format-string &rest args)
  "Append a trace line to `*elark-log*' when `elark--debug' is on.
The line is FORMAT-STRING formatted with ARGS.
Plain timestamped lines; for the always-on labelled activity log
\(CLI requests/responses, auth events) see `elark--cli-log'."
  (when elark--debug
    (with-current-buffer (get-buffer-create elark--log-buffer-name)
      (goto-char (point-max))
      (insert (format-time-string "[%H:%M:%S] ")
              (apply #'format format-string args)
              "\n"))))

(defun elark--cli-log (label fmt &rest args)
  "Append a labelled, timestamped section to `*elark-log*'.
LABEL is a short tag (e.g. \"CLI REQUEST\", \"CLI RESPONSE\", \"AUTH\");
FMT and ARGS are passed to `format' to produce the body.  Always on —
this buffer is intentionally a permanent activity record so failures
can be diagnosed without flipping a debug flag first."
  (let ((entry (apply #'format fmt args)))
    (with-current-buffer (get-buffer-create elark--log-buffer-name)
      (goto-char (point-max))
      (insert (format "\n=== [%s] %s ===\n%s\n"
                      (format-time-string "%H:%M:%S.%3N")
                      label entry)))))

(defun elark--cli-log-output (output)
  "Return OUTPUT clipped to a sane length for the activity log."
  (let ((s (or output "")))
    (if (> (length s) 8000)
        (concat (substring s 0 8000) "\n…[truncated]")
      s)))

;;;###autoload
(defun elark-show-log ()
  "Pop up the `*elark-log*' buffer (Lark activity: CLI + auth traffic).
For AI-internals (LLM streams, plan parsing, agent steps), see
`elark-ai-show-debug' / `elark-ai-toggle-debug' instead — different
buffer, different concern."
  (interactive)
  (let ((buf (get-buffer elark--log-buffer-name)))
    (if buf
        (pop-to-buffer buf)
      (user-error "No %s buffer yet — make a elark command first"
                  elark--log-buffer-name))))

;;;; JSON parsing

(defun elark--strip-debug-lines (string)
  "Remove CLI debug/log lines from STRING so the rest is valid JSON.

Two passes:
1. Strip lines matching known noisy prefixes anywhere in the output —
   `[command +subcommand]` traces (e.g. `[vc +recording] …`), `tip:`
   hint lines, and `warning:` / `info:` / `debug:` / `error:` log
   levels.  lark-cli occasionally prepends one of these to stdout
   before the JSON body (recent example: `warning:
   reactions_partial_failed: …` from `im +chat-messages-list`), which
   makes the response unparseable.
2. As a defence against unknown future prefixes, drop any remaining
   leading lines before the first line whose first non-whitespace
   character is `{` or `[` (the start of a JSON value)."
  (let* ((lines (split-string string "\n"))
         (kept (seq-remove
                (lambda (line)
                  (or (string-match-p "^\\[[-a-zA-Z0-9_]+ \\+[-a-zA-Z0-9_]+\\]" line)
                      (string-match-p "^tip[ :]" line)
                      (string-match-p "^\\(warning\\|info\\|debug\\|error\\)[ :]"
                                      line)))
                lines))
         (trimmed (let ((p kept))
                    (while (and p
                                (let ((ln (string-trim-left (car p))))
                                  (and (not (string-empty-p ln))
                                       (not (memq (aref ln 0) '(?\{ ?\[))))))
                      (setq p (cdr p)))
                    p)))
    (mapconcat #'identity (or trimmed '()) "\n")))

(defun elark--parse-json (string)
  "Parse STRING as JSON, returning an alist.
Returns nil if STRING is empty or not valid JSON.
Debug lines from the CLI (e.g. `[vc +recording] ...') are stripped first."
  (when (and string (not (string-empty-p (string-trim string))))
    (let ((cleaned (elark--strip-debug-lines string)))
      (when (not (string-empty-p (string-trim cleaned)))
        (condition-case err
            (json-parse-string cleaned :object-type 'alist :array-type 'list)
          (json-parse-error
           (elark--log "JSON parse error: %s\nInput: %s" (error-message-string err) cleaned)
           nil))))))

(defun elark--parse-ndjson (string)
  "Parse STRING as newline-delimited JSON, returning a list of alists."
  (when (and string (not (string-empty-p (string-trim string))))
    (let ((lines (split-string (string-trim string) "\n" t)))
      (delq nil (mapcar #'elark--parse-json lines)))))

;;;; Error handling

(defun elark--handle-error (response)
  "Check RESPONSE for errors and signal `user-error' if found.
RESPONSE should be an alist parsed from lark-cli JSON output.
Returns RESPONSE if no error is detected.

Recognises two error envelopes:
- Lark API style: {code: N, msg: STR, data: ...} — signals when
  code is non-zero and not nil.
- lark-cli wrapper style: {ok: :false, error: {message, type}, ...}
  — signals when `ok' is the JSON false sentinel.  Without this
  branch an auth-failed or rejected request looks identical to a
  successful empty result, and the callback silently receives nil."
  (when (and response (listp response))
    (let ((code (alist-get 'code response))
          (msg (alist-get 'msg response))
          (ok-key (assq 'ok response)))
      (cond
       ;; Lark API error: code != 0
       ((and code (not (eq code 0)))
        (user-error "Lark API error (code %s): %s"
                    code (or msg "unknown error")))
       ;; CLI wrapper error: explicit ok = false
       ((and ok-key (eq (cdr ok-key) :false))
        (let* ((err (alist-get 'error response))
               (emsg (or (and (listp err) (alist-get 'message err))
                         (and (stringp err) err)))
               (etype (and (listp err) (alist-get 'type err))))
          (user-error "Lark CLI error%s: %s"
                      (if etype (format " (%s)" etype) "")
                      (or emsg "unknown error")))))))
  response)

;;;; Auth gate
;;
;; `elark--run-command' and `elark--run-command-sync' route through
;; `elark--ensure-auth-for' before spawning so that an unauthenticated
;; user gets a clear prompt instead of a silent CLI failure.  The
;; helper is a soft dependency on `elark-auth' (via `fboundp'); core
;; stays loadable on its own.

(defun elark--ensure-auth-for (cmd-args keys)
  "Run the auth gate before spawning a lark-cli call.
Calls `elark-auth-ensure' when it is defined.  Skipped when:
- CMD-ARGS starts with \"auth\" (the gate itself runs lark-cli to
  probe status, so calling the gate would recurse infinitely), or
- KEYS contains `:no-auth-check' with a non-nil value (escape hatch
  for callers that want to bypass — currently used by the gate's
  own status probe)."
  (unless (or (equal (car cmd-args) "auth")
              (plist-get keys :no-auth-check))
    (when (fboundp 'elark-auth-ensure)
      (elark-auth-ensure))))

;;;; Async process execution

(defun elark--dispatch-result (exit-code output event no-error raw callback on-error)
  "Route a finished process to CALLBACK or ON-ERROR.
EXIT-CODE is the process exit status, OUTPUT the accumulated stdout,
and EVENT the sentinel event string.

On a non-zero (or non-integer/abnormal) EXIT-CODE: optionally `message'
the error unless NO-ERROR, then call ON-ERROR with (EXIT-CODE MESSAGE)
when given.  ON-ERROR runs regardless of NO-ERROR so callers can react
to a failure instead of stalling — CALLBACK only fires on success.

On a zero EXIT-CODE: parse OUTPUT (unless RAW), run `elark--handle-error'
unless RAW or NO-ERROR, then call CALLBACK with the result."
  (cond
   ((not (and (integerp exit-code) (zerop exit-code)))
    (let ((msg (or (and output
                        (not (string-empty-p (string-trim output)))
                        (string-trim output))
                   (string-trim (or event "")))))
      (unless no-error
        (message "Lark CLI error (exit %s): %s" exit-code msg))
      (when on-error
        (funcall on-error exit-code msg))))
   (callback
    (let ((result (if raw output (elark--parse-json output))))
      (unless (or raw no-error)
        (elark--handle-error result))
      (funcall callback result)))))

(defun elark--run-command (cmd-args callback &optional extra-args &rest keys)
  "Run a lark-cli command asynchronously.

CMD-ARGS is a list of positional arguments (e.g., (\"calendar\" \"+agenda\")).
CALLBACK is called with the parsed JSON result on success.
EXTRA-ARGS is a plist of keyword arguments.
KEYS are keyword arguments:
  :format         - output format (default: `elark-default-format')
  :raw            - pass raw string to callback instead of parsed JSON
  :no-error       - don't signal errors from the response
  :literal        - use CMD-ARGS as-is (skip --format/--as/--dry-run)
  :no-auth-check  - skip the `elark-auth-ensure' gate (rarely needed)
  :on-error       - function called as (EXIT-CODE MESSAGE) when the
                    process exits non-zero or is killed by :timeout.
                    Runs regardless of :no-error.  Lets callers react to
                    failures instead of stalling because CALLBACK (which
                    only fires on success) never runs.
  :timeout        - seconds; if the process is still alive after this, it
                    is killed (routing through :on-error) so a hung CLI
                    can't stall the caller forever.

Returns the process object."
  (elark--ensure-auth-for cmd-args keys)
  (let* ((format (plist-get keys :format))
         (raw (plist-get keys :raw))
         (no-error (plist-get keys :no-error))
         (literal (plist-get keys :literal))
         (on-error (plist-get keys :on-error))
         (timeout (plist-get keys :timeout))
         (full-args (if literal cmd-args
                      (elark--build-command cmd-args extra-args format)))
         (exe (elark--executable))
         (proc-name (format "elark-%s" (string-join cmd-args "-")))
         proc timer)
    (elark--log "Running: %s %s" exe (string-join full-args " "))
    (elark--cli-log "CLI REQUEST" "lark-cli %s" (string-join full-args " "))
    (setq proc
          (let ((default-directory (elark--safe-default-directory)))
            (make-process
             :name proc-name
             :command (cons exe full-args)
             :connection-type 'pipe
             :noquery t
             :filter (lambda (proc output)
                       (let ((existing (alist-get proc elark--process-output-alist)))
                         (setf (alist-get proc elark--process-output-alist)
                               (concat (or existing "") output))))
             :sentinel (lambda (proc event)
                         (unwind-protect
                             (let ((output (alist-get proc elark--process-output-alist))
                                   (exit-code (process-exit-status proc)))
                               (when (timerp timer) (cancel-timer timer))
                               (elark--log "Process %s exited (%s): %s"
                                          (process-name proc) exit-code
                                          (string-trim event))
                               (elark--cli-log
                                (if (and (integerp exit-code) (zerop exit-code))
                                    "CLI RESPONSE" "CLI ERROR")
                                "exit %s\n%s"
                                exit-code
                                (elark--cli-log-output output))
                               (elark--dispatch-result
                                exit-code output event no-error raw callback on-error))
                           (setf (alist-get proc elark--process-output-alist nil t) nil))))))
    (when (and timeout (numberp timeout) (> timeout 0))
      (setq timer
            (run-with-timer
             timeout nil
             (lambda ()
               (when (process-live-p proc)
                 (elark--log "Killing %s after %ss timeout" proc-name timeout)
                 (kill-process proc))))))
    proc))

;;;; Sync process execution

(defun elark--run-command-sync (cmd-args &optional extra-args &rest keys)
  "Run a lark-cli command synchronously and return the parsed result.

CMD-ARGS is a list of positional arguments.
EXTRA-ARGS is a plist of keyword arguments.
KEYS are keyword arguments:
  :format         - output format (default: `elark-default-format')
  :raw            - return raw string instead of parsed JSON
  :no-error       - don't signal errors from the response
  :literal        - use CMD-ARGS as-is (skip --format/--as/--dry-run)
  :no-auth-check  - skip the `elark-auth-ensure' gate (rarely needed)"
  (elark--ensure-auth-for cmd-args keys)
  (let* ((format (plist-get keys :format))
         (raw (plist-get keys :raw))
         (no-error (plist-get keys :no-error))
         (literal (plist-get keys :literal))
         (full-args (if literal cmd-args
                      (elark--build-command cmd-args extra-args format)))
         (exe (elark--executable))
         (output-buf (generate-new-buffer " *elark-sync*"))
         exit-code output)
    (elark--log "Running (sync): %s %s" exe (string-join full-args " "))
    (elark--cli-log "CLI REQUEST" "lark-cli %s" (string-join full-args " "))
    (unwind-protect
        (progn
          (setq exit-code
                (let ((default-directory (elark--safe-default-directory)))
                  (apply #'call-process exe nil output-buf nil full-args)))
          (setq output (with-current-buffer output-buf (buffer-string)))
          (elark--log "Sync exit %d: %s" exit-code (truncate-string-to-width output 200))
          (elark--cli-log
           (if (zerop exit-code) "CLI RESPONSE" "CLI ERROR")
           "exit %d\n%s" exit-code (elark--cli-log-output output))
          (if (not (zerop exit-code))
              (unless no-error
                (user-error "Lark CLI error (exit %d): %s"
                            exit-code (string-trim output)))
            (if raw
                output
              (let ((result (elark--parse-json output)))
                (unless no-error
                  (elark--handle-error result))
                result))))
      (kill-buffer output-buf))))

;;;; Pagination

(defun elark--paginate (cmd-args callback &optional extra-args &rest keys)
  "Run a lark-cli command with --page-all and collect all results.

CMD-ARGS, EXTRA-ARGS, and KEYS are as in `elark--run-command'.
CALLBACK receives the full parsed result (lark-cli handles pagination
internally with --page-all)."
  (let ((paginate-args (append extra-args '(:page-all t))))
    (apply #'elark--run-command cmd-args callback paginate-args keys)))

;;;; Utility functions

(defun elark--get-nested (alist &rest keys)
  "Access a nested value in ALIST by following KEYS.
Example: (elark--get-nested data \\='items 0 \\='name)
A JSON null leaf (the `:null' sentinel) is returned as nil — JSON
null means absent, and the truthy sentinel crashes list/string
operations downstream."
  (let ((current alist))
    (dolist (key keys)
      (setq current
            (cond
             ((and (integerp key) (listp current))
              (nth key current))
             ((listp current)
              (alist-get key current))
             (t nil))))
    (if (eq current :null) nil current)))

(defun elark--record-list-p (v)
  "Return non-nil when V is a list of records (alists), not one alist.
A parsed JSON array of objects is a list whose first element is
itself an alist — i.e. its car's car is a cons.  A response
envelope like ((has_more . :false) (items . :null)) fails this:
its first element's car is a bare symbol."
  (and (listp v)
       (consp (car-safe v))
       (consp (car-safe (car-safe v)))))

(defun elark--list-field (alist key)
  "Return ALIST's value for KEY when it is a real list, else nil.
lark-cli encodes empty collections as JSON null, which parses to
the truthy `:null' sentinel and is fatal to list operations
\(\"items\": null → seq-filter over `:null' → wrong-type-argument).
Use this instead of `alist-get' wherever a list is expected."
  (and (listp alist)
       (let ((v (alist-get key alist)))
         (and (listp v) v))))

(defun elark--format-timestamp (timestamp)
  "Format a Unix TIMESTAMP (string or number) to a human-readable string."
  (when timestamp
    (let ((ts (if (stringp timestamp) (string-to-number timestamp) timestamp)))
      (when (> ts 0)
        (format-time-string "%Y-%m-%d %H:%M" (seconds-to-time ts))))))

;;;; Time input parsing

(defun elark--parse-time-input (input)
  "Parse a human-friendly time INPUT into ISO 8601 with timezone.
Accepted formats:
  HH:MM                 — today at HH:MM
  MM-DD HH:MM           — this year, month-day at HH:MM
  YYYY-MM-DD HH:MM      — full date and time
  YYYY-MM-DDTHH:MM:SS+… — already ISO 8601, passed through
Returns a string like \"2026-04-11T10:00:00+08:00\"."
  (let ((s (string-trim input)))
    (cond
     ;; Already ISO 8601 (contains T or full datetime with timezone)
     ((string-match-p "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}T" s)
      s)
     ;; YYYY-MM-DD HH:MM
     ((string-match "^\\([0-9]\\{4\\}\\)-\\([0-9]\\{1,2\\}\\)-\\([0-9]\\{1,2\\}\\)[[:space:]]+\\([0-9]\\{1,2\\}\\):\\([0-9]\\{2\\}\\)$" s)
      (elark--build-iso8601
       (string-to-number (match-string 1 s))
       (string-to-number (match-string 2 s))
       (string-to-number (match-string 3 s))
       (string-to-number (match-string 4 s))
       (string-to-number (match-string 5 s))))
     ;; MM-DD HH:MM
     ((string-match "^\\([0-9]\\{1,2\\}\\)-\\([0-9]\\{1,2\\}\\)[[:space:]]+\\([0-9]\\{1,2\\}\\):\\([0-9]\\{2\\}\\)$" s)
      (elark--build-iso8601
       (string-to-number (format-time-string "%Y"))
       (string-to-number (match-string 1 s))
       (string-to-number (match-string 2 s))
       (string-to-number (match-string 3 s))
       (string-to-number (match-string 4 s))))
     ;; HH:MM — today
     ((string-match "^\\([0-9]\\{1,2\\}\\):\\([0-9]\\{2\\}\\)$" s)
      (elark--build-iso8601
       (string-to-number (format-time-string "%Y"))
       (string-to-number (format-time-string "%m"))
       (string-to-number (format-time-string "%d"))
       (string-to-number (match-string 1 s))
       (string-to-number (match-string 2 s))))
     ;; Fallback: pass through as-is
     (t s))))

(defun elark--tz-offset ()
  "Return the local timezone offset as ISO 8601 (e.g. \"+08:00\")."
  (let ((tz (format-time-string "%z")))
    ;; %z gives "+0800", ISO 8601 needs "+08:00"
    (concat (substring tz 0 3) ":" (substring tz 3 5))))

(defun elark--build-iso8601 (year month day hour minute)
  "Build an ISO 8601 string from YEAR, MONTH, DAY, HOUR, MINUTE with local tz."
  (format "%04d-%02d-%02dT%02d:%02d:00%s"
          year month day hour minute (elark--tz-offset)))

(defun elark--format-time-iso8601 (&optional time)
  "Format TIME (default now) as ISO 8601 with timezone offset."
  (concat (format-time-string "%Y-%m-%dT%H:%M:%S" time)
          (elark--tz-offset)))

(provide 'elark-core)
;;; elark-core.el ends here
