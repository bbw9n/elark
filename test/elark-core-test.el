;;; elark-core-test.el --- Tests for elark-core.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 bbw9n

;; Author: bbw9n <bbw9nio@gmail.com>
;; Assisted-by: Claude:claude-opus-5
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; ERT tests for elark core functions.

;;; Code:

(require 'ert)

;; Add project directories to load-path
(let ((root (expand-file-name ".." (file-name-directory (or load-file-name (buffer-file-name))))))
  (dolist (sub '("." "core" "ui" "domain" "ai"))
    (add-to-list 'load-path (expand-file-name sub root))))

(require 'elark-core)

;;;; elark--format-args tests

(ert-deftest elark-test-format-args-empty ()
  "Empty plist produces empty args."
  (should (equal (elark--format-args nil) nil)))

(ert-deftest elark-test-format-args-key-value ()
  "Key-value pair produces --key value."
  (should (equal (elark--format-args '(:page-size 20))
                 '("--page-size" "20"))))

(ert-deftest elark-test-format-args-boolean ()
  "Boolean t produces a flag with no value."
  (should (equal (elark--format-args '(:verbose t))
                 '("--verbose"))))

(ert-deftest elark-test-format-args-nil-skipped ()
  "Nil values are omitted entirely."
  (should (equal (elark--format-args '(:foo nil :bar "baz"))
                 '("--bar" "baz"))))

(ert-deftest elark-test-format-args-list ()
  "List values produce repeated flags."
  (should (equal (elark--format-args '(:fields ("a" "b")))
                 '("--fields" "a" "--fields" "b"))))

(ert-deftest elark-test-format-args-string-value ()
  "String values are passed through."
  (should (equal (elark--format-args '(:title "My Event"))
                 '("--title" "My Event"))))

(ert-deftest elark-test-format-args-mixed ()
  "Mixed argument types work together."
  (should (equal (elark--format-args '(:page-size 20 :verbose t :skip nil))
                 '("--page-size" "20" "--verbose"))))

;;;; elark--parse-json tests

(ert-deftest elark-test-parse-json-object ()
  "Parse a JSON object to alist."
  (let ((result (elark--parse-json "{\"name\": \"test\", \"count\": 42}")))
    (should (equal (alist-get 'name result) "test"))
    (should (equal (alist-get 'count result) 42))))

(ert-deftest elark-test-parse-json-array ()
  "Parse a JSON array."
  (let ((result (elark--parse-json "[1, 2, 3]")))
    (should (equal result '(1 2 3)))))

(ert-deftest elark-test-parse-json-empty-string ()
  "Empty string returns nil."
  (should (null (elark--parse-json "")))
  (should (null (elark--parse-json "  ")))
  (should (null (elark--parse-json nil))))

(ert-deftest elark-test-parse-json-invalid ()
  "Invalid JSON returns nil without error."
  (should (null (elark--parse-json "not json"))))

;;;; elark--strip-debug-lines tests

(ert-deftest elark-test-strip-debug-lines-removes-prefixed ()
  "Strip lines with [cmd +sub] debug prefixes."
  (let ((input "[vc +recording] fetching recording...\n[vc +recording] done\n{\"data\": 1}"))
    (should (equal (string-trim (elark--strip-debug-lines input))
                   "{\"data\": 1}"))))

(ert-deftest elark-test-strip-debug-lines-preserves-json-array ()
  "JSON arrays starting with [ are not stripped."
  (let ((input "[1, 2, 3]"))
    (should (equal (elark--strip-debug-lines input) input))))

(ert-deftest elark-test-strip-debug-lines-no-debug ()
  "Plain JSON passes through unchanged."
  (let ((input "{\"key\": \"value\"}"))
    (should (equal (elark--strip-debug-lines input) input))))

(ert-deftest elark-test-parse-json-with-debug-lines ()
  "parse-json strips debug lines and parses the JSON."
  (let ((result (elark--parse-json
                 "[vc +recording] querying...\n{\"data\": {\"id\": \"abc\"}}")))
    (should result)
    (should (equal (alist-get 'id (alist-get 'data result)) "abc"))))

(ert-deftest elark-test-parse-json-only-debug-lines ()
  "parse-json returns nil when input is only debug lines."
  (should (null (elark--parse-json
                 "[vc +recording] querying...\n[vc +recording] done\n"))))

(ert-deftest elark-test-strip-debug-lines-removes-tip ()
  "Strip tip lines from CLI output."
  (let ((input "tip: use --verbose for more details\n{\"data\": 1}"))
    (should (equal (string-trim (elark--strip-debug-lines input))
                   "{\"data\": 1}"))))

(ert-deftest elark-test-strip-debug-lines-tip-no-colon ()
  "Strip tip lines without colon separator."
  (let ((input "tip something helpful\n{\"count\": 5}"))
    (should (equal (string-trim (elark--strip-debug-lines input))
                   "{\"count\": 5}"))))

(ert-deftest elark-test-parse-json-with-tip-line ()
  "parse-json strips tip lines and parses the JSON."
  (let ((result (elark--parse-json
                 "tip: login required\n{\"items\": [1, 2]}")))
    (should result)
    (should (equal (alist-get 'items result) '(1 2)))))

(ert-deftest elark-test-strip-debug-lines-removes-warning ()
  "Strip `warning:' prefix lines — `im +chat-messages-list' now emits
\"warning: reactions_partial_failed: …\" before the JSON body, which
made the response unparseable."
  (let ((input "warning: reactions_partial_failed: 1 message(s) failed ([om_xx])\n{\"ok\": true}"))
    (should (equal (string-trim (elark--strip-debug-lines input))
                   "{\"ok\": true}"))))

(ert-deftest elark-test-strip-debug-lines-removes-other-log-levels ()
  "Strip info/debug/error log levels too."
  (dolist (prefix '("info: starting" "debug: x=1" "error: y"))
    (let* ((input (concat prefix "\n{\"ok\": true}"))
           (out (string-trim (elark--strip-debug-lines input))))
      (should (equal "{\"ok\": true}" out)))))

(ert-deftest elark-test-strip-debug-lines-drops-unknown-leading-prefix ()
  "Unknown leading non-JSON lines are dropped until the first `{' or `['."
  (let ((input "Some future prefix lark-cli might emit\n  another fluff line\n[1, 2, 3]"))
    (should (equal (string-trim (elark--strip-debug-lines input))
                   "[1, 2, 3]"))))

(ert-deftest elark-test-strip-debug-lines-preserves-title-field ()
  "JSON containing a key like `title' is not stripped."
  (let ((input "{\"title\": \"tips for success\"}"))
    (should (equal (elark--strip-debug-lines input) input))))

;;;; elark--parse-ndjson tests

(ert-deftest elark-test-parse-ndjson ()
  "Parse newline-delimited JSON."
  (let ((result (elark--parse-ndjson "{\"a\":1}\n{\"b\":2}\n")))
    (should (= (length result) 2))
    (should (equal (alist-get 'a (car result)) 1))
    (should (equal (alist-get 'b (cadr result)) 2))))

(ert-deftest elark-test-parse-ndjson-empty ()
  "Empty NDJSON returns nil."
  (should (null (elark--parse-ndjson ""))))

;;;; elark--handle-error tests

(ert-deftest elark-test-handle-error-success ()
  "Response with code 0 passes through."
  (let ((resp '((code . 0) (data . "ok"))))
    (should (equal (elark--handle-error resp) resp))))

(ert-deftest elark-test-handle-error-nil ()
  "Nil response passes through."
  (should (null (elark--handle-error nil))))

(ert-deftest elark-test-handle-error-signals ()
  "Non-zero code signals user-error."
  (should-error
   (elark--handle-error '((code . 403) (msg . "forbidden")))
   :type 'user-error))

(ert-deftest elark-test-handle-error-ok-true-passes ()
  "ok=t (success) passes through untouched."
  (let ((resp '((ok . t) (data . ((items . nil))))))
    (should (equal (elark--handle-error resp) resp))))

(ert-deftest elark-test-handle-error-ok-false-signals ()
  "ok=:false envelope signals user-error with the embedded message."
  (let ((resp '((ok . :false)
                (error . ((type . "auth")
                          (message . "not authenticated"))))))
    (should-error (elark--handle-error resp) :type 'user-error)
    ;; Confirm the message reaches the signal payload.
    (condition-case err (elark--handle-error resp)
      (user-error
       (should (string-match-p "not authenticated"
                               (error-message-string err)))
       (should (string-match-p "auth" (error-message-string err)))))))

(ert-deftest elark-test-handle-error-ok-false-string-error ()
  "ok=:false with a string error (rather than alist) still signals."
  (let ((resp '((ok . :false) (error . "boom"))))
    (should-error (elark--handle-error resp) :type 'user-error)))

(ert-deftest elark-test-handle-error-no-ok-key-untouched ()
  "Responses without an ok key are still subject to the code check
only — a bare {data: ...} without ok or code passes through."
  (let ((resp '((data . ((items . nil))))))
    (should (equal (elark--handle-error resp) resp))))

;;;; Result dispatch — `elark--dispatch-result' (success vs failure routing)

(ert-deftest elark-test-dispatch-success-calls-callback ()
  "Zero exit parses output and calls CALLBACK, not ON-ERROR."
  (let (cb-result err-called)
    (elark--dispatch-result
     0 "{\"a\": 1}" "finished\n" t nil
     (lambda (r) (setq cb-result r))
     (lambda (&rest _) (setq err-called t)))
    (should (equal (alist-get 'a cb-result) 1))
    (should-not err-called)))

(ert-deftest elark-test-dispatch-nonzero-calls-on-error-not-callback ()
  "Non-zero exit calls ON-ERROR with (code msg) and never CALLBACK.
This is the regression guard for the silent-stall bug: a failed step
must notify ON-ERROR so the caller can advance instead of hanging."
  (let (cb-called err-code err-msg)
    (elark--dispatch-result
     2 "{\"ok\": false, \"error\": {\"message\": \"--content is required\"}}"
     "exited abnormally\n" t nil
     (lambda (&rest _) (setq cb-called t))
     (lambda (code msg) (setq err-code code err-msg msg)))
    (should-not cb-called)
    (should (= err-code 2))
    (should (string-match-p "--content is required" err-msg))))

(ert-deftest elark-test-dispatch-on-error-fires-even-with-no-error ()
  "ON-ERROR runs regardless of NO-ERROR (NO-ERROR only mutes `message')."
  (let (err-called)
    (elark--dispatch-result
     1 "boom" "exited abnormally\n" t nil
     #'ignore
     (lambda (&rest _) (setq err-called t)))
    (should err-called)))

(ert-deftest elark-test-dispatch-nonzero-without-on-error-is-safe ()
  "A non-zero exit with no ON-ERROR neither calls CALLBACK nor errors."
  (let (cb-called)
    (should-not
     (elark--dispatch-result
      1 "boom" "exited\n" t nil
      (lambda (&rest _) (setq cb-called t))
      nil))
    (should-not cb-called)))

(ert-deftest elark-test-dispatch-raw-skips-parsing ()
  "RAW passes the unparsed output string straight to CALLBACK."
  (let (cb-result)
    (elark--dispatch-result
     0 "not json at all" "finished\n" t t
     (lambda (r) (setq cb-result r))
     nil)
    (should (equal cb-result "not json at all"))))

;;;; Auth gate tests — `elark--ensure-auth-for'

(ert-deftest elark-test-ensure-auth-calls-for-non-auth-cmd ()
  "Non-auth commands trigger `elark-auth-ensure'."
  (let ((called 0))
    (cl-letf (((symbol-function 'elark-auth-ensure)
               (lambda () (cl-incf called) t)))
      (elark--ensure-auth-for '("calendar" "+agenda") nil)
      (should (= called 1)))))

(ert-deftest elark-test-ensure-auth-skips-auth-cmd ()
  "Auth subcommands skip the gate to avoid infinite recursion."
  (let ((called 0))
    (cl-letf (((symbol-function 'elark-auth-ensure)
               (lambda () (cl-incf called) t)))
      (elark--ensure-auth-for '("auth" "status") nil)
      (elark--ensure-auth-for '("auth" "login") '(:literal t))
      (should (= called 0)))))

(ert-deftest elark-test-ensure-auth-skips-when-no-auth-check ()
  "`:no-auth-check' bypasses the gate."
  (let ((called 0))
    (cl-letf (((symbol-function 'elark-auth-ensure)
               (lambda () (cl-incf called) t)))
      (elark--ensure-auth-for '("calendar" "+agenda")
                             '(:no-auth-check t))
      (should (= called 0)))))

(ert-deftest elark-test-ensure-auth-no-op-when-unbound ()
  "When `elark-auth-ensure' isn't defined, the gate is a silent
no-op — `elark-core' must load and run with no hard dependency on
`elark-auth'.  This test runs against a clean environment where
the only other tests that touch the symbol use `cl-letf', which
restores it to unbound on exit."
  ;; Defensive: if a previous test or load chain has bound the
  ;; symbol, temporarily unbind it for this case.
  (let ((had-it (fboundp 'elark-auth-ensure))
        (saved (and (fboundp 'elark-auth-ensure)
                    (symbol-function 'elark-auth-ensure))))
    (unwind-protect
        (progn
          (when had-it (fmakunbound 'elark-auth-ensure))
          (should-not (fboundp 'elark-auth-ensure))
          ;; No error, returns nil.
          (should-not (elark--ensure-auth-for
                       '("calendar" "+agenda") nil)))
      (when had-it (fset 'elark-auth-ensure saved)))))

(ert-deftest elark-test-ensure-auth-propagates-user-error ()
  "When `elark-auth-ensure' signals (user declined login), the gate
unwinds the caller — `elark--run-command' never reaches `make-process'."
  (cl-letf (((symbol-function 'elark-auth-ensure)
             (lambda () (user-error "Lark authentication required"))))
    (should-error
     (elark--ensure-auth-for '("calendar" "+agenda") nil)
     :type 'user-error)))

;;;; elark--get-nested tests

(ert-deftest elark-test-get-nested ()
  "Navigate nested alist structure."
  (let ((data '((items . (((name . "first")) ((name . "second")))))))
    (should (equal (elark--get-nested data 'items 0 'name) "first"))
    (should (equal (elark--get-nested data 'items 1 'name) "second"))))

(ert-deftest elark-test-get-nested-missing ()
  "Missing keys return nil."
  (let ((data '((foo . "bar"))))
    (should (null (elark--get-nested data 'missing 'key)))))

;;;; elark--format-timestamp tests

(ert-deftest elark-test-format-timestamp-number ()
  "Format a numeric timestamp."
  (let ((result (elark--format-timestamp 1700000000)))
    (should (stringp result))
    (should (string-match-p "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}" result))))

(ert-deftest elark-test-format-timestamp-string ()
  "Format a string timestamp."
  (let ((result (elark--format-timestamp "1700000000")))
    (should (stringp result))))

(ert-deftest elark-test-format-timestamp-nil ()
  "Nil timestamp returns nil."
  (should (null (elark--format-timestamp nil))))

(ert-deftest elark-test-format-timestamp-zero ()
  "Zero timestamp returns nil."
  (should (null (elark--format-timestamp 0))))

;;;; elark--build-command tests

(ert-deftest elark-test-build-command-basic ()
  "Basic command building — no --format or --as added by default."
  (let ((elark-dry-run nil))
    (let ((result (elark--build-command '("calendar" "+agenda") nil)))
      (should (equal (car result) "calendar"))
      (should (equal (cadr result) "+agenda"))
      (should-not (member "--format" result))
      (should-not (member "--as" result)))))

(ert-deftest elark-test-build-command-with-dry-run ()
  "Dry run flag is included when enabled."
  (let ((elark-dry-run t))
    (let ((result (elark--build-command '("tasks" "create") nil)))
      (should (member "--dry-run" result)))))

(ert-deftest elark-test-build-command-format-override ()
  "Explicit format adds --format flag."
  (let ((elark-dry-run nil))
    (let ((result (elark--build-command '("mail" "+triage") nil "json")))
      (should (member "--format" result))
      (should (member "json" result)))))

(ert-deftest elark-test-build-command-no-format-without-override ()
  "No format arg means no --format flag."
  (let ((elark-dry-run nil))
    (let ((result (elark--build-command '("calendar" "+create") nil)))
      (should-not (member "--format" result)))))

;;;; elark--parse-time-input tests

(ert-deftest elark-test-parse-time-hhmm ()
  "HH:MM expands to today's date in ISO 8601."
  (let ((result (elark--parse-time-input "10:00")))
    (should (string-match-p "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}T10:00:00" result))
    ;; Should include timezone offset
    (should (string-match-p "[+-][0-9]\\{2\\}:[0-9]\\{2\\}$" result))))

(ert-deftest elark-test-parse-time-mmdd-hhmm ()
  "MM-DD HH:MM expands to this year in ISO 8601."
  (let ((result (elark--parse-time-input "04-15 14:30")))
    (should (string-match-p "^[0-9]\\{4\\}-04-15T14:30:00" result))))

(ert-deftest elark-test-parse-time-full ()
  "YYYY-MM-DD HH:MM expands to ISO 8601."
  (let ((result (elark--parse-time-input "2026-04-15 14:30")))
    (should (string-match-p "^2026-04-15T14:30:00" result))))

(ert-deftest elark-test-parse-time-iso8601-passthrough ()
  "Already ISO 8601 is passed through."
  (should (equal (elark--parse-time-input "2026-04-15T14:30:00+08:00")
                 "2026-04-15T14:30:00+08:00")))

(ert-deftest elark-test-parse-time-unknown-passthrough ()
  "Unrecognized input is passed through."
  (should (equal (elark--parse-time-input "tomorrow") "tomorrow")))

;;;; Activity log (*elark-log*)

(ert-deftest elark-core-test-cli-log-writes-labelled-section ()
  "`elark--cli-log' appends a timestamped labelled section to `*elark-log*'."
  (with-current-buffer (get-buffer-create elark--log-buffer-name)
    (let ((inhibit-read-only t)) (erase-buffer))
    (elark--cli-log "CLI REQUEST" "lark-cli %s %s" "auth" "status")
    (let ((text (buffer-substring-no-properties (point-min) (point-max))))
      (should (string-match-p "=== \\[[0-9:.]+\\] CLI REQUEST ===" text))
      (should (string-match-p "lark-cli auth status" text)))))

(ert-deftest elark-core-test-cli-log-output-truncation ()
  "`elark--cli-log-output' clips long output and appends an ellipsis."
  (should (equal "" (elark--cli-log-output nil)))
  (should (equal "abc" (elark--cli-log-output "abc")))
  (let* ((big (make-string 9000 ?x))
         (out (elark--cli-log-output big)))
    (should (= (+ 8000 (length "\n…[truncated]")) (length out)))
    (should (string-suffix-p "\n…[truncated]" out))))

;;;; Stale default-directory guard

(ert-deftest elark-core-test-safe-default-directory ()
  "Falls back to an existing dir when `default-directory' was purged."
  ;; Existing directory passes through untouched.
  (let ((default-directory temporary-file-directory))
    (should (equal temporary-file-directory (elark--safe-default-directory))))
  ;; Deleted directory falls back to something that exists.
  (let* ((dir (make-temp-file "elark-core-test-dir" t))
         (default-directory (file-name-as-directory dir)))
    (delete-directory dir)
    (let ((safe (elark--safe-default-directory)))
      (should-not (equal default-directory safe))
      (should (file-directory-p safe)))))

(ert-deftest elark-core-test-run-command-from-dead-directory ()
  "Spawning from a buffer whose directory was deleted must not error.
Regression: a doc buffer's temp cache dir gets purged by the OS;
`make-process' then fails with \"Setting current directory: No
such file or directory\" unless the spawn site rebinds
`default-directory'."
  (let* ((dir (make-temp-file "elark-core-test-dir" t))
         (default-directory (file-name-as-directory dir))
         (done nil))
    (delete-directory dir)
    (cl-letf (((symbol-function 'elark--executable) (lambda () "true"))
              ((symbol-function 'elark--ensure-auth-for) #'ignore))
      (elark--run-command '("noop") (lambda (_result) (setq done t))
                         nil :no-error t)
      (let ((deadline (+ (float-time) 5)))
        (while (and (not done) (< (float-time) deadline))
          (accept-process-output nil 0.05))))
    (should done)))

;;;; Persistent cache directory

(ert-deftest elark-core-test-cache-dir-persistent ()
  "`elark--cache-dir' resolves under XDG cache home, never the temp dir."
  (let ((xdg (make-temp-file "elark-core-xdg-test" t))
        (old (getenv "XDG_CACHE_HOME")))
    (unwind-protect
        (progn
          (setenv "XDG_CACHE_HOME" xdg)
          (let ((dir (elark--cache-dir "unit-test")))
            (should (equal (file-name-as-directory
                            (expand-file-name "elark/unit-test" xdg))
                           dir))
            (should (file-directory-p dir)))
          ;; Without XDG_CACHE_HOME, fall back to ~/.cache.
          (setenv "XDG_CACHE_HOME" nil)
          (should (string-prefix-p
                   (file-name-as-directory (expand-file-name "~/.cache"))
                   (expand-file-name (elark--cache-dir "unit-test")))))
      (setenv "XDG_CACHE_HOME" old)
      (delete-directory xdg t))))

(ert-deftest elark-core-test-get-nested-null-leaf ()
  "A JSON null leaf returns nil, never the truthy `:null' sentinel."
  (should-not (elark--get-nested '((data . ((items . :null)))) 'data 'items))
  (should-not (elark--get-nested '((a . :null)) 'a))
  (should (equal "x" (elark--get-nested '((a . ((b . "x")))) 'a 'b))))

(ert-deftest elark-core-test-list-field ()
  "`elark--list-field' yields real lists only."
  (should (equal '(1 2) (elark--list-field '((items . (1 2))) 'items)))
  (should-not (elark--list-field '((items . :null)) 'items))
  (should-not (elark--list-field '((items . "str")) 'items))
  (should-not (elark--list-field :null 'items))
  (should-not (elark--list-field nil 'items)))

(provide 'elark-core-test)
;;; elark-core-test.el ends here
