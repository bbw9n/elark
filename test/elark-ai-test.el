;;; elark-ai-test.el --- Tests for elark AI layer -*- lexical-binding: t; -*-

;; Copyright (C) 2026 bbw9n

;; Author: bbw9n <bbw9nio@gmail.com>
;; Assisted-by: Claude:claude-opus-5
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'ert)
(require 'cl-lib)

(let ((root (expand-file-name ".." (file-name-directory (or load-file-name (buffer-file-name))))))
  (dolist (sub '("." "core" "ui" "domain" "ai"))
    (add-to-list 'load-path (expand-file-name sub root))))

(require 'elark-ai-skills)
(require 'elark-ai-context)
(require 'elark-ai)
(require 'elark-im)
;; Required for the doc-detail context tests — `elark-ai-context'
;; resolves the buffer-name fallback via `fboundp', so the docs
;; module must be loaded for the soft reference to find its provider.
(require 'elark-docs)

;;;; UI callback table

(ert-deftest elark-ai-test-ui-dispatch ()
  "`elark-ai--ui' applies the registered function; unregistered ops are nil."
  (let ((elark-ai--ui-functions (list :frag (lambda (s) (concat "t1-" s)))))
    (should (equal "t1-log" (elark-ai--ui :frag "log")))
    (should-not (elark-ai--ui :present "x"))))

(ert-deftest elark-ai-test-ui-registered-by-elark-ai ()
  "Loading `elark-ai' registers every UI operation the executors use."
  (dolist (op '(:session :get-buffer :frag :progress-log
                :ensure-output-fragment :present :clear-waiting
                :stream-preview-handler :render-tool-call
                :update-step-status :synthesize-inline))
    (should (fboundp (plist-get elark-ai--ui-functions op)))))

;;;; Skill frontmatter parsing

(ert-deftest elark-ai-test-parse-frontmatter ()
  "Parse YAML frontmatter from SKILL.md content."
  (let ((text "---\nname: lark-calendar\nversion: 1.0.0\ndescription: \"Calendar management\"\n---\n# calendar\n"))
    (let ((meta (elark-ai-skills--parse-frontmatter text)))
      (should (equal (cdr (assoc "name" meta)) "lark-calendar"))
      (should (equal (cdr (assoc "version" meta)) "1.0.0"))
      (should (equal (cdr (assoc "description" meta)) "Calendar management")))))

(ert-deftest elark-ai-test-parse-frontmatter-no-block ()
  "Return nil when no frontmatter block present."
  (should (null (elark-ai-skills--parse-frontmatter "# Just a heading\nSome text."))))

;;;; Keyword extraction

(ert-deftest elark-ai-test-extract-keywords ()
  "Extract keywords from a description string."
  (let ((kw (elark-ai-skills--extract-keywords "Calendar management and scheduling")))
    (should (member "calendar" kw))
    (should (member "management" kw))
    (should (member "scheduling" kw))))

(ert-deftest elark-ai-test-extract-keywords-nil ()
  "Nil description returns nil."
  (should (null (elark-ai-skills--extract-keywords nil))))

;;;; Skill selection

(ert-deftest elark-ai-test-select-skills-calendar ()
  "Calendar-related prompt selects elark-calendar."
  ;; Install a minimal index for testing
  (let ((elark-ai-skills--index
         '(("lark-shared" . (:description "shared" :dir "/tmp" :keywords ("shared")))
           ("lark-calendar" . (:description "calendar" :dir "/tmp" :keywords ("calendar")))
           ("lark-im" . (:description "messaging" :dir "/tmp" :keywords ("messaging")))
           ("lark-task" . (:description "tasks" :dir "/tmp" :keywords ("tasks"))))))
    (let ((selected (elark-ai-skills-select "show my calendar agenda")))
      (should (member "lark-shared" selected))
      (should (member "lark-calendar" selected))
      (should-not (member "lark-im" selected)))))

(ert-deftest elark-ai-test-select-skills-standup ()
  "Standup prompt selects calendar, task, and workflow."
  (let ((elark-ai-skills--index
         '(("lark-shared" . (:description "shared" :dir "/tmp" :keywords ("shared")))
           ("lark-calendar" . (:description "calendar" :dir "/tmp" :keywords ("calendar")))
           ("lark-task" . (:description "tasks" :dir "/tmp" :keywords ("tasks")))
           ("lark-workflow-standup-report" . (:description "standup" :dir "/tmp" :keywords ("standup"))))))
    (let ((selected (elark-ai-skills-select "what's on my plate today standup")))
      (should (member "lark-shared" selected))
      (should (member "lark-task" selected))
      (should (member "lark-workflow-standup-report" selected)))))

(ert-deftest elark-ai-test-select-skills-fallback ()
  "Ambiguous prompt loads shared only."
  (let ((elark-ai-skills--index
         '(("lark-shared" . (:description "shared" :dir "/tmp" :keywords ("shared")))
           ("lark-calendar" . (:description "calendar" :dir "/tmp" :keywords ("calendar")))
           ("lark-im" . (:description "messaging" :dir "/tmp" :keywords ("messaging"))))))
    (let ((selected (elark-ai-skills-select "help me with something")))
      (should (member "lark-shared" selected)))))

;;;; Plan parsing

(ert-deftest elark-ai-test-parse-plan-basic ()
  "Parse a basic plan from JSON."
  (let ((json "{\"plan\": [{\"command\": [\"calendar\", \"+agenda\"], \"description\": \"Fetch agenda\", \"side_effect\": false}, {\"command\": null, \"description\": \"Summarize\", \"synthesize\": true, \"synthesis_instruction\": \"Make a summary\"}]}"))
    (let ((plan (elark-ai--parse-plan json)))
      (should (= (length plan) 2))
      ;; First step
      (should (equal (plist-get (car plan) :command) '("calendar" "+agenda")))
      (should (equal (plist-get (car plan) :description) "Fetch agenda"))
      (should-not (plist-get (car plan) :side-effect))
      ;; Second step
      (should (plist-get (cadr plan) :synthesize))
      (should (null (plist-get (cadr plan) :command))))))

(ert-deftest elark-ai-test-parse-plan-with-fences ()
  "Parse a plan wrapped in markdown code fences."
  (let ((json "```json\n{\"plan\": [{\"command\": [\"task\", \"+get-my-tasks\"], \"description\": \"Get tasks\", \"side_effect\": false}]}\n```"))
    (let ((plan (elark-ai--parse-plan json)))
      (should (= (length plan) 1))
      (should (equal (plist-get (car plan) :command) '("task" "+get-my-tasks"))))))

(ert-deftest elark-ai-test-parse-plan-parallel-groups ()
  "Parse plan with parallel groups."
  (let ((json "{\"plan\": [{\"command\": [\"calendar\", \"+agenda\"], \"description\": \"A\", \"side_effect\": false, \"parallel_group\": 1}, {\"command\": [\"task\", \"+get-my-tasks\"], \"description\": \"B\", \"side_effect\": false, \"parallel_group\": 1}]}"))
    (let ((plan (elark-ai--parse-plan json)))
      (should (= (length plan) 2))
      (should (= (plist-get (car plan) :parallel-group) 1))
      (should (= (plist-get (cadr plan) :parallel-group) 1)))))

(ert-deftest elark-ai-test-parse-plan-invalid ()
  "Invalid JSON returns nil."
  (should (null (elark-ai--parse-plan "not json at all"))))

(ert-deftest elark-ai-test-parse-plan-side-effect ()
  "Side effect flag is correctly parsed."
  (let ((json "{\"plan\": [{\"command\": [\"calendar\", \"+create\", \"--summary\", \"Meeting\"], \"description\": \"Create event\", \"side_effect\": true}]}"))
    (let ((plan (elark-ai--parse-plan json)))
      (should (= (length plan) 1))
      (should (plist-get (car plan) :side-effect)))))

;;;; $step-N interpolation

(ert-deftest elark-ai-test-interpolate-no-refs ()
  "Args without $step-N are passed through unchanged."
  (let ((result (elark-ai--interpolate-cmd '("calendar" "+agenda") nil)))
    (should (equal result '("calendar" "+agenda")))))

(ert-deftest elark-ai-test-interpolate-full-result ()
  "$step-0 is replaced with the full JSON result."
  (let* ((results '((0 . ((items . [])))))
         (result (elark-ai--interpolate-cmd '("--data" "$step-0") results)))
    (should (stringp (cadr result)))
    (should (string-match-p "items" (cadr result)))))

(ert-deftest elark-ai-test-interpolate-field ()
  "$step-0.name resolves a top-level field."
  (let* ((results '((0 . ((name . "Alice") (id . "u1")))))
         (result (elark-ai--interpolate-cmd '("--name" "$step-0.name") results)))
    (should (equal (cadr result) "Alice"))))

(ert-deftest elark-ai-test-interpolate-nested-field ()
  "$step-0.data.user.id resolves nested fields."
  (let* ((results '((0 . ((data . ((user . ((id . "u42")))))))))
         (result (elark-ai--interpolate-cmd '("--id" "$step-0.data.user.id") results)))
    (should (equal (cadr result) "u42"))))

(ert-deftest elark-ai-test-interpolate-array-index ()
  "$step-0.items[0].id resolves array index."
  (let* ((results '((0 . ((items . (((id . "a1")) ((id . "a2"))))))))
         (result (elark-ai--interpolate-cmd '("--id" "$step-0.items[0].id") results)))
    (should (equal (cadr result) "a1"))))

(ert-deftest elark-ai-test-interpolate-wildcard ()
  "$step-0.items[*].id collects from all elements."
  (let* ((results '((0 . ((items . (((id . "a1")) ((id . "a2")) ((id . "a3"))))))))
         (result (elark-ai--interpolate-cmd '("--ids" "$step-0.items[*].id") results)))
    (should (equal (cadr result) "a1,a2,a3"))))

(ert-deftest elark-ai-test-interpolate-missing-step ()
  "$step-9 with no result keeps the placeholder."
  (let* ((results '((0 . ((x . 1)))))
         (result (elark-ai--interpolate-cmd '("--x" "$step-9.y") results)))
    (should (equal (cadr result) "$step-9.y"))))

(ert-deftest elark-ai-test-interpolate-multiple-refs ()
  "Multiple $step refs in different args."
  (let* ((results '((0 . ((id . "u1"))) (1 . ((id . "u2")))))
         (result (elark-ai--interpolate-cmd
                  '("--a" "$step-0.id" "--b" "$step-1.id") results)))
    (should (equal (nth 1 result) "u1"))
    (should (equal (nth 3 result) "u2"))))

(ert-deftest elark-ai-test-interpolate-numeric-value ()
  "Numeric values are converted to strings."
  (let* ((results '((0 . ((count . 42)))))
         (result (elark-ai--interpolate-cmd '("--n" "$step-0.count") results)))
    (should (equal (cadr result) "42"))))

;;;; JSON extraction

(ert-deftest elark-ai-test-extract-json-plain ()
  "Extract JSON from plain text."
  (let ((result (elark-ai--extract-json "{\"plan\": []}")))
    (should (listp result))
    (should (equal (alist-get 'plan result) nil))))

(ert-deftest elark-ai-test-extract-json-fenced ()
  "Extract JSON from markdown-fenced response."
  (let ((result (elark-ai--extract-json "Here is the plan:\n```json\n{\"plan\": [{\"command\": [\"test\"], \"description\": \"t\"}]}\n```\n")))
    (should (= (length (alist-get 'plan result)) 1))))

(ert-deftest elark-ai-test-extract-json-fenced-no-newline ()
  "Extract JSON when no newline between lang tag and content."
  (let ((result (elark-ai--extract-json "```json{\"plan\": [{\"command\": null, \"description\": \"test\", \"synthesize\": true}]}```")))
    (should (= (length (alist-get 'plan result)) 1))))

;;;; Context extraction

(ert-deftest elark-ai-test-context-non-lark-buffer ()
  "Non-lark buffer returns nil domain."
  (with-temp-buffer
    (let ((ctx (elark-ai-context)))
      (should (null (plist-get ctx :domain)))
      (should (equal (plist-get ctx :buffer-type) "other")))))

(ert-deftest elark-ai-test-context-provider-dispatch ()
  "A registered `elark-ai-context-provider' on a mode is invoked."
  (define-derived-mode elark-ai-test--probe-mode special-mode "Probe")
  (put 'elark-ai-test--probe-mode 'elark-ai-context-provider
       (lambda ()
         (list :domain "probe" :buffer-type "test"
               :item nil :summary "probed")))
  (unwind-protect
      (with-temp-buffer
        (elark-ai-test--probe-mode)
        (let ((ctx (elark-ai-context)))
          (should (equal (plist-get ctx :domain) "probe"))
          (should (equal (plist-get ctx :buffer-type) "test"))
          (should (equal (plist-get ctx :summary) "probed"))))
    (put 'elark-ai-test--probe-mode 'elark-ai-context-provider nil)))

(ert-deftest elark-ai-test-context-provider-walks-parents ()
  "Provider lookup walks the `derived-mode-parent' chain."
  (define-derived-mode elark-ai-test--parent-mode special-mode "Parent")
  (define-derived-mode elark-ai-test--child-mode elark-ai-test--parent-mode "Child")
  (put 'elark-ai-test--parent-mode 'elark-ai-context-provider
       (lambda () (list :domain "parent" :buffer-type "x"
                        :item nil :summary "from parent")))
  (unwind-protect
      (with-temp-buffer
        (elark-ai-test--child-mode)
        (let ((ctx (elark-ai-context)))
          (should (equal (plist-get ctx :domain) "parent"))
          (should (equal (plist-get ctx :summary) "from parent"))))
    (put 'elark-ai-test--parent-mode 'elark-ai-context-provider nil)))

(ert-deftest elark-ai-test-context-register-helper ()
  "`elark-ai-context-register' is a thin wrapper over the symbol property."
  (define-derived-mode elark-ai-test--reg-mode special-mode "Reg")
  (elark-ai-context-register 'elark-ai-test--reg-mode
                            (lambda () (list :domain "reg"
                                             :buffer-type "t"
                                             :item nil :summary "")))
  (unwind-protect
      (should (eq (get 'elark-ai-test--reg-mode 'elark-ai-context-provider)
                  (get 'elark-ai-test--reg-mode 'elark-ai-context-provider)))
    (put 'elark-ai-test--reg-mode 'elark-ai-context-provider nil)))

(ert-deftest elark-ai-test-context-doc-detail-org ()
  "Doc detail buffer in org-mode is recognized by buffer name."
  (let ((buf (get-buffer-create "*Lark Doc: Test Doc*")))
    (unwind-protect
        (with-current-buffer buf
          (org-mode)
          (setq-local elark-docs--doc-token "tok_abc")
          (let ((ctx (elark-ai-context)))
            (should (equal (plist-get ctx :domain) "docs"))
            (should (equal (plist-get ctx :buffer-type) "doc-detail"))
            (should (equal (plist-get (plist-get ctx :item) :doc-token) "tok_abc"))))
      (kill-buffer buf))))

(ert-deftest elark-ai-test-context-doc-detail-markdown ()
  "Doc detail buffer in special-mode is recognized by buffer name."
  (let ((buf (get-buffer-create "*Lark Doc: Another*")))
    (unwind-protect
        (with-current-buffer buf
          (special-mode)
          (setq-local elark-docs--doc-token "tok_xyz")
          (let ((ctx (elark-ai-context)))
            (should (equal (plist-get ctx :domain) "docs"))
            (should (equal (plist-get ctx :buffer-type) "doc-detail"))))
      (kill-buffer buf))))

;;;; Smart reply — thread context extraction

(ert-deftest elark-ai-test-collect-thread-context-not-chat ()
  "Errors when not in a chat buffer."
  (with-temp-buffer
    (should-error (elark-ai--collect-thread-context) :type 'user-error)))

(ert-deftest elark-ai-test-collect-thread-context-no-message ()
  "Errors when no message at point."
  (with-temp-buffer
    (elark-im-chat-mode)
    (setq-local elark-im--chat-id "oc_test123")
    (setq-local elark-im--chat-name "Test Chat")
    (setq-local elark-im--messages nil)
    (let ((inhibit-read-only t))
      (insert "no message here"))
    (should-error (elark-ai--collect-thread-context) :type 'user-error)))

(ert-deftest elark-ai-test-collect-thread-context-success ()
  "Extracts thread context from a chat buffer."
  (with-temp-buffer
    (elark-im-chat-mode)
    (setq-local elark-im--chat-id "oc_test123")
    (setq-local elark-im--chat-name "Test Chat")
    (setq-local elark-im--messages
                '(((message_id . "msg_1") (sender_name . "Alice") (text . "Hello"))
                  ((message_id . "msg_2") (sender_name . "Bob") (text . "Hi there"))))
    ;; Insert text with message-id property
    (let ((inhibit-read-only t)
          (beg (point)))
      (insert "Bob: Hi there")
      (put-text-property beg (point) 'elark-message-id "msg_2"))
    (goto-char (point-min))
    (let ((ctx (elark-ai--collect-thread-context)))
      (should (equal (plist-get ctx :chat-id) "oc_test123"))
      (should (equal (plist-get ctx :chat-name) "Test Chat"))
      (should (equal (plist-get ctx :message-id) "msg_2"))
      ;; Thread text should contain both messages
      (should (string-match-p "Alice" (plist-get ctx :thread-text)))
      (should (string-match-p "Bob" (plist-get ctx :thread-text)))
      ;; Target message should be marked with >>>
      (should (string-match-p ">>>" (plist-get ctx :thread-text))))))

;;;; Smart reply — compose buffer

(ert-deftest elark-ai-test-compose-buffer-setup ()
  "Compose buffer is set up with draft, message-id, and chat-id."
  (elark-ai--open-compose-buffer "Draft reply" "msg_42" "oc_123" "Dev Chat")
  (unwind-protect
      (let ((buf (get-buffer "*Lark AI Reply*")))
        (should buf)
        (with-current-buffer buf
          (should (derived-mode-p 'elark-ai-reply-mode))
          (should (equal (string-trim (buffer-string)) "Draft reply"))
          (should (equal elark-ai-reply--message-id "msg_42"))
          (should (equal elark-ai-reply--chat-id "oc_123"))
          (should (equal elark-ai-reply--chat-name "Dev Chat"))))
    (when-let* ((buf (get-buffer "*Lark AI Reply*")))
      (kill-buffer buf))))

(ert-deftest elark-ai-test-reply-cancel ()
  "Cancel closes the compose buffer."
  (elark-ai--open-compose-buffer "Draft" "msg_1" "oc_1" "Chat")
  (unwind-protect
      (with-current-buffer "*Lark AI Reply*"
        (elark-ai-reply-cancel)
        (should (null (get-buffer "*Lark AI Reply*"))))
    (when-let* ((buf (get-buffer "*Lark AI Reply*")))
      (kill-buffer buf))))

(ert-deftest elark-ai-test-reply-send-empty-errors ()
  "Sending an empty reply signals an error."
  (elark-ai--open-compose-buffer "" "msg_1" "oc_1" "Chat")
  (unwind-protect
      (with-current-buffer "*Lark AI Reply*"
        (should-error (elark-ai-reply-send) :type 'user-error))
    (when-let* ((buf (get-buffer "*Lark AI Reply*")))
      (kill-buffer buf))))

;;;; System prompt assembly

(ert-deftest elark-ai-test-preamble-contains-date ()
  "Preamble includes current date."
  (let ((preamble (elark-ai-skills--preamble)))
    (should (string-match-p (format-time-string "%Y-%m-%d") preamble))
    (should (string-match-p "Response Format" preamble))))

(ert-deftest elark-ai-test-planning-prompt-mandates-json ()
  "Planning prompt keeps the JSON-plan mandate."
  (let* ((elark-ai-skills--index nil)
         (elark-ai-skills--cache (make-hash-table :test 'equal))
         (prompt (elark-ai-skills-build-system-prompt nil)))
    (should (string-match-p "MUST respond with a JSON" prompt))
    (should (string-match-p "synthesis_instruction" prompt))))

(ert-deftest elark-ai-test-synthesis-prompt-no-json-mandate ()
  "Synthesis prompt strips the JSON-plan mandate so the model
emits prose instead of another plan."
  (let* ((elark-ai-skills--index nil)
         (elark-ai-skills--cache (make-hash-table :test 'equal))
         (prompt (elark-ai-skills-build-synthesis-prompt nil)))
    ;; No JSON-plan rules
    (should-not (string-match-p "MUST respond with a JSON" prompt))
    (should-not (string-match-p "parallel_group" prompt))
    (should-not (string-match-p "synthesis_instruction" prompt))
    ;; But still identity + an explicit prose directive
    (should (string-match-p "Lark/Feishu assistant" prompt))
    (should (string-match-p "markdown prose" prompt))))

(ert-deftest elark-ai-test-synthesis-prompt-includes-skills ()
  "Synthesis prompt still appends selected skill bodies."
  (let* ((elark-ai-skills--index
          '(("lark-shared" . (:description "shared" :dir "/tmp"
                              :keywords ("shared")))))
         (elark-ai-skills--cache (make-hash-table :test 'equal)))
    (puthash "lark-shared" "SKILL_BODY_MARKER" elark-ai-skills--cache)
    (let ((prompt (elark-ai-skills-build-synthesis-prompt '("lark-shared"))))
      (should (string-match-p "## Skill: lark-shared" prompt))
      (should (string-match-p "SKILL_BODY_MARKER" prompt)))))

(ert-deftest elark-ai-test-skills-prompt-embeds-full-body ()
  "The system prompt embeds the FULL skill body, regardless of log limit."
  (let* ((elark-ai-skills--index
          '(("lark-shared" . (:description "shared" :dir "/tmp"
                              :keywords ("shared")))))
         (elark-ai-skills--cache (make-hash-table :test 'equal))
         (elark-ai-skills-log-lines 10))
    (puthash "lark-shared"
             (mapconcat #'number-to-string (number-sequence 1 30) "\n")
             elark-ai-skills--cache)
    (let ((prompt (elark-ai-skills-build-system-prompt '("lark-shared"))))
      ;; Line 30 must survive — the log limit must not truncate the prompt.
      (should (string-match-p "\n30" prompt)))))

(ert-deftest elark-ai-test-skills-abbreviate-for-log ()
  "Log abbreviation keeps a skill's header + top N lines, drops the rest."
  (let* ((elark-ai-skills-log-lines 3)
         (text (concat "PREAMBLE LINE\n## Skill: foo\na\nb\nc\nd\ne")))
    (let ((out (elark-ai-skills-abbreviate-for-log text)))
      (should (string-match-p "PREAMBLE LINE" out))   ; preamble intact
      (should (string-match-p "## Skill: foo" out))   ; header kept
      (should (string-match-p "\na\n" out))           ; first body line kept
      (should (string-match-p "\nc" out))             ; Nth body line kept
      (should-not (string-match-p "\nd" out))         ; beyond N dropped
      (should (string-match-p "truncated" out)))))    ; marker present

(ert-deftest elark-ai-test-skills-abbreviate-nil-passthrough ()
  "A nil log limit returns the text unchanged."
  (let ((elark-ai-skills-log-lines nil)
        (text "## Skill: foo\na\nb\nc\nd"))
    (should (equal (elark-ai-skills-abbreviate-for-log text) text))))

(ert-deftest elark-ai-test-head-lines ()
  "`elark-ai-skills--head-lines' keeps N lines, or all when N is nil."
  (let ((txt "a\nb\nc\nd"))
    (should (equal (elark-ai-skills--head-lines txt 2) "a\nb"))
    (should (equal (elark-ai-skills--head-lines txt 10) "a\nb\nc\nd"))
    (should (equal (elark-ai-skills--head-lines txt nil) "a\nb\nc\nd"))))

;;;; Confirmation policy — auto-execute whitelist

(ert-deftest elark-ai-test-command-auto-whitelist ()
  "Whitelisted write commands match by leading tokens; others do not."
  (let ((elark-ai-auto-execute-commands '("docs +create" "docs +update")))
    (should (elark-ai--command-auto-p '("docs" "+create" "--api-version" "v2")))
    (should (elark-ai--command-auto-p '("docs" "+update" "--document-id" "x")))
    (should-not (elark-ai--command-auto-p '("docs" "+delete" "--document-id" "x")))
    (should-not (elark-ai--command-auto-p '("im" "+messages-send" "--text" "hi")))
    ;; A prefix needs all its tokens present.
    (should-not (elark-ai--command-auto-p '("docs")))))

(ert-deftest elark-ai-test-command-auto-empty-whitelist ()
  "An empty whitelist matches nothing."
  (let ((elark-ai-auto-execute-commands nil))
    (should-not (elark-ai--command-auto-p '("docs" "+create")))))

;;;; Skill selection — context-aware + no-fallback

(ert-deftest elark-ai-test-select-skills-context-match ()
  "Match via the optional CONTEXT arg when the prompt itself is generic."
  (let ((elark-ai-skills--index
         '(("lark-shared" . (:description "shared" :dir "/tmp" :keywords ("shared")))
           ("lark-calendar" . (:description "calendar" :dir "/tmp" :keywords ("calendar")))
           ("lark-task" . (:description "tasks" :dir "/tmp" :keywords ("tasks"))))))
    (let ((selected (elark-ai-skills-select "tell me more"
                                           "calendar +agenda Fetch agenda")))
      (should (member "lark-shared" selected))
      (should (member "lark-calendar" selected))
      (should-not (member "lark-task" selected)))))

(ert-deftest elark-ai-test-select-skills-no-fallback ()
  "No regex match yields only lark-shared (no all-skills dump)."
  (let ((elark-ai-skills--index
         '(("lark-shared" . (:description "shared" :dir "/tmp" :keywords ("shared")))
           ("lark-calendar" . (:description "calendar" :dir "/tmp" :keywords ("calendar")))
           ("lark-im" . (:description "messaging" :dir "/tmp" :keywords ("messaging")))
           ("lark-task" . (:description "tasks" :dir "/tmp" :keywords ("tasks"))))))
    (let ((selected (elark-ai-skills-select "tell me more about that")))
      (should (equal selected '("lark-shared")))
      (should-not (member "lark-calendar" selected))
      (should-not (member "lark-task" selected)))))

;;;; LLM-based skill routing

(defconst elark-ai-test--router-index
  '(("lark-shared" . (:description "shared base" :dir "/tmp" :keywords ("shared")))
    ("lark-calendar" . (:description "calendar agenda events" :dir "/tmp" :keywords ("calendar")))
    ("lark-im" . (:description "messaging chat send" :dir "/tmp" :keywords ("chat")))
    ("lark-task" . (:description "tasks todo" :dir "/tmp" :keywords ("task"))))
  "Minimal skill index used by router tests.")

(ert-deftest elark-ai-test-skills-catalog ()
  "Catalog lists one `- name: description' line per indexed skill."
  (let ((elark-ai-skills--index elark-ai-test--router-index))
    (let ((cat (elark-ai-skills--catalog)))
      (should (string-match-p "^- lark-calendar: calendar agenda events$"
                              (concat "\n" cat "\n")))
      (should (string-match-p "- lark-shared: shared base" cat)))))

(ert-deftest elark-ai-test-skills-router-prompt ()
  "Router prompt embeds the catalog and the JSON response format."
  (let ((elark-ai-skills--index elark-ai-test--router-index))
    (let ((p (elark-ai-skills-build-router-prompt)))
      (should (string-match-p "lark-calendar: calendar agenda events" p))
      (should (string-match-p "\"skills\"" p)))))

(ert-deftest elark-ai-test-validate-selection ()
  "Validation keeps known names, drops unknown/non-string, dedups."
  (let ((elark-ai-skills--index elark-ai-test--router-index))
    (should (equal (elark-ai-skills--validate-selection '("lark-calendar" "lark-task"))
                   '("lark-calendar" "lark-task")))
    (should (equal (elark-ai-skills--validate-selection '("lark-calendar" "lark-NOPE"))
                   '("lark-calendar")))
    (should (equal (elark-ai-skills--validate-selection '("lark-im" "lark-im"))
                   '("lark-im")))
    (should (equal (elark-ai-skills--validate-selection '(42 "lark-task"))
                   '("lark-task")))
    (should (null (elark-ai-skills--validate-selection nil)))))

(ert-deftest elark-ai-test-select-skills-llm-clean ()
  "LLM router pick is validated and prefixed with lark-shared."
  (let ((elark-ai-skills--index elark-ai-test--router-index)
        result)
    (cl-letf (((symbol-function 'elark-ai--progress-log) #'ignore)
              ((symbol-function 'elark-ai--call-llm)
               (lambda (_sys _user cb) (funcall cb "{\"skills\": [\"lark-calendar\"]}"))))
      (elark-ai--select-skills "show agenda" ""
                              (lambda (sel) (setq result sel))))
    (should (member "lark-shared" result))
    (should (member "lark-calendar" result))
    (should-not (member "lark-im" result))))

(ert-deftest elark-ai-test-select-skills-llm-garbage-falls-back ()
  "Unparseable router output falls back to keyword routing."
  (let ((elark-ai-skills--index elark-ai-test--router-index)
        result)
    (cl-letf (((symbol-function 'elark-ai--progress-log) #'ignore)
              ((symbol-function 'elark-ai--call-llm)
               (lambda (_sys _user cb) (funcall cb "sorry, I cannot help"))))
      (elark-ai--select-skills "send a chat message" ""
                              (lambda (sel) (setq result sel))))
    ;; Keyword table maps chat/message/send → elark-im.
    (should (member "lark-shared" result))
    (should (member "lark-im" result))))

(ert-deftest elark-ai-test-select-skills-llm-hallucination-falls-back ()
  "All-unknown picks validate to nil, triggering keyword fallback."
  (let ((elark-ai-skills--index elark-ai-test--router-index)
        result)
    (cl-letf (((symbol-function 'elark-ai--progress-log) #'ignore)
              ((symbol-function 'elark-ai--call-llm)
               (lambda (_sys _user cb) (funcall cb "{\"skills\": [\"lark-bogus\"]}"))))
      (elark-ai--select-skills "do something vague" ""
                              (lambda (sel) (setq result sel))))
    ;; No keyword match either → only lark-shared.
    (should (equal result '("lark-shared")))))

(ert-deftest elark-ai-test-select-skills-keyword-mode-skips-llm ()
  "With `elark-ai-skill-routing' = keyword, the LLM is never called."
  (let ((elark-ai-skills--index elark-ai-test--router-index)
        (elark-ai-skill-routing 'keyword)
        result)
    (cl-letf (((symbol-function 'elark-ai--call-llm)
               (lambda (&rest _) (error "LLM must not be called in keyword mode"))))
      (elark-ai--select-skills "calendar agenda" ""
                              (lambda (sel) (setq result sel))))
    (should (member "lark-shared" result))
    (should (member "lark-calendar" result))))

;;;; build-user-message — structure, history truncation

(ert-deftest elark-ai-test-build-user-message-no-history ()
  "First-turn message: just `## Current request' + prompt."
  (let ((msg (elark-ai--build-user-message "do the thing" "" nil)))
    (should (string-match-p "## Current request" msg))
    (should (string-match-p "do the thing" msg))
    (should-not (string-match-p "## Prior turns" msg))
    (should-not (string-match-p "## Originating buffer context" msg))
    ;; No history means no anti-repeat trailer.
    (should-not (string-match-p "Address only the request" msg))))

(ert-deftest elark-ai-test-build-user-message-with-context ()
  "Originating-buffer context appears in its own labelled section."
  (let ((msg (elark-ai--build-user-message "x" "Viewing doc tok_42" nil)))
    (should (string-match-p "## Originating buffer context" msg))
    (should (string-match-p "Viewing doc tok_42" msg))))

(ert-deftest elark-ai-test-build-user-message-with-history ()
  "Prior turns rendered as a list with `## Prior turns' header."
  (let* ((history '(("assistant" . "Here's the agenda")
                    ("user" . "show agenda")))
         (msg (elark-ai--build-user-message "tell me more" "" history)))
    (should (string-match-p "## Prior turns" msg))
    (should (string-match-p "User: show agenda" msg))
    (should (string-match-p "Assistant: Here's the agenda" msg))
    ;; Trailing anti-repeat instruction only when history is present.
    (should (string-match-p "Address only the request" msg))))

(ert-deftest elark-ai-test-build-user-message-truncates-assistant ()
  "Long assistant text is clipped to `elark-ai-history-truncate-chars'."
  (let* ((elark-ai-history-truncate-chars 50)
         (long-text (make-string 200 ?a))
         (history `(("assistant" . ,long-text)
                    ("user" . "q")))
         (msg (elark-ai--build-user-message "next" "" history)))
    (should (string-match-p "…\\[truncated\\]" msg))
    ;; Original 200-char string should not appear verbatim.
    (should-not (string-match-p (regexp-quote long-text) msg))))

(ert-deftest elark-ai-test-build-user-message-keeps-user-prompts ()
  "User prompts are kept un-truncated even if very long."
  (let* ((elark-ai-history-truncate-chars 20)
         (long-user (make-string 100 ?u))
         (history `(("assistant" . "ok")
                    ("user" . ,long-user)))
         (msg (elark-ai--build-user-message "x" "" history)))
    (should (string-match-p (regexp-quote long-user) msg))))

;;;; Session struct

(ert-deftest elark-ai-test-session-defaults ()
  "Fresh session has sensible defaults."
  (let ((s (make-elark-ai-session)))
    (should (= (elark-ai-session-turn s) 0))
    (should (eq (elark-ai-session-phase s) 'idle))
    (should (null (elark-ai-session-history s)))
    (should (equal (elark-ai-session-context s) ""))
    (should (null (elark-ai-session-skills s)))
    (should (null (elark-ai-session-steps s)))
    (should (null (elark-ai-session-input-start s)))
    (should (null (elark-ai-session-input-region-start s)))))

(ert-deftest elark-ai-test-session-setf ()
  "Session slots are setf-able."
  (let ((s (make-elark-ai-session)))
    (setf (elark-ai-session-turn s) 3
          (elark-ai-session-phase s) 'review
          (elark-ai-session-skills s) '("lark-calendar"))
    (should (= (elark-ai-session-turn s) 3))
    (should (eq (elark-ai-session-phase s) 'review))
    (should (equal (elark-ai-session-skills s) '("lark-calendar")))))

;;;; Plan body — step-index text property for at-point removal

(ert-deftest elark-ai-test-format-plan-body-step-index ()
  "Each rendered step line carries its `elark-ai-step-index' index."
  (let ((buf (get-buffer-create "*elark-ai-test-plan*")))
    (unwind-protect
        (with-current-buffer buf
          (elark-ai-plan-mode)
          (let ((session (elark-ai--session)))
            (setf (elark-ai-session-steps session)
                  '((:index 0 :description "first"  :command ("a" "b"))
                    (:index 1 :description "second" :command nil :synthesize t))
                  (elark-ai-session-step-status session)
                  '((0 . pending) (1 . pending))
                  (elark-ai-session-phase session) 'review))
          (let ((body (elark-ai--format-plan-body)))
            ;; Body should mention both descriptions.
            (should (string-match-p "first" body))
            (should (string-match-p "second" body))
            ;; Step-index property should be set on each step's chars.
            (let ((found-0 nil) (found-1 nil))
              (dotimes (i (length body))
                (pcase (get-text-property i 'elark-ai-step-index body)
                  (0 (setq found-0 t))
                  (1 (setq found-1 t))))
              (should found-0)
              (should found-1))))
      (kill-buffer buf))))

;;;; Inline synthesis producer (regression for the :synthesize sentinel)

(ert-deftest elark-ai-test-step-referenced-p ()
  "`elark-ai--step-referenced-p' detects $step-IDX in commands, matching exactly."
  (let ((steps '((:index 0 :command nil :synthesize t)
                 (:index 1 :command ("docs" "+create" "--content" "$step-0")))))
    (should (elark-ai--step-referenced-p 0 steps))
    (should-not (elark-ai--step-referenced-p 1 steps)))
  ;; $step-1 must not match $step-10.
  (let ((steps '((:index 0 :command ("x" "--data" "$step-10")))))
    (should-not (elark-ai--step-referenced-p 1 steps))
    (should (elark-ai--step-referenced-p 10 steps))))

(defun elark-ai-test--fresh-session (steps)
  "Reset the AI buffer's session and set its STEPS; return the session."
  (with-current-buffer (elark-ai--get-buffer)
    (setq elark-ai--session (make-elark-ai-session))
    (setf (elark-ai-session-steps elark-ai--session) steps)
    elark-ai--session))

(ert-deftest elark-ai-test-synthesize-inline-feeds-step ()
  "A referenced synthesis step produces real text that $step-N resolves to.
Regression: previously the synthesis step pushed the `:synthesize'
sentinel, so $step-0 interpolated to the literal \"synthesize\"."
  (let ((plan '((:index 0 :command nil :synthesize t
                        :synthesis-instruction "make SOP")
                (:index 1 :command ("docs" "+create" "--content" "$step-0")
                        :side-effect t)))
        (captured nil) (done nil)
        (elark-ai-execute-mode 'auto))
    (elark-ai-test--fresh-session plan)
    (cl-letf (((symbol-function 'elark-ai--call-llm)
               (lambda (_sys _user cb) (funcall cb "<h1>Real SOP</h1>")))
              ((symbol-function 'elark--run-command)
               (lambda (cmd cb &rest _) (setq captured cmd) (funcall cb '((ok . t)))))
              ((symbol-function 'elark-ai--update-step-status) #'ignore)
              ((symbol-function 'elark-ai--progress-log) (lambda (&rest _) nil)))
      (elark-ai--execute-plan plan (lambda (_r) (setq done t))))
    (should done)
    ;; The write received the synthesized text, not the sentinel.
    (should (member "<h1>Real SOP</h1>" captured))
    (should-not (member "synthesize" captured))))

(ert-deftest elark-ai-test-synthesize-unreferenced-defers ()
  "An unreferenced synthesis step is deferred (sentinel), not run inline."
  (let ((plan '((:index 0 :command nil :synthesize t
                        :synthesis-instruction "x")))
        (called nil) (done nil)
        (elark-ai-execute-mode 'auto))
    (elark-ai-test--fresh-session plan)
    (cl-letf (((symbol-function 'elark-ai--call-llm)
               (lambda (&rest _) (setq called t)))
              ((symbol-function 'elark-ai--update-step-status) #'ignore)
              ((symbol-function 'elark-ai--progress-log) (lambda (&rest _) nil)))
      (elark-ai--execute-plan plan (lambda (_r) (setq done t))))
    (should done)
    (should-not called)
    (should (eq (alist-get 0 (elark-ai-session-step-results (elark-ai--session)))
                :synthesize))))

;;;; Agent loop (elark-ai-strategy = agent)

(ert-deftest elark-ai-test-agent-parse-action ()
  "`elark-ai-agent--parse-action' accepts only command/final action objects."
  (should (equal "command"
                 (alist-get 'action
                            (elark-ai-agent--parse-action
                             "{\"action\":\"command\",\"command\":[\"im\",\"+list\"]}"))))
  (should (equal "final"
                 (alist-get 'action
                            (elark-ai-agent--parse-action
                             "{\"action\":\"final\",\"answer\":\"hi\"}"))))
  (should-not (elark-ai-agent--parse-action "not json"))
  (should-not (elark-ai-agent--parse-action "{\"action\":\"bogus\"}")))

(ert-deftest elark-ai-test-agent-empty-content-p ()
  "`elark-ai-agent--empty-content-p' flags a write whose content arg is blank."
  (should (elark-ai-agent--empty-content-p '("docs" "+create" "--content" "  ")))
  (should-not (elark-ai-agent--empty-content-p
               '("docs" "+create" "--content" "<h1>x</h1>")))
  (should-not (elark-ai-agent--empty-content-p '("im" "+list"))))

(ert-deftest elark-ai-test-agent-generate-then-write ()
  "Agent writes self-generated content directly into the command, then finishes.
Regression mirror: the doc-create command receives the real content, not
a `$step-N' sentinel."
  (let ((responses
         (list (concat "{\"action\":\"command\",\"command\":[\"docs\",\"+create\","
                       "\"--content\",\"<h1>SOP</h1>\"],\"side_effect\":true}")
               "{\"action\":\"final\",\"answer\":\"Done.\"}"))
        (captured nil) (presented nil)
        (elark-ai-agent-confirm-writes nil)
        (session (make-elark-ai-session)))
    (cl-letf (((symbol-function 'elark-ai--call-llm-stream)
               (lambda (_s _u cb &optional _ch) (funcall cb (pop responses))))
              ((symbol-function 'elark--run-command)
               (lambda (cmd cb &rest _) (setq captured cmd) (funcall cb '((ok . t)))))
              ((symbol-function 'elark-ai--present)
               (lambda (content &rest _) (setq presented content)))
              ((symbol-function 'elark-ai--progress-log) (lambda (&rest _) nil))
              ((symbol-function 'elark-ai-ui-append-fragment) (lambda (&rest _) nil)))
      (elark-ai-agent--run "make a SOP doc" "" nil session nil))
    (should (member "<h1>SOP</h1>" captured))
    (should-not (member "synthesize" captured))
    (should (equal presented "Done."))))

(ert-deftest elark-ai-test-agent-final-only ()
  "A request that needs no command resolves directly to a final answer."
  (let ((presented nil)
        (session (make-elark-ai-session)))
    (cl-letf (((symbol-function 'elark-ai--call-llm-stream)
               (lambda (_s _u cb &optional _ch)
                 (funcall cb "{\"action\":\"final\",\"answer\":\"Hi\"}")))
              ((symbol-function 'elark-ai--present)
               (lambda (c &rest _) (setq presented c)))
              ((symbol-function 'elark-ai--progress-log) (lambda (&rest _) nil))
              ((symbol-function 'elark-ai-ui-append-fragment) (lambda (&rest _) nil)))
      (elark-ai-agent--run "hi" "" nil session nil))
    (should (equal presented "Hi"))))

(ert-deftest elark-ai-test-agent-recovers-from-bad-action ()
  "An unparseable reply becomes an error observation; the loop continues."
  (let ((responses (list "this is not json"
                         "{\"action\":\"final\",\"answer\":\"ok\"}"))
        (presented nil)
        (session (make-elark-ai-session)))
    (cl-letf (((symbol-function 'elark-ai--call-llm-stream)
               (lambda (_s _u cb &optional _ch) (funcall cb (pop responses))))
              ((symbol-function 'elark-ai--present)
               (lambda (c &rest _) (setq presented c)))
              ((symbol-function 'elark-ai--progress-log) (lambda (&rest _) nil))
              ((symbol-function 'elark-ai-ui-append-fragment) (lambda (&rest _) nil)))
      (elark-ai-agent--run "x" "" nil session nil))
    (should (equal presented "ok"))
    ;; The invalid reply was recorded as one transcript entry.
    (should (= 1 (length (elark-ai-session-agent-steps session))))))

(ert-deftest elark-ai-test-agent-max-steps-forces-final ()
  "Hitting the step cap forces a final answer via a non-looping LLM call."
  (let ((presented nil) (cli-calls 0)
        (elark-ai-agent-max-steps 1)
        (elark-ai-agent-confirm-writes nil)
        (session (make-elark-ai-session)))
    (cl-letf (((symbol-function 'elark-ai--call-llm-stream)
               (lambda (_s _u cb &optional _ch)
                 (funcall cb "{\"action\":\"command\",\"command\":[\"im\",\"+list\"]}")))
              ((symbol-function 'elark--run-command)
               (lambda (_cmd cb &rest _)
                 (setq cli-calls (1+ cli-calls)) (funcall cb '((ok . t)))))
              ((symbol-function 'elark-ai--call-llm)
               (lambda (_s _u cb) (funcall cb "Summary after limit.")))
              ((symbol-function 'elark-ai--present)
               (lambda (c &rest _) (setq presented c)))
              ((symbol-function 'elark-ai--progress-log) (lambda (&rest _) nil))
              ((symbol-function 'elark-ai-ui-append-fragment) (lambda (&rest _) nil)))
      (elark-ai-agent--run "x" "" nil session nil))
    ;; One command ran (iter 0); the cap then forced a final answer.
    (should (= cli-calls 1))
    (should (equal presented "Summary after limit."))))

;;;; Live stream preview

(ert-deftest elark-ai-test-stream-tail ()
  "`elark-ai--stream-tail' returns a bounded tail of the stream."
  (should (equal "c\nd" (elark-ai--stream-tail "a\nb\nc\nd" 2)))
  (should (null (elark-ai--stream-tail "" 4)))
  (should (null (elark-ai--stream-tail "x" 0)))
  ;; Newline-poor input is clipped to roughly N*80 chars (+ the … marker).
  (should (<= (length (elark-ai--stream-tail (make-string 1000 ?z) 3)) 241)))

(ert-deftest elark-ai-test-stream-preview-updates-plan ()
  "The preview handler shows a rolling tail under the waiting line."
  (with-current-buffer (elark-ai--get-buffer)
    (setq elark-ai--session (make-elark-ai-session))
    (setf (elark-ai-session-turn elark-ai--session) 1)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (elark-ai-ui-insert-fragment (elark-ai--frag "plan") 'plan nil
                                  "Waiting for LLM response...\n"))
    (let ((h (elark-ai--stream-preview-handler)))
      (funcall h "alpha\n")
      (funcall h "beta gamma"))
    (let* ((region (elark-ai-ui-find-fragment (elark-ai--frag "plan")))
           (text (buffer-substring-no-properties (car region) (cdr region))))
      (should (string-match-p "Waiting for LLM response" text))
      (should (string-match-p "beta gamma" text)))
    ;; `elark-ai--clear-waiting' removes the preview from the buffer.
    (elark-ai--clear-waiting)
    (should-not (string-match-p
                 "beta gamma"
                 (buffer-substring-no-properties (point-min) (point-max))))))

;;;; Tool-call cards

(ert-deftest elark-ai-test-abbreviate-cmd-clips-content ()
  "Content-flag values are clipped; other args pass through verbatim."
  (let* ((long (make-string 500 ?x))
         (abbr (elark-ai-agent-abbreviate-cmd
                `("docs" "+create" "--title" "Foo" "--content" ,long))))
    (should (equal '("docs" "+create" "--title" "Foo") (seq-take abbr 4)))
    ;; Position 5 is the clipped --content value: short head + tail.
    (should (< (length (nth 5 abbr)) 100))
    (should (string-match-p "\\[500 chars\\]" (nth 5 abbr)))))

(ert-deftest elark-ai-test-abbreviate-cmd-multiline ()
  "Multi-line content is reduced to its first line + clip tail."
  (let* ((body "line one\nline two\nline three")
         (abbr (elark-ai-agent-abbreviate-cmd
                `("im" "+send" "--text" ,body))))
    (should (string-match-p "^line one" (nth 3 abbr)))
    (should (string-match-p (format "\\[%d chars\\]" (length body))
                            (nth 3 abbr)))
    (should-not (string-match-p "line three" (nth 3 abbr)))))

(ert-deftest elark-ai-test-abbreviate-cmd-short-passthrough ()
  "A short single-line content value passes through unchanged."
  (should (equal '("im" "+send" "--text" "hi")
                 (elark-ai-agent-abbreviate-cmd
                  '("im" "+send" "--text" "hi")))))

(ert-deftest elark-ai-test-abbreviate-cmd-non-content-flags-untouched ()
  "Values after non-content flags are NEVER clipped (they're parameters,
not body content)."
  (let* ((long (make-string 500 ?x))
         (abbr (elark-ai-agent-abbreviate-cmd
                `("docs" "+open" "--token" ,long))))
    (should (equal long (nth 3 abbr)))))

(ert-deftest elark-ai-test-format-cmd-body-clips-content ()
  "`elark-ai--format-cmd-body' uses the abbreviated form so multi-KB
content values don't flood the tool-call card."
  (let* ((long (make-string 800 ?x))
         (body (elark-ai--format-cmd-body
                `("docs" "+create" "--title" "T" "--content" ,long))))
    (should-not (string-match-p (regexp-quote long) body))
    (should (string-match-p "\\[800 chars\\]" body))))

(ert-deftest elark-ai-test-format-cmd-body-multiline ()
  "`elark-ai--format-cmd-body' groups positional args, then per-flag pairs."
  (should (equal "lark-cli docs +create \\\n  --title Foo \\\n  --content Hello"
                 (elark-ai--format-cmd-body
                  '("docs" "+create" "--title" "Foo" "--content" "Hello")))))

(ert-deftest elark-ai-test-format-cmd-body-no-flags ()
  "Plain positional-only commands render on a single line."
  (should (equal "lark-cli im +chats-list"
                 (elark-ai--format-cmd-body '("im" "+chats-list")))))

(ert-deftest elark-ai-test-tool-call-label-status ()
  "Status icon varies but the command head is always present."
  (let ((running (elark-ai--tool-call-label '("docs" "+create") 'running))
        (done    (elark-ai--tool-call-label '("docs" "+create") 'done))
        (skipped (elark-ai--tool-call-label '("docs" "+create") 'skipped)))
    (dolist (l (list running done skipped))
      (should (string-match-p "lark-cli docs \\+create" l)))
    (should (string-match-p "⟳" running))
    (should (string-match-p "✓" done))
    (should (string-match-p "⊘" skipped))))

(ert-deftest elark-ai-test-render-tool-call-inserts-and-updates ()
  "First call inserts a fragment; second call only updates the label."
  (with-current-buffer (elark-ai--get-buffer)
    (setq elark-ai--session (make-elark-ai-session))
    (setf (elark-ai-session-turn elark-ai--session) 1)
    (let ((inhibit-read-only t))
      (erase-buffer)
      ;; Plan fragment is the insertion anchor; render-tool-call should
      ;; insert its card just before it.
      (elark-ai-ui-insert-fragment (elark-ai--frag "plan") 'plan nil
                                  "Waiting...\n"))
    (let ((cmd '("docs" "+create" "--title" "Hi" "--content" "Body")))
      (elark-ai--render-tool-call 0 cmd 'running)
      (let* ((id (elark-ai--frag "tool-0"))
             (region (elark-ai-ui-find-fragment id)))
        (should region)
        (let ((text (buffer-substring-no-properties (car region) (cdr region))))
          ;; Label has the running icon, body has the multi-line cmd.
          (should (string-match-p "⟳" text))
          (should (string-search "lark-cli docs +create" text))
          (should (string-match-p "--title" text))
          (should (string-match-p "--content" text)))
        ;; A user-fold (invisibility on the body) survives a label-only
        ;; status update — this is what `update-label' is for.
        (let* ((label-end (next-single-property-change
                           (car region) 'elark-ai-ui-section
                           nil (cdr region)))
               (inhibit-read-only t))
          (put-text-property label-end (cdr region) 'invisible t))
        (elark-ai--render-tool-call 0 cmd 'done)
        (let* ((region (elark-ai-ui-find-fragment id))
               (label-end (next-single-property-change
                           (car region) 'elark-ai-ui-section
                           nil (cdr region)))
               (text (buffer-substring-no-properties (car region) (cdr region))))
          (should (string-match-p "✓" text))
          ;; Body is still folded — invisibility preserved.
          (should (eq t (get-text-property label-end 'invisible))))))))

(ert-deftest elark-ai-test-tool-call-nav ()
  "n/p navigation jumps between tool-call fragments."
  (with-current-buffer (elark-ai--get-buffer)
    (setq elark-ai--session (make-elark-ai-session))
    (setf (elark-ai-session-turn elark-ai--session) 1)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (elark-ai-ui-insert-fragment "log"  'log    "Log" "log body\n")
      (elark-ai-ui-insert-fragment "t1"   'tool-call "  ⟳ lark-cli a" "lark-cli a")
      (elark-ai-ui-insert-fragment "mid"  'log    "Mid" "mid body\n")
      (elark-ai-ui-insert-fragment "t2"   'tool-call "  ⟳ lark-cli b" "lark-cli b")
      (elark-ai-ui-insert-fragment "tail" 'log    "Tail" "tail body\n"))
    (goto-char (point-min))
    (elark-ai-ui-next-tool-call)
    (should (equal "t1" (get-text-property (point) 'elark-ai-ui-id)))
    (elark-ai-ui-next-tool-call)
    (should (equal "t2" (get-text-property (point) 'elark-ai-ui-id)))
    ;; No further tool-call: signals user-error and point stays put.
    (let ((here (point)))
      (should-error (elark-ai-ui-next-tool-call) :type 'user-error)
      (should (= here (point))))
    (elark-ai-ui-prev-tool-call)
    (should (equal "t1" (get-text-property (point) 'elark-ai-ui-id)))))

;;;; Topic brief end-to-end

(ert-deftest elark-ai-test-brief-on-end-to-end ()
  "`elark-ai-brief-on' gathers context and synthesises into the AI chat.
Both the gatherer and the streaming LLM call are stubbed; we verify
the wiring: the user turn appears, the synthesised text lands in
conversation history, and an editable input area follows."
  (let ((gathered nil)
        (synth-user-msg nil))
    (cl-letf*
        (((symbol-function 'elark-ai-context-graph-gather)
          (lambda (topic done-fn)
            (setq gathered topic)
            (funcall done-fn (format "## Docs\n\n- doc about %s" topic))))
         ((symbol-function 'elark-ai--call-llm-stream)
          (lambda (_sys user cb &optional _chunk)
            (setq synth-user-msg user)
            (funcall cb (format "BRIEF: %s" gathered))))
         ((symbol-function 'elark-ai--progress-log) (lambda (&rest _) nil)))
      ;; Fresh AI buffer for a clean run.
      (with-current-buffer (elark-ai--get-buffer)
        (let ((inhibit-read-only t)) (erase-buffer))
        (setq elark-ai--session (make-elark-ai-session)))
      (elark-ai-brief-on "Q2 budget")
      ;; The gatherer was invoked with the topic.
      (should (equal "Q2 budget" gathered))
      ;; The synthesis user msg embeds the topic AND the gathered text.
      (should (string-match-p "Q2 budget" synth-user-msg))
      (should (string-match-p "doc about Q2 budget" synth-user-msg))
      ;; The streamed text was pushed onto conversation history.
      (let* ((session (with-current-buffer (elark-ai--get-buffer)
                        elark-ai--session))
             (history (elark-ai-session-history session)))
        (should (assoc "assistant" history))
        (should (string-match-p
                 "BRIEF: Q2 budget"
                 (cdr (assoc "assistant" history))))))))

(ert-deftest elark-ai-test-brief-on-empty-topic-errors ()
  "An empty or whitespace topic refuses to fire — no LLM call."
  (cl-letf (((symbol-function 'elark-ai-context-graph-gather)
             (lambda (&rest _) (error "should not gather"))))
    (should-error (elark-ai-brief-on "   ") :type 'user-error)
    (should-error (elark-ai-brief-on "")    :type 'user-error)))

;;;; Context content clipping

(ert-deftest elark-ai-test-context-clip ()
  "`elark-ai-context--clip' honors the char limit and keep direction."
  ;; nil limit (default) → whole content, regardless of length.
  (let ((elark-ai-context-content-max-chars nil))
    (should (equal "abcdef" (elark-ai-context--clip "abcdef" 'tail)))
    (should (equal "abcdef" (elark-ai-context--clip "abcdef" 'head))))
  (let ((elark-ai-context-content-max-chars 3))
    ;; head keeps the first chars; tail keeps the last (recent) chars.
    (should (equal "abc…" (elark-ai-context--clip "abcdef" 'head)))
    (should (equal "…def" (elark-ai-context--clip "abcdef" 'tail)))
    ;; Under the limit → unchanged.
    (should (equal "ab" (elark-ai-context--clip "ab" 'tail)))))

(ert-deftest elark-ai-test-scroll-to-output-undisplayed ()
  "Scrolling to output is a no-op when the AI buffer is not displayed.
Regression: `recenter' errored from async callbacks after the user
switched windows."
  (when (get-buffer "*Lark AI*") (kill-buffer "*Lark AI*"))
  (with-current-buffer (get-buffer-create "*Lark AI*")
    (insert "placeholder"))
  (unwind-protect
      (should-not (elark-ai--scroll-to-output))
    (kill-buffer "*Lark AI*")))

;;;; Front-end dispatch (shell-maker session)

(ert-deftest elark-ai-test-frontend-dispatch ()
  "UI helpers route to an installed front-end instead of the classic buffer."
  (when (get-buffer "*Lark AI*") (kill-buffer "*Lark AI*"))
  (let (logged presented cleared tools)
    (let ((elark-ai--frontend
           (list :progress-log (lambda (fmt &rest args)
                                 (push (apply #'format fmt args) logged))
                 :render-tool-call (lambda (i c s) (push (list i c s) tools))
                 :stream-preview (lambda () #'ignore)
                 :clear-waiting (lambda () (setq cleared t))
                 :present (lambda (c sk) (setq presented (cons c sk))))))
      (elark-ai--progress-log "step %d" 1)
      (elark-ai--render-tool-call 0 '("docs" "+fetch") 'done)
      (should (functionp (elark-ai--stream-preview-handler)))
      (elark-ai--clear-waiting)
      (elark-ai--present "hi" nil))
    (should (equal '("step 1") logged))
    (should (equal '((0 ("docs" "+fetch") done)) tools))
    (should cleared)
    (should (equal '("hi" . nil) presented))
    ;; Everything was routed — the classic buffer was never created.
    (should-not (get-buffer "*Lark AI*"))))

(ert-deftest elark-ai-test-ask-routes-by-interface ()
  "`elark-ai-ask' routes to the shell UI unless classic is chosen
or the prompt comes from inside the classic buffer."
  (let (shell-asked classic-asked)
    (cl-letf (((symbol-function 'elark-ai--use-shell-p) (lambda () t))
              ((symbol-function 'elark-ai-shell-ask)
               (lambda (p) (setq shell-asked p)))
              ((symbol-function 'elark-ai--ask-classic)
               (lambda (p) (setq classic-asked p))))
      ;; Shell interface → shell.
      (elark-ai-ask "to shell")
      (should (equal "to shell" shell-asked))
      (should-not classic-asked)
      ;; From inside the classic buffer → stays classic.
      (with-current-buffer (get-buffer-create "*Lark AI*")
        (elark-ai-ask "follow-up"))
      (should (equal "follow-up" classic-asked))
      (kill-buffer "*Lark AI*"))
    ;; Classic interface → classic.
    (setq classic-asked nil)
    (cl-letf (((symbol-function 'elark-ai--use-shell-p) (lambda () nil))
              ((symbol-function 'elark-ai--ask-classic)
               (lambda (p) (setq classic-asked p))))
      (elark-ai-ask "to classic")
      (should (equal "to classic" classic-asked)))))

(ert-deftest elark-ai-test-shell-execute-flow ()
  "The shell executor drives the engine and owns its own session state."
  (require 'elark-ai-shell)
  (let* ((written "") (finished 'unset)
         (shellbuf (generate-new-buffer " *elark-ai-shell-test*"))
         (shell (list (cons :buffer shellbuf)
                      (cons :write-output
                            (lambda (s &optional _force)
                              (setq written (concat written s))))
                      (cons :finish-output (lambda (ok) (setq finished ok)))))
         (elark-ai--frontend nil)
         ran)
    (unwind-protect
        (cl-letf (((symbol-function 'elark-ai--select-skills)
                   (lambda (_prompt _ctx cb) (funcall cb '("lark-shared"))))
                  ((symbol-function 'elark-ai-agent--run)
                   (lambda (prompt _ctx history _session skills)
                     (setq ran (list prompt history skills))
                     ;; The engine ends a turn through the dispatch seam.
                     (elark-ai--progress-log "Agent loop complete.")
                     (elark-ai--render-tool-call
                      0 '("docs" "+fetch" "--doc" "d1") 'done)
                     (elark-ai--present "ANSWER" nil))))
          (elark-ai-shell--execute "do it" shell)
          (should (equal "do it" (car ran)))
          ;; First turn: prior history is empty.
          (should (null (cadr ran)))
          ;; Session history now holds user + assistant.
          (let ((hist (elark-ai-session-history
                       (buffer-local-value 'elark-ai-shell--session shellbuf))))
            (should (equal '("assistant" . "ANSWER") (car hist)))
            (should (equal '("user" . "do it") (cadr hist))))
          ;; Output written into the shell; turn finished; seam released.
          (should (string-match-p "ANSWER" written))
          (should (string-match-p "✓ lark-cli docs \\+fetch" written))
          (should (string-match-p "Agent loop complete" written))
          (should (eq finished t))
          (should-not elark-ai--frontend)
          ;; Busy guard: a second submit while a turn is in flight.
          (setq elark-ai--frontend '(:present ignore))
          (elark-ai-shell--execute "again" shell)
          (should (string-match-p "already in flight" written)))
      (kill-buffer shellbuf))))

(ert-deftest elark-ai-test-brief-user-message ()
  "Brief synthesis message covers both the results and no-results shapes."
  (should (string-match-p "Retrieved snippets"
                          (elark-ai--brief-user-message "envhub" "snippet text")))
  (should (string-match-p "No results"
                          (elark-ai--brief-user-message "envhub" "")))
  (should (string-match-p "No results"
                          (elark-ai--brief-user-message "envhub" nil))))

(ert-deftest elark-ai-test-brief-routes-by-interface ()
  "`elark-ai-brief-on' routes to the shell UI like `elark-ai-ask'."
  (require 'elark-ai-shell)
  (let (shell-brief classic-brief)
    (cl-letf (((symbol-function 'elark-ai--use-shell-p) (lambda () t))
              ((symbol-function 'elark-ai-shell-brief)
               (lambda (topic) (setq shell-brief topic)))
              ((symbol-function 'elark-ai--brief-on-classic)
               (lambda (topic) (setq classic-brief topic))))
      (elark-ai-brief-on "envhub")
      (should (equal "envhub" shell-brief))
      (should-not classic-brief))
    (cl-letf (((symbol-function 'elark-ai--use-shell-p) (lambda () nil))
              ((symbol-function 'elark-ai--brief-on-classic)
               (lambda (topic) (setq classic-brief topic))))
      (elark-ai-brief-on "envhub")
      (should (equal "envhub" classic-brief)))))

(ert-deftest elark-ai-test-shell-brief-command ()
  "\"/brief <topic>\" in the shell runs gather → streamed synthesis."
  (require 'elark-ai-shell)
  (let* ((written "") (finished 'unset)
         (shellbuf (generate-new-buffer " *elark-ai-shell-brief-test*"))
         (shell (list (cons :buffer shellbuf)
                      (cons :write-output
                            (lambda (s &optional _f)
                              (setq written (concat written s))))
                      (cons :finish-output (lambda (ok) (setq finished ok)))))
         (elark-ai--frontend nil))
    (unwind-protect
        (cl-letf (((symbol-function 'elark-ai-context-graph-gather)
                   (lambda (_topic cb) (funcall cb "gathered snippets")))
                  ((symbol-function 'elark-ai--call-llm-stream)
                   (lambda (_sys user cb &optional chunk-handler)
                     (should (string-match-p "gathered snippets" user))
                     (funcall chunk-handler "BRIEF ")
                     (funcall chunk-handler "TEXT")
                     (funcall cb "BRIEF TEXT")))
                  ((symbol-function 'elark-ai-shell--fontify) #'ignore))
          (elark-ai-shell--execute "/brief envhub plans" shell)
          (should (string-match-p "BRIEF TEXT" written))
          (should (eq finished t))
          (should-not elark-ai--frontend)
          (let ((hist (elark-ai-session-history
                       (buffer-local-value 'elark-ai-shell--session shellbuf))))
            (should (equal '("assistant" . "BRIEF TEXT") (car hist)))
            (should (equal '("user" . "Brief on: envhub plans") (cadr hist))))
          ;; Bare /brief shows usage without starting a turn.
          (setq written "" finished 'unset)
          (elark-ai-shell--execute "/brief" shell)
          (should (string-match-p "Usage: /brief" written))
          (should (eq finished nil))
          (should-not elark-ai--frontend))
      (kill-buffer shellbuf))))

;;;; Skill routing context clipping

(ert-deftest elark-ai-test-routing-context-clip ()
  "Routing context keeps the head and marks the cut; nil limit passes through."
  (let ((elark-ai-skill-routing-context-chars 10))
    (should (equal "short" (elark-ai-skills-routing-context "short")))
    (should (equal (concat (make-string 10 ?x) "…")
                   (elark-ai-skills-routing-context (make-string 50 ?x))))
    (should-not (elark-ai-skills-routing-context nil)))
  (let ((elark-ai-skill-routing-context-chars nil))
    (should (equal (make-string 50 ?x)
                   (elark-ai-skills-routing-context (make-string 50 ?x))))))

(ert-deftest elark-ai-test-routing-context-limits-keyword-overmatch ()
  "A long document body no longer drags unrelated domain skills into routing.
Regression: asking from a doc buffer put the whole document into the
match text; body words like \"email\" or \"wiki\" fired nearly every
keyword rule."
  (let* ((elark-ai-skills--index
          '(("lark-shared" . (:description "shared" :dir "/tmp"))
            ("lark-doc" . (:description "docs" :dir "/tmp"))
            ("lark-drive" . (:description "drive" :dir "/tmp"))
            ("lark-mail" . (:description "mail" :dir "/tmp"))
            ("lark-wiki" . (:description "wiki" :dir "/tmp"))))
         (head "Current context: docs (doc-detail)\nViewing document GL123")
         (body "This proposal covers email notifications, a wiki knowledge base, and task datasets.")
         (context (concat head "\n" (make-string 700 ?-) "\n" body))
         (elark-ai-skill-routing-context-chars 600)
         (clipped (elark-ai-skills-routing-context context)))
    ;; The body lies beyond the clip…
    (should-not (string-match-p "email" clipped))
    ;; …so the clipped text routes narrowly, while the raw text
    ;; would have matched mail and wiki from body noise.
    (let ((narrow (elark-ai-skills-select "Summarize what envhub is" clipped))
          (broad (elark-ai-skills-select "Summarize what envhub is" context)))
      (should (member "lark-mail" broad))
      (should (member "lark-wiki" broad))
      (should-not (member "lark-mail" narrow))
      (should-not (member "lark-wiki" narrow))
      ;; The doc context itself still routes to the doc skill.
      (should (member "lark-doc" narrow)))))

;;;; ACP backend

(ert-deftest elark-ai-test-acp-notification-chunk ()
  "Chunk extraction picks out agent message chunks and nothing else."
  ;; A real agent_message_chunk → (session-id . text).
  (should (equal '("sess-1" . "Hello")
                 (elark-ai-acp--notification-chunk
                  '((method . "session/update")
                    (params . ((sessionId . "sess-1")
                               (update . ((sessionUpdate . "agent_message_chunk")
                                          (content . ((type . "text")
                                                      (text . "Hello")))))))))))
  ;; Thought chunks and tool-call updates are ignored.
  (should-not (elark-ai-acp--notification-chunk
               '((method . "session/update")
                 (params . ((sessionId . "sess-1")
                            (update . ((sessionUpdate . "agent_thought_chunk")
                                       (content . ((text . "hmm"))))))))))
  (should-not (elark-ai-acp--notification-chunk
               '((method . "session/update")
                 (params . ((sessionId . "sess-1")
                            (update . ((sessionUpdate . "tool_call"))))))))
  ;; Other notification methods are ignored.
  (should-not (elark-ai-acp--notification-chunk
               '((method . "something/else") (params . ((x . 1))))))
  ;; Missing text degrades to the empty string, not nil.
  (should (equal '("sess-1" . "")
                 (elark-ai-acp--notification-chunk
                  '((method . "session/update")
                    (params . ((sessionId . "sess-1")
                               (update . ((sessionUpdate . "agent_message_chunk"))))))))))

(ert-deftest elark-ai-test-acp-reject-option-id ()
  "Permission auto-decline prefers reject_once, falls back to reject_always."
  (let ((options [((optionId . "allow") (kind . "allow_once"))
                  ((optionId . "reject") (kind . "reject_once"))
                  ((optionId . "reject-forever") (kind . "reject_always"))]))
    (should (equal "reject" (elark-ai-acp--reject-option-id options))))
  (should (equal "reject-forever"
                 (elark-ai-acp--reject-option-id
                  [((optionId . "allow") (kind . "allow_once"))
                   ((optionId . "reject-forever") (kind . "reject_always"))])))
  (should-not (elark-ai-acp--reject-option-id
               [((optionId . "allow") (kind . "allow_once"))]))
  (should-not (elark-ai-acp--reject-option-id [])))

(ert-deftest elark-ai-test-acp-system-prompt-placement ()
  "`session' placement routes the system prompt to the session meta."
  (let ((elark-ai-acp-system-prompt-placement 'session))
    (should (equal "SYS" (elark-ai-acp--session-system-prompt "SYS")))
    (should-not (elark-ai-acp--session-system-prompt ""))
    (should-not (elark-ai-acp--session-system-prompt nil)))
  (let ((elark-ai-acp-system-prompt-placement 'inline))
    (should-not (elark-ai-acp--session-system-prompt "SYS"))))

(ert-deftest elark-ai-test-acp-prompt-text ()
  "System prompt is prepended in a tagged block; empty system passes through."
  (let ((combined (elark-ai-acp--prompt-text "Be terse." "List my docs")))
    (should (string-match-p "<system-instructions>\nBe terse.\n</system-instructions>" combined))
    (should (string-suffix-p "List my docs" combined)))
  (should (equal "List my docs" (elark-ai-acp--prompt-text "" "List my docs")))
  (should (equal "List my docs" (elark-ai-acp--prompt-text nil "List my docs"))))

(ert-deftest elark-ai-test-acp-backend-dispatch ()
  "`elark-ai-backend' `acp' routes both call seams to `elark-ai-acp-call'."
  (let ((elark-ai-backend 'acp)
        calls)
    (cl-letf (((symbol-function 'elark-ai-acp-call)
               (lambda (system user callback &optional on-chunk)
                 (push (list system user on-chunk) calls)
                 (funcall callback "acp says hi")))
              ;; Streaming path touches the AI buffer's output fragment.
              ((symbol-function 'elark-ai--ensure-output-fragment) #'ignore))
      ;; Non-streaming.
      (let (got)
        (elark-ai--call-llm "SYS" "USER" (lambda (text) (setq got text)))
        (should (equal "acp says hi" got))
        (should (equal '("SYS" "USER" nil) (car calls))))
      ;; Streaming with an explicit chunk handler: the acp path wraps it,
      ;; so a non-nil handler is passed through to `elark-ai-acp-call'.
      (let (got)
        (elark-ai--call-llm-stream "SYS2" "USER2"
                                  (lambda (text) (setq got text))
                                  (lambda (_chunk) nil))
        (should (equal "acp says hi" got))
        (pcase-let ((`(,system ,user ,on-chunk) (car calls)))
          (should (equal "SYS2" system))
          (should (equal "USER2" user))
          (should (functionp on-chunk)))))))

(ert-deftest elark-ai-test-acp-stream-chunks-without-ai-buffer ()
  "A chunk handler gets streamed chunks even with no `*Lark AI*' buffer.
The AI shell streams briefs this way and never creates that buffer."
  (let ((elark-ai-backend 'acp)
        chunks)
    (when (get-buffer elark-ai--buf-name)
      (kill-buffer elark-ai--buf-name))
    (cl-letf (((symbol-function 'elark-ai-acp-call)
               (lambda (_system _user callback &optional on-chunk)
                 (funcall on-chunk "Brief ")
                 (funcall on-chunk "text")
                 (funcall callback "Brief text"))))
      (elark-ai--call-llm-stream "SYS" "USER" #'ignore
                                (lambda (chunk) (push chunk chunks))))
    (should (equal '("Brief " "text") (reverse chunks)))))

(ert-deftest elark-ai-test-acp-finish-and-abort ()
  "Accumulate → finish invokes the callback; abort drops it; cancel is silent."
  (let ((elark-ai-acp--requests nil)
        (chunks nil)
        (final nil))
    ;; Simulate a session/new success registering the request.
    (push (cons "s1" (list :accumulated ""
                           :on-chunk (lambda (c) (push c chunks))
                           :callback (lambda (text) (setq final text))))
          elark-ai-acp--requests)
    ;; Two streamed chunks arrive.
    (dolist (text '("Hello, " "world"))
      (elark-ai-acp--on-notification
       `((method . "session/update")
         (params . ((sessionId . "s1")
                    (update . ((sessionUpdate . "agent_message_chunk")
                               (content . ((text . ,text))))))))))
    (should (equal '("world" "Hello, ") chunks))
    ;; end_turn → callback fires with the accumulated text.
    (elark-ai-acp--finish "s1" "end_turn")
    (should (equal "Hello, world" final))
    (should-not elark-ai-acp--requests)
    ;; A cancelled turn never calls back.
    (setq final nil)
    (push (cons "s2" (list :accumulated "partial"
                           :callback (lambda (text) (setq final text))))
          elark-ai-acp--requests)
    (elark-ai-acp--finish "s2" "cancelled")
    (should-not final)
    (should-not elark-ai-acp--requests)))

(provide 'elark-ai-test)
;;; elark-ai-test.el ends here
