;;; elark-ai-context-graph-test.el --- Tests for the cross-domain context provider -*- lexical-binding: t; -*-

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

(require 'elark-ai-context-graph)

;;;; HTML / text helpers

(ert-deftest elark-ai-context-graph-test-strip-html-removes-h-tags ()
  "Search highlight `<h>…</h>' tags are stripped from results."
  (should (equal "Budget Tracker"
                 (elark-ai-context-graph--strip-html
                  "<h>Budget</h> Tracker"))))

(ert-deftest elark-ai-context-graph-test-strip-html-decodes-entities ()
  "Common HTML entities (&amp;/&#39;/&lt;/&gt;/&quot;) are decoded."
  (should (equal "A & B's <plan>"
                 (elark-ai-context-graph--strip-html
                  "A &amp; B&#39;s &lt;plan&gt;"))))

(ert-deftest elark-ai-context-graph-test-strip-html-handles-nil ()
  "Nil input passes through (so missing fields don't error the pipeline)."
  (should (null (elark-ai-context-graph--strip-html nil))))

(ert-deftest elark-ai-context-graph-test-strip-html-decodes-numeric-entities ()
  "Numeric entities (`&#34;', `&#8217;') are decoded — Lark mixes them
with the named ones (`&quot;', `&#39;') and a missed one reads as
garbage in the rendered brief."
  (should (equal "say \"hi\" — got it"
                 (elark-ai-context-graph--strip-html
                  "say &#34;hi&#34; &#8212; got it"))))

(ert-deftest elark-ai-context-graph-test-squish-trims-leading-bullet ()
  "A leading newline must not leave a stray ` · ' separator at the head."
  (should (equal "first line · second"
                 (elark-ai-context-graph--squish-whitespace
                  "\nfirst line\nsecond"))))

(ert-deftest elark-ai-context-graph-test-truncate-clips-long ()
  "Over-budget strings are clipped with a trailing ellipsis."
  (should (equal "abcd…"
                 (elark-ai-context-graph--truncate "abcdefgh" 5))))

(ert-deftest elark-ai-context-graph-test-truncate-passes-short ()
  "Strings already under budget pass through unchanged."
  (should (equal "abc" (elark-ai-context-graph--truncate "abc" 10))))

(ert-deftest elark-ai-context-graph-test-squish-whitespace ()
  "Internal whitespace runs collapse; newlines become ` · ' separators."
  (should (equal "alpha · beta gamma"
                 (elark-ai-context-graph--squish-whitespace
                  "alpha\n\nbeta   gamma"))))

;;;; format-docs

(defvar elark-ai-context-graph-test--docs-fixture
  '((ok . t)
    (data
     (has_more . t)
     (total . 1442)
     (results .
              [((entity_type . "DOC")
                (result_meta
                 (title . "Budget Tracker")
                 (url . "https://example.com/budget")
                 (edit_user_name . "Alice")
                 (update_time_iso . "2026-06-04T18:53:26+08:00")
                 (doc_types . "SHEET"))
                (title_highlighted . "<h>Budget</h> Tracker")
                (summary_highlighted . "<h>Budget</h> Allocation for Q2"))
               ((entity_type . "WIKI")
                (result_meta
                 (title . "Workshop Policy")
                 (url . "https://example.com/wiki")
                 (edit_user_name . "Bob")
                 (update_time_iso . "2025-09-30T10:13:49+08:00")
                 (doc_types . "DOCX"))
                (title_highlighted . "Workshop <h>Budget</h> Policy")
                (summary_highlighted . "<h>Budget</h> management: granted per dept."))]))))

(ert-deftest elark-ai-context-graph-test-format-docs-basic ()
  "format-docs renders title, url, doctype, author, date, snippet, total."
  (let ((out (elark-ai-context-graph--format-docs
              elark-ai-context-graph-test--docs-fixture 10000)))
    (should (stringp out))
    (should (string-match-p "2 of 1442 matches shown" out))
    (should (string-match-p "Budget Tracker" out))
    (should (string-match-p "https://example.com/budget" out))
    (should (string-match-p "SHEET · Alice · 2026-06-04" out))
    (should (string-match-p "Workshop Budget Policy" out))
    ;; Highlight tags should NOT leak into the output.
    (should-not (string-match-p "<h>" out))))

(ert-deftest elark-ai-context-graph-test-format-docs-nil-on-empty ()
  "format-docs returns nil when there are no results — section is skipped."
  (should (null (elark-ai-context-graph--format-docs
                 '((ok . t) (data (results . []) (total . 0))) 10000))))

(ert-deftest elark-ai-context-graph-test-format-docs-respects-budget ()
  "Once the cumulative line length exceeds BUDGET, remaining items are dropped."
  (let ((out (elark-ai-context-graph--format-docs
              elark-ai-context-graph-test--docs-fixture 80)))
    ;; Tight budget — only one item should fit, but at least one is shown.
    (should (stringp out))
    (should (string-match-p "1 of 1442" out))
    (should-not (string-match-p "Workshop Budget Policy" out))))

;;;; format-im

(defvar elark-ai-context-graph-test--im-fixture
  '((ok . t)
    (data
     (has_more . :false)
     (total . 3)
     (messages .
               [((chat_id . "oc_1") (chat_name . "Eng Team")
                 (chat_type . "group")
                 (sender (id . "ou_alice") (name . "Alice"))
                 (create_time . "2026-05-29 17:45")
                 (content . "<card title=\"Status\">Budget review next Tue</card>")
                 (message_id . "om_a")
                 (message_app_link . "https://applink/m/a"))
                ((chat_id . "oc_2")
                 (chat_partner (open_id . "ou_bob"))
                 (chat_type . "p2p")
                 (sender (id . "ou_bob") (name . "Bob"))
                 (create_time . "2026-05-30 09:10")
                 (content . "alpha\n\nbeta   gamma")
                 (message_id . "om_b")
                 (message_app_link . "https://applink/m/b"))]))))

(ert-deftest elark-ai-context-graph-test-format-im-basic ()
  "format-im renders sender, chat name, time, link, and stripped content."
  (let ((out (elark-ai-context-graph--format-im
              elark-ai-context-graph-test--im-fixture 10000)))
    (should (stringp out))
    (should (string-match-p "2 of 3 matches shown" out))
    (should (string-match-p "Alice" out))
    (should (string-match-p "Eng Team" out))
    (should (string-match-p "Budget review next Tue" out))
    (should (string-match-p "https://applink/m/a" out))
    ;; <card> wrapper stripped.
    (should-not (string-match-p "<card" out))
    ;; Whitespace squished on the p2p message.
    (should (string-match-p "alpha · beta gamma" out))))

(ert-deftest elark-ai-context-graph-test-format-im-app-and-p2p-fallbacks ()
  "App senders show a `[sender-type]' tag; p2p chats render as `(direct)'."
  (let* ((fixture
          '((ok . t)
            (data
             (total . 1)
             (messages .
                       [((chat_id . "oc_x")
                         (chat_partner (open_id . "ou_someone"))
                         (chat_type . "p2p")
                         ;; No `name', so the sender block is bot-shaped.
                         (sender (id . "cli_bot") (sender_type . "app"))
                         (create_time . "2026-06-01 12:00")
                         (content . "release passed"))]))))
         (out (elark-ai-context-graph--format-im fixture 10000)))
    (should (string-match-p "\\[app\\]" out))
    (should (string-match-p "(direct)" out))
    ;; The raw open_id should NOT leak into the rendered brief.
    (should-not (string-match-p "ou_someone" out))))

(ert-deftest elark-ai-context-graph-test-format-im-nil-on-empty ()
  "format-im returns nil when no messages match."
  (should (null (elark-ai-context-graph--format-im
                 '((ok . t) (data (messages . []) (total . 0))) 10000))))

;;;; combine

(ert-deftest elark-ai-context-graph-test-combine-orders-by-registration ()
  "Combine preserves the provider registry order; empty sections are dropped."
  (let* ((results (make-hash-table :test 'equal)))
    (puthash "Docs" "doc body" results)
    (puthash "Messages" "" results)
    (puthash "Tasks" "task body" results)
    (let ((out (elark-ai-context-graph--combine
                '(("Docs" . nil) ("Messages" . nil) ("Tasks" . nil))
                results)))
      ;; Empty Messages section is omitted; the others appear in order.
      (should (string-match-p "## Docs\n\ndoc body" out))
      (should (string-match-p "## Tasks\n\ntask body" out))
      (should-not (string-match-p "## Messages" out))
      (should (< (string-match "Docs" out) (string-match "Tasks" out))))))

;;;; gather

(ert-deftest elark-ai-context-graph-test-gather-fans-out-and-aggregates ()
  "Two providers, both succeed: gather waits for both, then combines."
  (let* ((elark-ai-context-graph-providers
          `(("A" . ,(lambda (_topic _b cb) (funcall cb "A body")))
            ("B" . ,(lambda (_topic _b cb) (funcall cb "B body")))))
         (got nil))
    (elark-ai-context-graph-gather "anything"
                                  (lambda (text) (setq got text)))
    (should (stringp got))
    (should (string-match-p "## A\n\nA body" got))
    (should (string-match-p "## B\n\nB body" got))))

(ert-deftest elark-ai-context-graph-test-gather-empty-topic-shortcircuits ()
  "Empty or whitespace topic returns the empty string immediately."
  (let* ((called 0)
         (elark-ai-context-graph-providers
          `(("A" . ,(lambda (&rest _) (cl-incf called)))))
         (got nil))
    (elark-ai-context-graph-gather "  " (lambda (s) (setq got s)))
    (should (equal "" got))
    (should (= 0 called))))

(ert-deftest elark-ai-context-graph-test-gather-survives-provider-error ()
  "When one provider signals, the gatherer logs it and still returns the rest."
  (let* ((elark-ai-context-graph-providers
          `(("Good" . ,(lambda (_t _b cb) (funcall cb "good body")))
            ("Bad"  . ,(lambda (&rest _) (error "boom")))))
         (got nil))
    (cl-letf (((symbol-function 'elark-ai--progress-log)
               (lambda (&rest _) nil)))
      (elark-ai-context-graph-gather "topic" (lambda (s) (setq got s))))
    (should (stringp got))
    (should (string-match-p "## Good\n\ngood body" got))
    (should-not (string-match-p "## Bad" got))))

(ert-deftest elark-ai-context-graph-test-gather-skips-nil-providers ()
  "Provider that calls done-fn with nil contributes no section."
  (let* ((elark-ai-context-graph-providers
          `(("A" . ,(lambda (_t _b cb) (funcall cb "A body")))
            ("B" . ,(lambda (_t _b cb) (funcall cb nil)))))
         (got nil))
    (elark-ai-context-graph-gather "topic" (lambda (s) (setq got s)))
    (should (string-match-p "## A" got))
    (should-not (string-match-p "## B" got))))

(provide 'elark-ai-context-graph-test)
;;; elark-ai-context-graph-test.el ends here
