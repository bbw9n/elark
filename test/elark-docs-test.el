;;; elark-docs-test.el --- Tests for elark-docs.el -*- lexical-binding: t; -*-

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

(require 'elark-docs)

;;;; Field extractors

(ert-deftest elark-docs-test-doc-title ()
  (should (equal (elark-docs--doc-title '((title . "My Doc"))) "My Doc"))
  (should (equal (elark-docs--doc-title '((name . "Alt"))) "Alt"))
  (should (equal (elark-docs--doc-title '((foo . "bar"))) "(untitled)"))
  ;; search result with highlight tags
  (should (equal (elark-docs--doc-title '((title_highlighted . "My <h>Doc</h>")))
                 "My Doc")))

(ert-deftest elark-docs-test-doc-token ()
  (should (equal (elark-docs--doc-token '((document_id . "d1"))) "d1"))
  (should (equal (elark-docs--doc-token '((doc_token . "d2"))) "d2"))
  ;; search result with token in result_meta
  (should (equal (elark-docs--doc-token '((result_meta . ((token . "d3"))))) "d3"))
  (should (equal (elark-docs--doc-token '((obj_token . "d4"))) "d4")))

(ert-deftest elark-docs-test-doc-type ()
  ;; search result: entity_type at top, doc_types in meta
  (should (equal (elark-docs--doc-type '((entity_type . "WIKI"))) "WIKI"))
  (should (equal (elark-docs--doc-type
                  '((result_meta . ((doc_types . "DOCX")))))
                 "DOCX"))
  (should (equal (elark-docs--doc-type '((foo . "bar"))) "")))

(ert-deftest elark-docs-test-doc-url ()
  ;; search result: url in result_meta
  (should (equal (elark-docs--doc-url
                  '((result_meta . ((url . "https://example.com")))))
                 "https://example.com"))
  (should (equal (elark-docs--doc-url '((url . "https://direct.com")))
                 "https://direct.com"))
  (should (equal (elark-docs--doc-url '((foo . "bar"))) "")))

(ert-deftest elark-docs-test-doc-owner ()
  ;; search result: owner_name in result_meta
  (should (equal (elark-docs--doc-owner
                  '((result_meta . ((owner_name . "Alice")))))
                 "Alice"))
  (should (equal (elark-docs--doc-owner '((owner_name . "Bob"))) "Bob"))
  (should (equal (elark-docs--doc-owner '((foo . "bar"))) "")))

(ert-deftest elark-docs-test-doc-create-time-iso ()
  ;; search result: create_time_iso in result_meta
  (should (equal (elark-docs--doc-create-time
                  '((result_meta . ((create_time_iso . "2025-10-28T19:29:32+08:00")))))
                 "2025-10-28T19:29")))

(ert-deftest elark-docs-test-doc-create-time-number ()
  (let ((result (elark-docs--doc-create-time '((create_time . 1700000000)))))
    (should (stringp result))
    (should (string-match-p "^[0-9]\\{4\\}-" result))))

(ert-deftest elark-docs-test-doc-create-time-empty ()
  (should (equal (elark-docs--doc-create-time '((foo . "bar"))) "")))

(ert-deftest elark-docs-test-doc-update-time-iso ()
  (should (equal (elark-docs--doc-update-time
                  '((result_meta . ((update_time_iso . "2026-04-08T14:28:42+08:00")))))
                 "2026-04-08T14:28")))

;;;; Extraction

(ert-deftest elark-docs-test-extract-doc ()
  (let ((data '((data . ((title . "Hello") (document_id . "d1"))))))
    (should (equal (alist-get 'title (elark-docs--extract-doc data)) "Hello"))))

(ert-deftest elark-docs-test-extract-search-results-nested ()
  (let ((data '((data . ((results . (((title_highlighted . "A"))
                                      ((title_highlighted . "B"))))
                          (has_more . t)
                          (total . 2))))))
    (should (= (length (elark-docs--extract-search-results data)) 2))))

(ert-deftest elark-docs-test-extract-search-results-flat ()
  (let ((data '((results . (((title_highlighted . "A")))))))
    (should (= (length (elark-docs--extract-search-results data)) 1))))

(ert-deftest elark-docs-test-extract-search-results-items-fallback ()
  (let ((data '((data . ((items . (((title . "A")))))))))
    (should (= (length (elark-docs--extract-search-results data)) 1))))

;;;; Insert result

(ert-deftest elark-docs-test-insert-result ()
  "Test insertion of a search result with result_meta nesting."
  (let ((doc '((entity_type . "WIKI")
               (result_meta . ((token . "SrhtwtsCYi")
                               (owner_name . "Alice")
                               (create_time_iso . "2025-10-28T19:29:32+08:00")
                               (update_time_iso . "2026-04-08T14:28:42+08:00")
                               (url . "https://lark.example.com/wiki/SrhtwtsCYi")))
               (title_highlighted . "My <h>Document</h>")
               (summary_highlighted . "some <h>summary</h> text"))))
    (with-temp-buffer
      (elark-docs--insert-result doc)
      (goto-char (point-min))
      (should (search-forward "My Document" nil t))
      (should (search-forward "WIKI" nil t))
      (should (search-forward "SrhtwtsCYi" nil t))
      (should (search-forward "Alice" nil t))
      (should (search-forward "2025-10-28T19:29" nil t))
      (should (search-forward "some summary text" nil t))
      ;; Text properties
      (goto-char (point-min))
      (should (equal (get-text-property (point) 'elark-doc-token) "SrhtwtsCYi"))
      (should (equal (get-text-property (point) 'elark-doc-title) "My Document")))))

;;;; org-lark integration

(ert-deftest elark-docs-test-render-mode-org ()
  "Org render mode renders as Org."
  (let ((elark-docs-render-mode 'org))
    (should (elark-docs--use-org-p))))

(ert-deftest elark-docs-test-render-mode-markdown ()
  "Markdown render mode does not render as Org."
  (let ((elark-docs-render-mode 'markdown))
    (should-not (elark-docs--use-org-p))))

(ert-deftest elark-docs-test-toggle-render-mode ()
  "Toggle cycles between org and markdown."
  (let ((elark-docs-render-mode 'org))
    (elark-docs-toggle-render-mode)
    (should (eq elark-docs-render-mode 'markdown))
    (elark-docs-toggle-render-mode)
    (should (eq elark-docs-render-mode 'org))))

(ert-deftest elark-docs-test-display-document-markdown-fallback ()
  "Display document in markdown mode when content is present."
  (let ((elark-docs-render-mode 'markdown))
    (elark-docs--display-document-markdown "Test" "tok123"
                                          '((doc_type . "docx")) "# Hello")
    (unwind-protect
        (let ((buf (get-buffer "*Lark Doc: Test*")))
          (should buf)
          (with-current-buffer buf
            (should (string-match-p "Hello" (buffer-string)))
            (should (equal elark-docs--doc-token "tok123"))))
      (when-let* ((buf (get-buffer "*Lark Doc: Test*")))
        (kill-buffer buf)))))

(ert-deftest elark-docs-test-org-buffer-display ()
  "Display org content in an org-mode buffer."
  (elark-docs--display-org-buffer "#+title: Test\n\n* Heading\nBody\n" "Test" "tok456")
  (unwind-protect
      (let ((buf (get-buffer "*Lark Doc: Test*")))
        (should buf)
        (with-current-buffer buf
          (should (derived-mode-p 'org-mode))
          (should (string-match-p "Heading" (buffer-string)))
          (should (equal elark-docs--doc-token "tok456"))))
    (when-let* ((buf (get-buffer "*Lark Doc: Test*")))
      (kill-buffer buf))))

;; Visiting doc.org is what lets gptel-rewrite edits be pushed back
;; via `org-lark-publish-buffer' (which needs a file buffer).
(ert-deftest elark-docs-test-org-buffer-visits-cache-file ()
  "With BASE-DIR, the org buffer visits BASE-DIR/doc.org and is saved."
  (let ((dir (make-temp-file "elark-docs-test" t)))
    (elark-docs--display-org-buffer "#+lark_doc_id: tok\n\n* H\n" "FileT" "tok" dir)
    (unwind-protect
        (with-current-buffer "*Lark Doc: FileT*"
          (should (equal buffer-file-name (expand-file-name "doc.org" dir)))
          (should-not (buffer-modified-p))
          (should (file-exists-p buffer-file-name)))
      (when-let* ((buf (get-buffer "*Lark Doc: FileT*"))) (kill-buffer buf))
      (delete-directory dir t))))

(ert-deftest elark-docs-test-fetch-routes-to-org ()
  "elark-docs-fetch delegates to fetch-as-org in org mode."
  (let ((elark-docs-render-mode 'org)
        (called-with nil))
    (cl-letf (((symbol-function 'elark-docs-fetch-as-org)
               (lambda (doc) (setq called-with doc))))
      (elark-docs-fetch "my-doc-token")
      (should (equal called-with "my-doc-token")))))

(ert-deftest elark-docs-test-fetch-routes-to-markdown ()
  "elark-docs-fetch uses elark--run-command when render mode is markdown."
  (let ((elark-docs-render-mode 'markdown)
        (ran-command nil))
    (cl-letf (((symbol-function 'elark--run-command)
               (lambda (args &rest _) (setq ran-command args) nil)))
      (elark-docs-fetch "my-doc-token")
      (should (equal ran-command '("docs" "+fetch" "--doc" "my-doc-token"))))))

(ert-deftest elark-docs-test-cache-dir ()
  "Doc cache resolves persistently by default and honors the custom dir.
Regression: the temp-dir default was purged by the OS, re-downloading
every image and leaving doc buffers with a deleted working directory."
  ;; Custom directory wins, keeping the elark-docs/<id> layout.
  (let ((elark-docs-cache-directory "/custom/cache"))
    (should (equal "/custom/cache/elark-docs/GL_123"
                   (elark-docs--cache-dir "GL:123"))))
  ;; Default: persistent XDG cache, not the temp dir.
  (let ((elark-docs-cache-directory nil)
        (xdg (make-temp-file "elark-docs-xdg-test" t))
        (old (getenv "XDG_CACHE_HOME")))
    (unwind-protect
        (progn
          (setenv "XDG_CACHE_HOME" xdg)
          (should (equal (expand-file-name "elark/elark-docs/GL_123" xdg)
                         (elark-docs--cache-dir "GL:123"))))
      (setenv "XDG_CACHE_HOME" old)
      (delete-directory xdg t))))

(provide 'elark-docs-test)
;;; elark-docs-test.el ends here
