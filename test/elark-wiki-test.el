;;; elark-wiki-test.el --- Tests for elark-wiki.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 bbw9n

;; Author: bbw9n <bbw9nio@gmail.com>
;; Assisted-by: Claude:claude-opus-5
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'ert)

(let ((root (expand-file-name ".." (file-name-directory (or load-file-name (buffer-file-name))))))
  (dolist (sub '("." "core" "ui" "domain" "ai"))
    (add-to-list 'load-path (expand-file-name sub root))))

(require 'elark-wiki)

;;;; Space field extractors

(ert-deftest elark-wiki-test-space-id-of ()
  (should (equal (elark-wiki--space-id-of '((space_id . "sp1"))) "sp1"))
  (should (equal (elark-wiki--space-id-of '((foo . "bar"))) "")))

(ert-deftest elark-wiki-test-space-name ()
  (should (equal (elark-wiki--space-name '((name . "Eng KB"))) "Eng KB"))
  (should (equal (elark-wiki--space-name '((foo . "bar"))) "(unnamed space)")))

(ert-deftest elark-wiki-test-space-type ()
  (should (equal (elark-wiki--space-type '((space_type . "team"))) "team"))
  (should (equal (elark-wiki--space-type '((foo . "bar"))) "")))

;;;; Node field extractors

(ert-deftest elark-wiki-test-node-token ()
  (should (equal (elark-wiki--node-token '((node_token . "wikabc"))) "wikabc"))
  (should (equal (elark-wiki--node-token '((foo . "bar"))) "")))

(ert-deftest elark-wiki-test-obj-token-and-type ()
  (should (equal (elark-wiki--obj-token '((obj_token . "docxAAA"))) "docxAAA"))
  (should (equal (elark-wiki--obj-token '((foo . "bar"))) ""))
  (should (equal (elark-wiki--obj-type '((obj_type . "docx"))) "docx"))
  (should (equal (elark-wiki--obj-type '((foo . "bar"))) "")))

(ert-deftest elark-wiki-test-node-title ()
  (should (equal (elark-wiki--node-title '((title . "Design Doc"))) "Design Doc"))
  (should (equal (elark-wiki--node-title '((title . ""))) "(untitled)"))
  (should (equal (elark-wiki--node-title '((foo . "bar"))) "(untitled)")))

(ert-deftest elark-wiki-test-node-type ()
  (should (equal (elark-wiki--node-type '((node_type . "shortcut"))) "shortcut"))
  ;; Defaults to origin when absent.
  (should (equal (elark-wiki--node-type '((foo . "bar"))) "origin")))

(ert-deftest elark-wiki-test-has-child-p ()
  (should (elark-wiki--has-child-p '((has_child . t))))
  (should-not (elark-wiki--has-child-p '((has_child . :false))))
  (should-not (elark-wiki--has-child-p '((has_child . :json-false))))
  (should-not (elark-wiki--has-child-p '((foo . "bar")))))

(ert-deftest elark-wiki-test-node-url ()
  (should (equal (elark-wiki--node-url '((url . "https://x/wiki/abc"))) "https://x/wiki/abc"))
  (should (equal (elark-wiki--node-url '((foo . "bar"))) "")))

;;;; Response extraction

(ert-deftest elark-wiki-test-extract-spaces ()
  ;; Real CLI shape: data.spaces (single-page and --page-all).
  (should (equal (elark-wiki--extract-spaces
                  '((data . ((spaces . (((space_id . "s1")) ((space_id . "s2"))))))))
                 '(((space_id . "s1")) ((space_id . "s2")))))
  ;; Defensive fallback to data.items.
  (should (equal (elark-wiki--extract-spaces
                  '((data . ((items . (((space_id . "s3"))))))))
                 '(((space_id . "s3"))))))

(ert-deftest elark-wiki-test-extract-nodes ()
  ;; Real CLI shape: data.nodes.
  (should (equal (elark-wiki--extract-nodes
                  '((data . ((nodes . (((node_token . "n1"))))))))
                 '(((node_token . "n1")))))
  ;; Defensive fallback to data.items.
  (should (equal (elark-wiki--extract-nodes
                  '((data . ((items . (((node_token . "n2"))))))))
                 '(((node_token . "n2"))))))

;;;; Synthetic personal library

(ert-deftest elark-wiki-test-my-library-space ()
  (let ((lib (elark-wiki--my-library-space)))
    (should (equal (elark-wiki--space-id-of lib) "my_library"))
    (should (equal (elark-wiki--space-type lib) "person"))))

;;;; Dispatch is a real command

(ert-deftest elark-wiki-test-dispatch-is-command ()
  (should (commandp 'elark-wiki-dispatch))
  (should (commandp 'elark-wiki-spaces))
  (should (commandp 'elark-wiki-open)))

(provide 'elark-wiki-test)
;;; elark-wiki-test.el ends here
