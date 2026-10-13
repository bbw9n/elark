;;; elark-auth-test.el --- Tests for elark-auth.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 bbw9n

;; Author: bbw9n <bbw9nio@gmail.com>
;; Assisted-by: Claude:claude-opus-5
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; ERT tests for elark's auth helpers — currently focused on the
;; device-code polling success detector, which previously missed
;; completion when the CLI's response shape didn't include `ok: t'.

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Add project directories to load-path.
(let ((root (expand-file-name ".." (file-name-directory
                                    (or load-file-name (buffer-file-name))))))
  (dolist (sub '("." "core" "ui" "domain" "ai"))
    (add-to-list 'load-path (expand-file-name sub root))))

(require 'elark-auth)

;;;; Login success detection

(ert-deftest elark-auth-test-success-explicit-ok ()
  "An explicit `ok: t' is recognised as success."
  (should (elark-auth--login-success-p '((ok . t))))
  (should (elark-auth--login-success-p '((ok . t) (data . ((some . "thing")))))))

(ert-deftest elark-auth-test-success-user-fields ()
  "A user field at the top level (any of several names) means success."
  (should (elark-auth--login-success-p '((userName . "alice"))))
  (should (elark-auth--login-success-p '((user_name . "bob"))))
  (should (elark-auth--login-success-p '((name . "carol")))))

(ert-deftest elark-auth-test-success-nested-data ()
  "User / token fields nested under `data' also count."
  (should (elark-auth--login-success-p '((data . ((user_name . "dave"))))))
  (should (elark-auth--login-success-p '((data . ((userName . "eve"))))))
  (should (elark-auth--login-success-p '((data . ((access_token . "tok"))))))
  (should (elark-auth--login-success-p '((data . ((accessToken . "tok")))))))

(ert-deftest elark-auth-test-success-access-token-top ()
  "An access token at the top level is also accepted."
  (should (elark-auth--login-success-p '((access_token . "abc"))))
  (should (elark-auth--login-success-p '((accessToken . "abc")))))

(ert-deftest elark-auth-test-success-rejects-pending ()
  "Pending / error / nil responses are NOT treated as success."
  (should-not (elark-auth--login-success-p nil))
  (should-not (elark-auth--login-success-p '((ok . :false))))
  (should-not (elark-auth--login-success-p
               '((error . ((message . "authorization_pending"))))))
  ;; A bare error shape with no user / token fields anywhere.
  (should-not (elark-auth--login-success-p '((ok . :false) (error . ((code . 1)))))))

;;;; Shape-tolerant user extraction (the auth-status fix)

(ert-deftest elark-auth-test-extract-user-top-level ()
  "Top-level user fields are found."
  (should (equal "alice" (elark-auth--extract-user '((userName . "alice")))))
  (should (equal "bob"   (elark-auth--extract-user '((user_name . "bob")))))
  (should (equal "carol" (elark-auth--extract-user '((name . "carol")))))
  (should (equal "dave"  (elark-auth--extract-user '((displayName . "dave"))))))

(ert-deftest elark-auth-test-extract-user-data-envelope ()
  "User fields nested under `data' (the {ok, identity, data: {…}} envelope) are found."
  (should (equal "eve"  (elark-auth--extract-user
                         '((ok . t) (identity . "user")
                           (data . ((userName . "eve")))))))
  (should (equal "frank" (elark-auth--extract-user
                          '((data . ((user_name . "frank"))))))))

(ert-deftest elark-auth-test-extract-user-nested-user ()
  "User fields under a `user' sub-object (top-level or under data) are found."
  (should (equal "grace" (elark-auth--extract-user '((user . ((name . "grace")))))))
  (should (equal "henry"
                 (elark-auth--extract-user
                  '((data . ((user . ((userName . "henry"))))))))))

(ert-deftest elark-auth-test-extract-user-rejects-empty ()
  "No user fields → nil."
  (should-not (elark-auth--extract-user nil))
  (should-not (elark-auth--extract-user '((ok . :false))))
  (should-not (elark-auth--extract-user '((error . ((message . "not authed")))))))

(ert-deftest elark-auth-test-extract-user-auth-status-shape ()
  "The real `lark-cli auth status' shape (identities.<identity>.userName)."
  (let ((real '((appId . "cli_xxx")
                (brand . "feishu")
                (defaultAs . "auto")
                (identity . "user")
                (identities . ((bot . ((status . "ready") (available . t)
                                       (message . "Bot identity: ready")))
                               (user . ((status . "ready") (available . t)
                                        (userName . "Zhenjun Wang")
                                        (tokenStatus . "valid")
                                        (openId . "ou_xxx"))))))))
    (should (equal "Zhenjun Wang" (elark-auth--extract-user real)))
    (should (equal "valid" (elark-auth--extract-field real 'tokenStatus)))
    (should (equal "user"  (elark-auth--extract-field real 'identity)))))

(ert-deftest elark-auth-test-extract-user-auth-status-bot ()
  "When the active identity is `bot' and only `bot' has a userName."
  (let ((bot-active '((identity . "bot")
                      (identities . ((bot  . ((status . "ready")
                                              (userName . "bot-acct")
                                              (tokenStatus . "valid")))
                                     (user . ((status . "ready")
                                              (available . :false))))))))
    (should (equal "bot-acct" (elark-auth--extract-user bot-active)))
    (should (equal "valid"    (elark-auth--extract-field bot-active 'tokenStatus)))))

(provide 'elark-auth-test)
;;; elark-auth-test.el ends here
