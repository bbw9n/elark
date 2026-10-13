;;; elark-meetings-test.el --- Tests for elark-meetings.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 bbw9n

;; Author: bbw9n <bbw9nio@gmail.com>
;; Assisted-by: Claude:claude-opus-5
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'ert)

(let ((root (expand-file-name ".." (file-name-directory (or load-file-name (buffer-file-name))))))
  (dolist (sub '("." "core" "ui" "domain" "ai"))
    (add-to-list 'load-path (expand-file-name sub root))))

(require 'elark-meetings)

;;;; Field extractors

(ert-deftest elark-meetings-test-meeting-id ()
  (should (equal (elark-meetings--meeting-id '((id . "123"))) "123"))
  (should (equal (elark-meetings--meeting-id '((meeting_id . "456"))) "456"))
  (should (equal (elark-meetings--meeting-id '((foo . "bar"))) "")))

(ert-deftest elark-meetings-test-meeting-title ()
  "Title is first line of display_info."
  (should (equal (elark-meetings--meeting-title
                  '((display_info . "Team Standup\n\nToday 10:00")))
                 "Team Standup"))
  ;; Fallback to meta_data.description
  (should (equal (elark-meetings--meeting-title
                  '((meta_data . ((description . "Daily sync")))))
                 "Daily sync"))
  ;; Fallback to topic
  (should (equal (elark-meetings--meeting-title '((topic . "Retro")))
                 "Retro"))
  (should (equal (elark-meetings--meeting-title '((foo . "bar")))
                 "(untitled meeting)")))

(ert-deftest elark-meetings-test-meeting-description ()
  (should (equal (elark-meetings--meeting-description
                  '((meta_data . ((description . "10:00 | Org: Alice")))))
                 "10:00 | Org: Alice"))
  (should (equal (elark-meetings--meeting-description '((foo . "bar"))) "")))

(ert-deftest elark-meetings-test-meeting-app-link ()
  (should (equal (elark-meetings--meeting-app-link
                  '((meta_data . ((app_link . "https://example.com/meeting")))))
                 "https://example.com/meeting"))
  (should (equal (elark-meetings--meeting-app-link '((foo . "bar"))) "")))

;;;; Extraction

(ert-deftest elark-meetings-test-extract-meetings ()
  (let ((data '((data . ((items . (((id . "1")) ((id . "2")))))))))
    (should (= (length (elark-meetings--extract-meetings data)) 2))))

(ert-deftest elark-meetings-test-extract-meetings-flat ()
  (let ((data '((items . (((id . "1")))))))
    (should (= (length (elark-meetings--extract-meetings data)) 1))))

(ert-deftest elark-meetings-test-extract-meetings-nil ()
  (should (null (elark-meetings--extract-meetings nil))))

(ert-deftest elark-meetings-test-extract-notes ()
  (let ((data '((data . ((notes . (((meeting_id . "1") (summary . "hi")))))))))
    (should (= (length (elark-meetings--extract-notes data)) 1))))

;;;; Insert meeting

(ert-deftest elark-meetings-test-insert-meeting ()
  (let ((meeting '((id . "7627741315669528286")
                   (display_info . "Team Standup\n\nToday 14:24 | Org: Alice")
                   (meta_data . ((description . "Today 14:24 | Org: Alice")
                                 (app_link . "https://example.com/m/123"))))))
    (with-temp-buffer
      (elark-meetings--insert-meeting meeting)
      (goto-char (point-min))
      (should (search-forward "Team Standup" nil t))
      (should (search-forward "Today 14:24" nil t))
      (should (search-forward "7627741315669528286" nil t))
      ;; Text properties
      (goto-char (point-min))
      (should (equal (get-text-property (point) 'elark-meeting-id) "7627741315669528286"))
      (should (elark-meetings--meeting-at-point)))))

;;;; Time helpers

(ert-deftest elark-meetings-test-default-times ()
  "Default start/end are formatted dates."
  (let ((start (elark-meetings--default-start))
        (end (elark-meetings--default-end)))
    (should (string-match-p "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}$" start))
    (should (string-match-p "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}$" end))
    ;; end should be after start
    (should (string> end start))))

(provide 'elark-meetings-test)
;;; elark-meetings-test.el ends here
