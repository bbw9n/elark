;;; elark-drive-test.el --- Tests for elark-drive.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 bbw9n

;; Author: bbw9n <bbw9nio@gmail.com>
;; Assisted-by: Claude:claude-opus-5
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'ert)

(let ((root (expand-file-name ".." (file-name-directory (or load-file-name (buffer-file-name))))))
  (dolist (sub '("." "core" "ui" "domain" "ai"))
    (add-to-list 'load-path (expand-file-name sub root))))

(require 'elark-drive)

;;;; Field extractors

(ert-deftest elark-drive-test-file-token ()
  (should (equal (elark-drive--file-token '((token . "tok1"))) "tok1"))
  (should (equal (elark-drive--file-token '((file_token . "tok2"))) "tok2"))
  (should (equal (elark-drive--file-token '((foo . "bar"))) "")))

(ert-deftest elark-drive-test-file-name ()
  (should (equal (elark-drive--file-name '((name . "report.pdf"))) "report.pdf"))
  (should (equal (elark-drive--file-name '((title . "Alt"))) "Alt"))
  (should (equal (elark-drive--file-name '((foo . "bar"))) "(unnamed)")))

(ert-deftest elark-drive-test-file-type ()
  (should (equal (elark-drive--file-type '((type . "folder"))) "folder"))
  (should (equal (elark-drive--file-type '((foo . "bar"))) "file")))

(ert-deftest elark-drive-test-folder-p ()
  (should (elark-drive--folder-p '((type . "folder"))))
  (should-not (elark-drive--folder-p '((type . "docx")))))

(ert-deftest elark-drive-test-file-url ()
  (should (equal (elark-drive--file-url '((url . "https://example.com"))) "https://example.com"))
  (should (equal (elark-drive--file-url '((foo . "bar"))) "")))

(ert-deftest elark-drive-test-modified-time-numeric-string ()
  "Modified time from a numeric string timestamp."
  (let ((result (elark-drive--file-modified-time '((modified_time . "1775742859")))))
    (should (stringp result))
    (should (string-match-p "^[0-9]\\{4\\}-" result))))

(ert-deftest elark-drive-test-modified-time-number ()
  (let ((result (elark-drive--file-modified-time '((modified_time . 1775742859)))))
    (should (stringp result))
    (should (string-match-p "^[0-9]\\{4\\}-" result))))

(ert-deftest elark-drive-test-modified-time-empty ()
  (should (equal (elark-drive--file-modified-time '((foo . "bar"))) "")))

(ert-deftest elark-drive-test-type-indicator ()
  (should (equal (elark-drive--type-indicator "folder") "/"))
  (should (equal (elark-drive--type-indicator "docx") " [docx]"))
  (should (equal (elark-drive--type-indicator "sheet") " [sheet]"))
  (should (equal (elark-drive--type-indicator "file") "")))

;;;; Extract files

(ert-deftest elark-drive-test-extract-files-nested ()
  (let ((data '((data . ((files . (((token . "t1") (name . "A"))
                                    ((token . "t2") (name . "B")))))))))
    (should (= (length (elark-drive--extract-files data)) 2))))

(ert-deftest elark-drive-test-extract-files-flat ()
  (let ((data '((files . (((token . "t1")))))))
    (should (= (length (elark-drive--extract-files data)) 1))))

;;;; Rendering

(ert-deftest elark-drive-test-insert-file-entry ()
  "Insert a file entry with text properties."
  (let ((file '((token . "tok1")
                (name . "report.pdf")
                (type . "file")
                (modified_time . "1775742859"))))
    (with-temp-buffer
      (elark-drive--insert-file-entry file)
      (goto-char (point-min))
      (should (search-forward "report.pdf" nil t))
      ;; Text property covers the line
      (goto-char (point-min))
      (let ((f (get-text-property (point) 'elark-drive-file)))
        (should f)
        (should (equal (alist-get 'token f) "tok1"))
        (should (equal (alist-get 'type f) "file"))))))

(ert-deftest elark-drive-test-insert-folder-entry ()
  "Folder entries get a trailing / indicator."
  (let ((file '((token . "ftok")
                (name . "Documents")
                (type . "folder")
                (modified_time . "1775742859"))))
    (with-temp-buffer
      (elark-drive--insert-file-entry file)
      (goto-char (point-min))
      (should (search-forward "Documents/" nil t)))))

(ert-deftest elark-drive-test-render-sorts-folders-first ()
  "Render sorts folders before files."
  (let ((files '(((token . "f1") (name . "Zebra.pdf") (type . "file") (modified_time . "0"))
                 ((token . "f2") (name . "Alpha") (type . "folder") (modified_time . "0") (parent_token . "root"))
                 ((token . "f3") (name . "data.csv") (type . "file") (modified_time . "0")))))
    (elark-drive--render files "" "Test")
    (let ((buf (get-buffer "*Lark Drive*")))
      (should buf)
      (with-current-buffer buf
        (goto-char (point-min))
        ;; Folder should appear before files
        (let ((folder-pos (search-forward "Alpha/" nil t))
              (file-pos (progn (goto-char (point-min))
                               (search-forward "Zebra.pdf" nil t))))
          (should folder-pos)
          (should file-pos)
          (should (< folder-pos file-pos))))
      (kill-buffer buf))))

(provide 'elark-drive-test)
;;; elark-drive-test.el ends here
