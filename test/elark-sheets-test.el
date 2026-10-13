;;; elark-sheets-test.el --- Tests for elark-sheets.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 bbw9n

;; Author: bbw9n <bbw9nio@gmail.com>
;; Assisted-by: Claude:claude-opus-5
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'ert)

(let ((root (expand-file-name ".." (file-name-directory (or load-file-name (buffer-file-name))))))
  (dolist (sub '("." "core" "ui" "domain" "ai"))
    (add-to-list 'load-path (expand-file-name sub root))))

(require 'elark-sheets)

;;;; Value extraction

(ert-deftest elark-sheets-test-extract-values-nested ()
  "Extract values from nested response."
  (let ((data '((data . ((valueRange . ((values . (("a" "b") ("1" "2"))))))))))
    (should (equal (elark-sheets--extract-values data)
                   '(("a" "b") ("1" "2"))))))

(ert-deftest elark-sheets-test-extract-values-flat ()
  "Extract values from flat valueRange."
  (let ((data '((valueRange . ((values . (("x" "y"))))))))
    (should (equal (elark-sheets--extract-values data)
                   '(("x" "y"))))))

(ert-deftest elark-sheets-test-extract-values-nil ()
  (should (null (elark-sheets--extract-values nil))))

;;;; Sheet/spreadsheet extraction

(ert-deftest elark-sheets-test-extract-sheets ()
  (let ((data '((data . ((sheets . ((sheets . (((sheet_id . "s1") (title . "Sheet1")))))))))))
    (let ((sheets (elark-sheets--extract-sheets (alist-get 'data data))))
      (should (= (length sheets) 1))
      (should (equal (alist-get 'sheet_id (car sheets)) "s1")))))

(ert-deftest elark-sheets-test-extract-spreadsheet ()
  (let ((data '((data . ((spreadsheet . ((spreadsheet . ((token . "tok1") (title . "My Sheet"))))))))))
    (let ((ss (elark-sheets--extract-spreadsheet (alist-get 'data data))))
      (should (equal (alist-get 'token ss) "tok1"))
      (should (equal (alist-get 'title ss) "My Sheet")))))

;;;; Org table conversion

(ert-deftest elark-sheets-test-values-to-org-table ()
  "Convert 2D list to org table string."
  (let* ((values '(("Name" "Age") ("Alice" "30") ("Bob" "25")))
         (result (elark-sheets--values-to-org-table values)))
    (should (stringp result))
    ;; Header
    (should (string-match-p "| Name | Age |" result))
    ;; Separator after header
    (should (string-match-p "|-" result))
    ;; Data rows
    (should (string-match-p "| Alice | 30 |" result))
    (should (string-match-p "| Bob | 25 |" result))))

(ert-deftest elark-sheets-test-values-to-org-table-nil ()
  (should (null (elark-sheets--values-to-org-table nil))))

(ert-deftest elark-sheets-test-values-to-org-table-numbers ()
  "Numeric values are converted to strings."
  (let ((result (elark-sheets--values-to-org-table '(("x") (42)))))
    (should (string-match-p "| 42 |" result))))

(ert-deftest elark-sheets-test-values-to-org-table-nil-cells ()
  "Nil cell values become empty strings."
  (let ((result (elark-sheets--values-to-org-table '(("a" nil "b")))))
    (should (string-match-p "| a |  | b |" result))))

;;;; Org table parsing

(ert-deftest elark-sheets-test-org-table-to-values ()
  "Parse an org table back to a 2D list."
  (with-temp-buffer
    (org-mode)
    (insert "| Name  | Age |\n")
    (insert "|-------+-----|\n")
    (insert "| Alice | 30  |\n")
    (insert "| Bob   | 25  |\n")
    (goto-char (point-min))
    (let ((values (elark-sheets--org-table-to-values)))
      (should (= (length values) 3))  ; header + 2 data rows (hline stripped)
      (should (equal (car (car values)) "Name"))
      (should (equal (car (nth 1 values)) "Alice"))
      (should (equal (cadr (nth 2 values)) "25")))))

;;;; Range adjustment

(ert-deftest elark-sheets-test-adjust-range ()
  (should (equal (elark-sheets--adjust-range "sheet!A1:E10" 3 5)
                 "sheet!A3:E5"))
  (should (equal (elark-sheets--adjust-range "A1:C20" 2 8)
                 "A2:C8"))
  ;; Fallback for unrecognized format
  (should (equal (elark-sheets--adjust-range "weird" 1 2) "weird")))

(provide 'elark-sheets-test)
;;; elark-sheets-test.el ends here
