;;; elark-tasks-test.el --- Tests for elark-tasks.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 bbw9n

;; Author: bbw9n <bbw9nio@gmail.com>
;; Assisted-by: Claude:claude-opus-5
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'ert)

(let ((root (expand-file-name ".." (file-name-directory (or load-file-name (buffer-file-name))))))
  (dolist (sub '("." "core" "ui" "domain" "ai"))
    (add-to-list 'load-path (expand-file-name sub root))))

(require 'elark-tasks)

;;;; Task parsing tests

(ert-deftest elark-tasks-test-task-id ()
  "Extract task ID — guid preferred."
  (should (equal (elark-tasks--task-id '((guid . "g789")))
                 "g789"))
  (should (equal (elark-tasks--task-id '((task_id . "t_123")))
                 "t_123"))
  (should (equal (elark-tasks--task-id '((id . "456")))
                 "456")))

(ert-deftest elark-tasks-test-task-title ()
  "Extract task title."
  (should (equal (elark-tasks--task-title '((summary . "Fix bug")))
                 "Fix bug"))
  (should (equal (elark-tasks--task-title '((title . "Review PR")))
                 "Review PR"))
  (should (equal (elark-tasks--task-title '((name . "Deploy")))
                 "Deploy"))
  (should (equal (elark-tasks--task-title '((foo . "bar")))
                 "(no title)")))

(ert-deftest elark-tasks-test-task-status-todo ()
  "Default status is todo."
  (should (equal (elark-tasks--task-status '((summary . "x")))
                 "todo")))

(ert-deftest elark-tasks-test-task-status-done ()
  "Completed tasks return done."
  (should (equal (elark-tasks--task-status '((completed_at . "2026-01-01")))
                 "done"))
  (should (equal (elark-tasks--task-status '((is_completed . t)))
                 "done")))

(ert-deftest elark-tasks-test-task-status-done-zero-string ()
  "completed_at of \"0\" means not completed."
  (should (equal (elark-tasks--task-status '((completed_at . "0") (status . "todo")))
                 "todo")))

(ert-deftest elark-tasks-test-task-status-explicit ()
  "Explicit status field is used."
  (should (equal (elark-tasks--task-status '((status . "in_progress")))
                 "in_progress")))

(ert-deftest elark-tasks-test-task-due-due-at ()
  "due_at (ISO string from +get-my-tasks) is preferred."
  (should (equal (elark-tasks--task-due '((due_at . "2025-09-11T08:00:00+08:00")))
                 "2025-09-11")))

(ert-deftest elark-tasks-test-task-due-timestamp ()
  "Format numeric due timestamp."
  (let ((result (elark-tasks--task-due '((due . 1700000000)))))
    (should (stringp result))
    (should (string-match-p "^[0-9]\\{4\\}-" result))))

(ert-deftest elark-tasks-test-task-due-string ()
  "Format string due date."
  (should (equal (elark-tasks--task-due '((due . "2026-04-15")))
                 "2026-04-15")))

(ert-deftest elark-tasks-test-task-due-nil ()
  "Missing due returns empty string."
  (should (equal (elark-tasks--task-due '((summary . "x")))
                 "")))

(ert-deftest elark-tasks-test-task-creator ()
  "Extract creator name."
  (should (equal (elark-tasks--task-creator '((creator_name . "Alice")))
                 "Alice"))
  (should (equal (elark-tasks--task-creator
                  '((creator . ((display_name . "Bob")))))
                 "Bob"))
  (should (equal (elark-tasks--task-creator '((foo . "bar")))
                 "")))

(ert-deftest elark-tasks-test-task-url ()
  (should (equal (elark-tasks--task-url '((url . "https://example.com")))
                 "https://example.com"))
  (should (equal (elark-tasks--task-url '((foo . "bar"))) "")))

(ert-deftest elark-tasks-test-task-created-at ()
  (should (equal (elark-tasks--task-created-at '((created_at . "2026-02-13T14:32:17+08:00")))
                 "2026-02-13T14:32"))
  (should (equal (elark-tasks--task-created-at '((foo . "bar"))) "")))

(ert-deftest elark-tasks-test-task-origin ()
  (should (equal (elark-tasks--task-origin
                  '((origin . ((href . ((title . "My Doc")))))))
                 "My Doc"))
  (should (equal (elark-tasks--task-origin '((foo . "bar"))) "")))

(ert-deftest elark-tasks-test-task-subtask-count ()
  (should (equal (elark-tasks--task-subtask-count '((subtask_count . 3))) "3"))
  (should (equal (elark-tasks--task-subtask-count '((subtask_count . 0))) ""))
  (should (equal (elark-tasks--task-subtask-count '((foo . "bar"))) "")))

;;;; Extract tasks from various response shapes

(ert-deftest elark-tasks-test-extract-tasks-items ()
  "Extract tasks from {items: [...]}."
  (let ((data '((items . (((task_id . "1")) ((task_id . "2")))))))
    (should (= (length (elark-tasks--extract-tasks data)) 2))))

(ert-deftest elark-tasks-test-extract-tasks-nested ()
  "Extract tasks from {data: {tasks: [...]}}."
  (let ((data '((data . ((tasks . (((task_id . "1")))))))))
    (should (= (length (elark-tasks--extract-tasks data)) 1))))

(ert-deftest elark-tasks-test-extract-tasks-nested-guid ()
  "Extract tasks from {data: {items: [...]}} with guid."
  (let ((data '((data . ((items . (((guid . "g1") (summary . "A")))))))))
    (should (= (length (elark-tasks--extract-tasks data)) 1))))

(ert-deftest elark-tasks-test-extract-tasks-flat ()
  "Extract from flat list."
  (let ((data '(((task_id . "1") (summary . "A")))))
    (should (= (length (elark-tasks--extract-tasks data)) 1))))

(ert-deftest elark-tasks-test-extract-tasks-empty ()
  "Nil data returns nil."
  (should (null (elark-tasks--extract-tasks nil))))

;;;; Multi-line insert

(ert-deftest elark-tasks-test-insert-task ()
  "Insert task section with text properties."
  (let ((task '((guid . "g1")
                (summary . "Fix bug")
                (status . "todo")
                (due_at . "2026-04-15T08:00:00+08:00")
                (created_at . "2026-02-13T14:32:17+08:00")
                (url . "https://example.com/task/g1"))))
    (with-temp-buffer
      (elark-tasks--insert-task task)
      (goto-char (point-min))
      (should (search-forward "Fix bug" nil t))
      (should (search-forward "todo" nil t))
      (should (search-forward "2026-04-15" nil t))
      (should (search-forward "2026-02-13T14:32" nil t))
      (goto-char (point-min))
      (should (equal (get-text-property (point) 'elark-task-id) "g1")))))

;;;; Agenda entry insert

(ert-deftest elark-tasks-test-agenda-insert-entry ()
  "Insert agenda entry with TODO keyword."
  (let ((task '((guid . "g2")
                (summary . "Review PR")
                (status . "todo")
                (due_at . "2026-04-12T17:00:00+08:00"))))
    (with-temp-buffer
      (elark-tasks-agenda--insert-entry task 'today)
      (goto-char (point-min))
      (should (search-forward "TODO" nil t))
      (should (search-forward "Review PR" nil t))
      (should (search-forward "<2026-04-12>" nil t))
      (goto-char (point-min))
      (should (equal (get-text-property (point) 'elark-task-id) "g2")))))

(ert-deftest elark-tasks-test-agenda-insert-entry-done ()
  "Insert agenda entry with DONE keyword."
  (let ((task '((guid . "g3")
                (summary . "Deploy")
                (completed_at . "2026-04-10")
                (due_at . "2026-04-10T08:00:00+08:00"))))
    (with-temp-buffer
      (elark-tasks-agenda--insert-entry task 'today)
      (goto-char (point-min))
      (should (search-forward "DONE" nil t))
      (should (search-forward "Deploy" nil t)))))

;;;; Format date

(ert-deftest elark-tasks-test-format-date ()
  "Format date strings."
  (should (equal (elark-tasks--format-date "2026-04-15T10:00:00")
                 "2026-04-15"))
  (should (equal (elark-tasks--format-date "2026-04-15")
                 "2026-04-15"))
  (should (equal (elark-tasks--format-date nil) ""))
  (should (equal (elark-tasks--format-date "not a date") "not a date")))

(ert-deftest elark-tasks-test-extract-null-items ()
  "\"items\": null (no open tasks) extracts to nil, not `:null'.
Regression: the agenda crashed with \"Wrong type argument: listp,
:null\" when the user had no incomplete tasks."
  (should-not (elark-tasks--extract-tasks
               '((ok . t)
                 (data . ((has_more . :false)
                          (items . :null)
                          (page_token . "")))))))

(ert-deftest elark-tasks-test-agenda-render-empty ()
  "The agenda renders (not crashes) on the empty-tasks envelope."
  (cl-letf (((symbol-function 'pop-to-buffer)
             (lambda (buf &rest _) (set-buffer buf))))
    (elark-tasks-agenda--render
     '((ok . t)
       (data . ((has_more . :false) (items . :null) (page_token . "")))))
    (with-current-buffer "*Lark Task Agenda*"
      (should (string-match-p "nothing due today"
                              (buffer-substring-no-properties
                               (point-min) (point-max)))))))

(ert-deftest elark-tasks-test-members-null ()
  "A task with \"members\": null renders an empty assignee string."
  (should (equal "" (elark-tasks--task-members-string
                     '((summary . "t") (members . :null))))))

(provide 'elark-tasks-test)
;;; elark-tasks-test.el ends here
