;;; elark-tasks.el --- Lark Tasks integration -*- lexical-binding: t; -*-

;; Copyright (C) 2026 bbw9n

;; Author: bbw9n <bbw9nio@gmail.com>
;; Assisted-by: Claude:claude-opus-5
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Provides Task domain commands for elark: task listing, detail
;; view, create/update/complete tasks, subtask management, task list
;; (tasklist) management, and an org-agenda–style view.
;;
;; CLI command mapping:
;;   My tasks:     task +get-my-tasks [--query X] [--complete] [--due-start X]
;;                                    [--due-end X] [--created_at X]
;;   Create:       task +create --summary X [--description X] [--due X] [--assignee X]
;;   Update:       task +update --task-id X [--summary X] [--description X] [--due X]
;;   Complete:     task +complete --task-id X
;;   Reopen:       task +reopen --task-id X
;;   Get detail:   task tasks get --params '{"task_guid":"X"}'
;;   Delete:       task tasks delete --params '{"task_id":"X"}'
;;   Tasklists:    task tasklists list

;;; Code:

(require 'elark-core)
(require 'elark-ui)
(require 'elark-contact)
(require 'json)
(require 'transient)

;;;; Customization

(defgroup elark-tasks nil
  "Lark Tasks settings."
  :group 'elark
  :prefix "elark-tasks-")

;;;; Buffer-local variables

(defvar-local elark-tasks--items nil
  "Cached task data for the current buffer.")

(defvar-local elark-tasks--tasklist-id nil
  "Tasklist ID for the current buffer, or nil for all tasks.")

;;;; Task parsing

(defun elark-tasks--task-id (task)
  "Extract the task ID from TASK.
Prefers guid (from +get-my-tasks), falls back to task_id / id."
  (or (alist-get 'guid task)
      (alist-get 'task_id task)
      (alist-get 'id task)))

(defun elark-tasks--task-title (task)
  "Extract the title from TASK."
  (or (alist-get 'summary task)
      (alist-get 'title task)
      (alist-get 'name task)
      "(no title)"))

(defun elark-tasks--task-due (task)
  "Extract the due date from TASK as a display string."
  (let ((due (or (alist-get 'due_at task)
                 (alist-get 'due task)
                 (elark--get-nested task 'due 'timestamp)
                 (elark--get-nested task 'due 'date))))
    (cond
     ((numberp due) (or (elark--format-timestamp due) ""))
     ((stringp due) (elark-tasks--format-date due))
     (t ""))))

(defun elark-tasks--task-due-raw (task)
  "Extract the raw due date string from TASK (for sorting)."
  (or (alist-get 'due_at task)
      (alist-get 'due task)
      (let ((ts (or (elark--get-nested task 'due 'timestamp)
                     (elark--get-nested task 'due 'date))))
        (cond
         ((numberp ts) (or (elark--format-timestamp ts) ""))
         ((stringp ts) ts)
         (t "")))))

(defun elark-tasks--format-date (str)
  "Format a date STR for display (truncate to YYYY-MM-DD)."
  (if (and str (string-match-p "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}" str))
      (substring str 0 (min 10 (length str)))
    (or str "")))

(defun elark-tasks--format-datetime (str)
  "Format a datetime STR for display (truncate to YYYY-MM-DD HH:MM)."
  (if (and str (stringp str) (not (string-empty-p str)))
      (substring str 0 (min 16 (length str)))
    ""))

(defun elark-tasks--task-status (task)
  "Extract the completion status from TASK."
  (let ((completed (or (alist-get 'completed_at task)
                       (alist-get 'completed task)
                       (alist-get 'is_completed task))))
    (cond
     ((and completed (not (equal completed :false)) (not (eq completed 0))
           (not (equal completed "")) (not (equal completed "0")))
      "done")
     (t
      (or (alist-get 'status task) "todo")))))

(defun elark-tasks--task-creator (task)
  "Extract the creator name from TASK."
  (or (alist-get 'creator_name task)
      (elark--get-nested task 'creator 'display_name)
      (elark--get-nested task 'creator 'name)
      (let ((id (elark--get-nested task 'creator 'id)))
        (when (and id (stringp id) (not (string-empty-p id)))
          ;; Non-blocking: cached name or a tagged raw id that the
          ;; post-render `elark-contact-resolve-buffer-async' pass patches.
          (elark-contact-annotate id "open_id")))
      ""))

(defun elark-tasks--task-url (task)
  "Extract the URL from TASK."
  (or (alist-get 'url task) ""))

(defun elark-tasks--task-created-at (task)
  "Extract the created_at time from TASK."
  (let ((ts (alist-get 'created_at task)))
    (cond
     ((and (stringp ts) (not (string-empty-p ts)))
      (elark-tasks--format-datetime ts))
     ((numberp ts) (or (elark--format-timestamp ts) ""))
     (t ""))))

(defun elark-tasks--task-description (task)
  "Extract the description from TASK."
  (or (alist-get 'description task)
      (alist-get 'notes task)
      ""))

(defun elark-tasks--task-subtask-count (task)
  "Extract the subtask count from TASK."
  (let ((n (alist-get 'subtask_count task)))
    (if (and n (numberp n) (> n 0)) (format "%d" n) "")))

(defun elark-tasks--task-origin (task)
  "Extract the origin/source info from TASK."
  (or (elark--get-nested task 'origin 'href 'title) ""))

(defun elark-tasks--task-members-string (task)
  "Extract a string of assignee names from TASK members."
  (let ((members (or (elark--list-field task 'members)
                     (elark--list-field task 'assignees))))
    (if members
        (mapconcat
         (lambda (m)
           (let ((role (or (alist-get 'role m) ""))
                 (name (or (alist-get 'display_name m)
                           (alist-get 'name m)
                           (let ((id (alist-get 'id m)))
                             (when id (elark-contact-annotate id "open_id"))))))
             (if (equal role "assignee")
                 (or name "?")
               nil)))
         members ", ")
      "")))

(defun elark-tasks--extract-tasks (data)
  "Extract the task list from lark-cli response DATA.
Null-safe: an empty collection comes back as \"items\": null (the
`:null' sentinel), which must yield nil, not a truthy non-list."
  (or (elark--list-field data 'items)
      (elark--list-field data 'tasks)
      (let ((inner (elark--list-field data 'data)))
        (or (elark--list-field inner 'items)
            (elark--list-field inner 'tasks)
            (and (elark--record-list-p inner)
                 (or (alist-get 'task_id (car inner))
                     (alist-get 'guid (car inner))
                     (alist-get 'summary (car inner)))
                 inner)))
      (and (elark--record-list-p data)
           (or (alist-get 'task_id (car data))
               (alist-get 'guid (car data))
               (alist-get 'summary (car data)))
           data)))

;;;; Multi-line task list view

(defvar elark-tasks-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'elark-tasks-open)
    (define-key map (kbd "g")   #'elark-tasks-refresh)
    (define-key map (kbd "c")   #'elark-tasks-create)
    (define-key map (kbd "e")   #'elark-tasks-edit)
    (define-key map (kbd "x")   #'elark-tasks-complete)
    (define-key map (kbd "X")   #'elark-tasks-reopen)
    (define-key map (kbd "d")   #'elark-tasks-delete)
    (define-key map (kbd "y")   #'elark-tasks-copy-id)
    (define-key map (kbd "s")   #'elark-tasks-subtask-create)
    (define-key map (kbd "n")   #'elark-tasks--next-task)
    (define-key map (kbd "p")   #'elark-tasks--prev-task)
    (define-key map (kbd "A")   #'elark-tasks-agenda)
    (define-key map (kbd "?")   #'elark-tasks-dispatch)
    map)
  "Keymap for `elark-tasks-mode'.")

(define-derived-mode elark-tasks-mode special-mode
  "Lark Tasks"
  "Major mode for browsing Lark tasks.
Each task is displayed as a multi-line section.")

;;;; AI context provider — shared by both task list and agenda modes

(defun elark-tasks--ai-context ()
  "Return the AI context plist for a tasks buffer."
  (let ((items elark-tasks--items)
        (task-id (get-text-property (point) 'elark-task-id)))
    (list :domain "tasks"
          :buffer-type "task-list"
          :item (when task-id
                  (list :task-id task-id
                        :task (seq-find (lambda (tk)
                                          (equal (alist-get 'guid tk) task-id))
                                        items)))
          :summary (format "Task list with %d tasks%s"
                           (length items)
                           (if task-id
                               (format ", cursor on task %s" task-id)
                             "")))))

(put 'elark-tasks-mode 'elark-ai-context-provider #'elark-tasks--ai-context)

(defun elark-tasks--task-id-at-point ()
  "Return the task ID at point, or nil."
  (get-text-property (point) 'elark-task-id))

(defun elark-tasks--insert-field (label value)
  "Insert a LABEL: VALUE line if VALUE is non-empty."
  (elark-ui-insert-field label value))

(defun elark-tasks--status-face (status)
  "Return a face for task STATUS."
  (pcase status
    ("done" 'success)
    ("todo" 'warning)
    ("in_progress" 'font-lock-constant-face)
    (_ 'default)))

(defun elark-tasks--insert-task (task)
  "Insert a multi-line section for TASK into the current buffer."
  (let ((id (elark-tasks--task-id task))
        (title (elark-tasks--task-title task))
        (status (elark-tasks--task-status task))
        (due (elark-tasks--task-due task))
        (created (elark-tasks--task-created-at task))
        (url (elark-tasks--task-url task))
        (origin (elark-tasks--task-origin task))
        (beg (point)))
    (insert (propertize (if (equal status "done") "[x] " "[ ] ")
                        'face (elark-tasks--status-face status))
            (propertize title 'face 'bold) "\n")
    (elark-tasks--insert-field "Status" (propertize status 'face (elark-tasks--status-face status)))
    (elark-tasks--insert-field "Due" due)
    (elark-tasks--insert-field "Created" created)
    (elark-tasks--insert-field "Origin" origin)
    (elark-tasks--insert-field "URL" url)
    (insert "\n")
    (put-text-property beg (point) 'elark-task-id id)))

(defun elark-tasks--next-task ()
  "Move to the next task section."
  (interactive)
  (elark-ui-next-section 'elark-task-id))

(defun elark-tasks--prev-task ()
  "Move to the previous task section."
  (interactive)
  (elark-ui-prev-section 'elark-task-id))

;;;; Task listing
;; CLI: task +get-my-tasks [--query X] [--complete]

;;;###autoload
(defun elark-tasks-list (&optional show-completed)
  "List Lark tasks assigned to me.
With prefix argument SHOW-COMPLETED, include completed tasks."
  (interactive "P")
  (message "Lark: fetching tasks...")
  (let ((args (list "task" "+get-my-tasks" "--page-all")))
    (unless show-completed
      (setq args (append args (list "--complete=false"))))
    (elark--run-command
     args
     (lambda (data)
       (elark-tasks--display-list data nil)))))

(defun elark-tasks-refresh ()
  "Refresh the current task list buffer."
  (interactive)
  (elark-tasks-list))

(defun elark-tasks--display-list (data &optional tasklist-id)
  "Display task list DATA for TASKLIST-ID."
  (let* ((tasks (elark-tasks--extract-tasks data))
         (buf (get-buffer-create (if tasklist-id
                                     (format "*Lark Tasks: %s*" tasklist-id)
                                   "*Lark Tasks*"))))
    (with-current-buffer buf
      (elark-tasks-mode)
      (setq elark-tasks--items tasks
            elark-tasks--tasklist-id tasklist-id)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (if (null tasks)
            (insert "(no tasks)\n")
          (dolist (task tasks)
            (elark-tasks--insert-task task))))
      (goto-char (point-min))
      (setq header-line-format
            (format " Lark Tasks — %d task(s)" (length tasks))))
    (pop-to-buffer buf)
    ;; Creator/assignee names resolve in the background (see
    ;; `elark-contact-annotate') — rendering never blocks on the network.
    (elark-contact-resolve-buffer-async buf)))

;;;; Task detail
;; CLI: task tasks get --params '{"task_guid":"X"}'

(defun elark-tasks-open ()
  "Open the task at point in a detail view."
  (interactive)
  (let ((id (elark-tasks--task-id-at-point)))
    (unless id (user-error "No task at point"))
    (message "Lark: fetching task details...")
    (let ((params (json-encode `((task_guid . ,id)))))
      (elark--run-command
       (list "task" "tasks" "get" "--params" params)
       (lambda (data)
         (elark-tasks--display-detail data id))))))

(defun elark-tasks--display-detail (data task-id)
  "Display task detail DATA for TASK-ID."
  (let* ((raw (or (alist-get 'data data) data))
         (task (or (alist-get 'task raw) raw))
         (title (elark-tasks--task-title task))
         (buf (get-buffer-create (format "*Lark Task: %s*" title))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (elark-ui-insert-title title)
        ;; Status
        (let ((status (elark-tasks--task-status task)))
          (insert (propertize "Status: " 'face 'bold)
                  (propertize status 'face (elark-tasks--status-face status)) "\n"))
        ;; Due date
        (let ((due (elark-tasks--task-due task)))
          (unless (string-empty-p due)
            (insert (propertize "Due: " 'face 'bold) due "\n")))
        ;; Created
        (let ((created (elark-tasks--task-created-at task)))
          (unless (string-empty-p created)
            (insert (propertize "Created: " 'face 'bold) created "\n")))
        ;; Creator
        (let ((creator (elark-tasks--task-creator task)))
          (unless (string-empty-p creator)
            (insert (propertize "Creator: " 'face 'bold) creator "\n")))
        ;; Assignees
        (let ((assignees (elark-tasks--task-members-string task)))
          (unless (string-empty-p assignees)
            (insert (propertize "Assignees: " 'face 'bold) assignees "\n")))
        ;; Subtask count
        (let ((sc (elark-tasks--task-subtask-count task)))
          (unless (string-empty-p sc)
            (insert (propertize "Subtasks: " 'face 'bold) sc "\n")))
        ;; Origin
        (let ((origin (elark-tasks--task-origin task)))
          (unless (string-empty-p origin)
            (insert (propertize "Origin: " 'face 'bold) origin "\n")))
        ;; URL
        (let ((url (elark-tasks--task-url task)))
          (unless (string-empty-p url)
            (insert (propertize "URL: " 'face 'bold) url "\n")))
        ;; Description
        (let ((desc (elark-tasks--task-description task)))
          (unless (string-empty-p desc)
            (insert "\n" (propertize "Description" 'face 'bold) "\n"
                    (elark-ui-separator 40) "\n"
                    desc "\n")))
        ;; Subtasks
        (let ((subtasks (or (alist-get 'subtasks task)
                            (alist-get 'children task))))
          (when subtasks
            (insert "\n" (propertize "Subtasks" 'face 'bold) "\n"
                    (elark-ui-separator 40) "\n")
            (dolist (st subtasks)
              (let ((st-title (elark-tasks--task-title st))
                    (st-status (elark-tasks--task-status st)))
                (insert (if (equal st-status "done") "  [x] " "  [ ] ")
                        st-title "\n")))))
        ;; Task ID
        (insert "\n" (propertize "Task ID: " 'face 'font-lock-comment-face)
                (or task-id "") "\n"))
      (special-mode)
      (goto-char (point-min)))
    (pop-to-buffer buf)
    (elark-contact-resolve-buffer-async buf)))

;;;; Task creation
;; CLI: task +create --summary X [--description X] [--due X] [--assignee X]
;;                   [--tasklist-id X]

;;;###autoload (autoload 'elark-tasks-create "elark-tasks" nil t)
(transient-define-prefix elark-tasks-create ()
  "Create a new Lark task."
  ["Task Details"
   ("t" "Title"       "--summary=" :prompt "Task title: ")
   ("d" "Due date"    "--due=" :prompt "Due (YYYY-MM-DD or ISO 8601): ")
   ("n" "Description" "--description=" :prompt "Description: ")]
  ["Assignment"
   ("a" "Assignee"    "--assignee=" :prompt "Assignee open_id (ou_): ")
   ("l" "Tasklist"    "--tasklist-id=" :prompt "Tasklist ID: ")]
  ["Actions"
   ("RET" "Create"    elark-tasks--do-create)
   ("q"   "Cancel"    transient-quit-all)])

(defun elark-tasks--do-create (&rest _args)
  "Execute task creation with transient arguments."
  (interactive)
  (let ((args (transient-args 'elark-tasks-create)))
    (unless args
      (user-error "No task details provided"))
    (message "Lark: creating task...")
    (elark--run-command
     (append '("task" "+create") args)
     (lambda (data)
       (let ((id (or (elark--get-nested data 'data 'task_id)
                     (alist-get 'task_id data)
                     (alist-get 'id data))))
         (message "Lark: task created%s"
                  (if id (format " (ID: %s)" id) "")))))))

;;;; Task update
;; CLI: task +update --task-id X [--summary X] [--description X] [--due X]

(defun elark-tasks-edit ()
  "Edit the task at point (update title)."
  (interactive)
  (let ((id (elark-tasks--task-id-at-point)))
    (unless id (user-error "No task at point"))
    (let ((new-title (read-string "New title: ")))
      (when (string-empty-p new-title)
        (user-error "Title cannot be empty"))
      (message "Lark: updating task...")
      (elark--run-command
       (list "task" "+update" "--task-id" id "--summary" new-title)
       (lambda (_data)
         (message "Lark: task updated")
         (elark-tasks-refresh))))))

;;;; Task completion / reopen
;; CLI: task +complete --task-id X
;; CLI: task +reopen --task-id X

(defun elark-tasks-complete ()
  "Mark the task at point as complete."
  (interactive)
  (let ((id (elark-tasks--task-id-at-point)))
    (unless id (user-error "No task at point"))
    (message "Lark: completing task...")
    (elark--run-command
     (list "task" "+complete" "--task-id" id)
     (lambda (_data)
       (message "Lark: task completed")
       (elark-tasks-refresh)))))

(defun elark-tasks-reopen ()
  "Reopen the completed task at point."
  (interactive)
  (let ((id (elark-tasks--task-id-at-point)))
    (unless id (user-error "No task at point"))
    (message "Lark: reopening task...")
    (elark--run-command
     (list "task" "+reopen" "--task-id" id)
     (lambda (_data)
       (message "Lark: task reopened")
       (elark-tasks-refresh)))))

;;;; Task deletion
;; CLI: task tasks delete --params '{"task_id":"X"}'

(defun elark-tasks-delete ()
  "Delete the task at point."
  (interactive)
  (let ((id (elark-tasks--task-id-at-point)))
    (unless id (user-error "No task at point"))
    (when (yes-or-no-p (format "Delete task %s? " id))
      (message "Lark: deleting task...")
      (let ((params (json-encode `((task_id . ,id)))))
        (elark--run-command
         (list "task" "tasks" "delete" "--params" params)
         (lambda (_data)
           (message "Lark: task deleted")
           (elark-tasks-refresh)))))))

;;;; Copy task ID

(defun elark-tasks-copy-id ()
  "Copy the task ID at point to the kill ring."
  (interactive)
  (let ((id (elark-tasks--task-id-at-point)))
    (unless id (user-error "No task at point"))
    (kill-new id)
    (message "Copied: %s" id)))

;;;; Subtask creation
;; task subtasks create --params '{"task_id":"PARENT"}' --data '{"summary":"X"}'

(defun elark-tasks-subtask-create ()
  "Create a subtask under the task at point."
  (interactive)
  (let ((parent-id (elark-tasks--task-id-at-point)))
    (unless parent-id (user-error "No task at point"))
    (let ((title (read-string "Subtask title: ")))
      (when (string-empty-p title)
        (user-error "Title cannot be empty"))
      (message "Lark: creating subtask...")
      (let ((params (json-encode `((task_id . ,parent-id))))
            (body (json-encode `((summary . ,title)))))
        (elark--run-command
         (list "task" "subtasks" "create"
               "--params" params "--data" body)
         (lambda (data)
           (let ((id (or (elark--get-nested data 'data 'task_id)
                         (alist-get 'task_id data))))
             (message "Lark: subtask created%s"
                      (if id (format " (ID: %s)" id) "")))))))))

;;;; Tasklist management
;; CLI: task tasklists list

;;;###autoload
(defun elark-tasks-tasklists ()
  "List available task lists."
  (interactive)
  (message "Lark: fetching task lists...")
  (elark--run-command
   '("task" "tasklists" "list")
   #'elark-tasks--display-tasklists))

(defun elark-tasks--display-tasklists (data)
  "Display tasklist DATA."
  (let* ((lists (or (alist-get 'items data)
                    (elark--get-nested data 'data 'items)
                    (alist-get 'tasklists data)
                    (and (listp data) data)))
         (buf (get-buffer-create "*Lark Task Lists*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize "Lark Task Lists\n" 'face 'bold)
                (elark-ui-separator 60) "\n\n")
        (if (null lists)
            (insert "(no task lists found)\n")
          (dolist (tl lists)
            (let ((id (or (alist-get 'tasklist_id tl) (alist-get 'id tl)))
                  (name (or (alist-get 'name tl) (alist-get 'title tl) "")))
              (insert (propertize name 'face 'bold)
                      (format "  (ID: %s)\n" (or id "")))))))
      (special-mode)
      (goto-char (point-min)))
    (pop-to-buffer buf)))

;;; ================================================================
;;;; Org-agenda–style view
;;; ================================================================

(defvar elark-tasks-agenda-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'elark-tasks-agenda-open)
    (define-key map (kbd "g")   #'elark-tasks-agenda-refresh)
    (define-key map (kbd "x")   #'elark-tasks-agenda-complete)
    (define-key map (kbd "X")   #'elark-tasks-agenda-reopen)
    (define-key map (kbd "n")   #'elark-tasks-agenda--next)
    (define-key map (kbd "p")   #'elark-tasks-agenda--prev)
    (define-key map (kbd "?")   #'elark-tasks-dispatch)
    map)
  "Keymap for `elark-tasks-agenda-mode'.")

(define-derived-mode elark-tasks-agenda-mode special-mode
  "Lark Agenda"
  "Major mode for the Lark Tasks agenda-style view.")

(put 'elark-tasks-agenda-mode 'elark-ai-context-provider
     #'elark-tasks--ai-context)

(defvar-local elark-tasks-agenda--items nil
  "Cached tasks for the agenda buffer.")

;;;###autoload
(defun elark-tasks-agenda ()
  "Display Lark tasks in an agenda-style view.
Shows incomplete tasks sorted by due date."
  (interactive)
  (message "Lark: building task agenda...")
  (elark--run-command
   (list "task" "+get-my-tasks" "--page-all" "--complete=false")
   #'elark-tasks-agenda--render))

(defun elark-tasks-agenda-refresh ()
  "Refresh the task agenda."
  (interactive)
  (elark-tasks-agenda))

(defun elark-tasks-agenda--render (data)
  "Render the agenda-style view from DATA."
  (let* ((tasks (elark-tasks--extract-tasks data))
         (today (format-time-string "%Y-%m-%d"))
         ;; Partition into overdue, today, upcoming (has due), and no-due
         (with-due (seq-filter (lambda (tk) (not (string-empty-p (elark-tasks--task-due-raw tk)))) tasks))
         (no-due (seq-filter (lambda (tk) (string-empty-p (elark-tasks--task-due-raw tk))) tasks))
         ;; Sort by due date
         (sorted (sort with-due
                       (lambda (a b)
                         (string< (elark-tasks--task-due-raw a)
                                  (elark-tasks--task-due-raw b)))))
         (overdue (seq-filter (lambda (tk) (string< (elark-tasks--format-date (elark-tasks--task-due-raw tk)) today)) sorted))
         (due-today (seq-filter (lambda (tk) (equal (elark-tasks--format-date (elark-tasks--task-due-raw tk)) today)) sorted))
         (upcoming (seq-filter (lambda (tk) (string> (elark-tasks--format-date (elark-tasks--task-due-raw tk)) today)) sorted))
         (buf (get-buffer-create "*Lark Task Agenda*")))
    (with-current-buffer buf
      (elark-tasks-agenda-mode)
      (setq elark-tasks-agenda--items tasks)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize (format "Lark Task Agenda  %s\n" today) 'face 'bold)
                (elark-ui-separator 60) "\n")
        ;; Overdue
        (when overdue
          (insert "\n" (propertize "Overdue" 'face '(bold error)) "\n")
          (dolist (task overdue)
            (elark-tasks-agenda--insert-entry task 'overdue)))
        ;; Today
        (insert "\n" (propertize "Today" 'face '(bold warning)) "\n")
        (if due-today
            (dolist (task due-today)
              (elark-tasks-agenda--insert-entry task 'today))
          (insert (propertize "  (nothing due today)\n" 'face 'font-lock-comment-face)))
        ;; Upcoming
        (when upcoming
          (insert "\n" (propertize "Upcoming" 'face '(:weight bold)) "\n")
          (dolist (task upcoming)
            (elark-tasks-agenda--insert-entry task 'upcoming)))
        ;; No due date
        (when no-due
          (insert "\n" (propertize "No Due Date" 'face 'font-lock-comment-face) "\n")
          (dolist (task no-due)
            (elark-tasks-agenda--insert-entry task 'no-due))))
      (goto-char (point-min))
      (setq header-line-format
            (format " Lark Task Agenda — %d task(s)" (length tasks))))
    (pop-to-buffer buf)))

(defun elark-tasks-agenda--insert-entry (task category)
  "Insert a single-line agenda entry for TASK.
CATEGORY is one of overdue, today, upcoming, no-due."
  (let* ((id (elark-tasks--task-id task))
         (title (elark-tasks--task-title task))
         (status (elark-tasks--task-status task))
         (due (elark-tasks--task-due task))
         (keyword (if (equal status "done") "DONE" "TODO"))
         (due-face (pcase category
                     ('overdue 'error)
                     ('today 'warning)
                     ('upcoming 'default)
                     ('no-due 'font-lock-comment-face)))
         (beg (point)))
    (insert "  "
            (propertize (format "%-5s" keyword) 'face
                        (if (equal keyword "DONE") 'success 'org-todo))
            " "
            (propertize title 'face (if (equal keyword "DONE")
                                        '(:strike-through t)
                                      'default)))
    (when (and due (not (string-empty-p due)))
      (insert "  "
              (propertize (format "<%s>" due) 'face due-face)))
    (insert "\n")
    (put-text-property beg (point) 'elark-task-id id)))

(defun elark-tasks-agenda--task-id-at-point ()
  "Return the task ID at point in the agenda buffer."
  (get-text-property (point) 'elark-task-id))

(defun elark-tasks-agenda-open ()
  "Open the task at point in the agenda view."
  (interactive)
  (let ((id (elark-tasks-agenda--task-id-at-point)))
    (unless id (user-error "No task at point"))
    (message "Lark: fetching task details...")
    (let ((params (json-encode `((task_guid . ,id)))))
      (elark--run-command
       (list "task" "tasks" "get" "--params" params)
       (lambda (data)
         (elark-tasks--display-detail data id))))))

(defun elark-tasks-agenda-complete ()
  "Complete the task at point in the agenda view."
  (interactive)
  (let ((id (elark-tasks-agenda--task-id-at-point)))
    (unless id (user-error "No task at point"))
    (message "Lark: completing task...")
    (elark--run-command
     (list "task" "+complete" "--task-id" id)
     (lambda (_data)
       (message "Lark: task completed")
       (elark-tasks-agenda-refresh)))))

(defun elark-tasks-agenda-reopen ()
  "Reopen the task at point in the agenda view."
  (interactive)
  (let ((id (elark-tasks-agenda--task-id-at-point)))
    (unless id (user-error "No task at point"))
    (message "Lark: reopening task...")
    (elark--run-command
     (list "task" "+reopen" "--task-id" id)
     (lambda (_data)
       (message "Lark: task reopened")
       (elark-tasks-agenda-refresh)))))

(defun elark-tasks-agenda--next ()
  "Move to the next task entry in the agenda."
  (interactive)
  (let ((pos (point)))
    (forward-line 1)
    (while (and (not (eobp))
                (not (get-text-property (point) 'elark-task-id)))
      (forward-line 1))
    (when (eobp) (goto-char pos))))

(defun elark-tasks-agenda--prev ()
  "Move to the previous task entry in the agenda."
  (interactive)
  (let ((pos (point)))
    (forward-line -1)
    (while (and (not (bobp))
                (not (get-text-property (point) 'elark-task-id)))
      (forward-line -1))
    (unless (get-text-property (point) 'elark-task-id)
      (goto-char pos))))

;;;; Transient dispatch

;;;###autoload (autoload 'elark-tasks-dispatch "elark-tasks" nil t)
(transient-define-prefix elark-tasks-dispatch ()
  "Lark Tasks commands."
  ["Quick Actions"
   ("l" "My tasks"       elark-tasks-list)
   ("A" "Agenda view"    elark-tasks-agenda)
   ("c" "Create task"    elark-tasks-create)
   ("x" "Complete task"  elark-tasks-complete)
   ("X" "Reopen task"    elark-tasks-reopen)]
  ["Browse"
   ("t" "Task lists"     elark-tasks-tasklists)]
  ["At Point"
   ("RET" "Open"         elark-tasks-open)
   ("e"   "Edit"         elark-tasks-edit)
   ("s"   "Add subtask"  elark-tasks-subtask-create)
   ("d"   "Delete"       elark-tasks-delete)])

(provide 'elark-tasks)
;;; elark-tasks.el ends here
