;;; elark-meetings.el --- Lark Meetings (VC) integration -*- lexical-binding: t; -*-

;; Copyright (C) 2026 bbw9n

;; Author: bbw9n <bbw9nio@gmail.com>
;; Assisted-by: Claude:claude-opus-5
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Provides Meeting / Video Conference domain commands for elark:
;; meeting search, meeting detail, meeting notes, and recordings.
;;
;; The default search window is last week to tomorrow.
;;
;; CLI command mapping:
;;   Search:    vc +search --start X --end X [--query X] [--organizer-ids X]
;;                         [--participant-ids X] [--room-ids X]
;;   Notes:     vc +notes --meeting-ids X | --minute-tokens X | --calendar-event-ids X
;;   Recording: vc +recording --meeting-ids X | --calendar-event-ids X
;;   Detail:    vc meeting get --params '{"meeting_id":"X"}'

;;; Code:

(require 'elark-core)
(require 'elark-ui)
(require 'json)
(require 'transient)

;;;; Customization

(defgroup elark-meetings nil
  "Lark Meetings settings."
  :group 'elark
  :prefix "elark-meetings-")

(defcustom elark-meetings-search-page-size "15"
  "Default page size for meeting search."
  :type 'string
  :group 'elark-meetings)

;;;; Buffer-local variables

(defvar-local elark-meetings--items nil
  "Cached meeting list for the current buffer.")

(defvar-local elark-meetings--search-start nil
  "Last search start time.")

(defvar-local elark-meetings--search-end nil
  "Last search end time.")

(defvar-local elark-meetings--search-query nil
  "Last search query keyword.")

;;;; Time helpers

(defun elark-meetings--default-start ()
  "Return the default search start: 7 days ago as YYYY-MM-DD."
  (format-time-string "%Y-%m-%d" (time-subtract nil (* 7 86400))))

(defun elark-meetings--default-end ()
  "Return the default search end: tomorrow as YYYY-MM-DD."
  (format-time-string "%Y-%m-%d" (time-add nil 86400)))

;;;; Meeting field extractors

(defun elark-meetings--meeting-id (meeting)
  "Extract the meeting ID from MEETING."
  (or (alist-get 'id meeting)
      (alist-get 'meeting_id meeting)
      ""))

(defun elark-meetings--meeting-title (meeting)
  "Extract a display title from MEETING.
Uses the first line of display_info, falling back to description."
  (let ((info (alist-get 'display_info meeting)))
    (if (and info (stringp info) (not (string-empty-p info)))
        (car (split-string info "\n" t))
      (or (elark--get-nested meeting 'meta_data 'description)
          (alist-get 'topic meeting)
          "(untitled meeting)"))))

(defun elark-meetings--meeting-description (meeting)
  "Extract the description line from MEETING."
  (or (elark--get-nested meeting 'meta_data 'description)
      ""))

(defun elark-meetings--meeting-app-link (meeting)
  "Extract the app link from MEETING."
  (or (elark--get-nested meeting 'meta_data 'app_link)
      ""))

(defun elark-meetings--meeting-display-info (meeting)
  "Extract the full display_info from MEETING."
  (or (alist-get 'display_info meeting) ""))

;;;; Response extraction

(defun elark-meetings--extract-meetings (data)
  "Extract meeting list from lark-cli search response DATA."
  (or (elark--get-nested data 'data 'items)
      (elark--list-field data 'items)
      (elark--get-nested data 'data 'meetings)))

(defun elark-meetings--extract-notes (data)
  "Extract notes list from lark-cli notes response DATA."
  (or (elark--get-nested data 'data 'notes)
      (elark--list-field data 'notes)))

;;;; Meeting list mode (multi-line sections)

(defvar elark-meetings-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'elark-meetings-open)
    (define-key map (kbd "g")   #'elark-meetings-search-refresh)
    (define-key map (kbd "N")   #'elark-meetings-notes-at-point)
    (define-key map (kbd "y")   #'elark-meetings-copy-id)
    (define-key map (kbd "w")   #'elark-meetings-copy-link)
    (define-key map (kbd "n")   #'elark-meetings--next)
    (define-key map (kbd "p")   #'elark-meetings--prev)
    (define-key map (kbd "?")   #'elark-meetings-dispatch)
    map)
  "Keymap for `elark-meetings-mode'.")

(define-derived-mode elark-meetings-mode special-mode
  "Lark Meetings"
  "Major mode for browsing Lark meeting search results.
Each meeting is displayed as a multi-line section.")

;;;; AI context provider

(defun elark-meetings--ai-context ()
  "Return the AI context plist for a meetings buffer."
  (let ((items elark-meetings--items)
        (meeting-id (get-text-property (point) 'elark-meeting-id)))
    (list :domain "meetings"
          :buffer-type "meeting-list"
          :item (when meeting-id (list :meeting-id meeting-id))
          :summary (format "Meeting list with %d meetings"
                           (length items)))))

(put 'elark-meetings-mode 'elark-ai-context-provider
     #'elark-meetings--ai-context)

(defun elark-meetings--meeting-id-at-point ()
  "Return the meeting ID at point, or nil."
  (get-text-property (point) 'elark-meeting-id))

(defun elark-meetings--meeting-at-point ()
  "Return the meeting alist at point, or nil."
  (get-text-property (point) 'elark-meeting))

(defun elark-meetings--insert-field (label value)
  "Insert LABEL: VALUE if VALUE is non-empty."
  (elark-ui-insert-field label value 18))

(defun elark-meetings--insert-meeting (meeting)
  "Insert a multi-line section for MEETING."
  (let ((id (elark-meetings--meeting-id meeting))
        (title (elark-meetings--meeting-title meeting))
        (desc (elark-meetings--meeting-description meeting))
        (link (elark-meetings--meeting-app-link meeting))
        (beg (point)))
    (insert (propertize title 'face 'bold) "\n")
    (elark-meetings--insert-field "Description" desc)
    (elark-meetings--insert-field "Meeting ID" id)
    (elark-meetings--insert-field "Link" link)
    (insert "\n")
    (put-text-property beg (point) 'elark-meeting-id id)
    (put-text-property beg (point) 'elark-meeting meeting)))

(defun elark-meetings--next ()
  "Move to the next meeting section."
  (interactive)
  (elark-ui-next-section 'elark-meeting-id))

(defun elark-meetings--prev ()
  "Move to the previous meeting section."
  (interactive)
  (elark-ui-prev-section 'elark-meeting-id))

;;;; Search
;; CLI: vc +search --start X --end X [--query X] [--page-size N]

;;;###autoload
(defun elark-meetings-search (&optional query start end)
  "Search meeting records.
QUERY is an optional keyword.  START and END default to last
week through tomorrow."
  (interactive
   (list (read-string "Search meetings (keyword, optional): ")
         (read-string (format "Start date [%s]: " (elark-meetings--default-start)))
         (read-string (format "End date [%s]: " (elark-meetings--default-end)))))
  (let ((start (if (or (null start) (string-empty-p start))
                   (elark-meetings--default-start) start))
        (end (if (or (null end) (string-empty-p end))
                 (elark-meetings--default-end) end)))
    (message "Lark: searching meetings (%s to %s)..." start end)
    (let ((args (list "vc" "+search"
                      "--start" start
                      "--end" end
                      "--page-size" elark-meetings-search-page-size))
          (q (unless (string-empty-p (or query "")) query)))
      (when q
        (setq args (append args (list "--query" q))))
      (elark--run-command
       args
       (lambda (data)
         (elark-meetings--display-search data start end q))))))

(defun elark-meetings-search-refresh ()
  "Refresh meeting search with previous parameters."
  (interactive)
  (elark-meetings-search
   elark-meetings--search-query
   elark-meetings--search-start
   elark-meetings--search-end))

(defun elark-meetings--display-search (data start end &optional query)
  "Display search results DATA.  Store START, END, QUERY for refresh."
  (let* ((meetings (elark-meetings--extract-meetings data))
         (buf (get-buffer-create "*Lark Meetings*")))
    (with-current-buffer buf
      (elark-meetings-mode)
      (setq elark-meetings--items meetings
            elark-meetings--search-start start
            elark-meetings--search-end end
            elark-meetings--search-query query)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize (format "Meetings: %s → %s" start end) 'face 'bold)
                (if query (format "  [%s]" query) "")
                "\n"
                (elark-ui-separator 60) "\n\n")
        (if (null meetings)
            (insert "(no meetings found)\n")
          (dolist (m meetings)
            (elark-meetings--insert-meeting m))))
      (goto-char (point-min))
      (setq header-line-format
            (format " Lark Meetings — %d result(s)" (length meetings))))
    (pop-to-buffer buf)))

;;;; Meeting detail
;; CLI: vc meeting get --params '{"meeting_id":"X"}'

(defvar-local elark-meetings--detail-meeting nil
  "The meeting alist shown in this detail buffer.")

(defvar-local elark-meetings--detail-transcripts nil
  "Alist of (MINUTE-TOKEN . TRANSCRIPT-TEXT) fetched for this detail buffer.")

(define-derived-mode elark-meetings-detail-mode special-mode
  "Lark Meeting"
  "Major mode for a single Lark meeting detail buffer.")

(defun elark-meetings--detail-ai-context ()
  "Return the AI context plist for a meeting detail buffer.
Exposes the meeting's basic meta info in :summary and the full
rendered buffer (meta, recording info, and any fetched transcripts)
as :content so the LLM can use the transcript without re-fetching."
  (let* ((meeting elark-meetings--detail-meeting)
         (id (elark-meetings--meeting-id meeting))
         (title (elark-meetings--meeting-title meeting))
         (transcripts elark-meetings--detail-transcripts))
    (list :domain "meetings"
          :buffer-type "meeting-detail"
          :item (list :meeting-id id
                      :title title
                      :has-transcript (and transcripts t))
          :summary (format "Meeting detail: %s (ID: %s)%s"
                           title id
                           (if transcripts " — transcript fetched" ""))
          :content (buffer-substring-no-properties (point-min) (point-max)))))

(put 'elark-meetings-detail-mode 'elark-ai-context-provider
     #'elark-meetings--detail-ai-context)

(defun elark-meetings-open ()
  "Open the meeting at point — show full display info.
Automatically queries for recordings and appends them if found,
then downloads and renders the transcript for each minute token."
  (interactive)
  (let ((meeting (elark-meetings--meeting-at-point)))
    (unless meeting (user-error "No meeting at point"))
    (let* ((id (elark-meetings--meeting-id meeting))
           (title (elark-meetings--meeting-title meeting))
           (info (elark-meetings--meeting-display-info meeting))
           (desc (elark-meetings--meeting-description meeting))
           (link (elark-meetings--meeting-app-link meeting))
           (buf (get-buffer-create (format "*Lark Meeting: %s*" title))))
      (with-current-buffer buf
        (elark-meetings-detail-mode)
        (setq elark-meetings--detail-meeting meeting)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (elark-ui-insert-title title)
          (elark-meetings--insert-field "Meeting ID" id)
          (elark-meetings--insert-field "Description" desc)
          (elark-meetings--insert-field "Link" link)
          (when (and info (not (string-empty-p info)))
            (insert "\n" (propertize "Full Info" 'face 'bold) "\n"
                    (elark-ui-separator 40) "\n"
                    info "\n")))
        (goto-char (point-min)))
      (pop-to-buffer buf)
      ;; Async: check for recording and append if found
      (when (and id (not (string-empty-p id)))
        (elark--run-command
         (list "vc" "+recording" "--meeting-ids" id)
         (lambda (data)
           (let* ((items (or (elark--get-nested data 'data 'items)
                             (elark--get-nested data 'data 'recordings)
                             (alist-get 'data data)))
                  (items (if (and items (listp items) (not (listp (car items))))
                             (list items)
                           items)))
             (when items
               (when (buffer-live-p buf)
                 (with-current-buffer buf
                   (let ((inhibit-read-only t))
                     (goto-char (point-max))
                     (insert "\n" (propertize "Recording" 'face 'bold) "\n"
                             (elark-ui-separator 40) "\n\n")
                     (let ((idx 0))
                       (dolist (rec items)
                         (when (> idx 0)
                           (insert "\n" (elark-ui-separator 40) "\n\n"))
                         (elark-meetings--insert-recording rec)
                         (setq idx (1+ idx))))))
                 ;; Async: fetch and append transcripts for each minute_token
                 (dolist (tok (elark-meetings--minute-tokens items))
                   (elark-meetings--fetch-transcript tok buf))))))
         nil :no-error t)))))

;;;; Meeting notes
;; CLI: vc +notes --meeting-ids X

(defun elark-meetings-notes-at-point ()
  "Fetch notes for the meeting at point."
  (interactive)
  (let ((id (elark-meetings--meeting-id-at-point)))
    (unless id (user-error "No meeting at point"))
    (elark-meetings-notes id)))

;;;###autoload
(defun elark-meetings-notes (meeting-id)
  "Fetch and display notes for MEETING-ID."
  (interactive "sMeeting ID: ")
  (when (string-empty-p meeting-id)
    (user-error "Meeting ID is required"))
  (message "Lark: fetching meeting notes...")
  (elark--run-command
   (list "vc" "+notes" "--meeting-ids" meeting-id)
   (lambda (data)
     (elark-meetings--display-notes data meeting-id))
   nil :no-error t))

(defun elark-meetings--display-notes (data meeting-id)
  "Display meeting notes DATA for MEETING-ID."
  (let* ((notes (elark-meetings--extract-notes data))
         (buf (get-buffer-create (format "*Lark Meeting Notes: %s*" meeting-id))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize (format "Meeting Notes: %s" meeting-id) 'face 'bold) "\n"
                (elark-ui-separator 60) "\n\n")
        (if (null notes)
            (insert "(no notes available)\n")
          (dolist (note notes)
            (let ((err (alist-get 'error note))
                  (mid (or (alist-get 'meeting_id note) ""))
                  (summary (or (alist-get 'summary note) ""))
                  (todos (alist-get 'todos note))
                  (chapters (alist-get 'chapters note))
                  (transcript (or (alist-get 'transcript note) "")))
              (if err
                  (insert (propertize (format "  %s: %s\n" mid err) 'face 'error))
                ;; Summary
                (when (not (string-empty-p summary))
                  (insert (propertize "Summary" 'face 'bold) "\n"
                          summary "\n\n"))
                ;; Todos
                (when todos
                  (insert (propertize "Action Items" 'face 'bold) "\n")
                  (dolist (todo todos)
                    (let ((text (or (alist-get 'text todo)
                                    (alist-get 'content todo) "")))
                      (insert "  - " text "\n")))
                  (insert "\n"))
                ;; Chapters
                (when chapters
                  (insert (propertize "Chapters" 'face 'bold) "\n")
                  (dolist (ch chapters)
                    (let ((ch-title (or (alist-get 'title ch) ""))
                          (ch-content (or (alist-get 'content ch) "")))
                      (insert "  " (propertize ch-title 'face 'bold) "\n")
                      (unless (string-empty-p ch-content)
                        (insert "    " ch-content "\n"))))
                  (insert "\n"))
                ;; Transcript
                (when (not (string-empty-p transcript))
                  (insert (propertize "Transcript" 'face 'bold) "\n"
                          transcript "\n\n")))))))
      (special-mode)
      (goto-char (point-min)))
    (pop-to-buffer buf)))

;;;; Recording
;; CLI: vc +recording --meeting-ids X

(defun elark-meetings--format-value (value)
  "Format VALUE as a display string."
  (cond
   ((null value) "")
   ((stringp value) value)
   ((numberp value) (number-to-string value))
   ((eq value t) "true")
   ((eq value :false) "false")
   (t (format "%s" value))))

(defvar elark-meetings--download-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'elark-meetings--download-at-point)
    (define-key map [mouse-1]   #'elark-meetings--download-at-point)
    map)
  "Keymap for download links in recording buffers.")

(defun elark-meetings--insert-download-link (minute-token)
  "Insert a clickable download link for MINUTE-TOKEN.
Uses the nf-fa-download icon ()."
  (insert "  "
          (propertize " Download Recording"
                      'face 'link
                      'mouse-face 'highlight
                      'elark-minute-token minute-token
                      'keymap elark-meetings--download-keymap)
          "\n"))

(defun elark-meetings--download-at-point ()
  "Download the recording at point."
  (interactive)
  (let ((token (get-text-property (point) 'elark-minute-token)))
    (unless token (user-error "No download link at point"))
    (elark-meetings-download-recording token)))

(defun elark-meetings-download-recording (minute-token &optional output-dir)
  "Download recording for MINUTE-TOKEN.
Optional OUTPUT-DIR specifies where to save; prompts if interactive."
  (interactive
   (list (read-string "Minute token: ")
         (read-directory-name "Save to: ")))
  (when (string-empty-p minute-token)
    (user-error "Minute token is required"))
  (message "Lark: downloading recording...")
  (let ((args (list "minutes" "+download" "--minute-tokens" minute-token)))
    (when (and output-dir (not (string-empty-p output-dir)))
      (setq args (append args (list "--output" (expand-file-name output-dir)))))
    (elark--run-command
     args
     (lambda (data)
       (let ((path (or (alist-get 'path data)
                       (alist-get 'output data)
                       (elark--get-nested data 'data 'path))))
         (message "Lark: recording downloaded%s"
                  (if path (format " → %s" path) "")))))))

(defun elark-meetings--insert-recording (recording)
  "Insert a structured section for a single RECORDING alist."
  (let ((known-keys '(meeting_id minute_token url recording_url
                      download_url status duration file_size
                      title topic start_time end_time error)))
    ;; Display known fields first in order
    (dolist (key known-keys)
      (let ((val (alist-get key recording)))
        (when val
          (let ((label (capitalize (replace-regexp-in-string "_" " " (symbol-name key))))
                (str (elark-meetings--format-value val)))
            (unless (string-empty-p str)
              (elark-meetings--insert-field label str))))))
    ;; Display remaining unknown fields
    (dolist (pair recording)
      (unless (memq (car pair) known-keys)
        (let ((label (capitalize (replace-regexp-in-string "_" " " (symbol-name (car pair)))))
              (val (cdr pair)))
          (cond
           ((and (listp val) (consp (car val)))
            ;; Nested alist — render as indented sub-fields
            (insert (propertize (format "  %-18s" (concat label ":")) 'face 'font-lock-keyword-face) "\n")
            (dolist (sub-pair val)
              (let ((sub-label (capitalize (replace-regexp-in-string "_" " " (symbol-name (car sub-pair)))))
                    (sub-val (elark-meetings--format-value (cdr sub-pair))))
                (unless (string-empty-p sub-val)
                  (insert (propertize (format "    %-16s" (concat sub-label ":")) 'face 'font-lock-comment-face)
                          sub-val "\n")))))
           (t
            (let ((str (elark-meetings--format-value val)))
              (unless (string-empty-p str)
                (elark-meetings--insert-field label str))))))))
    ;; Download link when minute_token is available
    (let ((mt (elark-meetings--format-value (alist-get 'minute_token recording))))
      (when (and mt (not (string-empty-p mt)))
        (insert "\n")
        (elark-meetings--insert-download-link mt)))))

;;;; Transcript
;; CLI: vc +notes --minute-tokens X --output-dir DIR
;; Downloads artifact files (transcript.txt) into a temp directory,
;; then renders the transcript into the meeting detail buffer.

(defun elark-meetings--minute-tokens (recordings)
  "Return the unique non-empty minute tokens from RECORDINGS."
  (let (tokens)
    (dolist (rec recordings)
      (let ((mt (elark-meetings--format-value (alist-get 'minute_token rec))))
        (when (and (not (string-empty-p mt)) (not (member mt tokens)))
          (push mt tokens))))
    (nreverse tokens)))

(defun elark-meetings--fetch-transcript (minute-token buf)
  "Download the transcript for MINUTE-TOKEN and append it to BUF.
Runs `vc +notes --minute-tokens X --output-dir DIR' in a temporary
directory (the CLI only accepts output paths relative to its CWD),
reads the transcript file reported in the response, then deletes
the temporary directory."
  (let* ((tmp-dir (make-temp-file "elark-minutes-" t))
         (default-directory tmp-dir))
    (elark--run-command
     (list "vc" "+notes"
           "--minute-tokens" minute-token
           "--output-dir" "./artifacts")
     (lambda (data)
       (unwind-protect
           (let* ((note (car (elark--get-nested data 'data 'notes)))
                  (rel (elark--get-nested note 'artifacts 'transcript_file))
                  (file (and rel (expand-file-name rel tmp-dir))))
             (when (and file (file-readable-p file) (buffer-live-p buf))
               (let ((text (with-temp-buffer
                             (insert-file-contents file)
                             (buffer-string))))
                 (with-current-buffer buf
                   (setf (alist-get minute-token
                                    elark-meetings--detail-transcripts
                                    nil nil #'equal)
                         text)
                   (let ((inhibit-read-only t))
                     (save-excursion
                       (goto-char (point-max))
                       (insert "\n" (propertize "Transcript" 'face 'bold)
                               (propertize (format "  (%s)" minute-token)
                                           'face 'font-lock-comment-face)
                               "\n" (elark-ui-separator 40) "\n\n")
                       (elark-meetings--insert-transcript text)))))))
         (delete-directory tmp-dir t)))
     nil :no-error t
     :on-error (lambda (_code _msg)
                 (when (file-directory-p tmp-dir)
                   (delete-directory tmp-dir t))))))

(defun elark-meetings--insert-transcript (text)
  "Insert transcript TEXT with structured rendering.
The transcript format is: a `DATE|DURATION' header line, an optional
Keywords section, then `SPEAKER HH:MM:SS.mmm' blocks each followed
by the spoken text."
  (let ((first-line t))
    (dolist (line (split-string text "\n"))
      (cond
       ;; Header: "2026-06-11 16:34:33 CST|34min 54s"
       ((and first-line (string-match "\\`\\(.+\\)|\\(.+\\)\\'" line))
        (elark-meetings--insert-field "Time" (match-string 1 line))
        (elark-meetings--insert-field "Duration" (match-string 2 line)))
       ;; Speaker line: "Speaker 1 00:00:01.670"
       ((string-match "\\`\\(.+?\\) \\([0-9]+:[0-9][0-9]:[0-9][0-9]\\(?:\\.[0-9]+\\)?\\)[ \t]*\\'" line)
        (insert (propertize (match-string 1 line) 'face 'font-lock-function-name-face)
                "  "
                (propertize (match-string 2 line) 'face 'font-lock-comment-face)
                "\n"))
       ;; "Keywords:" section label
       ((string-match "\\`\\([A-Za-z ]+\\):[ \t]*\\'" line)
        (insert (propertize (match-string 1 line) 'face 'bold) "\n"))
       ;; Blank line
       ((string-empty-p (string-trim line))
        (insert "\n"))
       ;; Spoken text / other content
       (t
        (insert "  " line "\n")))
      (setq first-line nil))))

;;;###autoload
(defun elark-meetings-recording (meeting-id)
  "Query recording / minute_token for MEETING-ID."
  (interactive "sMeeting ID: ")
  (when (string-empty-p meeting-id)
    (user-error "Meeting ID is required"))
  (message "Lark: querying recording...")
  (elark--run-command
   (list "vc" "+recording" "--meeting-ids" meeting-id)
   (lambda (data)
     (let* ((items (or (elark--get-nested data 'data 'items)
                       (elark--get-nested data 'data 'recordings)
                       (alist-get 'data data)))
            ;; Normalize: if items is a single alist (not a list of alists), wrap it
            (items (if (and items (listp items) (not (listp (car items))))
                       (list items)
                     items))
            (buf (get-buffer-create (format "*Lark Recording: %s*" meeting-id))))
       (with-current-buffer buf
         (let ((inhibit-read-only t))
           (erase-buffer)
           (insert (propertize (format "Recording: %s" meeting-id) 'face 'bold) "\n"
                   (elark-ui-separator 60) "\n\n")
           (if (null items)
               (insert "(no recording found)\n")
             (let ((idx 0))
               (dolist (rec items)
                 (when (> idx 0)
                   (insert "\n" (elark-ui-separator 40) "\n\n"))
                 (elark-meetings--insert-recording rec)
                 (setq idx (1+ idx))))))
         (special-mode)
         (goto-char (point-min)))
       (pop-to-buffer buf)))))

;;;; Copy helpers

(defun elark-meetings-copy-id ()
  "Copy the meeting ID at point."
  (interactive)
  (let ((id (elark-meetings--meeting-id-at-point)))
    (unless id (user-error "No meeting at point"))
    (kill-new id)
    (message "Copied: %s" id)))

(defun elark-meetings-copy-link ()
  "Copy the meeting app link at point."
  (interactive)
  (let ((meeting (elark-meetings--meeting-at-point)))
    (unless meeting (user-error "No meeting at point"))
    (let ((link (elark-meetings--meeting-app-link meeting)))
      (if (string-empty-p link)
          (user-error "No link for this meeting")
        (kill-new link)
        (message "Copied: %s" link)))))

;;;; Transient dispatch

;;;###autoload (autoload 'elark-meetings-dispatch "elark-meetings" nil t)
(transient-define-prefix elark-meetings-dispatch ()
  "Lark Meetings commands."
  ["Search"
   ("s" "Search meetings"   elark-meetings-search)]
  ["At Point"
   ("RET" "Open detail"     elark-meetings-open)
   ("N"   "Meeting notes"   elark-meetings-notes-at-point)]
  ["By ID"
   ("n" "Notes by ID"       elark-meetings-notes)
   ("r" "Recording by ID"   elark-meetings-recording)])

(provide 'elark-meetings)
;;; elark-meetings.el ends here
