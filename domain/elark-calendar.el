;;; elark-calendar.el --- Lark Calendar integration -*- lexical-binding: t; -*-

;; Copyright (C) 2026 bbw9n

;; Author: bbw9n <bbw9nio@gmail.com>
;; Assisted-by: Claude:claude-opus-5
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Provides Calendar domain commands for elark: agenda view, event
;; listing, event detail, event creation, free/busy checks, and time
;; suggestions.  All data is fetched from lark-cli asynchronously and
;; displayed in tabulated-list buffers.
;;
;; CLI command mapping:
;;   Agenda:       calendar +agenda [--calendar-id X] [--start X] [--end X]
;;   Create event: calendar +create --summary X --start X --end X [--attendee-ids X]
;;   Free/busy:    calendar +freebusy [--user-id X]
;;   RSVP:         calendar +rsvp --event-id X --rsvp-status accept|decline|tentative
;;   Suggestions:  calendar +suggestion --attendee-ids X
;;   List cals:    calendar calendars list
;;   Get event:    calendar events get --params '{"calendar_id":X,"event_id":X}'
;;   Delete event: calendar events delete --params '{"calendar_id":X,"event_id":X}'

;;; Code:

(require 'elark-core)
(require 'elark-ui)
(require 'json)
(require 'transient)

;;;; Customization

(defgroup elark-calendar nil
  "Lark Calendar settings."
  :group 'elark
  :prefix "elark-calendar-")

(defcustom elark-calendar-default-days 1
  "Number of days to show in the agenda view."
  :type 'integer
  :group 'elark-calendar)

;;;; Buffer-local variables

(defvar-local elark-calendar--events nil
  "Cached event data for the current buffer.")

(defvar-local elark-calendar--calendar-id nil
  "Calendar ID for the current buffer, or nil for primary.")

;;;; Event parsing

(defun elark-calendar--resolve-time (time-val)
  "Resolve TIME-VAL to a displayable string.
TIME-VAL can be a number (unix timestamp), a string, or an alist
like ((datetime . \"...\") (timezone . \"...\"))."
  (cond
   ((numberp time-val) (or (elark--format-timestamp time-val) ""))
   ((stringp time-val) (elark-calendar--format-datetime time-val))
   ((and (listp time-val) (alist-get 'datetime time-val))
    (elark-calendar--format-datetime (alist-get 'datetime time-val)))
   ((and (listp time-val) (alist-get 'date time-val))
    (alist-get 'date time-val))
   ((and (listp time-val) (alist-get 'timestamp time-val))
    (or (elark--format-timestamp (alist-get 'timestamp time-val)) ""))
   (t "")))

(defun elark-calendar--parse-event-time (event)
  "Extract a display time string from EVENT."
  (let* ((start (or (alist-get 'start_time event)
                    (alist-get 'start event)))
         (end (or (alist-get 'end_time event)
                  (alist-get 'end event)))
         (start-str (elark-calendar--resolve-time start))
         (end-str (elark-calendar--resolve-time end)))
    (if (and start-str end-str
             (string-prefix-p (substring start-str 0 (min 10 (length start-str)))
                              end-str))
        ;; Same day: show "YYYY-MM-DD HH:MM - HH:MM"
        (format "%s - %s" start-str (substring end-str (min 11 (length end-str))))
      (format "%s - %s" start-str end-str))))

(defun elark-calendar--format-datetime (str)
  "Format a datetime STR for display (pass through if already readable)."
  (if (string-match-p "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}" str)
      (substring str 0 (min 16 (length str)))
    str))

(defun elark-calendar--event-id (event)
  "Extract the event ID from EVENT."
  (or (alist-get 'event_id event)
      (alist-get 'id event)))

(defun elark-calendar--event-title (event)
  "Extract the title from EVENT."
  (or (alist-get 'summary event)
      (alist-get 'title event)
      "(no title)"))

(defun elark-calendar--event-location (event)
  "Extract the location from EVENT."
  (let ((loc (alist-get 'location event)))
    (cond
     ((stringp loc) loc)
     ((listp loc) (or (alist-get 'name loc) ""))
     (t ""))))

(defun elark-calendar--event-status (event)
  "Extract the status/RSVP from EVENT."
  (or (alist-get 'self_rsvp_status event)
      (alist-get 'status event)
      (alist-get 'self_attendee_status event)
      ""))

(defun elark-calendar--event-organizer (event)
  "Extract the organizer from EVENT.
Prefer display_name from event_organizer; fall back to organizer_calendar_id."
  (or (elark--get-nested event 'event_organizer 'display_name)
      (let ((cal-id (alist-get 'organizer_calendar_id event)))
        (when (and cal-id (not (string-empty-p cal-id)))
          cal-id))
      ""))

(defun elark-calendar--event-meeting-url (event)
  "Extract the meeting/video chat URL from EVENT."
  (or (elark--get-nested event 'vchat 'meeting_url)
      (alist-get 'meeting_url event)
      ""))

(defun elark-calendar--event-start (event)
  "Extract the start time display string from EVENT."
  (elark-calendar--resolve-time
   (or (alist-get 'start_time event) (alist-get 'start event))))

(defun elark-calendar--event-end (event)
  "Extract the end time display string from EVENT."
  (elark-calendar--resolve-time
   (or (alist-get 'end_time event) (alist-get 'end event))))

(defun elark-calendar--extract-events (data)
  "Extract the event list from lark-cli response DATA.
Handles the two response shapes from lark-cli:
  {ok: true, data: [...]}       — +agenda returns data as a direct event array
  {code: 0, data: {items: ...}} — raw API style"
  (when (listp data)
    (let ((inner (alist-get 'data data)))
      (cond
       ;; {data: [<event>, ...]} — +agenda style, data is the array directly
       ((and (listp inner) (listp (car inner))
             (or (null inner)
                 (alist-get 'event_id (car inner))
                 (alist-get 'summary (car inner))))
        inner)
       ;; {data: {items: [...]}} — raw API style
       ((and (listp inner) (alist-get 'items inner))
        (alist-get 'items inner))
       (t nil)))))

;;;; Section-based event display

(defvar elark-calendar-events-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'elark-calendar-event-open)
    (define-key map (kbd "g")   #'elark-calendar-agenda-refresh)
    (define-key map (kbd "c")   #'elark-calendar-create-event)
    (define-key map (kbd "d")   #'elark-calendar-event-delete)
    (define-key map (kbd "y")   #'elark-calendar-event-copy-id)
    (define-key map (kbd "n")   #'elark-calendar--next-event)
    (define-key map (kbd "p")   #'elark-calendar--prev-event)
    (define-key map (kbd "?")   #'elark-calendar-dispatch)
    map)
  "Keymap for `elark-calendar-events-mode'.")

(define-derived-mode elark-calendar-events-mode special-mode
  "Lark Events"
  "Major mode for browsing Lark calendar events.
Each event is displayed as a multi-line section.")

;;;; AI context provider — lets `elark-ai-act' know what's at point
;;
;; Registered via the `elark-ai-context-provider' symbol property; the
;; AI module looks it up by major mode without reaching into our
;; buffer-locals.  No load-time dependency on `elark-ai-context'.

(defun elark-calendar--ai-context ()
  "Return the AI context plist for a calendar events buffer."
  (let ((event-id (get-text-property (point) 'elark-event-id))
        (events elark-calendar--events))
    (list :domain "calendar"
          :buffer-type "agenda"
          :item (when event-id
                  (list :event-id event-id
                        :event (seq-find (lambda (e)
                                           (equal (or (alist-get 'event_id e)
                                                      (alist-get 'id e))
                                                  event-id))
                                         events)))
          :summary (format "Calendar agenda with %d events%s"
                           (length events)
                           (if event-id
                               (format ", cursor on event %s" event-id)
                             "")))))

(put 'elark-calendar-events-mode 'elark-ai-context-provider
     #'elark-calendar--ai-context)

(defun elark-calendar--rsvp-face (status)
  "Return a face for RSVP STATUS."
  (pcase status
    ("accept" 'success)
    ("decline" 'error)
    ("tentative" 'warning)
    ("needs_action" 'font-lock-comment-face)
    (_ 'default)))

(defun elark-calendar--insert-field (label value)
  "Insert a LABEL: VALUE line if VALUE is non-empty.
VALUE may span multiple lines; continuation lines are indented."
  (elark-ui-insert-field label value 12))

(defun elark-calendar--insert-event (event)
  "Insert a multi-line section for EVENT into the current buffer."
  (let ((id (elark-calendar--event-id event))
        (title (elark-calendar--event-title event))
        (start (elark-calendar--event-start event))
        (end (elark-calendar--event-end event))
        (location (elark-calendar--event-location event))
        (status (elark-calendar--event-status event))
        (organizer (elark-calendar--event-organizer event))
        (meeting-url (elark-calendar--event-meeting-url event))
        (beg (point)))
    ;; Title line
    (insert (propertize title 'face 'bold) "\n")
    ;; Fields
    (elark-calendar--insert-field "Start" start)
    (elark-calendar--insert-field "End" end)
    (elark-calendar--insert-field "Location" location)
    (elark-calendar--insert-field "Organizer" organizer)
    (elark-calendar--insert-field "Meeting" meeting-url)
    (when (and status (not (string-empty-p status)))
      (insert (format "  %-12s" "RSVP:")
              (propertize status 'face (elark-calendar--rsvp-face status))
              "\n"))
    ;; Separator
    (insert "\n")
    ;; Tag the whole section with the event ID for navigation
    (put-text-property beg (point) 'elark-event-id id)))

(defun elark-calendar--render-events (events buf-name &optional calendar-id)
  "Render EVENTS into a section-based buffer named BUF-NAME.
Optional CALENDAR-ID is stored for refresh."
  (let ((buf (get-buffer-create buf-name)))
    (with-current-buffer buf
      (elark-calendar-events-mode)
      (setq elark-calendar--events events
            elark-calendar--calendar-id calendar-id)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (if (null events)
            (insert "(no events)\n")
          (dolist (event events)
            (elark-calendar--insert-event event))))
      (goto-char (point-min))
      (setq header-line-format
            (format " %s — %d event(s)" buf-name (length events))))
    (pop-to-buffer buf)))

;;;; Navigation within event sections

(defun elark-calendar--event-id-at-point ()
  "Return the event ID at point, or nil."
  (get-text-property (point) 'elark-event-id))

(defun elark-calendar--next-event ()
  "Move to the next event section."
  (interactive)
  (elark-ui-next-section 'elark-event-id))

(defun elark-calendar--prev-event ()
  "Move to the previous event section."
  (interactive)
  (elark-ui-prev-section 'elark-event-id))

;;;; Agenda
;; CLI: calendar +agenda [--calendar-id X] [--start X] [--end X]


;;;###autoload
(defun elark-calendar-agenda (&optional calendar-id)
  "Show today's Lark calendar agenda.
Optional CALENDAR-ID to show a specific calendar (default: primary)."
  (interactive)
  (message "Lark: fetching agenda...")
  (let ((args (list "calendar" "+agenda")))
    (when calendar-id
      (setq args (append args (list "--calendar-id" calendar-id))))
    (elark--run-command
     args
     #'elark-calendar--display-agenda)))

(defun elark-calendar-agenda-refresh ()
  "Refresh the current agenda buffer."
  (interactive)
  (elark-calendar-agenda elark-calendar--calendar-id))

(defun elark-calendar--display-agenda (data)
  "Display agenda DATA in a section-based buffer."
  (let ((events (elark-calendar--extract-events data)))
    (elark-calendar--render-events events "*Lark Agenda*")))

;;;; Event listing (uses +agenda with date range)
;; There is no "events list" command; use +agenda with --start/--end.

;;;###autoload
(defun elark-calendar-events (&optional calendar-id)
  "List events for CALENDAR-ID (or primary calendar).
Shows events for the next 7 days."
  (interactive)
  (message "Lark: fetching events...")
  (let ((args (list "calendar" "+agenda"
                    "--start" (elark--format-time-iso8601)
                    "--end" (elark--format-time-iso8601
                             (time-add nil (* 7 86400))))))
    (when calendar-id
      (setq args (append args (list "--calendar-id" calendar-id))))
    (elark--run-command
     args
     (lambda (data)
       (elark-calendar--display-events data calendar-id)))))

(defun elark-calendar--display-events (data &optional calendar-id)
  "Display event list DATA for CALENDAR-ID."
  (let ((events (elark-calendar--extract-events data))
        (name (if calendar-id
                  (format "*Lark Events: %s*" calendar-id)
                "*Lark Events*")))
    (elark-calendar--render-events events name calendar-id)))

;;;; Calendar listing
;; CLI: calendar calendars list [--params JSON]

;;;###autoload
(defun elark-calendar-list ()
  "List available Lark calendars."
  (interactive)
  (message "Lark: fetching calendars...")
  (elark--run-command
   '("calendar" "calendars" "list")
   #'elark-calendar--display-calendars))

(defun elark-calendar--display-calendars (data)
  "Display calendar list DATA."
  (let* ((calendars (elark--get-nested data 'data 'calendar_list))
         (buf (get-buffer-create "*Lark Calendars*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize "Lark Calendars\n" 'face 'bold)
                (elark-ui-separator 60) "\n\n")
        (if (null calendars)
            (insert "(no calendars found)\n")
          (dolist (cal calendars)
            (let ((id (or (alist-get 'calendar_id cal) (alist-get 'id cal)))
                  (name (or (alist-get 'summary cal) (alist-get 'name cal) ""))
                  (desc (or (alist-get 'description cal) ""))
                  (role (or (alist-get 'role cal) "")))
              (insert (propertize name 'face 'bold) "\n"
                      "  ID: " (or id "") "\n"
                      (if (string-empty-p desc) "" (format "  Description: %s\n" desc))
                      (if (string-empty-p role) "" (format "  Role: %s\n" role))
                      "\n")))))
      (special-mode)
      (goto-char (point-min)))
    (pop-to-buffer buf)))

;;;; Event detail
;; CLI: calendar events get --params '{"calendar_id":"primary","event_id":"X"}'

(defun elark-calendar-event-open ()
  "Open the event at point in a detail view."
  (interactive)
  (let ((id (elark-calendar--event-id-at-point)))
    (unless id (user-error "No event at point"))
    (message "Lark: fetching event details...")
    (let ((params (json-encode `((calendar_id . "primary")
                                 (event_id . ,id)))))
      (elark--run-command
       (list "calendar" "events" "get" "--params" params)
       (lambda (data)
         (elark-calendar--display-event-detail data id))))))

(defun elark-calendar--display-event-detail (data event-id)
  "Display event detail DATA for EVENT-ID."
  (let* ((event (or (alist-get 'data data) data))
         (title (elark-calendar--event-title event))
         (buf (get-buffer-create (format "*Lark Event: %s*" title))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (elark-ui-insert-title title)
        ;; Time
        (let ((time-str (elark-calendar--parse-event-time event)))
          (unless (string-empty-p time-str)
            (insert (propertize "Time: " 'face 'bold) time-str "\n")))
        ;; Location
        (let ((loc (elark-calendar--event-location event)))
          (unless (string-empty-p loc)
            (insert (propertize "Location: " 'face 'bold) loc "\n")))
        ;; Organizer
        (let ((org (or (alist-get 'organizer_name event)
                       (elark--get-nested event 'organizer 'display_name)
                       (elark--get-nested event 'organizer 'name))))
          (when org
            (insert (propertize "Organizer: " 'face 'bold) org "\n")))
        ;; Status
        (let ((status (elark-calendar--event-status event)))
          (unless (string-empty-p status)
            (insert (propertize "Status: " 'face 'bold) status "\n")))
        ;; Description
        (let ((desc (or (alist-get 'description event) "")))
          (unless (string-empty-p desc)
            (insert "\n" (propertize "Description" 'face 'bold) "\n"
                    (elark-ui-separator 40) "\n"
                    desc "\n")))
        ;; Attendees
        (let ((attendees (or (alist-get 'attendees event) nil)))
          (when attendees
            (insert "\n" (propertize "Attendees" 'face 'bold) "\n"
                    (elark-ui-separator 40) "\n")
            (dolist (att attendees)
              (let ((name (or (alist-get 'display_name att)
                              (alist-get 'name att) ""))
                    (rsvp (or (alist-get 'rsvp_status att)
                              (alist-get 'status att) "")))
                (insert "  " name
                        (if (string-empty-p rsvp) "" (format " (%s)" rsvp))
                        "\n")))))
        ;; Event ID
        (insert "\n" (propertize "Event ID: " 'face 'font-lock-comment-face)
                (or event-id "") "\n"))
      (special-mode)
      (goto-char (point-min)))
    (pop-to-buffer buf)))

;;;; Event creation
;; CLI: calendar +create --summary X --start X --end X [--description X]
;;                       [--attendee-ids X] [--calendar-id X]

;;;###autoload (autoload 'elark-calendar-create-event "elark-calendar" nil t)
(transient-define-prefix elark-calendar-create-event ()
  "Create a new Lark calendar event."
  ["Event Details"
   ("t" "Title"       "--summary=" :prompt "Event title: ")
   ("s" "Start time"  "--start=" :prompt "Start (HH:MM, MM-DD HH:MM, or YYYY-MM-DD HH:MM): ")
   ("e" "End time"    "--end=" :prompt "End (HH:MM, MM-DD HH:MM, or YYYY-MM-DD HH:MM): ")
   ("d" "Description" "--description=" :prompt "Description: ")]
  ["Attendees"
   ("a" "Attendees"   "--attendee-ids=" :prompt "Attendee IDs (comma-separated ou_/oc_): ")]
  ["Actions"
   ("RET" "Create"    elark-calendar--do-create-event)
   ("q"   "Cancel"    transient-quit-all)])

(defun elark-calendar--do-create-event (&rest _args)
  "Execute event creation with transient arguments.
Time values are expanded from shorthand (e.g. \"10:00\") to ISO 8601."
  (interactive)
  (let ((args (elark-calendar--expand-time-args
               (transient-args 'elark-calendar-create-event))))
    (unless args
      (user-error "No event details provided"))
    (message "Lark: creating event...")
    (elark--run-command
     (append '("calendar" "+create") args)
     (lambda (data)
       (let ((id (or (elark--get-nested data 'data 'event_id)
                     (alist-get 'event_id data)
                     (alist-get 'id data))))
         (message "Lark: event created%s"
                  (if id (format " (ID: %s)" id) "")))))))

(defun elark-calendar--expand-time-args (args)
  "Expand --start= and --end= values in ARGS through `elark--parse-time-input'."
  (mapcar
   (lambda (arg)
     (cond
      ((string-match "^--start=\\(.*\\)$" arg)
       (concat "--start=" (elark--parse-time-input (match-string 1 arg))))
      ((string-match "^--end=\\(.*\\)$" arg)
       (concat "--end=" (elark--parse-time-input (match-string 1 arg))))
      (t arg)))
   args))

;;;; Event deletion
;; CLI: calendar events delete --params '{"calendar_id":"primary","event_id":"X"}'

(defun elark-calendar-event-delete ()
  "Delete the event at point."
  (interactive)
  (let ((id (elark-calendar--event-id-at-point)))
    (unless id (user-error "No event at point"))
    (when (yes-or-no-p (format "Delete event %s? " id))
      (message "Lark: deleting event...")
      (let ((params (json-encode `((calendar_id . "primary")
                                   (event_id . ,id)))))
        (elark--run-command
         (list "calendar" "events" "delete" "--params" params)
         (lambda (_data)
           (message "Lark: event deleted")
           (elark-calendar-agenda-refresh)))))))

;;;; Copy event ID

(defun elark-calendar-event-copy-id ()
  "Copy the event ID at point to the kill ring."
  (interactive)
  (let ((id (elark-calendar--event-id-at-point)))
    (unless id (user-error "No event at point"))
    (kill-new id)
    (message "Copied: %s" id)))

;;;; Free/busy
;; CLI: calendar +freebusy [--user-id X] [--start X] [--end X]

;;;###autoload
(defun elark-calendar-freebusy (&optional user-id)
  "Check free/busy status for USER-ID (default: current user)."
  (interactive
   (list (let ((input (read-string "User ID (ou_ prefix, empty for self): ")))
           (if (string-empty-p input) nil input))))
  (message "Lark: checking free/busy...")
  (let ((args (list "calendar" "+freebusy")))
    (when user-id
      (setq args (append args (list "--user-id" user-id))))
    (elark--run-command args #'elark-calendar--display-freebusy)))

(defun elark-calendar--display-freebusy (data)
  "Display free/busy DATA."
  (let ((buf (get-buffer-create "*Lark Free/Busy*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize "Free/Busy\n" 'face 'bold)
                (elark-ui-separator 60) "\n\n"
                (pp-to-string data)))
      (special-mode)
      (goto-char (point-min)))
    (pop-to-buffer buf)))

;;;; Time suggestion
;; CLI: calendar +suggestion --attendee-ids X [--start X] [--end X]
;;                           [--duration-minutes N]

;;;###autoload
(defun elark-calendar-suggest-time (attendee-ids)
  "Get time suggestions for a meeting with ATTENDEE-IDS."
  (interactive "sAttendee IDs (comma-separated ou_/oc_): ")
  (when (string-empty-p attendee-ids)
    (user-error "Attendee IDs required"))
  (message "Lark: fetching time suggestions...")
  (elark--run-command
   (list "calendar" "+suggestion" "--attendee-ids" attendee-ids)
   (lambda (data)
     (let ((buf (get-buffer-create "*Lark Time Suggestions*")))
       (with-current-buffer buf
         (let ((inhibit-read-only t))
           (erase-buffer)
           (insert (propertize "Time Suggestions\n" 'face 'bold)
                   (elark-ui-separator 60) "\n\n")
           (let ((suggestions (or (alist-get 'suggestions data)
                                  (elark--get-nested data 'data 'suggestions)
                                  (and (listp data) data))))
             (if (null suggestions)
                 (insert "(no suggestions available)\n")
               (dolist (s suggestions)
                 (let ((start (or (alist-get 'start_time s)
                                  (elark--get-nested s 'start 'timestamp)))
                       (end (or (alist-get 'end_time s)
                                (elark--get-nested s 'end 'timestamp))))
                   (insert (format "  %s  -  %s\n"
                                   (or (elark--format-timestamp start) (format "%s" start))
                                   (or (elark--format-timestamp end) (format "%s" end)))))))))
         (special-mode)
         (goto-char (point-min)))
       (pop-to-buffer buf)))))

;;;; RSVP
;; CLI: calendar +rsvp --event-id X --rsvp-status accept|decline|tentative

;;;###autoload
(defun elark-calendar-rsvp (event-id status)
  "RSVP to EVENT-ID with STATUS (accept/decline/tentative)."
  (interactive
   (list (or (elark-calendar--event-id-at-point)
             (read-string "Event ID: "))
         (completing-read "RSVP: " '("accept" "decline" "tentative") nil t)))
  (elark--run-command
   (list "calendar" "+rsvp" "--event-id" event-id "--rsvp-status" status)
   (lambda (_data)
     (message "Lark: RSVP'd %s to event %s" status event-id))))

;;;; Transient dispatch

;;;###autoload (autoload 'elark-calendar-dispatch "elark-calendar" nil t)
(transient-define-prefix elark-calendar-dispatch ()
  "Lark Calendar commands."
  ["Quick Actions"
   ("a" "Agenda (today)" elark-calendar-agenda)
   ("n" "New event"      elark-calendar-create-event)
   ("f" "Free/busy"      elark-calendar-freebusy)
   ("s" "Suggest time"   elark-calendar-suggest-time)
   ("r" "RSVP"           elark-calendar-rsvp)]
  ["Browse"
   ("l" "List calendars" elark-calendar-list)
   ("e" "List events"    elark-calendar-events)])

(provide 'elark-calendar)
;;; elark-calendar.el ends here
