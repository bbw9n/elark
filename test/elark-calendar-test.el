;;; elark-calendar-test.el --- Tests for elark-calendar.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 bbw9n

;; Author: bbw9n <bbw9nio@gmail.com>
;; Assisted-by: Claude:claude-opus-5
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'ert)

(let ((root (expand-file-name ".." (file-name-directory (or load-file-name (buffer-file-name))))))
  (dolist (sub '("." "core" "ui" "domain" "ai"))
    (add-to-list 'load-path (expand-file-name sub root))))

(require 'elark-calendar)

;;;; Event parsing tests

(ert-deftest elark-calendar-test-event-id ()
  "Extract event ID from various formats."
  (should (equal (elark-calendar--event-id '((event_id . "ev_123")))
                 "ev_123"))
  (should (equal (elark-calendar--event-id '((id . "456")))
                 "456")))

(ert-deftest elark-calendar-test-event-title ()
  "Extract event title."
  (should (equal (elark-calendar--event-title '((summary . "Meeting")))
                 "Meeting"))
  (should (equal (elark-calendar--event-title '((title . "Standup")))
                 "Standup"))
  (should (equal (elark-calendar--event-title '((foo . "bar")))
                 "(no title)")))

(ert-deftest elark-calendar-test-event-location ()
  "Extract event location."
  (should (equal (elark-calendar--event-location '((location . "Room A")))
                 "Room A"))
  (should (equal (elark-calendar--event-location
                  '((location . ((name . "Room B")))))
                 "Room B"))
  (should (equal (elark-calendar--event-location '((foo . "bar")))
                 "")))

(ert-deftest elark-calendar-test-event-status ()
  "Extract event status."
  (should (equal (elark-calendar--event-status '((status . "accepted")))
                 "accepted"))
  (should (equal (elark-calendar--event-status '((foo . "bar")))
                 "")))

;;;; Extract events from actual response shapes

(ert-deftest elark-calendar-test-extract-events-agenda ()
  "Extract events from +agenda shape: {ok: t, data: [<events>]}."
  (let ((data '((ok . t)
                (data . (((event_id . "1") (summary . "A"))
                          ((event_id . "2") (summary . "B")))))))
    (should (= (length (elark-calendar--extract-events data)) 2))))

(ert-deftest elark-calendar-test-extract-events-raw-api ()
  "Extract events from raw API shape: {code: 0, data: {items: [...]}}."
  (let ((data '((code . 0)
                (data . ((items . (((event_id . "1") (summary . "A")))))))))
    (should (= (length (elark-calendar--extract-events data)) 1))))

(ert-deftest elark-calendar-test-extract-events-empty-data ()
  "Empty data array returns nil."
  (let ((data '((ok . t) (data))))
    (should (null (elark-calendar--extract-events data)))))

(ert-deftest elark-calendar-test-extract-events-nil ()
  "Nil data returns nil."
  (should (null (elark-calendar--extract-events nil))))

;;;; Field extractors

(ert-deftest elark-calendar-test-event-organizer ()
  "Extract organizer from event."
  ;; Prefer event_organizer.display_name
  (should (equal (elark-calendar--event-organizer
                  '((event_organizer . ((display_name . "Alice")))))
                 "Alice"))
  ;; Fall back to organizer_calendar_id
  (should (equal (elark-calendar--event-organizer
                  '((organizer_calendar_id . "cal@group")))
                 "cal@group"))
  ;; Empty when neither present
  (should (equal (elark-calendar--event-organizer '((foo . "bar"))) "")))

(ert-deftest elark-calendar-test-event-meeting-url ()
  "Extract meeting URL from event."
  (should (equal (elark-calendar--event-meeting-url
                  '((vchat . ((meeting_url . "https://vc.example.com/123")))))
                 "https://vc.example.com/123"))
  (should (equal (elark-calendar--event-meeting-url '((foo . "bar"))) "")))

(ert-deftest elark-calendar-test-event-start-end ()
  "Extract start/end times from event."
  (let ((event '((start_time . ((datetime . "2026-04-11T09:00:00+08:00")))
                 (end_time . ((datetime . "2026-04-11T09:30:00+08:00"))))))
    (should (equal (elark-calendar--event-start event) "2026-04-11T09:00"))
    (should (equal (elark-calendar--event-end event) "2026-04-11T09:30"))))

;;;; Section rendering

(ert-deftest elark-calendar-test-insert-event ()
  "Insert event renders all fields with text property."
  (let ((event '((event_id . "ev1")
                 (summary . "Standup")
                 (start_time . ((datetime . "2026-04-11T09:00:00+08:00")))
                 (end_time . ((datetime . "2026-04-11T09:30:00+08:00")))
                 (location . "Room 1")
                 (self_rsvp_status . "accept")
                 (vchat . ((meeting_url . "https://vc.example.com/1"))))))
    (with-temp-buffer
      (elark-calendar--insert-event event)
      (let ((text (buffer-string)))
        (should (string-match-p "Standup" text))
        (should (string-match-p "2026-04-11T09:00" text))
        (should (string-match-p "Room 1" text))
        (should (string-match-p "accept" text))
        (should (string-match-p "vc.example.com" text)))
      ;; Check text property
      (goto-char (point-min))
      (should (equal (get-text-property (point) 'elark-event-id) "ev1")))))

;;;; Time resolution

(ert-deftest elark-calendar-test-resolve-time-datetime-object ()
  "Resolve {datetime: ...} object."
  (should (equal (elark-calendar--resolve-time
                  '((datetime . "2026-04-11T09:00:00+08:00")))
                 "2026-04-11T09:00")))

(ert-deftest elark-calendar-test-resolve-time-string ()
  "Resolve plain datetime string."
  (should (equal (elark-calendar--resolve-time "2026-04-11T10:00:00")
                 "2026-04-11T10:00")))

(ert-deftest elark-calendar-test-resolve-time-number ()
  "Resolve numeric timestamp."
  (let ((result (elark-calendar--resolve-time 1700000000)))
    (should (stringp result))
    (should (string-match-p "^[0-9]\\{4\\}-" result))))

(ert-deftest elark-calendar-test-resolve-time-nil ()
  "Resolve nil returns empty string."
  (should (equal (elark-calendar--resolve-time nil) "")))

;;;; Format datetime

(ert-deftest elark-calendar-test-format-datetime ()
  "Format datetime strings."
  (should (equal (elark-calendar--format-datetime "2026-04-11T10:00:00")
                 "2026-04-11T10:00"))
  (should (equal (elark-calendar--format-datetime "2026-04-11")
                 "2026-04-11"))
  (should (equal (elark-calendar--format-datetime "not a date")
                 "not a date")))

(provide 'elark-calendar-test)
;;; elark-calendar-test.el ends here
