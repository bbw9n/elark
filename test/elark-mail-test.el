;;; elark-mail-test.el --- Tests for elark-mail.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 bbw9n

;; Author: bbw9n <bbw9nio@gmail.com>
;; Assisted-by: Claude:claude-opus-5
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'ert)

(let ((root (expand-file-name ".." (file-name-directory (or load-file-name (buffer-file-name))))))
  (dolist (sub '("." "core" "ui" "domain" "ai"))
    (add-to-list 'load-path (expand-file-name sub root))))

(require 'elark-mail)

;;;; Mail parsing

(ert-deftest elark-mail-test-mail-id ()
  (should (equal (elark-mail--mail-id '((mail_id . "m_123"))) "m_123"))
  (should (equal (elark-mail--mail-id '((message_id . "456"))) "456"))
  (should (equal (elark-mail--mail-id '((id . "789"))) "789")))

(ert-deftest elark-mail-test-mail-subject ()
  (should (equal (elark-mail--mail-subject '((subject . "Hello"))) "Hello"))
  (should (equal (elark-mail--mail-subject '((title . "Update"))) "Update"))
  (should (equal (elark-mail--mail-subject '((foo . "bar"))) "(no subject)")))

(ert-deftest elark-mail-test-mail-from-string ()
  (should (equal (elark-mail--mail-from '((from . "alice@example.com")))
                 "alice@example.com")))

(ert-deftest elark-mail-test-mail-from-alist ()
  (should (equal (elark-mail--mail-from '((from . ((name . "Alice")))))
                 "Alice"))
  (should (equal (elark-mail--mail-from '((sender . ((address . "bob@x.com")))))
                 "bob@x.com")))

(ert-deftest elark-mail-test-mail-from-nil ()
  (should (equal (elark-mail--mail-from '((foo . "bar"))) "")))

(ert-deftest elark-mail-test-mail-date-timestamp ()
  (let ((result (elark-mail--mail-date '((send_time . 1700000000)))))
    (should (stringp result))
    (should (string-match-p "^[0-9]\\{4\\}-" result))))

(ert-deftest elark-mail-test-mail-date-string ()
  (should (equal (elark-mail--mail-date '((date . "2026-04-11 10:00")))
                 "2026-04-11 10:00")))

(ert-deftest elark-mail-test-mail-date-nil ()
  (should (equal (elark-mail--mail-date '((foo . "bar"))) "")))

(ert-deftest elark-mail-test-mail-read-p ()
  (should (elark-mail--mail-read-p '((is_read . t))))
  (should-not (elark-mail--mail-read-p '((is_read . :false))))
  (should-not (elark-mail--mail-read-p '((foo . "bar")))))

(ert-deftest elark-mail-test-mail-has-attachment-p ()
  (should (elark-mail--mail-has-attachment-p '((has_attachment . t))))
  (should (elark-mail--mail-has-attachment-p '((attachments . (((name . "f.pdf")))))))
  (should-not (elark-mail--mail-has-attachment-p '((foo . "bar")))))

;;;; Extract mails

(ert-deftest elark-mail-test-extract-mails-items ()
  (let ((data '((items . (((mail_id . "1")) ((mail_id . "2")))))))
    (should (= (length (elark-mail--extract-mails data)) 2))))

(ert-deftest elark-mail-test-extract-mails-nested ()
  (let ((data '((data . ((mails . (((mail_id . "1")))))))))
    (should (= (length (elark-mail--extract-mails data)) 1))))

(ert-deftest elark-mail-test-extract-mails-empty ()
  (should (null (elark-mail--extract-mails nil))))

;;;; Insert entry

(ert-deftest elark-mail-test-insert-entry ()
  "Insert a read mail entry with text properties."
  (let ((mail '((mail_id . "m1")
                (subject . "Hello")
                (from . "alice@x.com")
                (date . "2026-04-11")
                (is_read . t))))
    (with-temp-buffer
      (elark-mail--insert-entry mail)
      (goto-char (point-min))
      ;; Read flag should be space, not N
      (should (looking-at "  "))
      ;; Subject and from present
      (should (search-forward "alice@x.com" nil t))
      (goto-char (point-min))
      (should (search-forward "Hello" nil t))
      ;; Text property
      (goto-char (point-min))
      (should (equal (get-text-property (point) 'elark-mail-id) "m1")))))

(ert-deftest elark-mail-test-insert-entry-unread ()
  "Unread mail entry should have N flag."
  (let ((mail '((mail_id . "m2")
                (subject . "Urgent")
                (from . "bob@x.com")
                (date . "2026-04-11"))))
    (with-temp-buffer
      (elark-mail--insert-entry mail)
      (goto-char (point-min))
      ;; Unread flag N
      (should (looking-at "N")))))

;;;; Format recipients

(ert-deftest elark-mail-test-format-recipients-nil ()
  (should (equal (elark-mail--format-recipients nil) "")))

(ert-deftest elark-mail-test-format-recipients-string ()
  (should (equal (elark-mail--format-recipients "alice@x.com") "alice@x.com")))

(ert-deftest elark-mail-test-format-recipients-list ()
  (should (equal (elark-mail--format-recipients
                  '(((name . "Alice")) ((address . "bob@x.com"))))
                 "Alice, bob@x.com")))

(ert-deftest elark-mail-test-extract-null-items ()
  "A null mail collection extracts to nil, not `:null'."
  (should-not (elark-mail--extract-mails
               '((data . ((items . :null))))))
  (should-not (elark-mail--extract-mails '((items . :null)))))

;;;; Compose buffer

(defmacro elark-mail-test--with-compose (kind id headers &rest body)
  "Open a compose buffer for KIND/ID/HEADERS without touching windows."
  (declare (indent 3))
  `(cl-letf (((symbol-function 'pop-to-buffer)
              (lambda (buf &rest _) (set-buffer buf))))
     (unwind-protect
         (progn
           (elark-mail-compose--open ,kind ,id ,headers "test")
           (with-current-buffer elark-mail-compose--buffer-name
             ,@body))
       (when-let* ((b (get-buffer elark-mail-compose--buffer-name)))
         (with-current-buffer b (set-buffer-modified-p nil))
         (kill-buffer b)))))

(ert-deftest elark-mail-test-compose-parse ()
  "Headers and body split at the separator; reply buffers are body-only."
  (cl-letf (((symbol-function 'pop-to-buffer)
             (lambda (buf &rest _) (set-buffer buf))))
    ;; New-mail shape.
    (elark-mail-test--with-compose 'new nil
        '(("To" . "a@x.com") ("Cc" . "") ("Subject" . ""))
      (goto-char (point-max))
      (insert "Hello\nthere")
      (goto-char (point-min))
      (search-forward "Subject:")
      (end-of-line) (insert " Greetings")
      (let* ((p (elark-mail-compose--parse))
             (h (plist-get p :headers)))
        (should (equal "a@x.com" (elark-mail-compose--header h "to")))
        (should (equal "Greetings" (elark-mail-compose--header h "subject")))
        (should-not (elark-mail-compose--header h "cc"))
        (should (equal "Hello\nthere" (plist-get p :body)))))
    ;; Reply shape: no headers, whole buffer is body.
    (elark-mail-test--with-compose 'reply "msg1" nil
      (insert "Sounds good!")
      (let ((p (elark-mail-compose--parse)))
        (should-not (plist-get p :headers))
        (should (equal "Sounds good!" (plist-get p :body)))))))

(ert-deftest elark-mail-test-compose-send-reply ()
  "C-c C-c on a reply fires mail +reply with the composed body."
  (let (sent)
    (cl-letf (((symbol-function 'elark--run-command)
               (lambda (args &rest _) (setq sent args)))
              ((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
      (elark-mail-test--with-compose 'reply "om_9" nil
        (insert "The print works gets my vote")
        (elark-mail-compose-send)))
    (should (equal '("mail" "+reply" "--message-id" "om_9"
                     "--body" "The print works gets my vote"
                     "--confirm-send")
                   sent))
    ;; Panel closed after send.
    (should-not (get-buffer elark-mail-compose--buffer-name))))

(ert-deftest elark-mail-test-compose-send-new-validates ()
  "New mail requires To and Subject before anything is sent."
  (let (sent)
    (cl-letf (((symbol-function 'elark--run-command)
               (lambda (args &rest _) (setq sent args)))
              ((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
      (elark-mail-test--with-compose 'new nil
          '(("To" . "") ("Cc" . "") ("Subject" . ""))
        (goto-char (point-max))
        (insert "body text")
        (should-error (elark-mail-compose-send) :type 'user-error)
        (should-not sent)))))

(provide 'elark-mail-test)
;;; elark-mail-test.el ends here
