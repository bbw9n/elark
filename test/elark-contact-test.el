;;; elark-contact-test.el --- Tests for elark-contact.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 bbw9n

;; Author: bbw9n <bbw9nio@gmail.com>
;; Assisted-by: Claude:claude-opus-5
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'ert)

(let ((root (expand-file-name ".." (file-name-directory (or load-file-name (buffer-file-name))))))
  (dolist (sub '("." "core" "ui" "domain" "ai"))
    (add-to-list 'load-path (expand-file-name sub root))))

(require 'elark-contact)

;; Keep tests hermetic: never touch the on-disk name cache.
(setq elark-contact-cache-file nil
      elark-contact--cache-loaded t)

;;;; User extraction

(ert-deftest elark-contact-test-extract-user-nested ()
  (let ((data '((data . ((user . ((name . "Alice") (open_id . "ou_1"))))))))
    (should (equal (alist-get 'name (elark-contact--extract-user data)) "Alice"))))

(ert-deftest elark-contact-test-extract-user-flat ()
  (let ((data '((user . ((name . "Bob"))))))
    (should (equal (alist-get 'name (elark-contact--extract-user data)) "Bob"))))

(ert-deftest elark-contact-test-extract-user-data-only ()
  (let ((data '((data . ((name . "Carol") (open_id . "ou_3"))))))
    (should (equal (alist-get 'name (elark-contact--extract-user data)) "Carol"))))

;;;; Display name extraction

(ert-deftest elark-contact-test-display-name ()
  (should (equal (elark-contact--user-display-name '((name . "Alice"))) "Alice"))
  (should (equal (elark-contact--user-display-name '((display_name . "Bob"))) "Bob"))
  (should (equal (elark-contact--user-display-name '((en_name . "Carol"))) "Carol"))
  (should (equal (elark-contact--user-display-name '((foo . "bar"))) "")))

;;;; Cache

(ert-deftest elark-contact-test-cache-roundtrip ()
  (let ((elark-contact--user-cache (make-hash-table :test 'equal)))
    (should-not (elark-contact--cache-get "ou_1" "open_id"))
    (elark-contact--cache-put "ou_1" "open_id" "Alice")
    (should (equal (elark-contact--cache-get "ou_1" "open_id") "Alice"))))

(ert-deftest elark-contact-test-cache-key-includes-type ()
  (let ((elark-contact--user-cache (make-hash-table :test 'equal)))
    (elark-contact--cache-put "id_1" "open_id" "Alice")
    (elark-contact--cache-put "id_1" "user_id" "Bob")
    (should (equal (elark-contact--cache-get "id_1" "open_id") "Alice"))
    (should (equal (elark-contact--cache-get "id_1" "user_id") "Bob"))))

(ert-deftest elark-contact-test-clear-cache ()
  (let ((elark-contact--user-cache (make-hash-table :test 'equal)))
    (elark-contact--cache-put "ou_1" "open_id" "Alice")
    (clrhash elark-contact--user-cache)
    (should-not (elark-contact--cache-get "ou_1" "open_id"))))

;;;; User entries

(ert-deftest elark-contact-test-extract-users-items ()
  (let ((data '((data . ((items . (((open_id . "ou_1")) ((open_id . "ou_2")))))))))
    (should (= (length (elark-contact--extract-users data)) 2))))

(ert-deftest elark-contact-test-extract-users-flat ()
  (let ((data '((items . (((open_id . "ou_1")))))))
    (should (= (length (elark-contact--extract-users data)) 1))))

(ert-deftest elark-contact-test-make-user-entries ()
  (let* ((users '(((open_id . "ou_1")
                    (name . "Alice")
                    (en_name . "Alice L")
                    (email . "alice@example.com"))))
         (entries (elark-contact--make-user-entries users)))
    (should (= (length entries) 1))
    (should (equal (car (car entries)) "ou_1"))
    (let ((vec (cadr (car entries))))
      (should (equal (aref vec 0) "Alice"))
      (should (equal (aref vec 1) "Alice L"))
      (should (equal (aref vec 2) "alice@example.com"))
      (should (equal (aref vec 3) "ou_1")))))

(ert-deftest elark-contact-test-annotate ()
  "Annotate returns the cached name, or a tagged raw id — never fetches."
  (let ((elark-contact--user-cache (make-hash-table :test 'equal)))
    (cl-letf (((symbol-function 'elark-contact-get-user-sync)
               (lambda (&rest _) (error "sync fetch from annotate"))))
      ;; Miss: raw id carrying the ref property.
      (let ((s (elark-contact-annotate "ou_1")))
        (should (equal "ou_1" (substring-no-properties s)))
        (should (equal '("ou_1" . "open_id")
                       (get-text-property 0 'elark-contact-ref s))))
      ;; Hit: plain cached name.
      (elark-contact--cache-put "ou_1" "open_id" "Alice")
      (should (equal "Alice" (elark-contact-annotate "ou_1"))))))

(ert-deftest elark-contact-test-resolve-buffer-async ()
  "Buffer pass fetches each distinct raw id once and patches in place."
  (let ((elark-contact--user-cache (make-hash-table :test 'equal))
        (calls nil))
    (cl-letf (((symbol-function 'elark--run-command)
               (lambda (args callback &rest _)
                 (push args calls)
                 (funcall callback
                          '((data . ((user . ((name . "Bob"))))))))))
      (with-temp-buffer
        (insert "Owner: " (elark-contact-annotate "ou_2") "\n"
                "Owner: " (elark-contact-annotate "ou_2") "\n")
        (elark-contact-resolve-buffer-async (current-buffer))
        (should (= 1 (length calls)))
        (should-not (string-match-p
                     "ou_2" (buffer-substring-no-properties
                             (point-min) (point-max))))
        (should (equal "Bob" (elark-contact--cache-get "ou_2" "open_id")))))))

(ert-deftest elark-contact-test-cache-persistence-roundtrip ()
  "Names persist to disk and come back in a fresh session."
  (let* ((file (make-temp-file "elark-contact-cache" nil ".eld"))
         (elark-contact-cache-file file))
    (unwind-protect
        (progn
          ;; Session 1: resolve and flush.
          (let ((elark-contact--user-cache (make-hash-table :test 'equal))
                (elark-contact--cache-loaded t)
                (elark-contact--cache-save-timer nil))
            (elark-contact--cache-put "ou_p1" "open_id" "Alice")
            (elark-contact--cache-save))
          ;; Session 2: fresh memory, loaded lazily from disk.
          (let ((elark-contact--user-cache (make-hash-table :test 'equal))
                (elark-contact--cache-loaded nil))
            (should (equal "Alice"
                           (elark-contact--cache-get "ou_p1" "open_id")))))
      (delete-file file))))

(ert-deftest elark-contact-test-cache-persistence-ttl ()
  "Stale persisted names are dropped at load so they re-resolve."
  (let* ((file (make-temp-file "elark-contact-cache" nil ".eld"))
         (elark-contact-cache-file file))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert ";; test\n")
            (prin1 (list (cons "ou_old:open_id"
                               (cons "Oldname" (- (float-time) 1000)))
                         (cons "ou_new:open_id"
                               (cons "Newname" (float-time))))
                   (current-buffer)))
          (let ((elark-contact--user-cache (make-hash-table :test 'equal))
                (elark-contact--cache-loaded nil)
                (elark-contact-cache-ttl 500))
            (should-not (elark-contact--cache-get "ou_old" "open_id"))
            (should (equal "Newname"
                           (elark-contact--cache-get "ou_new" "open_id")))))
      (delete-file file))))

(provide 'elark-contact-test)
;;; elark-contact-test.el ends here
