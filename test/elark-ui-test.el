;;; elark-ui-test.el --- Tests for elark-ui.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 bbw9n

;; Author: bbw9n <bbw9nio@gmail.com>
;; Assisted-by: Claude:claude-opus-5
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; ERT tests for elark shared UI helpers (the loading spinner).

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Add project directories to load-path
(let ((root (expand-file-name ".." (file-name-directory (or load-file-name (buffer-file-name))))))
  (dolist (sub '("." "core" "ui" "domain" "ai"))
    (add-to-list 'load-path (expand-file-name sub root))))

(require 'elark-ui)

;;;; Spinner tests

(ert-deftest elark-test-spinner-frames-nonempty ()
  "The default spinner frame set is a non-empty vector of strings."
  (should (vectorp elark-spinner-frames))
  (should (> (length elark-spinner-frames) 0))
  (should (cl-every #'stringp (append elark-spinner-frames nil))))

(ert-deftest elark-test-spinner-renders-first-frame-synchronously ()
  "RENDER-FN is called immediately with the first frame on start."
  (let ((seen nil))
    (let ((sp (elark-spinner-start
               (lambda (frame) (push frame seen) t))))
      (elark-spinner-stop sp))
    (should (equal seen (list (aref elark-spinner-frames 0))))))

(ert-deftest elark-test-spinner-no-timer-when-first-render-nil ()
  "When RENDER-FN returns nil immediately, no live timer is created."
  (let ((sp (elark-spinner-start (lambda (_frame) nil))))
    (should-not (and (timerp sp) (memq sp timer-list)))))

(ert-deftest elark-test-spinner-on-stop-fires-on-immediate-stop ()
  "ON-STOP runs when RENDER-FN signals stop on the first frame."
  (let ((stopped nil))
    (elark-spinner-start (lambda (_frame) nil) nil nil nil
                        (lambda () (setq stopped t)))
    (should stopped)))

(ert-deftest elark-test-spinner-stop-cancels-timer ()
  "`elark-spinner-stop' removes the timer from `timer-list'."
  (let ((sp (elark-spinner-start (lambda (_frame) t))))
    (should (memq sp timer-list))
    (elark-spinner-stop sp)
    (should-not (memq sp timer-list))))

(ert-deftest elark-test-spinner-stop-tolerates-nil ()
  "`elark-spinner-stop' is a no-op on nil."
  (should-not (elark-spinner-stop nil)))

;;;; Progress bar

(ert-deftest elark-test-bar-frames ()
  "`elark-ui-bar-frames' is a faced left-to-right fill that loops."
  (should (vectorp elark-ui-bar-frames))
  (should (= 10 (length elark-ui-bar-frames)))
  (should (equal "█░░░░░░░░░" (substring-no-properties (aref elark-ui-bar-frames 0))))
  (should (equal "██████████" (substring-no-properties (aref elark-ui-bar-frames 9))))
  ;; Filled cells carry the bar face; the track is shadowed.
  (should (eq 'elark-ui-bar (get-text-property 0 'face (aref elark-ui-bar-frames 4))))
  (should (eq 'shadow (get-text-property 9 'face (aref elark-ui-bar-frames 4)))))

;;;; Field rendering tests

(defun elark-test-ui--render (thunk)
  "Return the buffer text produced by calling THUNK in a temp buffer."
  (with-temp-buffer
    (funcall thunk)
    (buffer-string)))

(ert-deftest elark-test-ui-insert-field-basic ()
  "A non-empty value renders an aligned, faced \"label: value\" line."
  (let ((text (elark-test-ui--render
               (lambda () (elark-ui-insert-field "Start" "10:00")))))
    (should (equal text (concat "  " (format "%-14s" "Start:") "10:00\n")))
    ;; The label (through the colon) carries `font-lock-keyword-face'.
    (should (eq (get-text-property 0 'face text) 'font-lock-keyword-face))))

(ert-deftest elark-test-ui-insert-field-skips-empty-and-nil ()
  "Nil, empty, and non-string values insert nothing."
  (dolist (v (list nil "" 42))
    (should (equal "" (elark-test-ui--render
                       (lambda () (elark-ui-insert-field "X" v)))))))

(ert-deftest elark-test-ui-insert-field-width-and-indent ()
  "WIDTH controls label padding and INDENT the leading whitespace."
  (should (equal (concat (format "%-10s" "From:") "a@b\n")
                 (elark-test-ui--render
                  (lambda () (elark-ui-insert-field "From" "a@b" 10 ""))))))

(ert-deftest elark-test-ui-insert-field-multiline-aligned ()
  "Continuation lines are indented under the value column."
  (let ((prefix (concat "  " (format "%-4s" "K:"))))
    (should (equal (concat prefix "one\n" (make-string (length prefix) ?\s) "two\n")
                   (elark-test-ui--render
                    (lambda () (elark-ui-insert-field "K" "one\ntwo" 4)))))))

;;;; Separator / title tests

(ert-deftest elark-test-ui-separator ()
  "`elark-ui-separator' returns WIDTH rule chars (default 60)."
  (should (equal (make-string 60 elark-ui-rule-char) (elark-ui-separator)))
  (should (equal (make-string 7 elark-ui-rule-char) (elark-ui-separator 7)))
  (should (= 7 (length (elark-ui-separator 7)))))

(ert-deftest elark-test-ui-insert-title-scaled-rule ()
  "Title is bold; the rule scales to title length clamped to [20, max]."
  (let ((text (elark-test-ui--render
               (lambda () (elark-ui-insert-title "Hi")))))
    ;; "Hi" is shorter than 20, so the rule is clamped up to 20.
    (should (equal text (concat "Hi\n" (make-string 20 elark-ui-rule-char) "\n\n")))
    (should (eq (get-text-property 0 'face text) 'bold)))
  ;; A long title is clamped down to the max width.
  (let* ((long (make-string 80 ?x))
         (text (elark-test-ui--render
                (lambda () (elark-ui-insert-title long 1 72)))))
    (should (equal text (concat long "\n" (make-string 72 elark-ui-rule-char) "\n")))))

;;;; Section navigation tests

(defun elark-test-ui--sections-buffer ()
  "Create a buffer with two `elark-id'-tagged sections separated by a gap."
  (let ((buf (generate-new-buffer " *elark-nav-test*")))
    (with-current-buffer buf
      (let ((a (point)))
        (insert "Alpha\nfields\n")
        (put-text-property a (point) 'elark-id "a"))
      (insert "\n")                    ; gap with no property
      (let ((b (point)))
        (insert "Beta\nfields\n")
        (put-text-property b (point) 'elark-id "b")))
    buf))

(ert-deftest elark-test-ui-next-section ()
  "`elark-ui-next-section' jumps from one section to the next's start."
  (let ((buf (elark-test-ui--sections-buffer)))
    (unwind-protect
        (with-current-buffer buf
          (goto-char (point-min))
          (elark-ui-next-section 'elark-id)
          (should (equal "b" (get-text-property (point) 'elark-id)))
          (should (looking-at-p "Beta"))
          ;; No further section: point stays put.
          (let ((here (point)))
            (elark-ui-next-section 'elark-id)
            (should (= here (point)))))
      (kill-buffer buf))))

(ert-deftest elark-test-ui-prev-section ()
  "`elark-ui-prev-section' jumps back to the previous section's start."
  (let ((buf (elark-test-ui--sections-buffer)))
    (unwind-protect
        (with-current-buffer buf
          (goto-char (point-max))
          (elark-ui-prev-section 'elark-id)
          (should (equal "b" (get-text-property (point) 'elark-id)))
          (should (looking-at-p "Beta"))
          (elark-ui-prev-section 'elark-id)
          (should (equal "a" (get-text-property (point) 'elark-id)))
          (should (looking-at-p "Alpha")))
      (kill-buffer buf))))

;;;; Code-block panes

(ert-deftest elark-test-ui-block-bg-face-fallback ()
  "`elark-ui-block-bg-face' falls back to `elark-ui-block-bg' outside Org/Markdown."
  (with-temp-buffer
    (fundamental-mode)
    (should (eq 'elark-ui-block-bg (elark-ui-block-bg-face)))))

(ert-deftest elark-test-ui-block-bg-face-org ()
  "`elark-ui-block-bg-face' returns `org-block' in Org buffers."
  (skip-unless (require 'org nil t))
  (with-temp-buffer
    (let ((org-inhibit-startup t)) (delay-mode-hooks (org-mode)))
    (should (eq 'org-block (elark-ui-block-bg-face)))))

(ert-deftest elark-test-ui-fontify-block-writes-font-lock-face ()
  "`elark-ui-fontify-block' writes `font-lock-face' onto the region.
The destination buffer never runs the source MODE — only the
resulting face properties land on the original text."
  (with-temp-buffer
    (fundamental-mode)
    (let ((start (point)))
      (insert "echo hello")
      (elark-ui-fontify-block 'sh-mode start (point))
      ;; sh-mode marks `echo' as a builtin / function-name; we don't pin
      ;; the exact face, but *some* font-lock-face must be written.
      (should (text-property-not-all
               start (point) 'font-lock-face nil)))))

(provide 'elark-ui-test)
;;; elark-ui-test.el ends here
