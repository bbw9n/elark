;;; elark-im-test.el --- Tests for elark-im.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 bbw9n

;; Author: bbw9n <bbw9nio@gmail.com>
;; Assisted-by: Claude:claude-opus-5
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'ert)

(let ((root (expand-file-name ".." (file-name-directory (or load-file-name (buffer-file-name))))))
  (dolist (sub '("." "core" "ui" "domain" "ai"))
    (add-to-list 'load-path (expand-file-name sub root))))

(require 'elark-im)

;; Keep tests hermetic: never touch the on-disk name cache.
(setq elark-contact-cache-file nil
      elark-contact--cache-loaded t)

;;;; Chat parsing

(ert-deftest elark-im-test-chat-id ()
  (should (equal (elark-im--chat-id-of '((chat_id . "oc_123"))) "oc_123"))
  (should (equal (elark-im--chat-id-of '((id . "456"))) "456")))

(ert-deftest elark-im-test-chat-name ()
  (should (equal (elark-im--chat-name-of '((name . "Dev Team"))) "Dev Team"))
  (should (equal (elark-im--chat-name-of '((title . "General"))) "General"))
  (should (equal (elark-im--chat-name-of '((foo . "bar"))) "(unnamed)")))

(ert-deftest elark-im-test-chat-type ()
  (should (equal (elark-im--chat-type '((chat_type . "group"))) "group"))
  (should (equal (elark-im--chat-type '((foo . "bar"))) "")))

(ert-deftest elark-im-test-chat-member-count ()
  (should (equal (elark-im--chat-member-count '((member_count . 5))) "5"))
  (should (equal (elark-im--chat-member-count '((foo . "bar"))) "")))

;;;; Extract chats

(ert-deftest elark-im-test-extract-chats-items ()
  (let ((data '((items . (((chat_id . "1")) ((chat_id . "2")))))))
    (should (= (length (elark-im--extract-chats data)) 2))))

(ert-deftest elark-im-test-extract-chats-nested ()
  (let ((data '((data . ((chats . (((chat_id . "1")))))))))
    (should (= (length (elark-im--extract-chats data)) 1))))

(ert-deftest elark-im-test-extract-chats-empty ()
  (should (null (elark-im--extract-chats nil))))

(ert-deftest elark-im-test-extract-null-collections ()
  "Null chats/messages collections extract to nil, not `:null'."
  (should-not (elark-im--extract-chats
               '((data . ((chats . :null))))))
  (should-not (elark-im--extract-messages
               '((data . ((has_more . :false) (messages . :null)))))))

;;;; Message parsing

(ert-deftest elark-im-test-msg-id ()
  (should (equal (elark-im--msg-id '((message_id . "m_123"))) "m_123"))
  (should (equal (elark-im--msg-id '((id . "456"))) "456")))

(ert-deftest elark-im-test-msg-sender ()
  (should (equal (elark-im--msg-sender '((sender_name . "Alice"))) "Alice"))
  (should (equal (elark-im--msg-sender '((sender . ((name . "Bob"))))) "Bob"))
  (should (equal (elark-im--msg-sender '((foo . "bar"))) "unknown")))

(ert-deftest elark-im-test-msg-content ()
  (should (equal (elark-im--msg-content '((text . "hello"))) "hello"))
  (should (equal (elark-im--msg-content '((content . "world"))) "world"))
  (should (equal (elark-im--msg-content '((body . "text body"))) "text body"))
  (should (equal (elark-im--msg-content '((body . ((text . "nested"))))) "nested"))
  (should (equal (elark-im--msg-content '((foo . "bar"))) "")))

(ert-deftest elark-im-test-msg-type ()
  (should (equal (elark-im--msg-type '((msg_type . "image"))) "image"))
  (should (equal (elark-im--msg-type '((foo . "bar"))) "text")))

(ert-deftest elark-im-test-msg-time-numeric ()
  (let ((result (elark-im--msg-time '((create_time . 1700000000)))))
    (should (stringp result))
    (should (string-match-p "^[0-9]\\{4\\}-" result))))

(ert-deftest elark-im-test-msg-time-string ()
  (should (equal (elark-im--msg-time '((create_time . "2026-04-11")))
                 "2026-04-11")))

(ert-deftest elark-im-test-msg-time-nil ()
  (should (equal (elark-im--msg-time '((foo . "bar"))) "")))

;;;; Extract messages

(ert-deftest elark-im-test-extract-messages-items ()
  (let ((data '((items . (((message_id . "1")) ((message_id . "2")))))))
    (should (= (length (elark-im--extract-messages data)) 2))))

(ert-deftest elark-im-test-extract-messages-nested ()
  (let ((data '((data . ((messages . (((message_id . "1")))))))))
    (should (= (length (elark-im--extract-messages data)) 1))))

(ert-deftest elark-im-test-extract-messages-empty ()
  (should (null (elark-im--extract-messages nil))))

;;;; Make entries

(ert-deftest elark-im-test-insert-chat ()
  (let* ((elark-contact--user-cache (make-hash-table :test 'equal))
         (chat '((chat_id . "c1")
                 (name . "Dev Team")
                 (chat_type . "group")
                 (description . "Engineering chat")
                 (create_time . "2026-01-04T09:08:05Z")
                 (member_count . 10))))
    (with-temp-buffer
      (elark-im--insert-chat chat)
      (goto-char (point-min))
      ;; Title line present
      (should (search-forward "Dev Team" nil t))
      ;; Fields present
      (should (search-forward "group" nil t))
      (should (search-forward "Engineering chat" nil t))
      (should (search-forward "2026-01-04 09:08" nil t))
      (should (search-forward "10" nil t))
      ;; Text property covers the section
      (goto-char (point-min))
      (should (equal (get-text-property (point) 'elark-chat-id) "c1"))
      (should (equal (get-text-property (point) 'elark-chat-name) "Dev Team")))))

;;;; Schema fixes: textual types and deleted filter

(ert-deftest elark-im-test-msg-textual-p ()
  "Text, post, and markdown are textual; image/file/sticker are not."
  (should (elark-im--msg-textual-p '((msg_type . "text"))))
  (should (elark-im--msg-textual-p '((msg_type . "post"))))
  (should (elark-im--msg-textual-p '((msg_type . "markdown"))))
  (should-not (elark-im--msg-textual-p '((msg_type . "image"))))
  (should-not (elark-im--msg-textual-p '((msg_type . "file"))))
  (should-not (elark-im--msg-textual-p '((msg_type . "sticker")))))

(ert-deftest elark-im-test-msg-deleted-p ()
  "Only `deleted: t' counts as deleted (not :false, not missing)."
  (should     (elark-im--msg-deleted-p '((deleted . t))))
  (should-not (elark-im--msg-deleted-p '((deleted . :false))))
  (should-not (elark-im--msg-deleted-p '((other . "field")))))

(ert-deftest elark-im-test-extract-messages-filters-deleted ()
  "`extract-messages' drops messages flagged `deleted: t'."
  (let ((data `((data . ((has_more . :false)
                         (messages . (((message_id . "m1") (deleted . :false))
                                      ((message_id . "m2") (deleted . t))
                                      ((message_id . "m3") (deleted . :false)))))))))
    (let ((extracted (elark-im--extract-messages data)))
      (should (= 2 (length extracted)))
      (should (equal '("m1" "m3")
                     (mapcar (lambda (m) (alist-get 'message_id m)) extracted))))))

(ert-deftest elark-im-test-insert-message-post-no-prefix ()
  "A `post' message renders content directly, no \"[post message]: \" prefix."
  (with-temp-buffer
    (elark-im--insert-message
     '((message_id . "m1") (msg_type . "post") (create_time . "2026-05-27 20:17")
       (sender . ((name . "陈大伟"))) (content . "Hello readable text")))
    (let ((text (buffer-substring-no-properties (point-min) (point-max))))
      (should     (string-match-p "Hello readable text" text))
      (should-not (string-match-p "\\[post message\\]" text)))))

(ert-deftest elark-im-test-chat-mode ()
  "`elark-im--chat-mode' reads the `chat_mode' field (group/topic)."
  (should (equal "group" (elark-im--chat-mode '((chat_mode . "group")))))
  (should (equal "topic" (elark-im--chat-mode '((chat_mode . "topic")))))
  (should (equal ""      (elark-im--chat-mode '()))))

(ert-deftest elark-im-test-insert-chat-shows-mode ()
  "`insert-chat' renders a Mode: line populated from `chat_mode'."
  (with-temp-buffer
    (elark-im--insert-chat '((chat_id . "c1") (name . "Demo") (chat_mode . "topic")))
    (let ((text (buffer-substring-no-properties (point-min) (point-max))))
      (should (string-match-p "Mode" text))
      (should (string-match-p "topic" text)))))

(ert-deftest elark-im-test-insert-message-image-labelled ()
  "An `image' message is still labelled (since the content is metadata, not body)."
  (with-temp-buffer
    (elark-im--insert-message
     '((message_id . "m2") (msg_type . "image") (create_time . "2026-05-27 20:17")
       (sender . ((name . "alice"))) (content . "{\"image_key\":\"abc\"}")))
    (let ((text (buffer-substring-no-properties (point-min) (point-max))))
      (should (string-match-p "\\[image\\]" text)))))

;;;; Async owner resolution

(ert-deftest elark-im-test-owner-render-never-blocks ()
  "Chat rendering uses cache-or-raw-id; it must never fetch synchronously."
  (let ((elark-contact--user-cache (make-hash-table :test 'equal)))
    (cl-letf (((symbol-function 'elark-contact-get-user-sync)
               (lambda (&rest _) (error "sync fetch during render"))))
      ;; Cache miss → raw id, tagged for the async resolver.
      (with-temp-buffer
        (elark-im--insert-chat '((chat_id . "c1") (name . "Demo")
                                (owner_id . "ou_abc")))
        (goto-char (point-min))
        (should (search-forward "ou_abc" nil t))
        (should (equal '("ou_abc" . "open_id")
                       (get-text-property (match-beginning 0)
                                          'elark-contact-ref))))
      ;; Cache hit → name shown directly.
      (elark-contact--cache-put "ou_abc" "open_id" "Alice")
      (with-temp-buffer
        (elark-im--insert-chat '((chat_id . "c1") (name . "Demo")
                                (owner_id . "ou_abc")))
        (goto-char (point-min))
        (should (search-forward "Alice" nil t))))))

(ert-deftest elark-im-test-owner-async-patch ()
  "The async resolver patches raw ids in place, preserving section props."
  (let ((elark-contact--user-cache (make-hash-table :test 'equal))
        (fetched nil))
    (cl-letf (((symbol-function 'elark--run-command)
               (lambda (args callback &rest _)
                 (push args fetched)
                 (funcall callback
                          '((data . ((user . ((name . "Alice"))))))))))
      (with-temp-buffer
        ;; Two chats, same unresolved owner → ONE lookup, both patched.
        (elark-im--insert-chat '((chat_id . "c1") (name . "One")
                                (owner_id . "ou_abc")))
        (elark-im--insert-chat '((chat_id . "c2") (name . "Two")
                                (owner_id . "ou_abc")))
        (elark-contact-resolve-buffer-async (current-buffer))
        (should (= 1 (length fetched)))
        (let ((text (buffer-substring-no-properties (point-min) (point-max))))
          (should-not (string-match-p "ou_abc" text))
          (should (= 2 (with-temp-buffer
                         (insert text)
                         (count-matches "Alice" (point-min) (point-max))))))
        ;; Section property survived the splice.
        (goto-char (point-min))
        (search-forward "Alice")
        (should (equal "c1" (get-text-property (match-beginning 0)
                                               'elark-chat-id)))
        ;; Name landed in the cache for future renders.
        (should (equal "Alice"
                       (elark-contact--cache-get "ou_abc" "open_id")))))))

(ert-deftest elark-im-test-append-sent-message ()
  "A sent message is appended in place — no erase, no refetch."
  (with-temp-buffer
    (setq-local elark-im--chat-id "oc_1"
                elark-im--chat-name "Demo"
                elark-im--messages nil)
    (elark-im--insert-message
     '((message_id . "m1") (msg_type . "text")
       (create_time . "2026-10-04 10:00")
       (sender . ((name . "alice"))) (content . "existing message")))
    (setq-local elark-im--messages '(((message_id . "m1"))))
    (let ((before (buffer-substring-no-properties (point-min) (point-max))))
      (elark-im--append-sent-message
       (current-buffer) "my new reply"
       '((data . ((message_id . "om_new")))))
      (let ((text (buffer-substring-no-properties (point-min) (point-max))))
        ;; Old content intact (prefix preserved → nothing was erased).
        (should (string-prefix-p before text))
        (should (string-match-p "my new reply" text))
        (should (string-match-p "Me" text)))
      (should (= 2 (length elark-im--messages)))
      ;; Echoed entry carries the server message id for reply-at-point.
      (goto-char (point-max))
      (search-backward "my new reply")
      (should (equal "om_new"
                     (get-text-property (point) 'elark-message-id))))))

(ert-deftest elark-im-test-send-appends-instead-of-refreshing ()
  "Send and reply callbacks echo locally; the old full refresh is gone."
  (let (sent-args)
    (cl-letf (((symbol-function 'read-string)
               (lambda (&rest _) "hello there"))
              ((symbol-function 'elark-im-chat-refresh)
               (lambda (&rest _) (error "full refresh must not run")))
              ((symbol-function 'elark--run-command)
               (lambda (args callback &rest _)
                 (setq sent-args args)
                 (funcall callback '((data . ((message_id . "om_1"))))))))
      (with-temp-buffer
        (setq-local elark-im--chat-id "oc_1"
                    elark-im--chat-name "Demo"
                    elark-im--messages nil)
        (elark-im-send "oc_1")
        (should (equal '("im" "+messages-send" "--chat-id" "oc_1"
                         "--text" "hello there")
                       sent-args))
        (should (string-match-p "hello there"
                                (buffer-substring-no-properties
                                 (point-min) (point-max))))
        ;; Reply path too.
        (goto-char (point-min))
        (elark-im-reply)
        (should (equal "+messages-reply" (cadr sent-args)))
        (should (= 2 (length elark-im--messages)))))))

(ert-deftest elark-im-test-thread-replies-render-as-tree ()
  "THREAD-chat replies render nested under their root, oldest first,
deleted ones dropped; each reply keeps its own message id at point."
  (with-temp-buffer
    (elark-im--insert-message
     '((message_id . "root1") (msg_type . "text")
       (create_time . "2026-10-04 10:00")
       (sender . ((name . "alice")))
       (content . "root question")
       (thread_replies
        . (((message_id . "r2") (msg_type . "text")
            (create_time . "2026-10-04 10:20")
            (sender . ((name . "carol"))) (content . "second reply"))
           ((message_id . "rdel") (msg_type . "text") (deleted . t)
            (create_time . "2026-10-04 10:10")
            (sender . ((name . "x"))) (content . "deleted reply"))
           ((message_id . "r1") (msg_type . "text")
            (create_time . "2026-10-04 10:05")
            (sender . ((name . "bob"))) (content . "first reply"))))))
    (let ((text (buffer-substring-no-properties (point-min) (point-max))))
      ;; Both live replies present, behind the gutter, in time order.
      (should (string-match-p "│ .*first reply" text))
      (should (string-match-p "│ .*second reply" text))
      (should-not (string-match-p "deleted reply" text))
      (should (< (string-match "first reply" text)
                 (string-match "second reply" text)))
      ;; Root content is NOT indented.
      (should (string-match-p "^root question" text)))
    ;; Reply-at-point targets the reply, not the root.
    (goto-char (point-min))
    (search-forward "first reply")
    (should (equal "r1" (get-text-property (point) 'elark-message-id)))
    (goto-char (point-min))
    (search-forward "root question")
    (should (equal "root1" (get-text-property (point) 'elark-message-id)))))

(ert-deftest elark-im-test-reply-to-quote ()
  "DEFAULT-chat replies show a dim quote of their parent message."
  (with-temp-buffer
    (setq-local elark-im--messages
                '(((message_id . "m1") (msg_type . "text")
                   (sender . ((name . "alice")))
                   (content . "the original question about envhub"))
                  ((message_id . "m2") (msg_type . "text")
                   (reply_to . "m1")
                   (sender . ((name . "bob")))
                   (content . "an answer"))))
    (dolist (m elark-im--messages) (elark-im--insert-message m))
    (let ((text (buffer-substring-no-properties (point-min) (point-max))))
      (should (string-match-p "↳ alice: the original question" text))
      ;; Quote sits between bob's header and his content.
      (should (< (string-match "bob" text)
                 (string-match "↳ alice" text)
                 (string-match "an answer" text))))
    ;; Parent outside the loaded window → generic marker, no crash.
    (erase-buffer)
    (setq-local elark-im--messages
                '(((message_id . "m3") (reply_to . "gone")
                   (msg_type . "text") (sender . ((name . "bob")))
                   (content . "orphan reply"))))
    (elark-im--insert-message (car elark-im--messages))
    (should (string-match-p "↳ (reply to an earlier message)"
                            (buffer-substring-no-properties
                             (point-min) (point-max))))))

;;;; Inline media rendering

(ert-deftest elark-im-test-scan-media-markers ()
  "Image and file markers are extracted with their keys."
  (with-temp-buffer
    (insert "hello\n"
            "![Image](img_v3_0215r_e3e90b95-6806-42dd-a9a1-ecb3e40f206h)\n"
            "some text [Media: file_v3_0015l_2121369e-a8e0-4f17-917b-a1ffb7af03hu]\n"
            "![Image](img_v3_0215s_9cfbd5df-f884-43ab-b7a8-54191cc64b2h)\n")
    (let ((found (elark-im--scan-media-markers (point-min) (point-max))))
      (should (= 3 (length found)))
      (should (equal '(file image image) (sort (mapcar #'car found) #'string<)))
      (should (member "img_v3_0215r_e3e90b95-6806-42dd-a9a1-ecb3e40f206h"
                      (mapcar #'cadr found)))
      (should (member "file_v3_0015l_2121369e-a8e0-4f17-917b-a1ffb7af03hu"
                      (mapcar #'cadr found))))))

(ert-deftest elark-im-test-insert-message-marks-media ()
  "Inserted messages carry openable media properties on their markers."
  (with-temp-buffer
    (elark-im--insert-message
     '((message_id . "om_1") (msg_type . "post") (create_time . "2026-10-04 09:00")
       (sender . ((name . "alice")))
       (content . "see pic\n![Image](img_v3_abc-123h)\n[Media: file_v3_def-456u]")))
    ;; Image marker gets key/kind/msg-id and the open keymap.
    (goto-char (point-min))
    (search-forward "![Image]")
    (let ((pos (match-beginning 0)))
      (should (equal "img_v3_abc-123h" (get-text-property pos 'elark-media-key)))
      (should (eq 'image (get-text-property pos 'elark-media-kind)))
      (should (equal "om_1" (get-text-property pos 'elark-media-msg-id)))
      (should (keymapp (get-text-property pos 'keymap))))
    ;; File marker is a link.
    (goto-char (point-min))
    (search-forward "[Media:")
    (let ((pos (match-beginning 0)))
      (should (equal "file_v3_def-456u" (get-text-property pos 'elark-media-key)))
      (should (eq 'file (get-text-property pos 'elark-media-kind)))
      (should (eq 'link (get-text-property pos 'face))))))

(ert-deftest elark-im-test-media-download-relative-output ()
  "Resource downloads run inside the cache dir with a relative --output.
The CLI rejects absolute output paths, so the process cwd carries
the destination."
  (let* ((elark-im-media-cache-directory
          (make-temp-file "elark-im-media-test" t))
         (seen-args nil) (seen-dir nil))
    (cl-letf (((symbol-function 'elark--run-command)
               (lambda (args callback &rest _)
                 (setq seen-args args
                       seen-dir default-directory)
                 (funcall callback '((data . ((saved_path . "/x/img.jpg"))))))))
      (let (got)
        (elark-im--download-resource "om_1" "img_v3_k1h" "image"
                                    (lambda (path) (setq got path)))
        (should (equal "/x/img.jpg" got))
        (should (equal (file-name-as-directory elark-im-media-cache-directory)
                       seen-dir))
        (should (equal '("im" "+messages-resources-download"
                         "--message-id" "om_1"
                         "--file-key" "img_v3_k1h"
                         "--type" "image"
                         "--output" "img_v3_k1h")
                       seen-args))))
    (delete-directory elark-im-media-cache-directory t)))

(ert-deftest elark-im-test-media-cached-lookup ()
  "Cache lookup finds a key's file regardless of extension."
  (let ((elark-im-media-cache-directory
         (make-temp-file "elark-im-media-test" t)))
    (unwind-protect
        (progn
          (should-not (elark-im--media-cached "img_v3_k2h"))
          (with-temp-file (expand-file-name
                           "img_v3_k2h.png"
                           elark-im-media-cache-directory)
            (insert "fake"))
          (should (string-suffix-p "img_v3_k2h.png"
                                   (elark-im--media-cached "img_v3_k2h"))))
      (delete-directory elark-im-media-cache-directory t))))

(ert-deftest elark-im-test-display-image-caps-longer-side ()
  "Inline images are fitted to a square box so the longer side is capped.
Regression: only width was capped, so a tall phone screenshot
rendered thousands of pixels high."
  (let ((elark-im-image-max-size 100)
        (seen nil))
    (cl-letf (((symbol-function 'create-image)
               (lambda (_path &rest args)
                 (setq seen args)
                 '(image :type jpeg))))
      (with-temp-buffer
        (insert "![Image](img_v3_k3h)")
        (elark-im--display-image (point-min) (point-max)
                                "img_v3_k3h" "/x/img.jpg")
        (should (equal 100 (plist-get (cddr seen) :max-width)))
        (should (equal 100 (plist-get (cddr seen) :max-height)))
        (should (get-text-property (point-min) 'display))))))

(ert-deftest elark-im-test-video-file-p ()
  "Video detection goes by extension, case-insensitively."
  (should (elark-im--video-file-p "/x/clip.mp4"))
  (should (elark-im--video-file-p "/x/clip.MOV"))
  (should-not (elark-im--video-file-p "/x/report.pdf"))
  (should-not (elark-im--video-file-p "/x/noext")))

(ert-deftest elark-im-test-thumbnail-does-not-shadow-media-cache ()
  "Thumbnails live apart so the KEY.* media glob never returns them."
  (let ((elark-im-media-cache-directory
         (make-temp-file "elark-im-thumb-test" t)))
    (unwind-protect
        (let ((thumb (elark-im--thumbnail-file "file_v3_k4u")))
          (make-directory (file-name-directory thumb) t)
          (with-temp-file thumb (insert "fake"))
          (should (equal thumb (elark-im--thumbnail-cached "file_v3_k4u")))
          ;; The video itself is still considered un-cached.
          (should-not (elark-im--media-cached "file_v3_k4u")))
      (delete-directory elark-im-media-cache-directory t))))

(ert-deftest elark-im-test-video-preview-renders-thumbnail ()
  "A video marker gets a thumbnail display; a non-video file stays a link."
  (let ((elark-im-media-cache-directory
         (make-temp-file "elark-im-vprev-test" t))
        (created nil))
    (unwind-protect
        (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
                  ((symbol-function 'executable-find)
                   (lambda (prog &rest _) (equal prog "ffmpeg")))
                  ((symbol-function 'create-image)
                   (lambda (path &rest _)
                     (push path created) '(image :type jpeg)))
                  ;; Download hands back a video for k5, a pdf for k6.
                  ((symbol-function 'elark-im--download-resource)
                   (lambda (_id key _type callback)
                     (funcall callback
                              (if (equal key "file_v3_k5u") "/x/k5.mp4" "/x/k6.pdf"))))
                  ;; ffmpeg stub: write the thumbnail synchronously.
                  ((symbol-function 'elark-im--make-video-thumb)
                   (lambda (_video key callback)
                     (let ((thumb (elark-im--thumbnail-file key)))
                       (make-directory (file-name-directory thumb) t)
                       (with-temp-file thumb (insert "fake"))
                       (funcall callback thumb)))))
          (with-temp-buffer
            (elark-im--insert-message
             '((message_id . "om_1") (msg_type . "post")
               (create_time . "2026-10-04 10:00")
               (sender . ((name . "alice")))
               (content . "[Media: file_v3_k5u]\n[Media: file_v3_k6u]")))
            ;; Video marker got the thumbnail display…
            (goto-char (point-min))
            (search-forward "file_v3_k5u]")
            (should (get-text-property (match-beginning 0) 'display))
            (should (cl-some (lambda (p) (string-match-p "thumbs/file_v3_k5u" p))
                             created))
            ;; …the pdf marker stayed a plain link.
            (search-forward "file_v3_k6u]")
            (should-not (get-text-property (match-beginning 0) 'display))
            (should (eq 'link (get-text-property (match-beginning 0) 'face)))))
      (delete-directory elark-im-media-cache-directory t))))

(ert-deftest elark-im-test-media-cache-dir-persistent ()
  "Default media cache lives under XDG cache home, not the temp dir.
Regression: a temp-dir cache is purged by the OS, forcing media to
re-download every session."
  (let ((xdg (make-temp-file "elark-im-xdg-test" t))
        (old (getenv "XDG_CACHE_HOME"))
        (elark-im-media-cache-directory nil))
    (unwind-protect
        (progn
          (setenv "XDG_CACHE_HOME" xdg)
          ;; XDG_CACHE_HOME set → cache lives under it.
          (let ((dir (elark-im--media-cache-dir)))
            (should (string-prefix-p (file-name-as-directory xdg) dir))
            (should (file-directory-p dir)))
          ;; No XDG_CACHE_HOME → falls back to ~/.cache, never the
          ;; OS temp dir.
          (setenv "XDG_CACHE_HOME" nil)
          (should (string-prefix-p
                   (file-name-as-directory (expand-file-name "~/.cache"))
                   (expand-file-name (elark-im--media-cache-dir)))))
      (setenv "XDG_CACHE_HOME" old)
      (delete-directory xdg t))))

(ert-deftest elark-im-test-media-cache-prune-lru ()
  "Pruning deletes the oldest files first and keeps the cache under cap."
  (let ((elark-im-media-cache-directory
         (make-temp-file "elark-im-prune-test" t)))
    (unwind-protect
        (let ((old (expand-file-name "img_old.jpg" elark-im-media-cache-directory))
              (mid (expand-file-name "img_mid.jpg" elark-im-media-cache-directory))
              (new (expand-file-name "img_new.jpg" elark-im-media-cache-directory)))
          (dolist (spec `((,old . 10) (,mid . 10) (,new . 10)))
            (with-temp-file (car spec)
              (insert (make-string (cdr spec) ?x))))
          ;; Age the files: old < mid < new.
          (set-file-times old (time-subtract (current-time) 200))
          (set-file-times mid (time-subtract (current-time) 100))
          ;; Cap at 20 bytes → the 10-byte oldest file must go.
          (let ((elark-im-media-cache-max-bytes 20))
            (elark-im--media-cache-prune))
          (should-not (file-exists-p old))
          (should (file-exists-p mid))
          (should (file-exists-p new))
          ;; nil cap → no pruning.
          (let ((elark-im-media-cache-max-bytes nil))
            (elark-im--media-cache-prune))
          (should (file-exists-p new)))
      (delete-directory elark-im-media-cache-directory t))))

(ert-deftest elark-im-test-prepend-older-undisplayed-buffer ()
  "Prepending older messages into an undisplayed buffer must not error.
Regression: the async callback called `recenter'/`window-start'
against the selected window, which showed a different buffer —
\"`recenter'ing a window that does not display current-buffer\"."
  (with-temp-buffer
    (setq-local elark-im--chat-id "oc_1"
                elark-im--chat-name "Demo"
                elark-im--messages nil)
    (elark-im--insert-message
     '((message_id . "m1") (msg_type . "text")
       (create_time . "2026-10-04 10:00")
       (sender . ((name . "alice"))) (content . "newest")))
    (goto-char (point-min))
    ;; The temp buffer is NOT displayed in any window.
    (elark-im--prepend-older
     '((data . ((has_more . :false) (page_token . "")
                (messages . (((message_id . "m0") (msg_type . "text")
                              (create_time . "2026-10-04 09:00")
                              (sender . ((name . "bob")))
                              (content . "older"))))))))
    (let ((text (buffer-substring-no-properties (point-min) (point-max))))
      (should (string-match-p "older" text))
      (should (string-match-p "newest" text)))))

(provide 'elark-im-test)
;;; elark-im-test.el ends here
