;;; elark-im.el --- Lark Messenger integration -*- lexical-binding: t; -*-

;; Copyright (C) 2026 bbw9n

;; Author: bbw9n <bbw9nio@gmail.com>
;; Assisted-by: Claude:claude-opus-5
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Provides Messenger domain commands for elark: chat listing,
;; message history, send/reply messages, reactions, group chat
;; management, and real-time event subscription.
;;
;; CLI command mapping:
;;   Chat search:     im +chat-search [--query X]
;;   Chat messages:   im +chat-messages-list --chat-id X
;;   Send message:    im +messages-send --chat-id X --text X  (NO --format)
;;   Reply:           im +messages-reply --message-id X --text X  (NO --format)
;;   Search messages: im +messages-search --query X
;;   Create group:    im +chat-create --name X [--users X]
;;   Reactions:       im reactions create --params JSON --data JSON

;;; Code:

(require 'elark-core)
(require 'elark-ui)
(require 'elark-contact)
(require 'json)
(require 'transient)
(require 'elark-ai-llm)       ; smart reply: `elark-ai--call-llm'
(require 'elark-ai-skills)    ; smart reply: skill prompts

;;;; Customization

(defgroup elark-im nil
  "Lark Messenger settings."
  :group 'elark
  :prefix "elark-im-")

(defcustom elark-im-message-limit "50"
  "Default number of messages to fetch in history."
  :type 'string
  :group 'elark-im)

;;;; Buffer-local variables

(defvar-local elark-im--chat-id nil
  "Chat ID for the current buffer.")

(defvar-local elark-im--chat-name nil
  "Chat name for the current buffer.")

(defvar-local elark-im--messages nil
  "Cached messages for the current buffer.")

(defvar-local elark-im--page-token nil
  "Next-page token for fetching older messages in the current chat buffer.
nil means either no older page is known yet, or no more older messages.")

(defvar-local elark-im--has-more nil
  "Non-nil if more older messages can be fetched in the current chat buffer.")

(defvar-local elark-im--loading-older nil
  "Non-nil while an older-messages fetch is in flight.")

(defvar-local elark-im--spinner-timer nil
  "Active loading-spinner handle for the current chat buffer, or nil.")

(defvar-local elark-im--spinner-frame nil
  "Current spinner frame string while older messages are loading.")

(defvar-local elark-im--chats nil
  "Cached chat list for the current buffer.")

(defvar-local elark-im--chat-query nil
  "Last search query used to fetch the chat list.")

;;;; Event subscription process

(defvar elark-im--event-process nil
  "Long-running process for real-time event subscription.")

;;;; Chat parsing

(defun elark-im--chat-id-of (chat)
  "Extract the chat ID from CHAT."
  (or (alist-get 'chat_id chat)
      (alist-get 'id chat)))

(defun elark-im--chat-name-of (chat)
  "Extract the chat name from CHAT."
  (or (alist-get 'name chat)
      (alist-get 'chat_name chat)
      (alist-get 'title chat)
      "(unnamed)"))

(defun elark-im--chat-type (chat)
  "Extract the chat type from CHAT."
  (or (alist-get 'chat_type chat)
      (alist-get 'type chat)
      ""))

(defun elark-im--chat-mode (chat)
  "Extract the chat mode from CHAT (e.g. \"group\" / \"topic\").
This is what `lark-cli im +chat-list' returns under `chat_mode' for
each chat in the current response shape."
  (or (alist-get 'chat_mode chat)
      ""))

(defun elark-im--chat-member-count (chat)
  "Extract member count from CHAT."
  (let ((count (or (alist-get 'member_count chat)
                   (alist-get 'user_count chat))))
    (if count (format "%s" count) "")))

(defun elark-im--chat-owner-name (chat)
  "Return the owner display name from CHAT WITHOUT blocking.
Cached name when known, else the raw owner id tagged for the
async `elark-contact-resolve-buffer-async' pass — rendering must
never wait on a network round-trip per chat (the old synchronous
`elark-contact-resolve-name' froze Emacs for seconds on a fresh
list)."
  (let ((owner-id (alist-get 'owner_id chat))
        (id-type (or (alist-get 'owner_id_type chat) "open_id")))
    (if (and owner-id (not (string-empty-p owner-id)))
        (elark-contact-annotate owner-id id-type)
      "")))

(defun elark-im--extract-chats (data)
  "Extract chat list from lark-cli response DATA.
Null-safe: empty collections arrive as JSON null (`:null')."
  (or (elark--list-field data 'items)
      (elark--list-field data 'chats)
      (let ((inner (elark--list-field data 'data)))
        (or (elark--list-field inner 'items)
            (elark--list-field inner 'chats)
            (and (elark--record-list-p inner) inner)))
      (and (elark--record-list-p data)
           (or (alist-get 'chat_id (car data))
               (alist-get 'name (car data)))
           data)))

(defun elark-im--chat-description (chat)
  "Extract description from CHAT."
  (or (alist-get 'description chat) ""))

(defun elark-im--chat-create-time (chat)
  "Extract and format create time from CHAT.
Expects an ISO 8601 string like \"2026-01-04T09:08:05Z\".
Truncates to \"YYYY-MM-DD HH:MM\" for display."
  (let ((ts (alist-get 'create_time chat)))
    (if (and (stringp ts) (not (string-empty-p ts)))
        (string-replace "T" " " (substring ts 0 (min 16 (length ts))))
      "")))

(defun elark-im--insert-chat-field (label value)
  "Insert a LABEL: VALUE line if VALUE is non-empty."
  (elark-ui-insert-field label value))

(defun elark-im--insert-chat (chat)
  "Insert a multi-line section for CHAT into the current buffer."
  (let ((id (elark-im--chat-id-of chat))
        (name (elark-im--chat-name-of chat))
        (type (elark-im--chat-type chat))
        (mode (elark-im--chat-mode chat))
        (owner (elark-im--chat-owner-name chat))
        (desc (elark-im--chat-description chat))
        (created (elark-im--chat-create-time chat))
        (members (elark-im--chat-member-count chat))
        (beg (point)))
    (insert (propertize name 'face 'bold) "\n")
    (elark-im--insert-chat-field "Type" type)
    (elark-im--insert-chat-field "Mode" mode)
    ;; Owner text may carry `elark-contact-ref' (unresolved raw id) —
    ;; the async resolver pass patches it after display.
    (elark-im--insert-chat-field "Owner" owner)
    (elark-im--insert-chat-field "Description" desc)
    (elark-im--insert-chat-field "Created" created)
    (elark-im--insert-chat-field "Members" members)
    (insert "\n")
    (put-text-property beg (point) 'elark-chat-id id)
    (put-text-property beg (point) 'elark-chat-name name)))

;;;; Message parsing

(defun elark-im--msg-id (msg)
  "Extract message ID from MSG."
  (or (alist-get 'message_id msg)
      (alist-get 'id msg)))

(defun elark-im--msg-sender (msg)
  "Extract sender name from MSG."
  (or (alist-get 'sender_name msg)
      (elark--get-nested msg 'sender 'name)
      (elark--get-nested msg 'sender 'id)
      "unknown"))

(defun elark-im--msg-time (msg)
  "Extract and format message time from MSG."
  (let ((ts (or (alist-get 'create_time msg)
                (alist-get 'send_time msg)
                (alist-get 'timestamp msg))))
    (cond
     ((numberp ts) (or (elark--format-timestamp ts) ""))
     ((and (stringp ts) (string-match-p "^[0-9]+$" ts))
      (or (elark--format-timestamp ts) ""))
     ((stringp ts) ts)
     (t ""))))

(defun elark-im--msg-content (msg)
  "Extract message text content from MSG."
  (or (alist-get 'text msg)
      (alist-get 'content msg)
      (let ((body (alist-get 'body msg)))
        (when body
          (if (stringp body) body
            (or (alist-get 'text body)
                (alist-get 'content body)))))
      (elark--get-nested msg 'msg 'content)
      ""))

(defun elark-im--msg-type (msg)
  "Extract message type from MSG."
  (or (alist-get 'msg_type msg)
      (alist-get 'message_type msg)
      (alist-get 'type msg)
      "text"))

(defconst elark-im--textual-msg-types '("text" "post" "markdown")
  "Message types whose `content' field is already human-readable text.
For these, `elark-im--insert-message' renders the content directly
instead of wrapping it with a \"[type message]: …\" prefix.  Anything
not in this list (image/file/sticker/share_chat/etc.) is treated as a
non-text affordance and labelled.")

(defun elark-im--msg-textual-p (msg)
  "Return non-nil when MSG's content should be rendered as plain text."
  (member (elark-im--msg-type msg) elark-im--textual-msg-types))

(defun elark-im--msg-deleted-p (msg)
  "Return non-nil when MSG is marked deleted in the response."
  (eq (alist-get 'deleted msg) t))

;;;; Inline media rendering
;;
;; lark-cli renders post/media message content as text with resource
;; markers: images as "![Image](img_v3_…)" and files/videos as
;; "[Media: file_v3_…]".  After a message is inserted, the region is
;; scanned: image markers are replaced (display property) with the
;; actual image, downloaded on demand via
;; `im +messages-resources-download' and cached under
;; `elark-im-media-cache-directory'; file markers become actionable
;; links that download and open the file externally on RET/click.
;; The CLI only accepts a RELATIVE --output path, so downloads run
;; with `default-directory' bound to the cache dir.

(defcustom elark-im-render-images t
  "When non-nil, render image markers in chat buffers as inline images.
Only takes effect on graphical displays; in terminals the textual
marker is kept (still openable via RET)."
  :type 'boolean
  :group 'elark-im)

(defcustom elark-im-image-max-size 200
  "Pixel cap applied to an inline chat image's LONGER side.
The image is scaled with its aspect ratio preserved so that
neither width nor height exceeds this — a tall phone screenshot
gets height-capped, a wide one width-capped, so no image can
dominate the buffer.  RET/click still opens the full-size file."
  :type 'integer
  :group 'elark-im)

(defcustom elark-im-media-cache-directory nil
  "Directory for downloaded chat media.
When nil, uses a PERSISTENT per-user cache under XDG cache home
\(usually ~/.cache/elark/im-media/).  Resource file keys are
immutable, so cached media never goes stale — keeping the cache out
of variable `temporary-file-directory' means revisiting a chat after a
reboot or macOS temp cleanup does not re-download every image."
  :type '(choice (const :tag "XDG cache (persistent)" nil) directory)
  :group 'elark-im)

(defcustom elark-im-media-cache-max-bytes (* 512 1024 1024)
  "Soft cap on the media cache size, in bytes.
After each download the cache is pruned least-recently-used-first
until it fits (a cache hit refreshes a file's timestamp, so
frequently viewed media survives).  nil disables pruning."
  :type '(choice (const :tag "Unlimited" nil) integer)
  :group 'elark-im)

(defcustom elark-im-video-previews t
  "When non-nil, show an inline first-frame thumbnail for video messages.
Requires ffmpeg and a graphical display.  The video file is
downloaded in the background into the media cache (so RET plays it
instantly); non-video files keep the plain link.  Set to nil to
avoid background downloads of potentially large files."
  :type 'boolean
  :group 'elark-im)

(defconst elark-im--video-extensions
  '("mp4" "mov" "m4v" "webm" "mkv" "avi" "flv")
  "File extensions treated as video for inline thumbnails.")

(defconst elark-im--image-marker-re "!\\[Image\\](\\(img_[A-Za-z0-9_-]+\\))"
  "Match an inline image marker; group 1 is the image file key.")

(defconst elark-im--media-marker-re "\\[Media: \\(file_[A-Za-z0-9_-]+\\)\\]"
  "Match a media (file/video) marker; group 1 is the file key.")

(defun elark-im--media-cache-dir ()
  "Return the media cache directory, creating it if needed.
Persistent by default — see `elark--cache-dir'."
  (if elark-im-media-cache-directory
      (let ((dir (file-name-as-directory elark-im-media-cache-directory)))
        (make-directory dir t)
        dir)
    (elark--cache-dir "im-media")))

(defun elark-im--media-cached (key)
  "Return the cached file for resource KEY, or nil.
The extension is chosen by the CLI at download time, so match any.
A hit refreshes the file's modification time so LRU pruning keeps
recently viewed media."
  (when-let* ((path (car (file-expand-wildcards
                         (concat (elark-im--media-cache-dir) key ".*")))))
    ;; Bumping the mtime only steers LRU pruning; a read-only cache
    ;; file is still a valid hit.
    (condition-case nil (set-file-times path) (file-error nil))
    path))

(defun elark-im--video-file-p (path)
  "Return non-nil when PATH has a video file extension."
  (member (downcase (or (file-name-extension path) ""))
          elark-im--video-extensions))

(defun elark-im--thumbnail-file (key)
  "Return the thumbnail path for video KEY (may not exist yet).
Thumbnails live in a subdirectory so `elark-im--media-cached''s
KEY.* glob never mistakes a thumbnail for the video itself."
  (expand-file-name (concat "thumbs/" key ".jpg")
                    (elark-im--media-cache-dir)))

(defun elark-im--thumbnail-cached (key)
  "Return the cached thumbnail for video KEY, or nil."
  (let ((thumb (elark-im--thumbnail-file key)))
    (and (file-exists-p thumb) thumb)))

(defun elark-im--make-video-thumb (video key callback)
  "Extract VIDEO's first frame into KEY's cached thumbnail, async.
Calls CALLBACK with the thumbnail path, or nil when ffmpeg fails."
  (let ((thumb (elark-im--thumbnail-file key))
        (default-directory (elark--safe-default-directory)))
    (make-directory (file-name-directory thumb) t)
    (make-process
     :name (format "elark-im-thumb-%s" key)
     :command (list "ffmpeg" "-y" "-loglevel" "error"
                    "-i" video "-frames:v" "1" thumb)
     :noquery t
     :sentinel
     (lambda (proc _event)
       (when (memq (process-status proc) '(exit signal))
         (funcall callback
                  (and (file-exists-p thumb)
                       (> (or (file-attribute-size
                               (file-attributes thumb))
                              0)
                          0)
                       thumb)))))))

(defun elark-im--media-cache-prune ()
  "Delete least-recently-used cached media until under the size cap.
No-op when `elark-im-media-cache-max-bytes' is nil or the cache fits."
  (when elark-im-media-cache-max-bytes
    (let* ((files (directory-files-and-attributes
                   (elark-im--media-cache-dir) t "^[^.]" t))
           (total (apply #'+ 0 (mapcar (lambda (f)
                                         (or (file-attribute-size (cdr f)) 0))
                                       files))))
      (when (> total elark-im-media-cache-max-bytes)
        (dolist (f (sort files
                         (lambda (a b)
                           (time-less-p
                            (file-attribute-modification-time (cdr a))
                            (file-attribute-modification-time (cdr b))))))
          (when (> total elark-im-media-cache-max-bytes)
            (setq total (- total (or (file-attribute-size (cdr f)) 0)))
            ;; Already gone (e.g. pruned concurrently) is fine.
            (condition-case nil (delete-file (car f)) (file-error nil))))))))

(defun elark-im-media-cache-clear ()
  "Delete every file in the chat media cache."
  (interactive)
  (let ((dir (elark-im--media-cache-dir)))
    (when (yes-or-no-p (format "Delete all cached chat media in %s? " dir))
      (delete-directory dir t)
      (make-directory dir t)
      (message "Lark: media cache cleared"))))

(defun elark-im--scan-media-markers (beg end)
  "Collect media markers between BEG and END in the current buffer.
Returns a list of (KIND KEY MBEG MEND) where KIND is `image' or
`file' and MBEG/MEND bound the marker text."
  (let (found)
    (save-excursion
      (dolist (spec `((image . ,elark-im--image-marker-re)
                      (file . ,elark-im--media-marker-re)))
        (goto-char beg)
        (while (re-search-forward (cdr spec) end t)
          (push (list (car spec) (match-string-no-properties 1)
                      (match-beginning 0) (match-end 0))
                found))))
    (nreverse found)))

(defvar elark-im--media-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'elark-im-open-media-at-point)
    (define-key map [mouse-1] #'elark-im-open-media-at-point)
    map)
  "Keymap active on media markers in chat buffers.")

(defun elark-im--render-media (beg end msg-id)
  "Render media markers between BEG and END of message MSG-ID.
Images are displayed inline (asynchronously, cache-first); file
markers become openable links.  No-op without a message id — the
download API needs it."
  (when msg-id
    (let ((inhibit-read-only t))
      (pcase-dolist (`(,kind ,key ,mbeg ,mend)
                     (elark-im--scan-media-markers beg end))
        (add-text-properties
         mbeg mend
         (list 'elark-media-key key
               'elark-media-kind kind
               'elark-media-msg-id msg-id
               'keymap elark-im--media-keymap
               'mouse-face 'highlight
               'help-echo (format "%s %s — RET/click to open" kind key)))
        (pcase kind
          ('file
           (put-text-property mbeg mend 'face 'link)
           (when (and elark-im-video-previews
                      elark-im-render-images
                      (display-graphic-p)
                      (executable-find "ffmpeg"))
             (elark-im--preview-video msg-id key mbeg mend)))
          ((and 'image (guard (and elark-im-render-images
                                   (display-graphic-p))))
           (let ((cached (elark-im--media-cached key)))
             (if cached
                 (elark-im--display-image mbeg mend key cached)
               (elark-im--fetch-then-display msg-id key mbeg mend)))))))))

(defun elark-im--display-image (beg end key path &optional label)
  "Show the image at PATH over the marker text BEG..END for KEY.
LABEL names the underlying resource kind in the tooltip (default
\"image\" — video thumbnails pass \"video\")."
  (let ((inhibit-read-only t))
    (add-text-properties
     beg end
     (list 'display (create-image path nil nil
                                   :max-width elark-im-image-max-size
                                   :max-height elark-im-image-max-size)
           'help-echo (format "%s %s — RET/click to open"
                              (or label "image") key)))))

(defun elark-im--fetch-then-display (msg-id key beg end)
  "Download image KEY of MSG-ID, then display it over BEG..END.
Positions are tracked with markers so concurrent inserts (older
pages prepended) don't shift the target; a refresh that erased the
buffer is detected via the `elark-media-key' property and skipped."
  (let ((mb (copy-marker beg))
        (me (copy-marker end))
        (buf (current-buffer)))
    (elark-im--download-resource
     msg-id key "image"
     (lambda (path)
       (when (and path (buffer-live-p buf))
         (with-current-buffer buf
           (when (and (marker-position mb) (marker-position me)
                      (< mb me)
                      (equal (get-text-property mb 'elark-media-key) key))
             (elark-im--display-image mb me key path))))
       (set-marker mb nil)
       (set-marker me nil)))))

(defun elark-im--preview-video (msg-id key beg end)
  "Asynchronously show a first-frame thumbnail for video KEY over BEG..END.
MSG-ID is the message carrying the video.
Downloads the file into the cache when needed; files that turn out
not to be videos keep the plain link.  Marker/property bookkeeping
mirrors `elark-im--fetch-then-display'."
  (if-let* ((thumb (elark-im--thumbnail-cached key)))
      (elark-im--display-image beg end key thumb "video")
    (let ((mb (copy-marker beg))
          (me (copy-marker end))
          (buf (current-buffer)))
      (elark-im--ensure-video-thumb
       msg-id key
       (lambda (path)
         (when (and path (buffer-live-p buf))
           (with-current-buffer buf
             (when (and (marker-position mb) (marker-position me)
                        (< mb me)
                        (equal (get-text-property mb 'elark-media-key) key))
               (elark-im--display-image mb me key path "video"))))
         (set-marker mb nil)
         (set-marker me nil))))))

(defun elark-im--ensure-video-thumb (msg-id key callback)
  "Produce a cached first-frame thumbnail for KEY of MSG-ID.
Calls CALLBACK with the thumbnail path, or nil when the resource
is not a video (or any step fails).  Downloads the video into the
media cache first when it isn't there yet."
  (if-let* ((thumb (elark-im--thumbnail-cached key)))
      (funcall callback thumb)
    (let ((video (elark-im--media-cached key)))
      (cond
       ;; Cached and a video → just extract the frame.
       ((and video (elark-im--video-file-p video))
        (elark-im--make-video-thumb video key callback))
       ;; Cached but not a video → nothing to preview.
       (video (funcall callback nil))
       ;; Not cached → download, then decide.
       (t
        (elark-im--download-resource
         msg-id key "file"
         (lambda (path)
           (if (and path (elark-im--video-file-p path))
               (elark-im--make-video-thumb path key callback)
             (funcall callback nil)))))))))

(defun elark-im--download-resource (msg-id key type callback)
  "Download resource KEY (image/file TYPE) of MSG-ID into the cache.
Calls CALLBACK with the saved path, or nil on failure.  The CLI
accepts only relative output paths, so the process runs inside the
cache directory."
  (let ((default-directory (elark-im--media-cache-dir)))
    (elark--run-command
     (list "im" "+messages-resources-download"
           "--message-id" msg-id
           "--file-key" key
           "--type" type
           "--output" key)
     (lambda (result)
       (funcall callback (elark--get-nested result 'data 'saved_path))
       ;; Keep the persistent cache bounded; the file just written is
       ;; the newest, so LRU pruning never evicts it.
       (elark-im--media-cache-prune))
     nil
     :no-error t
     :on-error
     (lambda (_code errmsg)
       (elark--log "Media download failed (%s): %s" key errmsg)
       (message "Lark: media download failed: %s" key)
       (funcall callback nil)))))

(defun elark-im-open-media-at-point ()
  "Download (cache-first) and open the media resource at point."
  (interactive)
  (let ((key (get-text-property (point) 'elark-media-key))
        (kind (get-text-property (point) 'elark-media-kind))
        (msg-id (get-text-property (point) 'elark-media-msg-id)))
    (unless key
      (user-error "No media at point"))
    (let ((cached (elark-im--media-cached key)))
      (if cached
          (elark-im--open-media-file cached)
        (message "Lark: downloading %s…" key)
        (elark-im--download-resource
         msg-id key (if (eq kind 'image) "image" "file")
         (lambda (path)
           (when path
             (elark-im--open-media-file path))))))))

(defun elark-im--open-media-file (path)
  "Open downloaded media at PATH with the platform opener."
  (if (eq system-type 'darwin)
      (call-process "open" nil 0 nil path)
    (browse-url-of-file path)))

(defun elark-im--messages-container (data)
  "Return the alist of messages plus pagination metadata in DATA.
Falls back to nil when DATA is not a recognized response shape."
  (cond
   ((and (listp data) (or (alist-get 'items data)
                          (alist-get 'messages data)
                          (assq 'has_more data)
                          (assq 'page_token data)))
    data)
   ((and (listp data) (alist-get 'data data))
    (let ((inner (alist-get 'data data)))
      (and (listp inner) inner)))
   (t nil)))

(defun elark-im--extract-messages (data)
  "Extract message list from lark-cli response DATA.
Filters out messages marked `deleted: true' (which the new
`im +chat-messages-list' shape still includes) so they don't render
as empty/stale entries."
  (let* ((c (elark-im--messages-container data))
         (raw (cond
               ((elark--list-field c 'items))
               ((elark--list-field c 'messages))
               ((and (listp data) (listp (car-safe data))
                     (or (alist-get 'message_id (car data))
                         (alist-get 'sender_name (car data))))
                data)
               ((and c (listp c) (not (assq 'has_more c)) (not (assq 'page_token c)))
                c)
               (t nil))))
    (seq-remove #'elark-im--msg-deleted-p raw)))

(defun elark-im--truthy-p (v)
  "Return non-nil if V is a truthy JSON value (t, \"true\", non-zero)."
  (cond
   ((eq v t) t)
   ((or (null v) (eq v :false) (eq v :null)) nil)
   ((stringp v) (not (member v '("" "false" "False" "FALSE" "0"))))
   ((numberp v) (not (zerop v)))
   (t t)))

(defun elark-im--extract-has-more (data)
  "Return non-nil if DATA indicates more pages are available."
  (let ((c (elark-im--messages-container data)))
    (and c (elark-im--truthy-p (alist-get 'has_more c)))))

(defun elark-im--extract-page-token (data)
  "Return the next-page token from DATA, or nil if absent."
  (let* ((c (elark-im--messages-container data))
         (v (and c (alist-get 'page_token c))))
    (and (stringp v) (not (string-empty-p v)) v)))

;;;; Chat list mode

(defvar elark-im-chats-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'elark-im-chat-open)
    (define-key map (kbd "g")   #'elark-im-chats-refresh)
    (define-key map (kbd "s")   #'elark-im-send-to-chat)
    (define-key map (kbd "c")   #'elark-im-group-create)
    (define-key map (kbd "y")   #'elark-im-chat-copy-id)
    (define-key map (kbd "n")   #'elark-im--next-chat)
    (define-key map (kbd "p")   #'elark-im--prev-chat)
    (define-key map (kbd "?")   #'elark-im-dispatch)
    map)
  "Keymap for `elark-im-chats-mode'.")

(define-derived-mode elark-im-chats-mode special-mode
  "Lark Chats"
  "Major mode for browsing Lark chats.
Each chat is displayed as a multi-line section.")

(defun elark-im--chat-id-at-point ()
  "Return the chat ID at point, or nil."
  (get-text-property (point) 'elark-chat-id))

(defun elark-im--chat-name-at-point ()
  "Return the chat name at point, or nil."
  (get-text-property (point) 'elark-chat-name))

(defun elark-im--next-chat ()
  "Move to the next chat section."
  (interactive)
  (elark-ui-next-section 'elark-chat-id))

(defun elark-im--prev-chat ()
  "Move to the previous chat section."
  (interactive)
  (elark-ui-prev-section 'elark-chat-id))

;;;; Chat message mode

(defvar elark-im-chat-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g")   #'elark-im-chat-refresh)
    (define-key map (kbd "r")   #'elark-im-reply)
    (define-key map (kbd "R")   #'elark-ai-workflow-reply)
    (define-key map (kbd "s")   #'elark-im-send)
    (define-key map (kbd "e")   #'elark-im-react)
    (define-key map (kbd "o")   #'elark-im-load-older)
    (define-key map (kbd "q")   #'quit-window)
    (define-key map (kbd "?")   #'elark-im-dispatch)
    map)
  "Keymap for `elark-im-chat-mode'.")

(defun elark-im--maybe-autoload-older (window _start)
  "Auto-fetch older messages when WINDOW has scrolled to the top.
Hook attached to `window-scroll-functions' in chat buffers."
  (when (and elark-im--chat-id
             elark-im--has-more
             (not elark-im--loading-older)
             elark-im--page-token
             (= (window-start window) (point-min)))
    (elark-im-load-older)))

(define-derived-mode elark-im-chat-mode special-mode
  "Lark Chat"
  "Major mode for viewing a Lark chat thread."
  (add-hook 'window-scroll-functions
            #'elark-im--maybe-autoload-older nil t))

;;;; AI context providers

(defun elark-im--ai-context-chat ()
  "Return the AI context plist for a chat thread buffer.
Includes the rendered chat transcript as :content so the LLM can
work from what's already on screen instead of re-fetching via
the CLI."
  (let* ((chat-id elark-im--chat-id)
         (chat-name elark-im--chat-name)
         (msg-id (get-text-property (point) 'elark-message-id))
         (msg-count (length elark-im--messages))
         (has-more elark-im--has-more)
         (content (buffer-substring-no-properties (point-min) (point-max))))
    (list :domain "im"
          :buffer-type "chat"
          :item (list :chat-id chat-id
                      :chat-name chat-name
                      :message-id msg-id
                      :message-count msg-count
                      :has-older has-more)
          :content content
          ;; Messages render oldest→newest top→bottom, so if the content
          ;; is ever clipped keep the recent end (the bottom), not the top.
          :content-keep 'tail
          :summary (format "Chat: %s — %d message(s) loaded%s%s"
                           (or chat-name chat-id "unknown")
                           msg-count
                           (if has-more
                               " (older messages exist but are not loaded)"
                             "")
                           (if msg-id
                               (format "; cursor on message %s" msg-id)
                             "")))))

(defun elark-im--ai-context-chats ()
  "Return the AI context plist for a chat list buffer."
  (let ((chats elark-im--chats)
        (chat-id (get-text-property (point) 'elark-chat-id)))
    (list :domain "im"
          :buffer-type "chat-list"
          :item (when chat-id (list :chat-id chat-id))
          :summary (format "Chat list with %d chats" (length chats)))))

(put 'elark-im-chat-mode  'elark-ai-context-provider #'elark-im--ai-context-chat)
(put 'elark-im-chats-mode 'elark-ai-context-provider #'elark-im--ai-context-chats)

;;;; Chat listing
;; CLI: im +chat-search [--query X]


;;;###autoload
(defun elark-im-chats (&optional query)
  "List Lark chats.  With QUERY, search by keyword.
When called interactively, prompt for a search query."
  (interactive
   (list (read-string "Search chats (keyword): ")))
  (when (or (null query) (string-empty-p query))
    (user-error "A search query is required (e.g. a chat name or keyword)"))
  (message "Lark: fetching chats...")
  (let ((q query))
    (elark--run-command
     (list "im" "+chat-search" "--query" query)
     (lambda (data) (elark-im--display-chats data q)))))

(defun elark-im-chats-refresh ()
  "Refresh the chat list buffer with the previous query."
  (interactive)
  (elark-im-chats (or elark-im--chat-query
                      (read-string "Search chats (keyword): "))))

(defun elark-im--display-chats (data &optional query)
  "Display chat list DATA.  QUERY is stored for refresh."
  (let* ((chats (elark-im--extract-chats data))
         (buf (get-buffer-create "*Lark Chats*")))
    (with-current-buffer buf
      (elark-im-chats-mode)
      (setq elark-im--chats chats
            elark-im--chat-query query)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (if (null chats)
            (insert "(no chats)\n")
          (dolist (chat chats)
            (elark-im--insert-chat chat))))
      (goto-char (point-min))
      (setq header-line-format
            (format " Lark Chats — %d chat(s)" (length chats))))
    (pop-to-buffer buf)
    ;; Owner names resolve in the background; raw ids are patched in
    ;; place as lookups return (rendering never blocks on the network).
    (elark-contact-resolve-buffer-async buf)))

;;;; Chat messages
;; CLI: im +chat-messages-list --chat-id X [--page-size N] [--sort asc|desc]


(defun elark-im-chat-open ()
  "Open the chat at point and show message history."
  (interactive)
  (let ((id (elark-im--chat-id-at-point)))
    (unless id (user-error "No chat at point"))
    (let ((name (or (elark-im--chat-name-at-point) "")))
      (elark-im-messages id name))))

;;;###autoload
(defun elark-im-messages (chat-id &optional chat-name)
  "Show the latest message history for CHAT-ID (optionally with CHAT-NAME).
Fetches the most recent `elark-im-message-limit' messages and displays
them in chronological order (oldest at top, newest at bottom).  Older
messages can be loaded one page at a time with `elark-im-load-older'."
  (interactive "sChat ID: ")
  (message "Lark: fetching messages...")
  (elark--run-command
   (list "im" "+chat-messages-list"
         "--chat-id" chat-id
         "--page-size" elark-im-message-limit
         "--sort" "desc")
   (lambda (data)
     (elark-im--display-messages data chat-id (or chat-name chat-id)))))

(defun elark-im-chat-refresh ()
  "Refresh the current chat message buffer."
  (interactive)
  (when elark-im--chat-id
    (elark-im-messages elark-im--chat-id elark-im--chat-name)))

(defun elark-im--header-line ()
  "Build the header-line string for a chat buffer from buffer-local state.
While older messages load, the spinner animates here — the header line
is its own display region, so the glyph's `elark-spinner' face is never
clobbered by buffer text faces.  Otherwise it shows the \"press o\"
affordance when more history is available."
  (concat
   (format " Lark Chat: %s — %d message(s)"
           (or elark-im--chat-name "")
           (length elark-im--messages))
   (cond
    (elark-im--loading-older
     (concat "  "
             (or elark-im--spinner-frame (aref elark-spinner-frames 0))
             " loading older…"))
    (elark-im--has-more " (older available, press o)")
    (t ""))))

(defun elark-im--render-header (chat-name)
  "Insert the chat title block for CHAT-NAME at point.
The older-messages affordance and the loading spinner live on the
header line (see `elark-im--header-line'), not in the buffer body."
  (elark-ui-insert-title chat-name 1)
  (insert "\n"))

(defun elark-im--spinner-stop ()
  "Stop the loading spinner in the current chat buffer."
  (elark-spinner-stop elark-im--spinner-timer)
  (setq elark-im--spinner-timer nil))

(defun elark-im--spinner-start ()
  "Start the older-messages loading spinner on the chat buffer's header line.
Built on the shared `elark-spinner-start' engine: it animates the header
line while loading and self-cleans if loading finishes, the buffer
dies, or the safety timeout elapses (e.g. when the fetch errors without
ever invoking its callback).  Each tick stores the frame and forces a
header-line redisplay; `elark-im--header-line' renders it."
  (elark-im--spinner-stop)
  (let ((buf (current-buffer)))
    (setq elark-im--spinner-timer
          (elark-spinner-start
           (lambda (frame)
             (when (and (buffer-live-p buf)
                        (buffer-local-value 'elark-im--loading-older buf))
               (with-current-buffer buf
                 (setq elark-im--spinner-frame frame)
                 (force-mode-line-update))
               t))
           nil nil nil
           ;; On auto-stop (timeout / buffer death): clear stuck state.
           (lambda ()
             (when (buffer-live-p buf)
               (with-current-buffer buf
                 (setq elark-im--loading-older nil
                       elark-im--spinner-timer nil)
                 (force-mode-line-update))))))))

(defun elark-im--display-messages (data chat-id chat-name)
  "Display the latest message history DATA for CHAT-ID with CHAT-NAME.
DATA is expected to be desc-sorted (newest first); messages are reversed
so they render chronologically with the newest at the bottom."
  (let* ((raw (elark-im--extract-messages data))
         (messages (reverse raw))
         (has-more (elark-im--extract-has-more data))
         (page-token (elark-im--extract-page-token data))
         (buf (get-buffer-create (format "*Lark Chat: %s*" chat-name))))
    (with-current-buffer buf
      (elark-im-chat-mode)
      (setq elark-im--chat-id chat-id
            elark-im--chat-name chat-name
            elark-im--messages messages
            elark-im--has-more has-more
            elark-im--page-token page-token
            elark-im--loading-older nil)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (elark-im--render-header chat-name)
        (if (null messages)
            (insert "(no messages)\n")
          (dolist (msg messages)
            (elark-im--insert-message msg)))
        (goto-char (point-max)))
      (setq header-line-format '(:eval (elark-im--header-line))))
    (pop-to-buffer buf)))

(defun elark-im--header-end ()
  "Return buffer position just after the chat header block."
  (save-excursion
    (goto-char (point-min))
    (let ((p (point-min)))
      (while (and (not (eobp))
                  (not (get-text-property (point) 'elark-message-id)))
        (forward-line 1)
        (setq p (point)))
      p)))

(defun elark-im--prepend-older (data)
  "Insert older messages from DATA at the top of the current chat buffer.
Runs from an async callback: the chat buffer may no longer be in
the selected window, so all window-relative work (anchor point,
`window-start', the final `recenter') goes through the window
actually displaying the buffer — and is skipped entirely when the
buffer isn't displayed."
  (elark-im--spinner-stop)
  (let* ((win (get-buffer-window (current-buffer) t))
         (raw (elark-im--extract-messages data))
         (older (reverse raw))
         (has-more (elark-im--extract-has-more data))
         (page-token (elark-im--extract-page-token data))
         (anchor (or (and win
                          (with-selected-window win
                            (get-text-property (point) 'elark-message-id)))
                     (save-excursion
                       (goto-char (elark-im--header-end))
                       (get-text-property (point) 'elark-message-id))))
         (window-line (and anchor win
                           (with-selected-window win
                             (count-screen-lines (window-start) (point))))))
    (setq elark-im--has-more has-more
          elark-im--page-token page-token
          elark-im--loading-older nil
          elark-im--messages (append older elark-im--messages))
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char (point-min))
        (let ((hend (elark-im--header-end)))
          (delete-region (point-min) hend))
        (goto-char (point-min))
        (elark-im--render-header elark-im--chat-name)
        (dolist (msg older)
          (elark-im--insert-message msg))))
    (setq header-line-format '(:eval (elark-im--header-line)))
    (when anchor
      (let (found)
        (save-excursion
          (goto-char (point-min))
          (while (and (not found) (not (eobp)))
            (if (equal (get-text-property (point) 'elark-message-id) anchor)
                (setq found (point))
              (goto-char (or (next-single-property-change
                              (point) 'elark-message-id)
                             (point-max))))))
        (when found
          (goto-char found)
          (when (and window-line (window-live-p win))
            (with-selected-window win
              (goto-char found)
              (recenter window-line))))))
    (message "Lark: loaded %d older message(s)%s"
             (length older)
             (if has-more "" "; no more older messages"))))

(defun elark-im-load-older ()
  "Fetch one more page of older messages and prepend them to the buffer."
  (interactive)
  (unless elark-im--chat-id
    (user-error "Not in a chat buffer"))
  (cond
   (elark-im--loading-older
    (message "Lark: already loading older messages…"))
   ((not elark-im--has-more)
    (user-error "No more older messages"))
   ((not elark-im--page-token)
    (user-error "No pagination token available; try refreshing with g"))
   (t
    (setq elark-im--loading-older t)
    (elark-im--spinner-start)
    (message "Lark: loading older messages…")
    (let ((buf (current-buffer)))
      (elark--run-command
       (list "im" "+chat-messages-list"
             "--chat-id" elark-im--chat-id
             "--page-size" elark-im-message-limit
             "--sort" "desc"
             "--page-token" elark-im--page-token)
       (lambda (data)
         (if (buffer-live-p buf)
             (with-current-buffer buf
               (elark-im--prepend-older data))
           (message "Lark: chat buffer was closed"))))))))

(defun elark-im--find-message (id)
  "Return the loaded message alist with message id ID, or nil.
Searches the buffer's root messages and their thread replies."
  (when (and (stringp id) (not (string-empty-p id)))
    (catch 'hit
      (dolist (m elark-im--messages)
        (when (equal (elark-im--msg-id m) id) (throw 'hit m))
        (dolist (r (elark--list-field m 'thread_replies))
          (when (equal (elark-im--msg-id r) id) (throw 'hit r))))
      nil)))

(defun elark-im--quote-line (parent-id)
  "Build the dim quote line for a reply to PARENT-ID.
Shows the parent's sender and a one-line snippet when the parent
is among the loaded messages; a generic marker otherwise."
  (if-let* ((parent (elark-im--find-message parent-id)))
      (let* ((sender (elark-im--msg-sender parent))
             (content (replace-regexp-in-string
                       "[ \t\n]+" " "
                       (string-trim (or (elark-im--msg-content parent) ""))))
             (snippet (truncate-string-to-width content 60 nil nil t)))
        (propertize (format "↳ %s: %s\n" sender snippet) 'face 'shadow))
    (propertize "↳ (reply to an earlier message)\n" 'face 'shadow)))

(defun elark-im--prefix-region (beg end depth)
  "Prefix every line in BEG..END with DEPTH dim thread gutters.
Leaves point after the (now longer) region."
  (let ((gutter (propertize (apply #'concat (make-list depth "  │ "))
                            'face 'shadow))
        (end-marker (copy-marker end)))
    (goto-char beg)
    (while (< (point) end-marker)
      (insert gutter)
      (forward-line 1))
    (goto-char end-marker)
    (set-marker end-marker nil)))

(defun elark-im--insert-message (msg &optional depth)
  "Insert a formatted MSG into the current buffer.
DEPTH > 0 renders it as a thread reply, indented under its root
behind a dim gutter.  Thread replies embedded in MSG (the
`thread_replies' field of THREAD-mode chats) are rendered
recursively beneath it, oldest first."
  (let ((sender (elark-im--msg-sender msg))
        (time (elark-im--msg-time msg))
        (content (or (elark-im--msg-content msg) ""))
        (type (elark-im--msg-type msg))
        (id (elark-im--msg-id msg))
        (depth (or depth 0))
        (beg (point)))
    (insert (propertize sender 'face 'bold)
            "  "
            (propertize time 'face 'font-lock-comment-face)
            "\n")
    ;; DEFAULT-mode chats mark replies with `reply_to' — show a quote
    ;; of the parent.  Thread replies (depth > 0) already show their
    ;; structure, so no quote there.
    (let ((parent-id (alist-get 'reply_to msg)))
      (when (and (zerop depth)
                 (stringp parent-id)
                 (not (string-empty-p parent-id)))
        (insert (elark-im--quote-line parent-id))))
    (cond
     ;; Textual types (text / post / markdown) come back from lark-cli as
     ;; readable text — render the content directly so the buffer doesn't
     ;; look wrapped in "[post message]: …" labels.
     ((and (elark-im--msg-textual-p msg) (not (string-empty-p content)))
      (insert content "\n"))
     ;; Non-text affordances (image, file, sticker, share_chat, …) — label
     ;; them, and include any payload string the CLI returned.
     ((not (string-empty-p content))
      (insert (propertize (format "[%s] " type) 'face 'font-lock-type-face)
              content "\n"))
     (t
      (insert (propertize (format "[%s message]" type)
                          'face 'font-lock-type-face)
              "\n")))
    (insert "\n")
    (when (> depth 0)
      (elark-im--prefix-region beg (point) depth))
    (put-text-property beg (point) 'elark-message-id id)
    ;; Render image/file markers in the inserted content (inline
    ;; images on graphical displays, openable links otherwise).
    (elark-im--render-media beg (point) id)
    ;; THREAD-mode chats nest replies inside the root message —
    ;; render them as a tree instead of dropping them.
    (dolist (reply (sort (seq-remove #'elark-im--msg-deleted-p
                                     (elark--list-field msg 'thread_replies))
                         (lambda (a b)
                           (string< (format "%s" (alist-get 'create_time a))
                                    (format "%s" (alist-get 'create_time b))))))
      (elark-im--insert-message reply (1+ depth)))))

;;;; Send message
;; CLI: im +messages-send --chat-id X --text X

(defun elark-im--sent-message (text data &optional reply-to)
  "Build a message alist locally echoing TEXT that was just sent.
DATA is the lark-cli response; its message id is carried over when
present so reply-at-point works on the echoed entry without a
refresh.  REPLY-TO, when non-nil, marks the echo as a reply so the
quote line renders."
  `((message_id . ,(or (elark--get-nested data 'data 'message_id)
                       (alist-get 'message_id data)
                       ""))
    (msg_type . "text")
    (sender_name . "Me")
    (create_time . ,(format-time-string "%Y-%m-%d %H:%M"))
    ,@(when reply-to `((reply_to . ,reply-to)))
    (content . ,text)))

(defun elark-im--append-sent-message (buf text data &optional reply-to)
  "Append locally-echoed TEXT to chat buffer BUF without reloading.
Replaces the old post-send `elark-im-chat-refresh': re-fetching and
erasing the whole buffer flashed visibly and reset the user's
place.  The window showing BUF, if any, is moved to the new
message; `g' still does a full sync when wanted.  REPLY-TO marks
the echo as a reply to that message id."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (let ((msg (elark-im--sent-message text data reply-to))
            (inhibit-read-only t))
        (setq elark-im--messages (append elark-im--messages (list msg)))
        (save-excursion
          (goto-char (point-max))
          (elark-im--insert-message msg))
        (when-let* ((win (get-buffer-window buf t)))
          (set-window-point win (point-max)))))))


;;;###autoload
(defun elark-im-send (&optional chat-id)
  "Send a message to CHAT-ID (or the current chat buffer)."
  (interactive)
  (let ((id (or chat-id elark-im--chat-id
                (read-string "Chat ID: "))))
    (when (string-empty-p id)
      (user-error "No chat ID specified"))
    (let ((text (read-string "Message: "))
          ;; Echo locally only when sending into the chat buffer we
          ;; are looking at (not from the chat list).
          (chat-buf (and elark-im--chat-id
                         (equal elark-im--chat-id id)
                         (current-buffer))))
      (when (string-empty-p text)
        (user-error "Empty message"))
      (message "Lark: sending message...")
      (elark--run-command
       (list "im" "+messages-send" "--chat-id" id "--text" text)
       (lambda (data)
         (message "Lark: message sent")
         (elark-im--append-sent-message chat-buf text data))))))

(defun elark-im-send-to-chat ()
  "Send a message to the chat at point."
  (interactive)
  (let ((id (elark-im--chat-id-at-point)))
    (unless id (user-error "No chat at point"))
    (elark-im-send id)))

;;;; Reply
;; CLI: im +messages-reply --message-id X --text X

(defun elark-im-reply ()
  "Reply to a message in the current chat.
Prompts for message ID if not determinable from point."
  (interactive)
  (unless elark-im--chat-id
    (user-error "Not in a chat buffer"))
  (let* ((msg-id (or (get-text-property (point) 'elark-message-id)
                     (read-string "Reply to message ID: ")))
         (text (read-string "Reply: "))
         (chat-buf (current-buffer)))
    (when (string-empty-p text)
      (user-error "Empty reply"))
    (message "Lark: sending reply...")
    (elark--run-command
     (list "im" "+messages-reply" "--message-id" msg-id "--text" text)
     (lambda (data)
       (message "Lark: reply sent")
       (elark-im--append-sent-message chat-buf text data msg-id)))))

;;;; Reactions
;; CLI: im reactions create --params '{"message_id":"X"}' --data '{"reaction_type":{"emoji_type":"X"}}'


(defun elark-im-react ()
  "Add a reaction to a message in the current chat."
  (interactive)
  (unless elark-im--chat-id
    (user-error "Not in a chat buffer"))
  (let* ((msg-id (or (get-text-property (point) 'elark-message-id)
                     (read-string "Message ID: ")))
         (emoji (read-string "Emoji type (e.g., THUMBSUP): ")))
    (when (string-empty-p emoji)
      (user-error "No emoji specified"))
    (let ((params (json-encode `((message_id . ,msg-id))))
          (body (json-encode `((reaction_type . ((emoji_type . ,emoji)))))))
      (elark--run-command
       (list "im" "reactions" "create" "--params" params "--data" body)
       (lambda (_data)
         (message "Lark: reaction added"))))))

;;;; Chat copy ID

(defun elark-im-chat-copy-id ()
  "Copy the chat ID at point to the kill ring."
  (interactive)
  (let ((id (or (elark-im--chat-id-at-point) elark-im--chat-id)))
    (unless id (user-error "No chat ID"))
    (kill-new id)
    (message "Copied: %s" id)))

;;;; Group chat management
;; CLI: im +chat-create --name X [--description X] [--users X] [--type private|public]


;;;###autoload (autoload 'elark-im-group-create "elark-im" nil t)
(transient-define-prefix elark-im-group-create ()
  "Create a new Lark group chat."
  ["Group Details"
   ("n" "Name"        "--name=" :prompt "Group name: ")
   ("d" "Description" "--description=" :prompt "Description: ")]
  ["Members"
   ("m" "Users"       "--users=" :prompt "User open_ids (comma-separated ou_): ")]
  ["Actions"
   ("RET" "Create"    elark-im--do-group-create)
   ("q"   "Cancel"    transient-quit-all)])

(defun elark-im--do-group-create (&rest _args)
  "Execute group chat creation with transient arguments."
  (interactive)
  (let ((args (transient-args 'elark-im-group-create)))
    (unless args (user-error "No group details provided"))
    (message "Lark: creating group chat...")
    (elark--run-command
     (append '("im" "+chat-create") args)
     (lambda (data)
       (let ((id (or (elark--get-nested data 'data 'chat_id)
                     (alist-get 'chat_id data))))
         (message "Lark: group created%s"
                  (if id (format " (ID: %s)" id) "")))))))

;;;; Search messages
;; CLI: im +messages-search --query X


;;;###autoload
(defun elark-im-search (query)
  "Search Lark messages for QUERY."
  (interactive "sSearch messages: ")
  (message "Lark: searching messages...")
  (elark--run-command
   (list "im" "+messages-search" "--query" query)
   (lambda (data)
     (let* ((messages (elark-im--extract-messages data))
            (buf (get-buffer-create (format "*Lark Search: %s*" query))))
       (with-current-buffer buf
         (elark-im-chat-mode)
         (setq elark-im--messages messages)
         (let ((inhibit-read-only t))
           (erase-buffer)
           (insert (propertize (format "Search: %s\n" query) 'face 'bold)
                   (elark-ui-separator 60) "\n\n")
           (if (null messages)
               (insert "(no results)\n")
             (dolist (msg messages)
               (elark-im--insert-message msg))))
         (goto-char (point-min))
         (setq header-line-format
               (format " Lark Search: \"%s\" — %d result(s)"
                       query (length messages))))
       (pop-to-buffer buf)))))

;;;; Real-time event subscription

;;;###autoload
(defun elark-im-subscribe ()
  "Subscribe to real-time Lark messenger events.
Starts a long-running process that receives NDJSON events."
  (interactive)
  (when (and elark-im--event-process
             (process-live-p elark-im--event-process))
    (if (yes-or-no-p "Event subscription already active.  Restart? ")
        (elark-im-unsubscribe)
      (user-error "Subscription already active")))
  (message "Lark: subscribing to events...")
  (let ((exe (elark--executable)))
    (setq elark-im--event-process
          (make-process
           :name "elark-events"
           :command (list exe "event" "subscribe"
                         "--format" "ndjson" "--compact")
           :connection-type 'pipe
           :noquery t
           :filter #'elark-im--event-filter
           :sentinel (lambda (_proc event)
                       (message "Lark events: %s" (string-trim event))))))
  (message "Lark: event subscription started"))

(defun elark-im--event-filter (_proc output)
  "Process filter for real-time events: parse NDJSON OUTPUT lines."
  (dolist (event (elark--parse-ndjson output))
    (elark-im--handle-event event)))

(defun elark-im--handle-event (event)
  "Handle a single real-time EVENT.
Dispatches to appropriate handler based on event type."
  (let ((type (or (alist-get 'type event)
                  (alist-get 'event_type event))))
    (elark--log "Event received: %s" type)
    (cond
     ((and type (string-match-p "message" type))
      (elark-im--handle-new-message event))
     (t nil))))

(defun elark-im--handle-new-message (event)
  "Handle a new message EVENT — refresh chat buffer if open."
  (let* ((msg (or (alist-get 'message event)
                  (alist-get 'data event)
                  event))
         (chat-id (or (alist-get 'chat_id msg)
                      (elark--get-nested msg 'message 'chat_id))))
    (when chat-id
      (dolist (buf (buffer-list))
        (with-current-buffer buf
          (when (and (derived-mode-p 'elark-im-chat-mode)
                     (equal elark-im--chat-id chat-id))
            (elark-im-chat-refresh)))))))

;;;###autoload
(defun elark-im-unsubscribe ()
  "Stop the real-time event subscription."
  (interactive)
  (when (and elark-im--event-process
             (process-live-p elark-im--event-process))
    (delete-process elark-im--event-process))
  (setq elark-im--event-process nil)
  (message "Lark: event subscription stopped"))

;;;; AI smart reply


(defvar elark-ai-reply-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map text-mode-map)
    (define-key map (kbd "C-c C-c") #'elark-ai-reply-send)
    (define-key map (kbd "C-c C-k") #'elark-ai-reply-cancel)
    map)
  "Keymap for `elark-ai-reply-mode'.")

(define-derived-mode elark-ai-reply-mode text-mode "Lark Reply"
  "Major mode for editing an AI-drafted reply before sending.
\\[elark-ai-reply-send] to send, \\[elark-ai-reply-cancel] to cancel."
  (setq header-line-format
        " Lark AI Reply — C-c C-c send, C-c C-k cancel"))

(defvar-local elark-ai-reply--message-id nil
  "Message ID to reply to.")

(defvar-local elark-ai-reply--chat-id nil
  "Chat ID the reply belongs to.")

(defvar-local elark-ai-reply--chat-name nil
  "Chat name for refreshing after send.")

(defun elark-ai-reply-send ()
  "Send the drafted reply in the current compose buffer."
  (interactive)
  (let ((text (string-trim (buffer-string)))
        (msg-id elark-ai-reply--message-id)
        (chat-id elark-ai-reply--chat-id)
        (chat-name elark-ai-reply--chat-name))
    (when (string-empty-p text)
      (user-error "Empty reply"))
    (unless msg-id
      (user-error "No message ID — cannot send reply"))
    (quit-window t)
    (message "Lark AI: sending reply...")
    (elark--run-command
     (list "im" "+messages-reply" "--message-id" msg-id "--text" text)
     (lambda (_data)
       (message "Lark AI: reply sent")
       ;; Refresh the chat buffer if it exists
       (when chat-id
         (dolist (buf (buffer-list))
           (with-current-buffer buf
             (when (and (derived-mode-p 'elark-im-chat-mode)
                        (equal elark-im--chat-id chat-id))
               (elark-im-messages chat-id chat-name)))))))))

(defun elark-ai-reply-cancel ()
  "Cancel the reply draft."
  (interactive)
  (quit-window t)
  (message "Lark AI: reply cancelled."))

(defun elark-ai--open-compose-buffer (draft message-id chat-id chat-name)
  "Open a compose buffer pre-filled with DRAFT.
MESSAGE-ID is the message being replied to.
CHAT-ID and CHAT-NAME identify the chat for refresh after send."
  (let ((buf (get-buffer-create "*Lark AI Reply*")))
    (with-current-buffer buf
      (elark-ai-reply-mode)
      (erase-buffer)
      (insert draft)
      (setq elark-ai-reply--message-id message-id
            elark-ai-reply--chat-id chat-id
            elark-ai-reply--chat-name chat-name)
      (goto-char (point-min)))
    (pop-to-buffer buf)))

;;; Thread context extraction

(defun elark-ai--collect-thread-context ()
  "Collect thread context from the current chat buffer.
Returns a plist (:chat-id ID :chat-name NAME :message-id ID
:thread-text TEXT) or nil if not in a chat buffer."
  (unless (derived-mode-p 'elark-im-chat-mode)
    (user-error "Not in a Lark chat buffer"))
  (let ((chat-id elark-im--chat-id)
        (chat-name elark-im--chat-name)
        (messages elark-im--messages)
        (msg-id (get-text-property (point) 'elark-message-id)))
    (unless chat-id
      (user-error "No chat ID in current buffer"))
    (unless msg-id
      (user-error "No message at point — place cursor on a message"))
    ;; Build thread text from recent messages for LLM context
    (let ((thread-text
           (mapconcat
            (lambda (msg)
              (let ((sender (or (alist-get 'sender_name msg)
                                (elark--get-nested msg 'sender 'name)
                                "unknown"))
                    (content (or (alist-get 'text msg)
                                 (alist-get 'content msg)
                                 (let ((body (alist-get 'body msg)))
                                   (when body
                                     (if (stringp body) body
                                       (or (alist-get 'text body)
                                           (alist-get 'content body)))))
                                 ""))
                    (id (or (alist-get 'message_id msg)
                            (alist-get 'id msg))))
                (format "%s%s: %s"
                        (if (equal id msg-id) ">>> " "")
                        sender content)))
            (seq-take (reverse (reverse messages)) 20)
            "\n")))
      (list :chat-id chat-id
            :chat-name chat-name
            :message-id msg-id
            :thread-text thread-text))))

;;;###autoload
(defun elark-ai-workflow-reply ()
  "Draft an AI-powered reply to the message at point.
Reads thread context from the current chat buffer, asks the LLM
to draft a reply, and opens a compose buffer for editing before send."
  (interactive)
  (let* ((ctx (elark-ai--collect-thread-context))
         (chat-id (plist-get ctx :chat-id))
         (chat-name (plist-get ctx :chat-name))
         (msg-id (plist-get ctx :message-id))
         (thread-text (plist-get ctx :thread-text))
         (skill-names (elark-ai-skills-select "reply message chat"))
         ;; Prose draft, not a plan — use the synthesis prompt so the
         ;; system message doesn't fight the user message's "no JSON".
         (system-prompt (elark-ai-skills-build-synthesis-prompt skill-names))
         (user-msg (format "Draft a concise, natural reply to the message marked with >>> in this chat thread. Output ONLY the reply text, nothing else — no JSON, no plan, no explanation.\n\nChat: %s\n\n%s"
                           (or chat-name chat-id)
                           thread-text)))
    (message "Lark AI: drafting reply...")
    (elark-ai--call-llm
     system-prompt user-msg
     (lambda (draft)
       (elark-ai--open-compose-buffer
        (string-trim draft) msg-id chat-id chat-name)))))

;;;; Transient dispatch

;;;###autoload (autoload 'elark-im-dispatch "elark-im" nil t)
(transient-define-prefix elark-im-dispatch ()
  "Lark Messenger commands."
  ["Quick Actions"
   ("l" "Chat list"     elark-im-chats)
   ("s" "Send message"  elark-im-send)
   ("/" "Search"        elark-im-search)]
  ["Group"
   ("c" "Create group"  elark-im-group-create)]
  ["Real-time"
   ("S" "Subscribe"     elark-im-subscribe)
   ("U" "Unsubscribe"   elark-im-unsubscribe)]
  ["In Chat"
   ("r" "Reply"         elark-im-reply)
   ("R" "AI smart reply" elark-ai-workflow-reply)
   ("e" "React"         elark-im-react)
   ("o" "Load older"    elark-im-load-older)
   ("g" "Refresh"       elark-im-chat-refresh)])

(provide 'elark-im)
;;; elark-im.el ends here
