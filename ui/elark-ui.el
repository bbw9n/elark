;;; elark-ui.el --- Shared UI helpers for elark -*- lexical-binding: t; -*-

;; Copyright (C) 2026 bbw9n

;; Author: bbw9n <bbw9nio@gmail.com>
;; Assisted-by: Claude:claude-opus-5
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Presentation-layer helpers shared across elark domains, kept
;; separate from the process/IO engine in `elark-core'.
;;
;; It provides a small, render-target-agnostic loading spinner plus a
;; set of section-buffer rendering primitives shared by the domain
;; modules:
;;
;; - `elark-spinner-start' cycles through frames on a timer and calls a
;;   render function with each frame string; the render function decides
;;   what to do with it — update an in-buffer line, a header-line, the
;;   echo area, etc.  Domains build their own spinners on top of this;
;;   `elark-spinner-message' is a ready-made echo-area spinner for the
;;   common "fetching…" case.
;;
;; - `elark-ui-bar-frames' is an indeterminate progress-bar frame set for
;;   the same `elark-spinner-start' engine (a fill that loops).
;; - `elark-ui-insert-field' renders an aligned "Label: value" line.
;; - `elark-ui-separator' / `elark-ui-insert-title' render the horizontal
;;   rule and bold title-underline used at the top of detail buffers.
;; - `elark-ui-next-section' / `elark-ui-prev-section' move point between
;;   text-property-delimited sections in a list buffer.

;;; Code:

;;;; Faces

(defface elark-ui-info
  '((t :inherit font-lock-function-name-face))
  "Base face for elark informational / in-progress indicators."
  :group 'elark)

;;;; Loading spinner

(defface elark-spinner
  '((t :inherit elark-ui-info))
  "Face for elark loading-spinner glyphs.
Inherits `elark-ui-info' (an info blue) so the animated glyph stands out
from surrounding text."
  :group 'elark)

(defconst elark-spinner-frames
  (apply #'vector
         (mapcar (lambda (glyph) (propertize glyph 'face 'elark-spinner))
                 '("⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏")))
  "Default braille frames cycled by elark loading spinners.
Each glyph carries the `elark-spinner' face so the spinner renders in a
salient colour wherever it is displayed.")

(defun elark-spinner-start (render-fn &optional interval frames max-ticks on-stop)
  "Animate a loading spinner by repeatedly calling RENDER-FN.

RENDER-FN is called with one argument, the current frame string (an
element of FRAMES).  It is invoked immediately with the first frame and
then once per tick.  If RENDER-FN returns nil (or signals, which is
reported as a message), the spinner stops — callers use this to
signal completion (loading finished, the target buffer died, etc.).

INTERVAL   seconds between frames (default 0.1).
FRAMES     vector of frame strings (default `elark-spinner-frames').
MAX-TICKS  safety cap on total frames before auto-stopping (default 300,
           ~30s at the default interval) — guards against runaway timers
           when an async operation errors without ever signalling done.
ON-STOP    optional 0-arg function run when the spinner stops on its own
           (RENDER-FN returned nil or MAX-TICKS was hit).  It is NOT run
           when the spinner is stopped explicitly via `elark-spinner-stop'.

Returns a spinner handle; pass it to `elark-spinner-stop'."
  (let* ((frames (or frames elark-spinner-frames))
         (count (length frames))
         (interval (or interval 0.1))
         (max-ticks (or max-ticks 300))
         (index 0)
         timer)
    (if (not (with-demoted-errors "Lark spinner: %S" (funcall render-fn (aref frames 0))))
        (when on-stop (with-demoted-errors "Lark spinner: %S" (funcall on-stop)))
      (setq timer
            (run-with-timer
             interval interval
             (lambda ()
               (setq index (1+ index))
               (if (and (< index max-ticks)
                        (with-demoted-errors "Lark spinner: %S"
                          (funcall render-fn (aref frames (mod index count)))))
                   nil
                 (when (timerp timer) (cancel-timer timer))
                 (when on-stop (with-demoted-errors "Lark spinner: %S" (funcall on-stop))))))))
    timer))

(defun elark-spinner-stop (spinner)
  "Stop SPINNER, a handle returned by `elark-spinner-start'.
Does not invoke the spinner's ON-STOP callback (an explicit stop is the
caller's own action).  Safe to call with nil or an already-stopped
spinner."
  (when (timerp spinner)
    (cancel-timer spinner)))

(defun elark-spinner-message (format-string &rest args)
  "Show an echo-area loading spinner prefixed to a message.
FORMAT-STRING and ARGS are formatted like `message'; the current
spinner frame is prepended, e.g. \"⠋ Fetching messages…\".

The spinner refreshes the echo area every tick but logs nothing to
*Messages*, and stops itself if the echo area is taken over by an
unrelated message or while the minibuffer is active, so it never
clobbers other output.  Returns a spinner handle; call
`elark-spinner-stop' on it when the operation finishes."
  (let ((text (apply #'format format-string args))
        (last nil))
    (elark-spinner-start
     (lambda (frame)
       (cond
        ((active-minibuffer-window) nil)
        ((and last (current-message) (not (equal (current-message) last)))
         nil)
        (t
         (setq last (format "%s %s" frame text))
         (let ((message-log-max nil))
           (message "%s" last))
         t))))))

;;;; Progress bar

(defface elark-ui-bar
  '((t :inherit elark-ui-info))
  "Face for the filled cells of a `elark-ui' progress bar.
Inherits `elark-ui-info' (an info blue), shared with `elark-spinner', so a
running bar reads as active."
  :group 'elark)

(defconst elark-ui-bar-frames
  (apply #'vector
         (mapcar (lambda (n)
                   (concat
                    (propertize (make-string n ?█) 'face 'elark-ui-bar)
                    (propertize (make-string (- 10 n) ?░) 'face 'shadow)))
                 (number-sequence 1 10)))
  "Indeterminate progress-bar frames: a 10-cell left-to-right fill that loops.
Feed to `elark-spinner-start' as its FRAMES argument — the animation
engine is shared with the loading spinner.  Filled cells use the
`elark-ui-bar' face; empty cells use `shadow'.")

;;;; Field rendering

(defun elark-ui-insert-field (label value &optional width indent)
  "Insert a \"LABEL: VALUE\" line at point when VALUE is a non-empty string.
The label, with a trailing colon, is left-padded to WIDTH columns
\(default 14) and shown in `font-lock-keyword-face'.  INDENT is leading
whitespace before the label (default two spaces).  VALUE may span
multiple lines; continuation lines are aligned under the first line's
value column.  Does nothing when VALUE is nil, not a string, or empty."
  (when (and value (stringp value) (not (string-empty-p value)))
    (let* ((indent (or indent "  "))
           (width (or width 14))
           (prefix (concat indent
                           (format (format "%%-%ds" width) (concat label ":"))))
           (cont (make-string (length prefix) ?\s))
           (lines (split-string value "\n")))
      (insert (propertize prefix 'face 'font-lock-keyword-face)
              (car lines) "\n")
      (dolist (line (cdr lines))
        (insert cont line "\n")))))

;;;; Separators and titles

(defconst elark-ui-rule-char ?─
  "Character used to draw horizontal rules in elark buffers.")

(defun elark-ui-separator (&optional width)
  "Return a horizontal rule string of WIDTH `elark-ui-rule-char's.
WIDTH defaults to 60."
  (make-string (or width 60) elark-ui-rule-char))

(defun elark-ui-insert-title (title &optional trailing-newlines max-width)
  "Insert TITLE in bold at point, underlined by a horizontal rule.
The rule's width scales to TITLE's length, clamped to the range
\[20, MAX-WIDTH] (MAX-WIDTH defaults to 60).  TRAILING-NEWLINES blank
lines follow the rule (default 2)."
  (insert (propertize title 'face 'bold) "\n"
          (elark-ui-separator (min (or max-width 60) (max 20 (length title))))
          (make-string (or trailing-newlines 2) ?\n)))

;;;; Code-block panes
;;
;; A small toolkit for rendering "inline code-ish" sections (a CLI
;; command, an elisp expression, a JSON blob) the way the user's theme
;; would render an Org/Markdown code block — so the chat surface inherits
;; the same visual cues as the rest of the user's Emacs.

(defface elark-ui-block-bg
  '((t :inherit mode-line-inactive :extend t))
  "Background face for code-block-like content panes.
Inherits `mode-line-inactive' so the chosen theme drives the shade;
`:extend t' fills the colour across the whole line, not just the glyphs."
  :group 'elark)

(defun elark-ui-block-bg-face ()
  "Return a face suitable for a code-block content background.
Picks the buffer's mode-appropriate block face when one exists
\(`org-block' in Org buffers, `markdown-code-face' in Markdown) and
falls back to `elark-ui-block-bg' otherwise — so a rendered pane reads
as a code block under whatever theme the user has chosen."
  (cond
   ((derived-mode-p 'org-mode) 'org-block)
   ((derived-mode-p 'markdown-mode) 'markdown-code-face)
   (t 'elark-ui-block-bg)))

(defun elark-ui-fontify-block (mode start end)
  "Apply MODE's syntax highlighting to the buffer region START..END.

Copies the region into a temp buffer, activates MODE there, runs
`font-lock-ensure', and writes the resulting `face' values back onto
the original region as `font-lock-face' so they survive any later
refontification by the destination buffer's own mode.

Adapted from gptel-agent's tool-preview rendering: the trick is that
the destination buffer doesn't itself run MODE — fontification happens
in isolation, and only the resulting face properties are copied over."
  (let ((src (current-buffer)))
    (with-temp-buffer
      (insert-buffer-substring-no-properties src start end)
      ;; A trailing sentinel guarantees a property change at point-max so
      ;; the last span's faces are written back.
      (insert " ")
      (delay-mode-hooks (funcall mode))
      (font-lock-ensure)
      (let ((pos (point-min)))
        (while (< pos (1- (point-max)))
          (let* ((next (next-property-change pos nil (1- (point-max))))
                 (face-prop (get-text-property pos 'face)))
            (when face-prop
              (put-text-property
               (+ start (- pos (point-min)))
               (+ start (- (or next (1- (point-max))) (point-min)))
               'font-lock-face face-prop src))
            (setq pos (or next (1- (point-max))))))))))

;;;; Section navigation
;;
;; A "section" is a maximal run of consecutive characters that share the
;; same non-nil value of a given text PROPERTY.  Domains tag each rendered
;; item (an event, a chat, a task, …) with a per-item id under such a
;; property, then bind these commands to n/p to move between items.

(defun elark-ui-next-section (property)
  "Move point to the start of the next section keyed by text PROPERTY.
If point is inside a section, skip past it first, then move forward to
the first character of the next section.  Point does not move when there
is no following section."
  (let ((current (get-text-property (point) property))
        (pos (point)))
    (when current
      (while (and (not (eobp))
                  (equal (get-text-property (point) property) current))
        (forward-char)))
    (while (and (not (eobp))
                (not (get-text-property (point) property)))
      (forward-char))
    (when (eobp) (goto-char pos))))

(defun elark-ui-prev-section (property)
  "Move point to the start of the previous section keyed by text PROPERTY.
See `elark-ui-next-section' for what a section is.  Point does not move
when there is no preceding section."
  (let ((current (get-text-property (point) property))
        (pos (point)))
    (when current
      (while (and (not (bobp))
                  (equal (get-text-property (point) property) current))
        (backward-char)))
    (while (and (not (bobp))
                (not (get-text-property (point) property)))
      (backward-char))
    (let ((target (get-text-property (point) property)))
      (if target
          (while (and (not (bobp))
                      (equal (get-text-property (1- (point)) property) target))
            (backward-char))
        (goto-char pos)))))

(provide 'elark-ui)
;;; elark-ui.el ends here
