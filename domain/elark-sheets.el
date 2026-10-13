;;; elark-sheets.el --- Lark Sheets integration -*- lexical-binding: t; -*-

;; Copyright (C) 2026 bbw9n

;; Author: bbw9n <bbw9nio@gmail.com>
;; Assisted-by: Claude:claude-opus-5
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Provides Sheets domain commands for elark: reading spreadsheet
;; data as org-mode tables, writing data back, appending rows,
;; creating spreadsheets, finding cells, and exporting.
;;
;; CLI command mapping:
;;   Info:    sheets +info --spreadsheet-token X | --url X
;;   Read:    sheets +read --spreadsheet-token X --range R
;;   Write:   sheets +write --spreadsheet-token X --range R --values JSON
;;   Append:  sheets +append --spreadsheet-token X --range R --values JSON
;;   Create:  sheets +create --title X [--headers JSON] [--data JSON]
;;   Find:    sheets +find --spreadsheet-token X --find X [--range R]
;;   Export:  sheets +export --spreadsheet-token X --file-extension xlsx|csv

;;; Code:

(require 'elark-core)
(require 'elark-ui)
(require 'json)
(require 'transient)
(require 'org-table)

;;;; Customization

(defgroup elark-sheets nil
  "Lark Sheets settings."
  :group 'elark
  :prefix "elark-sheets-")

;;;; Buffer-local variables

(defvar-local elark-sheets--token nil
  "Spreadsheet token for the current buffer.")

(defvar-local elark-sheets--sheet-id nil
  "Active sheet ID for the current buffer.")

(defvar-local elark-sheets--range nil
  "Range string last used to read/write.")

(defvar-local elark-sheets--url nil
  "Spreadsheet URL for the current buffer.")

(defvar-local elark-sheets--title nil
  "Spreadsheet title for the current buffer.")

;;;; Response extraction

(defun elark-sheets--extract-info (data)
  "Extract spreadsheet + sheets info from DATA."
  (let ((inner (or (alist-get 'data data) data)))
    inner))

(defun elark-sheets--extract-values (data)
  "Extract the 2D value array from a read response DATA.
Returns a list of lists (rows of cells)."
  (or (elark--get-nested data 'data 'valueRange 'values)
      (elark--get-nested data 'data 'values)
      (elark--get-nested data 'valueRange 'values)))

(defun elark-sheets--extract-sheets (data)
  "Extract the list of sheet descriptors from info DATA."
  (or (elark--get-nested data 'sheets 'sheets)
      (elark--get-nested data 'data 'sheets 'sheets)))

(defun elark-sheets--extract-spreadsheet (data)
  "Extract the spreadsheet metadata from info DATA."
  (or (elark--get-nested data 'spreadsheet 'spreadsheet)
      (elark--get-nested data 'data 'spreadsheet 'spreadsheet)))

;;;; Org-table conversion

(defun elark-sheets--values-to-org-table (values)
  "Convert a 2D VALUES list to an Org table string.
First row is treated as the header."
  (when values
    (let ((lines nil)
          (first t))
      (dolist (row values)
        (let ((cells (mapcar (lambda (v)
                               (cond
                                ((null v) "")
                                ((numberp v) (number-to-string v))
                                ((stringp v) v)
                                (t (format "%s" v))))
                             row)))
          (push (concat "| " (mapconcat #'identity cells " | ") " |") lines))
        ;; Insert separator after first row (header)
        (when first
          (push "|-" lines)
          (setq first nil)))
      (mapconcat #'identity (nreverse lines) "\n"))))

(defun elark-sheets--org-table-to-values ()
  "Parse the Org table at point into a 2D list of strings.
Point must be inside an org table."
  (unless (org-at-table-p)
    (user-error "Not inside an org table"))
  (let ((table (org-table-to-lisp)))
    ;; org-table-to-lisp returns rows; separator lines are 'hline
    (delq nil
          (mapcar (lambda (row)
                    (unless (eq row 'hline)
                      (mapcar (lambda (cell)
                                (string-trim (or cell "")))
                              row)))
                  table))))

;;;; Info
;; CLI: sheets +info --spreadsheet-token X | --url X

(defvar elark-sheets-info-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'elark-sheets-read-at-point)
    (define-key map (kbd "?")   #'elark-sheets-dispatch)
    map)
  "Keymap for `elark-sheets-info-mode'.")

(define-derived-mode elark-sheets-info-mode special-mode
  "Lark Sheet Info"
  "Major mode for viewing Lark spreadsheet info.
Press RET on a sheet line to read its data as an org table.

\\{elark-sheets-info-mode-map}")

(defun elark-sheets--column-letter (n)
  "Convert 1-based column number N to a spreadsheet column letter.
E.g. 1->A, 26->Z, 27->AA."
  (let ((result ""))
    (while (> n 0)
      (setq n (1- n))
      (setq result (concat (string (+ ?A (% n 26))) result))
      (setq n (/ n 26)))
    result))

(defun elark-sheets--sheet-at-point ()
  "Return the sheet alist at point, or nil."
  (get-text-property (point) 'elark-sheets-sheet))

(defun elark-sheets-read-at-point ()
  "Read the sheet at point as an org table.
Fetches the first 100 rows from the sheet under the cursor."
  (interactive)
  (let ((sheet (elark-sheets--sheet-at-point)))
    (unless sheet (user-error "No sheet at point"))
    (unless elark-sheets--token
      (user-error "No spreadsheet token in this buffer"))
    (let* ((sid (or (alist-get 'sheet_id sheet) ""))
           (grid (alist-get 'grid_properties sheet))
           (col-count (or (and grid (alist-get 'column_count grid)) 26))
           (col-letter (elark-sheets--column-letter (min col-count 26)))
           (range (format "%s!A1:%s100" sid col-letter)))
      (elark-sheets-read (or elark-sheets--url elark-sheets--token) range))))

;;;###autoload
(defun elark-sheets-info (ref)
  "Show info for spreadsheet REF (token or URL)."
  (interactive "sSpreadsheet token or URL: ")
  (when (string-empty-p ref)
    (user-error "Spreadsheet token or URL is required"))
  (message "Lark: fetching spreadsheet info...")
  (let ((flag (if (string-match-p "^https?://" ref) "--url" "--spreadsheet-token")))
    (elark--run-command
     (list "sheets" "+info" flag ref)
     (lambda (data)
       (elark-sheets--display-info data ref)))))

(defun elark-sheets--display-info (data ref)
  "Display spreadsheet info DATA.  REF is the token or URL used."
  (let* ((info (elark-sheets--extract-info data))
         (ss (elark-sheets--extract-spreadsheet info))
         (sheets (elark-sheets--extract-sheets info))
         (title (or (alist-get 'title ss) ref))
         (token (or (alist-get 'token ss) ""))
         (url (or (alist-get 'url ss) ""))
         (buf (get-buffer-create (format "*Lark Sheet: %s*" title))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (elark-ui-insert-title title)
        (elark-sheets--insert-field "Token" token)
        (elark-sheets--insert-field "URL" url)
        (elark-sheets--insert-field "Owner" (or (alist-get 'owner_id ss) ""))
        (insert "\n" (propertize "Sheets" 'face 'bold) "\n"
                (elark-ui-separator 40) "\n")
        (if (null sheets)
            (insert "  (no sheets)\n")
          (dolist (s sheets)
            (let ((sid (alist-get 'sheet_id s))
                  (stitle (or (alist-get 'title s) ""))
                  (grid (alist-get 'grid_properties s))
                  (beg (point)))
              (insert "  " (propertize stitle 'face 'bold)
                      (format "  (ID: %s" (or sid ""))
                      (if grid
                          (format ", %dx%d)"
                                  (or (alist-get 'row_count grid) 0)
                                  (or (alist-get 'column_count grid) 0))
                        ")")
                      "\n")
              (put-text-property beg (point) 'elark-sheets-sheet s)))))
      (elark-sheets-info-mode)
      (setq elark-sheets--token token
            elark-sheets--url (when (string-match-p "^https?://" ref) ref))
      (goto-char (point-min))
      (setq header-line-format
            (format " Lark Sheet: %s — RET to read sheet data" title)))
    (pop-to-buffer buf)))

(defun elark-sheets--insert-field (label value)
  "Insert LABEL: VALUE if VALUE is non-empty."
  (elark-ui-insert-field label value 12))

;;;; Read — display as org-mode table
;; CLI: sheets +read --spreadsheet-token X --range R

(defvar elark-sheets-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g")       #'elark-sheets-read-refresh)
    (define-key map (kbd "C-c C-c") #'elark-sheets-write-table)
    (define-key map (kbd "C-c C-a") #'elark-sheets-append-table)
    (define-key map (kbd "?")       #'elark-sheets-dispatch)
    map)
  "Keymap for `elark-sheets-mode'.")

(define-derived-mode elark-sheets-mode org-mode
  "Lark Sheet"
  "Major mode for viewing/editing Lark spreadsheet data as org tables.
The buffer content is an Org table that can be edited normally.
Use \\[elark-sheets-write-table] to push changes back, or
\\[elark-sheets-append-table] to append new rows."
  (setq-local buffer-read-only nil))

;;;; AI context provider — shared by both info and table modes

(defun elark-sheets--ai-context ()
  "Return the AI context plist for a sheets buffer."
  (let ((token elark-sheets--token)
        (sheet-id elark-sheets--sheet-id))
    (list :domain "sheets"
          :buffer-type "spreadsheet"
          :item (list :token token :sheet-id sheet-id)
          :summary (format "Spreadsheet %s" (or token "unknown")))))

(put 'elark-sheets-mode      'elark-ai-context-provider #'elark-sheets--ai-context)
(put 'elark-sheets-info-mode 'elark-ai-context-provider #'elark-sheets--ai-context)

;;;###autoload
(defun elark-sheets-read (ref range)
  "Read spreadsheet REF (token or URL) at RANGE and display as org table.
RANGE is e.g. \"sheetId!A1:E10\" or \"A1:E10\" (uses first sheet)."
  (interactive
   (list (read-string "Spreadsheet token or URL: ")
         (read-string "Range (e.g. Sheet1!A1:E10): ")))
  (when (string-empty-p ref)
    (user-error "Spreadsheet token or URL is required"))
  (when (string-empty-p range)
    (user-error "Range is required"))
  (message "Lark: reading spreadsheet...")
  (let ((flag (if (string-match-p "^https?://" ref) "--url" "--spreadsheet-token"))
        (r range)
        (source ref))
    (elark--run-command
     (list "sheets" "+read" flag ref "--range" range)
     (lambda (data)
       (elark-sheets--display-table data source r)))))

(defun elark-sheets-read-refresh ()
  "Re-read the current range from the spreadsheet."
  (interactive)
  (unless elark-sheets--token
    (user-error "No spreadsheet token in this buffer"))
  (elark-sheets-read (or elark-sheets--url elark-sheets--token)
                    elark-sheets--range))

(defun elark-sheets--display-table (data ref range)
  "Display read DATA as an org table.  REF and RANGE are stored for refresh/write."
  (let* ((values (elark-sheets--extract-values data))
         (token (or (elark--get-nested data 'data 'spreadsheetToken) ""))
         (table-str (elark-sheets--values-to-org-table values))
         (buf-name (format "*Lark Sheet: %s*" (or range ref)))
         (buf (get-buffer-create buf-name)))
    (with-current-buffer buf
      (elark-sheets-mode)
      (setq elark-sheets--token token
            elark-sheets--range range
            elark-sheets--url (when (string-match-p "^https?://" ref) ref))
      (erase-buffer)
      (if (null values)
          (insert "(no data)\n")
        (insert table-str "\n"))
      (goto-char (point-min))
      (org-table-align)
      (setq header-line-format
            (format " Lark Sheet [%s] — %s  (C-c C-c to write back)"
                    (or token "") range)))
    (pop-to-buffer buf)))

;;;; Write — push org table back
;; CLI: sheets +write --spreadsheet-token X --range R --values JSON

(defun elark-sheets-write-table ()
  "Write the org table in the current buffer back to the spreadsheet.
Overwrites the cells in the stored range."
  (interactive)
  (unless elark-sheets--token
    (user-error "No spreadsheet token — read a sheet first"))
  (unless elark-sheets--range
    (user-error "No range — read a sheet first"))
  (goto-char (point-min))
  (unless (re-search-forward "^|" nil t)
    (user-error "No org table found in buffer"))
  (goto-char (match-beginning 0))
  (let* ((values (elark-sheets--org-table-to-values))
         (json-vals (json-encode values)))
    (message "Lark: writing %d rows back..." (length values))
    (elark--run-command
     (list "sheets" "+write"
           "--spreadsheet-token" elark-sheets--token
           "--range" elark-sheets--range
           "--values" json-vals)
     (lambda (_data)
       (message "Lark: spreadsheet updated (%d rows written)" (length values))))))

;;;; Append — add rows from org table
;; CLI: sheets +append --spreadsheet-token X --range R --values JSON

(defun elark-sheets-append-table ()
  "Append the org table rows (excluding header) to the spreadsheet."
  (interactive)
  (unless elark-sheets--token
    (user-error "No spreadsheet token — read a sheet first"))
  (unless elark-sheets--range
    (user-error "No range — read a sheet first"))
  (goto-char (point-min))
  (unless (re-search-forward "^|" nil t)
    (user-error "No org table found in buffer"))
  (goto-char (match-beginning 0))
  (let* ((all-values (elark-sheets--org-table-to-values))
         ;; Skip header row
         (data-rows (cdr all-values))
         (json-vals (json-encode data-rows)))
    (when (null data-rows)
      (user-error "No data rows to append (only header found)"))
    (message "Lark: appending %d rows..." (length data-rows))
    (elark--run-command
     (list "sheets" "+append"
           "--spreadsheet-token" elark-sheets--token
           "--range" elark-sheets--range
           "--values" json-vals)
     (lambda (_data)
       (message "Lark: %d rows appended" (length data-rows))))))

;;;; Write region — write selected org table rows
;; Bonus: write only the selected region back

(defun elark-sheets-write-region (start-row end-row)
  "Write rows START-ROW to END-ROW (1-indexed, inclusive) back.
Computes the correct sub-range and sends only those rows."
  (interactive "nStart row (1-indexed): \nnEnd row (inclusive): ")
  (unless elark-sheets--token
    (user-error "No spreadsheet token — read a sheet first"))
  (goto-char (point-min))
  (unless (re-search-forward "^|" nil t)
    (user-error "No org table found in buffer"))
  (goto-char (match-beginning 0))
  (let* ((all-values (elark-sheets--org-table-to-values))
         (selected (seq-subseq all-values (1- start-row) end-row))
         (json-vals (json-encode selected))
         ;; Adjust range: replace row numbers
         ;; e.g. "sheetId!A1:E10" → "sheetId!A<start>:E<end>"
         (range (elark-sheets--adjust-range elark-sheets--range start-row end-row)))
    (message "Lark: writing rows %d-%d..." start-row end-row)
    (elark--run-command
     (list "sheets" "+write"
           "--spreadsheet-token" elark-sheets--token
           "--range" range
           "--values" json-vals)
     (lambda (_data)
       (message "Lark: rows %d-%d written" start-row end-row)))))

(defun elark-sheets--adjust-range (range start end)
  "Adjust RANGE string to cover rows START to END.
E.g. \"sheet!A1:E10\" with start=3 end=5 → \"sheet!A3:E5\"."
  (if (string-match "\\([^!]*!\\)?\\([A-Z]+\\)[0-9]+:\\([A-Z]+\\)[0-9]+" range)
      (let ((prefix (or (match-string 1 range) ""))
            (col1 (match-string 2 range))
            (col2 (match-string 3 range)))
        (format "%s%s%d:%s%d" prefix col1 start col2 end))
    ;; Fallback: return as-is
    range))

;;;; Create
;; CLI: sheets +create --title X [--headers JSON] [--data JSON]

;;;###autoload (autoload 'elark-sheets-create "elark-sheets" nil t)
(transient-define-prefix elark-sheets-create ()
  "Create a new Lark spreadsheet."
  ["Spreadsheet"
   ("t" "Title"        "--title=" :prompt "Title: ")
   ("h" "Headers"      "--headers=" :prompt "Headers JSON array: ")
   ("d" "Data"         "--data=" :prompt "Data JSON 2D array: ")
   ("f" "Folder token" "--folder-token=" :prompt "Folder token: ")]
  ["Actions"
   ("RET" "Create" elark-sheets--do-create)
   ("q"   "Cancel" transient-quit-all)])

(defun elark-sheets--do-create (&rest _args)
  "Execute spreadsheet creation."
  (interactive)
  (let ((args (transient-args 'elark-sheets-create)))
    (unless args
      (user-error "No details provided"))
    (message "Lark: creating spreadsheet...")
    (elark--run-command
     (append '("sheets" "+create") args)
     (lambda (data)
       (let ((token (or (elark--get-nested data 'data 'spreadsheet 'spreadsheet_token)
                        (elark--get-nested data 'data 'token))))
         (message "Lark: spreadsheet created%s"
                  (if token (format " (token: %s)" token) "")))))))

;;;; Create from org table in current buffer

;;;###autoload
(defun elark-sheets-create-from-table (title)
  "Create a new spreadsheet from the org table at point.
TITLE is the spreadsheet name.  First row becomes headers."
  (interactive "sSpreadsheet title: ")
  (when (string-empty-p title)
    (user-error "Title is required"))
  (unless (org-at-table-p)
    (user-error "Not inside an org table"))
  (let* ((all (elark-sheets--org-table-to-values))
         (headers (car all))
         (data (cdr all))
         (args (list "sheets" "+create" "--title" title
                     "--headers" (json-encode headers))))
    (when data
      (setq args (append args (list "--data" (json-encode data)))))
    (message "Lark: creating spreadsheet from table...")
    (elark--run-command
     args
     (lambda (data)
       (let ((token (or (elark--get-nested data 'data 'spreadsheet 'spreadsheet_token)
                        (elark--get-nested data 'data 'token))))
         (message "Lark: spreadsheet created%s"
                  (if token (format " (token: %s)" token) "")))))))

;;;; Find
;; CLI: sheets +find --spreadsheet-token X --find X [--range R]

;;;###autoload
(defun elark-sheets-find (ref query &optional range)
  "Find cells matching QUERY in spreadsheet REF.
Optional RANGE limits the search area."
  (interactive
   (list (read-string "Spreadsheet token or URL: ")
         (read-string "Find text: ")
         (let ((r (read-string "Range (optional, e.g. Sheet1!A1:Z100): ")))
           (unless (string-empty-p r) r))))
  (when (string-empty-p ref)
    (user-error "Spreadsheet token or URL is required"))
  (when (string-empty-p query)
    (user-error "Search text is required"))
  (message "Lark: searching spreadsheet...")
  (let* ((flag (if (string-match-p "^https?://" ref) "--url" "--spreadsheet-token"))
         (args (list "sheets" "+find" flag ref "--find" query)))
    (when range
      (setq args (append args (list "--range" range))))
    (elark--run-command
     args
     (lambda (data)
       (elark-sheets--display-find-results data query)))))

(defun elark-sheets--display-find-results (data query)
  "Display find results DATA for QUERY."
  (let* ((results (or (elark--get-nested data 'data 'find_result)
                      (elark--get-nested data 'data 'results)
                      (alist-get 'data data)))
         (matched (or (and (listp results)
                           (alist-get 'matched_cells results))
                      (and (listp results) results)))
         (buf (get-buffer-create (format "*Lark Sheet Find: %s*" query))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize (format "Find: \"%s\"" query) 'face 'bold) "\n"
                (elark-ui-separator 60) "\n\n")
        (cond
         ((and (listp matched) matched)
          (dolist (cell matched)
            (let ((range (or (alist-get 'range cell)
                             (alist-get 'cell cell) ""))
                  (val (or (alist-get 'value cell) "")))
              (insert (propertize range 'face 'font-lock-constant-face)
                      ": " (format "%s" val) "\n"))))
         ((listp results)
          (insert (pp-to-string results) "\n"))
         (t
          (insert "(no matches)\n"))))
      (special-mode)
      (goto-char (point-min)))
    (pop-to-buffer buf)))

;;;; Export
;; CLI: sheets +export --spreadsheet-token X --file-extension xlsx|csv

;;;###autoload
(defun elark-sheets-export (ref extension &optional output-path)
  "Export spreadsheet REF as EXTENSION (xlsx or csv).
OUTPUT-PATH is the local save path."
  (interactive
   (list (read-string "Spreadsheet token or URL: ")
         (completing-read "Format: " '("xlsx" "csv") nil t)
         (read-file-name "Save to: ")))
  (when (string-empty-p ref)
    (user-error "Spreadsheet token or URL is required"))
  (message "Lark: exporting spreadsheet...")
  (let* ((flag (if (string-match-p "^https?://" ref) "--url" "--spreadsheet-token"))
         (args (list "sheets" "+export" flag ref
                     "--file-extension" extension)))
    (when (and output-path (not (string-empty-p output-path)))
      (setq args (append args (list "--output-path" (expand-file-name output-path)))))
    (elark--run-command
     args
     (lambda (data)
       (let ((path (or (alist-get 'path data)
                       (elark--get-nested data 'data 'path))))
         (message "Lark: exported%s"
                  (if path (format " → %s" path) "")))))))

;;;; Transient dispatch

;;;###autoload (autoload 'elark-sheets-dispatch "elark-sheets" nil t)
(transient-define-prefix elark-sheets-dispatch ()
  "Lark Sheets commands."
  ["Read"
   ("i" "Info"                elark-sheets-info)
   ("r" "Read as org table"   elark-sheets-read)
   ("/" "Find cells"          elark-sheets-find)]
  ["Write"
   ("c" "Create spreadsheet"  elark-sheets-create)
   ("C" "Create from table"   elark-sheets-create-from-table)]
  ["Export"
   ("e" "Export"              elark-sheets-export)]
  ["In Buffer (org table)"
   ("g"   "Refresh"           elark-sheets-read-refresh)
   ("w"   "Write back"        elark-sheets-write-table)
   ("a"   "Append rows"       elark-sheets-append-table)])

(provide 'elark-sheets)
;;; elark-sheets.el ends here
