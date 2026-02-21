;;; pgmacs-sql.el --- SQL mode integration for PGmacs -*- lexical-binding: t -*-

;; Copyright (C) 2026 Pavel Korytov

;;; Commentary:

;; Usage:
;;   1. Connect to database: M-x pgmacs RET
;;   2. Create SQL buffer: M-x pgmacs-open-sql-buffer RET
;;   3. Execute queries with C-c C-c (statement), C-c C-r (region), etc.
;;
;; Or open existing SQL file and use pgmacs-sql-mode manually.

;;; Code:

(require 'sql)

(defvar-local pgmacs-sql--connection nil
  "The pgmacs connection for this sql-mode buffer.
Set automatically or via `pgmacs-sql-set-connection'.")

(defun pgmacs-sql--table-name (table)
  "Extract table name string from TABLE object.
TABLE can be a string or a pg-qualified-name object."
  (if (pg-qualified-name-p table)
      (pg-qualified-name-name table)
    table))

(defun pgmacs-sql--get-connection ()
  "Get or find the pgmacs connection for this buffer.
Returns nil if no connection available."
  (or pgmacs-sql--connection
      (pgmacs-sql--find-recent-connection)))

(defun pgmacs-sql--find-recent-connection ()
  "Find the most recent pgmacs buffer's connection."
  (when-let ((buf (seq-find (lambda (b)
                              (with-current-buffer b
                                (and (eq major-mode 'pgmacs-mode)
                                     (bound-and-true-p pgmacs--con))))
                            (buffer-list))))
    (buffer-local-value 'pgmacs--con buf)))

(defun pgmacs-sql--list-connections ()
  "Return alist of (display-name . connection) for all pgmacs buffers."
  (let (connections)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (and (eq major-mode 'pgmacs-mode)
                   (bound-and-true-p pgmacs--con))
          (let* ((con pgmacs--con)
                 (dbname (pgcon-dbname con))
                 (display (format "%s (%s)" (buffer-name buf) dbname)))
            (push (cons display con) connections)))))
    (nreverse connections)))

(defun pgmacs-sql-set-connection (connection)
  "Set the pgmacs CONNECTION for this `sql-mode' buffer."
  (interactive
   (let ((choices (pgmacs-sql--list-connections)))
     (if (null choices)
         (user-error "No active pgmacs connections.  Run M-x pgmacs first")
       (list (cdr (assoc (completing-read "Select connection: "
                                          choices nil t)
                         choices))))))
  (setq pgmacs-sql--connection connection)
  (message "Connection set to %s" (pgcon-dbname connection)))

;;; SQL Buffer Creation

;;;###autoload
(defun pgmacs-open-sql-buffer (&optional connection)
  "Create a new SQL buffer connected to a pgmacs session.
If CONNECTION is not provided, prompts for an active pgmacs connection.
The buffer is set up with pgmacs-sql-mode enabled for query execution."
  (interactive)
  (let* ((con (or connection
                  (let ((choices (pgmacs-sql--list-connections)))
                    (if (null choices)
                        (user-error "No active pgmacs connections.  Run M-x pgmacs first")
                      (cdr (assoc (completing-read "Select connection: "
                                                   choices nil t)
                                  choices))))))
         (dbname (pgcon-dbname con))
         (buffer-name (generate-new-buffer-name (format "*SQL: %s*" dbname)))
         (buf (get-buffer-create buffer-name)))
    (with-current-buffer buf
      (sql-mode)
      (setq sql-product 'postgres)
      (pgmacs-sql-mode 1)
      (setq pgmacs-sql--connection con)
      (insert (format "-- Connected to database: %s\n" dbname))
      (insert "-- Execute queries with:\n")
      (insert "--   C-c C-c  Execute statement at point\n")
      (insert "--   C-c C-r  Execute region\n")
      (insert "--   C-c C-b  Execute buffer\n")
      (insert "--   C-c C-p  Execute paragraph\n")
      (insert "--   C-c C-l  Change connection\n")
      (insert "--   C-c C-t  Open table at point in pgmacs\n\n"))
    (pop-to-buffer buf)
    buf))

(defun pgmacs-sql-send-region (start end)
  "Execute SQL from START to END using pgmacs connection."
  (interactive "r")
  (let ((con (or (pgmacs-sql--get-connection)
                 (user-error "No pgmacs connection.  Run M-x pgmacs first")))
        (sql (string-trim (buffer-substring-no-properties start end))))
    (if (string-empty-p sql)
        (message "Empty SQL region")
      (pgmacs-show-result con sql))))

(defun pgmacs-sql-send-buffer ()
  "Execute entire buffer as SQL using pgmacs connection."
  (interactive)
  (pgmacs-sql-send-region (point-min) (point-max)))

(defun pgmacs-sql-send-paragraph ()
  "Execute current paragraph (blank-line separated) using pgmacs connection."
  (interactive)
  (let ((bounds (pgmacs-sql--paragraph-bounds)))
    (pgmacs-sql-send-region (car bounds) (cdr bounds))))

(defun pgmacs-sql-send-statement ()
  "Execute current SQL statement (semicolon-delimited) using pgmacs connection."
  (interactive)
  (let ((bounds (pgmacs-sql--statement-bounds)))
    (pgmacs-sql-send-region (car bounds) (cdr bounds))))

(defun pgmacs-sql--statement-bounds ()
  "Return (start . end) bounds of current SQL statement.
Statements are delimited by semicolons."
  (save-excursion
    (let* ((orig-point (point))
           (end (if (search-forward ";" nil t)
                    (point)
                  (point-max)))
           (start (progn
                    (goto-char orig-point)
                    (if (search-backward ";" nil t)
                        (1+ (point))
                      (point-min)))))
      (cons start end))))

(defun pgmacs-sql--paragraph-bounds ()
  "Return (start . end) bounds of current paragraph.
Paragraphs are separated by blank lines."
  (save-excursion
    (let ((start (progn (backward-paragraph) (point)))
          (end (progn (forward-paragraph) (point))))
      (cons start end))))

;;; Table Navigation

(defun pgmacs-sql-open-table ()
  "Open table at point in pgmacs row-list buffer."
  (interactive)
  (let ((con (or (pgmacs-sql--get-connection)
                 (user-error "No pgmacs connection.  Run M-x pgmacs first")))
        (table (thing-at-point 'symbol t)))
    (unless table
      (user-error "No table name at point"))
    ;; Verify table exists
    (unless (seq-find (lambda (tbl)
                        (string= (pgmacs-sql--table-name tbl) table))
                      (pg-tables con))
      (user-error "Table '%s' not found in database" table))
    ;; Find pgmacs buffer for this connection
    (let ((pgmacs-buf (seq-find (lambda (b)
                                  (with-current-buffer b
                                    (and (eq major-mode 'pgmacs-mode)
                                         (eq pgmacs--con con))))
                                (buffer-list))))
      (unless pgmacs-buf
        (user-error "No pgmacs buffer for this connection"))
      ;; Switch to pgmacs buffer and open table
      (with-current-buffer pgmacs-buf
        (pgmacs--display-table table)))))

;;; Minor Mode

(defvar pgmacs-sql-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'pgmacs-sql-send-statement)
    (define-key map (kbd "C-c C-r") #'pgmacs-sql-send-region)
    (define-key map (kbd "C-c C-b") #'pgmacs-sql-send-buffer)
    (define-key map (kbd "C-c C-p") #'pgmacs-sql-send-paragraph)
    (define-key map (kbd "C-c C-l") #'pgmacs-sql-set-connection)
    (define-key map (kbd "C-c C-t") #'pgmacs-sql-open-table)
    map)
  "Keymap for `pgmacs-sql-mode'.")

;;;###autoload
(define-minor-mode pgmacs-sql-mode
  "Minor mode for pgmacs integration in `sql-mode' buffers.

Key bindings:
\\{pgmacs-sql-mode-map}"
  :lighter " PGmacs"
  :keymap pgmacs-sql-mode-map)

(provide 'pgmacs-sql)
;;; pgmacs-sql.el ends here
