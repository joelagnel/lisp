;;; sql-completion.el --- Completion in *SQL* for mysql

;; Copyright 2006 Ye Wenbin
;;
;; Author: wenbinye@163.com
;; Version: $Id: sql-completion.el,v 1.2 2006/08/27 09:51:41 ywb Exp $
;; Keywords: 
;; X-URL: not distributed yet

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; 

;; Put this file into your load-path and the following into your ~/.emacs:
;; (require 'sql-completion)
;; (setq sql-interactive-mode-hook
;;       (lambda ()
;;         (define-key sql-interactive-mode-map "\t" 'comint-dynamic-complete)
;;         (sql-mysql-completion-init)))

;;; Code:

(provide 'sql-completion)
(eval-when-compile
  (require 'cl))
(require 'mysql)

(defvar sql-mysql-exclude-databases '("mysql" "information_schema")
  "The databases that will not appear in `sql-mysql-schema'")
(defvar sql-mysql-databases nil
  "The databases will build `sql-mysql-schema'. If you set this
variable non nil, the sql-mysql-schema will order as this.")

(defvar sql-mysql-schema nil)
(defvar sql-mysql-tables nil)
(defvar sql-mysql-fields nil)

(defvar sql-mysql-database nil "Current databases")
(defvar sql-mysql-update-databases nil)
(defvar sql-mysql-parse-function 'sql-mysql-parse-result)

(defvar sql-mysql-command-alist
  '("ALTER" "ANALYZE" "CACHE" "CHECK" "CHECKSUM" "COMMIT" "CREATE"
    "DELETE" "DESCRIBE" "DO" "DROP" "FLUSH" "GRANT" "HANDLER" "INSERT"
    "JOIN" "KILL" "LOAD" "LOAD" "LOCK" "OPTIMIZE" "REPAIR" "REPLACE"
    "RESET" "RESTORE" "REVOKE" "ROLLBACK" "SAVEPOINT" "SELECT" "SET"
    "SHOW" "START" "START" "STOP" "TRUNCATE" "UNION" "UPDATE" "USE"))

(defvar sql-mysql-other-alist
  '("FROM" "TO" "INTO" "LIMIT" "ORDER" "BY" "GROUP" "TABLE"
    "WHERE" "DATABASE" "INDEX" "VALUES" "SET" "LIKE"))

(defvar sql-mysql-command-option-alist
  '(("ALTER" "ADD" "KEY" "PRIMARY" "FOREIGN" "CHANGE" "ALTER"
     "MODIFY" "DEFAULT" "DROP" "DISABLE" "RENAME" "ENABLE"
     "CONVERT" "CHARACTER" "DISCARD" "IMPORT" "UNIQUE")
    ("CREATE" "DATABASE" "IF" "NOT" "EXISTS" "TABLE" "INDEX" "TINYINT"
     "SMALLINT" "MEDIUMINT" "INT" "INTEGER" "BIGINT" "REAL" "FLOAT"
     "DOUBLE" "DATE" "TIME" "NUMERIC" "TIMESTAMP" "TINYBLOB" "BLOB"
     "CHAR" "VARCHAR" "MEDIUMBLOB" "LONGBLOB" "TINYTEXT" "TEXT"
     "MEDIUMTEXT" "LONGTEXT" "ENUM" "SET" "REFERENCES"
     "TEMPORARY")
    ("GRANT" "WITH" "REQUIRE")
    ("HANDLER" "CLOSE" "READ" "FIRST" "NEXT" "OPEN" "PREV" "LAST")
    ("INSERT" "INTO" "VALUES" "ON" "DUPLICATE" "KEY" "UPDATE" "SET")
    ("LOAD" "DATA" "INFILE" "LOCAL" "INTO" "TABLE" "IGNORE" "FIELDS"
     "TERMINATED" "BY" "ENCLOSED" "ESCAPE" "LINES" "STARTING")
    ("SELECT" "INTO" "OUTFILE" "DUMPFILE" "FROM" "HAVING" "ASC" "DESC"
     "PROCEDURE")
    ("SET" "PASSWORD" "TRANSACTION")
    ("SHOW" "CHARACTER" "SET" "COLLATION" "COLUMNS" "CREATE" "TABLES"
     "DATABASES" "ENGINES" "INDEX" "GRANTS" "INNODB" "STATUS"
     "LOGS" "PRIVILEGES" "VARIABLES" "WARNINGS" "ERRORS")
    ("START" "SLAVE")
    ("START" "TRANSACTION")
    ("STOP" "SLAVE")))

(defun sql-mysql-completion-init ()
  (interactive)
  (set (make-local-variable 'comint-input-filter-functions)
       '(sql-mysql-input-filter))
  (set (make-local-variable 'comint-dynamic-complete-functions)
       '(sql-mysql-complete-command
         sql-mysql-complete-comopt
         sql-mysql-complete-table
         sql-mysql-complete-database
         sql-mysql-complete-others
         sql-mysql-complete-field))
  (unless sql-mysql-schema
    (or sql-mysql-databases
        (setq sql-mysql-databases
              (remove-if (lambda (db)
                           (member db sql-mysql-exclude-databases))
                         (mapcar 'car (cdr (mysql-shell-query "show databases"))))))
    (setq sql-mysql-update-databases sql-mysql-databases)
    (sql-mysql-build-databases)))

;;; build database struct
(defun sql-mysql-build-databases ()
  (let (sql tbls dblist)
    (dolist (db sql-mysql-update-databases)
      (setq tbls (cdr (mysql-shell-query "show tables" db)))
      (if (setq dblist (assoc db sql-mysql-schema))
          (setcdr dblist tbls)
        (setq sql-mysql-schema (append sql-mysql-schema (list (cons db tbls)))))
      (dolist (tbl tbls)
        (setq sql (concat sql (format "desc %s.%s;" db (car tbl))))))
    (mysql-query-background sql (mysql-connect)
                            'sql-mysql-parse-struct)))

(defun sql-mysql-parse-struct ()
  (let ((dbs sql-mysql-update-databases)
        db tbl)
    (setq db (cdr (assoc (car dbs) sql-mysql-schema))
          tbl (car db)
          db (cdr db)
          dbs (cdr dbs))
    (dolist (desc (cdr (split-string (mysql-output mysql-process)
                                     "Field\tType\tNull\tKey\tDefault\tExtra\n")))
      (setcdr tbl (mapcar 'car (mapcar 'split-string (split-string desc "\n" t))))
      (setq tbl (car db)
            db (cdr db))
      (if (null db)
          (setq db (cdr (assoc (car dbs) sql-mysql-schema))
                dbs (cdr dbs))))
    (setq sql-mysql-tables nil
          sql-mysql-fields nil)
    (dolist (db sql-mysql-schema)
      (setq db (cdr db))
      (dolist (tbl db)
        (add-to-list 'sql-mysql-tables (car tbl))
        (mapc (lambda (field) (add-to-list 'sql-mysql-fields field)) (cdr tbl))))
    (mysql-disconnect mysql-process)))

;;;###autoload
(defun sql-mysql-update-database (db)
  "If you make some change in certain database, you can update
`sql-mysql-schema' use this command. "
  (interactive (list
                (progn
                  (setq sql-mysql-update-databases nil)
                  (let ((newdb (mapcar 'car (cdr (mysql-shell-query "show databases")))))
                    (dolist (new newdb)
                      (if (and (not (member new sql-mysql-databases))
                               (not (member new
                                            sql-mysql-exclude-databases)))
                          (setq sql-mysql-databases (append sql-mysql-databases (list new))
                                sql-mysql-update-databases (list new))))
                    (completing-read "Database name: " sql-mysql-databases)))))
  (when (member db sql-mysql-databases)
    (setq sql-mysql-update-databases
          (append sql-mysql-update-databases (list db))))
  (when sql-mysql-update-databases
    (sql-mysql-build-databases)))

;;; completion functions
(defun sql-mysql-complete-command ()
  (let ((opt (comint-match-partial-filename)))
    (when (and opt
               (eq (match-beginning 0)
                   (save-excursion (comint-bol nil) (point))))
      (let ((success (let ((comint-completion-addsuffix nil))
                       (comint-dynamic-simple-complete
                        (upcase opt)
                        sql-mysql-command-alist))))
        (when success
          (upcase-region (save-excursion (backward-word) (point))
                         (point))
          (if (and (memq success '(sole shortest))
                   comint-completion-addsuffix)
              (insert " ")))
        success))))

(defun sql-mysql-complete-comopt ()
  (let ((opt (comint-match-partial-filename))
        (cmd (upcase (save-excursion
                       (comint-bol nil)
                       (skip-chars-forward " \t")
                       (current-word)))))
    (when opt
      (let ((success (let ((comint-completion-addsuffix nil))
                       (comint-dynamic-simple-complete
                        (upcase opt)
                        (assoc cmd sql-mysql-command-option-alist)))))
        (when success
          (upcase-region (save-excursion (backward-word) (point))
                         (point))
          (if (and (memq success '(sole shortest))
                   comint-completion-addsuffix)
              (insert " ")))
        success))))

(defun sql-mysql-complete-database ()
  (let ((opt (comint-match-partial-filename))
        (comint-completion-addsuffix nil))
    (when opt
      (comint-dynamic-simple-complete opt sql-mysql-databases))))

(defun sql-mysql-complete-table ()
  (let ((opt (comint-match-partial-filename))
        (db sql-mysql-database)
        (comint-completion-addsuffix nil)
        pos alist)
    (when opt
      (when (setq pos (string-match "\\." opt))
        (setq db (split-string opt "\\.")
              opt (cadr db)
              db (car db)))
      (when (assoc db sql-mysql-schema)
        (setq alist (mapcar 'car (assoc-default db sql-mysql-schema)))
        (comint-dynamic-simple-complete opt alist)))))

(defun sql-mysql-complete-field ()
  (let ((opt (comint-match-partial-filename)) pos tbl
        (comint-completion-addsuffix nil))
    (when opt
      (if (string-match "[.=]" opt)
          (setq opt (car (last (split-string opt "[.=]")))))
      (comint-dynamic-simple-complete opt sql-mysql-fields))))

(defun sql-mysql-complete-others ()
  (let ((opt (comint-match-partial-filename)))
    (when opt
      (if (comint-dynamic-simple-complete (upcase opt) sql-mysql-other-alist)
          (upcase-region (save-excursion (backward-word) (point))
                         (point))))))
        
(defun sql-mysql-input-filter (sql)
  "When use sql \"use dbname\" to change database, change variable
sql-mysql-database too."
  (when (string-match "\\s-*use\\s-+\\([a-zA-Z_]+\\)" sql)
    (let ((db (match-string 1 sql)))
      (when (member db sql-mysql-databases)
        (setq sql-mysql-database db)))))

;;; sql-completion.el ends here
