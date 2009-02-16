;;; sql-indent.el --- indentation of SQL statements

;; Copyright (C) 2000  Alex Schroeder

;; Author: Alex Schroeder <alex@gnu.org>
;; Maintainer: Alex Schroeder <alex@gnu.org>
;; Version: 1.1.2
;; Keywords: languages
;; URL: http://www.emacswiki.org/cgi-bin/wiki.pl?SqlIndent

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Indent SQL statements.

;; As the indentation of SQL statements depends not only on the previous
;; line but also on the current line, empty lines cannot always be
;; indented correctly.

;; Usage note: Loading this file will make all SQL mode buffers created
;; from then on use `sql-indent-line' for indentation.  A possible way
;; to install sql-indent.el would be to add the following to your
;; .emacs:

;; (eval-after-load "sql"
;;   '(load-library "sql-indent"))

;; Thanks:
;; Arcady Genkin <antipode@thpoon.com>

;;; Code:

(require 'sql)

;; Need the following to allow GNU Emacs 19 to compile the file.
(require 'regexp-opt)

(defcustom sql-indent-first-column-regexp
  (concat "^\\s-*" (regexp-opt '(
"select" "update" "insert" "delete" "create"
"union" "intersect" "drop" "grant"
"from" "where" "into" "group" "having" "order"
"set" "and" "or" "exists"
"--") t) "\\(\\b\\|\\s-\\)")
  "Regexp matching keywords relevant for indentation.
The regexp matches lines which start SQL statements and it matches lines
that should be indented at the same column as the start of the SQL
statement.  The regexp is created at compile-time.  Take a look at the
source before changing it.  All lines not matching this regexp will be
indented by `sql-indent-offset'."
  :type 'regexp
  :group 'SQL)

(defcustom sql-indent-offset 8
  "*Offset for SQL indentation."
  :type 'number
  :group 'SQL)

(defcustom sql-indent-maybe-tab nil
  "If non-nil, call `insert-tab' if `current-column' did not change."
  :type 'boolean
  :group 'SQL)

(defvar sql-indent-debug nil
  "If non-nil, `sql-indent-line' will output debugging messages.")

(defun sql-indent-line ()
  "Indent current line in a SQL statement."
  (interactive)
  (let ((pos (- (point-max) (point)))	; position from the end of the buffer
	(now (current-indentation))	; indentation of current line
	(before (current-column))
	new-column)
    (save-excursion
      (let* (;; t if current line should be at the first column
	     (curr (progn (beginning-of-line)
			  (looking-at sql-indent-first-column-regexp)))
	     ;; t if we are at a line with only whitespace
	     (space (looking-at "^\\s-+$"))
	     ;; indentation of previous non-empty line (see below)
	     (col 0)
	     ;; level of parenthesis in previous non-empty line
	     ;; (see below)
	     (paren 0)
	     ;; t if previous non-empty line should be at the
	     ;; first column (see below)
	     (prev nil))
	;; calc col, paren, and prev only if not on the first line
	;; of the buffer
	(when (>= (forward-line -1) 0)
	  (while (and (not (bobp))
		      (looking-at "^\\s-*$"))
	    (forward-line -1))
	  (setq prev (looking-at sql-indent-first-column-regexp)
		col (current-indentation)
		paren (let ((start (point))
			    (end (progn (end-of-line) (point))))
			(nth 0 (parse-partial-sexp start end)))))
	(if sql-indent-debug
	    (message "curr %S, prev %S, space %S, paren %d, col %S, now %d"
		     curr prev space paren col now))
	(setq new-column
	      (cond
	       ;; If we are the very first statement in the buffer, then
	       ;; this line should not be indented.  Maybe make the
	       ;; following search bound?
	       ((and curr (not prev)
		     (not (search-backward-regexp
			   sql-indent-first-column-regexp nil t))) now)
	       ;; If we are on a line with whitespace only and the
	       ;; previous line should be at the first column, toggle
	       ;; indentation by one level.  If we are on a line with
	       ;; whitespace only and the previous line should not be at
	       ;; the first column, toggle outdentation by one level.
	       (space
		(if (/= now col)
		    col
		  (if prev
		      (+ col sql-indent-offset)
		    (- col sql-indent-offset))))
	       ;; Indent the line by one level if the previous line
	       ;; should be at the first column and either a) the
	       ;; previous line opens a parenthesis or b) the current
	       ;; line should not be at the first column.
	       ((and prev (or (> paren 0) (not curr)))
		(+ col sql-indent-offset))
	       ;; Outdent the line by one level if the current line
	       ;; should be at the first column and either a) the
	       ;; previous line closes a parenthesis or b) the previous
	       ;; line should not be at the first column.
	       ((and curr (or (< paren 0) (not prev)))
		(- col sql-indent-offset))))
	;; If no new column found or if outdenting beyond column 0 (ie. the
	;; previous line was not part of a SQL statement), then indent the
	;; line the same as the previous line.
	(if (or (not new-column)
		(< new-column 0))
	    (setq new-column col))))
    (indent-line-to new-column)
    ;; If initial point was within line's indentation, position after
    ;; the indentation.  Else stay at same point in text.  Note how this
    ;; is done using the position from the end of the buffer!
    (if (> (- (point-max) pos) (point))
	(goto-char (- (point-max) pos)))
    ;; If point has not moved at all, then maybe insert a tab.
    (if (and (= before (current-column))
	     sql-indent-maybe-tab)
	(insert-tab))))

(add-hook 'sql-mode-hook
	  (function (lambda ()
		      (make-local-variable 'indent-line-function)
		      (setq indent-line-function 'sql-indent-line))))

(provide 'sql-indent)

;;; sql-indent.el ends here
