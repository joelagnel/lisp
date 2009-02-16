;;;; write-table-data.el
;;; Time-stamp: <2005-01-18 19:06:25 jcgs>

;;  This program is free software; you can redistribute it and/or modify it
;;  under the terms of the GNU General Public License as published by the
;;  Free Software Foundation; either version 2 of the License, or (at your
;;  option) any later version.

;;  This program is distributed in the hope that it will be useful, but
;;  WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;  General Public License for more details.

;;  You should have received a copy of the GNU General Public License along
;;  with this program; if not, write to the Free Software Foundation, Inc.,
;;  59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

(provide 'write-table-data)
(require 'cl)

;;;###autoload
(defun write-table-data (table)
  "Write the TABLE.
The data is a list of rows, in order.
Each row is a list of cells.
Each cell is a list of:
 a string representing the class option of the cell, or nil;
 the tag of the cell (td or th)
 the cell contents as a string
 an alist of the cell tag options."
  (insert "\n<table>\n")
  (dolist (row table)
    (message " Writing row=%S" row)
    (insert " <tr>\n")
    (dolist (cell row)
      (message "  Writing cell=%S" cell)
      (if cell
      (let ((tag (second cell))
	    (contents (third cell))
	    (options (fourth cell)))
	(insert "  <" tag)
	(dolist (option options)
	  (if (cdr option)
	      (insert " " (car option) "=\"" (cdr option) "\"")
	    (insert " " (car option))))
	(insert ">" contents)
	(insert "</" tag ">\n"))
      (insert "  <!-- null cell -->\n")))
    (insert " </tr>\n"))
  (insert "\n</table>\n"))


