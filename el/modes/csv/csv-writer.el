;;;; csv-writer.el -- write alists into csv files

;;; Time-stamp: <2005-05-18 14:51:45 john>

;; written by John C G Sturdy, 2004-11-20

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

;;

;;  Convert alists to CSV data.

;;  This is complementary to Ulf Jasper's csv.el, which converts CSV files to alists.
;;  If you want to edit csv files manually, you might be interested in Francis Wright's csv-mode.el

;;  Main routine is `csv-insert-data' which takes a list
;;  of alists and inserts it as CSV (Comma Separated Value) data into the current buffer.
;;  The first line of the CSV file contains the list of
;;  keys.  For example:

;;  ((("Key1" "Value1a") ("Key 2" "Value1b") ("Key3" "Value1c"))
;;   (("Key1" "Value2a") ("Key 2" "Value2b") ("Key3" "Very long Value
;;    2c")))

;;  gets translated into

;;  Key1,Key 2,"Key3"
;;  Value1a,Value1b,"Value1c"
;;  Value2a,Value2b,"Very long Value
;;  2c"

(provide 'csv-writer)
(require 'cl)

(defun csv-all-keys (data)
  "Return a list of all keys in DATA, which should be a list of alists."
  (let ((keys nil))
    (dolist (row data)
      (dolist (col row)
	(pushnew (car col) keys :test 'equal)))
    (nreverse keys)))			; return them in the order in which we saw them, to promote stability where possible

(defvar csv-always-quote-strings t
  "*Whether to quote string-valued cells even when they don't contain spaces.")

(defvar csv-cell-format-string "%s"
  "*Format to use for inserting strings that don't need quoting.")

(defvar csv-cell-format-quoted-string "%s"
  "*Format to use for inserting strings that need quoting.")

(defvar csv-cell-format-integer "%d"
  "*Format to use for inserting integers.")

(defvar csv-cell-format-number "%f"
  "*Format to use for inserting non-integer numbers.")

(defvar csv-cell-format-unknown "%S"
  "*Format to use for inserting things we don't specifically recognize.")
  
(defun csv-insert-cell (cell)
  "Insert a CSV data cell.
To control the format for each kind of cell, set the format variables:
  csv-always-quote-strings
  csv-cell-format-string
  csv-cell-format-quoted-string
  csv-cell-format-integer
  csv-cell-format-number
  csv-cell-format-unknown
See the documentation of each of these for what it does.

For example, you could override these if you want to pad cells with
spaces -- good for those who like padded cells?"
  (cond
   ((stringp cell)
    (setq cell (prin1-to-string cell))
    (insert (format (if (or csv-always-quote-strings
			    (string-match "[, ]" cell)
			    ;; (and (string-match " " cell) (not (string-match "," cell)))
			    )
			csv-cell-format-quoted-string
		      csv-cell-format-string)
		    cell)))
   ((integerp cell)
    (insert (format csv-cell-format-integer cell)))
   ((numberp cell)
    (insert (format csv-cell-format-number cell)))
   (t
    (insert (format csv-cell-format-unknown cell)))))

;;;###autoload
(defun csv-insert-data (data &optional specified-keys)
  "Insert DATA, as comma separated values, at point.
DATA is a list of alists.
The first row is constructed from all the keys in all of the alists,
or, if the optional second argument is given, SPECIFIED-KEYS is used.
The following rows are the values under those keys.
Missing values still get commas to delimit them.
See csv-insert-cell for details of how each cell is written."
  (let ((keys (or specified-keys
		  (csv-all-keys data)))
	(start (point)))
    (dolist (key keys)
      (csv-insert-cell key)
      (insert ","))
    (insert "\n")
    (dolist (row data)
      (dolist (key keys)
	(let ((pair (assoc key row)))
	  (if pair
	      (csv-insert-cell (cdr pair)))
	  (insert ",")))
      (insert "\n"))
    (let ((end (make-marker)))
      (set-marker end (point))
      (goto-char start)
      (while (re-search-forward ",$" end t)
	(replace-match "")))))

;;;###autoload
(defun csv-write-data-to-file (file data &optional coding-system specified-keys)
  "Into FILE write DATA as comma separated values.
If optional third argument is given and non-nil, it is the coding system to use.
Optional fourth argument is the column headings to use (instead of choosing them
automatically -- see csv-insert-data for details).
See csv-insert-data for more detail."
  (find-file file)
  (when coding-system
    (set-buffer-file-coding-system coding-system))
  (erase-buffer)
  (csv-insert-data data specified-keys)
  (basic-save-buffer))

;; (defun csv-writer-test ()
;;   "Test the csv writer."
;;   (interactive)
;;   (csv-write-data-to-file
;;    "~/common/tmp/csv-writer-test.csv"
;;    '((("english" . "apple") ("german" . "apfel") ("irish". "ull") ("number" . 1))
;;      (("irish" . "bad") ("english" . "boat") ("number" . 2))
;;      (("german" . "angst") ("irish" . "bron") ("number" . 3)))))

;;; end of csv-writer.el
