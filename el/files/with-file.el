;;;; with-file.el -- perform an operation on a file, restoring other state afterwards
;;; Time-stamp: <2005-07-06 18:28:02 jcgs>

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

(provide 'with-file)

(defun with-save-excursions-function (file form)
  "Select FILE in the current buffer, and eval FORM, then restore."
  (let* ((found (get-file-buffer file))
	 (top (eq (current-buffer) found)))
    (save-window-excursion
      (find-file file)
      (save-restriction
	(widen)
	(goto-char (point-min))
	(save-window-excursion
	(eval form)))
      (if (not top)
	  (bury-buffer (current-buffer)))
      (if (not found)
	  (kill-buffer (current-buffer))))))

(defmacro with-file (file form)
  "Select FILE in the current buffer, and eval FORM, then restore."
  (list 'with-save-excursions-function
	file
	(list 'quote
	      form)))

;;; end of with-file.el
