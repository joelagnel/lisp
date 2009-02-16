;;;; filename-cases.el -- fix up filename cases
;;; Time-stamp: <2005-07-06 18:31:01 jcgs>

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

(provide 'filename-cases)

(defun rename-file-cleverly (old new &rest extras)
  "Rename OLD to NEW, in such a way as not to upset
things if they differ in case alone."
  (interactive "fRename file:
FRename %s to: ")
  (if (string= (downcase old) (downcase new))
      (let ((intermediate (concat old "##")))
	(apply 'rename-file old intermediate extras)
	(apply 'rename-file intermediate new extras))
    (apply 'rename-file old new extras)))

(defun downcase-filename (filename)
  "Downcase FILENAME. If directory, do so for its contents."
  (interactive "fFile or directory to downcase: ")
  (if (not (string-match "^..?$" (file-name-nondirectory filename)))
      (if (file-directory-p filename)
	  (mapcar 'downcase-filename (directory-files filename t))
	(let ((newname (expand-file-name
			(downcase (file-name-nondirectory filename))
			(file-name-directory filename))))
	  (if (not (string= filename newname))
	      (rename-file-cleverly filename
				    newname))))))

;;; end of filename-cases.el
