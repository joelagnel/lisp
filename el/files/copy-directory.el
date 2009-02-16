;;;; copy-directory.el -- copy a directory
;;; Time-stamp: <2005-07-06 18:41:42 jcgs>

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

(provide 'copy-directory)

(defun copy-directory-new-files-only (from to)
  "Copy any files in FROM which do not exist in TO, into TO."
  (interactive "DCopy new files from directory:
DCopy new files from %s into directory: ")
  (let ((files (directory-files from)))
    (while files
      (let* ((file (car files))
	     (fromfile (expand-file-name file from))
	     (tofile (expand-file-name file to)))
	(if (and (not (file-directory-p fromfile))
		 (not (file-exists-p tofile)))
	    (copy-file fromfile tofile)))
      (setq files (cdr files)))))

;;; end of copy-directory.el
