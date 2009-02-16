;;;; update-autoloads.el -- update autoloads according to my conventions
;;; See also ../mode-setups/lisp-mode-config.el for the other end of these conventions.
;;; Time-stamp: <2005-08-09 19:52:47 jcgs>

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

(provide 'update-autoloads)

;;;###autoload
(defun update-jcgs-autoloads ()
  (interactive)
  (let ((files (directory-files (substitute-in-file-name "$COMMON/emacs/autoload/")
				nil "-autoload\\.el$" t))
	(directories nil))
    (dolist (file files)
      (message "File %s" file)
      (let ((directory (expand-file-name (substring file 0 -12)
					 (substitute-in-file-name "$COMMON/emacs/"))))
	(message "File %s directory %s" file directory)
	(when (file-directory-p directory)
	  (push directory directories))))
    (apply 'update-autoloads-from-directories directories)))

;;; end of update-autoloads.el
