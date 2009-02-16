;;;; multi-find-file.el -- find multiple files in one command
;;; Time-stamp: <2006-04-28 10:00:02 jcgs>

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

(provide 'multi-find-file)

(defun multi-find-file (regexp)
  "Find all files matching REGEXP, as expanded by the shell."
  (interactive "sFind files matching shell pattern: ")
  (set-buffer (get-buffer-create " *shell expansion*"))
  (erase-buffer)
  (shell-command (format "echo %s" regexp) t)
  (goto-char (point-min))
  (replace-string " " ",")
  (goto-char (point-min))
  (replace-string "\n" "")
  (mapcar 'find-file
	  (split-string (buffer-string))))

(defun find-files-matching (directory regexp)
  "In DIRECTORY, find files matching REGEXP."
  (interactive "DDirectory:
sFind files in %s matching: ")
    (mapcar 'find-file (directory-files directory t regexp)))

(defun find-files-in-tree (directory regexp)
  "In DIRECTORY and its subdirectories, find files matching REGEXP."
  (interactive "DDirectory:
sFind files in %s matching: ")
  (mapcar 'find-file (directory-files directory t regexp))
  (mapcar (lambda (file)
	    (when (and (file-directory-p file)
		       (not (string-match "/\\.+$" file)))
	      (find-files-in-tree file regexp)))
	  (directory-files directory t)))

;;; end of multi-find-file.el
