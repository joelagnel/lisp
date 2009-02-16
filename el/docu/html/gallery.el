;;;; gallery.el
;;; Time-stamp: <2005-01-18 19:04:55 jcgs>

;;; Make a simple gallery, from image files in this directory

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

;;;###autoload
(defun make-gallery (pattern)
  "Make a gallery in the current file, of pictures matching PATTERN"
  (interactive "sMake gallery of pictures matching: ")
  (let ((files (directory-files default-directory nil pattern nil)))
    (insert "<ul>\n")
    (while files
      (let ((file (car files)))
      (insert (format "  <li> %s:<br> <img src=\"%s\" alt=\"%s\">\n" file file file))
      (setq files (cdr files))))
    (insert "<ul>\n")))
