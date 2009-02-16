;;;; hyphenate.el -- hyphenate two adjacent words
;;; Time-stamp: <2005-08-10 08:45:50 jcgs>

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

(provide 'hyphenate)

;;;###autoload
(defun hyphenate ()
  "Hyphenate the words before and after point."
  (interactive)
  (delete-horizontal-space)
  (insert ?-))

;;; end of hyphenate.el
