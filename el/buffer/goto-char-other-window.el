;;;; goto-char-other-window.el -- jump to a specified place in the other window
;;; Time-stamp: <2006-03-01 13:43:16 john>

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

(provide 'goto-char-other-window)

(defun goto-char-other-window ()
  "Take the number at point, and jump to that place in the other window."
  (interactive)
  (let ((position (number-at-point)))
    (other-window-or-buffer)
    (goto-char position))))

;;; end of goto-char-other-window.el
