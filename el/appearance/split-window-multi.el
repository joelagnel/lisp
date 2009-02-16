;;;; split-window-multi.el -- multi-way split of window
;;; Time-stamp: <2006-05-05 17:30:56 john>

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

(provide 'split-window-multi)

;;;###autoload
(defun split-window-multi (how-many &optional horizontal each-window-function)
  "Make HOW-MANY windows in a column or row.
Each window is given a different buffer (until we run out of buffers)."
  (interactive "nNumber of windows in this column (+ve) or row (-ve): ")
  (when (< how-many 0) (setq how-many (- how-many) horizontal t))
  (let ((i 1)
	(per-window (/ (if horizontal (window-width) (window-height)) how-many)))
    (while (< i how-many)
      (split-window nil per-window horizontal)
      (when each-window-function (funcall each-window-function))
      (other-window 1)
      (switch-to-buffer (other-buffer))
      (incf i))
    (when each-window-function (funcall each-window-function))
    ;; (other-window 1)
    ))

;;;###autoload
(defun split-window-grid (horizontally vertically)
  "Split the window into HORIZONTALLY windows across and VERTICALLY windows down.
Each window is given a different buffer (until we run out of buffers)."
  (interactive "nNumber of windows across: 
nNumber of windows down: ")
  (split-window-multi
   horizontally t
   (function
    (lambda ()
      (split-window-multi vertically nil nil)))))
      
;;;###autoload
(defun split-window-squarishly (n)
  "Split the window into at least N windows, with about the same number across and down.
Each window is given a different buffer (until we run out of buffers)."
  (interactive "NNumber of windows: ")
  (let* ((fside (sqrt n))
	 (iside (floor fside)))
    (split-window-grid
     iside
     (if (= (* iside iside) n) iside (1+ iside)))))

;;; end of split-window-multi.el
