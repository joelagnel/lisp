;;;; shell-plus.el -- extra bits for talking to shells
;;; Time-stamp: <2006-04-24 15:53:12 john>

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

(defvar passwords nil
  "The passwords for this buffer.
Stored as an alist.")

(make-local-variable 'passwords)

(defun password-send (&optional re-enter)
  "Read a password silently, and send it to the process of this buffer."
  (interactive "P")
  (let* ((prompt (buffer-substring-no-properties (- (point) (current-column)) (point)))
	 (pair (assoc prompt passwords))
	 (password (cdr pair)))
    (when (or re-enter
	      (null password))
      (setq password (read-passwd (format "Password for \"\": " prompt)))
      (if pair
	  (rplacd pair password)
	(setq passwords (cons (cons prompt password) pair))))
    (process-send-string (get-buffer-process (current-buffer))
			 (concat password "\n"))))

;;; end of shell-plus.el
