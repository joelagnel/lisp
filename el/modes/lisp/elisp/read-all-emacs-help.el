;;;; read-all-emacs-help.el -- Show the user the help strings of all functions
;;; Time-stamp: <2006-05-06 17:59:50 jcgs>

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

(provide 'read-all-emacs-help)

(defun read-all-emacs-help (matching starting)
  "Show all the docstrings matching MATCHING, starting from STARTING."
  (interactive "sShow documentation matching: 
sStart from: ")
  (let ((symbols nil)
	(continue t))
    (mapatoms (function
	       (lambda (atom)
		 (if (commandp atom)
		     (setq symbols (cons atom symbols))))))
    (setq symbols (sort symbols (function (lambda (a b) (string< (symbol-name a) (symbol-name b))))))
    (while (and symbols
		(string< (symbol-name (car symbols)) starting))
      (setq symbols (cdr symbols)))
    (while (and symbols continue)
      (describe-function (car symbols))
      (message "Press q to quite, any other key to continue")
      (if (= (read-event) ?q) (setq continue nil))
      (message nil)
      (setq symbols (cdr symbols)))))

;;; end of read-all-emacs-help.el
