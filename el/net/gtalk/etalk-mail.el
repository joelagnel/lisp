;;; etalk-mail ---  connectin via mail routines
;;
;; Copyright (C) 1994 Free Software Foundation
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's author (see below) or write to:
;;
;;              The Free Software Foundation, Inc.
;;              675 Mass Ave.
;;              Cambridge, MA 02139, USA.
;;
;; Please send bug reports, etc. to zappo@gnu.ai.mit.edu.

;;; Commentary:
;;   To provide a last ditch effort to connect to someone via TCP talk
;; style program when all other avenues of connection fail, via a mail
;; facility.
;;
;; $Id: etalk-mail.el,v 1.3 1997/08/09 12:50:24 zappo Exp $
;;

;;; Code:
(defun etalk-mail-portnumber (address)
  "Create an mail buffer initialized with ADDRESS.
Sending this mail will create ringing effect for talk for poorly
enabled machines which don't have talk deamons, but do have etalk
installed locally."

  (mail)
  (goto-char (point-min))
  (re-search-forward "^To: ")
  (insert address)
  (goto-char (point-min))
  (re-search-forward "^Subject: ")
  (insert (format "Talk request on port %s\nEtalk-Reply-To: %s@%s"
		  etalk-remote-socket etalk-announce-as
		  (system-name)))
  (re-search-forward "^--text follows this line--\n")
  (insert "This is a mail message requesting the use of emacs talk.
To answer, view this message in an emacs buffer and type

             M-x etalk-mail-reply RET\n"))

(defun etalk-mail-extract-portnumber ()
  "Extract talk informaion from a mail message.
The mail must be from a requesting talk program.
Returns a list (\"address\" port#)"

  (if (not (or (equal major-mode 'vm-mode)
	       (equal major-mode 'rmail-mode)))
      (progn
	(message "Switch to a window displaying the mail message.")
	nil)
    (let ((name "")
	  (port 0))
      (goto-char (point-min))
      (if (re-search-forward "^Etalk-Reply-To: \\([^ ]+\\)$" nil t)
	  (progn
	    (setq name (buffer-substring (match-beginning 1) (match-end 1)))
	    (goto-char (point-min))
	    (if (re-search-forward
		 "^Subject: Talk request on port \\([0-9]+\\)$" nil t)
		(progn
		  (setq port (string-to-int
			      (buffer-substring (match-beginning 1)
						(match-end 1))))
		  (list name port))
	      (progn
		(message "Can't determine port.")
		nil)))
	(progn
	  (message "Can't determine sender.")
	  nil))))
  )

(provide 'etalk-mail)
;;; etalk-mail ends here
