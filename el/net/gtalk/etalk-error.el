;;; etalk-error --- Auto-create an error mail
;;
;; Copyright (C) 1994, 1998 Free Software Foundation
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
;;
;; History:
;;  eml 8/19/94 Added option for reporting the more complete test results.

;;; Commentary:
;;  Provide an easy way of getting that icky build information into a
;; mail message.
;;
;;
;; $Id: etalk-error.el,v 1.4 1998/10/11 02:18:01 zappo Exp $
;;
(require 'etalk)

;;; Code:
(defvar etalk-help-address "zappo@gnu.ai.mit.edu"
  "My address where mail can be sent for help and this bug report.")

(defun etalk-insert-subprocess-results (cmd)
  "Run CMD, and insert all sort of good stuff into default buffer."
  (insert (format "Result of running %s:\n\n" cmd))
  (message "Wait for command to finish")
  (sit-for 0)
  (insert
   (save-window-excursion
     (shell-command cmd)
     (set-buffer "*Shell Command Output*")
     (buffer-string))
   "\n\n"))

(defun etalk-variable-insert (sym)
  "Insert the SYM and it's value into text."
  (if (fboundp sym)
      (insert (format "%s: Function value\n" (symbol-name sym))))
  (if (boundp sym)
      (insert (format "%s: Variable value %S" sym))))

;;;###autoload
(defun etalk-report ()
  "Report emacs talk errors, and help create output."
  
  (interactive)
  (mail)
  (insert etalk-help-address)
  (let ((case-fold-search t))
    (if (re-search-forward "^subject:[ \t]+" (point-max) 'move)
        (insert "Report on " etalk-version))
    (if (not (re-search-forward mail-header-separator (point-max) 'move))
        (progn (goto-char (point-max))
               (insert "\n" mail-header-separator "\n")
               (goto-char (point-max)))
      (forward-line 1)))
  ;; What are they running on?
  (insert "\n" (emacs-version) "\n\n")

  (if (y-or-n-p "Does the error revolve around the binary?")
      ;; set up the build directory
      (let* ((curbuild (read-file-name "Build directory: "))
	     (default-directory (concat curbuild "/")))
	;; Now lets get right to the error reporting!
	(if (y-or-n-p "Is there an error with the configure script?")
	    (etalk-insert-subprocess-results "configure"))
	(if (y-or-n-p "Is there an error building the c source?")
	    (etalk-insert-subprocess-results "make etalk"))
	(if (y-or-n-p "Is there an error running the etalk process?")
	    (progn
	      (if (y-or-n-p "Does the error show up in the check?")
		  (etalk-insert-subprocess-results "etalk -t"))
	      (if (y-or-n-p "Does the error occur during use?")
		  (progn
		    (if (not (and (get-buffer etalk-log-buffer-name)
				  (y-or-n-p "Would the error still be in the log?")))
			(save-window-excursion
			  (let ((ob (get-buffer etalk-log-buffer-name)))
			    (kill-buffer etalk-log-buffer-name))
			  (etalk-start-one-process)
			  (etalk-send-command "verbose")
			  (save-excursion
			    (get-buffer-create "*Help*")
			    (set-buffer (get-buffer "*Help*"))
			    (delete-region (point-min) (point-max))
			    (insert "
  In order to facilitate the reporting of bugs, please enter the name

  and whatever is needed to create the bug.  When done, type in:

  \"M-C-c\" or \"M-x exit-recursive-edit\" to return to the bug reporter.
")
			    (display-buffer "*Help*"))
			  (setq etalk-inhibit-startup-message t)
			  (call-interactively 'etalk)
			  (recursive-edit)))
		    (message "Running the status commands...")
		    ;; just to make sure nothing is going on, abort...
		    (etalk-send-command "abort")
		    (etalk-send-command "version")
		    (etalk-send-command "status")
		    (etalk-send-command "hosts")
		    (etalk-send-command "device")
		    (etalk-send-command "users")
		    (insert "Results found in talk buffer:\n\n")
		    (message "Waiting for output..")
		    (sit-for 2)
		    (insert (save-excursion
			      (set-buffer etalk-log-buffer-name)
			      (buffer-string)))
		    ))))))
  (if (y-or-n-p "Is there a lisp bug?")
      (progn
	(if (y-or-n-p "Is it a completion problem?")
	    (progn
	      (etalk-variable-insert 'etalk-finger-command)
	      (etalk-insert-subprocess-results
	       (format "finger %s"
		       (read-string "Host name which causes probem: ")))))
	(message "Other lisp problems not managed in this script.")
	(sit-for 2))
    (message "Well, I don't know what the problem can be then. ;("))
  (insert "
Dear Eric,

  I really like your cool etalk program but I have some problems.

")
  (message "Please finish the message, and hit C-cC-c to send."))

(provide 'etalk-error)
;;; etalk-error ends here
