;;; -*- Mode: Emacs-Lisp -*-

;;; ilisp-bug.el --
;;; ILISP bug stuff.
;;;
;;; This file is part of ILISP.
;;; Please refer to the file COPYING for copyrights and licensing
;;; information.
;;; Please refer to the file ACKNOWLEGDEMENTS for an (incomplete) list
;;; of present and past contributors.
;;;
;;; $Id: ilisp-bug.el,v 1.3 2002/10/25 12:13:49 kevinrosenberg Exp $


(defun ilisp-bug ()
  "Generate an ilisp bug report."
  (interactive)
  (let ((buffer 
	 (if (y-or-n-p 
	      (format "Is %s the buffer where the error occurred? " 
		      (buffer-name (current-buffer))))
	     (current-buffer))))
    (if (or (not buffer)
	    ;; (not (mail))
	    (not (ignore-errors
		   (progn (compose-mail) t)))
					; 19990615 Marco Antoniotti
					; Somebody complained that
					; MAIL does not use the mail
					; agent chosen by the
					; user. Here is an attempt to
					; fix this.
					;
					; 19990912 Hannu Koivisto
					; the IGNORE-ERRORS may
					; alleviate some quirks in the
					; COMPOSE-MAIL call.
					; cf. <ilisp@cons.org> post of
					; 19990908 for details.
	    )
	(progn
	  (message 
	   (if buffer 
	       "Can't send bug report until mail buffer is empty."
	       "Switch to the buffer where the error occurred."))
	  (beep))
      (insert ilisp-bugs-to)
      (search-forward (concat "\n" mail-header-separator "\n"))
      (insert "\nYour problem: \n\n")
      (insert "Type C-c C-c to send\n")
      (insert "======= Emacs state below: for office use only =======\n")
      (forward-line 1)
      (insert (emacs-version))
      (insert 
       (if (string-match "XEmacs" emacs-version)
	   (format "\nWindow System: %s" (console-type) ) ;; XEmacs has no window-system-version
	 (format "\nWindow System: %s %s" window-system window-system-version) ) )
      (let ((mode (save-excursion (set-buffer buffer) major-mode))
	    (match "popper-\\|completer-")
	    (val-buffer buffer)
	    string)
	(if (or (memq mode lisp-source-modes) (memq mode ilisp-modes))
	    (progn
	      (setq match (concat "ilisp-\\|comint-\\|lisp-" match)
		    val-buffer (save-excursion (set-buffer buffer)
					       (or (ilisp-buffer) buffer)))
	      (mapcar (function (lambda (dialect)
				  (setq match (concat (format "%s-\\|" (car dialect))
						      match))))
		      ilisp-dialects)
	      (save-excursion
		(set-buffer buffer)
		(let ((point (point))
		      (start (lisp-defun-begin))
		      (end (lisp-end-defun-text t)))
		  (setq string
			(format "
Mode: %s
Start: %s
End: %s
Point: %s
Point-max: %s
Code: %s"
				major-mode start end point (point-max)
				(buffer-substring-no-properties start end)))))
	      (insert string)))
	(mapatoms
	 (function (lambda (symbol)
		     (if (and (boundp symbol)
			      (string-match match (format "%s" symbol))
			      (not (eq symbol 'ilisp-documentation)))
			 (let ((val (save-excursion
				      (set-buffer val-buffer) 
				      (symbol-value symbol))))
			   (if val
			       (insert (format "\n%s: %s" symbol val))))))))
	(insert (format "\nLossage: %s" (key-description (recent-keys))))
	(if (and (or (memq mode lisp-source-modes)
		     (memq mode ilisp-modes))
		 (ilisp-buffer) 
		 (memq 'clisp (ilisp-value 'ilisp-dialect t))
		 (not (cdr (ilisp-value 'comint-send-queue))))
	    (progn
	      (insert (format "\nLISP: %s"
			      (comint-remove-whitespace
			       (car (comint-send
				     (save-excursion
				       (set-buffer buffer)
				       (ilisp-process))
				     "(lisp-implementation-version)"
				     t t 'version)))))
	      (insert (format "\n*FEATURES*: %s"
			      (comint-remove-whitespace
			       (car (comint-send
				     (save-excursion
				       (set-buffer buffer)
				       (ilisp-process))
				     "(let ((*print-length* nil)
				       (*print-level* nil))
				   (print *features*)
				   nil)"
				     t t 'version)))))))
	(insert ?\n)
	(goto-char (point-min))
	(re-search-forward "^Subject")
	(end-of-line)
	(message "Send with sendmail or your favorite mail program.")))))

;;; end of file -- ilisp-bug.el --
