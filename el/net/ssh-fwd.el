;;; ssh-fwd.el --- a ssh wrapper for TCP/IP connections

;; Copyright (C) 2002 Katsumi Yamaoka

;; Author: Katsumi Yamaoka <yamaoka@jpl.org>
;; Created: 2002/11/11
;; Revised: 2002/11/20
;; Keywords: ssh forward

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This program provides secure TCP/IP connections using the ssh port
;; forwarding.  The ssh protocol version 2 is required.  To use this
;; program, you only need to load this file and specify the option
;; `ssh-forward-list' as follows:
;;
;;(require 'ssh-fwd)
;;(setq ssh-forward-list
;;      '((11011 "POPSERVER" 110) (25025 "MAILHOST" "smtp")))
;;
;; See the docstring for this option for details.
;;
;; Note that this program advises the functions `open-network-stream'
;; and `delete-process' to turn on and off the ssh port forwarding.
;; If a network process will not be deleted by using of the function
;; `delete-process' (e.g. killing a process buffer), you have to
;; delete a ssh wrapper process manually.

;;; Code:

(defvar ssh-forward-list nil
  "*List of configurations for the ssh port forwarding.
Each element is a list of a local port number, a name of a remote host,
and a remote port number or a service name.  For example:

\(setq ssh-forward-list
      '((11011 \"POPSERVER\" 110) (25025 \"MAILHOST\" \"smtp\")))

If you use a service name instead of a remote port number, it should
be specified in the option `ssh-forward-service-alist'.")

(defvar ssh-forward-additional-arguments (list "-C")
  "*List of additional arguments passed to the ssh command.")

(defvar ssh-forward-ssh-command "ssh"
  "*Name of the ssh command.")

(defvar ssh-forward-opening-message-regexp "Entering interactive session"
  "*Regexp matching the opening message of a ssh connection.")

(defvar ssh-forward-service-alist
  '(("smtp" . 25) ("pop" . 110) ("nntp" . 119) ("imap" . 143))
  "*Alist of service names and port numbers.")

(defvar ssh-forward-localhost-name "localhost"
  "*String used for a localhost name.")

(defvar ssh-forward-localhost-regexp
  (let ((system-name (system-name)))
    (concat "\\`127\\.0\\.0\\.1\\'\\|\\`localhost\\'\\|\\`"
	    (regexp-quote (substring system-name
				     0 (string-match "\\." system-name)))
	    "\\>"))
  "*Regexp matching the name of a localhost.")

(defvar ssh-forward-timeout 10
  "*Number of seconds to wait for starting up a ssh connection.
Give up if a connection is not established in this number of seconds.")

(defvar ssh-forward-release-time 60
  "*Number of seconds to delay releasing a ssh wrapper process when a
connection is closed.")

(defvar ssh-forward-config-wrapper-alist nil
  "Alist of configurations and wrapper processes.")

(defvar ssh-forward-process-wrapper-alist nil
  "Alist of network processes and wrapper processes.")

(defvar ssh-forward-wrapper-timer-alist nil
  "Alist of wrapper processes and timers for deleting wrapper processes.")

(eval-and-compile
  ;; needed for Emacs 19 and XEmacs.
  (autoload 'cancel-timer "timer"))

(defun ssh-forward-delete-process (process &optional kill-buffer)
  "Delete PROCESS and a corresponding buffer if KILL-BUFFER is non-nil."
  (let ((fn (if (fboundp 'ad-Orig-delete-process)
		'ad-Orig-delete-process
	      'delete-process))
	(buffer (if kill-buffer
		    (condition-case nil
			(process-buffer process)
		      (error nil)))))
    (condition-case nil
	(funcall fn process)
      (error))
    (if (and buffer
	     (buffer-live-p buffer))
	(kill-buffer buffer))))

(defun ssh-forward (local host remote)
  "Start a ssh wrapper process to forward a REMOTE port of a HOST to a
LOCAL port.  LOCAL should be a number, HOST should be the name of a
remote host, REMOTE should be a number or a service name.  If REMOTE is
a service name, it should be specified in `ssh-forward-service-alist'."
  (let* ((config (list local host remote))
	 (elem (assoc config ssh-forward-config-wrapper-alist))
	 (wrapper (cdr elem))
	 (process-connection-type t))
    (if (and wrapper
	     (let (timer)
	       (setq ssh-forward-wrapper-timer-alist
		     (delq (setq timer (assq wrapper
					     ssh-forward-wrapper-timer-alist))
			   ssh-forward-wrapper-timer-alist))
	       (condition-case nil
		   (cancel-timer (cdr timer))
		 (error)))
	     (memq (process-status wrapper) '(open run)))
	wrapper
      (if wrapper
	  (progn
	    (setq ssh-forward-config-wrapper-alist
		  (delq elem ssh-forward-config-wrapper-alist))
	    (ssh-forward-delete-process wrapper t)))
      (let* ((service (if (numberp remote)
			  remote
			(cdr (assoc remote ssh-forward-service-alist))))
	     (name (apply 'format "*%s forwarding %d:%s:%s*"
			  ssh-forward-ssh-command config))
	     (buffer (get-buffer-create (concat " " name)))
	     (timeout ssh-forward-timeout)
	     (time 0))
	(save-excursion
	  (set-buffer buffer)
	  (erase-buffer)
	  (setq case-fold-search nil)
	  (setq wrapper
		(if service
		    (condition-case code
			(apply
			 'start-process name buffer ssh-forward-ssh-command
			 (append
			  ssh-forward-additional-arguments
			  (list "-L" (format "%d:%s:%d" local host service)
				"-N" "-v" host)))
		      (error
		       (message "%s" (error-message-string code))
		       (sit-for 1)
		       nil))))
	  (unwind-protect
	      (if wrapper
		  (while (and (> timeout 0)
			      (memq (process-status wrapper) '(open run))
			      (progn
				(goto-char (point-min))
				(not (re-search-forward
				      ssh-forward-opening-message-regexp
				      nil t))))
		    (setq time (+ time 0.1)
			  timeout (- timeout time))
		    (sleep-for (setq time (+ time 0.1)))))
	    (if (and wrapper
		     (memq (process-status wrapper) '(open run))
		     (progn
		       (goto-char (point-min))
		       (re-search-forward ssh-forward-opening-message-regexp
					  nil t)))
		(progn
		  (process-kill-without-query wrapper)
		  (setq ssh-forward-config-wrapper-alist
			(cons (cons config wrapper)
			      ssh-forward-config-wrapper-alist)))
	      (ssh-forward-delete-process wrapper)
	      (setq wrapper nil)
	      (let ((name (concat "FAILED:" (buffer-name))))
		(rename-buffer name t)
		(message "ssh forwarding failed; see the buffer \"%s\"" name)
		(sit-for 1)))))
	wrapper))))

(defun ssh-forward-kill-wrapper (process)
  "Kill a ssh wrapper process corresponding to a network PROCESS."
  (let ((elem (assq process ssh-forward-process-wrapper-alist)))
    (if elem
	(let* ((wrapper (cdr elem))
	       (timer (assq wrapper ssh-forward-wrapper-timer-alist)))
	  (setq ssh-forward-process-wrapper-alist
		(delq elem ssh-forward-process-wrapper-alist))
	  (if (or timer ;; it has been booked to delete a wrapper
		  ;; or a wrapper is in use for another network process.
		  (rassq wrapper ssh-forward-process-wrapper-alist))
	      nil
	    (setq timer
		  (lambda (wrapper)
		    (let ((elem (assq wrapper
				      ssh-forward-wrapper-timer-alist)))
		      (if elem
			  (progn
			    (setq ssh-forward-wrapper-timer-alist
				  (delq elem ssh-forward-wrapper-timer-alist))
			    (setq ssh-forward-config-wrapper-alist
				  (delq (rassq
					 wrapper
					 ssh-forward-config-wrapper-alist)
					ssh-forward-config-wrapper-alist))
			    (ssh-forward-delete-process wrapper t))))))
	    (if (and (numberp ssh-forward-release-time)
		     (> ssh-forward-release-time 0))
		(setq ssh-forward-wrapper-timer-alist
		      (cons (cons wrapper
				  (run-at-time ssh-forward-release-time
					       nil timer wrapper))
			    ssh-forward-wrapper-timer-alist))
	      (funcall timer wrapper)))))))

(defadvice open-network-stream (around start-ssh-forwarding activate)
  "Start a ssh wrapper process for the port forwarding."
  (if (string-match ssh-forward-localhost-regexp (ad-get-arg 2))
      ad-do-it
    (let ((config (rassoc (list (ad-get-arg 2) (ad-get-arg 3))
			  ssh-forward-list))
	  wrapper)
      (if (and config
	       (setq wrapper (apply 'ssh-forward config)))
	  (progn
	    (ad-set-arg 2 ssh-forward-localhost-name)
	    (ad-set-arg 3 (car config))
	    ad-do-it
	    (if ad-return-value
		(setq ssh-forward-process-wrapper-alist
		      (cons (cons ad-return-value wrapper)
			    ssh-forward-process-wrapper-alist))
	      (ssh-forward-delete-process wrapper t)))
	ad-do-it))))

(defadvice delete-process (before kill-ssh-wrapper activate)
  "Kill a ssh wrapper process."
  (ssh-forward-kill-wrapper (ad-get-arg 0)))

(provide 'ssh-fwd)

;; ssh-fwd.el ends here
