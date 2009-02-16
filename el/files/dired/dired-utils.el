;;; dired-utils.el --- utility functions for manipulating remote directories

;; Copyright (C) 2002, 2003, 2004, 2006 Katsumi Yamaoka

;; Author: Katsumi Yamaoka <yamaoka@jpl.org>
;; Created: 2002/09/04
;; Revised: 2006/01/06
;; Keywords: dired tramp ange-ftp efs

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

;;; Commentary:

;; This program works with both Emacs and XEmacs and provides the
;; following four features:
;;
;; 1. Replace the command for the `q' key in dired buffers.  If a
;;    buffer visits a remote directory, it kills a ftp process (or
;;    possibly a Tramp process) and all buffers concerned to that
;;    remote connection.  It will conform to the netiquette that you
;;    can terminate a ftp session instantly without waiting for a
;;    time-out and it makes it possible to connect other people to
;;    that host.
;;
;; 2. Make it possible to use ftp urls like `ftp://ftp.jpl.org/pub/'
;;    as well as `/ftp.jpl.org:/pub/' for the `find-file' or the
;;    `dired' commands.  Those functions were presented by Keisuke
;;    Mori in the past.
;;
;; 3. Replace the destination directory for copying files by the `C'
;;    command.  You can set that directory by customizing the variable
;;    `my-dired-dwim-target-directory'.  The default value is "/tmp/".
;;
;; 4. Enable an Emacs user to specify `coding-system-for-read' when
;;    performing the `f' command or the `o' command.  Those commands
;;    will prompt you for the coding system if you give a prefix
;;    argument.
;;
;; NOTE: You should use Tramp of the version on and after 2004-01-24.

;;; Code:

(require 'dired)

(eval-and-compile
  (condition-case nil
      (require 'tramp)
    (error)))

(eval-when-compile
  (if (featurep 'tramp)
      (require 'cl))
  (let ((fns '(tramp-buffer-name
	       tramp-dissect-file-name tramp-file-name-host
	       tramp-file-name-method tramp-file-name-multi-method
	       tramp-file-name-user))
	fn)
    (while fns
      (setq fn (car fns)
	    fns (cdr fns))
      (if (not (fboundp fn))
	  (autoload fn "tramp"))))
  (autoload 'ange-ftp-get-user "ange-ftp"))

(defvar original-dired-q-command nil
  "Original dired command for the `q' key.")

(if (not original-dired-q-command)
    (progn
      (setq original-dired-q-command (lookup-key dired-mode-map "q"))
      (define-key dired-mode-map "q" 'kill-dired-process-and-buffers)))

(defun kill-dired-process-and-buffers ()
  "Kill a ftp process (or possibly a Tramp process) and related buffers.
It will be performed if a buffer has visited a remote directory.
Otherwise, run the original `q' command in the `dired-mode'."
  (interactive)
  (let ((file (or (buffer-file-name) default-directory))
	(regexp (car (rassq 'tramp-file-name-handler
			    file-name-handler-alist)))
	parsed method buffers buffer fpath fpbuf host user)
    (if (and regexp ;; Non-nil means Tramp is available.
	     (string-match regexp file)
	     (progn
	       (setq parsed (tramp-dissect-file-name file)
		     method (or (tramp-file-name-method parsed)
				(symbol-value 'tramp-default-method)))
	       (not (equal method "ftp"))))
	;; Tramp
	(if (prog1
		(y-or-n-p "Are you sure you want to kill Tramp process? ")
	      (message ""))
	    (progn
	      (setq regexp (concat
			    "\\`"
			    (regexp-quote
			     (if (string-match
				  (if (symbol-value 'tramp-unified-filenames)
				      "\\`/\\([^/:]+:\\)+"
				    "\\`/\\[.+\\]")
				  file)
				 (match-string 0 file)
			       file))))
	      (kill-buffer (tramp-buffer-name
			    (tramp-file-name-multi-method parsed)
			    method
			    (tramp-file-name-user parsed)
			    (tramp-file-name-host parsed)))
	      (setq buffers (buffer-list))
	      (save-excursion
		(while buffers
		  (setq buffer (car buffers)
			buffers (cdr buffers))
		  (set-buffer buffer)
		  (if (and (not
			    ;; This buffer is probably unrelated to Tramp,
			    ;; e.g. telnet is running there.
			    (get-buffer-process buffer))
			   (setq file (or (buffer-file-name)
					  default-directory))
			   (string-match regexp file))
		      (kill-buffer buffer))))))
      (cond ((featurep 'efs)
	     (setq fpath 'efs-ftp-path
		   fpbuf 'efs-ftp-process-buffer))
	    (regexp ;; Tramp is available.
	     (setq fpath (lambda (name)
			   (condition-case nil
			       (let ((rest (tramp-dissect-file-name name)))
				 (list (tramp-file-name-host rest)
				       (or (tramp-file-name-user rest)
					   (ange-ftp-get-user
					    (tramp-file-name-host rest)))
				       ;;(tramp-file-name-localname rest)
				       ))
			     (error nil)))
		   fpbuf 'ange-ftp-ftp-process-buffer))
	    ((featurep 'ange-ftp)
	     (setq fpath 'ange-ftp-ftp-name
		   fpbuf 'ange-ftp-ftp-process-buffer)))
      (if (and fpath
	       (setq parsed (funcall fpath (expand-file-name file))))
	  ;; ftp
	  (if (prog1
		  (y-or-n-p "Are you sure you want to kill ftp process? ")
		(message ""))
	      (save-excursion
		(setq host (nth 0 parsed)
		      user (nth 1 parsed))
		(kill-buffer (funcall fpbuf host user))
		(setq buffers (buffer-list))
		(while buffers
		  (setq buffer (car buffers)
			buffers (cdr buffers))
		  (set-buffer buffer)
		  (if (and
		       (not
			;; This buffer is probably unrelated to ftp,
			;; e.g. telnet is running there.
			(get-buffer-process buffer))
		       (setq file (or (buffer-file-name) default-directory))
		       (setq parsed (funcall fpath (expand-file-name file)))
		       (string-equal host (nth 0 parsed))
		       (string-equal user (nth 1 parsed)))
		      (kill-buffer buffer)))))
	(if original-dired-q-command
	    (call-interactively original-dired-q-command))))))

(defun convert-ftp-url-run-real-handler (operation args)
  (let ((inhibit-file-name-handlers
	 (cons 'convert-ftp-url-to-efs-filename
	       (and (eq inhibit-file-name-operation operation)
		    inhibit-file-name-handlers)))
	(inhibit-file-name-operation operation))
    (apply operation args)))

(defun convert-ftp-url-to-efs-filename (operation string &rest args)
  (string-match "^ftp://\\([^/@]+@\\)?\\([^/~]+\\)" string)
  (let ((user (if (match-beginning 1)
		  (substring (match-string 1 string) 0 -1)
		"anonymous"))
	(host (match-string 2 string))
	(file (substring string (match-end 2))))
    (convert-ftp-url-run-real-handler
     operation
     (cons (if (or (featurep 'xemacs)
		   (rassq 'ange-ftp-hook-function file-name-handler-alist))
	       (concat "/" user "@" host ":" file)
	     (if (symbol-value 'tramp-unified-filenames)
		 (concat "/ftp:" user "@" host ":" file)
	       (error "\
`tramp-unified-filenames' must be `t' for the ftp scheme")))
	   args))))

(defun convert-ftp-url-hook-function (operation &rest args)
  (let ((fn (get operation 'convert-ftp-url-to-efs-filename)))
    (if fn (apply fn operation args)
      (convert-ftp-url-run-real-handler operation args))))

(or (assoc "^ftp://[^/~]+" file-name-handler-alist)
    (setq file-name-handler-alist
	  (cons '("^ftp://[^/~]+" . convert-ftp-url-hook-function)
		file-name-handler-alist)))

(put 'substitute-in-file-name
     'convert-ftp-url-to-efs-filename 'convert-ftp-url-to-efs-filename)
(put 'expand-file-name
     'convert-ftp-url-to-efs-filename 'convert-ftp-url-to-efs-filename)
(put 'file-name-nondirectory
     'convert-ftp-url-to-efs-filename 'convert-ftp-url-to-efs-filename)

(defvar my-dired-dwim-target-directory "/tmp/"
  "*Destination directory for copying files from a remote directory.")

(defadvice dired-dwim-target-directory (after remote-to-local activate)
  "Copy remote files to the local directory."
  (if (and ad-return-value
	   (let ((fpath (cond ((featurep 'efs)
			       'efs-ftp-path)
			      ((featurep 'ange-ftp)
			       'ange-ftp-ftp-name))))
	     (and fpath
		  (funcall fpath ad-return-value))))
      (setq ad-return-value
	    (file-name-as-directory my-dired-dwim-target-directory))))

(if (not (featurep 'xemacs))
    (let ((fns '(dired-find-file dired-find-file-other-window)))
      (while fns
	(eval
	 `(defadvice ,(car fns) (around accept-coding-system activate)
	    "Let it enable specifying of coding system."
	    (let ((coding-system-for-read
		   (if current-prefix-arg
		       (read-coding-system "Coding-system: "
					   coding-system-for-read)
		     coding-system-for-read)))
	      ;; Make conf-mode not prompt a user as
	      ;; "Regexp to match keywords: ".
	      (setq current-prefix-arg nil)
	      ad-do-it)))
	(setq fns (cdr fns)))))

(provide 'dired-utils)

;;; dired-utils.el ends here
