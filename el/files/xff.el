;; 2002-10-27 T08:53:26-0500 (Sunday)    D. Goel
;; MODIFES WHEN LOADED.!!!
;;; From: Thomas Link <esendepe@yahoo.de>
;;; Subject: xff.el 1.1
;;; Newsgroups: gnu.emacs.sources
;;; Date: Thu, 10 Oct 2002 16:37:06 +0200
;;; Organization: Vienna University, Austria

;;; This package extends `find-file' with some special syntax of the form:

;;; 	file?keyword1=arg1&keyword2=arg2&...
;;; 	file?keyword1=arg1&keyword2:rest

;;; The REST argument may contain any characters, incl. '&'.

;;; At the moment the following keywords are defined (see `xff-commands'):

;;; line=LINE
;;; goto line char=CHAR
;;; goto char find=TEXT or rather find:SOME TEXT
;;; find text select=BEG-END
;;; select/mark the region from BEG to END

;;; Installation: Put (require 'xff) into your init file
;;; (~/.xemacs/init.el or ~/.emacs.el).

;;; Examples:

;;; 	(find-file "~/.bashrc?line=10")
;;; 	(find-file "~/.bashrc?char=10")
;;; 	(find-file "~/.bashrc?select=10-20")
;;; 	(find-file "~/.bashrc?find=TEXINPUTS")

;;; or from the command line:

;;; 	editclient.sh ~/.bashrc?find=TEXINPUTS

;;; (This requires tellib.el, which I posted here earlier this week.)

;;; Raison d'être: Under Windows I use XEmacs' winclient to open files. In
;;; opposition to gnuclient, winclient doesn't know how to tell Emacs to
;;; jump to a specific position in the file.

;;; Cheers,
;;; Thomas.
;;; xff.el --- extended find-file

;; Copyright (C) 2002 Thomas Link

;; Author: Thomas Link AKA samul AT web DOT de
;; Time-stamp: <2002-08-29>
;; Keywords:

(defconst xff-version "1.1")
(defconst xff-homepage
  "http://members.a1.net/t.link/CompEmacsXff.html")


;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; A copy of the GNU General Public License can be obtained from this
;; program's author or from the Free Software Foundation, Inc., 675 Mass
;; Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; ;---(:excerpt-beg xff :name desc)---
;; ;---(:excerpt-source "~/Etc/TL-Wiki/CompEmacsXff")---
;; 
;; This package extends `find-file' with some special syntax of the form:
;; <example>
;; 	file?keyword1=arg1&keyword2=arg2&...
;; 	file?keyword1=arg1&keyword2:rest
;; </example>
;; 
;; The REST argument may contain any characters, incl. '&'.
;; 
;; 
;; At the moment the following keywords are defined (see `xff-commands'):
;; 
;; line=LINE :: goto line
;; 
;; char=CHAR :: goto char
;; 
;; find=TEXT or rather find:SOME TEXT :: find text
;; 
;; select=BEG-END :: select/mark the region from BEG to END
;; 
;; 
;; Installation: Put =(require 'xff)= into your init file (~/.xemacs/init.el
;; or ~/.emacs.el).
;; 
;; 
;; Examples:
;; <example>
;; 	(find-file "~/.bashrc?line=10")
;; 	(find-file "~/.bashrc?char=10")
;; 	(find-file "~/.bashrc?select=10-20")
;; 	(find-file "~/.bashrc?find=TEXINPUTS")
;; </example>
;; 
;; or from the command line:
;; <example>
;; 	editclient.sh ~/.bashrc?find=TEXINPUTS
;; </example>
;; 
;; ;---(:excerpt-end xff :name desc)---


;;; Change log:

;; ;---(:excerpt-beg xff :name hist)---
;; ;---(:excerpt-source "~/Etc/TL-Wiki/CompEmacsXff")---
;; 
;; 1.1 :: Initial release (tested with XEmacs 21.4.8)
;; 
;; ;---(:excerpt-end xff :name hist)---


;;; Problems:

;; ;---(:excerpt-beg xff :name problems)---
;; ;---(:excerpt-source "~/Etc/TL-Wiki/CompEmacsXff")---
;; 
;; ;---(:excerpt-end xff :name problems)---


;;; Code:

(require 'tellib)


;; Customizations

(defgroup xff nil
  "Extended find file syntax."
  :prefix "xff-"
  :group 'find-file)

(defcustom xff-commands
  '(("line" (lambda (string)
	      (raise-frame)
	      (goto-line (string-to-int string))))
    ("char" (lambda (string)
	      (raise-frame)
	      (goto-char (string-to-int string))))
    ("find" (lambda (string)
	      (raise-frame)
	      (search-forward string)))
    ("select" (lambda (string)
		(let* ((args (mapcar #'string-to-int
				     (tellib-split-string-by-char string ?\-)))
		       (beg  (nth 0 args))
		       (end  (nth 1 args)))
		  (raise-frame)
		  (goto-char end)
		  (push-mark beg nil t))))
    )
  "*Lists of keywords and a function.
The function takes one string argument. If the function returns nil, the
further evaluation of keywords will be aborted."
  :type
  '(repeat :tag "Definition"
	   (list :tag "List"
		 :value ("" nil)
		 (string :tag "Keyword" :value "")
		 (function :tag "Function" :value nil)))
  :group 'xff)

(defcustom xff-handled-functions-list
  nil
  "*IO-Primitives handled by xff -- usually none.
The file name will always be translated."
  :type '(repeat :tag "List"
		 (function :tag "Function"))
  :group 'xff)

(defcustom xff-allow-function-calls-flag nil
  "*Non-nil means, allow the user to call functions via xff."
  :type 'boolean
  :group 'xff)
(put 'xff-allow-function-calls-flag 'risky-local-variable t)

(defcustom xff-allow-set-variables-flag nil
  "*Non-nil means, allow the user to set variables via xff."
  :type 'boolean
  :group 'xff)
(put 'xff-allow-set-variables-flag 'risky-local-variable t)


(defun xff-parse-args (string)
  "Parse an extended find-file string
of the form file?keyword=arg&... or file?keyword:rest
The rest argument may contain any characters -- incl. '&'.
"
  (let ((argstring (tellib-split-string-by-char string ?\?)))
    (if (> (length argstring) 2)
	(tellib-error 'error "xff: Malformed argument string" string)
      (let* ((file  (car argstring))
	     (xargs (cadr argstring))
	     (lxa   (length xargs))
	     (args
	      (when xargs
		(let ((keyword "")
		      (arg     "")
		      (pos     0)
		      rv)
		  (while (string-match "[:=&]" xargs pos)
		    (let ((beg  (match-beginning 0))
			  (next (match-end 0))
			  (txt  (match-string 0 xargs)))
		      ;;;(message "%s %s %s %s %s %s %s" pos lxa txt beg next keyword arg)
		      (cond
			((string= txt ":")
			 (if (string= keyword "")
			     (setq rv (append rv `((,(substring xargs pos beg)
						    ,(substring xargs next))))
				   pos lxa)
			   (setq arg (concat arg txt)
				 pos next)))
			((string= txt "=")
			 (if (string= keyword "")
			     (setq keyword (substring xargs pos beg))
			   (setq arg (concat arg txt)))
			 (setq pos next))
			((string= txt "&")
			 (if (string= keyword "")
			     (tellib-error 'error "xff: '&' not allowed" xargs)
			   (setq rv (append rv `((,keyword
						  ,(concat arg
							   (substring xargs
								      pos beg)))))
				 keyword ""
				 arg     ""
				 pos     next))))))
		  (cond
		   ((not (string= keyword ""))
		    (setq rv (append rv `((,keyword
					   ,(concat arg (substring xargs pos lxa)))))))
		   ((not (= pos lxa))
		    (tellib-error 'error "xff: Missing last argument" xargs pos lxa)))
		  rv))))
	(cons file args)))))
;;test: (xff-parse-args "file")
;;test: (xff-parse-args "file?bla=1&blo=2")
;;test: (xff-parse-args "file?bla=&blo=")
;;test: (xff-parse-args "file?bla:1&1&blo=2")
;;test: (xff-parse-args "file?bla=\"1&blo=2")
;;test: (xff-parse-args "file?bla=1&1&blo=2") --> error, ok
;;test: (xff-parse-args "file?bla") --> error, ok
;;test: (xff-parse-args "file?bla=&blo") --> error, ok
;;test: (xff-parse-args "file?bla=1?blo=2") --> error, ok


(defun xff-handle-extended-args (arglist)
  "Handle the argument list returned by `xff-parse-args'
according to `xff-commands'."
  (dolist (this arglist)
    (let* ((cmd (car this))
	   (arg (cadr this))
	   (sym (intern-soft cmd))
	   (def (assoc cmd xff-commands)))
      (cond
       ((and def cmd arg)
	;;(message "DEBUG: %S %S" cmd arg) (sleep-for 3)
	(funcall (nth 1 def) arg)
	)
       ((and sym
	     (fboundp sym)
	     xff-allow-function-calls-flag)
	(funcall sym arg))
       ((and sym
	     (boundp sym)
	     xff-allow-set-variables-flag)
	(set sym arg))
       ((and cmd arg)
	(tellib-error 'error "xff: Invalid keyword" cmd arg))))))

;;test: (xff-handle-extended-args nil)
;;test: (xff-handle-extended-args '(("line" 3)))
;;test: (xff-handle-extended-args '(("unknown" 3)))


(defadvice find-file-noselect (around xff first activate)
  "Provide an extended syntax for find file. See `xff-parse-args' and 
`xff-commands' for an explanation."
  (let* ((arglist (xff-parse-args (ad-get-arg 0)))
	 (file    (car arglist))
	 (xargs   (cdr arglist)))
    ;;(message "DEBUG: %s %s" (ad-get-arg 0) xargs)
    ;;(sleep-for 3)
    (ad-set-args 0 (cons file (ad-get-args 1)))
    (let ((buff (current-buffer)))
      (set-buffer ad-do-it)
      (xff-handle-extended-args xargs)
      (set-buffer buff))))

;;test: (find-file "~/.bashrc")
;;test: (find-file "~/.bashrc?line=10")
;;test: (find-file "~/.bashrc?char=10")
;;test: (find-file "~/.bashrc?select=10-20")
;;test: (find-file "~/.bashrc?find=TEX")
;;test: (let ((xff-allow-function-calls-flag t))
;;	(find-file "~/.bashrc?search-forward=tex"))
;;(ad-recover-normality)


(defun xff-file-name-handler (io-primitive &rest args)
  "Handle extended file names."
  (let* ((arglist (xff-parse-args (nth 0 args)))
	 (file    (car arglist))
	 (xargs   (cdr arglist))
	 (rv      (apply io-primitive (cons file (cdr args)))))
    ;;(message "DEBUG: %S %S %S" io-primitive (cons file (cdr args)) args)
    (when (member io-primitive xff-handled-functions-list)
      (xff-handle-extended-args xargs))
    rv))

(add-to-list 'file-name-handler-alist 
	     '(".*\\?.*=.*" . xff-file-name-handler))

;(setq file-name-handler-alist
;      '(("\\`/\\[.*\\]" . tramp-file-name-handler)
;	("^/[^/:]+:" . remote-path-file-handler-function)))
;;test: (expand-file-name "~/.bashrc?line=10")


(provide 'xff)

;;; xff.el ends here

;;; ;;; Local Variables:
;;; ;;; auto-recompile:1
;;; ;;; time-stamp-format:"%y-%02m-%02d"
;;; ;;; End:
