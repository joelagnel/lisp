;;; ginel-template.el --- Template insertion

;; Copyright (C) 1998, 1999 Stefan Hornburg

;; Author: Stefan Hornburg <racke@gundel.han.de>
;; Maintainer: Stefan Hornburg <racke@gundel.han.de>
;; Version: 0.5.0
;; Keywords: extensions, languages, tools

;; This file is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 2, or (at your option) any
;; later version.

;; This file is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied warranty
;; of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file; see the file COPYING.  If not, write to the Free
;; Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; * Introduction to Templates
;; Programmers and other text writers often use similar text chunks again
;; and again. Templates are better suited to these task than
;; cut-paste-modify-cycles. They consist of fixed text, intermixed with
;; variable parts, identified by special delimiters. The variable parts,
;; referenced as "instructions" in this document, are replaced by strings
;; like the current filename, the current user, and others interactively
;; queried from the user, fetched from variables or by functions.
;;
;; * Emacs Lisp Template Processors
;; `ginel-template' is my personal approach for template processing.
;; Currently it offers not that much of functionality, but I hope it will
;; mature to be a really useful Emacs Lisp library. At the moment `tempo'
;; or `dmacro' will be a better choice for you if you're not really
;; adventurous or helpful.
;;
;; * Setup
;; Ensure that this file (preferably byte-compiled) lives in a directory
;; within your `load-path' and add
;;
;;     (require 'ginel-template)
;;
;; to your Emacs init file (~/.emacs).
;;
;; If you like, you could use `ginel-template-bindings' to define the
;; following global key bindings:
;; 
;; C-x t s       ginel-template-from-string
;; C-x t f       ginel-template-from-file
;; C-x t i       ginel-template-add-instruction
;; C-x t v       ginel-template-insert-value
;;
;; * User functions
;; ginel-template-from-string STRING
;;  Processes the template STRING and inserts the result at point into
;;  the current buffer.
;; ginel-template-from-file FILE
;;  Reads the template from FILE, processes it and inserts the result at
;;  point into the current buffer.
;; ginel-template-add-instruction NAME VALUE
;;  Adds instruction NAME and instruction VALUE Lisp expression to the
;;  front of `ginel-template-instructions-alist'.
;; ginel-template-bindings
;;  Installs global key bindings for the other user functions.
;;
;; * Instructions and directives
;; `ginel-template' scans the template for matches of
;; `ginel-template-wrapper-regexp' to detect instructions.
;; Each instruction is examined for directives. If an value can be
;; determined for an instruction, `ginel-template' replaces
;; the instruction and the corresponding delimiters with the value.
;; Otherwise the instruction will be left alone.
;;
;; (ginel-template-from-string "prefix = @prefix@")
;;     -| prefix = @prefix@
;;
;; ** The Query Directive
;; The pattern for the query directive is `ginel-template-query-regexp',
;; a leading `>' sign by default. The query directive instructs
;; `ginel-template' to ask the user if the value for an instruction cannot
;; be determined by other means.
;;
;; (ginel-template-from-string "Greetings from @>country@")
;; Value for country: greece
;;     -| Greetings from greece
;;
;; ** The Massage Directive
;; The pattern for the massage directive is `ginel-template-massage-regexp',
;; a trailing string preceded by the `&' sign by default. The massage
;; directive instructs `ginel-template' to apply the function given by
;; the first submatch to the instruction value before inserting.
;;
;; (add-to-list 'ginel-template-instructions-alist '("nickname" . "racke"))
;; (ginel-template-from-string "My nickname is @nickname&capitalize@ !")
;;     -| My nickname is Racke !
;;
;; ** Instruction values
;; `ginel-template' tries to find the value in the following order:
;;
;; 1) searches the cache for instruction values for this template
;;    (results of user queries are stored there)
;; 2) searches the association list `ginel-template-instructions-alist'
;;    for a value (see below)
;; 3) queries the user if the instruction comes with a query directive
;;
;; *** Predefined values
;; `ginel-template' generates instruction values as specified by
;; `ginel-template-instructions-alist' dynamically:
;;
;; o lists are evaluated as lisp expressions for the value
;; o strings are used literally as value
;; o symbols provide their symbol value
;;
;; `ginel-template-instructions-alist' defines values for the following
;; instructions:
;;
;; address
;;     the value of `user-mail-address'
;; basename
;;     buffer's file name without its directory and last suffix,
;;     e.g. "ginel-template" for this file
;; changelogcap
;;     a change log entry header, as generated by `add-change-log-entry'
;; copyright
;;     a copyright notice, like `Copyright (C) 1998 Stefan Hornburg'
;; file
;;     buffer's file name without its directory, e.g. "ginel-template.el"
;;     for this file
;; fullname
;;     full name of the user logged in
;; loginname
;;     user's name
;; snapshot
;;     version number for snapshots, e.g. "19990112"
;; system
;;     name of machine, e.g. "gundel.han.de"
;; suffix
;;     extension of buffer's file name, e.g. "el" for this file
;; year
;;     the current year, e.g. "1998"
;; 
;; (ginel-template-from-string ";; Copyright (C) @year@ @fullname@")
;;     -| ;; Copyright (C) 1998 Stefan Hornburg
;;
;; * Selected User Options
;; ginel-template-directory
;;  Default directory used by `ginel-template-from-file' when invoked
;;  interactively.
;;
;; * Other Useful Variables
;; ginel-template-inserted-region
;;  `ginel-template-from-...' and `ginel-template-insert-value'
;;   record (begin and end) of the inserted region in this variable.
;;   This allows for further processing of the inserted region.
;;
;;; Change log:
;; Fri Jan 15 11:59:16 1999  Stefan Hornburg  <racke@linuxia.de>
;;  * new variable `ginel-template-inserted-region'
;; Tue Jan 12 17:34:29 1999  Stefan Hornburg  <racke@linuxia.de>
;;  * new instruction `snapshot'
;; Sat Jan  2 05:18:03 1999  Stefan Hornburg  <racke@linuxia.de>
;;  * new instructions `basename', `file', `loginname', `system'
;; Fri Dec 18 14:03:42 1998  Stefan Hornburg  <racke@linuxia.de>
;;  * new instruction `copyright'
;; Thu Dec 10 09:38:59 1998  Stefan Hornburg  <racke@linuxia.de>
;;  * The `ginel-template-from-...' functions return an empty string
;;    now to allow the usage of these functions as instruction
;;    values.
;; Tue Dec  1 09:17:17 1998  Stefan Hornburg  <racke@linuxia.de>
;;  * ginel-template.el v0.4.0 released. 
;; Thu Nov 12 14:21:00 1998  Stefan Hornburg  <racke@linuxia.de>
;;  * new command `ginel-template-insert-value'
;;  * new internal function `ginel-template-instruction-value'
;;  * dependency on `cl' dropped.
;; Wed Nov  4 03:02:37 1998  Stefan Hornburg  <racke@linuxia.de>
;;  * ginel-template.el v0.3.0 released.
;;  * customization support added.
;;  * fixed documentation strings for user options.
;;  * unused code removed.
;; Mon Nov  2 22:57:44 1998  Stefan Hornburg  <racke@linuxia.de>
;;  * ginel-template.el v0.2.0 released.
;;  * fixed instruction value for "changelogcap".
;;  * setup instructions added.
;;  * new function `ginel-template-bindings'.
;;  * new variable `ginel-template-vars-alist' for byte-compiler's sake.
;; Sun Nov  1 20:19:52 1998  Stefan Hornburg  <racke@linuxia.de>
;;  * ginel-template.el v0.1.0 released.
;;  * new function `ginel-template-add-instruction'.
;;  * relax the requirement for `cl' to a compile-time one
;;    as suggested by Richard Stallman <rms@gnu.org>.
;; Tue Oct 27 13:10:26 1998  Stefan Hornburg  <racke@linuxia.de>
;;  * ginel-template.el v0.0.1 released.
;;  * `changelogcap' added to predefined instruction values.
;; Mon Oct 26 23:04:03 1998 Stefan Hornburg <racke@linuxia.de>
;;  * `Invalid search bound' error fixed for templates concluded with
;;    an instruction.
;;  * Strings are accepted as values of `ginel-template-instruction-alist'.
;;  * Documentation added.
;; Wed Oct 21 11:14:22 1998 Stefan Hornburg <racke@linuxia.de>
;;  * ginel-template.el v0.0.0 released.

;;; Code: 

(require 'custom)

(defgroup template nil
  "Template processing"
  :tag "Templates"
  :group 'editing)
  
;;;###autoload
(defcustom ginel-template-directory "~/templates/"
  "*Default directory for templates."
  :group 'template
  :type 'directory)

;;;###autoload
(defcustom ginel-template-wrapper-regexp "@\\\([^\n@]*\\\)@"
  "*Regular expression matching a template instruction."
  :group 'template
  :type 'regexp)

;;;###autoload
(defcustom ginel-template-query-regexp "^>"
  "*Regular expresssion matching query directive.
Asks user for replacement, if instruction is undefined."
  :group 'template
  :type 'regexp)

;;;###autoload
(defcustom ginel-template-massage-regexp "&\\(.*\\)\\'"
  "*Regular expression matching massage directive.
Applies function found in first submatch to replacement before inserting."
  :group 'template
  :type 'regexp)

;;;###autoload
(defcustom ginel-template-instructions-alist
  '(("address" . user-mail-address)
	("basename" . (file-name-sans-extension (buffer-file-basename)))
	("changelogcap" .
	 (concat (current-time-string) "  " user-full-name "  <"
								   user-mail-address ">\n"))
	("copyright" . (ginel-template-from-string "Copyright (C) @year@ @fullname@"))
	("file" . buffer-file-basename)
	("fullname" . user-full-name)
	("loginname" . user-login-name)
	("snapshot" . (format-time-string "%Y%m%d"))
	("suffix" . (save-match-data (if (string-match "\\.\\([^.]*\\)$" (buffer-file-basename)) (match-string 1 (buffer-file-basename)) "")))
	("system" . system-name)
	("year" . (format-time-string "%Y" (current-time))))
  "*Association list with predefined template instructions.
The values can be of several types:

 - A function symbol. The return value of the function call is inserted.
 - A symbol. The symbol value is inserted.
 - A string. The string is inserted.
 - A list. Evaluated as lisp expression. The result is inserted"
  :group 'template
  :type '(repeat (cons :format "%v"
					   (string :tag "Instruction name")
					   (choice :tag "Instruction value" symbol string sexp))))

(defvar ginel-template-history nil
  "A history list for templates.")

(defvar ginel-template-inserted-region nil
  "Used to record the inserted region.")

(defvar ginel-template-vars-alist nil
  "Temporary association list with template instructions and their value.")

(defun ginel-template-horse ()
  "Replaces buffer part if instruction is defined."
  (let ((instruction (buffer-substring-no-properties
					  (match-beginning 1) (match-end 1)))
		(begin (match-beginning 0)) (end (match-end 0))
		(massage "identity") replacement query)
	;; Check for query directive
	(save-match-data
	  (if (string-match ginel-template-query-regexp instruction)
		  (progn
			(setq query t)
			(setq instruction (replace-match "" t t instruction)))))
	;; Check for massage directive
	(save-match-data
	  (if (string-match ginel-template-massage-regexp instruction)
		  (progn
			(setq massage (match-string 1 instruction))
			(setq instruction (replace-match "" t t instruction)))))

	;; Determine value for instruction
	(setq replacement (ginel-template-instruction-value instruction))
	(if (or replacement
			(and query (setq replacement
							 (read-from-minibuffer
							  (format "Value for %s: " instruction)))))
		(progn
		  ;; add variable to list
		  (setq ginel-template-vars-alist
				(cons (cons instruction replacement)
					  ginel-template-vars-alist))
		  ;; replace buffer part
		  (delete-region begin end)
		  (insert (funcall (intern-soft massage) replacement))))))

;;;###autoload
(defun ginel-template-from-file (file)
  "Replaces variable parts of a template stored in FILE and inserts result."
  (interactive (list (read-file-name "Template file: "
									 ginel-template-directory)))
  (let ((begin (point)) end endmarker result range instruction
		ginel-template-vars-alist)
	;; insert template
	(setq result (insert-file-contents file))
	(setq end (+ begin (car (cdr result))))
	(setq endmarker (copy-marker end))
	;; search for potential replacements
	(goto-char begin)
	(while (and (<= (point) endmarker)
				(re-search-forward ginel-template-wrapper-regexp endmarker t))
	  (ginel-template-horse))
	(goto-char endmarker)
	;; record resulting region for further processing
	(setq ginel-template-inserted-region (list begin endmarker)))
  ;; return empty string to use this function as instruction value
  ""
  )

;;;###autoload
(defun ginel-template-from-string (string)
  "Replaces variable parts of a template STRING and inserts result."
   (interactive (list (read-string "Template: " nil 'ginel-template-history)))
   (let ((begin (point)) end endmarker result range instruction
		 ginel-template-vars-alist)
	;; insert template
	(insert string)
	(setq end (point))
	(setq endmarker (copy-marker end))
	;; search for potential replacements
	(goto-char begin)
	(while (and (<= (point) endmarker)
				(re-search-forward ginel-template-wrapper-regexp endmarker t))
	  (ginel-template-horse))
	(goto-char endmarker)
	;; record resulting region for further processing
	(setq ginel-template-inserted-region (list begin endmarker)))
   ;; return empty string to use this function as instruction value
   ""
   )

;;;###autoload
(defun ginel-template-add-instruction (name value)
  "Adds instruction to `ginel-template-instructions-alist'."
  (interactive "sInstruction name: \nxValue as Lisp expression: ")
  (setq ginel-template-instructions-alist
		(cons (cons name value) ginel-template-instructions-alist)))

;;;###autoload
(defun ginel-template-insert-value (name)
  "Inserts value generated by instruction NAME at point."
  (interactive
   (list (completing-read "Instruction name: "
						  ginel-template-instructions-alist nil t)))
  (if (length name) ; ignore empty results of completion
	  (let (ginel-template-vars-alist ; ignore uncleansed cache
			(begin (point)))
		(insert (ginel-template-instruction-value name))
		(setq ginel-template-inserted-region (list begin (point))))))

;;;###autoload
(defun ginel-template-bindings ()
  "Installs global key bindings for the user functions in this module."
  (interactive)
  (define-key ctl-x-map "ti" 'ginel-template-add-instruction)
  (define-key ctl-x-map "tf" 'ginel-template-from-file)
  (define-key ctl-x-map "ts" 'ginel-template-from-string)
  (define-key ctl-x-map "tv" 'ginel-template-insert-value)
  )

;;; internal functions
(defun ginel-template-instruction-value (name)
  "Returns value for instruction NAME or nil if instruction not exists."
	(let ((definition (cdr-safe (assoc name
									  ginel-template-instructions-alist))))
	  (cond ((functionp definition)
			 (prin1-to-string (funcall definition) t))
			((not definition)
			 (cdr-safe (assoc name ginel-template-vars-alist)))
			((symbolp definition)
			 (prin1-to-string (symbol-value definition) t))
			((stringp definition)
			 definition)
			((listp definition)
			 (prin1-to-string (eval definition) t)))))	  
  
(provide 'ginel-template)

;;; ginel-template.el ends here

;;; Local Variables:
;;; generated-autoload-file: "ginel.el"
;;; End:
