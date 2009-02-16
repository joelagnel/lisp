;;; Debugging info for self: Saved through ges-version 1.5dev
;;; ;;; From: Thomas Link <esendepe@yahoo.de>
;;; ;;; Subject: properties.el 1.1
;;; ;;; Newsgroups: gnu.emacs.sources
;;; ;;; Date: Thu, 21 Nov 2002 10:09:50 +0100
;;; ;;; Organization: Vienna University, Austria

;;; Store file local variables without changing the file's contents. Add the
;;; variable's symbol to `properties-list', and the variable will be
;;; automatically saved and restored.

;;; Changes:
;;; 1.1 :: Save and restore some overlays; better control over where to save
;;; properties files (see `properties-location')

;;; Cheers,
;;; Thomas.
;;; properties.el --- non-intrusive file local variables

;; Copyright (C) 2002 Thomas Link

;; Author: Thomas Link AKA samul AT web DOT de
;; Time-stamp: <2002-11-21>
;; Keywords:

(defconst properties-version "1.1")
(defconst properties-homepage
  "http://members.a1.net/t.link/CompEmacsProperties.html")
 
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

;; ;---(:excerpt-beg properties :name desc)---
;; ;---(:excerpt-source "~/Etc/TL-Wiki/CompEmacsProperties")---
;; 
;; Store file local variables without changing the file's contents. Add the
;; variable's symbol to `properties-list', and the variable will be
;; automatically saved and restored.
;; 
;; In addition, overlays can be stored too -- see `properties-overlays'.
;; 
;; By default, the properties of file =/there/here/this.txt= will be stored
;; in =/there/here/.props/this.txt= -- see `properties-subdir'.
;; 
;; Installation: Put (require 'properties) (properties-install) into your
;; startup/user init file.
;; 
;; 
;; *** Commands
;; 
;; `properties-add' :: Add a variable to the file's property list.
;; 
;; `properties-remove' :: Remove a variable from the file's property list.
;; 
;; `properties-write' :: Write the property file.
;; 
;; `properties-install', `properties-deinstall' :: Add and remove
;; properties.el specific hooks.
;; 
;; 
;; *** Variables
;; 
;; `properties-list' :: A list of properties that should be stored in the
;; property file.
;; 
;; `properties-overlays' :: A list of symbols or plists identifying overlays
;; that should be stored.
;; 
;; `properties-overlay-ignored-properties' :: Ignore these properties when
;; dumping overlays.
;; 
;; `properties-file' :: A buffer local variable defining the properties
;; file -- relative to the current buffer's/file's location. Use this to
;; force the use of a non-standard properties file.
;; 
;; ;---(:excerpt-end properties :name desc)---

;;; Change log:

;; ;---(:excerpt-beg properties :name versionhist)---
;; ;---(:excerpt-source "~/Etc/TL-Wiki/CompEmacsProperties")---
;; 
;; 1.1 :: Save and restore some overlays; better control over where to save
;; properties files (see `properties-location')
;; 
;; 1.0 :: Initial release
;; 
;; ;---(:excerpt-end properties :name versionhist)---

;;; To do:

;; - save modes & file local hooks?

;;; Code:

(require 'tellib)
(tellib-version-check-with-name "tellib" "0.1.9")


(defgroup properties nil
  "Make backups with a time stamp"
  :prefix "properties-"
  :group 'tools)

(defcustom properties-subdir ".props"
  "*Subdirectory where to put property files
\(relative to the current file's directory)."
  :type 'string
  :group 'properties)

(defcustom properties-location
  '(
    ;;("^.+://" properties-get-urls-properties-file)
    ((lambda (file) (not (file-writable-p (file-name-directory file))))
     properties-get-nonwriteable-properties-file)
    (:default "%{CD}%/.props%/"))
  "*An alist of regexps and locations of property files.
An alist or form: \((MATCH DIR) ...)
MATCH ... either :default or a regexp or a predicate testing the filename
DIR   ... either a directory or a function returning the properties filename

Replacement strings:
%{CD} ... the file's current directory
%/ ... the default path separator
%% ... %
"
  :type '(repeat
	  (list :value ("" "")
		(choice (regexp :tag "RegExp" :value "")
			(function :tag "Predicate" :value nil)
			(const :tag "Default" :value :default))
		(choice (string :tag "Directory" :value "")
			(function :tag "Function" :value nil))))
  :group 'properties)

(defcustom properties-warn-if-not-readable t
  "*Non-nil means, warn user if the properties file is non readable
and if `properties-flag' is non-nil."
  :type 'boolean
  :group 'properties)

(defcustom properties-overlay-ignored-properties
  '(begin-glyph end-glyph local-map keymap)
  "*Ignore these properties when dumping overlays."
  :type '(repeat
	  (symbol :tag "Symbol" :value nil))
  :group 'properties)

(defvar properties-list nil
  "A list of properties that should be stored in the property file.
There are two types of list elements:

SYMBOL ... add '(SYMBOL \(eval SYMBOL)) to the property file.

\(SYMBOL PREDICATE) ... add '(SYMBOL \(eval SYMBOL)) only if \(PREDICATE
\(eval SYMBOL) is non-nil.")
(make-variable-buffer-local 'properties-list)

(defvar properties-write-out-hook nil
  "A list of predicates.
If one yields non-nil, the properties file will be written.")
(make-variable-buffer-local 'properties-write-out-hook)

(defvar properties-overlays nil
  "A list of symbols or plists identifying overlay that should be stored.
If it is a plist, the following fields are known:
:name ... Equal to symbol
:ignore ... A list of ignored properties
")
(make-variable-buffer-local 'properties-overlays)

(defvar properties-tmp-buffer nil)
(make-variable-buffer-local 'properties-tmp-buffer)

(defvar properties-flag nil
  "Non-nil means, warn user if the file's properties file can't be loaded.")
(make-variable-buffer-local 'properties-flag)

(defvar properties-file nil
  "A buffer local variable defining the properties file
-- relative to the current buffer's/file's location.")
(make-variable-buffer-local 'properties-file)


(defun properties-get-urls-properties-file (file)
  "Get the properties filename based on an url
protocol://there/here/etc"
  (if (string-match "^\\(.+\\)://\\(.+\\)$" file)
      (concat (file-name-as-directory user-init-directory)
	      (file-name-as-directory properties-subdir)
	      (file-name-as-directory (match-string 1 file))
	      (tellib-replace-args (match-string 2 file)
				   `(("" ,(string directory-sep-char)))
				   ?/))
    (tellib-error 'error "properties: Not an URL" file)))
;;test: (properties-get-urls-properties-file "http://localhost/wherever")

(defun properties-get-nonwriteable-properties-file (file)
  "Get the properties filename for an non-writeable file"
  (tellib-remove-redundant-dir-separators
   (concat (file-name-as-directory user-init-directory)
	   (file-name-as-directory properties-subdir)
	   (if (and (not (char= ?: directory-sep-char))
		    (string-match "^\\(.+\\):\\(.+\\)$" file))
	       (concat (match-string 1 file)
		       (match-string 2 file))
	     file))))
;;test: (properties-get-nonwriteable-properties-file "c:\\there\\here")
;;test: (properties-get-nonwriteable-properties-file "/there/here")

(defun properties-get-file-name (file &optional makedir-flag)
  "Return the name of the property file.

If MAKEDIR-FLAG is non-nil, this function is used if query mode and the
parent directory will be created if necessary.
"
  (if properties-file
      (concat (file-name-directory file) properties-file)
    (let* ((dd (file-name-directory file))
	   (ps (string directory-sep-char))
	   (rv (concat dd (file-name-as-directory properties-subdir)))
	   (file (tellib-remove-redundant-dir-separators
		  (catch 'exit
		    (dolist (this properties-location rv)
		      (let ((rx (nth 0 this))
			    (rs (nth 1 this)))
			(when (if (functionp rx)
				  (funcall rx file)
				(or (equal :default rx)
				    (string-match rx file)))
			  (throw 'exit
				 (cond
				  ((functionp rs)
				   (funcall rs file))
				  ((stringp rs)
				   (concat (file-name-as-directory
					    (tellib-replace-args rs
								 `(("{CD}" ,dd)
								   ("/"    ,ps))))
					   (file-name-nondirectory file)))
				  (t
				   (tellib-error 'error "properties: Bad type" rs))
				  ))))))))
	   (dir (file-name-directory file)))
      (when (and makedir-flag
		 (not (file-readable-p dir)))
	(message "DEBUG: %s %s" dir file)
	(make-directory dir t))
      file)))
;;test: (properties-get-file-name (buffer-file-name))
;;test: (properties-get-file-name "/etc/hosts")
;;test: (properties-get-file-name "http://localhost/wherever")
;;test: (let ((properties-file "x")) (properties-get-file-name (buffer-file-name)))

(defun properties-get-properties-list ()
  "Return a list of \(VARIABLE VALUE) list for the current buffer
as defined by `properties-list'."
  (tellib-mapcart (lambda (this)
		    (cond
		     ((listp this)
		      (let* ((sym (car this))
			     (val (eval sym))
			     (fn  (cadr this)))
			(when (funcall fn val)
			  (list sym (eval val)))))
		     (t
		      (list this (eval this)))))
		  properties-list))

(defun properties-get-overlay-list ()
  "Return a list of dumped overlays for the current buffer
as defined by `properties-overlays'."
  (tellib-overlay-collect (mapcar (lambda (x) 
				    (if (listp x)
					(plist-get x :name)
				      x))
				  properties-overlays)
			  nil
			  nil
			  (append properties-overlay-ignored-properties
				  (tellib-mappend (lambda (x)
						    (if (listp x)
							(plist-get x :ignore)
						      nil))
						  properties-overlays))))

(defun properties-save-properties-file-p (&optional properties overlays)
  "Return non-nil, if the properties file for current buffer should be saved."
  (not (not (or (or properties
		    (properties-get-properties-list))
		(or overlays
		    (properties-get-overlay-list))))))
;;test: (properties-save-properties-file-p)

;;;###autoload
(defun properties-write (&optional file)
  "Write the property file.
See the documentation of `properties-list'."
  (interactive)
  (when (and (not properties-tmp-buffer)
	     (or properties-list
		 properties-overlays))
    (when properties-overlays
      (add-to-list 'properties-list 'properties-overlays))
    (let ((file     (or file
			(buffer-file-name)
			(if (y-or-n-p "Save buffer to a file in order to use properties? ")
			    (save-buffer)
			  (message "properties: Can't write properties if the buffer has no `buffer-file-name'.")
			  nil))))
      (when file
	(let ((ols      (properties-get-overlay-list))
	      (proplst  (properties-get-properties-list))
	      (propfile (properties-get-file-name file t)))
	  ;;(message "DEBUG: %S" proplst)
	  (with-temp-buffer
	    (setq properties-tmp-buffer t)
	    (when (properties-save-properties-file-p proplst ols)
	      (let (;;(backup-inhibited t)
		    (auto-mode-alist nil)
		    (version-control 'never)
		    (comment-start ";")
		    (comment-end   ""))
		(insert (format "(:version 2\n :vars %S\n :overlays %S)" proplst ols))
		;;(tellib-update-local-variable-def 'backup-inhibited t)
		(tellib-update-local-variable-def 'version-control 'never)
		(write-file propfile)))))))))
;;test: (progn (setq al '(1 2 3)) (setq bl nil) (setq properties-list '(bl al)))
;;test: (progn 	(setq al '(1 2 3)) (setq bl nil)
;;		(setq properties-list '((bl identity) al)))
;;test: (properties-write)

(defun properties-read (&optional file)
  "Read the property file."
  (let* ((file     (or file
		       buffer-file-name))
	 (propfile (properties-get-file-name file)))
    (cond
     ((file-readable-p propfile)
      (let* ((lst     (car (read-from-string
			    (with-temp-buffer
			      (insert-file-contents-literally propfile)
			      (buffer-string)))))
	     (version (when (tellib-valid-plist-p lst)
			(plist-get lst :version 1)))
	     (vars    (case version
			((2)
			 (plist-get lst :vars))
			(t
			 lst)))
	     (ols     (case version
			((2)
			 (plist-get lst :overlays))
			(t
			 nil))))
	;;(message "DEBUG: %S" lst)(sleep-for 3)
	;;restore variables
	(dolist (this vars)
	  (let ((var (car this))
		(val (cadr this)))
	    (add-to-list 'properties-list var)
	    (make-local-variable var)
	    (set var val)))
	;;restore overlays
	(tellib-overlay-restore ols)
	))
     ((and properties-warn-if-not-readable
	   properties-flag)
      (let ((msg "properties: Properties file can't be loaded"))
	(unless (y-or-n-p (format "%s. Continue? " msg))
	  (tellib-error 'error msg file)))))))
;;test: (progn (setq al t) (setq bl t))
;;test: (progn (setq properties-list nil) (properties-read))

;;;###autoload
(defun properties-minor-mode (&optional check-if-needed)
  "Prepare the current buffer for use with properties.el."
  (let ((flag (if check-if-needed
		  (properties-save-properties-file-p)
		t)))
    (tellib-update-local-variable-def 'properties-flag flag
				      :if-different t
				      :set-var t
				      :no-error-if-read-only t)))

;;;###autoload
(defun properties-add (&optional variable predicate)
  "Add VARIABLE to the file's property list.
PREDICATE defaults to (not (null VARIABLE))."
  (interactive)
  (let ((var (or variable
		 (intern-soft (read-from-minibuffer "Variable: ")))))
    (if (boundp var)
	(progn
	  (if predicate
	      (add-to-list 'properties-list (list var predicate))
	    (add-to-list 'properties-list var))
	  (properties-minor-mode))
      (tellib-error 'error "Property: symbol not bound" var))))
;;test: (properties-add)
;;test: (let (x properties-list) (properties-add 'x #'null))

;;;###autoload
(defun properties-remove (&optional var)
  "Remove a variable from the file's property list."
  (interactive)
  (let ((var (or var
		 (intern-soft
		  (completing-read "Variable: "
				   (mapcar (lambda (x)
					     (list (symbol-name x)))
					   properties-list))))))
    (setq properties-list
	  (tellib-filter-list properties-list
			      #'(lambda (x) (not (equal x var)))))
    (properties-minor-mode t)))
;;test: (properties-remove)

;;;###autoload
(defun properties-overlay-add (&optional symbol)
  "Add SYMBOL to `properties-overlays'."
  (interactive)
  (let ((sym (or symbol
		 (intern (read-from-minibuffer "Variable: ")))))
    (when sym
      (add-to-list 'properties-overlays sym)
      (properties-minor-mode))))
;;test: (properties-overlay-add)
;;test: (let (x properties-overlays) (properties-overlay-add 'x))

;;;###autoload
(defun properties-overlay-remove (&optional symbol)
  "Remove SYMBOL from `properties-overlays'."
  (interactive)
  (let ((sym (or symbol
		 (intern (completing-read "Symbol: "
					  (mapcar (lambda (x)
						    (list (symbol-name x)))
						  properties-overlays))))))
    (tellib-del-from-list 'properties-overlays sym)
    (properties-minor-mode t)))
;;test: (properties-overlay-remove)

(defun properties-install ()
  "Install properties specific hooks."
  (interactive)
  (tellib-installer 'tellib 'properties)
  (add-hook 'find-file-hooks #'properties-read)
  ;;(add-hook 'write-file-hooks #'properties-write)
  (add-hook 'kill-buffer-hook #'properties-write)
  ;;(setq kill-buffer-hook nil)
  (when (boundp 'filesets-ingroup-collect-hook)
    (add-hook 'filesets-ingroup-collect-hook #'properties-read)))

(defun properties-uninstall ()
  "Deinstall properties specific hooks."
  (interactive)
  (tellib-uninstaller 'tellib 'properties)
  (remove-hook 'find-file-hooks #'properties-read)
  ;;(remove-hook 'write-file-hooks #'properties-write)
  (remove-hook 'kill-buffer-hook #'properties-write)
  ;;(setq kill-buffer-hook nil)
  (when (boundp 'filesets-ingroup-collect-hook)
    (remove-hook 'filesets-ingroup-collect-hook #'properties-read)))

;(properties-install)
;(properties-uninstall)

;;test: (setq properties-list nil)
;;test: (setq properties-overlays nil)
;;test: (setq al '(1 2 3))
;;test: (properties-add 'al)
;;test: (properties-remove 'al)
;;test: (properties-overlay-add 'al)
;;test: (properties-overlay-remove 'al)
;;test: (properties-save-properties-file-p)
;;test: (properties-get-properties-list)
;;test: (properties-get-overlay-list)

(provide 'properties)

;;; properties.el ends here

;;; ;;; Local Variables:
;;; ;;; properties-flag: t
;;; ;;; auto-recompile:1
;;; ;;; time-stamp-format:"%y-%02m-%02d"
;;; ;;; End:

