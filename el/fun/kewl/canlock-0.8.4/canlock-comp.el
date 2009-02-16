;;; canlock-comp.el --- installation tools for Canlock
;; Copyright (C) 2001 Katsumi Yamaoka

;; Author: Katsumi Yamaoka <yamaoka@jpl.org>
;; Keywords: cancel-lock

;; This file is part of Canlock.

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

;; This file is used exclusively for building and installing Canlock.
;; There are no serviceable parts for the run-time.

;;; Code:

;; Add paths to `load-path'.
(let ((addpath (getenv "ADDITIONAL_LOAD_PATH"))
      (start 0)
      path paths)
  (if addpath
      (progn
	(while (string-match "[\000-\037:]+" addpath start)
	  (setq path (substring addpath start (match-beginning 0))
		start (match-end 0))
	  (if (and (> (length path) 0)
		   (file-directory-p (setq path (expand-file-name path))))
	      (setq paths (cons (file-name-as-directory path) paths))))
	(if (and (> (length (setq path (substring addpath start))) 0)
		 (file-directory-p (setq path (expand-file-name path))))
	    (setq paths (cons (file-name-as-directory path) paths)))
	(setq load-path (append (nreverse paths) load-path)))))

;; Position the current directory on the forefront of `load-path'.
(let ((cur (file-truename default-directory))
      lpath)
  (while load-path
    (if (string-equal (file-name-as-directory (file-truename (car load-path)))
		      cur)
	nil
      (setq lpath (cons (car load-path) lpath)))
    (setq load-path (cdr load-path)))
  (setq load-path (cons default-directory (nreverse lpath))))

(defvar canlock-load-list nil)

(defun canlock-load-el (file)
  (if file
      (if (member file canlock-load-list)
	  nil
	(load file nil nil t)
	(setq canlock-load-list (cons file canlock-load-list)))))

(defun canlock-compile-1 (el silent &optional requirement)
  (let ((elc (concat el "c")))
    (if (and (file-exists-p elc)
	     (file-newer-than-file-p elc el))
	(or silent
	    (message "%s is up to date" elc))
      (canlock-load-el requirement)
      (byte-compile-file el))))

(defun canlock-compile (&optional silent)
  (if (not noninteractive)
      (error "`canlock-compile' is to be used only with -batch"))
  (require 'bytecomp)
  (let ((byte-compile-warnings
	 (if (featurep 'xemacs)
	     (delq 'unused-vars (copy-sequence byte-compile-default-warnings))
	   t)))
    (canlock-compile-1 "hex-util.el" silent)
    (canlock-compile-1 "sha1-el.el" silent)
    (if (boundp 'MULE)
	(progn
	  (if (and (file-exists-p "canlock-om.elc")
		   (file-newer-than-file-p "canlock-om.elc" "canlock-om.el"))
	      (progn
		(or silent
		    (message "canlock-om.elc is up to date"))
		(canlock-compile-1 "canlock.el" silent "canlock-om.el")
		(canlock-compile-1 "canlock-cmds.el" silent "canlock-om.el"))
	    (condition-case nil
		(canlock-load-el "canlock-om.el")
	      (error (error "\
Error: canlock.elc may have been compiled for another version of Emacs;\r
       try `make clean'")))
	    (byte-compile-file "canlock-om.el")
	    (byte-compile-file "canlock-cmds.el")
	    (byte-compile-file "canlock.el")))
      (canlock-compile-1 "canlock-cmds.el" silent)
      (if (and (fboundp 'base64-encode-string)
	       (subrp (symbol-function 'base64-encode-string)))
	  nil
	(message "\r
The function `base64-encode-string' is not built-in;\r
You should use base64.elc or other programs at the run-time.\r
")
	(autoload 'base64-encode-string "base64"))
      (canlock-compile-1 "canlock.el" silent))))

(if (boundp 'MULE)
    (defun canlock-locate-library (library &optional nosuffix)
      ;; Shut up.
      (catch 'answer
	(mapcar
	 '(lambda (dir)
	    (mapcar
	     '(lambda (suf)
		(let ((try (expand-file-name (concat library suf) dir)))
		  (and (file-readable-p try)
		       (null (file-directory-p try))
		       (throw 'answer try))))
	     (if nosuffix '("") '(".elc" ".el" ""))))
	 load-path)
	nil))
  (defalias 'canlock-locate-library 'locate-library))

(defun canlock-install (&optional all)
  (if (not noninteractive)
      (error "`canlock-install' is to be used only with -batch"))
  (let ((lispdir (getenv "LISPDIR"))
	(files '("hex-util" "sha1-el" "canlock" "canlock-cmds"))
	file)
    (or lispdir
	(error "`LISPDIR' is not specified"))
    (or (file-directory-p lispdir)
	(error "no such a directory: %s" lispdir))
    (or (file-writable-p lispdir)
	(error "permission denied: %s" lispdir))
    (setq lispdir (file-name-as-directory lispdir))
    (canlock-compile 'silent)
    (if (boundp 'MULE)
	(setq files (nconc files '("canlock-om"))))
    (if all
	nil
      (let ((load-path (cdr load-path))
	    (destination (file-truename lispdir))
	    rest lib)
	;; Exclude the current directory and the destination directory
	;; for searching the modules.
	(while load-path
	  (if (string-equal
	       (file-name-as-directory (file-truename (car load-path)))
	       destination)
	      nil
	    (setq rest (cons (car load-path) rest)))
	  (setq load-path (cdr load-path)))
	(setq load-path (nreverse rest)
	      rest nil)
	(while files
	  (setq file (car files)
		files (cdr files))
	  (if (setq lib (canlock-locate-library file))
	      (message "%s.elc is found in %s" file (file-name-directory lib))
	    (setq rest (cons file rest))))
	(setq files (nreverse rest))))
    (while files
      (setq file (concat (car files) ".el")
	    files (cdr files))
      (message "Copying %s -> %s" file lispdir)
      (copy-file file (expand-file-name file lispdir) t t)
      (setq file (concat file "c"))
      (message "Copying %s -> %s" file lispdir)
      (copy-file file (expand-file-name file lispdir) t t))))

(defun canlock-install-anyway ()
  (canlock-install 'all))

;;; canlock-comp.el ends here
