;;; canlock-om.el --- Mule 2 specific functions for canlock
;; Copyright (C) 2001 Katsumi Yamaoka

;; Author: Katsumi Yamaoka <yamaoka@jpl.org>
;; Keywords: mule, cancel-lock

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

;; This program is used to make canlock.el work with Mule 2.3 based on
;; Emacs 19.34.  See README.ja in the canlock distribution for details.

;;; Code:

(eval-and-compile
  (cond ((and (boundp 'emacs-major-version)
	      (> emacs-major-version 19))
	 (error "\
Error: You should never use canlock-om.el(c) for this environment"))
	((and (boundp 'MULE)
	      (boundp 'emacs-major-version)
	      (= emacs-major-version 19)
	      (>= emacs-minor-version 29)))
	(t
	 (error "Error: Canlock does not support this version of Emacs"))))

(eval-when-compile
  (require 'cl))

(require 'custom)
(eval-and-compile
  (unless (fboundp 'custom-declare-variable)
    (error "Error: Canlock requires new custom")))

(eval-when-compile
  (unless (fboundp 'byte-compile-file-form-custom-declare-variable)
    (defun byte-compile-file-form-custom-declare-variable (form)
      ;; Bind defcustom'ed variables.
      (if (memq 'free-vars byte-compile-warnings)
	  (setq byte-compile-bound-variables
		(cons (nth 1 (nth 1 form)) byte-compile-bound-variables)))
      (if (memq ':version (nthcdr 4 form))
	  ;; Make the variable uncustomizable.
	  `(defvar ,(nth 1 (nth 1 form)) ,(nth 1 (nth 2 form))
	     ,(substring (nth 3 form)
			 (if (string-match "^[\t *]+" (nth 3 form))
			     (match-end 0)
			   0)))
	;; Ignore unsupported keyword(s).
	(if (memq ':set-after (nthcdr 4 form))
	    (let ((newform (list (car form) (nth 1 form)
				 (nth 2 form) (nth 3 form)))
		  (args (nthcdr 4 form)))
	      (while args
		(or (eq (car args) ':set-after)
		    (setq newform (nconc newform (list (car args)
						       (car (cdr args))))))
		(setq args (cdr (cdr args))))
	      newform)
	  form)))
    (put 'custom-declare-variable 'byte-hunk-handler
	 'byte-compile-file-form-custom-declare-variable))

  (define-compiler-macro with-temp-buffer (&whole form &rest forms)
    (let ((def (if (fboundp 'with-temp-buffer)
		   (symbol-function 'with-temp-buffer))))
      (if (and def
	       (consp def)
	       (or (eq (car def) 'macro)
		   (and (eq (car def) 'autoload)
			(memq (nth 4 def) '(macro t)))))
	  form
	;; The function definition is imported from APEL.
	`(let ((obuffer (current-buffer))
	       (buffer (generate-new-buffer " *temp*")))
	   (unwind-protect
	       (progn
		 (set-buffer buffer)
		 ,@forms)
	     (if (buffer-name buffer)
		 (kill-buffer buffer))
	     (if (buffer-live-p obuffer)
		 (set-buffer obuffer))))))))

(autoload 'base64-encode "base64")

(defcustom canlock-base64-encode-function 'base64-encode-string
  "Function to call to base64 encode a string."
  :type '(radio (function-item base64-encode-string)
		(function-item base64-encode)
		(function-item canlock-base64-encode-string-with-mmencode)
		(function :tag "Other"))
  :group 'canlock)

(defcustom canlock-mmencode-program "mmencode"
  "Name of mmencode program."
  :type 'string
  :group 'canlock)

(defcustom canlock-mmencode-args-for-encoding nil
  "Arguments passed to mmencode program for encoding."
  :type 'sexp
  :group 'canlock)

(defun canlock-base64-encode-string-with-mmencode (string)
  "Base64 encode a string using mmencode."
  (with-temp-buffer
    (setq mc-flag nil)
    (insert string)
    (let ((default-process-coding-system (cons *iso-2022-jp*dos *noconv*))
	  program-coding-system-alist selective-display)
      (apply 'call-process-region (point-min) (point-max)
	     canlock-mmencode-program t t nil
	     canlock-mmencode-args-for-encoding))
    (goto-char (point-max))
    (skip-chars-backward "\n")
    (buffer-substring (point-min) (point))))

;; The following macros will only be used to byte-compile canlock.el.
(eval-when-compile
  (define-compiler-macro base64-encode-string
    (&whole form string &optional no-line-break)
    (if (and (string-equal (buffer-name) " *Compiler Input*")
	     (string-equal ";;; canlock.el"
			   (buffer-substring (point-min)
					     (min (+ (point-min) 14)
						  (point-max)))))
	(if no-line-break
	    `(let ((string ,string))
	       (if ,no-line-break
		   (with-temp-buffer
		     (insert (funcall canlock-base64-encode-function string))
		     (goto-char (point-min))
		     (while (search-forward "\n" nil t)
		       (delete-char -1))
		     (buffer-string))
		 (funcall canlock-base64-encode-function string)))
	  `(funcall canlock-base64-encode-function ,string))
      form))

  (define-compiler-macro split-string (&whole form string &optional pattern)
    (if (and (string-equal (buffer-name) " *Compiler Input*")
	     (string-equal ";;; canlock.el"
			   (buffer-substring (point-min)
					     (min (+ (point-min) 14)
						  (point-max)))))
	;; The function definition is imported from APEL.
	(if pattern
	    `(let ((string ,string)
		   (pattern ,pattern)
		   (start 0)
		   parts)
	       (while (string-match pattern string start)
		 (setq parts (cons (substring string
					      start (match-beginning 0))
				   parts)
		       start (match-end 0)))
	       (nreverse (cons (substring string start) parts)))
	  `(let ((string ,string)
		 (start 0)
		 parts)
	     (while (string-match "[ \f\t\n\r\v]+" string start)
	       (setq parts (cons (substring string
					    start (match-beginning 0))
				 parts)
		     start (match-end 0)))
	     (nreverse (cons (substring string start) parts))))
      form)))

;; The following variables might not be bound if the old version of
;; canlock.el(c) exists.
(eval-when-compile
  (defvar canlock-openssl-args)
  (defvar canlock-openssl-program))

(defun canlock-om-sha1-with-openssl (message)
  "Make a SHA-1 digest of MESSAGE using OpenSSL."
  (with-temp-buffer
    (setq mc-flag nil)
    (insert message)
    (let ((default-process-coding-system (cons *iso-2022-jp*dos *noconv*))
	  program-coding-system-alist selective-display)
      (apply 'call-process-region (point-min) (point-max)
	     canlock-openssl-program t t nil canlock-openssl-args))
    (goto-char (point-min))
    (insert "\"")
    (while (re-search-forward "\\([0-9A-Fa-f][0-9A-Fa-f]\\)" nil t)
      (replace-match "\\\\x\\1"))
    (insert "\"")
    (goto-char (point-min))
    (read (current-buffer))))

;; Override the original function.
(eval-after-load "canlock"
  '(defalias 'canlock-sha1-with-openssl 'canlock-om-sha1-with-openssl))

(provide 'canlock-om)

(require 'canlock)

;;; canlock-om.el ends here
