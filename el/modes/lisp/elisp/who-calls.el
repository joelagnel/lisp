;;; who-calls.el --- Display all known callers of a function

;; Copyright (C) 2004 Jesper Harder

;; Author: Jesper Harder <harder@ifa.au.dk>
;; Created: 20 Jun 2004
;; Version: 1.01
;; Location: <http://purl.org/harder/>
;; Keywords: lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;;; Commentary:

;; `who-calls' displays all known callers of an Emacs Lisp function.
;; To bind it to key, say `C-c C-w', add something like this to your
;; .emacs:
;;
;;   (autoload 'who-calls "who-calls" nil t)
;;   (define-key emacs-lisp-mode-map "\C-c\C-w" 'who-calls)
;;
;; To actually generate and save the call information, you need to
;; load this file when compiling the project, e.g. "-l who-calls.el",
;; probably by modifying the Makefile -- for Gnus it would be this
;; line:
;;
;; FLAGS = -batch -q -no-site-file -l who-calls.el -l $(srcdir)/dgnushack.el

;;; Code:

(require 'bytecomp)

(defvar who-calls-table nil
  "Hash table for caller information.")

(when noninteractive
  (defalias 'display-call-tree 'ignore)
  (setq byte-compile-generate-call-tree t)
  (defadvice kill-emacs (before save-call-tree activate)
    (who-calls-save-call-tree ".who-calls")))

(defun who-calls-save-call-tree (file)
  "Save call tree in FILE."
  (with-temp-file file
    (print (mapcar (lambda (x) (cons (car x) (cadr x)))
		   byte-compile-call-tree)
	   (current-buffer))))

(defun who-calls-load-tree (file)
  "Load the call tree from FILE."
  (unless (file-exists-p file)
    (setq file (read-file-name "Load caller information from file: " nil nil t)))
  (with-temp-buffer
    (insert-file-contents file)
    (setq who-calls-table (make-hash-table :test 'eq))
    (dolist (f (read (current-buffer)))
      (puthash (car f) (sort (cdr f) 'string<) who-calls-table))))

(defun who-calls (function)
  "Show all known callers of the function FUNCTION."
  (interactive
   (let ((fn (function-called-at-point))
	 (enable-recursive-minibuffers t)
	 val)
     (setq val (completing-read (if fn
				    (format "Who calls (default %s): " fn)
				  "Who calls: ")
				obarray 'fboundp t nil nil (symbol-name fn)))
     (list (if (equal val "")
	       fn (intern val)))))
  (unless (hash-table-p who-calls-table)
    (who-calls-load-tree ".who-calls"))
  (help-setup-xref (list #'who-calls function) (interactive-p))
  (save-excursion
    (with-output-to-temp-buffer (help-buffer)
      (princ (format "%s is called by:\n\n" function))
      (dolist (caller (gethash function who-calls-table))
	(princ (format "`%s'\n" caller))))))

(provide 'who-calls)

;;; who-calls.el ends here
