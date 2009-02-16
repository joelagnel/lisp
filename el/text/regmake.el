;;;-*- auto-recompile: t -*-
;; Time-stamp: <2002-05-23 14:59:56 deego>
;; GPL'ed under GNU'S public license..
;; Copyright (C) Deepak Goel 2000
;; Emacs Lisp Archive entry
;; Filename: regmake.el
;; Package: regmake
;; Author: Deepak Goel <deego@glue.umd.edu>
;; Version: 0.0alpha

;; Commentary: This makes regexps for you..  all you gotta do is write in
;; intuitive lispy commands..   this attempts to completely lisp-ize
;; regexp-generation for you..  THIS LIBRARY IS ANYTHING BUT
;; READY.. AND PERHAPS NEVER MAY BE READY..

;;  Try:
;; M-x regmake (or "file1.el" "dir/file2.el")
;; or
;; M-x regmake (concat (at-line-begin) "sss")
;; .. take it to arbitrary level of nesting you like..

;; Does not handle the "digit-th"..  help welcome..

(defvar regmake-version "0.0alpha")



(require 'cl)
(require 'thingatpt)


(defvar regmake-loudness .6)

;;;###autoload
(defmacro regmake-macro (form)
  "Is a macro.. handles unquoted lists.. "
  `(regmake-function (quote ,form))
;;  `(eval (regmake-parse-form 
;;	  (regmake-quote-user-strings
;;	   (quote ,form))))
)

;;;###autoload
(defun regmake (list)
  "Main provision..
Handles lists.
Interactively: no need to quote.. "
  (interactive "x(unquoted) List/string: ")
  (let ((result (eval
		 (regmake-parse-form 
		  (regmake-quote-user-strings
		   list)))))
    (if (or (interactive-p) (> regmake-loudness 0.4))
	(ignore-errors (message (format "%S" result))))
    result)
)


(defun regmake-quote-user-strings (form)
  (if (listp form)
      (mapcar 'regmake-quote-user-strings form)
    (if (stringp form)
	(regexp-quote form)
      form))
)

  

;;;###autoload
(defun regmake-parse-form (form)
  "Expects a list.. and returns a list that can be evaluated.."
  (if (not (listp form))
      (if (stringp form)
	  form
	(progn
	  (message "Warning: Form is neither list nor string")
	  form))
    (progn
      (cons
       (with-temp-buffer
	 (insert (format "%S" (first form)))
	 (goto-char (point-min))
	 (let ((found (search-forward "regmake" nil t)))
	   (unless found
	     (goto-char (point-min))
	     (insert "regmake-"))
	   (goto-char (point-min))
	   (sexp-at-point)))
       (mapcar 
	'regmake-parse-form 
	(cdr form)))))
  )



(defun regmake-or (sa sb)
  (concat "\\(" sa "\\|" sb "\\)")
)


(defun regmake-any (s)
  (concat "\\(" s "\\)" "*")
)

(defun regmake-some (s)
  (concat "\\(" s "\\)" "+")
)

(defun regmake-maybe (s)
  (concat "\\(" s "\\)" "?")
)


(defun regmake-char-word ()
  "\\w")

(defun regmake-char-no-word ()
  "\\W")


(defun regmake-at-buffer-begin ()
  "\\'")

(defun regmake-at-buffer-end ()
  "\\`")

(defun regmake-at-line-begin ()
  "^")

(defun regmake-at-line-end ()
  "$")

(defun regmake-at-word-boundary ()
  "\\b")

(defun regmake-at-not-word-boundary ()
  "\\B")

(defun regmake-at-word-begin ()
  "\\<")

(defun regmake-at-word-end ()
  "\\>")

(defun regmake-char-syntax (syntax)
  (concat "\\s" syntax))

(defun regmake-char-no-syntax (syntax)
  (concat "\\S" syntax))

(defun regmake-concat (&rest args)
  (apply 'concat args)
)
