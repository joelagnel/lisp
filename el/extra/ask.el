;; -*- auto-recompile: t -*-
;;; ask.el ---
;; Time-stamp: <2001-06-23 06:34:13 deego>
;; Copyright (C) Deepak Goel 2001
;; Emacs Lisp Archive entry
;; Filename: ask.el
;; Package: ask
;; Author: Deepak Goel <dgoel@wam.umd.edu>
;; Version:
;; Author's homepage: http://www.glue.umd.edu/~deego
;; For latest version:

(defvar ask-home-page  "http://www.glue.umd.edu/~deego")
;;; GPL'ed under GNU'S public license..

;; See also:


;; Quick start:
(defvar ask-quick-start
  "Still in the works. 
Do not use yet!
"
)

(defun ask-quick-start ()
  "Provides electric help for function `ask-quick-start'."
  (interactive)
  (with-electric-help
   '(lambda () (insert ask-quick-start) nil) "*doc*"))

;;; Introduction:
;; Stuff that gets posted to gnu.emacs.sources
;; as introduction
(defvar ask-introduction
  "type M-x ask-quick-start"
)

(defun ask-introduction ()
  "Provides electric help for function `ask-introduction'."
  (interactive)
  (with-electric-help
   '(lambda () (insert ask-introduction) nil) "*doc*"))

;;; Commentary:
(defvar ask-commentary
  "type M-x ask=quick-start "
)

(defun ask-commentary ()
  "Provides electric help for function `ask-commentary'."
  (interactive)
  (with-electric-help
   '(lambda () (insert ask-commentary) nil) "*doc*"))

;;; History:

;;; New features:
(defvar ask-new-features
  " ... USE M-X EDLIB-DOC OR C-X SPC SPC D HERE...
        TO EDIT DOCUMENTATION EASILY   "
)

(defun ask-new-features ()
  "Provides electric help for function `ask-new-features'."
  (interactive)
  (with-electric-help
   '(lambda () (insert ask-new-features) nil) "*doc*"))

;;==========================================
;;; Code:


(defun ask-ask-user (prompt &optional choices)
  "Choices is a list of one-letter strings..
Returns the one-letter string corresp. to user-response..

programmers consider using the more powerful ask-do-action instead."
  (if (null choices)
      (char-to-string (read-char-exclusive prompt))
    (let
	((response
	  (read-char-exclusive
	   (concat (apply 'concat prompt "( "
		  (mapcar
		   (lambda (arg)
		     (concat arg " "))
		   choices)) ")"))))
      (if (member response
		  (mapcar 'string-to-char choices))
	  (char-to-string response)
	(ask-ask-user prompt choices)))))

(defmacro ask-ask-and-do  (prompt &rest clauses)
  "Each clasuse is of the form (one-letter-string (body)..."
  (append
   (list 'ask-ask-and-do-char
	 prompt)
   (mapcar
    (lambda (arg)
      (cons (string-to-char (car arg)) (cdr arg)))
    clauses)))

(defmacro ask-ask-and-do-char (prompt &rest actions)
  "
Each action is of the form (char (action)...
"
    `(case
	 (string-to-char
	  (ask-ask-user
	   ,prompt
	   (mapcar 'char-to-string
		   (mapcar 'car (quote ,actions)))))
       ,@actions
       (t "rr")
       ))



(provide 'ask)

;;; ask.el ends here
