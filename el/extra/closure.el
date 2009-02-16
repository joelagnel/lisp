;;; closure.el -- Create lexical closures in Elisp

;; Copyright (C) 1997 Free Software Foundation, Inc.

;; $Id: closure.el,v 1.3 1997/05/29 18:53:25 petli Exp $

;; Author: Peter Liljenberg <petli@lysator.liu.se>
;; Created: May 1997
;; Keywords: lexical closure

;; This file is soon part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.

;;; Code:

(defconst lexical-closure-environment nil
  "This variable holds the current environment while parsing a closure
in preparse-closure. Don't touch!")

(defmacro closure (arglist varlist &rest forms)
  "Create a closure.

ARGLIST is the arglist to the closure, as for usual lambda lists.
VARLIST is the static vars, as for let.
FORMS is any number of forms that will be evaluated when the
`lexical-closure' is called.

Beware! Do not set the value of any variable in VARLIST with any
function other than setq. `closure' analyzes FORMS to figure
out what to replace with references/settings to the static variables,
and the code doesn't understand anything else. setf will work,
however, as it expands to some kind of setq and similar functions."

  (let* ((vars (mapcar (function (lambda (x)
				   (if (symbolp x) x (car x))))
		       varlist))

	 ;; Fetch the init values
	 (values (mapcar (function (lambda (x)
				     (if (symbolp x)
					 nil
				       (car (cdr x)))))
			 varlist))
	 (vectorvar (gensym))
	 (lexenv (mapcar 'cdr lexical-closure-environment))
	 (lexical-closure-environment (cons (cons vars vectorvar)
					    lexical-closure-environment))
	 
	 (hacked-forms (mapcar (function (lambda (x)
					   (lexc-hack-form x)))
			       forms)))

    (` (list 'lambda
	     '(, arglist)
	     (append
	      (list 'let
		    (list (list '(, vectorvar)
				(vector (,@ values)) )
			  (,@ (mapcar
			       (function (lambda (x)
					   (` (list '(, x)
						    (symbol-value '(, x))))))
			       lexenv)) ))
	      '(, hacked-forms))))))
   
(put 'closure 'lisp-indent-function 2)

(defun lexc-hack-form (form)
  "Hack FORM to work in a lexical closure.
Replace all refs to static vars to the vectors in
`lexical-closure-environment'."
  
  (setq form (macroexpand form))
  (cond
   ((symbolp form)
    (let ((pos (lexc-locate-var form lexical-closure-environment 0)))
      (if pos
	  (list 'aref (cdr pos) (car pos))
	form)))
   
   ((not (listp form)) form)

   ((eq (car form) 'setq)
    (lexc-hack-setq-form (cdr form)))

   ((eq (car form) 'quote) form)

   ((memq (car form) '(let let* lambda))
    (lexc-hack-let-form (car form) (car (cdr form)) (cdr (cdr form))))
     
   (t (cons (car form)
	    (mapcar (function (lambda (x)
				(lexc-hack-form x)))
		    (cdr form))))
   ))

(defun lexc-hack-setq-form (varval)
  "Hack a setq form with var-value pairs VARVAL."

  (let ((res nil)
	var value)
	
    (while varval
      (setq var (car varval)
	    value (car (cdr varval))
	    varval (cdr (cdr varval)))

      (let ((pos (lexc-locate-var var lexical-closure-environment 0)))
	(if pos
	    (setq res (cons (list 'aset (cdr pos) (car pos)
				  (lexc-hack-form value))
			    res))
	  (setq res (cons (list 'setq var (lexc-hack-form value))
			  res)))
	))

    (if (cdr res)
	(cons 'progn (nreverse res))
      (car res))
    ))
    
(defun lexc-hack-let-form (type vlist forms)
  "Hack the let-form of TYPE, with VLIST and FORMS."

  (let* ((venv (cons nil nil))
	 (lcenv (cons venv lexical-closure-environment))
	 (nvlist nil)
	 v)

    ;; Okay, so it's an ugly imperative solution.
    (while vlist
      (setq v (car vlist)
	    vlist (cdr vlist))
      (if (listp v)
	  ;; var with value, hack it's value form
	  (let ((lexical-closure-environment
		 (if (eq type 'let)
		     lexical-closure-environment
		   lcenv)))
	  
	    (setq nvlist (cons (list (car v) (lexc-hack-form (car (cdr v))))
			       nvlist))
	    (setcar venv (cons (car v) (car venv))))
      	  
	(setq nvlist (cons v nvlist))
	(setcar venv (cons v  (car venv))) ))

    (let ((lexical-closure-environment lcenv))
      (append (list type (nreverse nvlist))
	      (mapcar (function (lambda (x)
				  (lexc-hack-form x)))
		      forms)))
    ))

(defun lexc-locate-var (var env level)
  "Find the position of VAR in the lexical environment ENV.
LEVEL, is the level we're looking at, counting from zero.

Returns a cons: (pos-in-level vectorvar), or nil if not found."

  (if (null env)
      nil
    (let ((tail (member var (car (car env)))))
      (if tail
	  (and (cdr (car env))
	       (cons (- (length (car (car env))) (length tail))
		     (cdr (car env))))
	(lexc-locate-var var (cdr env) (1+ level)))
      )))

(provide 'closure)
;;; closure ends here
