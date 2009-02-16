;;; auto-define.el --- Define functions on the fly

;; Copyright (C) 1999 by Michael Abraham Shulman

;; Emacs Lisp Archive Entry
;; Filename: auto-define.el
;; Author: Michael Abraham Shulman <viritrilbia@users.sourceforge.net>
;; Version: 1.1
;; Keywords: extensions

;;{{{ GPL

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;}}}

;;; Commentary:

;; This file supplies (surprisingly small) code which allows elisp
;; functions to be defined "on the fly," as they are called.  Read the
;; documentation of `auto-define-functions' for instructions.

;; This feature is similar to the AUTOLOAD subroutine in Perl, except
;; that it always defines the function rather than standing in for it.
;; If you want to do something different every time a function is
;; called (although I don't know why you would want to do something
;; that you couldn't do by defining the function to do whatever you
;; wanted), you can define the function to "un-set" itself when
;; executed.

;; This file also supplies two examples of how Auto-define can be
;; used. Admittedly, neither of them is particularly useful, but they
;; are aesthetically pleasing (to me, anyway).  One conclusion that can
;; be drawn from the uselessness of these examples is that I haven't
;; found a real use for auto-define.  I just wrote it because I could,
;; and because I liked the examples.

;; I've always been bothered by the neccessity for `cadr', `caar',
;; `cdar', `cddr', and their longer brethren to each be explicitly
;; defined, one by one, rather than being able to give a general rule.
;; The general rule can be done with Auto-define; see "cX*r Example."
;; Similarly, we have `prog1' and `prog2' but nothing after that until
;; `progn'. "Progk Example" fixes that deficiency.

;;; Code:

;;{{{ Auto-Define Code

(defvar auto-define-functions nil
  "Functions to call when a function needs to be auto-defined.

This is a non-standard hook: Auto-define functions must take one
argument, a symbol representing the (nonexistent) function called,
should return either T, NIL, or a symbol or lambda expression. A
return value of NIL means that the function was not handled and the
next auto-define function should be tried. A return value of T means
that the function was defined and the call should be re-executed. A
return value of a symbol or lambda expression means that the function
should be defined to that value and the call re-executed.")

(defadvice eval (around auto-define)
  "Auto-define functions with the value of `auto-define-functions'.
If none of the functions in `auto-define-functions' handle an unbound
function, signal an error as normal."
  (condition-case err
      ad-do-it
    (void-function
     (if (auto-define (cadr err) auto-define-functions)
	 ad-do-it
       (signal (car err) (cdr err))))))

(ad-activate 'eval)

(defun auto-define (called-function fnlist)
  "Auto-define CALLED-FUNCTION with the functions in FNLIST. Returns
non-nil if successful, nil if none of them can handle it."
  (when fnlist
    (let ((retval (funcall (car fnlist) called-function)))
      (case retval
	((nil) (auto-define called-function (cdr fnlist)))
	((t) t)
	(t (fset called-function retval) t)))))

;;}}}
;;{{{ cX*r Example

;;; Make more cX*rs. Elisp only comes with length 2 ones, and cl
;;; defines them up to length four, but in a perfect world, we would
;;; have as many as we want.

(defun auto-define-cx*r (symbol)
  "Define the functions cX*r appropriately."
  (if (string-match "^c[ad]+r$" (symbol-name symbol))
      `(lambda (x)
	 ,@(cx*r->ca/drs (substring (symbol-name symbol) 1 -1) 'x))))

(add-hook 'auto-define-functions 'auto-define-cx*r)

(defun cx*r->ca/drs (string val)
  (if (string= string "")
      (list (concat "Return " (upcase (symbol-name val)) ".") val)
    (let ((prev (cx*r->ca/drs (substring string 1) val)))
      (list (concat "Return the `c" (substring string 0 1) "r' of " 
		    (substring (car prev) 7))
	    (list (intern (concat "c" (substring string 0 1) "r"))
		  (cadr prev))))))

;;}}}
;;{{{ Progk Example

;;; To go along with prog1, prog2, and progn

(defun auto-define-progk (symbol)
  (if (string-match "^prog[1-9][0-9]*$" (symbol-name symbol))
      `(macro lambda (&rest body)
	      (cons 'progk 
		    (cons ,(string-to-number (substring (symbol-name symbol) 4))
			  body)))))

(add-hook 'auto-define-functions 'auto-define-progk)

(defmacro progk (k &rest body)
  "Evaluates expressions in BODY in order, returning the value of the
Kth, if K is positive. Does not accept nonpositive arguments yet. The
idea would be to return NIL if K is zero and the (-K)th from the end
if K is negative. `prog-1' would be the same as `progn'."
  (labels ((prog-sub (k body)
		     (if (and (> k 1) body)
			 (append (list (car body))
				 (prog-sub (1- k) (cdr body)))
		       `((prog1 ,@body)))))
    `(progn ,@(prog-sub k body))))

;;}}}

(provide 'auto-define)

;;; auto-define.el ends here