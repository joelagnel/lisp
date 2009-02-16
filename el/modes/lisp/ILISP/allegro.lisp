;;; -*- Mode: Lisp -*-

;;; allegro.lisp --
;;; ILISP Franz ACL dialect support definitions.
;;;
;;; This file is part of ILISP.
;;; Please refer to the file COPYING for copyrights and licensing
;;; information.
;;; Please refer to the file ACKNOWLEGDEMENTS for an (incomplete) list
;;; of present and past contributors.
;;;
;;; $Id: allegro.lisp,v 1.7 2003/05/19 18:15:41 kevinrosenberg Exp $

;;; Allegro initializations
;;; Author: Chris McConnell, ccm@cs.cmu.edu

(in-package :ilisp)

;; Larry Hunter's fix
#+(and allegro-version>= (version>= 5))
(eval-when (compile load) (require :llstructs))

;;;
(defun ilisp-callers (symbol package)
  "Print a list of all of the functions that call FUNCTION.
Returns T if successful."
  (ilisp-errors
   (let ((function (ilisp-find-symbol symbol package))
	 (callers nil)
	 (*print-level* nil)
	 (*print-length* nil)
	 (*package* (find-package 'lisp)))
     (when (and function (fboundp function))
       (labels ((in-expression (function expression)
		  (cond ((null expression) nil)
			((listp expression)
			 (let ((header (first expression)))
			   (if (or (eq header function)
				   (and (eq header 'function)
					(eq (second expression) function)))
			       t
			       (dolist (subexp expression)
				 (when (in-expression function subexp)
				   (return t)))))))))
	 (excl::who-references
	  function
	  #'(lambda (function)
	      (push (excl::fn_symdef function) callers)))
	 (do-all-symbols (symbol)
	   (when (and (fboundp symbol)
		      (not (compiled-function-p (symbol-function symbol)))
		      (in-expression function (symbol-function symbol)))
	     (push symbol callers)))
	 (dolist (caller callers)
	   (print caller))
	 t)))))

;;;===========================================================================
;;; Epilogue

(eval-when (load eval)
  (unless (compiled-function-p #'ilisp-callers)
    (ilisp-message t "File is not compiled, use M-x ilisp-compile-inits")))

;;; end of file -- allegro.lisp --
