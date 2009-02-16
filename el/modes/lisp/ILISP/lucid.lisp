;;; -*- Mode: Lisp -*-

;;; lucid.lisp --
;;; Lucid initializations 
;;; Author: Chris McConnell, ccm@cs.cmu.edu
;;;
;;; This file is part of ILISP.
;;; Please refer to the file COPYING for copyrights and licensing
;;; information.
;;; Please refer to the file ACKNOWLEGDEMENTS for an (incomplete) list
;;; of present and past contributors.
;;;
;;; $Id: lucid.lisp,v 1.3 2002/03/26 09:41:04 anisotropy9 Exp $

(in-package :ilisp)

;;;
(defun ilisp-callers (symbol package &aux (list-of-callers nil))
  "Print the callers of PACKAGE::SYMBOL.  Only compiled functions
currently.  Return T if successful."
  (ilisp-errors
   (let ((function-name (ilisp-find-symbol symbol package))
	 (*print-level* nil)
	 (*print-length* nil)
	 (*package* (find-package 'lisp)))
     (when (and function-name (fboundp function-name))
       (flet
	   ((check-symbol (symbol)
	      (labels
		  ((check-function (function &optional exclusions)
		     (do ((i 4 (1+ i)))
			 ((>= i (lucid::procedure-length function)))
		       (let ((element (sys:procedure-ref function i)))
			 (cond ((eq element function-name)
				(pushnew symbol list-of-callers))
			       ((and (compiled-function-p element)
				     (not (find element exclusions)))
				(check-function
				 element
				 (cons element exclusions))))))))
		(check-function (symbol-function symbol)))))
	 (do-all-symbols (symbol)
	   (when (fboundp symbol)
	     (check-symbol symbol)))
	 (dolist (caller list-of-callers)
	   (print caller))
	 t)))))

;;;
(defun ilisp-source-files (symbol package type)
  "Print each file for PACKAGE:SYMBOL's TYPE definition on a line and
return T if successful."
  (ilisp-errors
   (let* ((symbol (ilisp-find-symbol symbol package))
	  (all (equal type "any"))
	  (type (unless all (ilisp-find-symbol type package)))
	  (paths (when symbol
		   (lucid::get-source-file symbol type all))))
     (if paths
	 (progn
	   (if all
	       (dolist (file (remove-duplicates paths
						:key #'cdr :test #'equal))
		 (print (namestring (cdr file))))
	       (print (namestring paths)))
	   t)
	 nil))))

;;;
(dolist (symbol '(ilisp-callers ilisp-source-files))
  (export symbol))
(unless (compiled-function-p #'ilisp-callers)
  (format t "\"ILISP: File is not compiled, use M-x ilisp-compile-inits\""))
