;;; -*- Mode: Lisp -*-

;;; cl-chs-init.lisp --
;;; Init file for CLisp H.S.
;;;
;;; This file is part of ILISP.
;;; Please refer to the file COPYING for copyrights and licensing
;;; information.
;;; Please refer to the file ACKNOWLEGDEMENTS for an (incomplete) list
;;; of present and past contributors.
;;;
;;; $Id: cl-chs-init.lisp,v 1.4 2003/05/09 23:29:59 bill_clementson Exp $

;;; Old history logs.
;;;
;;; 19990912 Marco Antoniotti
;;; Cleaned up for inclusion in 5.9.1.  See also file 'cl-ilisp.lisp'
;;; for related changes (e.g. in ILISP-ERRORS).
;;;
;;; 1999-09-03; M. Atzmueller
;;; removed obsolete stuff
;;;
;;; 1999-06-15: M. Atzmueller
;;; removed command to load inspect1.fas
;;; load any INSPECTOR yourself if there is no other inspect!
;;; 
;;; 1999-05-31: M. Atzmueller
;;; ilisp-arglist => #+clisp arglist (...) modified definition
;;; another option might be sys::arglist ...
;;; added command to preload inspect1

(in-package :ilisp)

;;;
(defun ilisp-inspect (sexp package)
  "Inspect SEXP in PACKAGE."
  (when (not (ignore-errors (functionp #'inspect)))
    (cerror
     "~% Try loading it yourself, or proceed without inspecting ... :-( !" 
     "~% There seems to be no INSPECTOR present!"))

  (ilisp-errors
   (let ((*package* (ilisp-find-package package)))
     (if (functionp #'inspect)
	 (let ((item-to-be-described (read-from-string sexp)))
	   (if (atom item-to-be-described)
	       (inspect item-to-be-described)
	     (inspect (eval item-to-be-described))))
       (format t "Sorry -- can't inspect ~S as Clisp has no inspector!"
		     sexp)))))

(defun arglist (sym)
  (when (fboundp sym)
    (let* ((s (with-output-to-string (s) (describe (symbol-function sym) s)))
	   (p (search "Argument list: " s)))
      (if p
	  (read-from-string (subseq s (+ 15 (search "Argument list: " s))))
	'(???)))))

;;; Epilogue

(eval-when (:execute :load-toplevel)
  (when (not (compiled-function-p #'ilisp-inspect))
    (ilisp-message t "File is not compiled, use M-x ilisp-compile-inits"))
  (sys::debug-unwind))


;;; end of file -- cl-chs-init.lsp --
