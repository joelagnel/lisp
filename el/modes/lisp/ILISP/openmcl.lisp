;;; -*- Mode: Lisp -*-

;;; openmcl.lisp --

;;; This file is part of ILISP.
;;; Please refer to the file COPYING for copyrights and licensing
;;; information.
;;; Please refer to the file ACKNOWLEGDEMENTS for an (incomplete) list
;;; of present and past contributors.


(in-package :ilisp)


;;;%% arglist/source-file utils.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(arglist source-file openmcl-trace)))


;;;%% arglist - return arglist of function

(defun arglist (symbol package)
  (ilisp-errors
   (let* ((package-name (if (packagep package)
			    (package-name package)
			    package))
	  (x (ilisp-find-symbol symbol package-name)))
     (ccl::arglist x))))


;;; source-file symbol package type --

    
(defun source-file (name package type)
  (ilisp-errors
   (flet ((print-source (path) (when path (print (namestring (truename path))) t)))
     (setq type (intern (string-upcase (string type)) "CL"))
     (let* ((symbol (ilisp-find-symbol name package))
	    (source-info (ccl::%source-files symbol)))
       (when source-info
	 (if (atom source-info)
	   (when (eq type 'function)
	     (print-source source-info))
	   (let* ((info (or (cdr (assoc type source-info))
			    (and (eq type 'function)
				 (mapcar #'cdr
					  (cdr (assoc 'ccl::method source-info)))))))
	     (when info
	       (if (atom info)
		 (print-source info)
		 (dolist (p info t)
		   (print-source p)))))))))))

(defun ilisp-callers (symbol package)
  (ilisp-errors
   (let* ((function-name (ilisp-find-symbol symbol package))
	  (callers (ccl::callers function-name)))
     (when callers
       (dolist (caller callers t) (print caller))))))



(defun openmcl-trace (symbol package breakp)
  "Trace SYMBOL in PACKAGE."
  (ilisp-errors
   (let ((real-symbol (ilisp-find-symbol symbol package)))
     (setq breakp (read-from-string breakp))
     (when real-symbol (eval `(trace (,real-symbol
				      :before ,(if breakp :break))))))))


;;; Some versions of OpenMCL don't define INSPECT.  The FTYPE declamation
;;; below will keep the compiler from generating UNDEFINED-FUNCTION warnings
;;; when it sees calls to INSPECT.

(declaim (ftype (function (t) t) inspect))

;;; end of file -- openmcl.lisp --
