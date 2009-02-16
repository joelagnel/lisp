;;; -*- Mode: Lisp; tab-width: 4 -*-
;;; cormanlisp.lisp --

;;; This file is part of ILISP.
;;; Version: 5.8 - 5.10
;;;
;;; Copyright (C) 1990, 1991, 1992, 1993 Chris McConnell
;;;               1993, 1994 Ivan Vasquez
;;;               1994, 1995, 1996 Marco Antoniotti and Rick Busdiecker
;;;               1996 Marco Antoniotti and Rick Campbell
;;;		  	      2000 Reini Urban
;;;
;;; Other authors' names for which this Copyright notice also holds
;;; may appear later in this file.
;;;
;;; ILISP is freely redistributable under the terms found in the file
;;; COPYING.

;;;
;;; Cormanlisp initializations
;;; Author: Reini Urban <rurban@x-ray.at>
;;;
(in-package :ilisp)

(defun ilisp-source-files (symbol package type)
  "Print each file for PACKAGE:SYMBOL's TYPE definition on a line and
return T if successful."
  (declare (ignore type))
  (ilisp-errors
   (let* ((symbol (ilisp-find-symbol symbol package))
	  (type t)
	  ;;(type (if (equal type "any") t (ilisp-find-symbol type "keyword")))
	  (paths (when symbol (debug::function-source-file symbol))))
     (if paths
	 (progn
	   (if (eq type t)
	       (dolist (path (remove-duplicates paths
						:key #'cdr :test #'equal))
		 (print (namestring (cdr path))))
	       (print (namestring paths)))
	   t)
	 nil))))

(export '(ilisp-source-files))

;;; ILISP Patches for cormanlisp <= 2.0

(in-package :common-lisp)

(defun inspect (symbol)
  (describe symbol))

;; not really needed with my cl-ilisp.lisp patch, but for legacy sake
(defun special-form-p (symbol)
  (special-operator-p symbol))

(unless (fboundp 'compile-file-pathname)

(defvar fasl-file-extension ".fasl")
;;;
;;; Common Lisp COMPILE-FILE-PATHNAME function.
;;;
;;; CLtL2: "If an implementation supports additional keyword arguments to
;;; compile-file, compile-file-pathname must accept the same arguments."
;;;
(defun compile-file-pathname (input-file &key 
                                         (output-file nil)
                                         (verbose *compile-verbose*)
                                         (print *compile-print*)
                                         (external-format :default))
  (create-pathname-from-string
   (compile-file-name  (namestring (pathname input-file))
					  :output-file output-file
					  :verbose     verbose
					  :print       print
					  :external-format external-format)))

(defun compile-file-name (input-file &key 
									 (output-file nil)
									 (verbose *compile-verbose*)
									 (print *compile-print*)
									 (external-format :default))
  "Returns the compiled filename string for the input-file string"
  (declare (ignore verbose external-format print))
  (if (null output-file)
      (if (string-equal
           (subseq input-file 
                   (- (length input-file)(length lisp-file-extension))
                   (length input-file))
           lisp-file-extension)
          (concatenate 'string 
                       (subseq input-file 
                               0
                               (- (length input-file)
								  (length lisp-file-extension)))
                       fasl-file-extension)
        (concatenate 'string input-file fasl-file-extension))
     (namestring (pathname output-file))))

) ; eof compile-file-pathname patch

(unless (fboundp 'readtable-case)
  
;;;
;;; Common Lisp READTABLE-CASE accessor
;;;
;;; Note: at booting check-type,warn,defun setf are not defined
;;;
(defun readtable-case (readtbl)
  (if (macro-function 'check-type)	; booting
	  (check-type readtbl readtable)
	;; else
    (if (not (readtablep readtbl))
		(error "Argument is no valid readtable: ~A" readtbl)))
  (uref readtbl readtable-case-offset))

(defun set-readtable-case (readtbl value)
  "For compatibility only. All values except :UPCASE are ignored."
  (let ((valid-case '(:upcase))
		(ignored-case '(:downcase :preserve :invert)))
	(if (macro-function 'check-type)	; booting
		(progn
		  (check-type readtbl readtable)
		  (check-type value symbol)))
	(cond 
	 ((member value valid-case)
	  (setf (uref readtbl readtable-case-offset) value))
	 ((member value ignored-case)
	  (error "SET-READTABLE-CASE: only :UPCASE supported: ~A" 
			value))
	 (T
	  (error "Argument is no valid readtable-case: ~A, expected ~A"
			 value valid-case)))))

;;; bootstrapping
(set-readtable-case *readtable* ':upcase)
(set-readtable-case *common-lisp-readtable* ':upcase)

(defsetf readtable-case set-readtable-case)

)  ; eof readtable-case patch

(in-package :ilisp)
