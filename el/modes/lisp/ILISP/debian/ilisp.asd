;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          ilisp.asd
;;;; Purpose:       ASDF system definition file for ilisp package
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  March 2003
;;;;
;;;; $Id: ilisp.asd,v 1.7 2003/05/27 17:42:31 kevinrosenberg Exp $
;;;;
;;;; UFFI users are granted the rights to distribute and use this software
;;;; as governed by the terms of the ILISP license.
;;;; *************************************************************************

(defpackage #:ilisp-system (:use #:cl #:asdf))
(in-package #:ilisp-system)

(defun symlink-ilisp-fasls ()
  (let ((fasls-path
	 (merge-pathnames
	  (make-pathname :directory '(:relative "ilisp"))
	  c-l-c::*fasl-root*))
	(dest-path (make-pathname :directory '(:absolute "usr" "lib" "ilisp"))))
    (format *trace-output* "~&Symlinking fasls~%")
    (dolist (fasl (directory
		   (make-pathname :defaults fasls-path
				  :name :wild
				  :type :wild)))
      (format t "~S~%" fasl)
      (when (pathname-type fasl) ;; Crude check to avoid matching a directory
	(let ((symlink (make-pathname
			:directory (pathname-directory dest-path)
			:name (pathname-name fasl)
			:type (pathname-type fasl))))
	  (when (probe-file symlink)
	    (delete-file symlink))
	(let ((cmd (format nil "ln -sf ~A ~A"
			   (namestring fasl) (namestring symlink))))
	  (run-shell-command cmd))
	)))))

#+(or allegro clisp lispworks cmu openmcl sbcl)
(defsystem :ilisp
  :name "ilisp"
  :maintainer "Kevin M. Rosenberg <kmr@debian.org>"
  :licence "ILISP license"
  :description "System loader for ILISP inferior-mode lisp interface"

  :perform (compile-op :after (op ilisp)
		       (symlink-ilisp-fasls))

  :components
  ((:file "ilisp-pkg")
   (:file "cl-ilisp" :depends-on ("ilisp-pkg"))
   #+allegro (:file "allegro" :depends-on ("cl-ilisp"))
   #+clisp (:file "cl-chs-init" :depends-on ("cl-ilisp"))
   #+cmu (:file "cmulisp" :depends-on ("cl-ilisp"))
   #+lispworks (:file "lispworks" :depends-on ("cl-ilisp"))
   #+openmcl (:file "openmcl" :depends-on ("cl-ilisp"))
   #+sbcl (:file "sbcl" :depends-on ("cl-ilisp"))
   ))


