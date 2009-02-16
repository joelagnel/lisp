;;; -*- Mode: Lisp -*-

;;; ilisp-pkg.lisp --
;;; ANSI CL DEFPACKAGE definition for ILISP.
;;;
;;; Common Lisp initializations
;;;
;;; Author: Marco Antoniotti, marcoxa@cs.nyu.edu
;;;
;;; This file is part of ILISP.
;;; Please refer to the file COPYING for copyrights and licensing
;;; information.
;;; Please refer to the file ACKNOWLEGDEMENTS for an (incomplete) list
;;; of present and past contributors.
;;;
;;; $Id: ilisp-pkg.lisp,v 1.11 2004/08/11 19:12:46 kevinrosenberg Exp $

;;;----------------------------------------------------------------------------
;;; Definitions

;;; ILISP package --

;;;
;;; GCL 2.2 doesn't have defpackage (yet) so we need to put the export
;;; here. (toy@rtp.ericsson.se)
;;;
;;; Please note that while the comment and the fix posted by Richard
;;; Toy are correct, they are deprecated by at least one of the ILISP
;;; maintainers. :) By removing the 'nil' in the following #+, you
;;; will fix the problem but will not do a good service to the CL
;;; community.  The right thing to do is to install DEFPACKAGE in your
;;; GCL and to write the GCL maintainers and to ask them to
;;; incorporate DEFPACKAGE in their standard builds.
;;; Marco Antoniotti <marcoxa@icsi.berkeley.edu> 19960715
;;;
;;; "The use of keyword and uninterned symbol names in the package
;;; definition is a result of internecine wars during the ANSI
;;; definition process. The solution to make CL case insensitive and
;;; have the reader use uppercase appears, with the power of
;;; hindsight, short-sighted. However, the backwardly incompatible
;;; solution provided by Franz Inc seems a sub-optimal fix."
;;; 27 March 2002 Will Deakin

#-(and gcl)
(defpackage :ilisp
  (:use :common-lisp
	#+:CMU :conditions
	;; MOP packages.  This avoids tons of #+ noise in the find-src.lisp
	;; code.
	;; [in allegro, :mop and :clos are the same package, though it isn't
	;; called that in 5.0.1; it has nicknames aclmop (which was dropped in
	;; 6.0) and acl-mop (still in 6.1).  -- rgr, 12-Sep-02.]
	#+allegro :clos
	;; Portable Common Loops (PCL) has a separate :mop package.
	#+pcl :pcl #+pcl :mop
	;; SBCL is PCL-derived, but uses distinct package names (and doesn't
	;; advertices :PCL on *features*).
	#+sbcl :sb-pcl)
  ;; Known cmucl/pcl glitch: the :mop and :pcl packages define different
  ;; versions of these, which are exported from the :lisp package.
  #+(or pcl sbcl)
  (:shadowing-import-from #-sbcl :mop #+sbcl :sb-pcl
			  :class-name :built-in-class :class-of :find-class
			  :structure-class)
  ;; Now get some non-exported symbols
  #+(or pcl sbcl)
  (:import-from #+sbcl :sb-pcl #-sbcl :pcl
		:arg-info-lambda-list :definition-source
		:definition-source-mixin :generic-function-methods
		;; [this is exported from PCL in CMUCL, but not from SB-PCL.  --
		;; rgr, 20-Sep-02.]
		#+sbcl :standard-accessor-method)
  #+(or pcl sbcl allegro)
  (:import-from #+sbcl :sb-pcl
		#+(and pcl (not sbcl)) :pcl
		#+allegro :excl
		:arg-info :lambda-list :specializers)
  ;; [i forget why i wanted this rather than lisp:standard-class . . .  -- rgr,
  ;; 15-Sep-02.]
  #+(or pcl sbcl)
  (:shadowing-import-from #+sbcl :sb-pcl #-sbcl :pcl
			  :standard-class)

  ;; The following symbols should properly 'shadow' the inherited
  ;; ones.
  (:export #:ilisp-errors
           #:ilisp-save
           #:ilisp-restore
           #:ilisp-symbol-name
           #:ilisp-find-symbol
           #:ilisp-find-package
           #:ilisp-eval
           #:ilisp-compile
           #:ilisp-describe
           #:ilisp-inspect
           #:ilisp-arglist
           #:ilisp-documentation
           #:ilisp-macroexpand
           #:ilisp-macroexpand-1
           #:ilisp-trace
           #:ilisp-untrace
           #:ilisp-compile-file-extension
           #:ilisp-compile-file
           #:ilisp-casify
           #:ilisp-matching-symbols
           #:ilisp-callers
           #:ilisp-source-files
           #:ilisp-print-info-message
           #+:SBCL #:sbcl-trace
           #+:CMU #:cmulisp-trace
           #+(or :SBCL :CMU) #:source-file
	   )
  )
;;; ILISP --

;;; end of file -- ilisp-pkg.lisp --
