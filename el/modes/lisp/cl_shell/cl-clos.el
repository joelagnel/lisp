;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FILE:          cl-clos.el
;;; DESCRIPTION:   Extensions to cl-shell.el for CLOS
;;; AUTHOR:        Eero Simoncelli, 
;;;                Vision Science Group, 
;;;                MIT Media Laboratory.
;;; CREATED:       December, 1989
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This file contains additional hacks for use with code in
;;; cl-shell.el when using the Common Lisp Object System provided with
;;; lucid.  It is loaded automatically by run-cl if :LCL4.0 and :CLOS
;;; are on the *features* list in the CL world.

(require 'cl-shell)

;;; Add some more special forms to the indentation list.  See
;;; cl-indent.el for more confusion.  Basically, the number refers to
;;; the number of "special" - i.e. non-body forms passed as arguments
;;; to these things. 
(put 'defgeneric  'common-lisp-indent-hook 'defun)
(put 'defmethod  'common-lisp-indent-hook 'defun)
(put 'defclass   'common-lisp-indent-hook 'defun)
(put 'with-slots 'common-lisp-indent-hook 2)
(put 'with-accessors 'common-lisp-indent-hook 2)
(put 'with-added-methods 'common-lisp-indent-hook 1)
(put 'symbol-macrolet 'common-lisp-indent-hook 1)
(put 'generic-flet 'common-lisp-indent-hook 1)
(put 'generic-function 'common-lisp-indent-hook 1)
(put 'generic-labels 'common-lisp-indent-hook 1)

;;;; ---------- Allow direct compilation of CLOS methods ---------

;;; Modify this so that CLOS methods are compiled directly (along with
;;; functions and macros):
(setq cl-fast-compile-regexp "(def\\(un\\|macro\\|method\\)[ \t\n]+")

;;; Modify the compile-def macro to allow direct compilation of CLOS
;;; methods!
(cl-send-string
 "#+(and LCL4.0 CLOS)
  (progn
   (defmacro user::compile-def (thing)
     (if (and (listp thing) (eq (car thing) 'clos:defmethod))
       `(clos-sys:compile-method ,thing)
       `(compile ,thing)))
   (values))\n")


;;;; ---------- Source file recording enhancements for CLOS in Lucid ---------

;;; These are helpful if the file source-file-extensions.lisp is
;;; loaded into lisp.  Nothing breaks if this is not the case.
;;; *** Need to do pushnew here.
(if (featurep 'cl-lucid)
    (setq *cl-definition-regexp-alist*
	  (append *cl-definition-regexp-alist*
		  '((CLASS . "(defclass[ \t\n]*%s")
		    (METHOD . cl-make-clos-method-regexp)
		    (GENERIC-FUNCTION . "(defgeneric[ \t\n]*%s")
		    ))))

;;; Type-spec defined in source-file-extensions.lisp is
;;; '(METHOD <name> . argument-classes)
(if (featurep 'cl-lucid)
    (defun cl-make-clos-method-regexp (symbol type-spec)
      (setq type-spec (cdr type-spec))	;strip 'method
      (setq type-spec (cdr type-spec))  ;strip method name
      (let (the-regexp)
	(if (listp (car type-spec))
	    (progn			;primary method
	      (setq the-regexp (format "(defmethod[ \t\n]*%s[ \t\n]*(" symbol))
	      (setq type-spec (car type-spec)))
	    (progn			;secondary (:around, :before, :after)
	      (setq the-regexp (format "(defmethod[ \t\n]*%s[ \t\n]*%s[ \t\n]*("
				       symbol (car type-spec)))
	      (setq type-spec (car (cdr type-spec)))))
	(while type-spec
	  (setq the-regexp
		(concat
		 the-regexp
		 (if (or (eq (car type-spec) t) (eq (car type-spec) 'T))
		     (format "[ \t\n]*\\(\\w*\\|(\\w*[ \t\n]*%s[ \t\n]*)\\)"
			     (cl-strip-package (car type-spec)))
		     (format "[ \t\n]*(\\w*[ \t\n]*%s[ \t\n]*)"
			     (cl-strip-package (car type-spec))))))
	  (setq type-spec (cdr type-spec)))
	the-regexp)))
	   
