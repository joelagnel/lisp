;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FILE:          cl-pcl.el
;;; DESCRIPTION:   Extensions to cl-shell.el for PCL
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
;;; cl-shell.el when using Portable Common Loops.  It was written to
;;; work with the 12/88 release.  It seems to work with Victoria Day
;;; (5/89) also.  It is loaded automatically by run-cl if :PCL is on
;;; the *features* list in the CL world.

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

;;; Modify this so that methods are compiled the fast way (along with
;;; functions and macros):
(setq cl-fast-compile-regexp "(def\\(un\\|macro\\|method\\)[ \t\n]+")

;;; Modify the compile-def macro to call a new function called
;;; compile-function-and-methods.  This comes from the PCL file high.lisp.
(cl-send-string
 "(progn
   (defmacro user::compile-def (thing)
    (let ((val (gensym)))
      (if (and (listp thing) (eq (car thing) 'pcl:defmethod))
        `(progn ,thing
                (user::compile-function-and-methods ',(cadr thing)))
        `(compile ,thing))))
   (defun user::compile-function-and-methods (the-symbol)
     (let ((the-function (symbol-function the-symbol))
	   func meth name)
       (when (not (compiled-function-p the-function)) (compile the-symbol))
       (when (pcl::generic-function-p the-function)
	 (dolist (m (pcl::generic-function-methods the-function))
	   (multiple-value-setq (func meth name) (pcl::parse-method-or-spec m))
	   (setq func (pcl::method-function meth))
	   (when (not (compiled-function-p func))
	     (compile name func)
	     (setf (pcl::method-function meth) (symbol-function name))))))
     the-symbol)
  (values))\n")

;;;; ---------- Source file recording enhancements for Lucid ---------

;;; These are helpful if the file source-file-extensions.lisp is
;;; loaded into lisp.  Nothing breaks if this is not true.
;;; *** Need to do pushnew here.
(if (featurep 'cl-lucid)		;if Lucid CL
    (setq *cl-definition-regexp-alist*
	  (append *cl-definition-regexp-alist*
		  '((:CLASS . "(defclass[ \t\n]*%s")
		    (:METHOD . cl-make-clos-method-regexp)))))

;;; Type-spec defined in source-file-extensions.lisp is
;;; '(:method . argument-classes)
(if (featurep 'cl-lucid)
    (defun cl-make-clos-method-regexp (symbol type-spec)
      (setq type-spec (cdr type-spec))
      (let ((the-regexp (format "(defmethod[ \t\n]*%s[ \t\n]*(" symbol)))
	(while type-spec
	  (setq the-regexp
		(concat
		 the-regexp
		 (format "[ \t\n]*(\\w*[ \t\n]*%s[ \t\n]*)"
			 (cl-strip-package (car type-spec)))))
	  (setq type-spec (cdr type-spec)))
	the-regexp)))
	   
