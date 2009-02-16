;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FILE:          cl-flavors.el
;;; DESCRIPTION:   Extensions to cl-shell.el for FLAVORS
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
;;; cl-shell.el when using FLavors.  It is loaded automatically by
;;; run-cl if :FLAVORS is on the *features* list in the CL world.

(require 'cl-shell)

;;; Add some more special forms to the indentation list.  See
;;; cl-indent.el for more confusion.  Basically, the number refers to
;;; the number of "special" - i.e. non-body forms passed as arguments
;;; to these things.  *** Many more forms should be added here...
(put 'defmethod  'common-lisp-indent-hook 'defun)
(put 'defflavor   'common-lisp-indent-hook 'defun)

;;;; ---------- Source file recording enhancements for Lucid ---------

;;; These are helpful if the file source-file-extensions.lisp is
;;; loaded into lisp.  Nothing breaks if this is not true.
;;; *** need to do pushnew here.
(if (featurep 'cl-lucid)		;if Lucid CL
    (setq *cl-definition-regexp-alist*
	  (append *cl-definition-regexp-alist*
		  '((:FLAVOR . "(defflavor[ \t\n]*%s")
		    (:METHOD . cl-make-flavor-method-regexp)))))

;;; Type-spec defined in source-file-extensions.lisp is
;;; '(:method <flavor> <type>).  We try to handle the old Symbolics
;;; syntax as well as the newer one.
(if (featurep 'cl-lucid)
    (defun cl-make-flavor-method-regexp (method type-spec)
      (let ((flavor (cl-strip-package (cadr type-spec)))
	    (type (cl-strip-package (caddr type-spec))))
	(concat "(defmethod[ \t\n]*([ \t\n]*" ;"(defmethod ("
		"\\("			;either
		(format "%s[ \t\n]*\\(%s\\|%s[ \t\n]*%s\\)" ;old syntax
			flavor method type method)
		"\\|"			;or
		(format "%s[ \t\n]*\\(%s\\|%s[ \t\n]*%s\\)" ;new syntax
			method flavor flavor type)
		"\\)"))))
