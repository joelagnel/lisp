;;; $Header: /tmp_mnt/am/p7/utility/gmacs/f2/RCS/advise.el,v 1.2 89/03/25 14:00:55 kahle Exp $

;;;
;;;				NO WARRANTY
;;;
;;; This software is distributed free of charge and is in the public domain.
;;; Anyone may use, duplicate or modify this program.  Thinking Machines
;;; Corporation does not restrict in any way the use of this software by
;;; anyone.
;;; 
;;; Thinking Machines Corporation provides absolutely no warranty of any kind.
;;; The entire risk as to the quality and performance of this program is with
;;; you.  In no event will Thinking Machines Corporation be liable to you for
;;; damages, including any lost profits, lost monies, or other special,
;;; incidental or consequential damages arising out of the use of this program.
;;;
;;; 9/18/88
;;;

;;This is an advise for gmacs lisp
;; roughly based on jim salem's common-lisp advise
;; in /cm/utilities/f5100/macros.lisp (if anyone wants the cl version,
;; write to brewster@think.com)
;;
;; -brewster
;;
;;; Using this advise is like using the lispm advise.
;;;  (advise foo :before (print 'this-is-before-foo-is-called))
;;;  (advise foo :after (print 'this-is-after-foo-is-called))
;;;  (advise foo :around (progn (print 'this-is-before-foo-is-called)
;;;                             :do-it
;;;                             (print 'this-is-after-foo-is-called))
;;;  
;;;  The arguments to the function being called can be accessed
;;;    by looking at the binding of arglist.
;;;  The return values can be modified by :around and :after code
;;;    by setq-ing VALUES with the list of the return values 
;;;    (it has to be a list even if it is just 1 value)
;;;
;;;
;;;  Unadvise (&optional function) works only on functions that have 
;;;    been advised 
;;;    with this advise.  It will replace the original function definition.
;;;    thus if a function is advised twice, the original will
;;;    be restored upon the call to unadvise.
;;;    If the function has been recompiled some other way, 
;;;    then the function will still be replaced with the function that 
;;;    was bound the first time advise was
;;;    called.  This seems like a bug and maybe should be fixed.
;;;    -brewster
;;; 
;;;  Test-Unadvise ()  is a simple test to see if advise and
;;;    unadvise work at all.
;;;    This is meant to be a verifier.  This should be run on other lisps.
;;;    This has been verified on a symbolics machine, lucid, and gmacs lisp.

(require 'cl)
(provide 'advise)

(defvar *all-advise* nil "a list of lists of function-symbol class and forms")

(defun member-equal (item list)
  "[cl] MEMBER ITEM LIST => Is ITEM in LIST?  Uses equal on the cars of LIST."
  (let ((ptr list)
        (done nil)
        (result '()))
    (while (not (or done (endp ptr)))
      (cond ((equal item (car ptr))
             (setq done t)
             (setq result ptr)))
      (setq ptr (cdr ptr)))
    result))

(defmacro advise (function-symbol class &rest forms)
  "Using this advise is like using the lispm advise.
  (advise foo :before (print 'this-is-before-foo-is-called))
  (advise foo :after (print 'this-is-after-foo-is-called))
  (advise foo :around (progn (print 'this-is-before-foo-is-called)
                             :do-it
                             (print 'this-is-after-foo-is-called))"
  (`
   (let ((descriptor-list '((, function-symbol) (, class) (, forms))))
     (unless (member-equal descriptor-list *all-advise*)
       (add-advise-internal '(, function-symbol) '(, class) '(, forms))
       (push descriptor-list *all-advise*)))))

(defun add-advise-internal (function class list-of-forms)
  "returns the new function"
  ;;this is for unadvise
  (let* ((old-function-cell (symbol-function function))
	 (autoload-file (and (listp old-function-cell)
			     (eq (car old-function-cell) 'autoload)
			     (cadr old-function-cell)))
	 )
    (if (not (get function ':original-function))
	(setf (get function ':original-function) old-function-cell))
    (setf (symbol-function function)
	  (cond (autoload-file
		 (` (lambda (&rest arglist)
		      (, (if (commandp function)
			     '(interactive)
			     nil))
		      (load (, autoload-file))
		      (add-advise-internal
		       '(, function) '(, class) '(, list-of-forms))
		      (if (interactive-p)
			  (call-interactively '(, function))
			  (apply '(, function) arglist)
			  ))))
		(t;; not autoload
		 (ecase class
		   (:after
		    (` (lambda (&rest arglist)
			 (, (if (commandp function)
				'(interactive)
				nil))
			 (let ((values
				(multiple-value-list 
				    (if (interactive-p)
					(call-interactively
					 '(, old-function-cell))
					(apply '(, old-function-cell) arglist))))
			       (progn 0);;this is here so that when
			;;;the bug in ` is fixed, this will still work.
			       )
			   (,@ list-of-forms)
			   progn;;this is here because of a bug in `
			   (values-list values)
			   ))))
		   (:before
		    (` (lambda (&rest arglist)
			 (, (if (commandp function)
				'(interactive)
				nil))
			 (,@ list-of-forms)
			 (if (interactive-p)
			     (call-interactively
			      '(, old-function-cell))
			     (apply '(, old-function-cell) arglist)))))
		   (:around
		    (` (lambda (&rest arglist)
			 (, (if (commandp function)
				'(interactive)
				nil))
			 (let (values 
			       (progn 0);;this is here so that
			       ;;when the bug in ` is fixed, this will still work.
			       )
			   (,@ (change-do-it-to-funcall 
				list-of-forms
				old-function-cell
				(interactive-p)))
			   progn;;this is here because of a bug in `
			   (values-list values)))))))))))

(defun change-do-it-to-funcall (list-of-forms old-function-cell
					      interactive-p)
  (cond ((eq list-of-forms ':do-it)
	 (` (setq values (multiple-value-list
			     (if (interactive-p)
				 (call-interactively
				   '(, old-function-cell))
				 (apply '(, old-function-cell) arglist))))))
	((atom list-of-forms) list-of-forms)
	(t (cons (change-do-it-to-funcall
		   (car list-of-forms) old-function-cell interactive-p)
		 (change-do-it-to-funcall
		   (cdr list-of-forms) old-function-cell interactive-p)))))


;;;***************************************************************************
;;; Unadvise
;;;***************************************************************************
;;;
;;; this does not find out if you recompiled the function since advising.
;;; This will restore the state of the function to before it was advised.
;;;  (this might want to be fixed)

(defun unadvise (function)
  "this removes all advise from the specified function."
  (unadvise-function function))

(defun unadvise-function (function)
  (cond ((and (not (assoc function *all-advise*))  ;;not on *all-advise*
	      (not (get function ':original-function)))
	 t ;;nothing to do
	 )
	((and (assoc function *all-advise*)  ;;on *all-advise*
	      (not (get function ':original-function)))
	 (delete-advise function)
	 (error "Function %s seemed to have been advised, but the original function was not saved.
Can not unadvise" function))
	((get function ':original-function)
	 (setf (symbol-function function) (get function ':original-function))
	 (setf (get function ':original-function) nil)
	 (delete-advise function))))

(defun delete-advise (function)
  (let ((new-list nil))
    (dolist (i *all-advise*)
      (if (not (eq function (car i)))
	  (push i new-list)))
    (setq *all-advise* (reverse new-list))))


(defun test-unadvise ()
  (unadvise 'test-unadvise-dummy)
  (defun test-unadvise-dummy (x) x)
  (if (not (eql 5 (test-unadvise-dummy 5)))
      (error "test-unadvise-dummy: %s didnt compile right" (test-unadvise-dummy 5)))
  (advise test-unadvise-dummy :after (setq values '(7)))
  (if (not (eql 7 (test-unadvise-dummy 5)))
      (error "test-unadvise-dummy: %s didnt advise right, should be 7" (test-unadvise-dummy 5)))
  (unadvise 'test-unadvise-dummy)
  (if (not (eql 5 (test-unadvise-dummy 5)))
      (error "test-unadvise-dummy: %s didnt unadvise right" (test-unadvise-dummy 5))))






