;;; Extensions to Lucid's source file recording capabilities.  These
;;; are meant for use with the Emacs code in cl-lucid.el, although
;;; they do not depend on that code in any way.  The converse is also
;;; true: the code in cl-lucid.el does not depend on this code in any
;;; way.

;;; We extend Lucid's source-file recording mechanism to allow Emacs
;;; to tell the user about different definitions and find them in
;;; source files.  We are particularly interested in extending to
;;; cover objects systems (Flavors and CLOS) and functions defined by
;;; side-effect (such as through defstruct or defclass). There are two
;;; ways to do this:

;;; 1) Use the type argument to record-source-file to store
;;; information which Emacs can use to determine a regexp to search
;;; for in the source file.  On the CL side, this is the simplest
;;; solution.  The loss is that it screws up redefinition warnings
;;; between different types of definition (eg: structure accessors and
;;; normal functions).  This is partly because lucid only compares the
;;; type slots with eq, so lists are not compared correctly.  Code
;;; could be written to check for the appropriate collisions, but this
;;; seems tricky.

;;; 2) Store auxilliary source-file information on the symbol plist,
;;; and provide a new function to access this.  Emacs will call this
;;; function instead of get-source-file.  The advantage is that this
;;; doesn't interfere with the existing source file recording
;;; mechanism.  The loss is that it requires additional CL code that
;;; Emacs must rely on, and it is hard to tie in the existing
;;; source-file list.

;;; We opt for the simplicity of the first approach.  To add
;;; source-file-recording for a new type of object/function, the user
;;; just calls record-source-file with a list.  To tell Emacs how to
;;; find definitions for this object/function, see instructions in
;;; cl-lucid.el.

#-:Lucid
(error "The code in this file depends on Lucid's source-file \
extensions to Common Lisp.")

(in-package 'lucid)

;;;; ----------------------- Common Lisp ------------------------

;;; If non-nil, this should be a list of pairs.  If the type arg to
;;; record-source-file matches the car of a pair, the cdr will be
;;; substituted.  This is used to ensure that definitions produced by
;;; side-effect (such as defstruct accessors) can be found by the
;;; editor.
(defvar *definition-type-replacements* nil)

;;; BUG: *** this should behave according to *redefinition-action*,
;;; giving warnings when necessary.  Also, the default
;;; record-source-file re-records source file if a new-type-pair is
;;; found (i.e. if the new type arg is not a symbol, the source file
;;; list is extended to contain the new def as well as the old).
(defadvice (lucid::record-source-file side-effect-extension)
    (object type &rest options)
  (let ((new-type-pair (assoc type *definition-type-replacements* :test #'equal))
	old-source-file-pair)
    (cond (new-type-pair
	   (setq old-source-file-pair (assoc (cdr new-type-pair)
					     (get-source-file object nil t)
					     :test #'equal))
	   (if (and old-source-file-pair
		    (equal (cdr old-source-file-pair) (car options)))
	       t			;don't re-record same source file
	       (apply-advice-continue object (cdr new-type-pair) options)))
	   (t (apply-advice-continue object type options)))))

;;; We set up defvar, defparameter, defconstant, and deftype to record
;;; source files with the type matching that used for the
;;; documentation function.  These were left out of Lucid 3.0, but
;;; they are already taken care of in Lucid 4.0.
#-LCL4.0
(defadvice (defvar record-source-file) (form &optional env)
  `(prog1
    ,(advice-continue form env)
    (record-source-file ',(second form) 'variable)))

#-LCL4.0
(defadvice (defconstant record-source-file) (form &optional env)
  `(prog1
    ,(advice-continue form env)
    (record-source-file ',(second form) 'variable)))

#-LCL4.0
(defadvice (defparameter record-source-file) (form &optional env)
  `(prog1
     ,(advice-continue form env)
     (record-source-file ',(second form) 'variable)))

;;; Put in source file recording for types.
(defadvice (deftype record-source-file) (form &optional env)
  `(prog1
    ,(macroexpand (advice-continue form env))
    (record-source-file ',(second form) 'type)))

;;; Alter defstruct to store source file for constructor and slot
;;; accessors, as well as the structure itself.  Ordinarily, Lucid
;;; would record source file info for constructor and slot accessors.
;;; But in order for the editor to find the definitions, we need to
;;; indicate that it should look for a defstruct in the source file.
;;; We do this by replacing the 'function type argument to
;;; record-source-file with the list '(:struct-function <struct-name>).
(defadvice (defstruct record-source-file) (form &optional env)
  (destructuring-bind (defstruct name-and-options &body slot-descriptions) form
    (declare (ignore defstruct slot-descriptions))
    (let* ((name (if (listp name-and-options)
		   (first name-and-options)
		   name-and-options)))
      `(let ((*definition-type-replacements*
	      '((function . (:struct-function ,name)))))
	,(advice-continue form env)))))

;;; Unfortunately, have to do this to prevent redefinition warning
;;; messages from breaking when the type is a list.  This problem has
;;; been fixed in Lucid 4.0!
#-LCL4.0
(defadvice (lisp:string list-extension) (thing)
  (when (listp thing) (setq thing (format nil "~S" thing)))
  (advice-continue thing))
	     
;;;; ----------------------- PCL -------------------------

;;; *** Need to add a pair to the *definition-type-replacements* list for
;;; accessors!
#+:PCL
(defadvice (pcl:defclass record-source-file) (form &optional env)
  `(progn
    (record-source-file ',(cadr form) ':class)
    ,(advice-continue form env)))

;;; We record the type arg of a pcl method as '(:method . arg-classes)
#+:PCL
(defadvice (pcl:defmethod record-source-file) (form &optional env)
  (destructuring-bind (defmethod name qualifiers arglist &body body) form
    (declare (ignore defmethod body))
    (let ((arg-classes (loop for arg in (if (listp qualifiers) qualifiers arglist)
			     until (not (listp arg))
			     collect (cadr arg))))
      `(progn
	(record-source-file ',name ',(cons ':method arg-classes))
	,(advice-continue form env)))))

;;;; --------------------- Flavors --------------------------

#+:FLAVORS
(defadvice (flavors:defflavor record-source-file) (form &optional env)
  `(progn
     (record-source-file ',(second form) :flavor *source-pathname*)
     ,(advice-continue form env)))

;;; We record the type arg of a flavors method as '(:method <flavor> <type>)
#+:FLAVORS
(defadvice (flavors:defmethod record-source-file) (form &optional env)
  (destructuring-bind (defmethod (flavor type &optional method) &body body) form
    (declare (ignore defmethod body))
    (when (null method)			;type may be omitted, defaults to :primary
      (setq method type 
	    type :primary))
    `(progn
       (record-source-file ',method '(:method ,flavor ,type))
      ,(advice-continue form env))))

