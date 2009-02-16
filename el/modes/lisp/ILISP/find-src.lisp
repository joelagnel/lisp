;;; -*- Mode: Lisp -*-
;;;
;;; find-src.lisp --
;;; ILISP source location support definitions for Common Lisp dialects.
;;; New version of the ilisp:source-file function (and friends).
;;; (Based on cmulisp.lisp for the CMU Common Lisp dialect, by Todd Kaufmann,
;;; May 1990; rewritten and extended by Bob Rogers July 2002 through March
;;; 2003.)
;;;
;;; This file is part of ILISP.
;;; Please refer to the file COPYING for copyrights and licensing information.
;;; Please refer to the file ACKNOWLEGDEMENTS for an (incomplete) list of
;;; present and past contributors.
;;;
;;; $Id: find-src.lisp,v 1.4 2003/11/27 03:40:29 rgrjr Exp $

(in-package :ilisp)

(defvar *known-definition-types* '(:function :setf :structure
				   #-(or cmu sbcl) :variable
				   #-(and cmu (not pcl)) :class)
  "The order of these tends to control what you get when you ask for them all.")
(defvar *function-spec-result* nil
  "accumulating function spec result.  [it would be nice to make this
obsolete.  -- rgr, 4-Sep-02.]")

;;; Low-level support.

(defun ilisp-find-class (class-name &optional (error-p t) environment)
  ;; it's a bit tacky to do all this at runtime, but on the other hand, cmucl
  ;; can be built without pcl, and this allows ilisp to work either way, without
  ;; having to recompile this file.  [except that this file depends on PCL in
  ;; other ways.  -- rgr, 12-Sep-02.]
  (let ((finder (cond ((find-package :pcl)
			 (intern (string :find-class) :pcl))
		      ((find-package :sb-pcl)
			 (intern (string :find-class) :sb-pcl))
		      (t
			'find-class))))
    (when (fboundp finder)
      (funcall finder class-name error-p environment))))

;;; Patch.

;; [not necessary in cmucl 18e and 19a, but probably in 18d and earlier.  --
;; rgr, 26-Nov-03.]
#+(and cmu (not cmu18e) (not cmu19) (not cmu20))
(defmethod class-name ((class structure-class))
  (slot-value class 'pcl::name))

;;; Definition specifications.

#+allegro
(defun canonicalize-function-spec-and-type (function-spec type)
  ;; Given a Lisp-specific function spec and type, convert them into something
  ;; the emacs code is likely to be able to understand and find.  This is
  ;; currently only necessary for Allegro, which has its own notion of type
  ;; names.  (According to
  ;; file:/usr/local/acl61/doc/pages/operators/excl/record-source-file.htm, ACL
  ;; also has a :special-declaration type, but I haven't observed this in
  ;; practice.  The :variable type is handled compatibly, and the others
  ;; (:operator and :setf-method) are dealt with at a higher level.  -- rgr,
  ;; 16-Sep-02.)
  (case type
    ;; Try to refine the definition type.
    (:type (cond ((or (not (symbolp function-spec))
		      (null function-spec)))
		 ((subtypep function-spec 'condition)
		   (setq type 'define-condition))
		 ((get function-spec 'excl::%structure-definition)
		   (setq type :structure))
		 ((find-class function-spec nil)
		   (setq type :class))))
    (:setf-method
      ;; canonicalize the name/type for this.
      (setq type :function)
      (setq function-spec (list 'setf function-spec)))
    (:compiler-macro (setq type 'define-compiler-macro)))
  (values function-spec type))

(defun make-definition-specification (function-spec type sources)
  ;; Central definition-spec maker.  Should also do type canonicalization here.
  ;; Note that we never fail to report source files just because we can't find
  ;; them; emacs may have a better time of it.
  (let ((result (list :definition)))
    ;; name/type canonicalization.
    #+allegro
    (multiple-value-setq (function-spec type)
      (canonicalize-function-spec-and-type function-spec type))
    ;; source file name canonicalization.
    (when sources
      (unless (listp sources)
	(setq sources (list sources)))
      (when (member :top-level sources)
	;; acl tells us this, but other lisps might usefully return this, too.
	(setq sources (delete :top-level sources))
	(push (format nil "~S is ~A via the Lisp top-level."
		      function-spec
		      (if sources "also defined" "defined only"))
	      (getf (cdr result) :comment)))
      (when sources
	(setf (getf (cdr result) :files)
	      (mapcar #'(lambda (source-file)
			  (handler-case
			      (namestring (truename source-file))
			    (file-error ()
			      (if (stringp source-file)
				  source-file
				  (namestring source-file)))))
		      sources))))
    ;; stuff the results.  (waiting until now puts :name & :type before :files.)
    (when type
      (setf (getf (cdr result) :type) type))
    (when function-spec
      (setf (getf (cdr result) :name) function-spec))
    result))

(defun definition-spec-name (spec) (getf (cdr spec) :name))
(defun definition-spec-type (spec) (getf (cdr spec) :type))

(defun maybe-add-definition-spec (spec)
  (when spec
    (push spec *function-spec-result*)
    spec))

(defun add-definition-source-files (function-spec type sources)
  ;; note that we always create a definition spec to indicate that the
  ;; definition was found, even if there are no sources.
  (maybe-add-definition-spec
    (make-definition-specification function-spec type sources)))

(defun push-format-comment (format-string &rest format-args)
  (push (list :comment
	      (apply #'format nil format-string format-args))
	*function-spec-result*)
  nil)

(defun ilisp-find-function-spec (string package-name)
  ;; function specs are normally symbols, but also include method specs and setf
  ;; functions.
  (let ((package (ilisp-find-package package-name :error-p nil)))
    (cond ((null package)
	    (push-format-comment "Can't find package ~S." package-name)
	    nil)
	  ((zerop (length string))
	    (push-format-comment "Oops; zero-length function name ~S." string)
	    nil)
	  ((some #'(lambda (funky-char) (find funky-char string))
		 '(#\( #\|))
	    ;; nontrivial syntax, e.g. "(method foo (bar))" or "|funky case|".
	    ;; this might generate a read error, though.
	    (let ((*package* package))
	      (read-from-string string)))
	  ((find-symbol (ilisp-symbol-name string) package))
	  (t
	    (push-format-comment "No symbol ~S available in package ~S."
				 string (package-name package))
	    nil))))

;;; source-file

;; Based on the "new version" by Richard Harris <rharris@chestnut.com> with
;; suggestions by Larry Hunter <hunter@work.nlm.nih.gov>, circa 21 Nov 1994.

#+(or pcl sbcl)
(defmethod object-source-file ((object definition-source-mixin))
  ;; returns the source file (if any) associated with the metaobject.  (the
  ;; assertion is because i don't understand the general format of this well
  ;; enough.)
  (let ((source (definition-source object)))
    (when source
      (when (consp source)
	(assert (= (length source) 2)))
      (multiple-value-bind (file definition-name)
	  (if (consp source)
	      (values (second source) (first source))
	      ;; [sbcl seems to have simplified this, at least for
	      ;; structure-class objects.  -- rgr, 25-Aug-02.]
	      (values source nil))
	(assert (pathnamep file))
	(when (eq (car definition-name) 'defmethod)
	  (setq definition-name (cons 'method (cdr definition-name))))
	(when (or #-sbcl (member (pathname-type file :case :common)
				 lisp::*load-lp-object-types*
				 :test #'string=)
		  #+sbcl (string= (pathname-type file)
				  sb-fasl:*fasl-file-type*))
	  ;; oops; this is actually the pathname of the binary; try to find a
	  ;; suitable lisp source file, but don't fail if we can't.
	  (dolist (type '("LISP" "CL" "LSP" "L"))
	    (let ((source (probe-file
			    (make-pathname :type type
					   :defaults file :case :common))))
	      (when source
		(setq file source)
		(return t)))))
	(values file definition-name)))))

#+(or pcl sbcl)
(defmethod object-source-file :around ((object standard-accessor-method))
  (with-slots (lambda-list generic-function) object
    (values (call-next-method)
	    ;; return a "normal" method name.
	    (list 'method
		  (generic-function-name generic-function)
		  lambda-list))))

#+(or pcl sbcl)
(defmethod object-source-file :around ((object standard-generic-function))
  ;; [PCL doesn't tell us directly whether an explicit defgeneric form exists
  ;; (there always seems to be a source file defined), but I'm not sure if the
  ;; hack below is right.  -- rgr, 5-Sep-02.]
  (unless (eq (arg-info-lambda-list (slot-value object 'arg-info))
	      :no-lambda-list)
    (call-next-method)))

#+allegro
(defmethod specializer-arg-spec ((specializer aclmop:eql-specializer))
  (list 'eql (mop:eql-specializer-object specializer)))

#+allegro
(defmethod specializer-arg-spec ((specializer class))
  (class-name specializer))

#+allegro
(defmethod function-spec ((method standard-method))
  (append (list 'method
		(generic-function-name (method-generic-function method)))
	  (method-qualifiers method)
	  (list (mapcar #'specializer-arg-spec (method-specializers method)))))

#+allegro
(defmethod function-spec ((gf standard-generic-function))
  (generic-function-name gf))

#+allegro
(defmethod object-source-file ((object metaobject))
  ;; returns the source file (if any) associated with the metaobject.
  (let ((definition-name (function-spec object)) (pathname nil))
    (handler-case
	(progn
	  (setq pathname (excl:source-file definition-name :function))
	  (values (and pathname
		       (truename pathname))
		  definition-name))
      (error ()
	(values pathname definition-name)))))

#+allegro
(defmethod object-source-file ((object class))
  ;; returns the source file (if any) associated with the generic function.
  (let ((definition-name (slot-value object 'excl::name)) (pathname nil))
    (handler-case
	(progn
	  (setq pathname (excl:source-file definition-name :type))
	  (values (and pathname
		       (truename pathname))
		  definition-name))
      (error ()
	(values pathname definition-name)))))

#+allegro
(defmethod object-source-file ((object standard-generic-function))
  ;; Returns the source file (if any) associated with the metaobject.
  ;; [***kludge***: unfortunately, there is no way we can tell in ACL whether a
  ;; generic function has an explicit defgeneric form.  so, we look for
  ;; documentation; if there's documentation, there must be an explicit
  ;; defgeneric.  (we have to examine the excl::plist slot directly, as the
  ;; documentation function can't handle (setf foo) GFs, and sometimes fails
  ;; even on GFs with ordinary symbolic names.)  of course, this fails for
  ;; defgeneric forms that have no documentation.  -- rgr, 9-Sep-02.]
  (with-slots (excl::plist) object
    (when (getf excl::plist 'documentation)
      (call-next-method))))

(defmethod add-method-pathnames ((method standard-method))
  (multiple-value-bind (source method-spec) (object-source-file method)
    (add-definition-source-files method-spec :function source)))

(defmethod add-method-pathnames :around ((method standard-accessor-method))
  ;; these are always defined within the parent defclass form, so we want to
  ;; record that fact.
  (let ((result (call-next-method)))
    (when result
      ;; ["(car (last (method-specializers method)))" is something of a hack,
      ;; but it works for writers as well as readers.  -- rgr, 18-Sep-02.]
      (let ((class (car (last (method-specializers method)))))
	(setf (getf (cdr result) :parent)
	      (make-definition-specification (class-name class) :class
					     (object-source-file class)))))
    result))

(defun find-method-from-spec (method-spec)
  (let ((gf-name (second method-spec)))
    (when (fboundp gf-name)
      (let ((rev-tail (reverse (cddr method-spec))))
	(assert (and (consp rev-tail)
		     (listp (car rev-tail))
		     (every #'keywordp (cdr rev-tail))))
	(find-method (fdefinition gf-name)
		     (nreverse (cdr rev-tail))
		     (mapcar #'ilisp-find-class (car rev-tail))
		     nil)))))

(defmethod add-method-pathnames ((method-spec list))
  ;; given a method spec, e.g. (method foo :after (bar t)), add its source
  ;; files.
  (let ((method (find-method-from-spec method-spec)))
    (when method
      (add-method-pathnames method))))

#+allegro
(defun maybe-annotate-with-structure-parent (definition-spec)
  (let* ((name (definition-spec-name definition-spec))
	 (setf-inverse (cond ((symbolp name)
			       (get name 'excl::setf-inverse))
			     ((eq (first name) 'setf)
			       (get (second name) 'excl::setf-inverse)))))
    (when (and (consp setf-inverse)
	       (eq (car setf-inverse) 'excl::defstruct-slot-defsetf-handler))
      ;; it is also true that (equal (arglist function) '(excl::struct)), but
      ;; unfortunately, i can't figure out how to get the structure name -- or
      ;; any other useful information, for that matter.  so we can't do any
      ;; better than getting the :operator source file name.
      (setf (getf (cdr definition-spec) :parent)
	    (make-definition-specification nil :structure nil)))
    definition-spec))

#+cmu
(defun defstruct-slot-accessor-info (fun)
  ;; [based on the lisp::encapsulation-info fn.  -- rgr, 11-Sep-02.]  [however,
  ;; note that (ext:info function lisp::accessor-for fn-name) also returns the
  ;; structure class in CMUCL.  -- rgr, 15-Sep-02.]
  (if (and (functionp fun)
	   (= (lisp::get-type fun) vm:closure-header-type))
      (values (system:find-if-in-closure
	        #'(lambda (x) (typep x 'structure-class))
		fun)
	      (system:find-if-in-closure
	        #'(lambda (x) (typep x 'kernel:defstruct-slot-description))
		fun))))

#+sbcl
(defun defstruct-slot-accessor-info (fun)
  ;; [based on the lisp::encapsulation-info fn.  -- rgr, 11-Sep-02.]  [however,
  ;; note that (ext:info function lisp::accessor-for fn-name) also returns the
  ;; structure class in CMUCL.  -- rgr, 15-Sep-02.]
  (when (and (functionp fun)
	     (= (the-function-if-defined ((#:widetag-of :sb-impl)
					  (#:get-type :sb-impl)) fun)
		;; <3>
		#.(the-symbol-if-defined
		   ((#:closure-header-widetag :sb-vm)
		    (#:closure-header-type :sb-vm) :eval-p t)))
	     (not (the-function-if-defined
		   ((#:interpreted-function-p :sb-eval) ()) fun)))
    (let ((layout (sb-impl::find-if-in-closure #'sb-kernel::layout-p fun)))
      (when layout
	(let ((info (sb-kernel::layout-info layout))
	      (class #+ignore (slot-value layout 'class)
		     (slot-value (slot-value layout 'class)
				 'sb-kernel::pcl-class)))
	  (values class nil info))))))

;; add-defined-functions takes a symbol or function object.  it
;; returns a pathname for the file the function was defined in.  if it was
;; not defined in some file, then nil is returned.
;;
;; add-defined-functions is from hemlock/rompsite.lisp (cmucl17f), 
;; with added read-time conditionalization to work in older versions
;; of cmucl.  it may need a little bit more conditionalization for
;; some older versions of cmucl.
;;
;; [this was originally the fun-defined-from-pathname fn.  -- rgr, 31-jul-02.]

(defun add-defined-functions (function &optional name)
  "returns the file where function is defined in (if the file can be found).
takes a symbol or function and returns the pathname for the file the
function was defined in.  if it was not defined in some file, nil is
returned."
  (labels (#+(or cmu sbcl)
	   (extract-code (function)
	     ;; given a function, return the code object.  [i barely know what
	     ;; all this means.  -- rgr, 11-Sep-02.]
	     (the-function-if-defined
	         ((#:fun-code-header #+sbcl :sb-kernel #-sbcl :kernel)
		  (#:function-code-header #+sbcl :sb-kernel #-sbcl :kernel))
	       (the-function-if-defined
		   ((#:%simple-fun-self #+sbcl :sb-kernel #-sbcl :kernel)
		    (#:%function-self #+sbcl :sb-kernel #-sbcl :kernel))
		 function)))
	   #+(or cmu sbcl)
	   (extract-sources (code)
	     (let* ((info #+sbcl (sb-kernel:%code-debug-info code)
			  #+cmu
			  (the-function-if-defined ((#:%code-debug-info
						     :kernel)
						    (#:code-debug-info
						     :kernel))
						   code))
		    (sources (and info
				  #+cmu (c::debug-info-source info)
				  #+sbcl (sb-c::debug-info-source info)))
		    (source-names nil))
	       (dolist (source sources)
		 (let ((name #+cmu (c::debug-source-name source)
			     #+sbcl (sb-c::debug-source-name source)))
		   (case #+cmu (c::debug-source-from source)
			 #+sbcl (sb-c::debug-source-from source)
		     (:file
		       (push name source-names))
		     ;; this accesses the c::source-info data installed by the
		     ;; ilisp-compile fn.
		     #-sbcl
		     (:stream
		       (when #+cmu (c::debug-source-info source)
			     #+sbcl (sb-c::debug-source-info source)
			 (let ((file-info
				 (first #+sbcl
					(sb-c::source-info-files
					  (sb-c::debug-source-info source))
					#+cmu
					(c::source-info-files
					  (c::debug-source-info source)))))
			   (when file-info
			     (push #+cmu (c::file-info-name file-info)
				   #+sbcl (sb-c::file-info-name file-info)
				   source-names))))))))
	       (values source-names sources)))
	   #+(or cmu sbcl)
	   (frob (code)
	     (multiple-value-bind (source-names sources)
		 (extract-sources code)
	       (let* ((name (cond (name)
				  ((or (symbolp function) (listp function))
				    function)))
		      (type (if (and (symbolp name)
				     (macro-function name))
				:macro
				:function))
		      (defn (make-definition-specification name type
							   source-names)))
		 (when (and sources (null source-names))
		   (push (format nil "~S was defined via the top level." name)
			 (getf (cdr defn) :comment)))
		 (maybe-add-definition-spec defn)))))
    (typecase function
      ((or symbol cons)
        (let ((fn (extract-function-info-from-name function)))
	  (when fn
	    (add-defined-functions fn function))))
      ;; normal function object types.
      #+cmu	;; [sbcl doesn't have byte functions.  -- rgr, 25-Aug-02.]
      (#.(the-symbol-if-defined ((#:byte-closure #+sbcl :sb-kernel
						 #-sbcl :kernel) ()))
	 (let ((fn (the-function-if-defined
		    ((#:byte-closure-function #+sbcl :sb-kernel
					      #-sbcl :kernel) ()
		     :function-binding-p t)
		    (funcall the-function function))))
	   (add-defined-functions fn name)))
      #+cmu	;; [sbcl doesn't have byte functions.  -- rgr, 25-Aug-02.]
      (#.(the-symbol-if-defined ((#:byte-function :kernel) ()))
	 (frob (c::byte-function-component function)))
      #+cmu	;; [sbcl doesn't have interpreted fns.  -- rgr, 25-Aug-02.]
      (eval:interpreted-function
	;; This hack is necessary because CMUCL does not
	;; correctly record source file information when 'loading'
	;; a non compiled file.
	;; In this case we fall back on the TAGS machinery.
	;; (At least as I understand the code).
	;; Marco Antoniotti 11/22/94.
	(push-format-comment "~S is an interpreted function, ~
			      for which source information is not kept."
			     name))
      (generic-function
        ;; When we find a generic function, we include the defgeneric itself (if
        ;; it exists), followed by all methods.  Often, there is no defgeneric
        ;; and only one method, in which case this allows M-. to work for the
        ;; single defmethod form as if it were a defun.
        (let ((methods (generic-function-methods function)))
	  (push-format-comment "~S is a generic function with ~D method~:P."
			       name (length methods))
	  (let ((source (object-source-file function)))
	    ;; We don't want to include a definition spec for the GF unless the
	    ;; source has an explicit defgeneric form.
	    (when source
	      ;; strictly, the generic function is still a function, but we get
	      ;; a better regular expression if we tell emacs that it is a
	      ;; :generic-function instead.
	      (add-definition-source-files name :generic-function source))
	    (dolist (method methods)
	      (add-method-pathnames method)))))
      #+allegro
      (excl::closure
        (maybe-annotate-with-structure-parent
	  (add-definition-source-files name :function
				       (excl:source-file name :operator))))
      (function
        #+(or cmu sbcl)
	(let ((defn (frob (extract-code function))))
	  (multiple-value-bind (structure-class #-sbcl slot)
	      (defstruct-slot-accessor-info function)
	    (when structure-class
	      ;; defstruct slot accessor function.  [for sbcl, this means that
	      ;; the :files we computed above are bogus.  -- rgr, 20-Sep-02.]
	      (let* ((class-name (class-name structure-class))
		     #-sbcl
		     (constructor
		       (kernel::structure-class-constructor structure-class))
		     (sources
		       ;; [now, why don't we want to do this for CMUCL as well?
		       ;; -- rgr, 30-Mar-03.]
		       #+sbcl (object-source-file structure-class)
		       #-sbcl
		       (and constructor
			    (extract-sources (extract-code constructor)))))
		#-sbcl (push (format nil "~S accesses the ~S slot ~
					 (number ~D) of ~S."
				     name
				     (kernel::dsd-%name slot)
				     (kernel::dsd-index slot)
				     class-name)
			     (getf (cdr defn) :comment))
		#+sbcl (push (format nil "~S accesses a slot of ~S."
				     name class-name)
			     (getf (cdr defn) :comment))
		;; the child's file will be src/code/target-defstruct.lisp,
		;; since it just finds the underlying accessor creator.
		#+sbcl (remf (cdr defn) :files)
		(setf (getf (cdr defn) :parent)
		      (make-definition-specification class-name :structure
						     sources)))))
	  defn)
	#+allegro
	(add-definition-source-files name :function
				     (handler-case
					 (excl:source-file function :operator)
				       (error ()
					 nil))))
      (t
        ;; this should make it obvious we missed something.
        (push-format-comment "Don't know how to find ~
				the source file of ~S as a function."
			     name)))))

(defun make-structure-or-class-definition-spec (class-name type)
  ;; [lumping classes and structures together makes this depend too heavily on
  ;; CLOS.  -- rgr, 2-Sep-02.]
  (let ((class (ilisp-find-class class-name nil)))
    (when (and class
	       ;; the alternatives would be structure-class and :structure.
	       (eq (typep class 'standard-class)
		   (eq type :class)))
      (make-definition-specification class-name type
				     (object-source-file class)))))

(defun find-available-source-info (function-spec definition-type)
  ;; function-spec will be a symbol or list, definition-type a keyword symbol.
  (labels (#+allegro
	   (try-excl-source-file (function-spec type)
	     (assert (not (eq definition-type :any)))
	     (let* ((acl-type
		      ;; functions and macros are handled at a higher level.
		      ;; [kludge: this could be generalized better.  -- rgr,
		      ;; 15-Sep-02.]
		      (case type
			((:class :structure define-condition) :type)
			(define-compiler-macro :compiler-macro)
			(t type)))
		    (source
		      (handler-case
			  (excl:source-file function-spec acl-type)
			(error ()
			  nil)))
		    (defn (make-definition-specification function-spec acl-type
							 source)))
	       ;; Don't confuse classes and structures.
	       (when (or (eq (definition-spec-type defn) type)
			 (not (member definition-type '(:structure :class))))
		 (maybe-annotate-with-structure-parent defn)
		 (maybe-add-definition-spec defn))))
	   (one-type (function-spec type)
	     ;; this returns a definition descriptor if found, else nil.
	     (case type
	       (:function
		 (typecase function-spec
		   (cons
		     (case (car function-spec)
		       (method
			 (add-method-pathnames function-spec))
		       (t
			 (if (string= (car function-spec) :fast-method)
			     ;; [assume this is pcl::fast-method or
			     ;; sb-pcl::fast-method.  -- rgr, 25-Aug-02.]
			     ;; [still don't import those symbols so that they
			     ;; continue to print with package prefixes.  --
			     ;; rgr, 12-Sep-02.]
			     (add-method-pathnames function-spec)
			     ;; otherwise, see if fdefinition can figure it out.
			     (add-defined-functions function-spec)))))
		   (symbol
		     (add-defined-functions function-spec))
		   (t
		     (push-format-comment "Unknown function spec ~S."
					  function-spec))))
	       #+(or pcl sbcl)
	       ((:class :structure)
		 (when (symbolp function-spec)
		   (let ((spec (make-structure-or-class-definition-spec
				 function-spec type)))
		     (when spec
		       ;; if function-spec name a structure, it will also have
		       ;; an associated structure-class, and we'll get two
		       ;; :structure entries if we don't check for duplicates.
		       (unless (find (definition-spec-type spec)
				     *function-spec-result*
				     :key #'(lambda (spec)
					      (and (eq (car spec) :definition)
						   (definition-spec-type
						      spec))))
			 (maybe-add-definition-spec spec))))))
	       #-allegro
	       (:setf
		 (when (symbolp function-spec)
		   (one-type (list 'setf function-spec) :function)))
	       (t
		 ;; [this results in divergent behavior for ACL, but that's only
		 ;; because ACL allows application programmers to extend the set
		 ;; of definition types.  -- rgr, 4-Sep-02.]
		 #+allegro (try-excl-source-file function-spec type)))))
    (case definition-type
      (:any
	#-allegro
	(dolist (known-type *known-definition-types*)
	  (one-type function-spec known-type))
	#+allegro
	(let ((type-alist nil) (need-operator? nil)
	      (known-sources (excl:source-file function-spec t)))
	  (when (and (null (cdr known-sources))
		     (eq (caar known-sources) :type)
		     (fboundp function-spec))
	    ;; [kludge around the fact that acl misrecords slot accessors as
	    ;; :type's when you ask for (excl:source-file function-spec t).  the
	    ;; :operator is actually what's defined; you get an error if you ask
	    ;; for a :type definition.  need to open the bandwidth more so we
	    ;; can use this as a function-parent hint in the search phase.  --
	    ;; rgr, 2-Sep-02.]  [bug: this hack doesn't find the accessor when
	    ;; function-spec is also defined as something else.  it's also
	    ;; hopeless when function-spec is also defined as a class in a
	    ;; different while.  -- rgr, 16-Sep-02.]
	    (setq known-sources nil)
	    (setq need-operator? t))
	  ;; refactor the (type . pathname) returned by excl:source-file when
	  ;; given a type arg of T into (type &rest pathnames), with at most one
	  ;; entry for each type.
	  (dolist (pair known-sources)
	    (destructuring-bind (type . pathname) pair
	      (case type
		(:operator
		  (setq need-operator? t))
		(t
		  (let ((pair (assoc type type-alist)))
		    (unless pair
		      (setq pair (list type))
		      (push pair type-alist))
		    (pushnew pathname (cdr pair) :test #'equalp))))))
	  ;; now we can use the add-definition-source-files interface.  [the
	  ;; maybe-annotate-with-structure-parent call is a kludge here because
	  ;; we can't use the ordinary add-defined-functions interface with
	  ;; (setf foo) specs.  -- rgr, 4-Sep-02.]
	  (dolist (entry type-alist)
	    (destructuring-bind (type &rest pathnames) entry
	      (if (eq type :type)
		  ;; [***kludge***: in acl, reader/writer/accessor methods are
		  ;; inccorrectly recorded as :type definitions.  so if there is
		  ;; no such type we get an error; if there is, we get the wrong
		  ;; file.  -- rgr, 8-Oct-02.]
		  (handler-case
		      (maybe-annotate-with-structure-parent
		        (add-definition-source-files function-spec type
						     pathnames))
		    (error nil))
		  (maybe-annotate-with-structure-parent
		    (add-definition-source-files function-spec type
						 pathnames)))))
	  ;; functions must be handled specially, as excl:source-file will give
	  ;; us a whole pile of pathnames for GF methods, but won't tell us the
	  ;; method specs.
	  (when need-operator?
	    ;; check this first so it shows up afterwards.
	    (one-type function-spec :function)
	    ;; [strangely, (setf foo) generic functions are not handled
	    ;; consistently.  -- rgr, 15-Sep-02.]
	    (when (and (symbolp function-spec)
		       (not (assoc :setf-method type-alist)))
	      (one-type (list 'setf function-spec) :function)))))
      (t
	(one-type function-spec definition-type)))))

(defun source-file-internal (symbol package type)
  ;; this is a separate function without the error wrapper for debugging.
  (let* ((*function-spec-result* nil)
	 (function-spec (ilisp-find-function-spec symbol package))
	 (type (etypecase type
		 (symbol type)
		 (string (if (find #\: type)
			     (read-from-string type)
			     (intern (ilisp-symbol-name type) :keyword)))))
	 ;; this sometimes puts line breaks into messages if enabled.
	 #+(or ansi allegro) (*print-pretty* nil)
	 ;; make sure we get it all!
	 (*print-level* nil) (*print-length* nil)
	 ;; bind *package* so that all symbols are printed with a suitable
	 ;; prefix.  [do this *after* the ilisp-find-function-spec call and the
	 ;; type read-from-string call so that it inherits the real current
	 ;; package.  -- rgr, 15-Sep-02.]
	 (*package* (find-package :ilisp)))
    (when function-spec
      (find-available-source-info function-spec type))
    (when (null *function-spec-result*)
      (push-format-comment "No~@[ ~A~] definitions of ~A."
			   (and (not (eq type :any))
				(string-downcase type))
			   function-spec))
    (print (nreverse *function-spec-result*))
    ;; indicate success.
    t))

(defun source-file (symbol package type)
  "Public interface for finding definition source file information."
  (ilisp-errors
    (source-file-internal symbol package type)))

;;; end of file -- find-src.lisp --
