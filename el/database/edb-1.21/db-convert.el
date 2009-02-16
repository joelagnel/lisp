;;; db-convert.el --- part of EDB, the Emacs database

;; See database.el for copyright notice, distribution conditions, etc.

;; Author: Michael Ernst <mernst@theory.lcs.mit.edu>
;; Keywords: EDB

;;; Commentary:

;; Functions for changing the field structure of a database.

;; This takes a database and rearranges the field order, converts fields
;; from one type to another, adds or removes fields, etc.  There are two
;; parts to the following code:  the user interface and the actual
;; conversion.

;;; Code:


;;; Variables used dynamically; avoid compiler messages about free variables.
;; Should this be?  Perhaps get rid of it.
(defvar computed-functions)


(provide 'db-convert)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Creation methods
;;;

;; To create a new database from an existing one, the user must specify a
;; creation method for each field of the new database.  This can be done
;; from a program by supplying a fieldnames-creation list, which is a list
;; of two-element lists of (fieldname creation-method).  It can also be
;; done interactively.

;; A creation method (for field F, say) is one of the following:
;; * A field reference.  Field F is set to the value of the specified field
;;   in the corresponding old record.
;; * A literal value.  Field F of every record is set to that value.
;; * Result of a function call.  The user must specify the function, what
;;   arguments should be passed to it, and which of the results is desired.
;;   Each argument specifier (one for each argument accepted by the
;;   function) specifies a field of the old record, the entire old record,
;;   or a literal.  The result specifier indicates that the function's
;;   result should be used as is or that the function returns a list, one
;;   element of which should be used as the value for field F.  This
;;   permits one function to do the computation for several fields in the
;;   new record; since the results are memoized, the function need only be
;;   called once per record.  For instance, suppose the original database
;;   stored addresses as a single field, but the new one stored street,
;;   city, state, and ZIP code separately.  The parsing of the address
;;   would only have to be done once.

;;; Represenation:
;; * Field reference:  a number (the field number in the old record) or a
;;   symbol (the field name in the old database).
;; * Literal value:  list of the symbol 'literal and the value; for
;;   strings, just the string itself.  Nil means the empty string.
;; * Function call:  The function is a symbol whose function cell should be
;;   bound.  Each argument specifier is a fieldnumber (or -1, for the
;;   entire record) or fieldname (or 'original-record, for the entire
;;   record), or a literal as above.  The result specifier is nil (for the
;;   entire result) or an integer (in which case the value of interest is
;;   obtained via (nth result-number result).)  (Should this be displayed
;;   in base 1 for the naive user's benefit?  No, because the naive user
;;   will be told by the function writer exactly which argument to
;;   specify.)  The full specification is a list of two elements, the first
;;   of which is a list of the function and the argument specifiers and the
;;   second of which is the result number.  Just the first element -- the
;;   list of the function and argument specifiers -- may be used if the
;;   result number is nil.

;;; Canonical representation:
;; The user is encouraged to use the fieldname representations, because
;; they are easier to read and write, but internally those are all
;; converted to the canonical representation, which is always the first one
;; listed in the descriptions above.  Thus, each creation method is a
;; number, a list whose car is 'literal, or a function specification.  This
;; permits the code which manipulates creation methods internally to be
;; more efficient.


(defsubst creation-method-literal-p (creation-method)
  (and (consp creation-method)
       (eq (car creation-method) 'literal)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; User interface
;;;

;; The screen is split into two columns (not necessarily two windows), with
;; the old database structure on the left and the new database structure on
;; the right.  From each record in the original database, one record is
;; created in the new database; by default, the new record is identical to
;; the old.

;; The user interface will be most useful for small changes, like adding a
;; single field; for major changes, I suspect that users will construct
;; fieldname-creation lists by hand.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Conversion
;;;

;; Currently this doesn't create a new database; it destructively changes
;; the format of the old one.  I suspect this is the desired behavior.

;;;###autoload
(defun db-convert (database fieldnames-creation)
  "Convert DATABASE's field structure according to FIELDNAMES-CREATION.
This function rearranges the field order, converts fields from one type to
another, adds or removes fields, and so forth.

FIELDNAMES-CREATION is a list of two-element lists of \(fieldnames
creation-method\); if creation-method would be nil, then fieldname alone
may be used in place of the two-element list."
  (let ((cmethods (db-canonicalize-creation-methods
		   (mapcar (function (lambda (fieldname-creation)
				       (if (symbolp fieldname-creation)
					   ""
					 (car (cdr fieldname-creation)))))
			   fieldnames-creation)
		   database))
	(no-of-new-fields (length fieldnames-creation)))
    (maplinks-macro
     (link-set-record maplinks-link
		      (db-convert-record (link-record maplinks-link)
					 cmethods no-of-new-fields))
     database)

    ;; Change like fieldlist, accessor functions, etc.

    ;; changed from database-set-fieldnames per Henry Thompson
    (db-set-fieldname-vars database
			   (mapcar (function car) fieldnames-creation))))

;; Create and return a new record from RECORD using CREATION-METHODS, a list
;; of canonical creation methods.  Optional argument NO-OF-FIELDS gives the
;; length of CREATION-METHODS and the number of fields in the result record.
;; Dynamically binds computed-functions.
(defun db-convert-record (record creation-methods &optional no-of-fields)
  (let ((new-record (make-vector (or no-of-fields
				     (length creation-methods))
				 nil))
	(this-field-no 0)
	computed-functions)
    (while creation-methods
      (aset new-record this-field-no
	    (db-convert-compute-field-value (car creation-methods) record))

      ;; ...

      (setq creation-methods (cdr creation-methods)
	    this-field-no (1+ this-field-no)))
    new-record
    ))

;; Uses and sets the "computed-functions" dynamic variable.
;; Perhaps get rid of the "result" variable.
(defun db-convert-compute-field-value (creation-method old-record)
  (cond ((numberp creation-method)
	 (aref old-record creation-method))
	((creation-method-literal-p creation-method)
	 (car (cdr creation-method)))
	((consp creation-method)
	 (let* ((function-spec (car creation-method))
		(result-no (car (cdr creation-method)))
		(result-found (if result-no
				  (assoc function-spec computed-functions))))
	   (if result-found
	       (nth result-no (cdr result-found))
	     (let* ((args (mapcar (function
				   (lambda (arg-spec)
				     (cond ((numberp arg-spec)
					    (aref old-record arg-spec))
					   ((creation-method-literal-p arg-spec)
					    (car (cdr arg-spec)))
					   (t (error "Bad argument specification %s." arg-spec)))))
				  (cdr function-spec)))
		    (result (apply (car function-spec) args)))
	       (if result-no
		   (progn
		     (setq computed-functions
			   (cons (cons function-spec result) computed-functions))
		     (nth result-no result))
		 result)))))
	(t (error "Unknown creation method %s" creation-method))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Canonicalizing creation methods
;;;

(defun db-canonicalize-creation-methods (creation-methods database)
  (mapcar (function (lambda (creation-method)
	     (db-canonicalize-creation-method creation-method database)))
	  creation-methods))

(defun db-canonicalize-creation-method (creation-method database)
  (cond
   ;; Literals
   ((null creation-method)
    ;; I don't think this clause is ever entered, but just in case...
    ;; The null test must precede the symbolp test, below.
    '(literal ""))
   ((stringp creation-method)
    (list 'literal creation-method))
   ((creation-method-literal-p creation-method)
    creation-method)
   ;; Field references
   ((symbolp creation-method)
    (or (fieldname->fieldnumber creation-method database)
	(error "%s isn't a fieldname in database %s."
	       creation-method (database-print-name database))))
   ((numberp creation-method)
    (if (and (> creation-method 0)
	     (< creation-method (database-no-of-fields database)))
	creation-method
      (error "%d isn't a valid field number in database %s."
	     creation-method (database-print-name database))))
   ;; Functions
   ((not (consp creation-method))
    (error "Ill-formed creation method %s." creation-method))
   (t
    (let ((function-spec (car creation-method))
	  function args result-no)
      (if (consp function-spec)
	  (setq function (car function-spec)
		args (cdr function-spec)
		result-no (car (cdr creation-method)))
	(setq function function-spec
	      args (cdr creation-method)
	      result-no nil))
      (if (not (fboundp function))
	  (error "%s has no function definition." function))
      (setq args (mapcar (function (lambda (name-or-number)
				     (if (numberp name-or-number)
					 name-or-number
				       fieldname->fieldnumber name-or-number)))
			 args))
      (cons (cons function args) result-no)))))

;;; db-convert.el ends here
