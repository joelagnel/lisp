;;; db-types.el --- part of EDB, the Emacs database

;; See database.el for copyright notice, distribution conditions, etc.

;; Author: Michael Ernst <mernst@theory.lcs.mit.edu>
;; Keywords: EDB

;;; Commentary:

;; Library of types for EDB database fields.

;; This file contains predefined types.  For efficiency, they're defined
;; in terms of the displayspec abstraction instead of via format strings.
;; Improvements and additions are welcome.  Predefined types for dates and
;; times can be found in db-time.el.

;;; Code:


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Contents
;;;

;;; Contents
;;; To Do
;;; Variables
;;; Displaytype
;;; Enumeration Displaytypes
;;; Numbers
;;; Booleans
;;; Strings
;;; Last Names
;;; First Names
;;; Full Names
;;; Places


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; To Do
;;;

;; Some of this information might be appropriate in the displayspec rather
;; than in the type.  Pros and cons:
;; In displayspec:
;;  * no need for extra indirection to get the information
;;  * easier to customize (eg to provide more specific help information).
;; In datatype:
;;  * less repetition of information, slightly more space-efficient.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Variables
;;;

(defvar db-use-completing-read t
  "*Non-nil if EDB should use `completing-read' when reading enumerated types.
Otherwise, on erroneous enumeration input, EDB shows the possible completions
but doesn't correct the input.")

(defvar db-enum-ignore-case t
  "If non-nil, assume that any association lists created from
enumeration types input names are to include both upper and lower case
versions of the name, if distinct.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Displaytype
;;;

;; WARNING!  We may want the type field of the displayspec to be different
;; from the argument here; this implies we want to be able to specify a
;; type either elsewhere or in optstring.  Easier for me if it's not in
;; optstring, of course.  Now, why make the user deal with optstring at
;; all?  Why not let him use an alist or something if he wants?  Answer:
;; because I'm too lazy just now for that.  [Uh, who uses the type field?
;; Wouldn't we want the type field to reflect the first argument?  Do we
;; just want to permit inheritance here?]

;; Perhaps permit inheritance based on inheritedtype; perhaps permit
;; setting of type field dependent on actualtype

;; Perhaps permit typename to be different from an abbreviation permitted
;; in field specifications.

(defun define-displaytype-from-optstring (typename optstring)
  "Define a displaytype named TYPENAME according to OPTSTRING.
TYPENAME is a symbol or string and OPTSTRING is the optional parameters
part of a display specification string."
  (if (stringp typename)
      (setq typename (intern typename)))
  (let ((displayspec (make-displayspec-from-type-and-options nil optstring t)))
    (define-displaytype-from-displayspec typename displayspec)))

;; Typename must be a symbol.  This is an internal function.
(defun define-displaytype-from-displayspec (typename displayspec)
  "Define a displaytype named TYPENAME (a symbol) with the default DISPLAYSPEC.
DISPLAYSPEC may also be a typename symbol itself."
  (setq db-displaytypes (cons (cons typename displayspec) db-displaytypes)))
(put 'define-displaytype-from-displayspec 'lisp-indent-hook 1)

(defun define-recordfieldtype-from-recordfieldspec (typename recordfieldspec)
  "Define a recordfieldtype named TYPENAME (a symbol) with the default RECORDFIELDSPEC.
DISPLAYSPEC may also be a typename symbol itself.
After this call, `recordfieldtype->recordfieldspec' called with argument
TYPENAME returns the proper record field specification."
  (setq db-recordfieldtypes (cons (cons typename recordfieldspec) db-recordfieldtypes)))
(put 'define-recordfieldtype-from-recordfieldspec 'lisp-indent-hook 1)

;; Provide functions to easily create aliases to existing types

(fset 'define-displaytype-alias 'define-displaytype-from-displayspec)
(make-obsolete 'define-displaytype-alias 'define-displaytype-from-displayspec)
(fset 'define-recordfieldtype-alias 'define-recordfieldtype-from-recordfieldspec)
(make-obsolete 'define-recordfieldtype-alias 'define-recordfieldtype-from-recordfieldspec)

(defun define-type-alias (alias typename)
  "Make symbol ALIAS refer to the same displaytype and recordfieldtype as TYPENAME."
  (define-displaytype-from-displayspec alias typename)
  (define-recordfieldtype-from-recordfieldspec alias typename))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Enumeration displaytypes
;;;

;; An enumeration displaytype is used for fields whose values are one of a
;; fixed set of alternatives.  There are two types of alternatives:  those
;; consisting of a single character (say, M or F for gender) and those
;; consisting of a whole string.  The types are nicknamed one-char-enum and
;; (for the multicharacter alternative type) enum.


;;;
;;; One-character alternatives
;;;

;; One-char-enum types don't necessarily get displayed as a single
;; character, but only a single character need be typed in order to set the
;; value.

(defun define-one-char-enum-displaytype (typename alternatives optstring)
  "Not yet implemented."
  (error "define-one-char-enum-displaytype isn't defined yet")
  )


;;;
;;; Multi-character alternatives
;;;

;; The user can specify for each alternative four pieces of information
;; (see documentation for `define-enum-type' for details).  We have the
;; following options for storing the relationships between those pieces of
;; information, which must be used by such functions as the displayspec's
;; display->actual function:
;;  * in global variables keyed by enum-type name and looked up when needed,
;;  * in database-local variables similar to the above, or
;;  * in the functions themselves (the functions are created when the
;;    enum-type is defined and must in any event contain some specific
;;    information such as the enum-type name).

;; Keeping the information in functions is faster at run-time (I suspect
;; this will be most noticable in stored->actual and actual->stored; it
;; might not be noticable elsewhere) but more difficult to debug and to
;; modify by hand.  If any of the information is kept in database-local
;; variables, all of it should be, because database-local variables are
;; saved when the database is stored in internal file layout and restored
;; when it is read in again.  However, other information is kept in global
;; variables such as db-displaytypes, so it is problematic to keep any
;; information in database-local variables; it is especially bad to have
;; the type half-defined so that an error is produced if the type is
;; redefined but the existing information is insufficient to provide the
;; full enum type functionality.  If all the information is stored in
;; database-local variables, then it is unnecessary for databases stored in
;; internal file layout to have calls to define-enum-type in their
;; auxiliary files.


(defun define-enum-type (typename alternatives &optional optstring)
  "Make TYPENAME (a symbol or string) an enumerated type.
Both a displaytype and a recordfieldtype are created.

ALTERNATIVES is a list.  Each alternative is a list of up to four components:
 the internal representation, any constant Lisp object, often a string;
 the input representation typed by the user to specify this alternative,
   a string or list of strings (for multiple input representations);
 the display representation, a string; and
 the file storage representation, a string.

If the input representation is omitted and the internal representation is a
string, that string is used.  If the display representation is omitted, it
defaults to the first input representation.  The display representation is
automatically also a valid input representation.  If the file storage
representation is omitted, it defaults to the display representation.
If all the other components are omitted, the internal representation string
may be used in place of a one-element list containing just it.

Optional argument OPTSTRING is a displayspec option string."

  (if (stringp typename)
      (setq typename (intern typename)))

  (let (d->a a->d a->s
	(processed-alternatives (db-enum-process-alternatives alternatives)))
    (setq d->a (car processed-alternatives)
	  a->d (car (cdr processed-alternatives))
	  a->s (car (cdr (cdr processed-alternatives))))

    (let ((ds (make-displayspec-from-type-and-options nil optstring t)))
      (displayspec-set-indent ds nil)
      (displayspec-set-display->actual ds (db-enum-make-display->actual
					   typename d->a))
      (displayspec-set-actual->display ds (db-enum-make-actual->display
					   typename a->d))
      (define-displaytype-from-displayspec typename ds))
    (let ((rs (make-recordfieldspec)))
      (recordfieldspec-set-type rs typename)
      (recordfieldspec-set-default-value rs "")
      (recordfieldspec-set-order-fn rs (make-enum-member-orderer typename a->d))
      (recordfieldspec-set-sort-fn rs (make-enum-member-sorter typename a->d))
      ;; With this value, we should have
      ;; (displayspec-set-match-actual->display ds 'string-match-actual->display)
      ;; (displayspec-set-match-display->actual ds 'string-match-display->actual)
      ;; in the displayspec, I believe.
      (recordfieldspec-set-match-function rs (function string-match-function))
      (recordfieldspec-set-help-info rs (db-enum-make-help-info typename d->a))
      (recordfieldspec-set-actual->stored rs (db-enum-make-actual->stored
					      typename a->s))
      (recordfieldspec-set-stored->actual rs (db-enum-make-stored->actual
					      typename a->s))
      (define-recordfieldtype-from-recordfieldspec typename rs))))

;; These names are provided for backward compatibility. (4/26/93)
(fset 'define-alternative-multi-char-displaytype 'define-enum-type)


;;   "Takes an alternatives list of the form accepted by `define-enum-type'.
;; Returns a list of three alists appropriate for use by display->actual,
;; actual->display, and actual->stored (also stored->actual) functions."
(defun db-enum-process-alternatives (alternatives)
  (let (d->a dinput->a a->d a->s a-s-differ
	alternative internal input display storage)
    (while alternatives
      (setq alternative (car alternatives)
	    alternative (if (listp alternative) alternative (list alternative))
	    internal (car alternative)
	    input (or (car (cdr alternative)) internal)
	    input (if (listp input) input (list input))
	    display (or (car (cdr (cdr alternative))) (car input))
	    storage (or (nth 3 alternative) display)

	    d->a (nconc d->a (mapcar (function (lambda (irep)
						 (cons irep internal)))
				     input))
	    dinput->a (if (member display input)
			  dinput->a
			(cons (cons display internal) dinput->a))
	    a->d (cons (cons internal display) a->d)
	    a->s (cons (cons internal storage) a->s)
	    a-s-differ (or a-s-differ (not (equal internal storage)))

	    alternatives (cdr alternatives)))
    ;; The order is significant in db-enum-make-help-info.
    ;; [It's not clear whether that's a feature or a bug.]
    (setq d->a (nconc d->a (nreverse dinput->a))
	  a->d (nreverse a->d)
	  a->s (and a-s-differ (nreverse a->s)))
    (list d->a a->d a->s)))


;; (defun db-enum-set-d->a (type alist &optional no-error)
;;   )
;; (defun db-enum-set-a->d (type alist &optional no-error)
;;   )
;; (defun db-enum-set-a->s (type alist &optional no-error)
;;   )
;; (defun db-enum-get-d->a (type alist &optional no-error)
;;   )
;; (defun db-enum-get-a->d (type alist &optional no-error)
;;   )
;; (defun db-enum-get-a->s (type alist &optional no-error)
;;   )


;;; make-enumerated-help  [Added by AKS, 10/13/92].
;;   "Return a string listing the input representations of TYPE."
(defun db-enum-make-help-info (type d->a)

  (let* ((buf (get-buffer-create " *temp*"))
	 (standard-output buf))
    (save-excursion
      (set-buffer buf)
      (erase-buffer)
      (princ (format "%s:  an enumerated type.  " (symbol-name type)))
      (display-completion-list (all-completions "" d->a))
      (prog1 (buffer-string)
	(kill-buffer buf)))))


;;   "Return a function appropriate for a displayspec's display->actual slot.
;; The input string is converted into one of the alternatives of TYPE, and
;; if ambiguities exist, they are resolved interactively."
(defun db-enum-make-display->actual (type d->a)
  (` (lambda (input-rep)
       (db-enum-do-completions input-rep (quote (, type)) (quote (, d->a))))))

;;   "Given a string INPUT-REP and an enum TYPE, return INPUT-REP if it is a
;; valid input representation, otherwise see if it completes to a valid one,
;; and show the possible completions."
(defun db-enum-do-completions (input-rep type d->a)

  (let ((assoc-hit (assoc input-rep d->a)))
    (if assoc-hit
	(cdr assoc-hit)
      (let* ((completion-ignore-case db-enum-ignore-case)
	     (try-comp (try-completion input-rep d->a)))
	(if (and try-comp
		 (setq assoc-hit (assoc try-comp d->a)))
	    (cdr assoc-hit)
	  (progn
	    (dbf-set-this-field-text (or try-comp ""))
	    (if db-use-completing-read
		(progn
		  (setq input-rep
			(completing-read
			 (format "Enter \"%s\" enum value (? for list): " type)
			 d->a nil t (or try-comp ""))
			assoc-hit (assoc input-rep d->a))
		  (cdr assoc-hit))
	      ;; no completing read, so display the options
	      (progn
		(with-electric-help-maybe
		 (display-completion-list
		  (all-completions (or try-comp "") d->a)))
		(error (if try-comp
			   "Not unique."
			 "No match."))))
	    ))))))


;;   "Return a function appropriate for a displayspec's actual->display slot."
(defun db-enum-make-actual->display (typename a->d)
  (` (lambda (enum-val)
       (cdr (assoc enum-val (quote (, a->d)))))))


;;   "Return nil or a function appropriate for a displayspec's actual->stored slot."
(defun db-enum-make-actual->stored (typename a->s)
  (if a->s
      (` (lambda (enum-val)
	   (cdr (assoc enum-val (quote (, a->s))))))))

;;   "Return nil or a function appropriate for a displayspec's stored->actual slot."
(defun db-enum-make-stored->actual (typename a->s)
  (if a->s
      (` (lambda (stored-val)
	   (car (db-rassoc stored-val (quote (, a->s))))))))


;;   "Return an ordering function for the enum type TYPE."
(defsubst make-enum-member-orderer (type a->d)
  (make-enum-member-orderer-internal type a->d -1 0 1))

;;   "Return a sorting function for the enum type TYPE."
(defsubst make-enum-member-sorter (type a->d)
  (make-enum-member-orderer-internal type a->d t nil nil))

;;   "Given an enum type TYPE, create an ordering function which compares two
;; items in the enum type's alternatives list.  The resulting function returns
;; LESS-VALUE if its first argument precedes its second argument in the
;; alternative list, EQUAL-VALUE if they're equal or neither appears, and
;; GREATER-VALUE if the first follows the second.
;; 
;; If one of the arguments doesn't appear in the alternatives list, the other
;; is considered to precede it."
(defun make-enum-member-orderer-internal (type a->d less-value equal-value greater-value)
  (` (lambda (item1 item2)
       (if (equal item1 item2)
	   (, equal-value)
	 (let ((alt-alist (quote (, a->d)))
	       (result (, equal-value))
	       alt-item)
	   (while alt-alist
	     (setq alt-item (car alt-alist))
	     (cond ((equal (car alt-item) item1)
		    (setq result (, less-value)
			  alt-alist nil))
		   ((equal (car alt-item) item2)
		    (setq result (, greater-value)
			  alt-alist nil))
		   (t
		    (setq alt-alist (cdr alt-alist)))))
	   result)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Numbers
;;;

;; I may wish to permit a numeric type that's stored as a string, not a
;; number; not because that will make conversion to display and storage
;; types easier (actually, moot) but because it will allow searching the
;; number like a string.


;; Ought I provide special handling of unspecified numbers?  Perhaps later;
;; that will require dealing with computation, too.

;;;
;;; Integers
;;;

(let ((ds (make-displayspec)))
  (displayspec-set-indent ds nil)
  (displayspec-set-actual->display ds (function int-to-string))
  (displayspec-set-display->actual ds (function string-to-int))
  (define-displaytype-from-displayspec 'integer ds))
(let ((rs (make-recordfieldspec)))
  (recordfieldspec-set-type rs 'integer)
  (recordfieldspec-set-default-value rs 0)
  (recordfieldspec-set-actual->stored rs (function int-to-string))
  (recordfieldspec-set-stored->actual rs (function string-to-int))
  (recordfieldspec-set-order-fn rs (function number-order))
  (recordfieldspec-set-sort-fn rs (function <))
  (recordfieldspec-set-match-function rs (function =))
  (recordfieldspec-set-help-info rs "An integer.")
  (define-recordfieldtype-from-recordfieldspec 'integer rs))

(let ((ds (make-displayspec)))
  (displayspec-set-indent ds nil)
  (displayspec-set-actual->display ds (function number-or-nil->string))
  (displayspec-set-display->actual ds (function string->integer-or-nil))
  (define-displaytype-from-displayspec 'integer-or-nil ds))
(let ((rs (make-recordfieldspec)))
  (recordfieldspec-set-type rs 'integer-or-nil)
  (recordfieldspec-set-default-value rs 0)
  (recordfieldspec-set-actual->stored rs (function number-or-nil->string))
  (recordfieldspec-set-stored->actual rs (function string->integer-or-nil))
  (recordfieldspec-set-order-fn rs (function number-or-nil-order-nil-greatest))
  ;; (recordfieldspec-set-sort-fn rs (function <))
  (recordfieldspec-set-match-function rs (function equal))
  (recordfieldspec-set-help-info rs "An integer, or nil.")
  (define-recordfieldtype-from-recordfieldspec 'integer-or-nil rs))

(defun string-or-nil->number-or-nil (string-or-nil)
  (and string-or-nil
       (string->number-or-nil string-or-nil)))
(make-obsolete 'string-or-nil->integer-or-nil 'string-or-nil->number-or-nil)

;; ;; Deal with requirement to take extra arguments.
;; (defun db-integer->string (i &rest ignore)
;;   (int-to-string i))
;; (defun db-string->integer (s &rest ignore)
;;   (string->integer s))
;; (defun db-integer-or-nil->string (i &rest ignore)
;;   (integer-or-nil->string i))
;; (defun db-string->integer-or-nil (s &rest ignore)
;;   (string->integer-or-nil s))
;; (proclaim-inline db-integer->string db-string->integer
;; 		 db-integer-or-nil->string db-string->integer-or-nil)


;;;
;;; Numbers (a number is an integer or a float)
;;;

;; Emacs 19.28 bug:  isfloat_string fails when its argument has trailing spaces,
;; so (string-to-number "5.4") => 5.4 but (string-to-number "5.4 ") => 5 .
(defun string-to-number-trim (string)
  (if (string-match " " string)
      ;; chop everything after the first space
      (string-to-number (substring string 0 (match-beginning 0)))
    (string-to-number string)))

(let ((ds (make-displayspec)))
  (displayspec-set-indent ds nil)
  (displayspec-set-actual->display ds (function number-to-string))
  (displayspec-set-display->actual ds (function string-to-number-trim))
  (define-displaytype-from-displayspec 'number ds))
(let ((rs (make-recordfieldspec)))
  (recordfieldspec-set-type rs 'number)
  (recordfieldspec-set-default-value rs 0)
  (recordfieldspec-set-actual->stored rs (function number-to-string))
  (recordfieldspec-set-stored->actual rs (function string-to-number-trim))
  (recordfieldspec-set-order-fn rs (function number-order))
  (recordfieldspec-set-sort-fn rs (function <))
  (recordfieldspec-set-match-function rs (function =))
  (recordfieldspec-set-help-info rs "A number.")
  (define-recordfieldtype-from-recordfieldspec 'number rs))

(let ((ds (make-displayspec)))
  (displayspec-set-indent ds nil)
  (displayspec-set-actual->display ds (function number-or-nil->string))
  (displayspec-set-display->actual ds (function string->number-or-nil))
  (define-displaytype-from-displayspec 'number-or-nil ds))
(let ((rs (make-recordfieldspec)))
  (recordfieldspec-set-type rs 'number-or-nil)
  (recordfieldspec-set-default-value rs 0)
  (recordfieldspec-set-actual->stored rs (function number-or-nil->string))
  (recordfieldspec-set-stored->actual rs (function string->number-or-nil))
  (recordfieldspec-set-order-fn rs (function number-or-nil-order-nil-greatest))
  ;; (recordfieldspec-set-sort-fn rs (function <))
  (recordfieldspec-set-match-function rs (function equal))
  (recordfieldspec-set-help-info rs "A number, or nil.")
  (define-recordfieldtype-from-recordfieldspec 'number-or-nil rs))


;;;
;;; Sorting and ordering
;;;

(defsubst number-order (a b)
  "Return -1, 0, or 1 depending on whether A < B, A = B, or A > B."
  (cond ((= a b) 0)
	((< a b) -1)
	(t 1)))

(defun number-or-nil-order-nil-greatest (a b)
  "Like number-order, but nil is treated as greater than any integer.
This puts nil after integers in an increasing list."
  (cond ((and a b)
	 (number-order a b))
	(a -1)
	(b 1)
	(t 0)))

(defun number-or-nil-order-nil-least (a b)
  "Like number-order, but nil is treated as smaller than any integer.
This puts nil after integers in a decreasing list."
  (cond ((and a b)
	 (number-order a b))
	(a 1)
	(b -1)
	(t 0)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Booleans
;;;

;; There could be a one-char boolean as well; implement it later.

(let ((ds (make-displayspec)))
  (displayspec-set-indent ds nil)
  (displayspec-set-min-width ds 3)
  (displayspec-set-max-width ds 3)
  (displayspec-set-actual->display ds (function boolean->yes-no-string))
  (displayspec-set-display->actual ds (function yes-no-string->boolean))
  (define-displaytype-from-displayspec 'yes-no ds))
(let ((rs (make-recordfieldspec)))
  (recordfieldspec-set-type rs 'boolean)
  (recordfieldspec-set-default-value rs nil)
  (recordfieldspec-set-actual->stored rs (function boolean->yn-string))
  (recordfieldspec-set-stored->actual rs (function yn-string->boolean))
  (recordfieldspec-set-order-fn rs (function boolean-order-function))
  (recordfieldspec-set-sort-fn rs (function boolean-lessp))
  (recordfieldspec-set-match-function rs (function eq))
  (recordfieldspec-set-help-info rs "A boolean value.")
  (define-recordfieldtype-from-recordfieldspec 'boolean rs))

(defsubst boolean->yes-no-string (boolean)
  (if boolean "Yes" "No"))

(defun yes-no-string->boolean (yes-or-no)
  (let ((downcased-yes-or-no (downcase yes-or-no)))
    (cond ((string= "yes" downcased-yes-or-no)
	   t)
	  ((string= "no" downcased-yes-or-no)
	   nil)
	  (t
	   (error "`%s' is not `Yes' or `No'." yes-or-no)))))

;; (defun db-boolean->yes-no-string (boolean &rest ignore)
;;   (boolean->yes-no-string boolean))
;; (defun db-yes-no-string->boolean (yes-or-no)
;;   (yes-no-string->boolean yes-or-no))
;; (proclaim-inline db-boolean->yes-no-string db-yes-no-string->boolean)

(defsubst boolean->yn-string (boolean)
  (if boolean "Y" "N"))

(defsubst yn-string->boolean (y-or-n)
  (string= "y" (downcase y-or-n)))

(defun string-is-yn-p (string)
  (and (= 1 (length string))
       (or (string= "Y" string)
	   (string= "y" string)
	   (string= "N" string)
	   (string= "n" string))))

;; t < nil so that in the "increasing" ordering "true" things occur before
;; false ones.  (This is somewhat arbitrary.)
(defun boolean-order-function (bool1 bool2)
  (cond ((or (and bool1 bool2)
	     (not (or bool1 bool2)))
	 0)
	(bool1
	 -1)
	(t
	 1)))

(defsubst boolean-lessp (bool1 bool2)
  (and (not bool1) bool2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Strings and variants
;;;

;; Type: string

(let ((ds (make-displayspec)))
  (displayspec-set-max-height ds nil)
  (displayspec-set-indent ds t)
  ;; use quote instead of function because of actual->display-call
  (displayspec-set-match-actual->display ds 'string-match-actual->display)
  (displayspec-set-match-display->actual ds 'string-match-display->actual)
  (define-displaytype-from-displayspec 'string ds))
(let ((rs (make-recordfieldspec)))
  (recordfieldspec-set-type rs 'string)
  (recordfieldspec-set-default-value rs "")
  (recordfieldspec-set-order-fn rs (function string-order-ci))
  (recordfieldspec-set-sort-fn rs (function string-lessp-ci))
  (recordfieldspec-set-match-function rs (function string-match-function))
  ;; String is the default type; I'm not sure I want
  ;; help-info for it.
  ;; (recordfieldspec-set-help-info rs "A string.")
  (define-recordfieldtype-from-recordfieldspec 'string rs))

;; Type: one-line-string

(let ((ds (copy-displayspec (displaytype->displayspec 'string))))
  (displayspec-set-min-height ds 1)
  (displayspec-set-max-height ds 1)
  (displayspec-set-indent ds nil)
  (define-displaytype-from-displayspec 'one-line-string ds))
;; This recordfieldspec is gratuitous.  Isn't it?
(let ((rs (copy-recordfieldspec (recordfieldtype->recordfieldspec 'string))))
  (recordfieldspec-set-type rs 'one-line-string)
  (define-recordfieldtype-from-recordfieldspec 'one-line-string rs))

;; Type: string-or-nil

(let ((ds (copy-displayspec (displaytype->displayspec 'string))))
  (displayspec-set-actual->display ds (function string-or-nil->string))
  (define-displaytype-from-displayspec 'string-or-nil ds))
(let ((rs (copy-recordfieldspec (recordfieldtype->recordfieldspec 'string))))
  (recordfieldspec-set-type rs 'string-or-nil)
  (recordfieldspec-set-order-fn rs (function string-or-nil-order-ci))
  (recordfieldspec-set-sort-fn rs (function string-or-nil-lessp-ci))
  (recordfieldspec-set-match-function rs (function string-or-nil-match-function))
  (define-recordfieldtype-from-recordfieldspec 'string-or-nil rs))

;; Type: nil-or-string

(let ((ds (copy-displayspec (displaytype->displayspec 'string-or-nil))))
  (displayspec-set-display->actual ds (function string->nil-or-string))
  (define-displaytype-from-displayspec 'nil-or-string ds))
(let ((rs (copy-recordfieldspec (recordfieldtype->recordfieldspec 'string-or-nil))))
  (recordfieldspec-set-type rs 'nil-or-string)
  (define-recordfieldtype-from-recordfieldspec 'nil-or-string rs))

;; Type: one-line-string-or-nil

(let ((ds (copy-displayspec (displaytype->displayspec 'one-line-string))))
  (displayspec-set-actual->display ds (function string-or-nil->string))
  (define-displaytype-from-displayspec 'one-line-string-or-nil ds))
(let ((rs (copy-recordfieldspec (recordfieldtype->recordfieldspec 'one-line-string))))
  (recordfieldspec-set-type rs 'one-line-string-or-nil)
  (recordfieldspec-set-order-fn rs (function string-or-nil-order-ci))
  (recordfieldspec-set-sort-fn rs (function string-or-nil-lessp-ci))
  (recordfieldspec-set-match-function rs (function string-or-nil-match-function))
  (define-recordfieldtype-from-recordfieldspec 'one-line-string-or-nil rs))

;; Helping functions for type string

(defsubst string-lessp-ci (string1 string2)
  "Case-insensitive version of string-lessp."
  (string-lessp (downcase string1) (downcase string2)))

(defun string-order-ci (string1 string2)
  "Return -1, 0, or 1 depending on whether STRING1 is lexicographically less
than, equal to, or greater than STRING2.  Case-insensitive."
  (let ((s1 (downcase string1))
	(s2 (downcase string2)))
    (cond ((string= s1 s2) 0)
	  ((string-lessp s1 s2) -1)
	  (t 1))))

;;; Matching strings

;; These variables used to be in db-search, but they're not used there.

(defvar dbm-string-regexp-prefix "^[ \t]*\\(/\\|regexp[ \t]+\\)")
(defvar dbm-string-regexp-prefix-string "/")

;; Pattern is a list of 'regexp and a regexp or a list of 'string, a
;; regexp, and a string.
(defun make-regexp-pattern (regexp)
  (list 'regexp regexp))
(defun regexp-pattern-regexp (regexp-pattern)
  (car (cdr regexp-pattern)))

(defun make-string-pattern (string &optional regexp)
  (list 'string (or regexp (regexp-quote string)) string))
(defun string-pattern-regexp (string-pattern)
  (car (cdr string-pattern)))
(defun string-pattern-string (string-pattern)
  (car (cdr (cdr string-pattern))))

(defsubst string-match-function (pattern string)
  ;; The second element of pattern is a regexp whether the pattern is a
  ;; string or a regexp.
  ;; However, I also deal with the case where pattern is incorrectly a string.
  (string-match (if (stringp pattern) pattern (car (cdr pattern)))
		string))

;; Return a pattern
(defun string-match-display->actual (string)
  (if (string-match dbm-string-regexp-prefix string)
      (make-regexp-pattern (substring string (match-end 0)))
    (make-string-pattern string)))

(defun string-match-actual->display (pattern)
  (cond ((eq (car pattern) 'string)
	 (string-pattern-string pattern))
	((eq (car pattern) 'regexp)
	 (concat dbm-string-regexp-prefix-string
		 (regexp-pattern-regexp pattern)))
	(t
	 (error "string-match-actual->display:  bad pattern %s" pattern))))

;; Helping functions for type string-or-nil

(defsubst string-or-nil->string (s-o-n)
  (or s-o-n ""))

(defsubst string-or-nil-lessp-ci (s-o-n-1 s-o-n-2)
  (string-lessp-ci (string-or-nil->string s-o-n-1)
		   (string-or-nil->string s-o-n-2)))

(defsubst string-or-nil-order-ci (s-o-n-1 s-o-n-2)
  (string-order-ci (or s-o-n-1 "") (or s-o-n-2 "")))

(defsubst string-or-nil-match-function (pattern s-o-n)
  (string-match (if (stringp pattern)
		    pattern
		  (if pattern (car (cdr pattern)) ""))
		(or s-o-n "")))

;; Helping functions for type nil-or-string

(defsubst string->nil-or-string (string)
  (if (equal "" string)
      nil
    string))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Last names
;;;

;; Note stringency of equality test:  capitalization and spacing matter.
(defun order-last-names (last-name-1 last-name-2)
  "Return -1, 0, or 1 depending on whether LAST-NAME-1 is lexographically
less than, equal to, or greater than LAST-NAME-2."
  (cond ((equal last-name-1 last-name-2)
	 0)
	((string-lessp (canonicalize-name last-name-1)
		       (canonicalize-name last-name-2))
	 -1)
	(t
	 1)))

;; Remove spaces and quotation marks, and ignore capitalization.
(defun canonicalize-name (last-name)
  (let ((result ""))
    (while (string-match "[ ']+" last-name)
      (setq result (concat result (substring last-name 0 (match-beginning 0)))
	    last-name (substring last-name (match-end 0))))
    (downcase (concat result last-name))))
(make-obsolete 'make-name-comparable 'canonicalize-name)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; First names
;;;

(defconst nicknames
  '(
    ("Abraham" "Abe")
    ("Charles" "Charlie")
    ("David" "Dave")
    ("Elizabeth" "Beth" "Liz")
    ("Gerald" "Gerry")
    ("James" "Jamey" "Jamie")
    ("Leonard" "Lenny")
    ("Michael" "Mike")
    ("Richard" "Dick" "Dicky" "Rick" "Ricky" "Rico")
    ("Robert" "Bob")
    ("Theodore" "Ted")
    )
  "List of nicknames which are not prefixes of the full name.
It is by no means comprehensive.")

(defun same-first-name-p (fname1 fname2)
  (or (nicknamep fname1 fname2)
      (nicknamep fname2 fname1)))

(defun nicknamep (nickname fullname)
  (or (string-match (concat "^" (regexp-quote nickname)) fullname)
      (member nickname
	      ;; I think this db-string-split is utterly unnecessary.
	      ;; [That was for a particular application, I believe.]
	      (cdr (assoc (car (db-string-split-first-word fullname))
			  nicknames)))))

(defun order-first-names (first-name-1 first-name-2)
  (let ((first-middle-1 (db-string-split-first-word first-name-1))
	(first-middle-2 (db-string-split-first-word first-name-2)))
    (cond ((or
	    ;; This first test isn't necessary.
	    ;; [Is it?]
	    (same-first-name-p (car first-middle-1) (car first-middle-2))
	    (same-first-name-p first-name-1 first-name-2))
	   0)
	  ((string-lessp (car first-middle-1) (car first-middle-2))
	   -1)
	  (t
	   1))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Full names
;;;

(defun standardize-name (name)
  ;; remove leading honorifics
  (if (string-match (concat "^\\(\\(Dr\\|Mrs?\\|Prof\\|1?Lt\\)\\.?"
			    "\\|Miss\\|Ensign\\|Doctor\\) ") name)
      (setq name (substring name (match-end 0))))
  ;; remove trailing gubbish
  (setq name (substring name
			0 (string-match ",? \\(PhD\\|MD\\|Esq\\.?\\|Esquire\\)$" name)))
  ;; add periods after initials
  (while (string-match "\\(^\\| \\)[A-Z]\\([ ,]\\|$\\)" name)
    (setq name (concat (substring name 0 (match-beginning 2))
		       "."
		       (substring name (match-beginning 2)))))
  name)

;; preferred suffixes
(defvar jr-assoc-list
  '(("3d" . "3rd")))

;; return list of (suffixless-name suffix)
(defun name->name-jr (name)
  (if (string-match ",? +\\(jr\\.?\\|sr\\.?\\|3r?d\\|[45]th\\|I+\\)$" name)
      (list (substring name 0 (match-beginning 0))
	    (let ((jr (substring name (match-beginning 1))))
	      (or (cdr (assoc (downcase jr) jr-assoc-list))
		  jr)))
    (list name "")))

;; return list of (fname lname jr)
(defun name->first-last-jr (fullname)
  (let* ((name-jr (name->name-jr (standardize-name fullname)))
	 (split-names (db-string-split-last-word (car name-jr)
					      "\\(La\\|de\\) [A-Z-]+")))
    (append split-names (cdr name-jr))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Places
;;;

(defvar UK-postal-code-regexp
  "[A-Z][0-9][A-Z] ?[0-9][A-Z][0-9]")

(defvar zip-code-regexp
  "[0-9][0-9][0-9][0-9][0-9]\\(-?[0-9][0-9][0-9][0-9]\\)?")

(defvar postal-code-regexp
  (mapconcat
   (function identity)
   (list
    ;; US zip code
    zip-code-regexp
    ;; UK postal code
    UK-postal-code-regexp
    ;; other
    "[0-9][0-9A-Z]*")
   "\\|"))

(defsubst postal-code-p (string)
  (string-match postal-code-regexp string))

;; Full state-alist and associated functions added by
;; Alan Stebbens, UCSB, Sep 18 '92.

(defvar state-alist
  '(
    ( "AK" . "Alaska" )
    ( "AL" . "Alabama" )
    ( "AR" . "Arkansas" )
    ( "AZ" . "Arizona" )
    ( "CA" . "California" )
    ( "CO" . "Colorado" )
    ( "CT" . "Conneticut" )
    ( "DC" . "District of Columbia" )
    ( "DE" . "Deleware" )
    ( "FL" . "Florida" )
    ( "GA" . "Georgia" )
    ( "GU" . "Guam" )
    ( "HI" . "Hawaii" )
    ( "ID" . "Idaho" )
    ( "IL" . "Illinois" )
    ( "IN" . "Indiana" )
    ( "IA" . "Iowa" )
    ( "KS" . "Kansas" )
    ( "KY" . "Kentucky" )
    ( "LA" . "Louisiana" )
    ( "MA" . "Massachusetts" )
    ( "MD" . "Maryland" )
    ( "ME" . "Maine" )
    ( "MI" . "Michigan" )
    ( "MN" . "Minnessota" )
    ( "MS" . "Mississippi" )
    ( "MO" . "Missouri" )
    ( "MT" . "Montana" )
    ( "NE" . "Nebraska" )
    ( "NH" . "New Hampshire" )
    ( "NC" . "North Carolina" )
    ( "ND" . "North Dakota" )
    ( "NJ" . "New Jersey" )
    ( "NM" . "New Mexico" )
    ( "NV" . "Nevada" )
    ( "NY" . "New York" )
    ( "OH" . "Ohio" )
    ( "OK" . "Oklahoma" )
    ( "OR" . "Oregon" )
    ( "PA" . "Pennsylvania" )
    ( "PR" . "Puerto Rico" )
    ( "RI" . "Rhode Island" )
    ( "SC" . "South Carolina" )
    ( "SD" . "South Dakota" )
    ( "TX" . "Texas" )
    ( "TN" . "Tennessee" )
    ( "UT" . "Utah" )
    ( "VA" . "Virginia" )
    ( "VT" . "Vermont" )
    ( "WA" . "Washington" )
    ( "WI" . "Wisconson" )
    ( "WV" . "West Virginia" )
    ( "WY" . "Wyoming" )
    )
  "An alist of ABBREV and FULLNAME of each of the United States, and its
territories.  Used by \"full-state-name\" and \"abbreviate-state\"."
  )

(defun statep (string)
  "Return non-nil if STRING is a valid state abbreviation."
  (full-state-name string))

(defun full-state-name (abbrev)
  "Return the full state name for ABBREV, or nil if not found."
  (and (= 2 (length abbrev))
       ;; (let ((elt (assoc (upcase abbrev) state-alist)))
       ;;   (if elt (cdr elt)))
       (cdr (assoc (upcase abbrev) state-alist))))

(defun abbreviate-state (state)
  "Return the postal abbreviation for STATE, or nil if not found."
  (car (db-rassoc (capitalize state) state-alist)))

;; (defun abbreviate-state (state)
;;   "Return the postal abbreviation for STATE."
;;   (let ((alist state-alist)
;; 	(key (capitalize state)))
;;     (while (and alist
;; 		(not (equal key (cdr (car alist)))))
;;       (setq alist (cdr alist)))
;;     (if alist (car (car alist)))))


(defvar country-list
  '("Canada"
    "Israel"
    "Switzerland"
    "Japan"
    "West Germany"
    "United States"
    "Zambia")
  "List of country names.  Far from complete.")

(defsubst countryp (string)
  (member (capitalize string) country-list))


;; Dates and times moved to db-time.el

;;; db-types.el ends here
