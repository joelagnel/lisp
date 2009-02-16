;;; db-format.el --- part of EDB, the Emacs database

;; See database.el for copyright notice, distribution conditions, etc.

;; Author: Michael Ernst <mernst@theory.lcs.mit.edu>
;; Keywords: EDB

;;; Commentary:

;; Displaying and editing database records.

;;; Code:

;; Is this necessary?
(require 'db-rep)

(require 'easymenu)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Variables
;;;

;;
;; The format -- text and displayspecs
;;

(deflocalvar dbf-displayspecs-length nil
  "The number of displayspecs in the current format.")

(deflocalvar dbf-displayspecs nil
  "An array of field specifiers, one for each field of the display format.
There are `dbf-displayspecs-length' of them.")

(deflocalvar dbf-inter-field-text nil
  "A vector with one string, the constant text that precedes that field in
the display, per displayspec, plus a final slot for trailing text.")

(defvar db-fontification db-running-lucid-emacs
  "Non-nil if uneditable text in data display buffers should use a special font.
Don't set this unless you are running Lucid GNU Emacs!")

(deflocalvar dbf-recordindex-displayspecno-vector nil)

(defun dbf-fieldname->displayspecno (fieldname)
  (aref dbf-recordindex-displayspecno-vector
	(fieldname->fieldnumber fieldname dbc-database)))

;; This is from Aborted-2.0.  It probably isn't used.
(deflocalvar dbf-displayspec-begin-markers nil
  "An array of markers, one for the beginning of each field of the display format.")


;; Faces

;; There should perhaps be another face for read-only fields.

(deflocalvar dbf-default-field-face nil
  "If non-nil, the face for fields in data display buffers.
If the text already has a face, that takes precedence.
This variable is examined only when a data display buffer is being set up.")

(deflocalvar dbf-default-inter-field-face nil
  "If non-nil, the face for uneditable text in data display buffers.
If the text already has a face, that takes precedence.
This variable is examined only when a data display buffer is being set up.")

(deflocalvar dbf-selected-field-face
  "If non-nil, the face for the field being currently edited.")


;; Should this be buffer-local?  Yes, since it's specific to a format, not
;; a database.
;; This isn't getting set anywhere.
(deflocalvar dbf-fieldabbrevs nil
  "Database-format-specific alist of fieldabbrevs and displayspecs.")

(deflocalvar dbf-always-forms nil
  "Forms executed every time that the format is selected.
These forms are only executed when a different format is replaced, not
every time that a record is displayed (or even every time that
`db-change-format' is called).
See also `dbf-before-display-record-function'.")

;;
;; Location in the format (current field info)
;;

;; All of these variables include "this-" in their names.

(deflocalvar dbf-this-displayspec nil
  "The displayspec currently being operated upon, or nil.")

(deflocalvar dbf-this-field-index nil
  "The index in `dbf-displayspecs' of the current displayspec, or nil.")

;; Maybe get rid of "this" from the name.
(defsubst dbf-set-this-field-index (new-index)
  ;; Can I guarantee this never happens?  If so, then remove this.
  (db-debug (if (and new-index (>= new-index dbf-displayspecs-length))
		(error "Index %d too large." new-index)))
  ;; (let ((index (and new-index (% new-index dbf-displayspecs-length)))) ...)
  (setq dbf-this-field-index new-index
	dbf-this-displayspec (and new-index
				  (aref dbf-displayspecs new-index))))

;; This is primarily used by the change-functions, which may not even be
;; interested in the information.  Perhaps give them the field number
;; instead and have them compute the info if they care.  But it's not so
;; expensive to compute field indices or to compute with them.
;; It is also called for messages to the user.
(defsubst dbf-this-field-name ()
  (and dbf-this-displayspec
       (fieldnumber->fieldname (displayspec-record-index dbf-this-displayspec)
			       dbc-database)))

;; The text actually in the buffer.
(defsubst dbf-this-field-text ()
  (buffer-substring dbf-this-field-beginning-pos (dbf-this-field-end-pos)))

;; Set the text actually in the buffer.
(defsubst dbf-set-this-field-text (field-text)
  "Make the format display FIELD-TEXT in the current field."
  ;; Maybe eventually I'll have to reverse the order of deletion and
  ;; insertion so as not to get on the wrong side of a marker.
  ;; delete old value
  (delete-region dbf-this-field-beginning-pos (dbf-this-field-end-pos))
  ;; insert new value
  (goto-char dbf-this-field-beginning-pos)
  (insert field-text))

;; A region-modification-hook could get rid of the need for the functions
;; and turn this back into an ordinary variable; but this is kind of a nice
;; solution, I think.
;; (deflocalvar dbf-this-field-modified-p-internal nil
;;   "T if the current field has been modified, nil otherwise.
;; Don't use this directly; use the functions
;; dbf-this-field-modified-p and dbf-set-this-field-modified-p.")
(defsubst dbf-this-field-modified-p ()
  (buffer-modified-p))
(defsubst dbf-set-this-field-modified-p (arg)
  (set-buffer-modified-p arg))

;; This is not currently used anywhere.
(deflocalvar dbf-wraparound-p 't
  "Value t, nil, or 'delay determines whether going forward from the last
field (or backward from the first) wraps, is prohibited, or delays.
'delay has the effect of prohibiting such movement the first time, but if
the user immediately makes a second attempt, that one is successful.
Somewhat analogous to dbc-wraparound-p.")

(deflocalvar dbf-this-field-beginning-pos nil
  "A position, the beginning of the current field.")

;; Maybe this should be next-field-beginning-mark.  No, because there might
;; not be any space between the end of this field and the beginning of the
;; next one, which is bad for the same reason putting the mark righat at
;; the end of this field is:  in the event of deleting the entire field,
;; the mark get put at the beginning of the field; but ordinarily
;; characters are inserted after marks, which would leave the mark at the
;; beginning instead of the end of the field.
(deflocalvar dbf-this-field-end-marker (make-marker)
  "A mark one character past the end of the current field, or nil if current
field extends to end of buffer.")

;; Only need one of these ever.
;;   "Remember where the user just moved while possibly munging a field."
(defvar dbf-moving-mark (make-marker))

;; The data display buffer should never be narrowed.
;; This is a macro so that, if the above assertion becomes no longer true,
;; I can easily rectify the situation.
(defsubst dbf-point-min ()
  1)


;;
;; The displayed record
;;

;; Could add another variable to determine which record is being used,
;; since we might cause a record to be put into the database and then
;; immediately begin editing it again; but it's not all that expensive to
;; copy the slots, and that situation should be rare anyway; we don't need
;; any more variables, after all.

;; Do not confuse with the record in the current link.  Real live database
;; records are never directly operated upon; we always munge the copy so
;; that the original can be restored if desired.
(deflocalvar dbf-this-record nil
  "The record currently displayed and edited.  This is an honest-to-goodness
record whose slots are filled from `dbf-this-record-original' if it's modified.
The variable's value should never be set except by `copy-record-to-record'; its
slots may be freely modified, however.
This is only used if `dbf-this-record-modified-p' is t.")

(deflocalvar dbf-this-record-original nil
  "The original of `dbf-this-record'; a pointer to some poor unsuspecting
record that shouldn't be modified until everything has been checked out.
That is, when the user is setting fields, this record remains unchanged
and  dbf-this-record, a copy of the original, is munged instead.")

(deflocalvar dbf-this-record-modified-p nil
  "T if the current record has been modified, nil otherwise.
This determines which record is returned by `dbf-displayed-record':
if non-nil, then `dbf-this-record-original' has been copied to `dbf-this-record'.
It's best to use `dbf-set-this-record-modified-p' to set this variable.")

(deflocalvar dbf-set-this-record-modified-function nil
  "A function called when the current record is marked as modified.
The function takes no arguments and its return value is ignored.
It is called after `dbf-this-record-original' is copied to `dbf-this-record'
and after `dbf-this-record-modified-p' is set to t.")

(defsubst dbf-set-this-record-modified-p (arg)
  "Set the value of `dbf-this-record-modified-p' to ARG.
If ARG is non-nil and `dbf-this-record-modified-p' is nil, also do the
necessary record-copying and call `dbf-set-this-record-modified-function'."
  (cond
   ((and arg (not dbf-this-record-modified-p))
    (setq dbf-this-record-modified-p arg)
    (copy-record-to-record dbf-this-record-original dbf-this-record)
    (if dbf-set-this-record-modified-function
	(funcall dbf-set-this-record-modified-function)))
   (t
    (setq dbf-this-record-modified-p arg))))

(defsubst dbf-displayed-record ()
  "Return the record currently displayed in this data display buffer.
This is `dbf-this-record' if `dbf-this-record-modified-p' is non-nil and
`dbf-this-record-original' otherwise."
  (if dbf-this-record-modified-p
      dbf-this-record
    dbf-this-record-original))

;; Maybe this should be in the fieldspec.  But I don't think so; it should
;; tell how to format, not remember what was formatted.
(deflocalvar dbf-fields-displayed nil
  "A vector of one string, the displayed text for that field, per displayspec.")
;; Eventually:
;; The element is either a string representing the displayed text for that field
;; or a list of strings (for indented fields).)

(deflocalvar dbf-redisplay-entire-record-p nil
  "T if the whole record needs to be redisplayed.
This is often set by change functions.")


;;
;; Hooks
;;

;;; Minor mode hooks

(defvar db-view-mode-hooks nil
  "Function or list of functions called when Database View mode is entered.")

(defvar db-edit-mode-hooks nil
  "Function or list of functions called when Database Edit mode is entered.")

;;; Movement hooks

(deflocalvar dbf-before-display-record-function nil
  "A function called before a record is displayed by `display-record'.
The function takes one argument, the record.

This is a good place to put calls to `db-change-format'.  Depending on
your function's implementation, however, you may silently override any user
calls to that function.")

(deflocalvar dbf-after-display-record-hook nil
  "Hooks called after a record is displayed by `display-record'.
The hooks are called in the data display buffer.
Call `dbf-displayed-record' to get the just-displayed record.")

(deflocalvar dbf-enter-field-hook nil
  "A function (of no arguments) called whenever a display field is entered.
The displayspec index is `dbf-this-field-index'.")
;; This function is only be called when the field is entered for real, not just
;; on the way to the actual destination field.

;;; Change hooks

(deflocalvar dbf-first-change-function nil
  "A function called the first time a record field is modified, or nil.
The function takes the fieldname and the old and new values as arguments,
and returns t if the record should be redisplayed.")

(deflocalvar dbf-every-change-function nil
  "A function called whenever a record field is modified, or nil.
The function takes the fieldname and the old and new values as arguments,
and returns t if the record should be redisplayed.")

(deflocalvar dbf-change-functions nil
  "A vector of one function (or nil) per record field (not display field).
The functions take the fieldname and the old and new values as arguments,
and return t if the record should be redisplayed.
Use `dbf-set-change-function' to set the fields of this vector.")

(defun dbf-set-change-function (fieldname function)
  "Set the change function for FIELDNAME to FUNCTION in the current database.
FUNCTION takes the fieldname and the old and new values as arguments,
and returns t if the record should be redisplayed."
  (aset dbf-change-functions
	(fieldname->fieldnumber fieldname dbc-database)
	function))

(deflocalvar dbf-after-record-change-function nil
  "Function called whenever changes to a record are recorded semi-permanently
by `dbf-process-current-record-maybe'.  For convenience, the function
takes the record as an argument, which is guaranteed to be `dbf-this-record'.
Its return value is ignored.")


;; Should this be reset-on-display-list?  Well, I have a hook there, so
;; programmers can get the same effect by putting the code there by hand.
;; Of course, the question is whether we want something so specific at all;
;; perhaps the display-hook really is the right place to put all this.
(deflocalvar dbf-reset-on-edit-list nil
  "An alist of (variable-name . default-value) pairs.
Every time Database Edit mode is entered, these buffer-local variables are
reset to their default values.  This is good for making sure that something
only happens once each time a record is edited.")


;;
;; The minor mode
;;

(deflocalvar dbf-minor-mode nil
  "A symbol, either 'view or 'edit.")

(deflocalvar dbf-minor-mode-name nil
  "\"View\" or \"Edit\".")

;;
;; The format
;;

;; Some variables local to the data display buffer don't need to be changed
;; when the display format changes.  The ones appearing below do.

;; Say how to set it; don't leave it a mystery.
(deflocalvar dbf-format-name nil
  "This buffer-local string names the format currently in use.
This should not be set by the user.")

(deflocalvar dbf-format-file nil
  "The format file from which the current format was built.")

(deflocalvar dbf-format-name-spec-alist nil
  "Association list of format names and format specifiers.
Each format name is an arbitrary string.
A format specifier is a filename or format file specifier, which is
a list of values for format variables.
The user sets the format specifier to a filename, and after that format file
has been read, EDB replaces the filename with a list of values for format
variables, so that the file need not be read again.

It is convenient for a database designer to set this, pre-assigning format
names to files so that the user only needs to remember the format names,
not the filenames.")

;; These two functions are from Aborted-2.0 and might not be used.
(defun dbf-install-format-name-and-spec (format-name format-spec)
  (if format-name
      (let ((old-fmtname-assoc (assoc dbf-format-name
				      dbf-format-name-spec-alist)))
	(if old-fmtname-assoc
	    (setcdr old-fmtname-assoc format-spec)
	  (setq dbf-format-name-spec-alist
		(cons (cons format-name format-spec)
		      dbf-format-name-spec-alist))))))


;; Returns a format file specifier (a list of values for format variables).
(defun dbf-format-name->spec (format-name)
  (let ((format-spec (and dbf-format-name
			  (cdr (assoc dbf-format-name
				      dbf-format-name-spec-alist)))))
    (or (and (stringp format-spec)
	     (cdr (assoc format-spec dbf-format-file-spec-alist)))
	format-spec)))

(deflocalvar dbf-format-file-spec-alist nil
  "Association list of file names and format file specifiers.
A format file specifier is a list of values for format variables.
Don't set this variable; use `dbf-format-name-spec-alist' instead.")

;; I should perhaps split this up.
(defun dbf-make-format-spec ()
  ;; All of these items vary from format to format within a particular
  ;; data display buffer; that is why I save them away, so that they can be
  ;; restored when the user returns to a format which was used previously in
  ;; this data display buffer.
  (list
   dbf-format-file
   ;; These can vary between data display buffers which happen to be using
   ;; the same format file to specify the layout of the record's fields.
   ;; That is, these are specific to a particular data display buffer, not
   ;; to a format, because they have to do with what is actually being
   ;; displayed and/or because we might expect the user to change them
   ;; after reading in the format.  This is why we can't just associate
   ;; this information with the format file, but have to save it on a
   ;; per-data-display-buffer basis.  If this function only stored away the
   ;; name of the format file and dbf-install-format-spec inferred the
   ;; values of the following variables, I wouldn't get what I want.
   dbf-summary-format
   dbf-summary-function
   dbf-fields-displayed
   dbf-field-search-defaults))

(defun dbf-install-format-spec (format-spec)
  (setq dbf-format-file (car format-spec))
  (setq format-spec (cdr format-spec))
  (setq dbf-summary-format (car format-spec))
  (setq format-spec (cdr format-spec))
  (setq dbf-summary-function (car format-spec))
  (setq format-spec (cdr format-spec))
  (setq dbf-fields-displayed (car format-spec))
  (setq format-spec (cdr format-spec))
  (setq dbf-field-search-defaults (car format-spec))
  (setq format-spec (cdr format-spec)))

(defun format-spec-format-file (format-spec)
  (if (listp format-spec)
      (car format-spec)
    format-spec))

(defun dbf-make-format-file-spec ()
  ;; These are constant for a particular format file.
  (list
   dbf-always-forms
   dbf-displayspecs
   dbf-displayspecs-length
   dbf-inter-field-text
   dbf-recordindex-displayspecno-vector))

(defun dbf-install-format-file-spec (format-file-spec)
  (setq dbf-always-forms (car format-file-spec))
  (mapcar (function eval) dbf-always-forms)
  (setq dbf-displayspecs (car (cdr format-file-spec))
	dbf-displayspecs-length (car (cdr (cdr format-file-spec)))
	dbf-inter-field-text (nth 3 format-file-spec)
	dbf-recordindex-displayspecno-vector (nth 4 format-file-spec)))


;;
;; Etc.
;;

;; Anything in the "Etc." section probably doesn't belong here.



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Constants
;;;

;;   "Regular expression which matches any number of pairs of backslashes.
;; Usually used in conjunction with other regexps.")
(defconst doubled-backslash-regexp "\\(\\\\\\\\\\)*")

(defconst non-backslash-character-regexp "\\(^\\|[^\\]\\)")

;; For finding displayspecs, use these regexps:

(defconst symbol-or-number-regexp "[-<>a-zA-Z0-9]+")
(defconst symbol-regexp "[a-zA-Z][-<>a-zA-Z0-9]*")
(defconst fieldname-regexp (concat "\\\\" symbol-regexp))
;; last item is brackets-surrounded material, for one-char alternative types
(defconst displaytype-nonsymbol-regexp "#\\|\\$\\|\"\\|'\\|\\[[^]]+\\]")
;; Does NOT include leading backslashes or commas.

(defvar displaytype-leading-comma-optional nil
  "Non-nil if symbolic displaytypes may immediately follow a fieldname.
Nil if a comma must come between the fieldname and the displaytype symbol.
Displaytype symbols include # $ \" '.
When this is non-nil, then sometimes Emacs 19 searches are intolerably slow.")

;; Perhaps the comma shouldn't be optional; but then I'd have to do special
;; work for the first field, which I'm loathe to do.
(defconst displaytype-regexp (concat (if displaytype-leading-comma-optional
					 ",?" ",")
				     "\\(" symbol-regexp
				     "\\|" displaytype-nonsymbol-regexp
				     "\\)"))

(defconst fieldoption-regexp (concat displaytype-regexp
				     "\\(=\\(" symbol-or-number-regexp "\\)\\)?"))
(defconst fieldoption-regexp-symbol 1)
(defconst fieldoption-regexp-equals 3)
(defconst fieldoptions-regexp (concat "\\(" fieldoption-regexp "\\)*"))

;; (defconst displayspec-regexp-no-context (concat "\\(" fieldname-regexp "\\)"
;; 						fieldoptions-regexp))
;; (defconst displayspec-regexp (concat doubled-backslash-regexp
;; 				     non-backslash-character-regexp
;; 				     "\\(" fieldname-regexp "\\)"
;; 				     fieldoptions-regexp
;; 				     ;; possibly "\ " at the end
;; 				     "\\(\\\\ \\)?"))
(defconst displayspec-regexp (concat "\\(" fieldname-regexp "\\)"
				     fieldoptions-regexp
				     ;; possibly "\ " at the end
				     "\\(\\\\ \\)?"))
(defconst displayspec-regexp-fieldname 1)
(defconst displayspec-regexp-fieldoptions 2)
(defconst displayspec-regexp-content-beginning displayspec-regexp-fieldname)
(defconst displayspec-regexp-content-end 0)
;; If there was no match for the fieldoptions
(defconst displayspec-regexp-content-end-alt displayspec-regexp-fieldname)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Abstraction
;;;

;;
;; Displayspec
;;

;; The displayspec contains record-index (indicates from which slot of the
;; database record this data comes); all other information has to do with
;; display only.  The fields are documented in the texinfo file.

;;; (Is this comment out of date?)
;; ** Change optspec-list if this structure is changed! **
(defstruct displayspec
  record-index

  ;; size and shape
  indent
  min-width
  max-width
  min-height				; default 1
  max-height				; default 1
  min-bytes
  max-bytes

  ;; other display info
  truncation-display-action
  padding-action
  actual->display
  display->actual
  ;; Is this where these belong?  Well, it lets me not make a new displayspec
  ;; for them...
  match-actual->display
  match-display->actual

  ;; editing info
  truncation-editing-action
  (reachablep t)
  )

;; Some of these might be wrong, set for slots of a different type.
(defsubst displayspec-set-record-index (ds val) (setf (displayspec-record-index ds) val))
(defsubst displayspec-set-indent (ds val) (setf (displayspec-indent ds) val))
(defsubst displayspec-set-min-width (ds val) (setf (displayspec-min-width ds) val))
(defsubst displayspec-set-max-width (ds val) (setf (displayspec-max-width ds) val))
(defsubst displayspec-set-min-height (ds val) (setf (displayspec-min-height ds) val))
(defsubst displayspec-set-max-height (ds val) (setf (displayspec-max-height ds) val))
(defsubst displayspec-set-min-bytes (ds val) (setf (displayspec-min-bytes ds) val))
(defsubst displayspec-set-max-bytes (ds val) (setf (displayspec-max-bytes ds) val))
(defsubst displayspec-set-truncation-display-action (ds val) (setf (displayspec-truncation-display-action ds) val))
(defsubst displayspec-set-padding-action (ds val) (setf (displayspec-padding-action ds) val))
(defsubst displayspec-set-actual->display (ds val) (setf (displayspec-actual->display ds) val))
(defsubst displayspec-set-display->actual (ds val) (setf (displayspec-display->actual ds) val))
(defsubst displayspec-set-match-actual->display (ds val) (setf (displayspec-match-actual->display ds) val))
(defsubst displayspec-set-match-display->actual (ds val) (setf (displayspec-match-display->actual ds) val))
(defsubst displayspec-set-truncation-editing-action (ds val) (setf (displayspec-truncation-editing-action ds) val))
(defsubst displayspec-set-reachablep (ds val) (setf (displayspec-reachablep ds) val))
;; Permit use of the old name, "reachable" (which may be more intuitive)
(defsubst displayspec-set-reachable (ds val) (displayspec-set-reachablep ds val))


;;
;; Optspecinfo
;;

;; An optspecinfo tells how to interpret optional parameters to a
;; display field specification.  An optspecinfo is a three-element list of
;;  * param-name:  a string
;;  * settor:  function taking a displayspec and a value and setting a slot
;;  * opt-param->value:  a function converting the optional parameter (that
;;    is, the string that follows the equal sign) into the actual value.

(defsubst opspectinfo-param-name (optspec-info)
  (car optspec-info))
(defsubst optspecinfo-settor (optspec-info)
  (car (cdr optspec-info)))
(defsubst optspecinfo-param->value (optspec-info)
  (car (cdr (cdr optspec-info))))


;; Settor-or-accessor is either a settor function, a slotname, or a list of
;; slotnames.  In the latter two cases, it's first converted into a settor.
(defmacro make-optspecinfo (param-name settor-or-accessor value-fn)
  (if (and (listp settor-or-accessor)
	   (eq 'quote (car settor-or-accessor)))
      (setq settor-or-accessor (car (cdr settor-or-accessor))))
  (let ((settor
	 (cond ((db-functionp settor-or-accessor)
		(list 'quote settor-or-accessor))
	       ((symbolp settor-or-accessor)
		(list 'quote
		      (symbol-append 'displayspec-set- settor-or-accessor)))
	       (t
		(let ((accessors settor-or-accessor)
		      (body '()))
		  (while accessors
		    (setq body (cons (list (symbol-append 'displayspec-set-
							  (car accessors))
					   'displayspec 'value)
				     body)
			  accessors (cdr accessors)))
		  (` (quote (lambda (displayspec value)
			      (,@ body)))))))))
    (list 'list param-name settor value-fn)))

;; (macroexpand '(make-optspecinfo "foo" list 'bar))
;; (macroexpand '(make-optspecinfo "foo" foo 'bar))
;; (macroexpand '(make-optspecinfo "foo" '(foo baz quux) 'bar))


;; List of optspecinfos, which tell how to interpret optional parameters to a
;; display field specification.
;; The functions are not symbol-function'ed because that makes debugging a
;; nightmare for a small performance increase.
(defconst optspec-list
  (list
   (make-optspecinfo "indent" 'indent '(lambda (x) t))
   (make-optspecinfo "noindent" 'indent '(lambda (x) nil))

   (make-optspecinfo "width" '(min-width max-width) 'string->number)
   (make-optspecinfo "min-width" 'min-width 'string->number)
   (make-optspecinfo "max-width" 'max-width 'string->number)
   (make-optspecinfo "length" '(min-width max-width) 'string->number)
   (make-optspecinfo "min-length" 'min-width 'string->number)
   (make-optspecinfo "max-length" 'max-width 'string->number)
   (make-optspecinfo "height" '(min-height max-height) 'string->number)
   (make-optspecinfo "min-height" 'min-height 'string->number)
   (make-optspecinfo "max-height" 'max-height 'string->number)
   (make-optspecinfo "bytes" '(min-bytes max-bytes) 'string->number)
   (make-optspecinfo "min-bytes" 'min-bytes 'string->number)
   (make-optspecinfo "max-bytes" 'max-bytes 'string->number)

   (make-optspecinfo "trunc-display" 'truncation-display-action 'intern)
   (make-optspecinfo "truncation-display-action" 'truncation-display-action 'intern)
   (make-optspecinfo "padding-action" 'padding-action 'intern)
   (make-optspecinfo "right-justify" 'padding-action 'right-justify-slotsetter-function)
   (make-optspecinfo "actual->display" 'actual->display 'intern)
   (make-optspecinfo "a->d" 'actual->display 'intern)
   (make-optspecinfo "display->actual" 'display->actual 'intern)
   (make-optspecinfo "d->a" 'display->actual 'intern)

   ;; match-actual->display and match-display->actual, fields 13 and 14

   (make-optspecinfo "truncation-editing-action" 'truncation-editing-action 'intern)
   (make-optspecinfo "trunc-edit" 'truncation-editing-action 'intern)
   (make-optspecinfo "reachable" 'reachablep '(lambda (x) t))
   (make-optspecinfo "unreachable" 'reachablep '(lambda (x) nil))
   ))

(defun right-justify-slotsetter-function (&rest args)
  ;;
  (cons ?  t))

;;; Predefined padding and truncation functions

(defun left-justify-padding-function (min-width display-rep display-rep-length)
  (concat display-rep (make-string (- min-width display-rep-length) ? )))

(defun right-justify-padding-function (min-width display-rep display-rep-length)
  (concat (make-string (- min-width display-rep-length) ? ) display-rep))

(defun return-right-justify-padding-function (&rest args)
  'right-justify-padding-function)

(defun ordinary-truncation-function (max-width display-rep display-rep-length)
  (put-text-property max-width display-rep-length 'invisible t display-rep))

;; ;; These are funcalled [in code that I've given up on]; they can't be macros.
;; 
;; (defsubst optspecinfo-accessor (optspec-info)
;;   (car (cdr optspec-info)))
;; 
;; (defsubst optspecinfo-specfunction (optspec-info)
;;   (car (cdr (cdr optspec-info))))

(defmacro display->actual-call (d->a fieldtext prev-value record recordfieldno)
  (` (let ((dac-d->a (, d->a))
	   (dac-fieldtext (, fieldtext)))
       (if dac-d->a
	   (db-vararg-call funcall 2 5
			   dac-d->a dac-fieldtext
			   (, prev-value) (, record) (, recordfieldno))
	 dac-fieldtext))))

(defvar actual->display-error-string "<ERROR>"
  "String displayed when the actual->display function doesn't return a string.")

(defmacro actual->display-call (a->d fieldtext record recordfieldno)
  (` (let* ((adc-a->d (, a->d))
	    (adc-fieldtext (, fieldtext))
	    (adc-result (if adc-a->d
			    (db-vararg-call funcall 2 4
					    adc-a->d adc-fieldtext
					    (, record) (, recordfieldno))
			  adc-fieldtext)))
       (if (stringp adc-result)
	   adc-result
	 actual->display-error-string))))

;; (macroexpand '(display->actual-call foo bar baz bum quux))
;; (macroexpand '(actual->display-call foo bar baz bum))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Macros
;;;

;; If the user has deleted some of the leading spaces, they'll be restored.
;; Don't do anything about tabs, not even untabifying.

;; The text actually in the buffer, adjusted for rectangularp.
(defun dbf-this-field-text-unrect ()
  (let ((text (dbf-this-field-text)))
    (db-debug-message "dbf-this-field-text-unrect: indent = %s"
		      (displayspec-indent dbf-this-displayspec))
    (unindentify text)))

;; Uses dbf-this-displayspec.
(defun indentify-absolute (text)
  (let ((amt (dbf-this-field-indent)))
    (if amt
	(db-string-substitute-substring-general-case
	 (concat "\n" (make-string amt ? )) "\n" text)
      text)))
(defun unindentify (text)
  (let ((amt (dbf-this-field-indent)))
    (if amt
	(db-string-substitute-substring-general-case
	 "\n" (concat "\n" (space-maybe-regexp amt)) text)
      text)))

;; Emacs 19's regexp routines fix bugs in the Emacs 18 and Lucid Emacs
;; versions, but are sometimes much slower.  For deeply indented fields,
;; this can result in very slow editing.  We disable some error-checking
;; and correction (which oughtn't ever be invoked anyway) for fields
;; indented more than `space-maybe-regexp-limit' characters.
;; Eventually EDB will use a completely different approach to indented fields.
(defvar space-maybe-regexp-limit 8)

;;   "Return a regexp matching N or fewer occurrences of the space character.
;; If N is nil, return the empty string, which is sometimes not a regexp you
;; want to search for by itself."
(defun space-maybe-regexp (n)
  (if n
      (if (> n space-maybe-regexp-limit)
	  (make-string (or n 0) ? )
	(let ((result (make-string (* 2 n) ? )))
	  (setq n (1- (* 2 n)))
	  (while (> n 0)
	    (aset result n ??)
	    (setq n (- n 2)))
	  result))
    ""))

;; Problem:  for the current field, "displayed" may not correspond to
;; what's actually shown, producing problems in moving around.  I want to
;; be able to skip over that, or to be careful to go around it.

;; I don't want to just remember the needed changes and make them later,
;; since the user may be asked questions, etc.

;; Maybe using db-emergency-restore-format is better; it needn't do all
;; this searching, for instance.


;; Avoid any processing, etc; just go to the field, do the work, come back.
(defun dbf-set-field-text (fieldno field-text)

  (error "dbf-set-field-text not yet implemented.")
  ;; ...

  )

;; Avoid any processing, etc; just go to the field setting the few
;; variables that must be set.
(defun dbf-goto-field (fieldno)

  (error "dbf-goto-field not yet implemented.")
  ;; ...

  )


;;; This is never used.
;; ;; Sets the text actually in the buffer, adjusted for rectangularp.
;; (defun dbf-set-this-field-text-unrect (field-text)
;;   (dbf-set-this-field-text)
;;   (if (displayspec-rectangularp dbf-this-displayspec)
;;       (save-restriction
;; 	(narrow-to-region (point) dbf-this-field-beginning-pos)
;; 	(goto-char dbf-this-field-beginning-pos)
;; 	(replace-string "\n" (concat "\n" (make-string (current-column) 32))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mode selection
;;;

;; Quiet the byte-compiler
(defvar database-view-mode-menu)
(defvar database-edit-mode-menu)

;; Question:  should view-mode be doing this work?  Given that the user can
;; call it directly, probably.  Should there be an -internal version?
;; Probably.

;; Note that this does NOT call dbf-process-current-record-maybe.  Should
;; it?  There are arguments both ways.

(defun db-view-mode (&optional arg)
  "Switch to Database View mode.
With an argument, toggle between Database View and Database Edit modes."
  (interactive "P")

  (cond ((not (db-data-display-buffer-p))
	 (error "Only call this in database mode."))
	((and arg (eq dbf-minor-mode 'view))
	 (db-edit-mode))
	;; If already in Database View mode, don't do anything.
	((not (eq dbf-minor-mode 'view))
	 (dbf-process-field-maybe t)
	 (setq dbf-minor-mode 'view
	       dbf-minor-mode-name "View")
	 (use-local-map database-view-mode-map)
	 (dbf-set-this-field-index nil)
	 (setq buffer-read-only t)
	 (goto-char (dbf-point-min))
	 (dbf-set-this-field-modified-p nil)
	 (easy-menu-remove database-edit-mode-menu)
	 (easy-menu-add database-view-mode-menu)
	 (run-hooks 'db-view-mode-hooks)
	 (force-mode-line-update))))

(defun db-edit-mode (&optional arg)
  "Switch to Database Edit mode.
With an argument, toggle between Database Edit and Database View modes."

  ;; This isn't interactive because it doesn't move point anywhere reasonable.
  ;;  (interactive "P")

  (cond ((not (db-data-display-buffer-p))
	 (error "Only call this in database mode."))
	((and arg (eq dbf-minor-mode 'edit))
	 (db-view-mode))
	(t
	 (setq dbf-minor-mode 'edit
	       dbf-minor-mode-name "Edit")
	 (use-local-map database-edit-mode-map)
	 (if (database-modifiable-p dbc-database)
	     (setq buffer-read-only nil)
	   (message (substitute-command-keys
		     (concat "Database is not modifiable; "
			     "change that with \\[db-toggle-modifiable-p]"))))
	 (mapcar (function (lambda (varname-value)
			     (make-variable-buffer-local (car varname-value))
			     (set (car varname-value) (cdr varname-value))))
		 dbf-reset-on-edit-list)
	 (easy-menu-add database-edit-mode-menu)
	 (easy-menu-remove database-view-mode-menu)
	 (run-hooks 'db-edit-mode-hooks)
	 (force-mode-line-update))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Movement in the format
;;;

;; This could ask a y-or-n-p question about calling emergency-restore, but
;; the user will probably always answer yes anyway.

(defun db-parse-buffer-error (format-string &rest args)
  (if db-debug-p
      (apply (function error) format-string args)
    (progn
      (db-emergency-restore-format)
      (db-message "I was confused about where I was.  Changes to the field might have been lost."))))

;; The obvious implementation doesn't work because after moving to the
;; correct row and column and doing db-jump-to-point, we might end up on
;; another row.  And we wouldn't know whether we belong there (because it's
;; the next occupied line) or we've overshot (because there was a field in
;; front of point on the line we originally tried).  The latter case is
;; unusual but possible nonetheless.
(defun db-next-line-or-field (arg)
  "Move to ARGth next line.  If that would move out of the current field,
move to the closest field to that, but not the current one, wrapping if necessary."
  (interactive "p")
  (let ((goal-column (current-column))
	goal-line)
    ;; Determine goal line.
    (db-forward-line-wrapping arg)
    (db-jump-to-point)
    (setq goal-line (db-current-line))
    ;; Move to proper column.
    (move-to-column goal-column)
    (db-jump-to-point)
    ;; If off the goal line, move back and as near to the goal column as possible.
    (if (> (db-current-line) goal-line)
	(progn
	  (db-previous-field-internal 1)
	  (goto-char (dbf-this-field-end-pos))))))

;; (defun old-db-next-line-or-field (arg)
;;   "Move to ARGth next line.  If that would move out of the current field,
;; move to ARGth next field instead, wrapping if necessary."
;;   (interactive "p")
;;   (if (save-excursion
;; 	(end-of-line)
;; 	(eobp))
;;       (db-next-field arg)
;;     (progn
;;       (next-line arg)
;;       (if (> (point) (dbf-this-field-end-pos))
;; 	  (progn
;; 	    (goto-char dbf-this-field-beginning-pos)
;; 	    (db-next-field arg))))))

(defun db-move-to-field-exact (arg)
  "Move to the ARGth field in the display.  Ignores reachablep."
  (db-first-field-internal t)
  (db-next-field-internal arg t)
  (run-hooks 'dbf-enter-field-hook))

(defun db-next-field (arg)
  "Move to ARGth next reachable field, wrapping if necessary.
When called interactively, ARG defaults to 1."
  (interactive "p")
  (dbf-process-field-maybe t)
  (goto-char dbf-this-field-beginning-pos)
  (if (> arg 0)
      (db-next-field-internal arg)
    (db-previous-field-internal (- arg)))
  ;; We have just moved to a new field, which certainly isn't modified yet.
  (dbf-set-this-field-modified-p nil)
  (run-hooks 'dbf-enter-field-hook))

;; I believe this, or db-previous-field-internal, is called whenever a new
;; field is moved to.

;; Arg should be positive.  Assumes point is at the beginning of the field.
;; If EXACT is non-nil, reachablep is ignored.
(defun db-next-field-internal (arg &optional exact)
  (while (> arg 0)
    (if (db-skip-string-forward (aref dbf-fields-displayed dbf-this-field-index))
	(progn
	  (setq dbf-this-field-index (1+ dbf-this-field-index)
		arg (1- arg))
	  (if (= dbf-this-field-index dbf-displayspecs-length)
	      (progn
		(if (not (db-skip-string-forward
			  (aref dbf-inter-field-text dbf-displayspecs-length)))
		    (db-parse-buffer-error
		     "Didn't find trailing text `%s' after field %s."
		     (aref dbf-inter-field-text dbf-displayspecs-length)
		     (1- dbf-displayspecs-length)))
		(setq dbf-this-field-index 0)
		(goto-char (dbf-point-min))))
	  (if (not (db-skip-string-forward
		    (aref dbf-inter-field-text dbf-this-field-index)))
	      (db-parse-buffer-error
	       "Didn't find field separator `%s' before field %s."
	       (aref dbf-inter-field-text dbf-this-field-index)
	       dbf-this-field-index))
	  ;;; Implement reachablep.
	  ;; *** Still need to guarantee no infinite loop.  Should go
	  ;; *** *somewhere* if all fields are unreachable somehow.
	  (if (not (or exact
		       (displayspec-reachablep
			(aref dbf-displayspecs dbf-this-field-index))))
	      (setq arg (1+ arg)))
	  )
      (db-parse-buffer-error
       "Didn't find field %s text `%s'."
       dbf-this-field-index
       (aref dbf-fields-displayed dbf-this-field-index))))
  (setq dbf-this-displayspec (aref dbf-displayspecs dbf-this-field-index)
	dbf-this-field-beginning-pos (point))
  (buffer-disable-undo (current-buffer))
  (buffer-enable-undo)

  ;; These two implementations seem about equally vile.
  ;; 1.
  (if (looking-at (regexp-quote (aref dbf-fields-displayed dbf-this-field-index)))
      (let ((end-of-match (match-end 0)))
	(set-marker dbf-this-field-end-marker
		    (if (= end-of-match (point-max))
			nil
		      (1+ end-of-match))
		    (current-buffer))))
  ;;   ;; 2.
  ;;   (if (db-skip-string-forward (aref dbf-fields-displayed dbf-this-field-index))
  ;;       (progn
  ;; 	(set-marker dbf-this-field-end-marker
  ;; 		    (if (eobp) nil (1+ (point)))
  ;; 		    (current-buffer))
  ;;  	(goto-char dbf-this-field-beginning-pos)))
  )

(defun db-previous-line-or-field (arg)
  "Move to ARGth previous line.  If that would move out of the current field,
move to the closest field to that, but not the current one, wrapping if necessary."
  (interactive "p")
  (let ((goal-column (current-column))
	(vacated-line (db-current-line))
	this-line)
    (db-forward-line-wrapping (- arg))
    (move-to-column goal-column)
    (db-jump-to-point)
    (setq this-line (db-current-line))
    (if (= this-line vacated-line)
	(progn
	  ;; We moved to a line containing no field, so db-jump-to-point
	  ;; put us in the field following point; ie, one on the line in
	  ;; which we started.  This is not the desired behavior.
	  ;; Get to a line containing a field.
	  (db-previous-field-internal 1)
	  (goto-char (dbf-this-field-end-pos))
	  ;; Go to the correct column.
	  (move-to-column goal-column)
	  ;; Avoid getting dumped back into this field.
	  (goto-char (min (point) (dbf-this-field-end-pos)))
	  ;; And end up there.
	  (db-jump-to-point)))))

;; (defun old-db-previous-line-or-field (arg)
;;   "Move to ARGth previous line.  If that would move out of the current field,
;; move to ARGth previous field instead, wrapping if necessary."
;;   (interactive "p")
;;   ;; This is for when point is on the first buffer line and in a field.
;;   ;; I don't believe it works if the first field starts at the first character
;;   ;; of the data display buffer.
;;   (if (save-excursion (beginning-of-line) (bobp))
;;       (beginning-of-line)
;;     (previous-line arg))
;;   (if (< (point) dbf-this-field-beginning-pos)
;;       (progn
;; 	(goto-char dbf-this-field-beginning-pos)
;; 	(db-previous-field arg))))

(defun db-previous-field (&optional arg)
  "Move to ARGth previous reachable field, wrapping if necessary.
When called interactively, ARG defaults to 1."
  (interactive "p")
  (dbf-process-field-maybe t)
  (goto-char dbf-this-field-beginning-pos)
  (if (> arg 0)
      (db-previous-field-internal arg)
    (db-next-field-internal (- arg)))
  (dbf-set-this-field-modified-p nil)
  (run-hooks 'dbf-enter-field-hook))

;; Arg should be positive.  Assumes point is at the beginning of the field.
(defun db-previous-field-internal (arg)
  (let ((prev-inter-field-text-beginning (marker-position
					  dbf-this-field-end-marker)))
    (if prev-inter-field-text-beginning
	(setq prev-inter-field-text-beginning
	      (1- prev-inter-field-text-beginning)))
    (while (> arg 0)
      (if (db-skip-string-backward (aref dbf-inter-field-text dbf-this-field-index))
	  (progn
	    (setq prev-inter-field-text-beginning (point)
		  dbf-this-field-index (1- dbf-this-field-index)
		  arg (1- arg))
	    (if (< dbf-this-field-index 0)
		(progn
		  (setq dbf-this-field-index (1- dbf-displayspecs-length))
		  (goto-char (point-max))
		  (if (db-skip-string-backward (aref dbf-inter-field-text
						  dbf-displayspecs-length))
		      (setq prev-inter-field-text-beginning (point))
		    (db-parse-buffer-error
		     "Didn't find trailing text `%s' after field %s."
		     (aref dbf-inter-field-text dbf-displayspecs-length)
		     dbf-this-field-index))))
	    (if (not (db-skip-string-backward
		      (aref dbf-fields-displayed dbf-this-field-index)))
		(db-parse-buffer-error
		 "Didn't find field %s text `%s'."
		 dbf-this-field-index
		 (aref dbf-fields-displayed dbf-this-field-index)))
	    ;;; Implement reachablep.
	    ;; *** Still need to guarantee no infinite loop.
	    (if (not (displayspec-reachablep
		      (aref dbf-displayspecs dbf-this-field-index)))
		(setq arg (1+ arg)))
	    )
	(db-parse-buffer-error
	 "Didn't find field separator `%s' before field %s."
	 (aref dbf-inter-field-text dbf-this-field-index)
	 dbf-this-field-index)))
    (setq dbf-this-displayspec (aref dbf-displayspecs dbf-this-field-index)
	  dbf-this-field-beginning-pos (point))
    (buffer-disable-undo (current-buffer))
    (buffer-enable-undo)
    (set-marker dbf-this-field-end-marker
		(and prev-inter-field-text-beginning
		     (if (or (= 1 prev-inter-field-text-beginning)
			     (= (point-max) prev-inter-field-text-beginning))
			 nil
		       (1+ prev-inter-field-text-beginning))))))

;; Call this when the first field isn't the final destination, to avoid
;; calling the enter-field hook.
;;   "Move to first field.  Optional EXACT means ignore reachability."
(defun db-first-field-internal (&optional exact)
  (if dbf-this-field-index
      (dbf-process-field-maybe t)
    (db-edit-mode))
  (setq dbf-this-field-index 0)
  ;; We need this even if field-index was nil, because someone might have
  ;; sneakily moved point.  (In fact, this is called after point is moved
  ;; via mouse.)
  (goto-char (dbf-point-min))
  (if (not (db-skip-string-forward (aref dbf-inter-field-text 0)))
      (db-parse-buffer-error
       "Didn't find field separator `%s' before field %s."
       (aref dbf-inter-field-text dbf-this-field-index)
       dbf-this-field-index))
  (db-next-field-internal 0)
  ;; Implement reachablep
  (if (not (or exact
	       (displayspec-reachablep
		(aref dbf-displayspecs dbf-this-field-index))))
      (db-next-field-internal 1))
  (dbf-set-this-field-modified-p nil))

(defun db-first-field ()
  "Move to first field."
  (interactive)
  (db-first-field-internal nil)
  (run-hooks 'dbf-enter-field-hook))

;; This isn't particularly efficient; ought to mirror db-first-field.  Oh, well.
(defun db-last-field ()
  "Move to last field."
  (interactive)
  (db-first-field-internal nil)
  (db-previous-field 1))

;; ought to permit a numeric prefix argument.
(defun db-scroll-up ()
  "Like scroll-up, but also edits the nearest database field."
  (interactive)
  (scroll-up)
  (db-jump-to-point t))

;; ought to permit a numeric prefix argument.
(defun db-scroll-down ()
  "Like scroll-down, but also edits the nearest database field."
  (interactive)
  (scroll-down)
  (db-jump-to-point t))

;; If not in a field, could beep or go to nearest.  Could try to be clever
;; about which field is "nearest" in some direction.  But not now.

;; This has major problems if the record gets displayed, as the marker gets
;; shoved to the front of the buffer.

;; Does nothing if not in a database buffer.
(defun db-jump-to-point (&optional quietly)
  "If in a data display buffer, move to the field containing or following point.
In a summary buffer, move to the record displayed around point."
  (interactive)
  (cond ((db-data-display-buffer-p)
	 (if (not (and dbf-this-field-index
		       (and (<= dbf-this-field-beginning-pos (point))
			    (<= (point) (dbf-this-field-end-pos)))))
	     ;; moving outside current field.
	     (let ((new-point (point)))
	       (set-marker dbf-moving-mark (point))
	       ;; Go back to where we were:  if we were in a field, get back in it.
	       (if dbf-this-field-index
		   (goto-char dbf-this-field-beginning-pos))
	       (if (and dbf-this-field-index
			(> (marker-position dbf-moving-mark) (point)))
		   ;; We are in a field and moving forward
		   (progn
		     (dbf-process-field-maybe t)
		     (goto-char dbf-this-field-beginning-pos))
		 (db-first-field-internal nil))
	       (db-debug-message "db-jump-to-point:  new-point = %d" new-point)
	       ;; If the dbf-process-field-maybe redisplays the entire record,
	       ;; the marker gets wiped out (points to the beginning of the
	       ;; buffer, because the buffer is cleared and refilled).
	       (let ((moving-pos (marker-position dbf-moving-mark)))
		 (if (not (= 1 moving-pos))
		     (setq new-point moving-pos)))
	       (set-marker dbf-moving-mark nil)
	       (while (and (> new-point (dbf-this-field-end-pos))
			   (< dbf-this-field-index (1- dbf-displayspecs-length)))
		 ;; The EXACT argument is t so we don't infinite-loop when
		 ;; the last field is unreachable.
		 (db-next-field-internal 1 t)
		 )
	       (if (not (displayspec-reachablep dbf-this-displayspec))
		   (progn
		     ;; This message is getting wiped out by the
		     ;; mouse-button-up event.  How can I fix this?
		     ;; Hint:  Transposing the following two statements is
		     ;; not the answer.
		     (if (not quietly)
			 (db-message "%s field is unreachable."
				     (fieldnumber->fieldname
				      (displayspec-record-index
				       dbf-this-displayspec)
				      dbc-database)))
		     (db-next-field-internal 1)))

	       (run-hooks 'dbf-enter-field-hook)
	       ;; The max makes sure we're in a field, not beyond it.
	       ;; The min is there only for the last field (because we could
	       ;; be past it, in which case there's not a following field).
	       (goto-char (min (max new-point dbf-this-field-beginning-pos)
			       (dbf-this-field-end-pos)))))
	 ;; Check not in indentation even if didn't move to a new field.
	 (if (dbf-in-indentation-p)
	     (db-beginning-of-line-or-field)))
	((db-summary-buffer-p)
	 ;; This is wrong in the presence of hidden directory lines.
	 (beginning-of-line)
	 (let* ((lines (count-lines dbs-point (point)))
		(lines-signed (if (< dbs-point (point)) lines (- lines)))
		(difference (/ lines-signed dbfs-lines)))
	   (goto-char dbs-point)
	   (dbs-next-record-ignore-hiding difference)))))

(defvar db-mouse-buffer-switch-moves-point-p t
  "If this variable is non-nil, then whenever a mouse event causes a database
buffer to become the current buffer, `db-jump-to-point' is called, placing
point as close to the mouse click as possible.
If this variable is nil, then mouse clicks in a database buffer only move
point when they do not cause a buffer switch, that is, when that database
buffer was alrady the current buffer.

Set this variable if you prefer that switching to a database buffer via
mouse clicks does not move point.  (One reason besides personal preference
is the use of software such as Hyperbole which causes mouse events outside
the data display buffer to make it active.)")

;;; This is obsolete; it's for Emacs 18.  FIX.
;; Best would be if we could know where the mouse was actually pressed or
;; released.  I have no mouse support, so I don't know how to do this.  Ideas?
;;   "Move to the field or record nearest the mouse position.
;; See `db-jump-to-point' for more details."
(defsubst db-x-jump-to-point ()
  (interactive)
  (let ((here (point))
	(this-buffer (current-buffer)))
    (x-flush-mouse-queue)
    (if (if db-mouse-buffer-switch-moves-point-p
	    ;; This window event caused point to move, or it switched buffers.
	    (not (and (eq here (point))
		      (eq this-buffer (current-buffer))))
	  ;; This window event caused point to move, but not switch buffers.
	  (and (not (eq here (point)))
	       (eq this-buffer (current-buffer))))
	;; db-jump-to-point is harmless if we're not in a database buffer.
	(db-jump-to-point))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Movement within a field
;;;

;; These shouldn't be called if not on a field, so they don't check.

;; This is so heavily called that I'm almost afraid to inline it for fear
;; of code explosion.  Well, it's not that bad, I guess.
;; Don't call this if not on a field.
(defsubst dbf-this-field-end-pos ()
  (let ((pos (marker-position dbf-this-field-end-marker)))
    (if pos
	(1- pos)
      (point-max))))

;; So that this can look like a function, like dbf-this-field-end-pos does.
;; I don't think I use this.
(defsubst dbf-this-field-beginning-pos ()
  dbf-this-field-beginning-pos)

(defun dbf-this-field-indent ()
  (and (displayspec-indent dbf-this-displayspec)
       (if (numberp (displayspec-indent dbf-this-displayspec))
	   (displayspec-indent dbf-this-displayspec)
	 (save-excursion
	   (goto-char dbf-this-field-beginning-pos)
	   (current-column)))))

;;;
;;; Checking
;;;

;; Which way should the default go on these functions?

;; Moves point to end of field if it's beyond that.
(defun dbf-check-if-beyond-field (&optional quietly)
  (let ((end-pos (dbf-this-field-end-pos)))
    (if (> (point) end-pos)
	(progn
	  (goto-char end-pos)
	  (if (not quietly)
	      (dbf-inform-outside-field "End of field."))))))

;; Moves point to beginning of field if it's before that.
(defun dbf-check-if-before-field (&optional quietly)
  (if (< (point) dbf-this-field-beginning-pos)
      (progn
	(goto-char dbf-this-field-beginning-pos)
	(if (not quietly)
	    (db-message "Beginning of field.")))))

;;   "If point is outside current field, it is move to the field's limit."
(defsubst dbf-check-if-outside-field (&optional quietly)
  (dbf-check-if-before-field quietly)
  (dbf-check-if-beyond-field quietly))

;; So keyboard macros terminate.
(deflocalvar dbf-field-boundary-action 'error
  "Controls action when point attempts to leave a field.
One of nil, 'message, 'beep, 'ding, 'error.
'beep and 'ding are identical and also show a message.
Having a variable is overkill, but I don't yet know what the Right Thing is.")

(defun dbf-inform-outside-field (message)
  (cond ((eq 'error dbf-field-boundary-action)
	 (error message))
	((eq 'ding dbf-field-boundary-action)
	 (db-message message)
	 (ding))
	((eq 'beep dbf-field-boundary-action)
	 (db-message message)
	 (beep))
	((eq 'message dbf-field-boundary-action)
	 (db-message message))
	((eq nil dbf-field-boundary-action)
	 nil)
	(t
	 (error "What value does this dbf-field-boundary-action value mean?  %s"
		dbf-field-boundary-action))))

;;;
;;; Movement
;;;

(defsubst db-beginning-of-field ()
  "Move to the beginning of the current field."
  (interactive)
  (goto-char dbf-this-field-beginning-pos))

(defsubst db-end-of-field ()
  "Move to the end of the current field."
  (interactive)
  (goto-char (dbf-this-field-end-pos)))

(defun dbf-in-indentation-p ()
  (let ((amt (dbf-this-field-indent)))
    (and amt
	 (> amt 0)
	 ;; Replaced by following lines.
	 ;; (db-looking-back-at (concat "^" (space-maybe-regexp (1- amt))))
	 (db-looking-back-at "^ +")
	 ;; Probably faster than (length (db-match-string 0)).
	 (< (current-column) amt))))

(defun db-beginning-of-line-or-field ()
  "Move to the beginning of the current line of the current field."
  (interactive)
  (beginning-of-line)
  (db-skip-regexp-forward (space-maybe-regexp (dbf-this-field-indent)))
  (dbf-check-if-outside-field t))

(defun db-end-of-line-or-field (arg)
  "Move to the end of the current line of the current field."
  (interactive "p")
  ;; Maybe just use (min end-of-line-pos end-of-field-pos) to avoid the noise.
  (end-of-line arg)
  (dbf-check-if-outside-field t))

(defun db-forward-char (arg)
  "Like forward-char, but won't go outside field."
  (interactive "p")
  (if (< arg 0)
      (db-backward-char (- arg))
    (let ((indent (dbf-this-field-indent)))
      (while (> arg 0)
	(if (eobp)
	    ;; This is so we get "End of field" instead of "End of buffer".
	    (progn
	      (setq arg 0)
	      (dbf-inform-outside-field "End of field."))
	  (progn
	    (forward-char 1)
	    (db-skip-regexp-forward (concat "^" (space-maybe-regexp indent)))
	    (setq arg (1- arg)))))
      (dbf-check-if-outside-field))))

(defun db-backward-char (arg)
  "Like backward-char, but won't go outside field."
  (interactive "p")
  (if (< arg 0)
      (db-forward-char (- arg))
    (let ((indent (dbf-this-field-indent)))
      (while (> arg 0)
	(if (bobp)
	    ;; This is so we get the error "Beginning of field"
	    ;; instead of "Beginning of buffer".
	    (progn
	      (setq arg 0)
	      (dbf-inform-outside-field "Beginning of field."))
	  (progn
	    ;; Is there a better way to do this check?
	    (db-skip-regexp-backward (concat "^" (space-maybe-regexp indent)))
	    (backward-char 1)
	    (setq arg (1- arg)))))
      (dbf-check-if-outside-field))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Editing
;;;

(defun db-delete-char (arg)
  (interactive "p")
  "Like delete-char, but won't delete outside the field."
  (delete-region (point) (progn (db-forward-char arg) (point))))

(defun db-backward-delete-char (arg)
  (interactive "p")
  "Like delete-backward-char, but won't delete outside the field."
  (delete-region (point) (progn (db-backward-char arg) (point))))

(defun db-forward-word (arg)
  "Like forward-word, but won't go outside field."
  (interactive "p")
  (forward-word arg)
  (dbf-check-if-outside-field))

(defun db-backward-word (arg)
  "Like backward-word, but won't go outside field."
  (interactive "p")
  (db-forward-word (- arg)))

(defun db-copy-region-as-kill (beg end)
  "Save the region as if killed, but don't kill it."
  (interactive "r")
  (if (eq last-command 'db-kill-region)
      (kill-append (unindentify (buffer-substring beg end)) (< end beg))
    (setq kill-ring (cons (unindentify (buffer-substring beg end)) kill-ring))
    (if (> (length kill-ring) kill-ring-max)
	(setcdr (nthcdr (1- kill-ring-max) kill-ring) nil)))
  (setq this-command 'db-kill-region)
  (setq kill-ring-yank-pointer kill-ring))

(defun db-kill-region (beg end)
  "Kill between point and mark.
The text is deleted but saved in the kill ring.  See `kill-region' for details."
  (interactive "*r")
  (db-copy-region-as-kill beg end)
  (delete-region beg end))

(defun db-kill-word (arg)
  "Like kill-word, but won't delete outside the field."
  (interactive "p")
  (db-kill-region (point) (progn (db-forward-word arg) (point))))

(defun db-backward-kill-word (arg)
  "Like backward-kill-word, but won't delete outside the field."
  (interactive "p")
  (db-kill-word (- arg)))

(defun db-kill-line (arg)
  "Like kill-line, but won't delete outside the field."
  (interactive "p")
  (let ((here (point)))
;;     (if (and (= arg 1)
;; 	     (looking-at (concat "[ \t]*\n"
;; 				 (space-maybe-regexp (dbf-this-field-indent)))))
;; 	(progn
;; 	  (goto-char (match-end 0))
;; 	  (dbf-check-if-outside-field))
;;       (db-end-of-line-or-field arg))
    (db-end-of-line-or-field arg)
    (if (< (point) (dbf-this-field-end-pos))
	(db-skip-regexp-forward
	 (concat "[ \t]*\n" (space-maybe-regexp (dbf-this-field-indent)))))
    (db-kill-region here (point))))

(defun db-kill-to-end ()
  "Kill from point to the end of the current field."
  (interactive)
  (db-kill-region (point) (dbf-this-field-end-pos)))


(defun db-newline (arg)
  "Insert a newline.  Will not make the current field too tall.
If the current field's maximum height is 1 line, move to the next field instead."
  (interactive "p")
  ;; ignores the argument
  (let ((max-height (displayspec-max-height dbf-this-displayspec)))
    (if (or (not max-height)
	    (< (count-lines dbf-this-field-beginning-pos (dbf-this-field-end-pos))
	       max-height))
	(let ((indent (dbf-this-field-indent)))
	  (newline 1)
	  ;;; I'm having second thoughts about this.
	  ;; 	;; this always returns t
	  ;; 	(looking-at (space-maybe-regexp indent))
	  ;; 	(replace-match (make-string indent ? ))
	  (if indent (db-old-insert (make-string indent ? ))))
      (if (= 1 max-height)
	  (db-next-field 1)
	(db-message "Field is at maximum height already.")))))

;; save-excursion wasn't doing the right thing here because it makes a
;; marker and the insertion occurred before the marker:
;;   (save-excursion
;;     (db-newline arg))

(defun db-open-line (arg)
  "Insert a newline and leave point before it.
Will not make the current field too tall."
  (interactive "p")
  (let ((here (point)))
    (db-newline arg)
    (goto-char here)))


(if (not (fboundp 'db-old-insert))
    (fset 'db-old-insert (symbol-function 'insert)))

;; These are lifted from simple.el.
;; This is a silly place for these functions to be defined.

;;; Superceded by db-insert-item
;; (defun db-insert-string (string)
;;   (db-old-insert (indentify-absolute string)))

(defun db-insert-item (string-or-char)
  (db-old-insert
   (indentify-absolute
    (if (stringp string-or-char)
	string-or-char
      (char-to-string string-or-char)))))

(defun db-insert (&rest args)
  "Any number of args, strings or chars.  Insert them after point, moving point forward.
Does special manipulations in database data display buffers."
  (if (db-data-display-buffer-p)
      (mapcar (function db-insert-item)
	      args)
    ;; Reduce total number of function applications by not using mapcar here.
    (apply (function db-old-insert) args)))

(fset 'insert 'db-insert)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Value processing for fields and records
;;;

;; I should also check that I can still see the inter-field-text after the
;; end of the field text.  If I'm paranoid (or if there is little or no
;; inter-field-text), check that I can see the next field text as well.

;; Make the return value of this meaningful.
;;   "Set the value of the current record from the current field.
;; If arg SET-FIELD-TEXT-P is non-nil, update the display as well.
;; Return t if field is unmodified or text is OK; nil otherwise.
;; May move point."
(defsubst dbf-process-field-maybe (set-field-text-p)
  ;; This outer test should not be necessary (perhaps remove it when I
  ;; reinstate miniwindow et al).
  (if dbf-this-field-index
      (if (dbf-this-field-modified-p)
	  (dbf-process-field set-field-text-p)
	t)
    t))


;; Should only be called if the field is really modified, or appears so.
(defun dbf-process-field (set-field-text-p)

  ;; I used to check for dbf-this-displayspec; but I think it cannot be non-nil
  ;; if (dbf-this-field-modified-p) returns t (and all is well).
  ;; Once I thought to have region-change-hook set dbf-this-field-modified-p.

  (if (or (< (point) dbf-this-field-beginning-pos)
	  (> (point) (dbf-this-field-end-pos)))
      (db-parse-buffer-error "Point was outside (%d) of current field (%d - %d)."
			     (point)
			     dbf-this-field-beginning-pos
			     (dbf-this-field-end-pos)))

  ;; This field has been modified, and point is in the field as expected.
  (if (not (equal (dbf-this-field-text)
		  (aref dbf-fields-displayed dbf-this-field-index)))
      ;; Perhaps it would behoove us to have an additional check with
      ;; unrect text, but I don't bother to remember it anywhere.
      (let* ((field-value (display->actual-call
			   (displayspec-display->actual dbf-this-displayspec)
			   (dbf-this-field-text-unrect)
			   (aref (dbf-displayed-record) dbf-this-field-index)
			   (dbf-displayed-record)
			   dbf-this-field-index))
	     (record-index (displayspec-record-index dbf-this-displayspec))
	     (old-field-value (aref (dbf-displayed-record) record-index))
	     (saved-modified-p dbf-this-record-modified-p))
	(db-debug-message "dbf-process-field-maybe:  record-index = %s"
			  record-index)
	(db-debug-message "dbf-process-field-maybe:  field-value = %s"
			  field-value)
	(if (not (equal field-value old-field-value))
	    ;; The new value is different from the old.
	    (progn
	      ;; Use dbf-set-this-record-modified-p in order to call
	      ;; dbf-set-this-record-modified-function when necessary, and to
	      ;; move the record into dbf-this-record.  (Should I be doing this
	      ;; before calling the constraint function?)
	      (dbf-set-this-record-modified-p t)
	      (record-set-field-from-index
	       dbf-this-record record-index field-value dbc-database)

	      (if set-field-text-p
		  (aset dbf-fields-displayed dbf-this-field-index
			(displayspec->displayed-rep dbf-this-displayspec
						    dbf-this-record)))
	      ;; No need to do redisplay before the change-hooks are
	      ;; called since the user's version is already onscreen
	      ;; and that will be very similar indeed to the display
	      ;; text.
	      (db-debug-message "dbf-redisplay-entire-record-p = %s"
				dbf-redisplay-entire-record-p)
	      (if (not saved-modified-p)
		  (setq dbf-redisplay-entire-record-p
			(or (and dbf-first-change-function
				 (funcall dbf-first-change-function
					   (dbf-this-field-name)
					   old-field-value
					   field-value))
			    dbf-redisplay-entire-record-p)))
	      (db-debug-message "after checking dbf-this-record-modified-p, dbf-redisplay-entire-record-p = %s"
				dbf-redisplay-entire-record-p)
	      (setq dbf-redisplay-entire-record-p
		    (or (and dbf-every-change-function
			     (funcall dbf-every-change-function
				       (dbf-this-field-name)
				       old-field-value
				       field-value))
			dbf-redisplay-entire-record-p))
	      (db-debug-message "dbf-redisplay-entire-record-p = %s"
				dbf-redisplay-entire-record-p)
	      (setq dbf-redisplay-entire-record-p
		    (let ((change-function (aref dbf-change-functions record-index)))
		      (or (and change-function
			       (funcall change-function
					(dbf-this-field-name)
					old-field-value
					field-value))
			  dbf-redisplay-entire-record-p)))
	      ))
	(db-debug-message "dbf-redisplay-entire-record-p = %s, set-field-text-p = %s"
			  dbf-redisplay-entire-record-p set-field-text-p)
	;; The text is different; the value may or may not have differed.
	;; Display the standard representation for this value, which has
	;; already been computed.
	(if set-field-text-p
	    ;; Perhaps add some sort of test of
	    ;; dbf-before-display-record-function here:  if non-nil, then
	    ;; we ought to be redisplaying regardless of anything else.
	    (if (not (dbf-redisplay-entire-record-maybe))
		;; set-field-text always returns nil
		(dbf-set-this-field-text
		 (aref dbf-fields-displayed dbf-this-field-index))))
	(dbf-set-this-field-modified-p nil))
    ;; Field is unchanged, so mark it unmodified.
    (dbf-set-this-field-modified-p nil)
    ))

;;   "If `dbf-redisplay-entire-record-p' is non-nil, redisplay current record
;; and return t; otherwise return nil."
(defun dbf-redisplay-entire-record-maybe ()
  (if dbf-redisplay-entire-record-p
      (progn
	(db-debug-message "Redisplaying entire record.")
	(setq dbf-redisplay-entire-record-p nil)
	(db-emergency-restore-format t)
	t)))


;; This should be a dbc- function, perhaps.
;; We should be able to say, NO, we can not commit the changes to the
;; current record, and we cannot proceed (if d-p-c-r-m returns t).
;; Currently, all callers IGNORE return value.

;; This does the right thing when called in the summary buffer -- nothing,
;; because dbc-index is nil there.
;;   "Commit changes to the record being displayed and edited.
;; If the current record (whatever is returned by `dbf-displayed-record') is a
;; modified copy of a database record, this copies it back to
;; dbf-this-record-original, which is the original database record.  Thus,
;; this procedure modifies the database by side effect.
;; Return t if successful, nil otherwise.
;; Updates the display if SET-TEXT-P is non-nil."
(defun dbf-process-current-record-maybe (set-text-p)
  (if dbc-index
      (progn
	;; Sets the field unmodified, if appropriate
	(dbf-process-field-maybe set-text-p)
	(if dbf-this-record-modified-p
	    (progn
	      ;; Do any programmer-requested checking or postprocessing here.
	      ;; This function may err, aborting out of whatever was trying to
	      ;; process the current record and do something else.
	      (if dbf-after-record-change-function
		  (funcall dbf-after-record-change-function
			   (dbf-displayed-record)))
	      (copy-record-to-record dbf-this-record dbf-this-record-original)
	      (link-set-summary dbc-link nil)
	      ;; (dbf-set-summary-out-of-date-p)
	      (dbf-update-summary-item dbc-index dbc-link)
	      ;; should set link-hiddenp too.
	      (dbc-set-database-modified-p t)
	      (setq dbf-this-record-modified-p nil)
	      (dbf-set-this-field-modified-p nil))))
    ;; This function shouldn't have been called on a non-database record;
    ;; how did we get here?
    ;; It may not be the case that the info is about to be abandoned.
    (or (not dbf-this-record-modified-p)
	(y-or-n-p "Abandon the displayed information? ")
	(error "Don't abandon displayed information."))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Undoing changes
;;;

(defun db-revert-field (&optional quietly)
  "Undo any changes made since entering this field.
Replace the onscreen text in this field with that of the underlying record.

A similar effect can be had by invoking \\[advertised-undo] multiple times."
  (interactive)

  (if (dbf-this-field-modified-p)
      (progn
	(dbf-set-this-field-text
	 (aref dbf-fields-displayed dbf-this-field-index))
	(dbf-set-this-field-modified-p nil)
	(if (not quietly)
	    (db-message "Reverted %s." (dbf-this-field-name))))
    (if (not quietly)
	(db-message "Can't revert %s; no changes since moving onto it."
		 (dbf-this-field-name)))))

(defun db-revert-record ()
  "Set the record to be the same as the corresponding one in the database.
In other words, undo any changes made since entering this record."
  (interactive)
  ;; This work might be wasted, but since usually this will be called from
  ;; Database View mode (not inside a record), it won't have any effect, and if
  ;; called from Database Edit mode and only one field is modified, it's a win.
  (db-revert-field t)
  (if dbf-this-record-modified-p
      (let ((buffer-read-only nil))
	(setq dbf-this-record-modified-p nil)
	(display-record (dbf-displayed-record) t)
	(if dbf-this-field-index
	    (db-move-to-field-exact dbf-this-field-index))
	(db-message "Reverted record."))
    (db-message "Can't revert this record; no changes since selecting it.")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Set displayspec from string
;;;


;; This doesn't work yet for multichar alternatives.

;; Do I really want displaytypes to be symbols?  Strings might well be easier.

;;   "Return the displaytype (a symbol) corresponding to STRING.
;; Non-strings are returned unchanged."
(defun string->displaytype (string)
  (if (stringp string)
      (cond ((string= string "#")
	      'integer)
	    ((string= string "$")
	      'number)
	    ((string= string "\"")
	      'string)
	    ((string= string "'")
	      'one-line-string)
	    ((string-match "^\\[.*\\]$" string)
	     (list 'alternative-one-char (substring string 1 -1)))
	    (t
	     (intern string)))
    string))

(defun displaytype->displayspec (displaytype)
  "Return a copy of the displayspec corresponding to string or symbol DISPLAYTYPE.
Return nil if there's no corresponding displayspec."
  (let ((displayspec
	 (cdr (assoc (string->displaytype displaytype) db-displaytypes))))
    (cond ((displayspec-p displayspec)
	   (copy-displayspec displayspec))
	  ((and displayspec (symbolp displayspec))
	   ;; make a recursive call
	   (displaytype->displayspec displayspec)))))

;;; Used to err.
;; (defun displaytype->displayspec (displaytype)
;;   "Return a copy of the displayspec corresponding to string or symbol DISPLAYTYPE."
;;   (copy-displayspec (or (cdr (assoc (string->displaytype displaytype)
;; 				  db-displaytypes))
;; 		      (error "%s is not a known displaytype." displaytype))))


;; From Aborted-2.0; possibly not used.
(defun comma-delimited-string->list (string)
  (let ((prev 0)
	(parse-point (string-match "," string))
	result)
    (while parse-point
      (setq result (cons (substring string prev parse-point) result)
	    prev (1+ parse-point)
	    parse-point (string-match "," string prev)))
    (nreverse (cons (substring string prev) result))))
;; (comma-delimited-string->list "a,bc,def")


;; Create a displayspec from a specification string.
(defun make-displayspec-from-string (displayspec-string database)
  "Create a displayspec from specification string DISPLAYSPEC-STRING.
Second argument is DATABASE."
  (if (not (string-match (concat "^" displayspec-regexp "$")
			 displayspec-string))
      (error "`%s' doesn't look like a field specification"
	     displayspec-string))
  (make-displayspec-from-string-internal displayspec-string database))

;; Assumes the match-data is set.  DISPLAYSPEC-STRING is nil if from the buffer.
(defun make-displayspec-from-string-internal (displayspec-string database)
  (let* ((fieldname (db-match-string displayspec-regexp-fieldname
				  displayspec-string))
	 (abbrev-assoc (assoc fieldname dbf-fieldabbrevs)))
    (if abbrev-assoc
	(copy-displayspec (cdr abbrev-assoc))
      (progn
	;; get rid of leading backslash
	(setq fieldname (intern (substring fieldname 1)))
	(let ((index (and database (fieldname->fieldnumber fieldname database)))
	      displayspec)
	  (if (and database (not index))
	      (error "%s is not a field or field abbreviation."
		     fieldname))
	  (db-debug-message "About to mdftao %s (%s); args = %s %s"
			    fieldname
			    (db-match-string displayspec-regexp-fieldname displayspec-string)
			    (database-recordfieldspec-type database index)
			    (db-match-string-maybe displayspec-regexp-fieldoptions
						displayspec-string))
	  (setq displayspec
		(make-displayspec-from-type-and-options
		 (database-recordfieldspec-type database index)
		 (db-match-string-maybe displayspec-regexp-fieldoptions
				     displayspec-string)))
	  (if (not displayspec)
	      (error "Type %s in field %d (%s) not recognized."
		     (database-recordfieldspec-type database index)
		     fieldname index))
	  (displayspec-set-record-index displayspec index)
	  displayspec)))))


;; This is abstracted out for the use of define-displaytype and others.

(defun make-displayspec-from-type-and-options (displaytype optionstring &optional notype-ok)
  ;; Either DISPLAYTYPE or OPTIONSTRING must specify a type, unless
  ;; optional argument NOTYPE-OK is specified, in which case an empty
  ;; displayspec may be returned.

  ;; Ordinarily (for instance, when this is being called to parse part of a
  ;; format), NOTYPE-OK should not be specified, so that invalid
  ;; displaytypes aren't created.

  ;; A type in OPTIONSTRING overrides DISPLAYTYPE.

  (if (not optionstring)
      (if displaytype
	  (or (displaytype->displayspec displaytype)
	      (error "No such displaytype as `%s'." displaytype))
	(make-displayspec))
    (let (displayspec match-end-0)
      ;; set the displayspec
      ;; Is it cheaper to do the concatenation or to test for the result being 0?

      ;; note tricky sequencing
      (if (and (string-match (concat "^" displaytype-regexp) optionstring)
	       (setq displayspec (displaytype->displayspec
				(string->displaytype
				 (db-match-string 1 optionstring)))))
	  (setq optionstring (substring optionstring (match-end 0)))
	(if displaytype
	    (setq displayspec (displaytype->displayspec displaytype))
	  (error "No type specified in `%s'." optionstring)))

      (while (not (equal "" optionstring))
	(if (not (string-match (concat "^" fieldoption-regexp) optionstring))
	    (error "`%s' isn't an optional field specification."
		   optionstring))
	(setq match-end-0 (match-end 0))
	;; (db-debug-message "mdftao:  match-data = %s" (show-match-data optionstring))
	;; Function in the third optspec position might clobber match-data.
	(update-displayspec-from-optspec-and-value
	 displayspec
	 (or (assoc (db-match-string fieldoption-regexp-symbol optionstring)
		    optspec-list)
	     (error "%s isn't a valid optional field specifier name or type."
		    (db-match-string fieldoption-regexp-symbol optionstring)))
	 (db-match-string-maybe fieldoption-regexp-equals optionstring))
	(setq optionstring (substring optionstring match-end-0)))
      displayspec)))


(defun update-displayspec-from-optspec-and-value (displayspec optspec value)
  (let ((settor (optspecinfo-settor optspec))
	(value (funcall (optspecinfo-param->value optspec) value)))
    (funcall settor displayspec value)))

;;; Old code
;; (defun update-displayspec-from-optspec-and-value (displayspec optspec value)
;;   (let ((accessor (optspecinfo-accessor optspec))
;; 	(value (funcall (optspecinfo-specfunction optspec) value)))
;;     (cond ((numberp accessor)
;; 	   (aset displayspec accessor value))
;; 	  ((db-functionp accessor)
;; 	   (funcall accessor displayspec value))
;; 	  ((listp accessor)
;; 	   ;; list of numbers
;; 	   (while accessor
;; 	     (aset displayspec (car accessor) value)
;; 	     (setq accessor (cdr accessor))))
;; 	  (t
;; 	   (error "Unrecognized optspecinfo-accessor %s." accessor)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Read a format file
;;;

;; Called by read-database-file, make-similar-database.

;; Perhaps this should add the result to the data-display-buffers slot of the
;; database as well.
;; The format file should exist.
;;   "Create and return a data display buffer.
;; This is only called when a brand-new data display buffer is being created, not
;; when one is being refreshed.
;; Arguments are FORMAT-FILE DATABASE NEW-DATABASE-P.
;; If NEW-DATABASE-P is non-nil, then the database's auxiliary file is read
;; and its field variables are set.
;;
;; WARNING:  If the format file's local variables set particular database
;; slots \(such as fieldnames\), and NEW-DATABASE-P is nil, then the database
;; may be left in an inconsistent state.  The \"primary\" format, which is
;; read in before the database is, should perform any such neccessary
;; actions."
(defun db-setup-data-display-buffer (format-file database new-database-p)

  (setq format-file (expand-file-name format-file))
  (if (not (file-readable-p format-file))
      (error "Can't read format file `%s'." format-file))

  ;; Do I want switch-to-buffer instead?
  (let ((result (db-make-data-display-buffer database new-database-p)))
    (set-buffer result)
    (db-debug (assert (eq dbc-database database) t))
    (setq buffer-read-only nil)		; (database-mode) set it to t

    (db-insert-file-contents format-file)
    (setq dbf-format-file format-file)

    (if new-database-p (load-db-aux-file database))

    (db-debug-message "db-setup-data-display-buffer:  hacking local variables")
    (db-really-hack-local-variables)
    (db-debug-message "hacked.")

    ;; Initialize local variables:  dbf-change-functions, dbf-this-record
    (dbf-set-buffer-local-variables)

    (if new-database-p
	(progn
	  ;; Initialize database variables.  We didn't do this earlier because
	  ;; they may depend on some values set in the format file.
	  (if (not (database-internal-file-layout-p database))
	      (db-set-field-variables database))))

    (db-setup-ddb-parse-displayspecs database)

    (setq buffer-read-only t)
    (current-buffer)))

;; Create and return a data display buffer; set a few buffer and database vars.
(defun db-make-data-display-buffer (database new-database-p)
  (let ((result (create-file-buffer (database-file database))))
    (in-buffer result
      (let ((dir (file-name-directory (database-file database))))
	(if dir
	    (setq default-directory (expand-file-name dir))))
      (setq dbc-database database)
      ;; Is this necessary?
      (if (not new-database-p)
	  ;; These are per-data-display-buffer variables.
	  (setq dbf-change-functions (make-vector (database-no-of-fields database)
						  nil)
		dbf-this-record (make-record dbc-database)))
      ;;; This is from Aborted-2.0
      ;; (database-set-data-display-buffers database
      ;;   (cons result (database-non-killed-data-display-buffers database)))
      ;; Given that lots of variables aren't set yet, I believe this works only
      ;; if buffer-modified-p is nil, which it is for brand-new buffers.
      (database-mode)
      (current-buffer))))

;; dbc-database must already be set
(defun dbf-set-buffer-local-variables ()
  ;; Initialize local variables.
  (if (not (database-no-of-fields dbc-database))
      (error "Can't tell how many fields database has."))
  (setq dbf-change-functions (make-vector (database-no-of-fields dbc-database)
					  nil)
	dbf-this-record (make-record dbc-database)))

;; Call this when the data display buffer has been set up and a format file's
;; contents have been inserted into it.
(defun db-setup-ddb-parse-displayspecs (database)

  ;; Get rid of local variables.
  (db-operate-on-local-variables (function delete-region))
  ;; Get rid of whitespace at end of buffer.
  (goto-char (point-max))
  (re-search-backward "[^ \t\n]")
  (delete-region (match-end 0) (point-max))
  ;; Get rid of whitespace at ends of lines.
  (goto-char (dbf-point-min))
  (while (re-search-forward  "[ \t]+$" nil t)
    (replace-match ""))

  (let ((prev-field-end (dbf-point-min))
	(backslash-placeholder (and (goto-char (dbf-point-min))
				    (search-forward "\\\\" nil t)
				    ;; assume this doesn't return nil
				    (db-unused-char-in-buffer)))
	beginning end this-displayspec displayspec-list inter-field-text-list)

    (if backslash-placeholder
	(progn
	  (setq backslash-placeholder (char-to-string backslash-placeholder))
	  (goto-char (dbf-point-min))
	  (replace-string "\\\\" backslash-placeholder)))

    (setq dbf-default-summary-format nil)

    (goto-char (dbf-point-min))
    (while (re-search-forward displayspec-regexp nil t)
      (db-debug-message "found field %s" (db-match-string 0))
      (setq beginning (match-beginning displayspec-regexp-content-beginning)
	    end (or (match-end displayspec-regexp-content-end)
		    (match-end displayspec-regexp-content-end-alt))
	    ;; Call "internal" version of function because match-data is set.
	    ;; nil as first argument means make it from the buffer.
	    this-displayspec (make-displayspec-from-string-internal nil database))

      ;; Fix up backslash-replacement.  The buffer is fixed up instead of
      ;; just the inter-field-text-list because of the call to current-column.
      (if backslash-placeholder
	  (save-excursion
	    (save-restriction
	      (narrow-to-region prev-field-end beginning)
	      (goto-char prev-field-end)
	      (replace-string backslash-placeholder "\\"))))

      (setq inter-field-text-list
	    (cons (buffer-substring prev-field-end beginning) inter-field-text-list))
      ;; because the match is about to be deleted, and we just used the old value.
      (setq prev-field-end beginning)

      (if (null dbf-default-summary-format)
	  (progn
	    (setq dbf-default-summary-format (save-excursion
					       (buffer-substring
						(progn (beginning-of-line 1)
						       (point))
						(progn (end-of-line 1)
						       (point)))))
	    ;; This will cause an error if one of the fields on the first
	    ;; line has variable height.  Or it should, at least.  I think.
	    (if (null dbf-summary-format)
		(setq dbf-summary-format dbf-default-summary-format))))


      (delete-region beginning end)
      ;; (displayspec-set-location this-displayspec (point-marker))

      (if (eq t (displayspec-indent this-displayspec))
	  (displayspec-set-indent this-displayspec (current-column)))

      (setq displayspec-list
	    (cons this-displayspec displayspec-list))

      ;; ;; This isn't really necessary since when the user sees it, it will
      ;; ;; be filled with real data (or this will have been taken care of).
      ;; (if (displayspec-min-width this-displayspec)
      ;;     (insert (make-string (displayspec-min-width this-displayspec) ? )))
      )
    ;; Fix up backslash-replacement for the post-last text.
    (if backslash-placeholder
	(save-excursion
	  (save-restriction
	    (narrow-to-region prev-field-end (point-max))
	    (goto-char prev-field-end)
	    (replace-string backslash-placeholder "\\"))))

    (setq inter-field-text-list
	  (cons (buffer-substring prev-field-end (point-max)) inter-field-text-list))

    (db-debug-message "db-setup-ddb:  displayspec-list = %s" displayspec-list)

    (setq dbf-inter-field-text (vconcat (nreverse inter-field-text-list))
	  dbf-displayspecs (vconcat (nreverse displayspec-list))
	  dbf-displayspecs-length (length dbf-displayspecs)
	  dbf-fields-displayed (make-vector dbf-displayspecs-length nil)
	  dbf-field-search-defaults (make-vector (1+ dbf-displayspecs-length) nil))
    )

  ;; initialize more local variables
  (setq dbf-recordindex-displayspecno-vector
	(make-vector (database-no-of-fields database) nil))
  (let ((fsno 0))
    (while (< fsno dbf-displayspecs-length)
      (aset dbf-recordindex-displayspecno-vector
	    (displayspec-record-index (aref dbf-displayspecs fsno))
	    fsno)
      (setq fsno (1+ fsno))))

  (db-debug-message "db-setup-ddb:  dbf-displayspecs = %s" dbf-displayspecs)
  (db-debug-message "db-setup-ddb: dbf-summary-format = %s" dbf-summary-format)

  (dbf-set-summary-format dbf-summary-format)

  ;; Is this necessary?
  (set-buffer-modified-p nil))

;; (defun undouble-backslashes (string)
;;   "Return a copy of STRING, replacing doubled backslashes by single ones."
;;   (db-string-substitute-substring-general-case "\\\\" "\\\\\\\\" string))

;; Should use dbf-make-format-spec and dbf-install-format-spec, not
;; db-setup-data-display-buffer.  The user knows to use dbf-always around
;; anything he wants set here.
;; Actually, now that I have db-copy-buffer-local-variables, it can be much
;; simpler and more foolproof.

(defun db-additional-data-display-buffer ()
  "Create another data display buffer in which to view this database."
  (interactive)
  (dbf-process-current-record-maybe t)
  (let* ((orig-buffer (current-buffer))
	 (database dbc-database)
	 ; (format-spec (dbf-make-format-spec))
	 (data-display-buffer (db-make-data-display-buffer database nil))
	 ; (af-names dbf-format-name-spec-alist)
	 ; (af-files dbf-format-file-spec-alist)
	 )
    (database-set-data-display-buffers database
       (cons data-display-buffer (database-data-display-buffers database)))
    (switch-to-buffer-other-window data-display-buffer)

    (db-copy-buffer-local-variables orig-buffer)
    ;; Here are the trampled-on variables that I really cared about:
    (setq dbf-this-record (make-record dbc-database))
    (db-emergency-restore-format t)

    ; (dbf-install-format-spec format-spec)
    ; (setq dbf-format-name-spec-alist af-names
    ;       dbf-format-file-spec-alist af-files)
    ; (db-change-format format-name format-file)
    ; ;; This is of highly questionable taste.
    ; (db-first-record)
    ))

(defun db-change-format (&optional format-name filename)
  "Select and use an alternate display format to view the database.
If neither FORMAT-NAME nor FILENAME is specified (as is the case when this
is called interactively), the user is prompted for them.  In Emacs Lisp
code, if `dbf-format-name-spec-alist' has been been set, usually only one of
the arguments is specified.  If both are specified, then FORMAT-NAME
becomes a name for the format FILENAME specifies; if FORMAT-NAME is already
associated with a different format file, an error is signalled.

If the current format is unnamed, the user is prompted for a name
to give it, so that it can be conveniently restored if need be.  This
behavior is suppressed, and the record is not displayed, if the function is
not being called interactively.

The data display buffer is left in Database View mode.

Selecting the current format does not cause any work to be done.

Some databases automatically set the format of the record being displayed,
usually by setting `dbf-before-display-record-function' to a function that
overrides the format in effect when a record is about to be displayed.
This may cause this function to appear not to be doing any work.  In
actuality the format is being set, then reset."
  (interactive)

  (if (not (and format-name
		(equal format-name dbf-format-name)))
      ;; We're not already in the requested format
      (progn
	(db-view-mode)

	;; If neither format-name nor filename is specified,
	;; as the user for one of them.
	(if (not (or format-name filename))
	    (progn
	      (setq format-name
		    (completing-read "Use which format? (? for options, RET to specify a file) "
				     ;; This is expensive.  Can't be helped.
				     ;; The "" is getting pushed to the
				     ;; beginning of the alphabetical list.
				     ;; I should fix that but don't know how.
				     (cons '("") dbf-format-name-spec-alist)
				     (function (lambda (assoc-elt)
						 (stringp (car assoc-elt))))
				     t))
	      (if (equal "" format-name)
		  (progn
		    (setq format-name nil
			  filename (read-file-name "File for new format: "
						   nil nil t))))))

	;; Either format-name or filename (or possibly both, if not called
	;; interactively) is set.
	(if filename
	    (setq filename (locate-format-file filename)))
	(if format-name
	    (let ((format-spec (cdr (assoc format-name dbf-format-name-spec-alist))))
	      (if format-spec
		  ;; successful format-name
		  (let ((fs-filename (format-spec-format-file format-spec)))
		    (if filename
			(if (and fs-filename
				 ;; This test is required for interactive
				 ;; uses of db-change-format.
				 (not (db-same-file-p filename fs-filename)))
			    (error "Format name %s is associated with %s, not %s."
				   format-name fs-filename filename))
		      (setq filename (locate-format-file fs-filename))))
		;; unsuccessful format-name
		(if filename
		    (setq dbf-format-name-spec-alist
			  (cons (cons format-name filename)
				dbf-format-name-spec-alist))
		  ;; no filename, failed format-name
		  (error "`%s' is not the name of a format." format-name)))))
	;; Filename is now set.

	;; First save away current format.  No need to do anything with filename.
	(if (and (interactive-p)
		 (not dbf-format-name)
		 (y-or-n-p "Would you like to give the current format a name? "))
	    (setq dbf-format-name (read-string "Name for current format: ")))
	(if dbf-format-name
	    (let ((old-fmtname-assoc (assoc dbf-format-name
					    dbf-format-name-spec-alist)))
	      (if old-fmtname-assoc
		  (setcdr old-fmtname-assoc (dbf-make-format-spec))
		(setq dbf-format-name-spec-alist
		      (cons (cons dbf-format-name (dbf-make-format-spec))
			    dbf-format-name-spec-alist)))))

	;; Now install the new format.
	(setq dbf-format-name format-name
	      dbf-format-file filename)
	(let ((new-format-spec
	       (cdr (assoc dbf-format-file dbf-format-file-spec-alist))))
	  (if new-format-spec
	      (progn
		(dbf-install-format-file-spec new-format-spec)
		(dbf-install-format-spec
		 (cdr (assoc (or dbf-format-name (intern dbf-format-file))
			     dbf-format-name-spec-alist))))
	    ;; We didn't find dbf-format-file in dbf-format-file-spec-alist; we
	    ;; probably didn't find more than just a filename at dbf-format-name
	    ;; in dbf-format-name-spec-alist either.
	    ;; This let is for the benefit of the new format file.
	    (let ((database dbc-database)
		  (buffer-read-only nil))
	      (db-message "Reading format from %s." dbf-format-file)
	      (buffer-disable-undo (current-buffer))
	      (erase-buffer)
	      (insert-file dbf-format-file)

	      (db-really-hack-local-variables)

	      (db-setup-ddb-parse-displayspecs dbc-database)

	      ;; Save away the file-invariant stuff.
	      (setq dbf-format-file-spec-alist
		    (cons (cons dbf-format-file (dbf-make-format-file-spec))
			  dbf-format-file-spec-alist))
	      ;; Install the defaults under a symbol associated with the format
	      ;; file (so it's not user-accessible).
	      (let ((dbf-summary-format dbf-default-summary-format)
		    (dbf-summary-function (if (equal
					       dbf-summary-format
					       dbf-default-summary-format)
					      dbf-summary-function)))
		(setq dbf-format-name-spec-alist
		      (cons (cons (intern dbf-format-file) (dbf-make-format-spec))
			    dbf-format-name-spec-alist)))
	      (erase-buffer))))

	(if (interactive-p)
	    (display-record (dbf-displayed-record) t)))))


(defun db-emergency-restore-format (&optional recompute)
  "Throw away the contents of the format buffer and redisplay the current record.
Use this if the format gets munged.
Changes made to the current field since last moving onto it may be lost.
If optional prefix argument RECOMPUTE is non-nil, `display-record' recomputes
the displayed text as well."
  (interactive "P")

  ;; (db-setup-data-display-buffer dbf-format-file dbc-database (current-buffer))

  (display-record (dbf-displayed-record) recompute)

  (if dbf-this-field-index
      (let ((this-field-index dbf-this-field-index))
	(dbf-set-this-field-modified-p nil)
	(db-move-to-field-exact this-field-index)
	;; If the hook changed formats, we'll be in Database View mode
	(db-edit-mode)
	)))

;; This should be somewhere in a set of functions that the user is told about.

;; Does this get run in the proper buffer if it appears in the database or
;; auxiliary file?  No, but it shouldn't be called in such buffers; it's a
;; format function, for goodness sake.

;; Calling this function causes db-make-summary-maker to be called at the
;; appropriate time.  This is usually right away, but if the database
;; information hasn't been read (ie, a call to this appears in the format
;; or auxiliary file), it is after the database fieldnames are known.

(defun dbf-set-summary-format (summary-format)
  "Specify the format used in the Database Summary buffer.
Argument SUMMARY-FORMAT is a string containing display specifications.
Call this in the data display buffer, or in a format file or auxiliary file."
  (interactive "sSummary format: ")
  (if (not (stringp summary-format))
      (error "Argument to dbf-set-summary-format should be a string, not %s"
	     summary-format))
  (if (= ?\n (elt summary-format (1- (length summary-format))))
      (setq summary-format
	    (substring summary-format 0 (1- (length summary-format)))))
  (setq dbf-summary-format summary-format)
  ;; This shouldn't require db-summary to be loaded; we want to put that off
  ;; until it is actually required.  Worry about that later.
  (require 'db-summary)
  (dbf-set-summary-out-of-date-p)
  (setq dbf-summary-recompute-all-p t)
  ;; If the alist isn't yet set, then we're still setting up, and this will
  ;; be called later on; do nothing for now.
  (if (database-fieldname-alist dbc-database)
      (dbf-make-summary-maker summary-format dbc-database)))

(defmacro dbf-always (&rest body)
  "Execute BODY, and place its forms in `dbf-always-forms'.
They will be executed each time that this format replaces another."
  (` (progn
       (setq dbf-always-forms (nconc dbf-always-forms (, body)))
       (,@ body))))
(put 'dbf-always 'edebug-form-spec '(&rest form))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Display data in a format
;;;

;; Will it be more efficient to erase the buffer and fill it again or to
;; find the fields and replace them?

;; Why would I want this to take a record as argument instead of using
;; dbf-displayed-record?
;; Joe Wells has used this feature, so don't remove it.

(defun display-record (record &optional recompute fieldno-limit)
  "Display RECORD in the current buffer, which is a data display buffer.
If optional arg RECOMPUTE is non-nil, the display representations will be
computed first; RECOMPUTE is typically non-nil only the first time a record
is shown.  If optional third arg FIELDNO-LIMIT is non-nil, only
fieldnumbers strictly less than it will be displayed."
  (let ((field-index 0)
	displayspec
	printed-rep
	(buffer-read-only nil)
 	(is-displayed-record-p (eq record (dbf-displayed-record)))
	;; If the user quits in this middle of this operation, EDB becomes
	;; very confused.  Inhibitting quitting is dangerous, though, so do
	;; it only if db-debug-p is nil.
 	(inhibit-quit (not db-debug-p))
	ext-start
	)
    ;; This must be called with is-displayed-record bound.
    (db-funcall-maybe dbf-before-display-record-function record)
    ;; Allow dbf-before-display-record-function to do
    ;; dbf-set-this-record-modified-p if it wants to.
    (if is-displayed-record-p
 	(setq record (dbf-displayed-record)))
    ;; Why is this here?  I guess it can't hurt.
    (dbc-update-database-modified-p)
    (buffer-disable-undo (current-buffer))
    (if db-fontification
	(map-extents (function (lambda (x y) (delete-extent x)))
		     (current-buffer) (point-min) (point-max) nil))
    (erase-buffer)
    (while (< field-index dbf-displayspecs-length)
      ;; (db-debug-message "display-record:  field %s" field-index)
      (setq displayspec (aref dbf-displayspecs field-index))
      (setq ext-start (point))
      (db-old-insert (aref dbf-inter-field-text field-index))
      (if db-fontification
	  (db-fontify ext-start (point)))
      (if recompute
	  (aset dbf-fields-displayed field-index
		(if (and fieldno-limit
			 (>= field-index fieldno-limit))
		    ;; Should deal with min-height and min-bytes, too.
		    ;; Probably want a function displayspec->empty-printed-rep.
		    (make-string (or (displayspec-min-width displayspec) 0) ? )
		  ;; Could use displayspec->displayed-rep, but it is mainly for
		  ;; updating fields that already exist.
		  (progn
		    (setq printed-rep
			  (displayspec->printed-rep displayspec record))
		    (db-string-substitute-substring-general-case
		     (concat "\n" (make-string (current-column) 32))
		     "\n"
		     printed-rep)))))
      ;; Does (dbf-this-field-indent) work at this point?
      ;; If not, db-insert-string won't.  And I suspect it won't.
      (db-old-insert (aref dbf-fields-displayed field-index))
      (setq field-index (1+ field-index)))
    (setq ext-start (point))
    (db-old-insert (aref dbf-inter-field-text field-index))
    (if db-fontification
	(db-fontify ext-start (point)))
    ;; Why?  Shouldn't caller be worrying about this?
    (dbf-set-this-field-modified-p nil)
    ;; This place is as good as any for leaving the cursor by default.
    ;; In fact, if dbf-this-field-index is nil, I think I assume the
    ;; cursor is at point-min.
    (goto-char (dbf-point-min))
    (buffer-enable-undo (current-buffer))
    (run-hooks 'dbf-after-display-record-hook)
    ;; If the user tried to quit out while this was happening, ignore it.
    (setq quit-flag nil)
    ))

;; ;; It would probably behoove me to inline the work instead of calling
;; ;; dbf-next-field, dbf-set-this-field-text, etc.
;; ;; But I would rather just rewrite the whole buffer than parse it cleverly.
;; 
;; (defun display-record-2 (record &optional fieldno-limit)
;;   "Display RECORD.  If optional arg FIELDNO-LIMIT is non-nil, only
;; fieldnumbers strictly less than it will be displayed."
;;   (let ((field-index 0)
;; 	displayspec
;; 	(buffer-read-only nil))
;;     (while (< field-index dbf-displayspecs-length)
;;       ;; (db-debug-message "display-record-2:  field %s" field-index)
;;       (setq displayspec (aref dbf-displayspecs field-index))
;;       ;; Unfortunately this does a process-field.
;;       (db-next-field 1)
;;       (dbf-set-this-field-text
;;        (if (and fieldno-limit
;; 		(>= field-index fieldno-limit))
;; 	   (make-string (or (displayspec-min-width displayspec) 0) ? )
;; 	 (displayspec->printed-rep displayspec record)))
;;       (setq field-index (1+ field-index)))
;;     (dbf-set-this-field-modified-p nil)
;;     ;; This place is as good as any for leaving the cursor by default.
;;     ;; In fact, if dbf-this-field-index is non-nil, I think I assume the
;;     ;; cursor is at point-min.
;;     (goto-char (dbf-point-min))
;;     ))


;; The goal:  abstract the heck out of this.
;; The reason:  so that make-summary-printer can use only parts of it,
;; preprocessing when (say) it knows the value of actual->display,
;; min-width, and max-width.
;; Do it later.

(defun displayspec->printed-rep (displayspec record)
  (let* ((record-index (displayspec-record-index displayspec))
	 (display-rep (actual->display-call
		       (displayspec-actual->display displayspec)
		       (aref record record-index)
		       record
		       record-index)))
    ;; (db-debug-message "displayspec->p-r:  display-rep = `%s'" display-rep)

    (let ((min-height (displayspec-min-height displayspec))
	  (max-height (displayspec-max-height displayspec)))
      (if (or min-height max-height)
	  (let ((display-rep-height (1+ (count-array ?\n display-rep))))
	    (cond ((and min-height (< display-rep-height min-height))
		   ;; too short
		   (setq display-rep
			 (concat display-rep
				 (make-string (- min-height display-rep-height)
					      ?\n))))
		  ((and max-height (> display-rep-height max-height))
		   ;; too tall
		   (setq display-rep
			 (substring display-rep 0
				    (db-find-char-from-end
				     ?\n display-rep
				     (- display-rep-height min-height)))))))))

    ;; These conditions are much too simplistic; they only work for one-line
    ;; representations.
    (let ((display-rep-length (length display-rep))
	  (min-width (displayspec-min-width displayspec))
	  (max-width (displayspec-max-width displayspec)))
      (cond ((and min-width (< display-rep-length min-width))
	     ;; The display representation is too short
	     (setq display-rep (funcall (or (displayspec-padding-action displayspec)
					    (function left-justify-padding-function))
					min-width display-rep display-rep-length))
	     (if (not (= (length display-rep) min-width))
		 (error "Padding function %s returned \"%s\", which has length %d, not %d."
			(or (displayspec-padding-action displayspec)
			    'left-justify-padding-function)
			display-rep (length display-rep) min-width))
	     (setq display-rep-length min-width)
	     ;;; Old code
	     ;; (let ((padding-action (displayspec-padding-action displayspec)))
	     ;;   (if (db-functionp padding-action)
	     ;;       (funcall padding-action
	     ;; 	       min-width
	     ;; 	       display-rep
	     ;; 	       display-rep-length)
	     ;;     ;; if padding-action is not a function, it's nil or a cons.
	     ;;     (let ((pad-string (make-string (max 0 (- min-width
	     ;; 					     display-rep-length))
	     ;; 				   (or (car padding-action) ? ))))
	     ;;       (if (cdr padding-action)
	     ;; 	  (concat pad-string display-rep)
	     ;; 	(concat display-rep pad-string)))))
	     )
	    ((and max-width (> display-rep-length max-width))
	     ;; The display representation is too long.
	     (funcall (or (displayspec-truncation-display-action displayspec)
			  (function ordinary-truncation-function))
		      max-width display-rep display-rep-length)
	     ;; Assume the truncation function did the right thing.
	     (setq display-rep-length max-width)
	     ;; Old code
	     ;; (let ((trunc-action (displayspec-truncation-display-action displayspec)))
	     ;; (cond ((eq 'widen trunc-action)
	     ;;        display-rep)
	     ;;       ((eq 'error trunc-action)
	     ;;        (error "Value %s is too wide; should be between %s and %s characters."
	     ;; 	      display-rep min-width max-width))
	     ;;       ((null trunc-action)
	     ;;        (substring display-rep 0 max-width))
	     ;;       ((integerp trunc-action)
	     ;;        ;; trunc-action is a character
	     ;;        (concat (substring display-rep 0 (1- max-width))
	     ;; 	       trunc-action))
	     ;;       (t
	     ;;        (error "Unrecognized trunc-action %s." trunc-action))))
	     ))
      ;;; From Aborted-2.0
      ;;       (set-text-properties 0 display-rep-length
      ;; 			   (displayspec-text-properties displayspec)
      ;; 			   display-rep)
      display-rep)))

;; Like displayspec->printed-rep, but more so
(defun displayspec->displayed-rep (displayspec record)
  (let ((pr (displayspec->printed-rep dbf-this-displayspec
				      dbf-this-record)))
    (if (displayspec-indent displayspec)
	(if (numberp (displayspec-indent displayspec))
	    (db-string-substitute-substring-general-case
	     (concat "\n" (make-string (dbf-this-field-indent) 32))
	     "\n"
	     pr)
	  ;; Why can't I use (dbf-this-field-indent) even here??
	  (if (db-find-char ?\n pr)
	      (error "Don't know how much to indent.")
	    pr))
      pr)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Data display buffer fontification
;;;

(if db-fontification
    ;; Allowing user to set his own preferences in ~/.Xdefaults
    (progn
      (or (find-face 'db-inter-field-face)
 	  (make-face 'db-inter-field-face))
      (or (face-differs-from-default-p 'db-inter-field-face)
	  (copy-face 'bold 'db-inter-field-face))))

;; This is a bit of a hack.  Leaving out the white space stops the field
;; text from occassionally taking on the 'db-inter-field-face'.  If the
;; user did not use white space the this would evidently not work.

(defun db-fontify (start end)
  "Fontify the region between START and END.  Leave out the leading and
  trailing white space."
  (let (ext-start)
    (save-excursion
      (goto-char start)
      (skip-chars-forward " \t\n")
      (setq ext-start (point))
      (goto-char end)
      (skip-chars-backward " \t\n")
      (if (< ext-start (point))
	  (set-extent-face
	   (make-extent ext-start (point))
	   'db-inter-field-face)))))

;;; Old version
;; (defun db-fontify (start end)
;;   "Fontify the region between START and END.  Leave out the leading and
;; trailing white space."
;;   (let (ext-start)
;;     (save-excursion
;;       (if (< ext-start (point))
;; 	  (set-extent-face
;; 	   (make-extent ext-start (point))
;; 	   'db-inter-field-face)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Etc.
;;;

;; This is useful mostly because it gives help for database mode.

;; It would be nice to have, for the sake of this documentation string,
;; three more keymaps, one each for the bindings unique to edit map, unique
;; to the view map, and common to both.

(defun database-mode ()
  "A mode for viewing and editing formatted data; a database front end.
In Database Edit mode, fields of the database may be changed.
In Database View mode, keystrokes are bound to database commands.
Typically, if point is on a field, the buffer is in Database Edit mode; if
point is at the beginning of the buffer, the buffer is in Database View mode.
The mode line indicates which mode the buffer is in.

Database View mode key bindings:

\\{database-view-mode-map}

Database Edit mode key bindings:

\\{database-edit-mode-map}"

  (setq major-mode 'database-mode
	mode-name "Database"
	buffer-file-name nil
	mode-line-format database-mode-line-format)
  ;; What is the point of this?  We oughtn't be writing out this buffer.
  (make-local-variable 'require-final-newline)
  (setq require-final-newline nil)
  (auto-save-mode 0)
  (db-view-mode))

;; I ought to do something about read-only databases.
(defvar database-mode-line-format
  '("-"
    ;; I don't particularly want the % for read-only.  Or do I?
    ;; "%*"
    (dbc-database-modified-p "*" "-")
    (dbf-this-record-modified-p "*" "-")
    ;; I don't particularly want the % for read-only.
    ;; ((buffer-modified-p) "*" "-")
    "%*"
    "-Database: %17b   %[("
    dbf-minor-mode-name
    minor-mode-alist
    " "
    dbc-index-fraction
    ")%]"
    "---"
    (-3 . "%p")
    "-%-"))


;;   "T if this buffer is a database data display buffer."
(defsubst db-data-display-buffer-p ()
  (eq major-mode 'database-mode))


;;   "T if this buffer is a database data display buffer or database summary buffer."
(defsubst database-buffer-p ()
  (or (db-data-display-buffer-p)
      (db-summary-buffer-p)))

;;; db-format.el ends here
