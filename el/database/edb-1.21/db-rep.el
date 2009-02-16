;;; db-rep.el --- part of EDB, the Emacs database

;; See database.el for copyright notice, distribution conditions, etc.

;; Author: Michael Ernst <mernst@theory.lcs.mit.edu>
;; Keywords: EDB

;;; Commentary:

;; Representation and basic operations for
;; database, link, recordfieldspec objects.

;;; Code:


;; Exactly why are these here?
(require 'db-util)
(provide 'db-rep)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Database abstraction
;;;

;; A database is just a doubly-linked circular list, with some supporting
;; information.

;; See the texinfo file for more complete descriptions of the slots of this
;; structure; the information in this file is intended to jog your memory,
;; not to instruct you.

(defvar databases-made 0)

(defstruct database
  (print-name (progn
		(setq databases-made (1+ databases-made))
		(concat "Unnamed Database " (int-to-string databases-made))))
					; string

  first-link	    ; pointer to a link
  ;; this could be "size" or "length"
  no-of-records	    ; integer.  First link is numbered 1.

  file		    ; filename
  file-local-variables ; string, the local variables section
		    ; of the file from which this db came.
  aux-file	    ; filename
  data-display-buffers ; list of buffers
  default-format-file ; filename

  ;; This does not appear to be used anywhere at all!
  hide-functions

  ;; field information
  no-of-fields	    ; integer
  fieldnames	    ; this is repeated in the recordpsecs
  fieldname-alist   ; alist of (name . number)
  recordfieldspecs  ; vector of symbols or recordfieldspecs
		    ;  if symbol, look up in db-recordfieldtypes

  field-priorities  ; maybe call this order-fields instead
  hidden-to-end-p   ; boolean

  ;; For file i/o
  internal-file-layout-p
  (record-sepinfo (make-sepinfo))
  (field-sepinfo (make-sepinfo))
  (alternative-sepinfo (make-sepinfo))
  read-record-from-region
  write-region-from-record
  sub-fieldsep-string
  sub-recordsep-string

  ;; for i/o conversion (quoting the special strings)
  ;; quotation-char
  ;; quotation-char-regexp
  ;; ;; These are unprocessed; vars are set from them when reading/writing.
  ;; quoted-regexp	    ; if nil, use quoted-strings
  ;; quoted-strings    ; a list; if nil, use function quoted-strings-default
  ;; actual-quoted-regexp ; actually used; user should never set
  substitutions	    ; list of (actual . stored) string pairs

  ;; I'm not convinced these are all that useful.  Maybe add them in later.
  ;; max-field-size
  ;; max-record-size
  ;; pad-with-whitespace

  modified-p
  modifiable-p

  locals	    ; alist of (symbol . value) pairs
  )

;;; Accessors

(defsubst database-set-print-name (db val) (setf (database-print-name db) val))
(defsubst database-set-first-link (db val) (setf (database-first-link db) val))
(defsubst database-set-no-of-records (db val) (setf (database-no-of-records db) val))
(defsubst database-set-file (db val) (setf (database-file db) val))
(defsubst database-set-file-local-variables (db val) (setf (database-file-local-variables db) val))
(defsubst database-set-aux-file (db val) (setf (database-aux-file db) val))
(defsubst database-set-data-display-buffers (db val) (setf (database-data-display-buffers db) val))
(defsubst database-set-default-format-file (db val) (setf (database-default-format-file db) val))
(defsubst database-set-hide-functions (db val) (setf (database-hide-functions db) val))
(defsubst database-set-no-of-fields (db val) (setf (database-no-of-fields db) val))
(defsubst database-set-fieldnames (db val) (setf (database-fieldnames db) val))
(defsubst database-set-fieldname-alist (db val) (setf (database-fieldname-alist db) val))
(defsubst database-set-recordfieldspecs (db val) (setf (database-recordfieldspecs db) val))
(defsubst database-set-field-priorities (db val) (setf (database-field-priorities db) val))
(defsubst database-set-hidden-to-end-p (db val) (setf (database-hidden-to-end-p db) val))
(defsubst database-set-internal-file-layout-p (db val) (setf (database-internal-file-layout-p db) val))
(defsubst database-set-record-sepinfo (db val) (setf (database-record-sepinfo db) val))
(defsubst database-set-field-sepinfo (db val) (setf (database-field-sepinfo db) val))
(defsubst database-set-alternative-sepinfo (db val) (setf (database-alternative-sepinfo db) val))
(defsubst database-set-read-record-from-region (db val) (setf (database-read-record-from-region db) val))
(defsubst database-set-write-region-from-record (db val) (setf (database-write-region-from-record db) val))
(defsubst database-set-sub-fieldsep-string (db val) (setf (database-sub-fieldsep-string db) val))
(defsubst database-set-sub-recordsep-string (db val) (setf (database-sub-recordsep-string db) val))
;; (defsubst database-set-quotation-char (db val) (setf (database-quotation-char db) val))
;; (defsubst database-set-quotation-char-regexp (db val) (setf (database-quotation-char-regexp db) val))
;; (defsubst database-set-quoted-regexp (db val) (setf (database-quoted-regexp db) val))
;; (defsubst database-set-quoted-strings (db val) (setf (database-quoted-strings db) val))
;; (defsubst database-set-actual-quoted-regexp (db val) (setf (database-actual-quoted-regexp db) val))
(defsubst database-set-substitutions (db val) (setf (database-substitutions db) val))
(defsubst database-set-modifiable-p (db val) (setf (database-modifiable-p db) val))

(defsubst database-set-modified-p-internal (db val)
  (setf (database-modified-p db) val))
(defsubst database-set-modified-p (db val)
  (let ((old-val (database-modified-p db)))
    (if (not (eq old-val val))
	(progn
	  (setf (database-modified-p db) val)
	  ;; Reflect the change in each data display buffer.
	  (map-data-display-buffers (lambda (ddb)
				      (in-buffer-simple ddb
					(setq dbc-database-modified-p val)
					(set-buffer-modified-p val)
					(force-mode-line-update)))
				    db)))))

(defsubst database-set-locals (db val) (setf (database-locals db) val))

;;; Constructor

;; I could copy the old one and change some values, or make a new one and
;; copy some values.
;; The latter makes explicit what's being copied; but nearly everything is.
(defun make-similar-database (original)
  ;; Return a database similar to ORIGINAL.
  (let ((result (copy-database original)))
    (db-debug-message "Created result database.")
    (database-set-print-name result
	  (concat "Copy of " (database-print-name original)))
    (db-debug-message "Changed print name.")
    (database-set-first-link result nil)
    (database-set-no-of-records result 0)
    ;; Should this go after choose-format-file?
    (database-set-file result (concat (database-file original) "-COPY"))
    (db-debug-message "Changed filename.")
    ;; Could get info from (car (database-data-display-buffers original)) if
    ;; there is no default-format-file; could also try to infer format name.
    (database-set-data-display-buffers result
	  (list (db-setup-data-display-buffer
		 (choose-format-file result nil nil)
		 result
		 t)))
    (db-debug-message "Created data display buffer.")
    (setq db-databases (cons result db-databases))
    result))

;;; Non-primitive accessors

(defsubst database-last-link (database)
  (link-prev (database-first-link database)))

;; could also use (= 0 (database-no-of-records database)).
(defsubst database-empty-p (database)
  (null (database-first-link database)))

(defsubst database-unnamed-p (database)
  (let ((print-name (database-print-name dbc-database)))
    (or (not print-name)
	(equal "" print-name)
	(equal "Unnamed Database "
		      (substring print-name 0 (min 17 (length print-name)))))))

(defsubst database-list-of-links (database)
  (maplinks (function identity)
	    database nil nil t))

;;; Database-local variables

;; Possibly get rid of database-make-local altogether and make no-error
;; behavior the default in database-{gs}et-local.  Usually reliable sources
;; inform me that right now they're somewhat of a pain to use.

(defun database-make-local (symbol database &optional value)
  "Declare a database-local variable named by SYMBOL for DATABASE.
Each such variable should only be declared once.
If optional argument VALUE is specified, the variable is set to it."
  (let ((lookup (assq symbol (database-locals database))))
    (if lookup
	(error "%s is already defined as a local variable in %s."
	       symbol (database-print-name database))
      (database-set-locals database (cons (cons symbol value)
					  (database-locals database))))))

(defun database-set-local (symbol database value &optional no-error)
  "Set the value of database-local variable SYMBOL, in DATABASE, to VALUE.
SYMBOL must have been declared by a previous call to `database-make-local'
unless optional argument NO-ERROR is supplied, in which case the function
does that automatically, if necessary."
  (let ((lookup (assq symbol (database-locals database))))
    (if lookup
	(setcdr lookup value)
      (if no-error
	  (database-make-local symbol database value)
	(error "%s is not a database-local variable for %s."
	       symbol (database-print-name database))))))

(defun database-get-local (symbol database &optional no-error)
  "Return the value of database-local variable SYMBOL for DATABASE.
If SYMBOL was not declared by a previous call to `database-make-local',
an error is signalled unless optional argument NO-ERROR is non-nil,
in which case nil is returned."
  (let ((lookup (assq symbol (database-locals database))))
    (cond (lookup
	   (cdr lookup))
	  (no-error
	   nil)
	  (t
	   (error "%s is not a database-local variable for %s."
		  symbol (database-print-name database))))))

(defun database-local-p (symbol database)
  "Return non-nil if SYMBOL is a database-local variable for DATABASE."
  (assq symbol (database-locals database)))


;;; Non-primitive setters

;; This is very close to db-set-fieldname-vars now.
(defun database-set-fieldnames-to-list (database fieldnames-list)
  "Set DATABASE's fieldnames and record field types according to FIELDNAMES-LIST.
Users should not call `database-set-fieldnames' directly.
FIELDNAMES-LIST is a list of fieldnames (symbols); each list element may
instead be a cons of fieldname and type to specify the field's
recordfieldtype as well.  If no type is specified for a field, the value of
`db-default-field-type' is used.

This function sets several database slots besides the fieldnames slot, but
has no effect if the fieldnames slot of the database is already set."
  (if (not (database-fieldnames database))
      (db-set-fieldname-vars database fieldnames-list)))

;;; Basic functions

(defsubst database-index-in-range (index database)
  (and (> index 0) (<= index (database-no-of-records database))))

;; Make INDEX be in the range 1 to (database-no-of-records database).
(defsubst database-normalize-index (index database)
  (let ((remainder (% index (database-no-of-records database))))
    (if (zerop remainder) (database-no-of-records database) remainder)))

;;   "Return the link of DATABASE at index N.  The first link is numbered 1."
(defsubst database-link (database n)
  (car (database-link-and-index database n nil nil)))

;;; Not quite so basic functions.

;; The string that really separates database record fields.
(defun database-full-fieldsep-string (database)
  (if (database-write-region-from-record database)
      ;; might the write-record-function want to access this value?  I think not.
      nil
    (let ((field-sepinfo (database-field-sepinfo database)))
      (sepinfo-sep-string field-sepinfo))))

;; The string that really separates database records.
(defun database-full-recordsep-string (database)
  (let ((record-sepinfo (database-record-sepinfo database)))
    (if (database-write-region-from-record database)
	(sepinfo-sep-string record-sepinfo)
      (let ((field-sepinfo (database-field-sepinfo database)))
	(concat (sepinfo-post-last-string field-sepinfo)
		(sepinfo-sep-string record-sepinfo)
		(sepinfo-pre-first-string field-sepinfo))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Link abstraction
;;;

;; Perhaps marks should be format-local.  The current implementation makes
;; them database-local and so visible in all formats.

(defstruct (link (:constructor old-make-link))
  prev
  next
  markedp
  hiddenp
  summary
  record)

(defsubst link-set-prev (l val) (setf (link-prev l) val))
(defsubst link-set-next (l val) (setf (link-next l) val))
(defsubst link-set-markedp (l val) (setf (link-markedp l) val))
(defsubst link-set-hiddenp (l val) (setf (link-hiddenp l) val))
(defsubst link-set-summary (l val) (setf (link-summary l) val))
;; There is hidden work happening here.  This should also set link-hiddenp.
(defsubst link-set-record (link result)
  (setf (link-record link) result)
  (link-set-summary link nil))


;; I don't know that I should be using this often; the old record will get
;; garbage-collected.  In fact, I can't think of an occasion when I should
;; use it.

(defun make-link ()			;(&rest args)
  (error "Should use make-link-from-record instead."))
(make-obsolete 'make-link 'make-link-from-record)

;;   "Place LINK1 and LINK2 in a prev-next relationship."
(defsubst link-two (link1 link2)
  (link-set-next link1 link2)
  (link-set-prev link2 link1))

;; I oughtn't ever have to use the standard make-link procedure.  Maybe even
;; have defstruct make this the standard.
(defun make-link-from-record (record)
  (let ((result (old-make-link)))
    (link-set-record result record)
    result))

;; Beware of cleverer implementations; maplinks-macro always returns nil.
;;   "Return a list of (link index) for the link containing RECORD in DATABASE.
;; Return nil if there is no such link."
(defun record->link-and-index (record database)
  (let (result)
    (maplinks-macro
     (if (eq record (link-record maplinks-link))
	 (progn
	   (setq result (list maplinks-link maplinks-index))
	   (maplinks-break)))
     database nil)
    result))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Recordfieldspec abstraction
;;;

;; This should be called a contentspec or something; it has to do only with
;; the value of the field.  This means that name and printname probably
;; don't belong.  Does type?  And a->s and s->a are questionable:  they're
;; only used when reading and writing variables, but then again, why not
;; keep them here instead of cluttering up the database object even more?
;; They probably belong here.

;; Now instead of one recordfieldspec per database, there's one per field.  Two
;; values were moved into the database, and the database also contains a
;; vector of recordfieldspecs.


(defstruct recordfieldspec

  ;; datatype information
  type					; eg 'string
  default-value
  common-form-function
  merge-function

  ;; {order,sort}-fn are shadowed by the pseudoslots {order,sort}-function.
  order-fn				; takes two objects
  sort-fn				; takes two objects
  match-function			; takes a pattern and an object

  help-info				; perhaps should be help-string;
					; or should be more complicated.

  ;; Quite possibly these belong in the database; why should the
  ;; recordfieldspec care?  [Becuase it has to do with actual values and
  ;; manipulations thereof?]
  actual->stored
  stored->actual

  ;; customizations
  change-hook				; not currently used
  constraint-function
  )

(defsubst recordfieldspec-set-type (rs val) (setf (recordfieldspec-type rs) val))
(defsubst recordfieldspec-set-default-value (rs val) (setf (recordfieldspec-default-value rs) val))
(defsubst recordfieldspec-set-common-form-function (rs val) (setf (recordfieldspec-common-form-function rs) val))
(defsubst recordfieldspec-set-merge-function (rs val) (setf (recordfieldspec-merge-function rs) val))
(defsubst recordfieldspec-set-order-fn (rs val) (setf (recordfieldspec-order-fn rs) val))
(defsubst recordfieldspec-set-sort-fn (rs val) (setf (recordfieldspec-sort-fn rs) val))
(defsubst recordfieldspec-set-match-function (rs val) (setf (recordfieldspec-match-function rs) val))
(defsubst recordfieldspec-set-help-info (rs val) (setf (recordfieldspec-help-info rs) val))
(defsubst recordfieldspec-set-actual->stored (rs val) (setf (recordfieldspec-actual->stored rs) val))
(defsubst recordfieldspec-set-stored->actual (rs val) (setf (recordfieldspec-stored->actual rs) val))
(defsubst recordfieldspec-set-change-hook (rs val) (setf (recordfieldspec-change-hook rs) val))
(defsubst recordfieldspec-set-constraint-function (rs val) (setf (recordfieldspec-constraint-function rs) val))


;; Order-function and sort-function pseudo-slots

(defsubst recordfieldspec-set-order-function (rs val) (setf (recordfieldspec-order-fn rs) val))
(defsubst recordfieldspec-set-sort-function (rs val) (setf (recordfieldspec-sort-fn rs) val))


;; Perhaps the functions returned here should be byte-compiled (ie, call
;; byte-compile on the result to be returned), at least when they're consed
;; up at run-time.

(defun recordfieldspec-sort-function (recordfieldspec &optional reversep)
  "Return a sort function for records described by RECORDFIELDSPEC.
If optional argument REVERSEP is non-nil, then the sort function goes in
the opposite order.
If the sort-fn slot of the appropriate recordfieldspec of  database  doesn't
contain one, one is made up on the fly from the order-fn slot.
If the order-fn slot is also empty, the resulting function always returns
nil, indicating that it is not the case that the first argument is less
than the second."
  (let ((sort-fn (recordfieldspec-sort-fn recordfieldspec)))
    (if sort-fn
	(if reversep
	    ;; (list 'lambda '(value1 value2)
	    ;; 	  (list sort-fn 'value2 'value1))
	    (` (lambda (value1 value2)
		 ((, sort-fn) value2 value1)))
	  sort-fn)
      (order->sort (recordfieldspec-order-fn recordfieldspec) reversep))))

;;   "Given an order function, return a sort function."
(defun order->sort (order-fn reversep)
  (if order-fn
      (list 'lambda (if reversep '(value2 value1) '(value1 value2))
	    (list '= -1
		  (list 'funcall (list 'function order-fn)
			'value1 'value2)))
    (function nil-function)))

(defun recordfieldspec-order-function (recordfieldspec &optional reversep)
  "Return an order function for records described by RECORDFIELDSPEC.
If optional argument REVERSEP is non-nil, then the order function goes in
the opposite order.
If the order-fn slot of the appropriate recordfieldspec of  database  doesn't
contain one, one is made up on the fly from the sort-fn slot; `equal'
is used to determine whether two records are equal.
If the sort-fn slot is also empty, the resulting function always
returns 0, indicating equality."
  (let ((order-fn (recordfieldspec-order-fn recordfieldspec)))
    (if order-fn
	(if reversep
	    (` (lambda (value1 value2)
		 ((, order-fn) value2 value1)))
	  order-fn)
      (sort->order (recordfieldspec-sort-fn recordfieldspec) reversep))))

;;   "Given a sort function, return an order function."
(defun sort->order (sort-fn reversep)
  (if sort-fn
      (` (lambda (, (if reversep '(value2 value1) '(value1 value2)))
	   (cond ((equal value1 value2)
		  0)
		 ((funcall (function (, sort-fn))
		   value1 value2)
		  -1)
		 (t
		  1))))

    (function (lambda (value1 value2) 0))))


;;   "Return t if the databases' recordfieldspecs have the same field names and type."
(defun recordfieldspecs-compatible (db1 db2)
  (let ((result t)
	(fno 0)
	(fields1 (length (database-recordfieldspecs db1)))
	(fields2 (length (database-recordfieldspecs db2)))
	recordfieldspec1
	recordfieldspec2)
    (if (= fields1 fields2)
	(progn
	  (while (and result (< fno fields1))
	    (setq recordfieldspec1 (database-recordfieldspec db1 fno)
		  recordfieldspec2 (database-recordfieldspec db2 fno)
		  ;; used to also check recordfieldspec-name here
		  result (eq (recordfieldspec-type recordfieldspec1)
			     (recordfieldspec-type recordfieldspec2))
		  fno (1+ fno)))
	  result))))

(defun database-recordfieldspec (database record-index)
  "Return the recordfieldspec of DATABASE corresponding to RECORD-INDEX.
Dereferences via `recordfieldtype->recordfieldspec' any symbol found in the
recordfieldspecs slot of DATABASE."
  (let ((rs (aref (database-recordfieldspecs database) record-index)))
    (cond ((symbolp rs)
	   (recordfieldtype->recordfieldspec rs))
	  ((recordfieldspec-p rs)
	   rs)
	  (t
	   (error "database-recordfieldspec:  rs = %s" rs)))))

(defun database-set-recordfieldspec (database record-index rs)
  "Set the recordfieldspec of DATABASE corresponding to RECORD-INDEX to RS.
Use this to redefine, on a per-field basis, subfields of the recordfieldspec."
  (aset (database-recordfieldspecs database) record-index
	(cond ((symbolp rs)
	       (recordfieldtype->recordfieldspec rs))
	      ((recordfieldspec-p rs)
	       rs)
	      (t
	       (error "database-set-recordfieldspec: Bad spec: rs= %s" rs)))))

(defun database-recordfieldspec-type (database record-index)
  "Return the type of the recordfieldspec of DATABASE corresponding to RECORD-INDEX."
  (let ((rs (aref (database-recordfieldspecs database) record-index)))
    (cond ((symbolp rs)
	   rs)
	  ((recordfieldspec-p rs)
	   (recordfieldspec-type rs))
	  (t
	   (error "database-recordfieldspec:  rs = %s" rs)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Records
;;;

;; Abstraction

(defsubst make-record (database)
  "Return a record with number of fields specified by argument DATABASE."
  (make-vector (database-no-of-fields database) nil))

(defsubst copy-record (record)
  "Return a copy of RECORD."
  (copy-sequence record))

(defun copy-record-to-record (source target)
  "Copy the field values of the SOURCE record to the TARGET record."
  (let ((fno 0)
	(fields (length source)))
    (while (< fno fields)
      (aset target fno (aref source fno))
      (setq fno (1+ fno)))))

;;; Fieldnames and record fieldnumbers

(defsubst fieldname->fieldnumber (fieldname database)
  "Given a FIELDNAME and DATABASE, return a record fieldnumber.
Do not be fooled into thinking this is a format fieldnumber."
  (cdr (assq fieldname (database-fieldname-alist database))))

(defsubst fieldnumber->fieldname (fieldnumber &optional database)
  "Given a record FIELDNUMBER and DATABASE, return a record fieldname.
If DATABASE is not specified, the value of `dbc-database' is used.
The first argument is not a format fieldnumber."
  (aref (database-fieldnames (or database dbc-database)) fieldnumber))

;;; Retrieving field values

(defsubst record-field-from-index (record fieldno)
  "Return from RECORD the value of the FIELDNOth field."
  (aref record fieldno))

(defsubst record-field (record fieldname database)
  "Return from RECORD the field with name FIELDNAME.  Third argument is DATABASE."
  (let ((fieldnumber (fieldname->fieldnumber fieldname database)))
    (if fieldnumber
	(record-field-from-index record fieldnumber)
      (error "No %s field in current record." fieldname))))

;; One should check dbf-this-record-modified-p before using dbf-this-record.
(defsubst dbf-this-record-field (fieldname)
  "Return the value of the field with name FIELDNAME from `dbf-this-record'.
You may want to use `dbf-displayed-record-field' instead."
  (record-field dbf-this-record fieldname dbc-database))

;;; Checking constraints

(defun record-check-constraint (field-value record record-index database)
  (let ((constraint (recordfieldspec-constraint-function
		     (database-recordfieldspec database record-index))))
    (if (and constraint
	     (not (funcall constraint field-value record record-index database)))
	(error "The value `%s' does not satisfy the constraint for field %s."
	       field-value (fieldnumber->fieldname record-index database)))))

;;; Setting field values

(defsubst record-set-field-from-index (record fieldno value database)
  "Set, in RECORD, the FIELDNOth field to VALUE.
Checks field constraints first if DATABASE is non-nil."
  (if database
      (record-check-constraint value record fieldno database))
  (aset record fieldno value))

(defun record-set-field (record fieldname value database &optional nocheck)
  "Set, in RECORD, field FIELDNAME to VALUE.  Fourth argument is DATABASE.
Check constraints first unless optional fifth argument NOCHECK is non-nil."
  (let ((fieldnumber (fieldname->fieldnumber fieldname database)))
    (if fieldnumber
	(record-set-field-from-index
	 record fieldnumber value (and (not nocheck) database))
      (error "No %s field in current record." fieldname))))


;;; Setting fields in dbf-this-record

;; One should check dbf-this-record-modified-p before using dbf-this-record.
(defsubst dbf-this-record-set-field (fieldname value)
  "Set field with name FIELDNAME in `dbf-this-record' to VALUE.
Causes the entire record to be redisplayed pretty soon.
You may want to use `dbf-displayed-record-set-field' instead."
  (record-set-field dbf-this-record fieldname value dbc-database)
  (setq dbf-redisplay-entire-record-p t))

;; One should check dbf-this-record-modified-p before using dbf-this-record.
(defsubst dbf-this-record-set-field-and-redisplay (fieldname value)
  "Set field with name FIELDNAME in `dbf-this-record' to VALUE.
Causes the entire record to be redisplayed immediately.
You may want to use `dbf-displayed-record-set-field-and-redisplay' instead."
  (dbf-this-record-set-field fieldname value)
  (dbf-redisplay-entire-record-maybe))
(make-obsolete 'dbf-this-record-set-field-and-redisplay 'dbf-displayed-record-set-field-and-redisplay)

;;; The displayed record

(defsubst dbf-displayed-record-field (fieldname)
  "Return the value of the field named FIELDNAME from the displayed record."
  (record-field (dbf-displayed-record) fieldname dbc-database))

(defun dbf-displayed-record-set-field (fieldname value)
  "Set field with name FIELDNAME in displayed record to VALUE.
Cause the entire record to be redisplayed soon."
  ;; This call guarantees that displayed-record = this-record.
  (dbf-set-this-record-modified-p t)
  (dbf-this-record-set-field fieldname value))

(defsubst dbf-displayed-record-set-field-and-redisplay (fieldname value)
  "Set field with name FIELDNAME in displayed record to VALUE.
Cause the entire record to be redisplayed immediately."
  ;; Is this call correct?  Maybe displayed-record != this-record.
  (dbf-this-record-set-field fieldname value)
  (dbf-redisplay-entire-record-maybe))

;;; Mapping

;; Since EDB contains one use of both of these macros combined, perhaps I
;; don't really need them.

(defvar mapfields-field nil
  "The current field value in a call to `mapfields-macro'.")
(defvar mapfields-index nil
  "The current field index in a call to `mapfields' or `mapfields-macro'.")

;; I don't know where in the EDB source files this really belongs.
(defmacro mapfields (func record database)
  "Apply FUNC to each field in RECORD, with variable `mapfields-index' bound.
Third argument is DATABASE."
  (` (let ((mapfields-index 0)
	   (mapfields-record (, record))
	   (mapfields-fields (database-no-of-fields (, database))))
       (while (< mapfields-index mapfields-fields)
	 (funcall (, func)
		  (record-field-from-index mapfields-record mapfields-index))
	 (setq mapfields-index (1+ mapfields-index))))))

;; ;;  "Like mapfields, but also binds  mapfields-name."
;; (defmacro mapfields-name (func record database)
;;   ;; use fieldno->fieldname
;;   (error "Not yet implemented; if you really need it, ask me."))

(defmacro mapfields-macro (body record database)
  "Execute BODY for each field of RECORD, a record of DATABASE,
with variables `mapfields-field' and `mapfields-index' bound."
  (` (let ((mapfields-index 0)
	   (mapfields-record (, record))
	   (mapfields-fields (database-no-of-fields (, database)))
	   mapfields-field)
       (while (< this-field-index field-index-max)
	 (setq mapfields-field
	       (record-field-from-index this-record this-field-index))
	 ;; BODY must be a single form since it's the first argument
	 (, body)
	 (setq this-field-index (1+ mapfields-index))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Sepinfo abstraction
;;;

;; This tells how a list of information appears in a file.  See the texinfo
;; documentation for more details.

;; Would it be more natural to have a function that finds the extent of a
;; record rather than of the separating space?  Well, that can probably be
;; a regexp most of the time anyway...

(defstruct sepinfo
  pre-first-string
  pre-first-regexp
  pre-first-regexp-submatch
  sep-string
  sep-regexp
  sep-regexp-submatch
  sep-function				; returns (end-pos . next-start-pos) pair
					; takes prev-end-pos as an argument
					; next-start-pos is nil for last record
  post-last-string
  post-last-regexp
  post-last-regexp-submatch)


(defsubst sepinfo-set-pre-first-string (si val) (setf (sepinfo-pre-first-string si) val))
(defsubst sepinfo-set-pre-first-regexp (si val) (setf (sepinfo-pre-first-regexp si) val))
(defsubst sepinfo-set-pre-first-regexp-submatch (si val) (setf (sepinfo-pre-first-regexp-submatch si) val))
(defsubst sepinfo-set-sep-string (si val) (setf (sepinfo-sep-string si) val))
(defsubst sepinfo-set-sep-regexp (si val) (setf (sepinfo-sep-regexp si) val))
(defsubst sepinfo-set-sep-regexp-submatch (si val) (setf (sepinfo-sep-regexp-submatch si) val))
(defsubst sepinfo-set-sep-function (si val) (setf (sepinfo-sep-function si) val))
(defsubst sepinfo-set-post-last-string (si val) (setf (sepinfo-post-last-string si) val))
(defsubst sepinfo-set-post-last-regexp (si val) (setf (sepinfo-post-last-regexp si) val))
(defsubst sepinfo-set-post-last-regexp-submatch (si val) (setf (sepinfo-post-last-regexp-submatch si) val))

;; The user is asked to remember to set the -regexp slot to nil when
;; he sets the string.  Another possibility is a function that does this
;; for him, but he'd have to remember to call that just as he remembers to
;; do the setf himself, and it could be confusing to have two different
;; methods for setting the slot value with slightly different semantics.
;; Yet another possibility is a set of secret regexp slots in the sepinfo;
;; these are the ones that are really used, and they're set from the
;; visible string and regexp slots.  (They can always be safely blown away
;; and set again without danger of throwing away a user-set value.)  But
;; this would double the size of the sepinfo and would be conceptually
;; ugly.


;; Can't have the call to function here; that screws things up.
(defun make-n-line-sep-function (n)
  "Return a sep-function useful when all records have exactly N lines on disk."
  (` (lambda (prev-end)
       (forward-line (, n))
       (cons (point) (if (not (eobp)) (point))))))


;; If the variable is the empty string, I should set it to nil.

;; Should let the pre-first stuff be optional:  if it's not there, then the
;; whole thing is one value, and if it is there, then there are multiple
;; items present.

;; To indicate the region of the buffer to be read, we could use
;; locations/marks, or we could do narrowing.  We choose the former.
;; Body-func is repeatedly called with two buffer position arguments:
;; the start and end of the region it's to operate upon.
(defun read-sep-items (sepinfo beg end body-func)
  (db-debug-message "read-sep-items from %s to %s." beg end)
  (let* ((post-last-item-pos
	  (progn
	    (goto-char beg)
	    ;; 	   (if (sepinfo-post-last-regexp sepinfo)
	    ;; 	       (db-debug-message "post-last-regexp %s found at %s"
	    ;; 			(sepinfo-post-last-regexp sepinfo)
	    ;; 			(re-search-forward-maybe (sepinfo-post-last-regexp sepinfo) end t)))

	    (if (and (sepinfo-post-last-regexp sepinfo)
		     (re-search-forward (sepinfo-post-last-regexp sepinfo) end t))
		(progn
		  (db-debug-message "found post-last at %s (vs %s)"
			   (match-beginning (sepinfo-post-last-regexp-submatch sepinfo))
			   end)
		  (match-beginning (sepinfo-post-last-regexp-submatch sepinfo)))
	      end)))
	 (start-of-this-item
	  (progn
	    (goto-char beg)
	    (if (sepinfo-pre-first-regexp sepinfo)
		(db-skip-regexp-forward (sepinfo-pre-first-regexp sepinfo)
				     (sepinfo-pre-first-regexp-submatch sepinfo)))
	    (if (< (point) post-last-item-pos)
		(point))))
	 end-of-this-item
	 start-of-next-item)
    (while start-of-this-item
      (goto-char start-of-this-item)
      (if (sepinfo-sep-function sepinfo)
	  (let ((end-start (funcall (sepinfo-sep-function sepinfo)
				    post-last-item-pos)))
	    (setq end-of-this-item (car end-start)
		  start-of-next-item (cdr end-start)))
	(if (re-search-forward (sepinfo-sep-regexp sepinfo)
			       post-last-item-pos t)
	    (setq end-of-this-item
		  (match-beginning (sepinfo-sep-regexp-submatch sepinfo))
		  start-of-next-item
		  (match-end (sepinfo-sep-regexp-submatch sepinfo)))
	  (setq end-of-this-item post-last-item-pos
		start-of-next-item nil)))
      (funcall body-func start-of-this-item end-of-this-item)
      (setq start-of-this-item start-of-next-item))))


;; I sorta want this to take an item at a time and produce a bit of output
;; at a time, but I need to know when I have the last item so that I can
;; add post-last instead of sep after it.  So there also needs to be a way
;; of indicating "that last item was the last one".

;; body could return:
;;  * t if wrote something, nil otherwise
;;  * t if this was the last one, nil otherwise (always write something)
;; The latter would be more convenient for this function; which is more
;; convenient for body?

;; Two possible approaches:
;;  * have something that accepts a producer.
;;  * have something I can call repeatedly (in mapcar or maplinks, for example)
;;    and which would accept additional arguments to tell when a new list was
;;    being started/ended/whatever.

(defun write-sep-items (sepinfo producer)

  ;; write pre-

  ;; repeat:
  ;;   call body, which should write to the buffer and return t (if it wrote
  ;;     anything) or nil (if it didn't)
  ;;   if body returned t

  ;; not first time through, write sep.

  ;; write post-

  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Fieldnumbers vs. fieldnames philosophy
;;;

;; I will try to use numbers whereever possible and everywhere that the
;; user can't see them; the user will be able to use fieldnames, naturally.
;; I should always know whether I'm dealing with a fieldname or a
;; fieldnumber.  When the user gives me a fieldname, I should convert it to
;; a fieldnumber right away.


;; Proposed solution:

;; Provide macros for user code (eg (record-fieldname1), (record-fieldname2
;; record)), so I don't have to compile it down but it executes efficiently
;; anyway.  (User may want access to slower accessors that take fieldname
;; as an argument as well; that will be easy to implement.  Eg,
;; (record-field 'fieldname [record]).)  Warn the user:  if byte-compiling
;; code that uses the macros, better make darn sure that the macros in
;; effect are those of the database that you're worried about, or that
;; there are no macros defined.

;; In my own code, always pass around fieldnumbers.  I don't see how to
;; call macros except via eval [which probably isn't efficient enough for
;; the main field-getting routines; the user code must be eval'ed in any
;; case, but I don't want eval in my code], I don't want to have to define
;; all the functions by hand [that's a pretty weak reason], and field
;; numbers will be more efficient anyway.  Will have to have a
;; fieldnumber-fieldname assoc list anyway (for the non-macro user-level
;; accessors), so can determine fieldnames from fieldnumbers.

;; How to switch the macros when I switch databases (or switch buffers)?
;; [But wait:  will there be buffer-local functions in Emacs 19?  If so, no
;; such worries.  Even in 18 I could bind the functions to variables and
;; then funcall.  (No, I can't.  The point of these is convenience for the
;; user, you blockhead!)]  Will there be a select-buffer-hook?  Could
;; constantly check db-name-for-record-defstruct against current-db-name.
;; The switching will be done via a simple defstruct (though unfortunately
;; that doesn't undefine the old accessors).  While the accessors don't
;; check the type of their argument, having different structure names could
;; still be a win since the bad ones won't be defined.  Or I could just do
;; the defining and undefining myself:  while no simpler, it wouldn't be
;; overly complex, and I could undefine the obsolete accessors.  (And it
;; could all be functions instead of macros, which might be nice:  no
;; compilation worries.)  For now, defstruct; later, maybe something else.

;; Compiling down is bad:
;; * can't be sure to get everything since arbitrary Lisp expressions may
;;   define database-accessing stuff (could even appear in many files).
;; * may be inefficient to compile down user-level code on the fly
;;   [probably not too bad, though:  if compiling down is worse than
;;   executing, then neither can be all that tough]
;; * implementing it sounds like work.
;; * may not know fieldnames yet when some code is seen, or the fielnames
;;   might change later (but before we load the database proper).


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Moving from record to record
;;;

;; None of these functions have anything to do with the display; formatting
;; the current record is done elsewhere.  These functions don't even set
;; dbc-link and dbc-index.


;; ;; Will it ever be the case that I know a link number without being at the
;; ;; link?  I suspect that this will be very rare and that typically it will
;; ;; be fastest to go in the direction of n since it will usually be small.
;; ;; Besides, I can't be clever if hiding is on.
;; 
;; ;; Perhaps I should check for the special case of going to the last record;
;; ;; but if I know I'm doing that, then I might as well not use these
;; ;; functions at all, and if I don't, then I'm probably in the usual case
;; ;; anyway.


;; I could pass in the number of database records instead and make the
;; wraparound tests numeric instead of symbolic.  I need that number
;; regardless, for the last mod.

;; This doesn't check that it isn't looping forever.  Maybe it should.

;; Is the polarity of wraparoundp wrong??  That is, does it do the wrong
;; thing, or should it be renamed to no-wraparoundp?

;; ;; Should I have two versions of this, one that doesn't take the last three
;; ;; arguments and one that does?  Nah, this isn't that much slower than that
;; ;; would be, and besides, I don't want to maintain two versions of this.

;;   "Arguments DATABASE LINK LINK-INDEX N HIDEP MARKP WRAPAROUNDP.
;; Return a list of (link index) for Nth successor of LINK, whose index in
;; the database is LINK-INDEX.  N may be negative.  If MARKEDP is non-nil, find
;; the Nth marked successor of LINK.  If HIDEP is non-nil, skip hidden links.
;; If WRAPAROUNDP is nil, stop at the first or last candidate link (ie,
;; properly marked and/or non-hidden, or LINK itself if no such encountered);
;; if the end of the database stops the search in this way, the returned list
;; also contains a third element, the number of elements yet to go."
(defun next-link-and-index (database link link-index n hidep markedp wraparoundp)

  ;; Recent-link and recent-index are used only if hidep or markedp is true.
  ;; They are used to remember the last acceptable link, in case we examine
  ;; every link.
  (let ((recent-link link)
	(recent-index link-index)
	(final-link (and wraparoundp
			(if (> n 0)
			    (database-last-link database)
			  (database-first-link database)))))
    (while (and (> n 0) (not (eq link final-link)))
      (setq link (link-next link)
	    link-index (1+ link-index))
      (if (or hidep markedp)
	  (if (not (or (and hidep (link-hiddenp link))
		       (and markedp (not (link-markedp link)))))
	      ;; This link passes the tests.
	      (setq recent-link link
		    recent-index link-index
		    n (1- n))
	    ;; This link failed the tests.
	    ;; I could add infinite-loop testing here.
	    )
	(setq n (1- n)))
      )
    (while (and (< n 0) (not (eq link final-link)))
      (setq link (link-prev link)
	    link-index (1- link-index))
      (if (or hidep markedp)
	  (if (not (or (and hidep (link-hiddenp link))
		       (and markedp (not (link-markedp link)))))
	      ;; This link passes the tests.
	      (setq recent-link link
		    recent-index link-index
		    n (1+ n))
	    ;; This link failed the tests.
	    ;; I could add infinite-loop testing here.
	    )
	(setq n (1+ n))))
    (if (not (zerop n))
	(if (or hidep markedp)
	    (list recent-link (database-normalize-index recent-index database) n)
	  (list link (database-normalize-index link-index database) n))
      (list link (database-normalize-index link-index database)))))

;; Note that here I can't be clever about counting from the end because of
;; hiding.

;;   "Return a cons of (link . index) for the link of DATABASE with index N.
;; If HIDEP is non-nil, then hidden links are skipped.
;; If MARKEDP is non-nil, only marked links count.
;; 
;; This shouldn't be used for going to fixed points in the database (like the
;; last record), both because hiding may change its semantics and
;; because, in that case, just using (link-prev (database-first-link database))
;; and (database-no-of-records database) is more efficient."
(defsubst database-link-and-index (database n hidep markedp)
  (next-link-and-index database
		       (database-first-link database) 1
		       (1- n) hidep markedp nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Manipulating database records
;;;

;;   "Add RECORD to DATABASE.  If optional third argument LOCATION is a
;; number, insert immediately before that index; if it is nil, insert at the
;; end; if it is t, insert in order."
(defun database-add-record (record database &optional location)

  (db-debug-message "database-add-record:  %s" record)
  (let ((this-link (make-link-from-record record)))
    (db-debug-message "database-add-record:  this-link = %s" this-link)
    (if (database-empty-p database)
	(progn
	  (db-debug-message "database-add-record:  empty database")
	  (link-two this-link this-link)
	  (database-set-first-link database this-link))
      (let* ((afterlink (if (numberp location)
			    (database-link database location)
			  (database-first-link database)))
	     (foo (db-debug-message "database-add-record:  afterlink set"))
	     (beforelink (link-prev afterlink)))
	(db-debug-message "database-add-record:  nonempty database")
	(link-two this-link afterlink)
	(link-two beforelink this-link)
	(if (equal 1 location)
	    (database-set-first-link database
		  this-link))))
    (database-set-no-of-records database
	  (1+ (database-no-of-records database)))))


(defun make-default-record (database)
  (let ((record (make-record database))
	(fno 0))
    (while (< fno (database-no-of-fields database))
      (record-set-field-from-index record fno
				   (recordfieldspec-default-value
				    (database-recordfieldspec database fno))
				   nil)
      (setq fno (1+ fno)))
    record))


(defun database-delete-link (database link)
  (if (eq (link-next link) link)
      ;; This is the only link in the database.
      (database-set-first-link database nil)
    (progn
      (if (eq link (database-first-link database))
	  (database-set-first-link database
		(link-next link)))
      (link-two (link-prev link) (link-next link))))
  (database-set-no-of-records database
	(1- (database-no-of-records database))))

(defun database-delete-record-at-index (database record-index)

  ;; ...

  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mapping over a database
;;;

;; The mapping functions dynamically bind maplinks-link and maplinks-index.

(defvar maplinks-link nil
  "The current link in a call to `maplinks' or `maplinks-macro'.")
(defvar maplinks-index nil
  "The current index in a call to `maplinks' or `maplinks-macro'.")

(defmacro maplinks-break ()
  "Cause the maplinks loop to quit after executing the current iteration.
This is not a nonlocal exit!  It sets a flag which prevents future iterations.
\(Actually, it sets variable `maplinks-link'.\)"
  (` (setq maplinks-link (link-prev first-link))))

(defun maplinks (maplinks-func database &optional hide message accumulate)
  "Apply MAPLINKS-FUNC to every link in DATABASE.
If optional third arg HIDE is non-nil, apply MAPLINKS-FUNC only to unhidden links.
If optional fourth arg MESSAGE is non-nil, it should be a format string
containing one numeric \(%d\) specifier.  That message will be issued every
`db-inform-interval' links.
If optional fifth arg ACCUMULATE is non-nil, return a list of the results;
otherwise return nil.

In the body, variable `maplinks-index' is bound to the index of the link being
operated upon, and `maplinks-link' is the argument to MAPLINKS-FUNC.
The loop may be short-circuited (aborted) by calling `maplinks-break'.
To avoid the per-link function call overhead, use `maplinks-macro' instead."
  (let* ((first-link (database-first-link database))
	 (maplinks-link first-link)
	 (maplinks-index 1)
	 results
	 (not-done t))
    (setq message (and db-inform-interval message))
    (if first-link
	(while not-done
	  (if (not (and hide (link-hiddenp maplinks-link)))
	      (if accumulate
		  (setq results
			(cons (funcall maplinks-func maplinks-link) results))
		(funcall maplinks-func maplinks-link)))
	  (if (and message
		   ;; No test for db-inform-interval because of (setq message )
		   ;; db-inform-interval
		   (zerop (% maplinks-index db-inform-interval)))
	      (db-message message maplinks-index))
	  (setq maplinks-link (link-next maplinks-link)
		maplinks-index (1+ maplinks-index))
	  (if (eq maplinks-link first-link)
	      (setq not-done nil))))
    (if accumulate
	(nreverse results))))

(defmacro maplinks-macro (maplinks-body database &optional hide message)
  "Execute MAPLINKS-BODY for each link in DATABASE, and return nil.
If optional third arg HIDE is non-nil, execute MAPLINKS-BODY only for unhidden links.
If optional fourth arg MESSAGE is non-nil, it should be a format string
containing one numeric \(%d\) specifier.  That message will be issued every
`db-inform-interval' links.

In the body, variable `maplinks-link' is bound to the link being operated upon,
and `maplinks-index' is bound to its index.
The loop may be short-circuited (aborted) by calling `maplinks-break'.
Speed demons should call this instead of `maplinks' to avoid a function call
overhead per link."
  (` (let* ((,@ (if hide (` ((hidep (, hide))))))
	    (,@ (if message
		    (` ((message-evalled (and db-inform-interval (, message)))))))
	    (first-link (database-first-link (, database)))
	    (maplinks-link first-link)
	    (maplinks-index 1)
	    (maplinks-not-done t))
       (,@ (if hide '((db-debug-message "maplinks-macro:  hidep = %s" hidep))))
       (if first-link
	   (while maplinks-not-done
	     (progn
	       (, (if hide
		      (` (if (not (and hidep
				       (link-hiddenp maplinks-link)))
			     ;; Body is a single form
			     (, maplinks-body)))
		    maplinks-body))
	       (,@ (if message
		       '((if (and message-evalled
				  (zerop (% maplinks-index db-inform-interval)))
			     (db-message message-evalled maplinks-index)))))
	       (setq maplinks-link (link-next maplinks-link)
		     maplinks-index (1+ maplinks-index))
	       (if (eq maplinks-link first-link)
		   (setq maplinks-not-done nil)))))
       nil)))
;; The second arg is 'sexp because the macro uses its unevalled value.
(put 'maplinks-macro 'edebug-form-spec '(form sexp &optional form form))

(fset 'maprecords-break 'maplinks-break)

;; The formal parameter name MAPRECORDS-FUNC is different than that of maplinks
;; because if they're the same, then when the function created here is run and
;; FUNC is looked up, the nearest dynamically enclosing binding of FUNC will be
;; returned.  That won't be the one I'm hoping for.

(defun maprecords (maprecords-func database &optional hide message accumulate)
  "Apply MAPRECORDS-FUNC to every record in DATABASE.  Return nil.
If optional third arg HIDE is non-nil, apply MAPRECORDS-FUNC only to
unhidden records.
If optional fourth arg MESSAGE is non-nil, it should be a format string
containing one numeric \(%d\) specifier.  That message will be issued every
`db-inform-interval' records.
If optional fifth arg ACCUMULATE is non-nil, return a list of the results;
otherwise return nil.

This is syntactic sugar for a call to `maplinks', which see.
See also `maprecords-macro'."
  (maplinks (function (lambda (thislink)
			(funcall maprecords-func (link-record thislink))))
	    database hide message accumulate))

(defmacro maprecords-macro (maprecords-body database &optional hide message)
  "Execute MAPRECORDS-BODY for each record in DATABASE, and return nil.
If optional third arg HIDE is non-nil, execute MAPRECORDS-BODY only for
unhidden records.
If optional fourth arg MESSAGE is non-nil, it should be a format string
containing one numeric \(%d\) specifier.  That message will be issued every
`db-inform-interval' links.

In the body, variable  maprecords-record  is bound to the record being operated
upon.
The loop may be short-circuited (aborted) by calling `maprecords-break'.

This is syntactic sugar for a call to `maplinks-macro', which see.
See also `maprecords'."
  (` (let (maprecords-record)
       (maplinks-macro
	(progn
	  (setq maprecords-record (link-record maplinks-link))
	  (, maprecords-body))
	(, database)
	(, hide)))))
(put 'maprecords-macro 'edebug-form-spec '(form sexp &optional form form))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Databases and buffers
;;;

;; (defun database-delete (database)
;;   "Get rid of DATABASE and its associated buffers."
;;   (map-data-display-buffers (function (lambda (buf)
;; 					(in-buffer buf
;; 					  (db-exit t))))
;; 			    database))

(defun map-data-display-buffers (function database)
  "Apply FUNCTION to each data display buffer of DATABASE."
  (let ((dd-buffers (database-clean-data-display-buffers database)))
    (while dd-buffers
      (funcall function (car dd-buffers))
      (setq dd-buffers (cdr dd-buffers)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Printing
;;;

;; I realize that this entire section is a hack.


(defun print-database (database)
  (maprecords (function (lambda (record)
			  (print-record record
					database)))
	      database))

(defun print-record (record database)
  (let ((fno 0)
	(no-of-fields (database-no-of-fields database))
	(fieldnames (database-fieldnames database)))
    (princ "\n")
    (while (< fno no-of-fields)
      (princ (format "%s:  %s\n"
		     (aref fieldnames fno) (aref record fno)))
      (setq fno (1+ fno)))))

;; This is getting a compilation error:  an open-coded lambda is being
;; called with too few arguments.

;; The records are assumed to be different.
;; A . next to a field means an inessential difference.
;; A * means an essential difference.
(defun print-compare-records (record1 record2 database)
  (let ((field-number 0)
	;; maybe get rid of these bindings and trust the compiler to be smart.
	(max-field-number (database-no-of-fields database))
	recordfieldspec
	fieldname
	order-function
	field1 field2)
    (princ "\n")
    (while (< field-number max-field-number)
      (setq recordfieldspec (database-recordfieldspec database field-number)
	    fieldname (fieldnumber->fieldname field-number database)
	    order-function (recordfieldspec-order-function recordfieldspec)
	    field1 (aref record1 field-number)
	    field2 (aref record2 field-number))
      (cond ((equal field1 field2)
	     (princ (format "  %s:  %s\n" fieldname field1)))
	    ((and order-function
		  (zerop (funcall order-function field1 field2)))
	     (princ (format ". %s:  %s\n. %s:  %s\n"
			    fieldname field1 fieldname field2)))
	    (t
	      (princ (format "* %s:  %s\n* %s:  %s\n"
			    fieldname field1 fieldname field2))))
      (setq field-number (1+ field-number)))))

;; (defun print-database-old (database)
;;   (let ((fieldnames (database-fieldnames database)))
;;     (maplinks (function (lambda (link)
;; 			  (print-record (link-record link)
;; 					fieldnames)))
;; 	      database)))
;;
;; (defun print-database-alt-old (database)
;;   (let ((fieldnames (database-fieldnames database)))
;;     (maprecords (lambda (record)
;; 		  (print-record record fieldnames))
;; 		database)))

;; (defun print-record-old (record fieldnames)
;;   (let ((slot-number 0)
;; 	(fn-length (length fieldnames)))
;;     (princ "\n")
;;     (while (< slot-number fn-length)
;;       (princ (format "%s:  %s\n"
;; 		     (aref fieldnames slot-number) (aref record slot-number)))
;;       (setq slot-number (1+ slot-number)))))

;;; db-rep.el ends here
