;;; db-rdb.el --- part of EDB, the Emacs database

;; See database.el for copyright notice, distribution conditions, etc.

;; Author: Alan K. Stebbens <aks@hub.ucsb.edu>
;; Keywords: EDB
;; Adapted-By: Michael Ernst <mernst@theory.lcs.mit.edu>

;;; Commentary:

;; Provide support for RDB files, which can be a special case of a tagged
;; file, or, a tabular-format file.
;; 
;; The RDB header for both kinds of files contains the field names and
;; the field descriptors, which are its length, type, and a "help" string.
;;
;; There are Perl scripts (by Walter Hobbs) implementing RDB files, and this
;; file is an attempt to provide EDB access to these kinds of files.

;; This file began as a copy of db-tagged.el, but has since been almost
;; completely rewritten.

;; The basic way to use this is to call "db-rdb-setup" with a list
;; describing the fields.  The purpose of the fields is to provide the
;; Elisp programmer a handle on the external fields.  The usage is
;; documented in the function.

;;; Code:

(require 'database)
(provide 'db-rdb)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Variables
;;;

;;; Variables used dynamically; avoid compiler messages about free variables.
(defvar database)

;;; These are the variables used for customization, these are actually
;;; database-local variables and not real variables.  The defvars are
;;; mainly to get documentation somewhere...the values are used as
;;; defaults when initializing the database.

(defvar db-rdb-field-name-charset "a-zA-Z0-9_"
  "The characters allowed in an RDB field name \(using regexp format
sans \"[]\"\).")

(defvar db-rdb-list-separator-string " | "
  "The string that separates field names from values.")

(defvar db-rdb-list-entry-regexp
  (concat "[ \t]*\\(["
	  db-rdb-field-name-charset
	  "]*\\)"
	  (regexp-quote db-rdb-list-separator-string)
	  "[ \t]*")
  "A regexp which matches the beginning of a List-format field
entry, grouping the field name as \\1.")

(defvar db-rdb-list-continuation-regexp "^[ \t]*|[ \t]+"
  "A regexp which matches the beginning of a List-format entry
continuation.")

(defvar db-rdb-list-continuation-output nil
  "A computed string used to identify and align continuation data.")

(defvar db-rdb-rrfr-hooks ()
  "Hooks run on each database record before RDB parse.")

(defvar db-rdb-wrfr-before-hooks ()
  "Hooks run before each RDB write of a database record.
The record is bound to the dynamic variable  record, and point is where the
record will be inserted in the buffer.")

(defvar db-rdb-wrfr-after-hooks ()
  "Hooks run after each RDB write of a database record.
The record is bound to the dynamic variable  record, and point is immediately
after the file representation of the record.")

(defvar db-rdb-converted-p ()
  "Non-nil if `database-stored->actual' has already been run.")

(defvar db-rdb-file-type nil
  "Set to 'list or 'table by rdb-read-fields according to the type
of the file.  This is a database-local variable.")

(defvar db-rdb-header-fields nil
  "Set to the list of header fields parsed by db-rdb-read-list-field-defs
or db-rdb-read-table-field-defs.")

(defvar db-rdb-field-defs nil
  "Set to the field definitions computed from the programmer-supplied field
definitions and the RDB header field definitions.")

(defvar db-rdb-header-string nil
  "Set to the header string, for either List or Table RDB files.
This is used to rewrite the header when the database is updated.")

;;; There are also a few settings in EDB that you might wish to play with:
; sepinfo-set-sep-string		Default "\n\n"
; sepinfo-set-post-last-string		Default "\n"
;; It might be that I should have defvars for these defaults...


;;; Functions

;;; Abstraction of the RDB field specification:  (HANDLE TAG HELP)
;;; HANDLE --> (NAME . TAG) or NAME

(defsubst rdb-field-spec-handle (fspec) (car fspec))
(defsubst rdb-field-spec-name (fspec) (if (consp (car fspec))
				       (car (car fspec))
				     (car fspec)))
(defsubst rdb-field-spec-type (fspec) (if (consp (car fspec))
				       (cdr (car fspec))))
(defsubst rdb-field-spec-tag (fspec) (car (cdr fspec)))
(defsubst rdb-field-spec-help (fspec) (car (cdr (cdr fspec))))


;; db-rdb-p

(defsubst db-rdb-p (database)
  "Non-nil if DATABASE is in RDB file layout."
  (database-get-local 'db-rdb-field-defs database t))

;; db-rdb-setup

;;;###autoload
(defun db-rdb-setup (rdb-field-specs &optional lock-flag)

  "Ready the database to read files in RDB format.
Creates database local variables and sets database slots.  Argument
RDB-FIELD-SPECS is a list of rdb-field specifications, one for each
field in a database record.  Optional, second argument LOCK-FLAG should
be non-nil to lock the file for synchronized updates.  The locking and
unlocking is done with \"rdblock\" and \"rdbunlock\", which must be
available in the current PATH environment variable.

Each field specification is a three-element list of the field name \(a
symbol\), the tag used to identify it in the file \(a string\), and a
brief help string.  Instead of a symbol, the rdb-field name may be a
two-element list of the field name its type.  To indicate that a field
is never found in the input file \(typically because it is computed on
the fly\), use nil for its tag."

  ;; Try to detect whether this database was once in RDB file layout, but is
  ;; now in internal file layout.
  (if (db-rdb-p database)
      (if (not (database-get-local 'db-rdb-converted-p database t))
	  (database-set-local 'db-rdb-converted-p database t))
    (db-rdb-setup-internal rdb-field-specs)))

;; db-rdb-setup-internal

(defun db-rdb-setup-internal (field-specs)

  "Internal function to do the real work of `db-rdb-setup', without
certain safety checks.  This function should only be called once per
database.  Argument is the FIELD-SPECS \(see db-rdb-setup\)."

  (let* ((db database)
	 (rsi (database-record-sepinfo db))
	 (efields (db-rdb-read-fields db))
	 (fspecs (db-rdb-correlate-field-defs field-specs efields))
	 (max-len 0))
    (database-set-local 'db-rdb-field-defs db fspecs t)
    (cond
     ;; table format
     ((eq db-rdb-file-type 'table)
      ;; table style is the same a regular file layout
      (error "db-rdb-setup: table format not implemented yet!"))

     ;; list format
     ((eq db-rdb-file-type 'list)
      ;; discover the maximum of the length of all the field tags
      (mapcar (function (lambda (fld)
			  (let ((tag (rdb-field-spec-tag fld)))
			    (if (> (length tag) max-len)
				(setq max-len (length tag))))))
	      fspecs)

      (database-set-local 'db-rdb-max-field-name-length db max-len t)
      (database-set-local 'db-rdb-continuation-output db
			  (concat (make-string max-len ? )
				  db-rdb-list-separator-string)
			  t)
      (database-set-read-record-from-region db 'db-rdb-list-rrfr)
      (database-set-write-region-from-record db 'db-rdb-list-wrfr)
      ;; set pre-first-regexp to remove the header, up to the
      ;; first pair of newlines
      (sepinfo-set-pre-first-regexp rsi "\\`\\(\\([^\n]\\|\n[^\n]\\)*\n\n\\)")
      (sepinfo-set-pre-first-regexp-submatch rsi 1)
      (sepinfo-set-sep-string rsi "\n\n")
      (sepinfo-set-post-last-string rsi "\n")
      ;; save the header as the pre-first-string so it will get written
      ;; back on output
      ;; at some point, maybe we should allow for dynamic changes to the field
      ;; definitions.
      (sepinfo-set-pre-first-string
       rsi (database-get-local 'db-rdb-header-string db))))

    ;; externalize (as database local) some variables
    (mapcar (function (lambda (fld)
			(database-make-local fld db (symbol-value fld))))
	    '(db-rdb-rrfr-hooks
	      db-rdb-wrfr-before-hooks
	      db-rdb-wrfr-after-hooks
	      db-rdb-converted-p))

    ;; Most fields are strings (or missing, here represented as nil)
    ;; Need to declare this before defining fields
    (setq db-default-field-type 'nil-or-string) ; this var is buffer-local

    ;; tell EDB about the fields now
    (database-set-fieldnames-to-list db (mapcar
					 (function rdb-field-spec-handle)
					 fspecs))

    ;; Insert the help strings into *copies* of the recordfieldspecs
    ;; in order to not redefine the prototype fieldtypes.
    (mapcar (function (lambda (fspec)
			(let* ((fn (rdb-field-spec-name fspec))
			       (help (rdb-field-spec-help fspec))
			       (fnum (fieldname->fieldnumber fn db))
			       (rs (copy-recordfieldspec
				    (database-recordfieldspec db fnum)))
			       (ohelp (recordfieldspec-help-info rs)))
			  (recordfieldspec-set-help-info
			   rs (if ohelp (concat help "\n" ohelp) help))
			  (database-set-recordfieldspec db fnum rs))))
	    fspecs)
    (add-hook 'db-after-read-hooks 'db-rdb-database-stored->actual)))

;; db-rdb-database-stored->actual

(defun db-rdb-database-stored->actual (&optional db)
  "Like `database-stored->actual', but only in RDB file layout."
  (if (and (db-rdb-p (or db database))
	   (not (database-get-local 'db-rdb-converted-p database t)))
      (progn
	(database-stored->actual db)
	(database-set-local 'db-rdb-converted-p database t))))

;; db-rdb-list-rrfr -- Read RDB List format region record

(defun db-rdb-list-rrfr ()

  "With the current buffer narrowed to a single RDB List record, parse
it into field values."

  (let* ((db database)
	 (new-rec (make-record db))
	 (start-re "^[ \t]*\\([a-zA-Z0-9_]*\\)[ \t]*|[ \t]*")
	 (cont-re "^[ \t]*|[ \t]*")
	 fld-tag fld old-val fld-val)
    ;; run-hooks takes a SYMBOL as its argument
    (let ((hooks (database-get-local 'db-rdb-rrfr-hooks db)))
      (run-hooks 'hooks))
    (goto-char (point-min))
    (while (not (eobp))
      (if (looking-at start-re)
	  (progn
	    (setq fld-tag (db-match-string 1)
		  fld (db-rdb-lookup-field fld-tag)
		  old-val (record-field new-rec fld db))
	    (end-of-line)
	    (setq fld-val (buffer-substring (match-end 0) (point)))
	    ;; Why isn't this (forward-char 1)?
	    (forward-line 1)
	    (while (looking-at cont-re)
	      (end-of-line)
	      (setq fld-val (concat fld-val "\n"
				    (buffer-substring (match-end 0) (point))))
	      (forward-line 1))
	    (record-set-field new-rec fld
			      (if old-val (concat old-val "\n" fld-val)
				fld-val)
			      db))
	;; else looking-at failed
	(error "This didn't look right to me (point = %s)"
	       (int-to-string (point)))))
    new-rec))

;; RDB write RDB List file record from database record

(defun db-rdb-list-wrfr (record)

  "Given an EDB RECORD, write convert to the RDB List format
representation and write to the file."

  ;; run-hooks takes a SYMBOL as its argument
  (let* ((db database)
	 (max-len (database-get-local 'db-rdb-max-field-name-length db))
	 (contin (database-get-local 'db-rdb-continuation-output db)))
    ;; run any before-hooks
    (let ((before-hooks (database-get-local 'db-rdb-wrfr-before-hooks db)))
      (run-hooks 'before-hooks))
    (mapcar
     '(lambda (fld-def)
	(let ((fld-nam (rdb-field-spec-name fld-def))
	      (fld-tag (rdb-field-spec-tag fld-def)))
	  (if (not (null fld-tag))
	      ;;Fields without tags are computed, so don't get stored
	      (let* ((fld-nbr (fieldname->fieldnumber fld-nam db))
		     (a->s (recordfieldspec-actual->stored
			    (database-recordfieldspec db fld-nbr)))
		     (fld-val (db-funcall-maybe a->s (aref record fld-nbr))))
		;; null or empty values don't get written
		(if (not (or (null fld-val)
			     (equal "" fld-val)))
		    (let ((fld-pad (- max-len (length fld-tag)))
			  (i 0) j)
		      ;; right-align the RDB field names
		      (if (> fld-pad 0)
			  (insert (make-string fld-pad ? )))
		      (insert fld-tag " | ")
		      (while (setq j (string-match "\n" fld-val i))
			(insert (substring fld-val i j) "\n" contin)
			(setq i (+ j 1)))
		      (insert (substring fld-val i) "\n")
		      ))))))
     (database-get-local 'db-rdb-field-defs db))
    ;;HACK: punt last newline, it'll be added back later
    (delete-char -1)
    ;; run any after-hooks
    (let ((after-hooks (database-get-local 'db-rdb-wrfr-after-hooks db)))
      (run-hooks 'after-hooks))))

;; db-rdb-lookup-key

(defun db-rdb-lookup-field (tag)
  "Lookup FIELD-TAG in the RDB database field list, returning its
associated NAME \(a symbol\)."
  (let ((field-list (database-get-local 'db-rdb-field-defs database))
	(sym nil))
    (while field-list
      (if (equal tag (car (cdr (car field-list))))
	  (setq sym (car (car field-list))))
      (setq field-list (cdr field-list)))
    (if sym				;found it?
	(if (consp sym)			;is it a node?
	    (setq sym (car sym)))	;return just the symbol
      ;; Field not defined -- report it, then add it
      (message "Field name % encountered, but not defined." tag)
      (setq field-list (database-get-local 'db-rdb-field-defs database))
      (nconc field-list (list (list (cons (setq sym (intern tag))
					  'string-or-nil)
				    tag "Undefined field"))))
    sym))

;; db-rdb-read-fields

(defun db-rdb-read-fields (db)
  "In DATABASE, read the RDB-style headers, creating a list of field
definitions, which is returned as the result."
  "Setup the field names from the current EDB database file, which is
assumed to be formatted as an RDB database.  The RDB database may be
either in List format or Table form.  Any updates will continue to
maintain the RDB file in its current form.  Any EDB format files for the
database will be used automatically.

The argument, FIELDDEFS, is a list of elements: \(\(SYMBOL FIELD-NAME
FIELD-HELP\)...\).  SYMBOL may itself be either an atom, a symbol, or a
cons cell of \(SYMBOL . TYPE\).  SYMBOL is used to identify the field by
name in elisp code.  TYPE, if given, must be a valid EDB fieldtype,
either preconfigured, or user-defined.  If TYPE is null or hidden, it
will be deduced from the corresponding RDB header, utimately defaulting
to \"string\".

The FIELD-NAME is a string which must match one of the corresponding
field names in the database.  FIELD-NAMES which do not exist in the RDB
file are shown in an error message, as are any unmentioned field names
read from the RDB header.

The FIELD-HELP is a string used for information when queried with \"?\"
interactively.  If FIELD-HELP is null, any comment in the field
definition from the RDB file is used instead.

Lastly, if FIELDDEFS is null, then all of the information will be
obtained entirely from the RDB file, with the SYMBOL defaulting to the
symbol with the same print-string as FIELD-NAME."
  (save-excursion
    (set-buffer db-buffer)
    (goto-char (point-min))
    (while (looking-at "^#")
      (forward-line 1))
    ;; if the first non-comment line is empty, we're in List mode
    (if (looking-at "^[ \t]*\n")
	(db-rdb-read-list-field-defs)
      (db-rdb-read-table-field-defs))))

;; rdb-read-list-field-defs

(defun db-rdb-read-list-field-defs ()

  "Positioned at the blank line preceeding the field definitions in an
RDB List file, read the field definitions and return them as an
association list: \(\(FIELD-NAME FIELD-DEF FIELD-HELP\)...\)."

  (let (fields name defn width help end)
    (forward-line 1)			;go to the first real line
    (while (not (looking-at "^[ \t]*$"))
      (cond ((looking-at "^\#"))	;ignore comments
	    ((looking-at "^[ \t]*\\(\\w+\\)[ \t]+|[ \t]+\\(.*\\)$")
	     (setq name (db-match-string 1))	;FIELD-NAME
	     (setq defn (db-string-split-first-word (db-match-string 2)))
	     (setq help (car-safe (cdr-safe defn))) ; FIELD-HELP
	     (setq defn (car defn))		;FIELD-DEF
	     (setq width (and (string-match "\\([0-9]+\\)" defn)
			      (string-to-int (db-match-string 1 defn))))
	     (setq defn (and (string-match "\\([SNDM<>]\\)" defn)
			     (db-match-string 1 defn)))
	     (setq fields (append fields (list (list name width defn help))))
	     ))
      (forward-line 1))
    ;; we're just past the header -- copy the entire thing and save it in
    ;; a local variable, so our caller can stuff into a record sepinfo.
    (forward-line 1)			;skip the header/record separator
    (database-set-local 'db-rdb-header-string database
			(buffer-substring (point-min) (point))
			t)
    (database-set-local 'db-rdb-file-type database db-rdb-file-type t)
    (database-set-local 'db-rdb-header-fields database fields t)
    ;; leave some clues as to the kind of RDB file we've parsed
    (setq db-rdb-file-type 'list)
    ;; return with the fields
    fields))

;; db-rdb-correlate-field-defs

(defun db-rdb-correlate-field-defs (ifields efields)

  "Given INTERNAL-FIELDS and EXTERNAL-FIELDS, correlate the fields with
each other and produce a field list suitable for M-x db-rdb-setup.

INTERNAL-FIELDS is a list: \(\(HANDLE NAME HELP\)...\), where HANDLE is
either \(SYMBOL . TYPE\) or just SYMBOL.

EXTERNAL-FIELDS is a list: \(\(NAME WIDTH FORMAT HELP\)...\), produced
by the function rdb-read-field-defs.

If TYPE is hidden, it is deduced from the corresponding FORMAT.
Similarly, if the internal HELP is hidden, any external HELP is used.
The internal definition always overrides the external, since it is more
specific to the EDB implementation.

The resulting list format is: \(\(\(SYMBOL . TYPE\) NAME HELP\)...\)."

  (let (fieldspec inames (errs 0))
    (if (null ifields)			;null internal field spec?
	(setq ifields (mapcar		;make default be same names
		       (function (lambda (edef)
				   (let ((name (car edef)))
				     (list (intern name)))))
		       efields)))
    ;; now correlate internal fields with the external fields
    ;; while we're mapping over the idefs, build an alist of names
    ;; to use later for the reverse correlation check.
    (setq fieldspec
	 (mapcar
	  (function
	   (lambda (idef)
	     (let* ((handle (nth 0 idef))
		    (name (nth 1 idef))	; field name (tag)
		    (ihelp (nth 2 idef))
		    (symbol (or (and (atom handle) handle)
				(car handle)))
		    (type (and (consp handle)
			       (cdr handle)))
		    (edef (and name
			       (assoc name efields)))
		    width format ehelp)
	       (setq inames (append inames (list (list name))))
	       (if (and name (not edef))
		   (progn
		     (message "Internal field %s is not externally defined"
			      name)
		     (beep t)
		     (sit-for 3)
		     (setq errs (1+ errs)))
		 ;; else name could be null because it's dynamic
		 (if edef
		     (setq width (nth 1 edef)
			   format (nth 2 edef)
			   ehelp (nth 3 edef))))
	       ;; do merge of field info and return:
	       ;; ( (SYMBOL . TYPE) NAME HELP )
	       (list (cons symbol (or type
				      (db-rdb-format-to-type format)))
		     name
		     (or ihelp ehelp)))))
	  ifields))			;end setq
    ;; Now make sure we got all the external fields named
    ;; using the inames alist we built in the mapcar above
    (mapcar (function
	     (lambda (edef)
	       (let* ((name (nth 0 edef))
		      (idef (assoc name inames)))
		 (if (not idef)
		     (progn
		       (message "External field %s is not defined internally."
				name)
		       (sit-for 3)
		       (setq errs (1+ errs)))))))
	    efields)			;mapcar over this
    (if (not (zerop errs))
	(if (not (y-or-n-p (format "%d errors occurred; continue? " errs)))
	    (error "db-rdb-setup: %d errors" errs)))
    ;; return the field spec
    fieldspec))

;; db-rdb-format-to-type

(defun db-rdb-format-to-type (format)

  "Given FORMAT from an RDB field definition, return an EDB record field
TYPE."

  (cond					;convert RDB format to EDB fieldtype
   ;; Left-aligned string
   ((or (null format)
	(equal format "S")
	(equal format "<"))	'one-line-string)
   ;; someday we'll do something fancier -- like make sure the
   ;; associated field displayspec has right-alignment turned on
   ;; but, for now...
   ((equal format ">")    	'one-line-string)
   ((equal format "N")		'integer-or-nil)
   ;; dates
   ((or (equal format "D")
	(equal format "M"))	'date-mmddyy)

   ;; There are no other alternatives -- cause an error report
   (t
    (message "Unknown field format: %s, string assumed" format)
    (beep t)
    (sit-for 3)
    'one-line-string)))

;; db-rdb-read-table-field-defs

(defun db-rdb-read-table-field-defs ()
  "Positioned at the field name line in an RDB Table file, read the
field definitions and setup EDB for regular-file format.  Leave the
buffer positioned before the first record, if any."
  (let ((db database))
    (error "Not implemented yet.")
    (database-set-local 'db-rdb-file-type db 'table t))
  )					;end defun db-rdb-read-table-field-defs

;;; db-rdb.el ends here
