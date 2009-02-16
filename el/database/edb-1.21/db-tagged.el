;;; db-tagged.el --- part of EDB, the Emacs database

;; See database.el for copyright notice, distribution conditions, etc.

;; Author: Michael A. Patton <MAP@lcs.mit.edu>
;;	Michael Ernst <mernst@theory.lcs.mit.edu>
;; Keywords: EDB

;;; Commentary:

;; Provide support for files of tagged fields, such as
;;  Name:  John Doe
;;  Age:   42
;;  Salary:22000


;; This file adds to EDB basic support for parsing and generating
;; files where records consist of a group of lines with tags and
;; values.  All functions and variables defined here start with the
;; prefix "db-tagged-".

;; The basic way to use this is to
;; call "db-tagged-setup" with a list describing the fields.
;; There are hooks to parse out and generate possible added entries
;; that don't fit the basic model.  There are variables to customize
;; all the separator strings and regexps for complete parsing.  These
;; should be set in the aux file after calling "db-tagged-setup".

;; The field info passed to db-tagged-setup is a list with one entry
;; per field.  Each entry is a three-element list:  the field name, the
;; tag used to identify it in the file, and a short description for
;; documentation (not presently used).  The field name can be a two
;; element list with a field name and type to use types other than
;; the default.  Two values in the tag position are special, nil
;; means that it will be a computed field and is never stored in the
;; database (and thus doesn't need a tag), and the empty string marks
;; a field where "comments" are collected, comments consist of lines
;; starting with the separator, i.e. empty tags.


;; The default is records separated by blank lines, tags separated from
;; fields by ":", white space around the separator is not significant on
;; input and that the separator should be followed by one tab on output,
;; and continuation lines start with whitespace.  All of these can be
;; customized.


;;; Code:


(require 'database)
(provide 'db-tagged)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Variables
;;;

;;; Variables used dynamically; avoid compiler messages about free variables.
(defvar database)


;;; These are the variables used for customization, these are actually
;;; database-local variables and not real variables.  The defvars are
;;; mainly to get documentation somewhere...the values are used as
;;; defaults when initializing the database.

(defvar db-tagged-tag-chars "-_A-Za-z"
  "The characters that are allowed in field tags, in a form suitable for
placing inside [] in a regular expression.")

(defvar db-tagged-pre-tag ""
  "The string that precedes each tag.
Used only if  db-tagged-pre-tag-regexp  or  db-tagged-pre-tag-output
nil \(depending on whether the record is being read or written\).")

(defvar db-tagged-pre-tag-regexp nil
  "A regexp for what precedes each tag on its line when parsing.
It must NOT contain any grouping constructs.")

(defvar db-tagged-pre-tag-output nil
  "The fixed string to place before each tag on output.")

(defvar db-tagged-separator ":"
  "The string that separates field names from values.
Used only if `db-tagged-separator-regexp' or `db-tagged-separator-output' is
nil \(depending on whether the record is being read or written\).")

(defvar db-tagged-separator-regexp nil
  "A regexp for the separator between field names and values when parsing.")

(defvar db-tagged-separator-output nil
  "The separator between field names and values on output.")

(defvar db-tagged-continuation "\t"
  "The string that marks (the beginning of) a continuation line.
Used only if `db-tagged-continuation-regexp' or `db-tagged-continuation-output'
is nil \(depending on whether the record is being read or written).")

(defvar db-tagged-continuation-regexp "[ \t]+"
  "A regexp for a continuation line in a value when parsing.")

(defvar db-tagged-continuation-output nil
  "The fixed string to use (before) continuing values on output.")

(defvar db-tagged-rrfr-hooks ()
  "Hooks run on each database record before tagged parse.")

(defvar db-tagged-wrfr-before-hooks ()
  "Hooks run before each tagged write of a database record.
The record is bound to the dynamic variable  record, and point is where the
record will be inserted in the buffer.")

(defvar db-tagged-wrfr-after-hooks ()
  "Hooks run after each tagged write of a database record.
The record is bound to the dynamic variable  record, and point is immediately
after the file representation of the record.")

(defvar db-tagged-converted-p ()
  "Non-nil if `database-stored->actual' has already been run.")

(defvar db-tagged-default-field nil
  "A fieldname (symbol) for the field indicated by an illegal or empty tag.
\(For instance, you might set it to 'comments.\)
Nil if those values should be discarded.")


;;; There are also a few settings in EDB that you might wish to play with:
; sepinfo-set-sep-string		Default "\n\n"
; sepinfo-set-post-last-string		Default "\n"
;; It might be that I should have defvars for these defaults...


;;; Abstraction of the tagged field specification.
(defsubst tagged-field-spec-name (tfs) (car tfs))
(defsubst tagged-field-spec-tag (tfs) (car (cdr tfs)))
(defsubst tagged-field-spec-help (tfs) (car (cdr (cdr tfs))))


;;;###autoload
(defun db-tagged-setup (tagged-field-specs)
  "Ready the database to read files in tagged format.
Creates database local variables and sets database slots.
Argument TAGGED-FIELD-SPECS is a list of tagged-field specifications, one
for each field in a database record.  Each tagged-field specification is a
three-element list of the field name \(a symbol\), the tag used to identify
it in the file \(a string\), and a brief help string.
Instead of a symbol, the tagged-field name may be a cons of the
field name and its type.
To indicate that a field is never found in the input file \(typically
because it is computed on the fly\), use nil for its tag.

This function should be called first in an auxiliary or format file, so
that the defaults it chooses can be overridden.
`database-set-fieldnames-to-list' should not be called if this function is."

  ;; Try to detect whether this database was once in tagged file layout, but is
  ;; now in internal file layout.
  (if (db-tagged-p database)
      (if (not (database-get-local 'db-tagged-converted-p database t))
	  (database-set-local 'db-tagged-converted-p database t))
    (db-tagged-setup-internal tagged-field-specs)))

(defun db-tagged-setup-internal (tagged-field-specs)
  "Do the real work of `db-tagged-setup'.
This function should only be called once per database."
  (database-make-local 'db-tagged-field-spec database tagged-field-specs)
  (mapcar (function (lambda (fld)
		      (database-make-local fld database (symbol-value fld))))
	  '(db-tagged-rrfr-hooks
	    db-tagged-wrfr-before-hooks
	    db-tagged-wrfr-after-hooks
	    db-tagged-tag-chars
	    db-tagged-pre-tag
	    db-tagged-pre-tag-regexp
	    db-tagged-pre-tag-output
	    db-tagged-separator
	    db-tagged-separator-regexp
	    db-tagged-separator-output
	    db-tagged-continuation
	    db-tagged-continuation-regexp
	    db-tagged-continuation-output
	    db-tagged-converted-p
	    ))
  ;; Most fields are strings (or missing, here represented as nil)
  ;; Need to declare this before defining fields
  (setq db-default-field-type 'nil-or-string) ; this var is buffer-local
  (database-set-fieldnames-to-list
   database (mapcar (function tagged-field-spec-name) tagged-field-specs))
  ;; Insert the help strings into *copies* of the recordfieldspecs in order to
  ;; not redefine the prototype fieldtypes.
  (mapcar
   (function (lambda (fspec)
	       (let ((fn (tagged-field-spec-name fspec))
		     (help (tagged-field-spec-help fspec))
		     fnum rs ohelp)
		 (if (consp fn)
		     (setq fn (car fn)))
		 (setq fnum (fieldname->fieldnumber fn database)
		       rs (copy-recordfieldspec
			   (database-recordfieldspec database fnum))
		       ohelp (recordfieldspec-help-info rs))
		 (recordfieldspec-set-help-info rs (if ohelp
						       (concat help "\n" ohelp)
						     help))
		 (database-set-recordfieldspec database fnum rs))))
   tagged-field-specs)
  (database-set-read-record-from-region database 'db-tagged-rrfr)
  (database-set-write-region-from-record database 'db-tagged-wrfr)
  (sepinfo-set-sep-string (database-record-sepinfo database) "\n\n")
  (sepinfo-set-post-last-string (database-record-sepinfo database) "\n")
  (add-hook 'db-before-read-hooks 'db-tagged-before-read)
  (add-hook 'db-after-read-hooks 'db-tagged-database-stored->actual)
  )

(defsubst db-tagged-p (database)
  "Non-nil if DATABASE is in tagged file layout."
  (database-get-local 'db-tagged-field-spec database t))

(defun db-tagged-database-stored->actual (&optional db)
  "Like `database-stored->actual', but only in tagged file layout."
  (if (and (db-tagged-p (or db database))
	   (not (database-get-local 'db-tagged-converted-p database t)))
      (progn
	(database-stored->actual db)
	(database-set-local 'db-tagged-converted-p database t))))

(defun db-tagged-before-read ()
  (if (db-tagged-p database)
      (progn
	(if (not (database-get-local 'db-tagged-pre-tag-regexp database))
	    (database-set-local 'db-tagged-pre-tag-regexp database
			(regexp-quote
			 (database-get-local 'db-tagged-pre-tag database))))
	(if (not (database-get-local 'db-tagged-pre-tag-output database))
	    (database-set-local 'db-tagged-pre-tag-output database
			(database-get-local 'db-tagged-pre-tag database)))
	(if (not (database-get-local 'db-tagged-separator-regexp database))
	    (database-set-local 'db-tagged-separator-regexp database
			(concat "[ \t]*"
				(regexp-quote
				 (database-get-local 'db-tagged-separator database))
				"[ \t]*")))
	(if (not (database-get-local 'db-tagged-separator-output database))
	    (database-set-local 'db-tagged-separator-output database
			(concat (database-get-local 'db-tagged-separator database)
				"\t")))
	(if (not (database-get-local 'db-tagged-continuation-regexp database))
	    (database-set-local 'db-tagged-continuation-regexp database
			(regexp-quote
			 (database-get-local 'db-tagged-continuation database))))
	(if (not (database-get-local 'db-tagged-continuation-output database))
	    (database-set-local 'db-tagged-continuation-output database
			(database-get-local 'db-tagged-continuation database)))
	)))

;; The buffer is narrowed to the region which is one record.
(defun db-tagged-rrfr ()
  (let* ((result-record (make-record database))
	 (cnt-re (database-get-local 'db-tagged-continuation-regexp database))
	 (tag-re (concat
		  "^" (database-get-local 'db-tagged-pre-tag-regexp database)
		  "\\(["
		  (database-get-local 'db-tagged-tag-chars database)
		  "]*\\)"
		  (database-get-local 'db-tagged-separator-regexp database)))
	 fld-nam fld prev-value)
    ;; run-hooks takes a SYMBOL as its argument
    (let ((hooks (database-get-local 'db-tagged-rrfr-hooks database)))
      (run-hooks 'hooks))
    (goto-char (point-min))
    (while (not (eobp))
      (cond ((looking-at tag-re)
	     (setq fld-nam (db-match-string 1)
		   fld (lookup-field fld-nam))
	     (end-of-line)
	     (let ((fld-val (buffer-substring (match-end 0) (point))))
	       ;; Why isn't this (forward-char 1)?
	       (forward-line 1)
	       (while (looking-at cnt-re)
		 (end-of-line)
		 (setq fld-val (concat fld-val "\n"
				       (buffer-substring (match-end 0)
							 (point))))
		 (forward-line 1))
	       (if (null fld)
		   (progn
		     ;; Should allow an escape hook here, and provide a
		     ;; generic one that adds to an alist-like entry
		     (db-message "Invalid field name %s."
				 fld-nam)
		     (setq fld db-tagged-default-field)))
	       (setq prev-value (record-field result-record fld database))
	       (if fld
		   (if (null prev-value)
		       (record-set-field result-record fld fld-val database)
		     (record-set-field result-record fld
				       (concat prev-value "\n" fld-val)
				       database)))))
	    (t
	     (error "At char %d, didn't find either a tag or the end of the record." (point)))))
    result-record))

(defun db-tagged-wrfr (record)
  ;; run-hooks takes a SYMBOL as its argument
  (let ((hooks (database-get-local 'db-tagged-wrfr-before-hooks database)))
    (run-hooks 'hooks))
  (let ((sep (database-get-local 'db-tagged-separator-output database))
	(contin (database-get-local 'db-tagged-continuation-output database)))
    (mapcar (function
	     (lambda (fld-def)
	       (let ((fld-nam (tagged-field-spec-name fld-def))
		     (fld-tag (tagged-field-spec-tag fld-def)))
		 ;; Handle case of typed field
		 (if (consp fld-nam)
		     (setq fld-nam (car fld-nam)))
		 (if (not (null fld-tag))
		     ;; Fields without tags are computed, so don't get stored
		     (let* ((fld-nbr (fieldname->fieldnumber fld-nam database))
			    (a->s (recordfieldspec-actual->stored
				   (database-recordfieldspec database fld-nbr)))
			    (fld-val (db-funcall-maybe a->s (aref record fld-nbr))))
		       (if (not (or (null fld-val) (equal "" fld-val)))
			   (let ((i 0)
				 j
				 (cnt (if (equal "" fld-tag)
					  sep
					contin)))
			     (insert fld-tag
				     sep)
			     (while (setq j (string-match "\n" fld-val i))
			       (insert (substring fld-val i j) "\n" cnt)
			       (setq i (+ j 1)))
			     (insert (substring fld-val i) "\n"))))))))
	    (database-get-local 'db-tagged-field-spec database)))
  (delete-char -1)			;HACK: punt last newline, it'll
					;be added back later
  ;; run-hooks takes a SYMBOL as its argument
  (let ((hooks (database-get-local 'db-tagged-wrfr-after-hooks database)))
    (run-hooks 'hooks))
  )



(defun lookup-field (key)
  (let ((l (database-get-local 'db-tagged-field-spec database))
	(r nil))
    (while l
      (if (equal key (car (cdr (car l))))
	  (setq r (car (car l))))
      (setq l (cdr l)))
    (if (consp r)
	(car r)
      r)))

;;; db-tagged.el ends here
