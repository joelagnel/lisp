;;; db-file-io.el --- part of EDB, the Emacs database

;; See database.el for copyright notice, distribution conditions, etc.

;; Author: Michael Ernst <mernst@theory.lcs.mit.edu>
;; Keywords: EDB

;;; Commentary:

;; Read and write database files.

;;; Code:


(require 'db-rep)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DB variables
;;;

;; This won't be such a concern once every field is explicitly specified
;; when the database is written out.  But until then, should it be
;; buffer-local?  (It's not in the data display buffer that I care so much
;; but in the read-database buffer.)

(deflocalvar db-default-field-type 'string
  "The type to use for record fields whose type is not explicitly specified.")

(defvar db-format-file-path nil
  "List of directories (strings) to search, in order, for format files not
found in the directory with their associated databases.")

(defvar db-aux-file-path nil
  "List of directories (strings) to search, in order, for auxiliary files not
found in the directory with their associated databases.")


;; Non-nil if there has been an error while reading or writing the database.
;; This is set on file read, but the value is not currently used.
(defvar db-io-error-p nil)

(defvar db-before-read-hooks nil
  "Function or list of functions run immediately before a database is first read
but after all local variables are set.
The hooks are run in the data display buffer with variable `database' bound.
Variable `db-buffer' is bound to a buffer containing the database file.

This is a global variable.  If you set it to be specific to a particular
database \(for instance, in the format or auxiliary file\), then consider
having its last action be to reset the variable to nil.")

(defvar db-after-read-hooks nil
  "Function or list of functions run after a database is completely read.
The hooks are run in the data display buffer with variable `database' bound.
For databases with nonregular layouts, you might put a call to
`database-stored->actual' here, for instance.

This is a global variable.  If you set it to be specific to a particular
database \(for instance, in the format or auxiliary file), then consider
having its last action be to reset the variable to nil.")

(defvar db-format-file-suffixes '(".dbf" ".fmt" "f")
  "List of format file suffixes; the basename is that of the database file.
The suffixes are tried in order; the default is \(\".dbf\" \".fmt\" \"f\").
The . that may precede the extension must be specified explicitly.")

(defvar db-aux-file-suffixes '(".dba" ".aux" "a")
  "List of auxiliary file suffixes; the basename is that of the database file.
The suffixes are tried in order; the default is \(\".dba\" \".aux\" \"a\").
The . that may precede the extension must be specified explicitly.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Internal variables
;;;

;; These have documentation strings so docstring-substitute will recognize them.

(defvar db-buffer nil "Buffer containing a database file being read.")

(defvar database nil "Database being read from a file; also other uses.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Read a database file
;;;


;; This assumes that the database doesn't exist.
;; If the database exists, call read-database-file-helper instead to
;; refresh it (which requires getting the database file into a buffer first).
;; "Return the database.  The data display buffer will be the first element of
;; its data-display-buffers slot.
;; 
;; Optional arg FORMAT-FILE, a filename, specifies the format file which
;; describes how database records are displayed.  If it is nil, the default
;; \(in the default-format-file slot of the database\) will be used, or one
;; returned by `db-file->format-file' will be used, or the user will be
;; prompted for one.
;; 
;; If optional prefix arg CONFIRM is non-nil, then no default will be used
;; without confirmation from the user."
(defun read-database-file (db-file &optional format-file confirm)
  (interactive "fDatabase file: \nP")
  (setq db-io-error-p nil)
  (let ((db-buffer (generate-new-buffer "read-database-file"))
	data-display-buffer
	database)

    ;; (make-database :file db-file)
    ;; (hack-local-variables t)

    (set-buffer db-buffer)
    (if (file-exists-p db-file)
	(insert-file-contents db-file nil)
      (message "New database file."))

    (setq database (read-database-internal-file-layout-maybe))
    (database-set-file database db-file)
    (database-set-modifiable-p database (file-writable-p db-file))

    (db-debug-message "read-database-file:  database = %s" database)

    ;; This is done in db-setup-data-display-buffer.
    ;; (load-db-aux-file database)
    ;; (db-set-fieldname-vars database)

    ;; when could format-file be t?
    ;; (if (eq format-file t)
    ;;     (setq format-file nil))

    (if (or (not format-file)
	    confirm)
	;; We may be calling this because the argument to
	;; read-database-file wasn't specified.
	(setq format-file (choose-format-file database format-file confirm)))

    (setq data-display-buffer
	  (db-setup-data-display-buffer format-file database t))
    (database-set-data-display-buffers database
	  (cons data-display-buffer (database-data-display-buffers database)))

    ;; (db-debug-message "rdf:  recordfieldspecs = %s" (database-recordfieldspecs database))

    (in-buffer data-display-buffer
       (run-hooks 'db-before-read-hooks))

    ;; Local variables are now set from database, aux, and format files.
    (read-database-file-helper db-buffer database)

    (in-buffer data-display-buffer
       (run-hooks 'db-after-read-hooks))

    database))


;;   "Return a database.
;; If the buffer contains a database in internal file layout, it is returned.
;; \(The records are not read from the buffer, only the database structure.\)
;; Otherwise, an empty database is returned."
(defun read-database-internal-file-layout-maybe ()
  (if (db-skip-string-forward ";; Database file written by EDB")
      (let ((here (point)))
	(emacs-lisp-mode)
	;; Update old formats in small increments.
	(if (db-skip-string-forward "; format 0.1")
	    (progn
	      (delete-backward-char 1)
	      (insert "2")
	      (forward-sexp)
	      (backward-char 1)
	      ;; add locals slot to database
	      (insert " nil")
	      (goto-char here)))
	(if (db-skip-string-forward "; format 0.2")
	    (progn
	      (delete-backward-char 1)
	      (insert "3")
	      (forward-sexp)
	      (backward-char 1)
	      (backward-sexp 1)
	      (backward-char 1)
	      ;; add modified-p slot to database
	      (insert " nil")
	      (goto-char here)))
	(if (db-skip-string-forward "; format 0.3")
	    (progn
	      (delete-backward-char 1)
	      (insert "4")
	      (down-list 1)
	      (forward-sexp 16)
	      ;; add internal-file-layout-p slot to database
	      (insert " t")
	      (forward-sexp 14)
	      ;; add modifiable-p slot to database
	      (insert " t")
	      (goto-char here)))
	(if (db-skip-string-forward "; format 0.4")
	    (progn
	      (delete-backward-char 1)
	      (insert "5")
	      ;; change "[database" to "[cl-struct-database"
	      (down-list 1)
	      (insert "cl-struct-")
	      (forward-sexp 17)
	      ;; change "[sepinfo" to "[cl-struct-sepinfo"
	      (down-list 1)
	      (insert "cl-struct-")
	      (up-list 1)
	      (down-list 1)
	      (insert "cl-struct-")
	      (up-list 1)
	      (down-list 1)
	      (insert "cl-struct-")
	      (goto-char here)))
	(if (db-skip-string-forward "; format 0.5")
	    (progn
	      (delete-backward-char 1)
	      (insert "6")
	      ;; Remove five slots for quotation information
	      (down-list 1)
	      (forward-sexp 24)
	      (kill-sexp 5)
	      (goto-char here)))

	;; Don't forget to change write-database file if the format
	;; number is updated.
	(if (not (db-skip-string-forward "; format 0.6"))
	    (db-message "I don't know if I can read the database, but I'll try."))
	(read (current-buffer)))
    (make-database)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Format file
;;;

(defun db-file->format-file (db-file)
  "Return a format file or nil."
  (let* ((subdir (file-name-directory db-file))
	 (default-directory (if subdir
				(expand-file-name subdir)
			      default-directory))
	 (default-file (db-locate-file-with-extensions-on-path
			db-file db-format-file-suffixes db-format-file-path)))
    (if default-file
	(expand-file-name default-file))))

;;   "Return a full pathname for the format file named FILENAME, or err."
(defun locate-format-file (filename)
  ;; The expand-file-name is needed only if the file is in the
  ;; current directory.  Perhaps db-locate-file-on-path should always
  ;; return a full pathname for the file.
  (let ((checked-filename (db-locate-file-on-path filename db-format-file-path)))
    (if checked-filename
	(expand-file-name checked-filename)
      (error "I can't find a format file named %s." filename))))

;;   "Return a format file according to DATABASE or FORMAT-FILE-DEFAULT.
;; Prompt if CONFIRM is set or if we can't get one from DATABASE alone."
(defun choose-format-file (database format-file-default confirm)
  (let* ((db-file (database-file database))
	 (default (or format-file-default
		      (database-default-format-file database)
		      (db-file->format-file db-file))))
    (db-debug-message "db-file = %s, sans extension = %s"
		      db-file (db-filename-sans-extension db-file))
    (if (and default (not confirm))
	(let* ((dir (file-name-directory db-file))
	       (default-directory (if dir
				       (expand-file-name dir)
				     default-directory)))
	  (locate-format-file default))
      ;; Don't need locate-format-file because MUSTMATCH arg
      ;; to read-file-name is t.
      (expand-file-name
       (read-file-name
	(if default
	    (format "Display format for %s: [default %s] "
		    (file-name-nondirectory db-file)
		    (file-name-nondirectory default))
	  (format "Display format for %s: "
		  (file-name-nondirectory db-file)))
	(file-name-directory db-file)
	(if default
	    (expand-file-name default (file-name-directory db-file)))
	t)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Read database file:  helper functions
;;;

;; The format and the auxiliary file should already be loaded; ie, all
;; special variables should be set.

;; This is separate because we might want to simply refresh a database from
;; disk without going through the hoopla of setting variables, etc.
;;   "DB-BUFFER is a buffer containing the database file; DATABASE is the
;; database to be read into.
;; The local variables section of DB-BUFFER is not executed, nor are the
;; database's auxiliary file read, nor are any database slots filled \(except
;; for first-link\)."
(defun read-database-file-helper (db-buffer database)

  (db-message "Reading database...")

  (set-buffer db-buffer)

  (if (database-internal-file-layout-p database)
      (let ((record-list (read (current-buffer))))
	(db-message "Linking records...")
	(database-set-links-from-list
	 database
	 (mapcar (function make-link-from-record)
		 record-list)))
    (progn
      (database-io-setup database t)
      (database-delete-pre-and-post database)

      (if (database-read-record-from-region database)
	  (read-database-file-custom database)
	(read-database-file-delimited database))))

  (db-debug-message "read-database-file-helper:  about to kill %s"
		    (current-buffer))
  (kill-buffer (current-buffer))
  (setq db-databases
	(cons database db-databases))
  (db-message "Done reading database.  %s records total."
	      (database-no-of-records database))
  database)


(defun database-set-links-from-list (database list-of-links)

  ;; Takes a list of links, sets database-first-link to the first one and
  ;; sets link-next and link-prev of each such that all the links are
  ;; arranged in a doubly-linked list in the order of the list.

  (database-set-no-of-records database (length list-of-links))
  (db-debug-message "database-set-links-from-list:  length = %s"
		    (length list-of-links))
  (let ((prev-link (car list-of-links))
	this-link)
    (database-set-first-link database prev-link)
    (setq list-of-links (cdr list-of-links))
    (while list-of-links
      (setq this-link (car list-of-links))
      (link-two prev-link this-link)
      (setq prev-link this-link)
      (setq list-of-links (cdr list-of-links)))
    (link-two prev-link (database-first-link database))))


;; Body should setq this-record.  Body uses dynamic variable database.
(defmacro read-database-file-noninternalformat-macro (condition &rest body)

  (` (let* (this-record
	    this-link
	    last-link
	    first-link)

       (setq dbc-index 0)
       (while (, condition)
	 (setq dbc-index (1+ dbc-index))

	 ;; This is noisy, and it obscures the warning messages, but
	 ;; reading is so slow that it's necessary.
	 (if (and db-inform-interval
		  (zerop (% dbc-index db-inform-interval)))
	     (db-message "Reading database...%d" dbc-index))

	 (,@ body)

	 (setq this-link (make-link-from-record this-record))
	 (if (not first-link)
	     (setq first-link this-link
		   last-link this-link))
	 (link-two last-link this-link)
	 (setq last-link this-link))

       (db-debug-message "read-database-file-noninternalformat-macro:  condition satisfied")

       (if first-link
	   (link-two last-link first-link))
       (database-set-first-link database first-link)
       (database-set-no-of-records database dbc-index))))
(put 'read-database-file-noninternalformat-macro 'lisp-indent-hook 1)
(put 'read-database-file-noninternalformat-macro 'edebug-form-spec '(&rest form))

;; Uses database-read-record-from-region and sepinfo-sepfunc from
;; database-record-sepinfo.

;; Maybe turn some of the support structure that's used both here and in
;; read-database-file-delimited-internal (eg linking stuff) into a
;; macro.

(defun read-database-file-custom (database)
  (let ((region->record (database-read-record-from-region database))
	(record-sepinfo (database-record-sepinfo database)))

    ;; Check that the sepinfo has enough information.
    (if (sepinfo-sep-regexp record-sepinfo)
	(if (not (sepinfo-sep-regexp-submatch record-sepinfo))
	    (error "Need a submatch to go with sep-regexp `%s' of record sepinfo."
		   (sepinfo-sep-regexp record-sepinfo)))
      (if (not (sepinfo-sep-function record-sepinfo))
	  (let ((sep-string (sepinfo-sep-string record-sepinfo)))
	    (if (or (not sep-string)
		    (equal "" sep-string))
		(error "You haven't specified a string, regexp, or function in record sepinfo."))
	    (sepinfo-set-regexp-and-submatch-from-string
	     sepinfo-set-sep-regexp sepinfo-set-sep-regexp-submatch
	     record-sepinfo
	     (sepinfo-sep-string record-sepinfo) database))))

    ;; Merge this into the above, maybe, so I don't have to check twice
    ;; whether we're using sep-function or regexp.

    (db-debug-message "read-database-file-custom:  done checking sepinfo.")

    (if (sepinfo-sep-function record-sepinfo)
	;; use sep-function to delimit the records

	;; return a pair of (end-pos . start-pos)
	(let ((delimit-record (sepinfo-sep-function record-sepinfo))
	      ;; Read-record-from-region might do insertions or deletions,
	      ;; so just remembering the position isn't enough.
	      ;; I don't know that this works, though, due to narrowing.
	      next-start-marker
	      (prev-end-pos 'nil)
	      (end-start-pair '(nil)))
	  (goto-char (point-min))
	  (setq next-start-marker (point-marker))

	  ;; This assumes that there is at least one record in the file.
	  ;; This is a bug.  (Fix it at the beginning of this
	  ;; function or in the caller.)
	  (read-database-file-noninternalformat-macro
	      (marker-position next-start-marker)

	    (setq end-start-pair (funcall delimit-record prev-end-pos))
	    (db-debug-message "read-database-file-custom:  end-start-pair = %s"
			      end-start-pair)
	    (narrow-to-region (marker-position next-start-marker)
			      (car end-start-pair))
	    ;; Writers of record->region often forget to do this.
	    (goto-char (point-min))
	    (db-debug-message "read-database-file-custom:  region = %s %s, next-start = %s"
			      (point-min) (point-max) (cdr end-start-pair))
	    (setq this-record (funcall region->record))
	    ;; not (car end-start-pair) in case buffer was modified
	    (setq prev-end-pos (point-max))
	    (widen)
	    ;; In 18.55, set-marker can only make markers point to the
	    ;; accessible portion of the buffer, so do this after widening.
	    ;; (This may have changed by 18.59.)
	    ;; Don't use just (cdr end-start-pair) because the buffer may
	    ;; have been modified.
	    (set-marker next-start-marker (and (cdr end-start-pair)
					       (+ (- (cdr end-start-pair)
						     (car end-start-pair))
						  prev-end-pos)))
	    (db-debug-message "read-database-file-custom:  next-start-marker = %s"
			      next-start-marker)
	    )

	  )
      ;; use regexp to delimit the records
      (let ((record-sep-regexp (sepinfo-sep-regexp record-sepinfo))
	    (record-sep-regexp-submatch (sepinfo-sep-regexp-submatch
					 record-sepinfo))
	    (next-start-pos (point-min))
	    ;; How many characters to skip before looking for the start
	    ;; of the next record.  This is because region->record may
	    ;; insert or delete, so we can't use a position, but markers
	    ;; get squeezed when narrow-to-region is done.
	    sep-length)
	;; The caller also called database-delete-pre-and-post.
	(goto-char (point-min))

	(read-database-file-noninternalformat-macro
	    (< next-start-pos (point-max))

	  (goto-char next-start-pos)
	  ;; re-search-forward errs if it fails
	  (re-search-forward record-sep-regexp)
	  (setq sep-length (- (match-end record-sep-regexp-submatch)
			      (match-beginning record-sep-regexp-submatch)))
	  (narrow-to-region next-start-pos
			    (match-beginning record-sep-regexp-submatch))
	  (setq this-record (funcall region->record))
	  (setq next-start-pos (+ (point-max) sep-length))
	  (widen))))))



;; If there are any regexps set in the spinfos at this point, then we
;; assume that the programmer specified them explicitly; we assume that if
;; any substitutions are requested, then the programmer knows that they
;; won't cause any ambiguities.
(defun read-database-file-delimited (database)

  (db-debug-message "read-database-file-delimited:  converting complex->simple")

  (goto-char (point-min))

  ;; We have now cleared away the pre- and post- garbage.
  (if (or (sepinfo-sep-regexp (database-field-sepinfo database))
	  (sepinfo-sep-regexp (database-record-sepinfo database)))
      ;; There are regexps involved, so do no substitution on the separators,
      ;; except that the user has explicitly requrested; then read the database.
      (progn
	(database-perform-substitutions database t)
	(read-database-file-delimited-regexp database))

    ;; There are no regexps involved.
    (let* ((fieldsep-string (database-full-fieldsep-string database))
	   (recordsep-string (database-full-recordsep-string database))
	   confirmation-list)

      ;; Convert the database buffer from "complex" to "simple" form.
      (let* ((field-sepinfo (database-field-sepinfo database))
	     (pre-first-field-string (sepinfo-pre-first-string field-sepinfo))
	     (pre-first-field-regexp (sepinfo-pre-first-regexp field-sepinfo))
	     ;; (post-last-field-string (sepinfo-post-last-string field-sepinfo))
	     ;; (post-last-field-regexp (sepinfo-post-last-regexp field-sepinfo))
	     )
	;; database-delete-pre-and-post only did the record separators;
	;; here we do the field separators as well.
	(goto-char (point-min))
	(cond (pre-first-field-regexp
	       (if (db-skip-regexp-forward pre-first-field-regexp)
		   (delete-region (point-min) (point))
		 (error "Didn't find match for regexp `%s' leading the first field."
			pre-first-field-regexp)))
	      (pre-first-field-string
	       (if (db-skip-string-forward pre-first-field-string)
		   (delete-region (point-min) (point))
		 (error "Didn't find string `%s' leading the first field."
			pre-first-field-string))))

	(goto-char (point-max))
	;; Don't get rid of post-last-field-regexp or post-last-field-string
	;; because we look for them at the end of every record.

	;; Make sure that record-sepinfo-sep-string appears at the end.
	(if (sepinfo-sep-string (database-record-sepinfo database))
	    (if (db-skip-string-backward
		 (sepinfo-sep-string (database-record-sepinfo database)))
		(goto-char (point-max))
	      (insert (sepinfo-sep-string (database-record-sepinfo database)))))
	(if pre-first-field-string
	    (insert pre-first-field-string)))

      ;; We're still inside the big let.

      ;; This pre-read confirmation should be optional.  And it should be
      ;; able to deal with regexps.
      ;; When would this test fail?  Won't fieldsep-string and
      ;; recordsep-string always be non-nil when we get here?
      (if (and fieldsep-string recordsep-string)
	  (progn
	    (db-message "Confirming database format...")
	    (setq confirmation-list (database-confirm-fieldsep-and-recordsep
				     fieldsep-string recordsep-string
				     (database-no-of-fields database) nil))
	    ;;      (db-debug-message "read-database-file-delimited:  confirmation-list = %s"
	    ;;	       confirmation-list)
	    (if (or (conflist-fieldsep-bad-p confirmation-list)
		    (conflist-recordsep-bad-p confirmation-list))
		(progn
		  (db-warning "The database file is malformed!")
		  (if (conflist-fieldsep-bad-p confirmation-list)
		      (db-warning "Extra field separator `%s' found in data."
				  fieldsep-string))
		  (if (conflist-recordsep-bad-p confirmation-list)
		      (db-warning "Extra record separator `%s' found in data."
				  recordsep-string))
		  ;; show the db warning buffer
		  (if (yes-or-no-p "Database file is improperly formatted; try to read it anyway? ")
		      (db-message "Damaged database being read; expecting approximately %s records."
			       (conflist-no-of-records confirmation-list))
		    (progn
		      (kill-buffer (current-buffer))
		      (error "Aborted attempt to read database."))))
	      (db-message "Database looks OK from here; expecting %d records."
			  (conflist-no-of-records confirmation-list)))))

      ;; This recomputes the full-fieldsep and full-recordsep; oh well.
      ;; It also sets the io-sep variables.
      (database-substitute-for-read database)

      (read-database-file-delimited-string database)
      )))


;; When we call this, the database is in the following form:
;; Point at start of first field of first record.
;; Each field, except last, is ended by actual-fieldsep.
;; Each record, including last, is ended by actual-recordsep.
;; End of last recordsep-string = eob.
;; Field-sep and record-sep must be strings.  (Perhaps permit regexps later.)

(defun read-database-file-delimited-string (database)

  (db-message "Reading database...")

  (goto-char (point-min))
  (let* ((field-sep (database-sub-fieldsep-string database))
	 (record-sep (database-sub-recordsep-string database))
	 (field-sep-length (length field-sep))
	 (record-sep-length (length record-sep))
	 (no-of-fields (database-no-of-fields database))
	 (max-field-no (1- (database-no-of-fields database)))
	 (here (point))
	 field-no
	 ;; (recordfieldspecs (database-recordfieldspecs database))
	 end-of-record-sep
	 end-of-record)

    (read-database-file-noninternalformat-macro
	(not (or (eobp)
		 ;; Does this cause any problems?
		 ;; Special case for record-sep = "\n\n", extra newline at end
		 (and (string= record-sep "\n\n")
		      (looking-at "\n\\'"))))
      ;; This progn is not strictly necessary, but it serves to delimit the
      ;; extent of the body of read-database-file-noninternalformat-macro.
      (progn
	;; (db-message "read-database-file-delimited-string:  not eobp at %s/%s" (point) (point-max))
	(setq this-record (make-vector no-of-fields nil))
	(if (search-forward record-sep nil t)
	    (setq end-of-record-sep (point)
		  end-of-record (- (point) record-sep-length))
	  (progn
	    (db-warning "Didn't find `%s' at end of last field of record %d, and I put it there myself!"
			record-sep dbc-index)
	    (setq end-of-record-sep (point-max)
		  end-of-record (point-max))))
	(goto-char here)

	(setq field-no 0)
	(while (< field-no max-field-no)

	  ;; I should trap errors here.  Don't check for record-sep in the
	  ;; field text, however, because that's too slow.

	  (if (search-forward field-sep end-of-record t)
	      (progn
		(record-set-field-from-index
		 this-record field-no
		 (buffer-substring here
				   ;; more efficient than (match-beginning 0)
				   (- (point) field-sep-length))
		 nil)
		(setq here (point))
		(setq field-no (1+ field-no)))
	    (progn
	      (db-warning "Hit the end of the record after %d fields of record %d (didn't find field-sep `%s')"
			  field-no dbc-index field-sep)
	      (record-set-field-from-index
	       this-record field-no
	       (buffer-substring here end-of-record)
	       nil)
	      (setq here end-of-record)
	      (setq field-no (1+ field-no))
	      (while (<= field-no max-field-no)
		(record-set-field-from-index
		 this-record field-no
		 (recordfieldspec-default-value
		  (database-recordfieldspec database field-no))
		 nil)
		(setq field-no (1+ field-no))))))
	;; If there weren't too few fields, set the last one (else it's already set).
	(if (= field-no max-field-no)
	    (progn
	      (if (search-forward field-sep end-of-record t)
		  (progn
		    (db-warning "Extra fields in record %d packed into the last field; beware when writing."
				dbc-index)
		    (setq db-io-error-p t)))
	      (record-set-field-from-index this-record max-field-no
					   (buffer-substring here end-of-record)
					   nil)))
	(goto-char end-of-record-sep)
	(setq here (point))))


    ;; The caller takes care of this.
    ;; (kill-buffer (current-buffer))

    ;; Convert from stored to actual format.  It's possible that it will be
    ;; cheaper to do something above without even creating the
    ;; about-to-be-trashed string (ie, operate directly on the buffer).
    ;; But that sounds like too much work by far.
    ;; (db-debug-message "read-database-file-delimited-string:  converting")

    (database-stored->actual database)

    ;; (db-debug-message "read-database-file-delimited-string:  returning")

    ;; This function is called for side-effect, but return the database anyway.
    database))


;; I should clearly merge this with ...-delimited-string eventually.

;; For now I don't deal with pre- and post- on the field sepinfo.  I will soon.
;; (Actually, I do deal with post-, but don't tell.)
(defun read-database-file-delimited-regexp (database)

  (db-message "Reading database...")

  (goto-char (point-min))

  ;; I have a feeling of deja vu.  Have I don't this elsewhere before?
  ;; Maybe in read-sepinfo-items.
  (let* ((field-sepinfo (database-field-sepinfo database))
	 (record-sepinfo (database-record-sepinfo database))
	 (field-regexp (or (sepinfo-sep-regexp field-sepinfo)
			   (regexp-quote (sepinfo-sep-string
					  (database-field-sepinfo database)))))
	(field-regexp-submatch (or (sepinfo-sep-regexp-submatch field-sepinfo)
				   0))
	(record-regexp (or (sepinfo-sep-regexp record-sepinfo)
			   (regexp-quote (sepinfo-sep-string record-sepinfo))))
	(record-regexp-submatch (or (sepinfo-sep-regexp-submatch record-sepinfo)
				    0))

	;; I can't fold these in to the record-regexp because they may have
	;; submatches of their own.
	(pre-first-field-string (sepinfo-pre-first-string field-sepinfo))
	(pre-first-field-regexp (or (sepinfo-pre-first-regexp field-sepinfo)
				    (and pre-first-field-string
					 (regexp-quote pre-first-field-string))))
	(pre-first-field-regexp-submatch (or (sepinfo-pre-first-regexp-submatch
					      field-sepinfo)
					     0))
	(post-last-field-string (sepinfo-post-last-string field-sepinfo))
	(post-last-field-regexp (or (sepinfo-post-last-regexp field-sepinfo)
				    (and post-last-field-string
					 (regexp-quote post-last-field-string))))
	(post-last-field-regexp-submatch (or (sepinfo-post-last-regexp-submatch
					      field-sepinfo)
					     0))

	(no-of-fields (database-no-of-fields database))
	(max-field-no (1- (database-no-of-fields database)))
	(here (point))
	field-no
	;; (recordfieldspecs (database-recordfieldspecs database))
	end-of-record-regexp
	end-of-record)

    (read-database-file-noninternalformat-macro
	(not (eobp))
      ;; This progn is not strictly necessary, but it serves to delimit the
      ;; extent of the body of read-database-file-noninternalformat-macro.
      (progn
	;; (debug "in read-database-file-delimited-macro")
	;; (db-message "read-database-file-delimited-regexp:  not eobp at %s/%s" (point) (point-max))
	(setq this-record (make-vector no-of-fields nil))

	;; Note that in this case I didn't want to get rid of the pre-first stuff.
	(if pre-first-field-regexp
	    (if (db-skip-regexp-forward pre-first-field-regexp)
		(progn
		  (setq here (match-end pre-first-field-regexp-submatch))
		  (goto-char here))
	      (error "Didn't find pre-first stuff I expected.")))

	(setq field-no 0)
	(while (< field-no max-field-no)

	  ;; I should trap errors here.  Don't check for record-regexp in the
	  ;; field text, however, because that's too slow.

	  (if (re-search-forward field-regexp nil t)
	      (progn
		(record-set-field-from-index
		 this-record field-no
		 (buffer-substring here
				   (match-beginning field-regexp-submatch))
		 nil)
		(setq here (match-end field-regexp-submatch))
		(setq field-no (1+ field-no)))
	    (progn
	      (db-warning "Hit the end of the database after %d fields of record %d (didn't find field-regexp `%s')"
			  field-no dbc-index field-regexp)
	      (setq field-no max-field-no))))
	(if (re-search-forward record-regexp nil t)
	    (setq end-of-record-regexp (match-end record-regexp-submatch)
		  end-of-record (match-beginning record-regexp-submatch))
	  (progn
	    (db-warning "Didn't find `%s' at end of last field of record %d."
			record-regexp dbc-index)
	    (setq end-of-record-regexp (point-max)
		  end-of-record (point-max))))
	(goto-char here)
	(if (re-search-forward field-regexp end-of-record t)
	    (progn
	      (db-warning "Too many fields in record %d; packing them all into the last field; beware when writing."
			  dbc-index)
	      (setq db-io-error-p t)))
	(record-set-field-from-index this-record max-field-no
				     (buffer-substring here
						       end-of-record)
				     nil)
	(goto-char end-of-record-regexp)
	(setq here (point))))


    (database-stored->actual database)

    database))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Read database utilities
;;;


;; Each field of each database record has a string; convert some of these
;; into some other internal representation (or even into different
;; strings).

;;   "Convert string slot values in a newly-read database to the actual format.
;; If no argument is specified, use the value of the dynamic variable  database.
;; This makes it possible to be put directly in  db-after-read-hook."
(defun database-stored->actual (&optional db)

  (database-stored->actual-internal (or db database)))


;; The following function used to be part of the above function, but
;; edebug2.7 had trouble debugging it for some reason.

;;   "Do the real work of `database-stored->actual'."
(defun database-stored->actual-internal (db)
  (let (fno-s->a-alist
	s->a
	(field-no 0)
	field-val
	(no-of-fields (database-no-of-fields db)))
    (while (< field-no no-of-fields)
      (setq s->a (recordfieldspec-stored->actual
		  (database-recordfieldspec db field-no)))
      (if s->a
	  (setq fno-s->a-alist
		(cons (cons field-no s->a) fno-s->a-alist)))
      (setq field-no (1+ field-no)))
    (db-debug-message "database-stored->actual:  fno-s->a-alist = %s" fno-s->a-alist)
    (if fno-s->a-alist
	(progn
	  (db-message "Converting from stored record format...")
	  (maprecords-macro
	   (let ((s->a-pairs fno-s->a-alist))
	     (while s->a-pairs
	       (setq field-no (car (car s->a-pairs))
		     field-val (aref maprecords-record field-no)
		     s->a (cdr (car s->a-pairs))
		     s->a-pairs (cdr s->a-pairs))
	       (record-set-field-from-index
		maprecords-record field-no
		(if (stringp field-val)
		    (funcall s->a field-val)
		  field-val)
		nil)))
	   db nil "Converting record format...%s")
	  (db-message "Converting record format...done.")))))

;; Takes a database file as input, reads the auxiliary file if it can.
;; Note that the `database' variable is dynamically bound when the
;; auxiliary file is read.
(defun load-db-aux-file (database)
  (let ((aux-file (or (database-aux-file database)
		       (db-locate-file-with-extensions-on-path
			(database-file database)
			db-aux-file-suffixes db-aux-file-path))))
    (if aux-file
	(load-file aux-file))))

;; This only does the RECORD separators.
;;   "Modify current buffer according to DATABASE.
;; Remove pre-first-record- and post-last-record strings or regexps, then
;; add a field separator to the end so that every record is terminated by one."
(defun database-delete-pre-and-post (database)

  (let ((record-sepinfo (database-record-sepinfo database)))

    ;; Remove gubbish before first record.
    (goto-char (point-min))
    (let ((pre-first-record-string (sepinfo-pre-first-string
				    record-sepinfo))
	  (pre-first-record-regexp (sepinfo-pre-first-regexp
				    record-sepinfo)))
      (cond (pre-first-record-regexp
	     (if (db-skip-regexp-forward pre-first-record-regexp)
		 (progn
		   (goto-char (match-end (sepinfo-pre-first-regexp-submatch
					    record-sepinfo)))
		   (delete-region (point-min) (point)))
	       (error "Didn't find match for regexp `%s' leading the data."
		      pre-first-record-string)))
	    (pre-first-record-string
	     (if (db-skip-string-forward pre-first-record-string)
		 (delete-region (point-min) (point))
	       (error "Didn't find string `%s' leading the data."
		      pre-first-record-string)))))

    ;; Remove gubbish after last record.
    (goto-char (point-max))
    (let ((post-last-record-string (sepinfo-post-last-string
				    record-sepinfo))
	  (post-last-record-regexp (sepinfo-post-last-regexp
				    record-sepinfo)))
      (db-debug-message "database-delete-pre-and-post:  post-string = `%s', post-regexp = `%s'" post-last-record-string post-last-record-regexp)
      (cond (post-last-record-regexp
	     (if (re-search-backward post-last-record-regexp)
		 (progn
		   (db-debug-message "regexp-deleting %d to %d" (point) (point-max))
		   (goto-char (match-beginning (sepinfo-post-last-regexp-submatch
						record-sepinfo)))
		   (delete-region (point) (point-max)))
	       (error "Didn't find match for post-last-record-regexp `%s'."
		      post-last-record-string)))
	    (post-last-record-string
	     (if (search-backward post-last-record-string)
		 (delete-region (point) (point-max))
	       (error "Didn't find post-last-record-string `%s'."
		      post-last-record-string)))))

    ;; (debug)

    ;; Problem:  what if only regexp (not sep-string) is specified?
    ;; Then this crashes.

    ;; Maybe this should be happening in the caller, not here.
    ;; If there isn't one there already, add a final record separator so that
    ;; every record is terminated by one.
    (let ((sep-string (sepinfo-sep-string record-sepinfo)))
      (db-debug-message "sep-string = `%s'" sep-string)
      (if (not (db-skip-string-backward sep-string))
	  (insert sep-string)))
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Write database file
;;;


;; This is called from the data display buffer, and we need the values of
;; variables local to that buffer.  It used to do output to the end of that
;; buffer, call write-region, and finally delete the output, but now it
;; uses a different buffer.

;; I process-current-record before calling this from
;; db-write-database-file, and it isn't called from anywhere else.

(defun write-database-file (database &optional db-file)

  (setq db-io-error-p nil)
  (if db-file
      (if (or (not (file-name-directory db-file))
	      (file-exists-p (file-name-directory db-file)))
	  (database-set-file database db-file)
	(error "Can't write %s: directory does not exist." db-file))
    (setq db-file (database-file database)))

  (in-buffer (generate-new-buffer "write-database-file")
    (buffer-disable-undo (current-buffer))
    (auto-save-mode 0)
    (if (database-internal-file-layout-p database)
	(let ((fl (database-first-link database))
	      (ddbs (database-data-display-buffers database))
	      (modified-p (database-modified-p database))
	      (standard-output (current-buffer)))
	  (insert ";; Database file written by EDB; format 0.6\n")
	  (unwind-protect
	      (progn
		(database-set-first-link database nil)
		(database-set-data-display-buffers database nil)
		(database-set-modified-p database nil)
		(print database))
	    (database-set-first-link database fl)
	    (database-set-data-display-buffers database ddbs)
	    ;; in case we abort out of the write
	    (database-set-modified-p database modified-p)
	    )
	  (insert "(\n")
	  (maprecords (function print) database)
	  (insert "\n)\n")
	  ;; This is not catching the error raised by basic-save-buffer if
	  ;; the destination is not writable.
	  (condition-case error
	      (let ((require-final-newline nil)
		    ;; get around write-file
		    (auto-save-default nil))
		(write-file db-file)
		(database-set-modified-p database nil)
		(kill-buffer (current-buffer))
		)
	    ;; Used to be file-error, which didn't catch the error raised
	    ;; by basic-save-buffer if the destination is not writable.
	    (error
	     (setq db-io-error-p t)
	     ;; This must come before the buffer is killed.
	     (db-warning "Error `%s' while writing buffer %s to file %s."
			 error (buffer-name (current-buffer)) db-file)
	     ;; This ignores `db-disable-debugging-support-p'.  Oh, well.
	     (if (not db-debug-p)
		 (progn
		   ;; avoid further questions about killing the buffer.
		   (set-buffer-modified-p nil)
		   (kill-buffer (current-buffer))))))
	  )
      ;; Don't use internal representation.
      (progn
	;; Fine example of use of undocumented functionality
	;; (this-buffer is bound by in-buffer).
	(db-debug-message "Calling db-copy-buffer-local-variables.")
	(db-copy-buffer-local-variables this-buffer)
	;; We don't want to be *exactly* like the data display buffer.
	(setq major-mode 'write-database-mode
	      buffer-read-only nil)
	(database-io-setup database)
	(let ((first-record t)
	      (record-sepinfo (database-record-sepinfo database)))
	  (db-debug-message "About to insert-maybe pre-first-record-string `%s'."
			    (sepinfo-pre-first-string record-sepinfo))
	  (if (sepinfo-pre-first-string record-sepinfo)
	      (insert (sepinfo-pre-first-string record-sepinfo)))
	  (if (database-write-region-from-record database)
	      ;; Don't do any separator checking at all in the case of
	      ;; write-record-function, even for recordsep, since I have no
	      ;; idea how clever read-record-function is going to be.  Also
	      ;; don't do substitution or quoting.  Probably I shouldn't even
	      ;; be inserting record-sep-string, but it will probably be the
	      ;; empty string anyway.  [Another possibility is that if
	      ;; I don't want it in the speciialized reading/writing
	      ;; functions, I could just set the substitutions list to nil.]
	      (let ((record-sep-string (sepinfo-sep-string record-sepinfo))
		    (write-region-fn (database-write-region-from-record
				      database)))
		(db-message "Writing database...")
		(maprecords (function (lambda (record)
					(if first-record
					    (setq first-record nil)
					  (insert record-sep-string))
					(funcall write-region-fn record)))
			    database nil "Writing database...%d")
		(db-message "Writing database to disk..."))
	    (write-database-file-internal-delimited database))
	  (db-debug-message "done with write-database-file-internal-delimited")
	  (if (sepinfo-post-last-string record-sepinfo)
	      (insert (sepinfo-post-last-string record-sepinfo)))

	  ;; Add the local variables, if any, to the end.
	  (if (database-file-local-variables database)
	      (insert (database-file-local-variables database)))

	  (condition-case error
	      (let ((require-final-newline nil)
		    ;; get around write-file
		    (auto-save-default nil))
		(write-file db-file)
		(database-set-modified-p database nil)
		(kill-buffer (current-buffer))
		)
	    ;; Used to be file-error, which didn't catch the error raised
	    ;; by basic-save-buffer if the destination is not writable.
	    (error
	     (setq db-io-error-p t)
	     ;; This must come before the buffer is killed.
	     (db-warning "Error `%s' while writing buffer %s to file %s."
			 error (buffer-name (current-buffer)) db-file)
	     ;; Did write-file
	     (auto-save-mode 0)
	     ;; This ignores  db-disable-debugging-support-p.  Oh, well.
	     (if (not db-debug-p)
		 (progn
		   ;; avoid further questions about killing the buffer.
		   (set-buffer-modified-p nil)
		   (kill-buffer (current-buffer))))))
	  ))
      )
    )
  ;; In case we wrote the curently-visible database.  This probably fails
  ;; if both the data display buffer and the summary buffer are visible.
  ;; I guess I should have a procedure that fixes the modified bit for the
  ;; mode lines of both buffers.
  (if (database-buffer-p)
      (progn
	(dbc-update-database-modified-p)
	(force-mode-line-update)))
  )


;; This inserts the records, fields separated by fieldsep and records
;; separated by recordsep, into the current buffer; it uses delimited
;; format.  It is called by write-database-file, which arranges
;; unwind-protect boundaries, calls the -internal function that uses the
;; proper output file layout, etc.
(defun write-database-file-internal-delimited (database)

  (let* ((previous-point (point))
	 (fno 0)
	 (first-record t)
	 (no-of-fields (database-no-of-fields database))
	 (write-record-function (database-write-region-from-record database))
	 (fieldsep-string (database-full-fieldsep-string database))
	 (recordsep-string (database-full-recordsep-string database))
	 (sub-fieldsep (or (database-sub-fieldsep-string database) fieldsep-string))
	 (sub-recordsep (or (database-sub-recordsep-string database) recordsep-string))
	 confirmation-list)

    ;; Check the delimiters
    (if (not (database-acceptable-delimiter-p sub-fieldsep))
	(setq sub-fieldsep (database-generate-delimiter nil)))
    (if (not (database-acceptable-delimiter-p sub-recordsep))
	(setq sub-recordsep (database-generate-delimiter nil)))

    (db-message "Writing database...")
    (maprecords
     (function
      (lambda (record)
	;; (db-debug-message "record %s about to be written." record)

	;; This isn't abstracted out, partly because I don't want to pay the
	;; overhead of a function call.

	(setq fno 0)
	(while (< fno no-of-fields)
	  (if (> fno 0) (insert sub-fieldsep))
	  ;; this is record-field-stored, inlined.
	  (insert (db-funcall-maybe (recordfieldspec-actual->stored
				     (database-recordfieldspec database fno))
				    (aref record fno)))
	  (setq fno (1+ fno)))
	(insert sub-recordsep)))
     database nil "Writing database...%d")
    ;; (db-debug-message "wrote all records.")

    (db-message "Writing database...confirming")

    (narrow-to-region previous-point (point-max))
    (setq confirmation-list (database-confirm-fieldsep-and-recordsep
			      sub-fieldsep sub-recordsep
			      (database-no-of-fields database)
			      (database-no-of-records database)))
    (db-debug-message "write-database-file-internal-delimited:\n  confirmation-list = %s" confirmation-list)
    (if (or (conflist-fieldsep-bad-p confirmation-list)
	    (conflist-recordsep-bad-p confirmation-list))
	(progn
	  (db-debug-message "confirmation failed")
	  (if (conflist-fieldsep-bad-p confirmation-list)
	      (progn
		(db-warning "Tripped over an unexpected field separator `%s' in data; trying again."
			    sub-fieldsep)
		(database-set-sub-fieldsep-string database
		      (database-generate-delimiter database t))))
	  (if (conflist-recordsep-bad-p confirmation-list)
	      (progn
		(db-warning "Tripped over an unexpected record separator `%s' in data; trying again."
			 sub-recordsep)
		(database-set-sub-recordsep-string database
		      (database-generate-delimiter database t))))

	  ;; We've chosen new separators; erase the work so far.
	  (delete-region previous-point (point))

	  ;; Call this function recursively.
	  (write-database-file-internal-delimited database)
	  ;; I've called this recursively; don't do any substitution
	  ;; or quoting.
	  )
      ;; Confirmation was OK:  correct number of field and record
      ;; separators found.
      (progn
	(db-debug-message "confirmation succeeded")

	;; Put off adding the pre- and post- field strings until
	;; after checking separators, as they may contain anomolous
	;; field separators, for instance.

	;; But do it before substitution so that all field pre- and
	;; post- strings are treated identically.

	;; The whole point of using io-separators is so they
	;; appear exactly as the user specified, unaffected by
	;; substitution.


	;; But note that pre- and post- record strings will be added
	;; later, after substitution.

	(database-substitute-for-write database
				       fieldsep-string sub-fieldsep
				       recordsep-string sub-recordsep)
	;; Convert from "simple" to "complex" form.
	(goto-char (point-min))
	(let ((pre-first (sepinfo-pre-first-string (database-field-sepinfo database))))
	  (if pre-first
	      (insert pre-first)))

	(goto-char (point-max))
	(db-debug-message "at point-max")
	(if (and (db-skip-string-backward (or (sepinfo-pre-first-string
					    (database-field-sepinfo database))
					   ""))
		 (db-skip-string-backward (or (sepinfo-sep-string
					    (database-record-sepinfo database))
					   "")))
	    (delete-region (point) (point-max))
	  (error "Didn't find expected trailing junk `%s' or `%s'."
		 (sepinfo-pre-first-string (database-field-sepinfo database))
		 (sepinfo-sep-string (database-record-sepinfo database))))
	(db-debug-message "about to widen")


	))
    (widen)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; I/O utilities
;;;

(defun database-io-setup (database &optional dont-set-regexps)

  ;; Perhaps these should be dealt with via with- forms as well.
  ;; Or have local variables that they set.
  ;; Or global ones that they set unconditionally.

  ;; Some of this information may already be correctly set (especially if
  ;; we're now writing), but just in case some of the database slots have
  ;; changed since reading (eg, quotation character is different now).

  (db-debug-message "database-io-setup called.")
  (database-check-all-sepinfos database dont-set-regexps)
  (db-debug-message "database-check-all-sepinfos returned in database-io-setup.")
  ;; (db-set-field-quotation-vars database)
  (setq db-substitution-no-no-string
	(apply (function concat)
	       (mapcar (function (lambda (string-cons)
				   (concat (car string-cons)
					   (cdr string-cons))))
		       (database-substitutions database))))
  (db-debug-message "database-io-setup returning.")
  )


;;; The user should set the slots directly.
;; Doesn't set unless the variables are non-nil.
;; (defun set-database-slots-from-buffer-local-values-maybe (database)
;;   (if db-record-fieldnames
;;       (database-set-fieldnames database db-record-fieldnames))
;;   (if db-substitutions
;;       (database-set-substitutions database db-substitutions)))

;; This is the stuff that needs to happen before the format is parsed but after
;; the format's local variables have been hacked.
;; It has nothing to do with the format per se, only with database slots.
;; Keep it abstracted out!
(defun db-set-field-variables (database)
  ;; This is now done by database-set-fieldnames-to-list.
  ;; (db-set-fieldname-vars database)
  (if (not (database-field-priorities database))
      (database-set-field-priorities database
	    (let ((fno -1))
	      (cons
	       (mapcar (function (lambda (fieldname)
				   (setq fno (1+ fno))
				   (list fno)))
		       (database-fieldname-alist database))
	       nil)))))


;; When converting strings to regexps, must be careful to watch out for
;; substitution and quotation:  don't get fooled.
(defun database-check-all-sepinfos (database &optional dont-set-regexps)

  ;; When writing to the file, we take the previous version's local
  ;; variables section verbatim.

  ;; We're only setting the regexp variables, which the average user won't
  ;; touch.

  ;; When reading, we'll get the variables from the database, auxiliary,
  ;; and format files anew each time anyway.

  (database-check-sepinfo (database-record-sepinfo database) database
			  "record" "\n" dont-set-regexps)
  (database-check-sepinfo (database-field-sepinfo database) database
			  "field" "\t" dont-set-regexps)
  (database-check-sepinfo (database-alternative-sepinfo database) database
			  "alternative" t dont-set-regexps)
  )


;; It's good that this setting of -regexp slots doesn't happen until the
;; last possible moment, when quotation-char and other variables that these
;; values depend on are already set to their final values.

(defun database-check-sepinfo (sepinfo database sepinfo-name
				       &optional sep-string-default dont-set-regexps)

  ;; If the sep-string slot of the sepinfo is nil, then:
  ;;  * if SEP-STRING-DEFAULT is nil, signal an error.
  ;;  * if SEP-STRING-DEFAULT is t, do nothing.
  ;;  * otherwise set the slot to SEP-STRING-DEFAULT.

  ;; If the string is the empty string, then we must have just set it that
  ;; way, which means that we either also just set the regexp to nil,
  ;; or we set the regexp to something we care about.  In either case don't
  ;; mess further with the regexp.

  ;; The submatches could default to 0 if they're nil; but I want to be
  ;; more paranoid than that.

  (if (and (sepinfo-pre-first-string sepinfo)
	   (equal "" (sepinfo-pre-first-string sepinfo)))
      ;; pre-first-string = ""
      (sepinfo-set-pre-first-string sepinfo nil))
  (if (sepinfo-pre-first-regexp sepinfo)
      (if (not (sepinfo-pre-first-regexp-submatch sepinfo))
	  (error "Need a submatch to go with pre-first-regexp `%s' of %s."
		 (sepinfo-pre-first-regexp sepinfo) sepinfo-name))
    (if (and (sepinfo-pre-first-string sepinfo)
	     (not dont-set-regexps))
	(sepinfo-set-regexp-and-submatch-from-string
	 sepinfo-set-pre-first-regexp sepinfo-set-pre-first-regexp-submatch
	 sepinfo
	 (sepinfo-pre-first-string sepinfo) database)))

  ;; Don't test for sep-function because the sepinfo must be valid for
  ;; output as well as input.
  ;; On the other hand, we don't need sep-string to be set if a wrfr
  ;; function is in use, but this function doesn't do any such checks.
  (if (or (not (sepinfo-sep-string sepinfo))
	  (and (equal "" (sepinfo-sep-string sepinfo))
	       (not (sepinfo-sep-regexp sepinfo))))
      ;; sep-string isn't set appropriately; it contains no information
      (cond ((eq t sep-string-default)
	     ;; do nothing
	     )
	    ((not sep-string-default)
	     (error "Sep-string must be non-empty in %s." sepinfo-name))
	    (t
	     (sepinfo-set-sep-string sepinfo sep-string-default))))
  (if (sepinfo-sep-regexp sepinfo)
      (if (not (sepinfo-sep-regexp-submatch sepinfo))
	  (error "Need a submatch to go with sep-regexp `%s' of %s."
		 (sepinfo-sep-regexp sepinfo) sepinfo-name))
    (if (and (not (sepinfo-sep-function sepinfo))
	     (not dont-set-regexps))
	(sepinfo-set-regexp-and-submatch-from-string
	 sepinfo-set-sep-regexp sepinfo-set-sep-regexp-submatch sepinfo
	 (sepinfo-sep-string sepinfo) database)))

  (if (and (sepinfo-post-last-string sepinfo)
	   (equal "" (sepinfo-post-last-string sepinfo)))
      (sepinfo-set-post-last-string sepinfo nil))
  (if (sepinfo-post-last-regexp sepinfo)
      (if (not (sepinfo-post-last-regexp-submatch sepinfo))
	  (error "Need a submatch to go with post-last-regexp `%s' of %s."
		 (sepinfo-post-last-regexp sepinfo) sepinfo-name))
    (if (and (sepinfo-post-last-string sepinfo)
	     (not dont-set-regexps))
	(sepinfo-set-regexp-and-submatch-from-string
	 sepinfo-set-post-last-regexp sepinfo-set-post-last-regexp-submatch
	 sepinfo
	 (sepinfo-post-last-string sepinfo) database))))


;; These are macros because we're doing a setq on the variable names.

;; Shouldn't be using this global variable "database", perhaps.

;; Note that each argument will be evaluated only once; no need to let
;; a gensym to (, string), though that might result in smaller code.


;; (defmacro setf-regexp-and-submatch-from-string
;;   (regexp-name submatch-name string database)
;;   (` (let ((str (, string))
;; 	   (db (, database)))
;;        (cond ((or (null str) (equal "" str))
;; 	      ((!!) setf (, regexp-name) nil
;; 		    (, submatch-name) nil))
;; 	     ((database-quotation-char db)
;; 	      ((!!) setf (, regexp-name) (when-not-preceded-by-quotation-char
;; 				     str db)
;; 		    (, submatch-name) 2))
;; 	     (t
;; 	      ((!!) setf (, regexp-name) (regexp-quote str)
;; 		    (, submatch-name) 0))))))


;; REGEXP-NAME and SUBMATCH-NAME are symbols
(defmacro sepinfo-set-regexp-and-submatch-from-string
  (regexp-setter submatch-setter sepinfo string database)
  (` (let ((str (, string))
	   (db (, database))
	   (si (, sepinfo)))
       (cond ((or (null str) (equal "" str))
	      ((, regexp-setter) si nil)
	      ((, submatch-setter) si nil))
	     ;; ((database-quotation-char db)
	     ;;  ((, regexp-setter) si (when-not-preceded-by-quotation-char
	     ;; 			     str db))
	     ;;  ((, submatch-setter) si 2))
	     (t
	      ((, regexp-setter) si (regexp-quote str))
	      ((, submatch-setter) si 0))))))

;; All this hinges on the database fieldnames, which has been set to a list
;; but will now be converted to a vector.

;;   "Set variables and slots of DATABASE that can be set from optional argument
;; FIELDNAMES-LIST or, if it is nil, from the fieldnames slot of the database."
(defun db-set-fieldname-vars (database &optional fieldnames-list)
  (if (not fieldnames-list)
      (setq fieldnames-list (database-fieldnames database)))
  (db-debug-message "db-set-fieldname-vars:  %s" fieldnames-list)

  ;; the `type' variable can be done away with.
  (let* ((no-of-fields (length fieldnames-list))
	 (fno -1)
	 type
	 (recordfieldspecs (make-vector no-of-fields nil))
	 (fieldnames-vector (make-vector no-of-fields nil)))
    (database-set-fieldname-alist database
	  (mapcar (function
		   (lambda (fname)
		     (setq fno (1+ fno)
			   type (if (consp fname)
				    (prog1 (cdr fname)
				      (setq fname (car fname)))
				  db-default-field-type))
		     (db-debug-message "%s %s %s" fname fno type)
		     (if (recordfieldtype-p type)
			 (aset recordfieldspecs fno type)
		       (error "db-set-fieldname-vars:  bad type %s" type))
		     (aset fieldnames-vector fno fname)
		     (cons fname fno)))
		  fieldnames-list))
    ;; (db-debug-message "set-recordfieldspec-vars:  recordfieldspec-fieldnames = %s" fieldnames-list)
    ;; (db-debug-message "set-recordfieldspec-vars:  recordfieldspec-fieldname-alist = %s" fieldnames-and-numbers)
    (database-set-no-of-fields database no-of-fields)
    (database-set-fieldnames database fieldnames-vector)
    (database-set-recordfieldspecs database recordfieldspecs)))

;; (defun set-db-vars (database)
;;   (db-set-field-quotation-vars database))

(defun recordfieldtype->recordfieldspec (recordfieldtype)
  "Return the recordfieldspec associated with symbol RECORDFIELDTYPE."
  (let ((result (cdr (assoc recordfieldtype db-recordfieldtypes))))
    (cond ((recordfieldspec-p result)
	   result)
	  ((symbolp result)
	   ;; recursive call
	   (recordfieldtype->recordfieldspec result)))))
;; Was inlined when there was no chance of a recursive call.
;; (proclaim-inline recordfieldtype->recordfieldspec)
;; Not really necessary even now.
;; (proclaim-notinline recordfieldtype->recordfieldspec)

(defun recordfieldtype-p (recordfieldtype)
  (assoc recordfieldtype db-recordfieldtypes))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Substitution
;;;

;; When substitution is called, the database is always in "simple" format:
;; fields are separated by sub-fieldsep, and records (including the last
;; one) are followed by sub-recordsep.

;; The "complicated" format includes pre-first-field at the beginning and
;; post-last-field (without record-sep-string or pre-first-field, which are
;; the other elements of sub-recordsep) at the end.

;; Simple format is nice because it requires no special-casing at the
;; beginning (because there's no gubbish there) or at the end (because
;; what's there is exactly what's between each pair of records).

;; The caller of database-substitute-for-* arranges to convert to/from
;; simple format before/after the call.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This is called only by read-database-file-delimited.

;; Its purspose is to convert field/record separators (perhaps choosing new
;; ones), so they won't get damaged by the substitution, and then doing the
;; substitution.  It also must set the sub-{field,record}sep slots, because
;; later on in read-database-file-delimited-internal, those fields are
;; slavishly followed.

;; No substitution should occur if either of the separators is a regexp, as
;; I can't parse them (or do substitution in them).

;; Perhaps it should trust the settings for sub-fieldsep et al in the
;; database, and not do anything if they're set?

(defun database-substitute-for-read (database)
  (let* ((fieldsep-orig (database-full-fieldsep-string database))
	 (recordsep-orig (database-full-recordsep-string database))
	 (fieldsep-sub (or (database-sub-fieldsep-string database)
			   fieldsep-orig))
	 (recordsep-sub (or (database-sub-recordsep-string database)
			    recordsep-orig)))

    ;; fieldsep-sub and recordsep-sub are never used after this!
    (db-debug-message "d-s-f-r `%s' `%s' `%s' `%s'"
		      fieldsep-orig recordsep-orig fieldsep-sub recordsep-sub)

    (if (or (sepinfo-sep-regexp (database-record-sepinfo database))
	    (sepinfo-sep-regexp (database-field-sepinfo database)))
	(progn
	  (database-set-sub-recordsep-string database
	     (or (sepinfo-sep-regexp (database-record-sepinfo database))
		 (database-sub-recordsep-string database)
		 recordsep-orig))
	  (database-set-sub-fieldsep-string database
	     (or (sepinfo-sep-regexp (database-field-sepinfo database))
		 (database-sub-fieldsep-string database)
		 fieldsep-orig)))
      ;; No regexps to be found.
      (progn
	;; If the recordsep is a substring of fieldsep, this order is a
	;; disaster (unless we do substitution in the fieldsep string as well
	;; as in the buffer, which is an interesting idea); perhaps get some
	;; info back in the confirmation list about that, too.  Or just trust
	;; that that won't happen; fieldsep being a substring of recordsep is
	;; much more likely, after all.

	;; Since I'll be checking against a database file on which
	;; substitution has already been performed, will the order of
	;; reverse substitution be important?

	(db-debug-message "Checking recordsep-orig `%s'." recordsep-orig)
	(if (not recordsep-orig)
	    (error "No record separator specified."))
	(if (database-acceptable-delimiter-p recordsep-orig)
	    (database-set-sub-recordsep-string database recordsep-orig)
	  (progn
	    (db-message "Substituting record delimiter for read...")
	    (database-set-sub-recordsep-string database
					(database-generate-delimiter database))
	    (db-debug-message "Substituting record delimiter for read... (`%s' for `%s')"
			      (database-sub-recordsep-string database) recordsep-orig)
	    (goto-char (point-min))
	    (replace-string recordsep-orig (database-sub-recordsep-string database))
	    (setq fieldsep-orig (db-string-substitute-substring-general-case
				 (database-sub-recordsep-string database) recordsep-orig
				 fieldsep-orig))
	    (db-message "Substituting record delimiter for read...done")
	    ))

	(db-debug-message "Checking fieldsep-orig `%s'." fieldsep-orig)
	(if (or (database-acceptable-delimiter-p fieldsep-orig)
		(database-read-record-from-region database))
	    (progn
	      (db-message "fieldsep-orig `%s' is acceptable." fieldsep-orig)
	      (db-message "because (or %s %s)"
		       (database-acceptable-delimiter-p fieldsep-orig)
		       (database-read-record-from-region database))
	      (database-set-sub-fieldsep-string database fieldsep-orig))
	  (progn
	    (if (not fieldsep-orig)
		(error "No field separator specified."))
	    (db-message "Substituting field delimiter for read...")
	    (database-set-sub-fieldsep-string database
				       (database-generate-delimiter database))
	    (db-message "Substituting field delimiter for read... (`%s' for `%s')"
			(database-sub-fieldsep-string database) fieldsep-orig)
	    (goto-char (point-min))
	    (replace-string fieldsep-orig (database-sub-fieldsep-string database))
	    (db-message "Substituting field delimiter for read...done")
	    ))
	(db-message "sub-fieldsep = %s" (database-sub-fieldsep-string database))

	(db-debug-message "database-substitute-for-read:  substitutions = %s"
			  (database-substitutions database))

	(database-perform-substitutions database t)
	))))


;; Perhaps have an optional argument which causes this to ignore problems
;; with ambiguities.
(defun database-perform-substitutions (database backward)
  (if (database-substitutions database)
      (progn
	(db-debug-message "Substituting %s" (database-substitutions database))
	(db-message "Substituting...")
	(let ((ambiguities (buffer-substitute
			    (database-substitutions database) backward t)))
	  (if ambiguities
	      (error "Ambiguities:  %s" ambiguities)))
	(db-message "Substituting...done"))))


;; Database-substitute-for-write does the substitution, then, if field
;; separators were changed in order to prevent them from getting damaged by
;; the substitution, converts them back to the user-specified strings,
;; which might contain substrings that would have been substituted for in
;; the previous operation, had we not been careful.
(defun database-substitute-for-write (database new-fieldsep sub-fieldsep
					       new-recordsep sub-recordsep)
  ;; Check that there are the correct number of fieldseps and recordseps
  ;; here; if wrong number, choose new fieldsep and/or recordsep and take
  ;; it from the top.
  ;; Or will that have been done beforehand?

  (db-debug-message "database-substitute-for-write: %s %s %s %s"
		    (prin1-to-string new-fieldsep)
		    (prin1-to-string sub-fieldsep)
		    (prin1-to-string new-recordsep)
		    (prin1-to-string sub-recordsep))

  (db-debug-message "database-substitute-for-write:  substitutions = %s"
		    (database-substitutions database))
  (db-debug-message "database-substitute-for-write:  about to call database-perform-substitutions")

  (database-perform-substitutions database nil)

  (if (not (equal sub-fieldsep new-fieldsep))
      (progn
	(db-debug-message "replacing fieldsep")
	(goto-char (point-min))
	(replace-string sub-fieldsep new-fieldsep)))
  (if (not (equal sub-recordsep new-recordsep))
      (progn
	(db-debug-message "replacing recordsep")
	(goto-char (point-min))
	(replace-string sub-recordsep new-recordsep)))

  ;; Now the buffer is ready to have the preceding and trailing junk
  ;; added and to be written to disk.
  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Substitution Utilities
;;;


;; Remember that the separators used when reading worked, and if they still
;; pass the substitution test, use them again when writing.
;; [Do this via a database slot.]


;; If there are no conflicts, these can be set to their usual values and
;; everything is happy.  Otherwise...


;; Maybe get rid of "database-" from the name of this function.

;; Is there a better way (ie, fewer false alarms) of checking whether a
;; separator is OK (wrt substitution) than checking each character
;; individually?

;; If the seps are strings, I can test whether an overlapping match is
;; possible by testing whether each prefix of the string is a suffix.  (Do
;; I need to cross-test both of them?)  If I could show that overlaps are
;; impossible, I could use how-many (or how-many-string, etc.) directly.

;;; This is wrong:  I always get it right the first time around, because I
;;; check for substitution conflicts.
;; If I've been too generous about the recordsep (ie, I know the fieldsep
;; is not OK but it looks like the only problem is that there are too many
;; fieldseps), then I can always catch it the next time around...when
;; reading, I always get it on the first try.

;; Return a list of (fieldsep-bad-p recordsep-bad-p no-of-records), aka a
;; conflist.  Useful both when reading and when writing.  (When reading,
;; database-no-of-records should be nil.)
(defsubst conflist-fieldsep-bad-p (conflist) (car conflist))
(defsubst conflist-recordsep-bad-p (conflist) (car (cdr conflist)))
(defsubst conflist-no-of-records (conflist) (car (cdr (cdr conflist))))

;; This assumes that recordsep appears at the end of the database as well.

(defun database-confirm-fieldsep-and-recordsep (fieldsep recordsep no-of-fields no-of-records)
  (let* ((fieldsep-matches (progn (goto-char (point-min))
				  (db-how-many-string-overlapping fieldsep)))
	 (recordsep-matches (progn (goto-char (point-min))
				   (db-how-many-string-overlapping recordsep)))
	 (goal-fieldsep-matches (and no-of-records
				     (* no-of-records
					(1- no-of-fields))))
	 (goal-recordsep-matches no-of-records))
    ;; Possibly even this can be merged into the main body.
    ;; But keeping it separate might save some computation and special-casing.
    (if (equal fieldsep recordsep)
	(progn
	  (db-debug-message "database-confirm-fieldsep-and-recordsep:  fieldsep = recordsep")
	  (db-debug-message "   fm = %s, rm = %s, gfm = %s, grm = %s, nof = %s, nor = %s"
		   fieldsep-matches recordsep-matches
		   goal-fieldsep-matches goal-recordsep-matches
		   no-of-fields no-of-records)

	  (if no-of-records
	      (if (= recordsep-matches (+ goal-recordsep-matches
					  goal-fieldsep-matches))
		  (list nil nil no-of-records)
		(list t t nil))
	    ;; Remember that fieldsep = recordsep when reading this code.
	    (if (zerop (% recordsep-matches no-of-fields))
		(list nil nil (/ recordsep-matches no-of-fields))
	      (if (zerop (% (1+ recordsep-matches) no-of-fields))
		  ;; A field separator at the end of the data was misinterpreted
		  ;; as a record separator, so no record separator was added.
		  (progn (goto-char (point-max))
			 (insert recordsep)
			 (list nil nil (/ (1+ recordsep-matches) no-of-fields)))
		(list t t (/ recordsep-matches no-of-fields))))))
      ;; fieldsep and recordsep unequal; see if one is a substring of the other.

      ;; At least one of these must be zero.
      (let ((f-in-r (db-how-many-substring-overlapping fieldsep recordsep))
	    (r-in-f (db-how-many-substring-overlapping recordsep fieldsep)))
	(db-debug
	 (db-debug-message "database-confirm-fieldsep-and-recordsep:")
	 (db-debug-message "   fm = %s, rm = %s, gfm = %s, grm = %s, nof = %s, nor = %s, f-in-r = %s, r-in-f = %s"
		  fieldsep-matches recordsep-matches
		  goal-fieldsep-matches goal-recordsep-matches
		  no-of-fields no-of-records
		  f-in-r r-in-f)
	 )

	(if no-of-records
	    (progn
	      ;; at most one of these is nonzero, so cond is OK.
	      (cond ((> f-in-r 0)
		     (setq goal-fieldsep-matches
			   (+ goal-fieldsep-matches
			      (* goal-recordsep-matches f-in-r))))
		    ((> r-in-f 0)
		     (setq goal-recordsep-matches
			   (+ goal-recordsep-matches
			      (* goal-fieldsep-matches r-in-f)))))
	      (list (not (= fieldsep-matches goal-fieldsep-matches))
		    (not (= recordsep-matches goal-recordsep-matches))
		    no-of-records))
	  (progn
	    (setq no-of-fields (+ no-of-fields f-in-r))
	    (let ((apparent-records (/ fieldsep-matches (1- no-of-fields))))
	      (setq goal-recordsep-matches
		    (if (zerop apparent-records)
			1
		      (* apparent-records
			 (1+ (* r-in-f fieldsep-matches)))))
	      (db-debug
	       (db-debug-message "database-confirm-fieldsep-and-recordsep:  ar = %d, grm = %d, nof = %d"
			apparent-records goal-recordsep-matches no-of-fields))
	      (list (or
		     ;; Wrong number of fields:  some record has too many or too few.
		     ;; This clause equivalent to:
		     ;; (not (zerop (% fieldsep-matches (1- no-of-fields))))
		     ;; which is more efficient?
		     (not (= fieldsep-matches (* apparent-records
						 (1- no-of-fields))))
		     ;; too many fieldseps compared to recordseps
		     (< recordsep-matches goal-recordsep-matches))
		    ;; too many recordseps compared to fieldseps
		    (< goal-recordsep-matches recordsep-matches)
		    apparent-records))))))
    ))



;; Note that this is global; hope we only write/read one database at a
;; time.  It should be nilled out before starting to read/write a database.
;; Maybe don't bother with this and go searching down the substitutions
;; list (which won't be very long, typically) as is.  Maybe use a big
;; vector (one elt per character) to show which characters are OK.  Maybe
;; I'm worrying too much about this little issue.

(defvar db-substitution-no-no-string nil)

;; Calling this repeatedly could be slow because of repeated computation of
;; substitution-no-no-string; maybe compute it (or, better, nil it out; can
;; check and set if need be) at the beginning of reading/writing.

;; Get rid of database argument.

;; Return t iff no characters of delimiter appear in substitution-no-no-string.
(defun database-acceptable-delimiter-p (delimiter)
  (if delimiter
      (let ((result t)
	    (string-index 0)
	    (delimiter-length (length delimiter)))
	(while (and result (< string-index delimiter-length))
	  (if (db-find-char (elt delimiter string-index)
			 db-substitution-no-no-string)
	      (setq result nil)
	    (setq string-index (1+ string-index))))
	result)))

;; call this in the write buffer.
(defun database-generate-delimiter (database &optional check-buffer)
  (let ((string (make-string 1 0))
	(candidate-char 0)
	result)
    (while (and (< candidate-char 256) (not result))
      (aset string 0 candidate-char)
      (if (and (database-acceptable-delimiter-p string)
	       (not (and check-buffer
			 (progn
			   (goto-char (point-min))
			   (search-forward string nil t))))
	       (not (db-find-char candidate-char
			       (or (database-sub-fieldsep-string database)
				   (database-full-fieldsep-string database))))
	       (not (db-find-char candidate-char
			       (or (database-sub-recordsep-string database)
				   (database-full-recordsep-string database)))))
	  (setq result string)
	(setq candidate-char (1+ candidate-char))))
    (or result
	(error "I can't find an acceptable delimiter!"))))


(defun database-set-temp-delimiters (database db-buffer)
  ;; DB-BUFFER is only set if this is for reading.
  ;; (Well, for writing I'm only going to call this if there was some
  ;; sort of trouble and I need to recompute from the database buffer,
  ;; so I suspect that DB-BUFFER will always be set.  Maybe just use
  ;; the current buffer.)


  ;; If reading, we have to trust that there are only the correct number
  ;; of field delimiters currently in the buffer.




  )


;; Quotation is no longer enabled.

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;; Quotation
;; ;;;
;; 
;; (defun db-set-field-quotation-vars (database)
;;   (let ((quotation-char (database-quotation-char database)))
;;     (if (stringp quotation-char)
;; 	(let ((quot-char-len (length quotation-char)))
;; 	  (cond ((= 0 quot-char-len)
;; 		 (setq quotation-char nil))
;; 		((= 0 quot-char-len)
;; 		 (setq quotation-char (elt quotation-char 1)))
;; 		(t
;; 		 (error "Quotation character should be just that, not some rambling string like `%s'." quotation-char)))
;; 	  (database-set-quotation-char database quotation-char)))
;;     (if quotation-char
;; 	(progn
;; 	  (database-set-quotation-char-regexp database
;; 	      (regexp-quote (char-to-string (database-quotation-char database))))
;; 	  (database-set-actual-quoted-regexp database
;; 	      (or (database-quoted-regexp database)
;; 		  (mapconcat (function regexp-quote)
;; 			     (sort (or (database-quoted-strings database)
;; 				       (quoted-strings-default database))
;; 				   (function string-longer-p-function))
;; 			     "\\|")))))))
;; 
;; ;; There are so many things that this could be dependent on (including all
;; ;; the strings in all the sepinfos) that I have a hidden variable here.
;; ;; This ensures that all the proper changes get propagated to the proper
;; ;; places.  (Say that ten times fast.)
;; 
;; 
;; ;; (set-field-quotation-vars '("foo" "bar" "f" "foobar" "[]"))
;; 
;; ;; This could be inefficient:  since it will be called repeatedly, it would
;; ;; be nice not to have to check database-quotation-character and other
;; ;; database slots all the time.
;; 
;; ;; One possibility is to put it all in a buffer (say, different fields on
;; ;; different lines) and do a replace-regexp, then cons it all up.  But then
;; ;; again, I expect there to be very little quoting in general, and that
;; ;; sounds like a painful solution.
;; 
;; (defun field-quote (fieldvalue database)
;;   (if (and (database-quotation-char database)
;; 	   (string-match (database-quoted-regexp database) fieldvalue))
;;       (concat (substring fieldvalue 0 (match-beginning 0))
;; 	      (database-quotation-char database)
;; 	      (substring fieldvalue (match-beginning 0) (match-end 0))
;; 	      (field-quote (substring fieldvalue (match-end 0)) database))
;;     fieldvalue))
;; 
;; ;; Is this use of actual-quoted-regexp correct?
;; (defun field-unquote (fieldvalue database)
;;   (if (database-quotation-char database)
;;       (let ((i (string-match (database-quotation-char-regexp database)
;; 			     fieldvalue)))
;; 	(if i
;; 	    (if (= (1+ i) (string-match (database-actual-quoted-regexp database)
;; 					fieldvalue (1+ i)))
;; 		(concat (substring fieldvalue 0 i)
;; 			(substring fieldvalue (1+ i) (match-end 0))
;; 			(field-unquote (substring fieldvalue (match-end 0))
;; 				       database))
;; 	      (concat (substring fieldvalue 0 (1+ i))
;; 		      (field-unquote (substring fieldvalue (1+ i))
;; 				     database)))
;; 	  fieldvalue))
;;     fieldvalue))
;; 
;; ;; Later on the result will probably get sorted by length, but I don't do it now.
;; (defun quoted-strings-default (database)
;;   (if (database-quotation-char database)
;;       (delq nil
;; 	    (list
;; 	     (sepinfo-pre-first-string (database-record-sepinfo database))
;; 	     (sepinfo-sep-string (database-record-sepinfo database))
;; 	     (sepinfo-post-last-string (database-record-sepinfo database))
;; 
;; 	     (sepinfo-pre-first-string (database-field-sepinfo database))
;; 	     (sepinfo-sep-string (database-field-sepinfo database))
;; 	     (sepinfo-post-last-string (database-field-sepinfo database))
;; 
;; 	     (sepinfo-pre-first-string (database-alternative-sepinfo database))
;; 	     (sepinfo-sep-string (database-alternative-sepinfo database))
;; 	     (sepinfo-post-last-string (database-alternative-sepinfo database))
;; 
;; 	     (char-to-string (database-quotation-char database))))))
;; 
;; (defun when-not-preceded-by-quotation-char (string database)
;;   (concat
;;    "[^" (database-quotation-char database) "]"
;;    "\\("
;;    (regexp-quote (make-string 2 (database-quotation-char database)))
;;    "\\)*"
;;    "\\(" (regexp-quote string) "\\)"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Random useful functions
;;;

;; A sep-function for records with n lines.
(defun record-sep-lines-function (n)
  (` (lambda (post-last-item-pos)
		 (next-line (, n))
		 (let ((here (point)))
		   (cons (1- here)
			 (if (< here post-last-item-pos)
			     here))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Stuff
;;;


;; I don't want to have to use db-how-many-*-overlapping because I ought to
;; have been smarter than that in choosing my field separators (such that
;; there's no overlap).  But, on the other hand, it is possible that after
;; all is said and done and I've replaced the actual for the generated
;; separator strings, then ambiguities arise.  Eg, if one field ends "ab"
;; and the actual separator string is "aba".  This seems to imply that
;; quoting alone is insufficient (unless I decided to quote single
;; characters; yes, that's the right thing to do).

;; [As mentioned elsewhere, this is a danger if (?? and only if ??) some
;; prefix of the separator is a suffix of the separator.  (?? Check only
;; within wone separator, or check both?  Overlapping has nothing to do
;; with having characters in common with substitutions, however; it is
;; after substituting in that ambiguities may arise.  The only solution for
;; them is quoting, or substitution out of any possible ambiguities (ie,
;; substitution for every character of, or at least first and last
;; characters of, separators); this is feasible if separators are small,
;; but totherwise? ??)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Quoting and substitution take too long (too many function calls, and too
;; much work usually for naught, too much garbage creation) when called on
;; individual strings.  Don't want to put quoting/substitution in the s->a and
;; a->s functions (and in the constraint functions) because of the burden on
;; the user, even though that would amortize the time for this operation
;; (check each time a field is changed) and speed quoting/substitution (call
;; it only on the fields that might need it); besides, if it was called on
;; many fields, using s->a and a->s still results in many function calls.
;;
;; So do quoting and substitution on the database when it's in a buffer.
;;
;; Don't do them by record or by field, even though that would guarantee that
;; we didn't munge the separators, because it would be too slow, even in the
;; database buffer.
;;
;; If q/s won't munge the separators (look at the substitutions about to be
;; done), then great.  Otherwise use some characters that are guaranteed to be
;; ok as field and record separators.  When reading, read from disk, replace
;; the actual separators by these characters, do quoting/substitution, and
;; then parse the database using the generated characters.  When writing,
;; write to a buffer with the characters, do quoting/substitution, replace the
;; charcters with the real delimiters, and finally write to disk.
;;
;;
;;
;; Have variables that control whether the separator characters are allowed in
;; the database; if no, add that to the constraint function for every field.
;; (Of course, this works only if display and stored representations are the
;; same; if they're different, then I'd have to call actual->stored just to
;; check this.)
;;
;;
;;   [Only worry is that then something
;; will come out of the last field badly; perhaps insert an extra field
;; delimiter right before the record delimiter.  Yes, that would make things
;; easy for me.  But that sort of goes against the grain of what I want to do.]
;;
;;
;;   Now, the real whole point of this was to permit a wide variety of
;; formats.  But that's only useful if the formats are easy to read.  Is
;; quoting, which changes the field value, really going to make it easier to
;; read?  One should be able to tell where fields start and end... (actually,
;; I think quoting *does* help with that).
;;   Furthermore, my functions only work if the fields are regular (well,
;; describable by regexps for reading in and by strings for writing out).
;; More complicated formats will need to have a custom Lisp function for
;; reading/writing anyway, so don't sweat too much over them.
;;   The real problem, then, is substituting for the field separator
;; character(s).  If the field separator is multi-char, we only need to make
;; sure that one character in any match is substituted for.
;;   Or I could deal with the field separator character specially and disallow
;; it in the general checking function.
;;   To try to write my own replace-one-char-by-one-char that's more efficient
;; than replace-string is lunacy.  [Or is it?  Search-forward, delete char,
;; insert char ought to be quicker than than serach-forward, replace-match.]
;;
;;   How to devise the characters (or even strings, if I so choose) of my
;; devising?  If they're in the text, I lose.  I guess I have to check for
;; them all the time; but they might be in the actual or display rep and not
;; in the stored rep, or vice-versa.  So only check for string types, mayhap.
;;   Have a variable:  if set, then check for the char.  Do this via
;; count-occurrences.  [Always do this; it won't be so terribly expensive.]
;;   There need only be one special char (ie, not one for field and another
;; for record) if my replace-one-char-by-one-char takes a repetition count and
;; I call it repeatedly; but I don't particularly want that many function
;; calls.  So make it a defsubst or something.
;;
;; The plan for writing:
;;   Have a variable which controls whether to use a generated character (and
;; if so, which one, unless it's t, in which case try to find one, by trying
;; something, and if it doesn't work looking for a character that doesn't
;; appear; probably try again in any case if failure).  [Maybe figure this out
;; by looking at the substitutions; though if quoting is used instead (should
;; I still have quoting at all?), then the substitutions tell me nothing.]  If
;; not using a generated character, then either use the field separator
;; throughout (can substitute in at record boundaries later on) or use both
;; the field and record separators.  Choosing a generated character:
;; shoudln't be in either side of any substitution stuff.  Shouldn't be
;; quotation character.
;;   write fields one after the other using this character only to separate
;; them.
;;   check whether there are too many of the character or just the right
;; number.
;;   do substitution
;;   substitute for the separator character.
;;
;; Do I want to permit regexps instead of strings as separators?  Do I want to
;; permit multi-char strings as separators?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; It may be necessary as a last resort to call quoting/substitution on
;; fields individually if every character appears in the database file
;; representation.  In this case substitution *will* lose information.
;; What a nightmare!

;; When doing substitution, warn if information is about to be lost.  (ie,
;; if target appears in database).

;;; db-file-io.el ends here
