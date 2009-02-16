;;; db-summary.el --- part of EDB, the Emacs database

;; See database.el for copyright notice, distribution conditions, etc.

;; Author: Michael Ernst <mernst@theory.lcs.mit.edu>
;; Keywords: EDB

;;; Commentary:

;; Patterned in part after rmail-new-summary.

;;; Code:


(provide 'db-summary)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Variables
;;;

;;;
;;; Hooks
;;;

(defvar database-summary-mode-hooks nil
  "Function or list of functions run when switching to Database Summary mode.")


;;;
;;; Summary variables
;;;

;; All of these are strictly auxiliary variables; no original information
;; is kept here.  Don't change this since the summary buffer may be
;; destroyed at any time.

;; I could keep more information here (eg dbs-format-lines) for
;; convenience; that would mean I'd have to do more work when the summary
;; format changed and when the summary buffer was created.  Maybe later.

(deflocalvar dbs-data-display-buffer nil
  "The buffer of the format for which this buffer is a summary.")

(deflocalvar dbs-index nil
  "The index of the record summary at point.
Used in determining whether the data display buffer and its summary are in synch.
Don't set this variable directly; use `dbs-set-index' instead.")

;;   "Set  dbs-index  to INDEX and  dbs-index-fraction  appropriately."
(defsubst dbs-set-index (index)
  (setq dbs-index index
	dbs-index-fraction
	(format "%d/%d" dbs-index dbs-no-of-records)))

(deflocalvar dbs-no-of-records nil
  "The number of records in the database when this summary was made.")

(deflocalvar dbs-point nil
  "The beginning of the current record.")

(deflocalvar dbs-index-fraction nil
  "Like `dbc-index-fraction', for the benefit of the mode line.")

(deflocalvar dbs-recompute-p nil
  "T if some summary information is out of date, nil otherwise.
This is usually set to t when some link-summary is set to nil.")



;;;
;;; Format variables related to the summary
;;;

;; One might like to have several summaries of a database, so perhaps these
;; variables should be local to the summary rather than to the format.  How
;; often would one want multiple summaries, anyway?
;; Pro:
;;  * Less dbs-in-format-buffer to look up variable values.
;; Con:
;;  * If I update summaries I'd have to keep track of, and update, them all.
;;  * Summaries might be less sensitive to changes the format wants to make.
;;  * The format must maintain these anywa, in case summary buffer destroyed.


(deflocalvar dbf-summary-format nil
  "A string in the same format as the format-file.
Use `dbf-set-summary-format' to set it.")

;; Needn't be buffer-local.
(defvar dbf-default-summary-format nil)

(deflocalvar dbf-summary-function nil
  "Function which inserts summary information for a single record in
the summary buffer; it takes the record as its argument.")

(deflocalvar dbf-summary-buffer nil
  "The summary buffer associated with this format.")

(deflocalvar dbf-summary-show-hidden-records-p t
  "Nil if hidden records should be hidden from the summary, t otherwise.")

(deflocalvar dbf-summary-recompute-all-p nil
  "T if every record summary in this buffer should be recomputed.")


;;;
;;; Variables in both the format and summary buffers
;;;

;; These variables are too important to be kept only in the summary buffer,
;; which may disappear at any time, but are often used by it and so should
;; be handy.

;; This doesn't depend on the field values in the individual records
;; because format->lines-and-stringform-list errs if min-height is not
;; equal to max-height (unelss variable-height is set).  That makes
;; determining which summary point is in, and getting to a particular
;; summary, much easier.
(deflocalvar dbfs-lines nil
  "The (constant) number of screen lines occupied by each record summary.
This variable is computed automatically from the summary format.
It has a value in both the summary and data display buffers.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Musings
;;;

;; How often should the summary buffer be updated?
;; * always, when it exists (This could be advantageous for large databases:
;;   incremental computation instead of all at once.  This proposal also has
;;   grassroots support, so I'll implement it.  Could have a variable to
;;   defer updates and get the other behavior.)
;; * on demand (Thus, experience no slowdown when marking, etc.)
;; * have some operations defer updating the summary until they
;;   are completed (eg, long operations that make a lot of changes or do a
;;   lot of marking and which we don't want to slow down).
;; * when visible in a window (no, too confusing, could surprise the user.
;;   But a lot of emacs stuff does work this way)
;; Just changing marks could be made efficient.

;; Maybe don't bother to do as much work if the data display buffer isn't
;; visible.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Macros for working in the correct buffer
;;;

;; Assumes the current buffer is the data display buffer.
(defsubst dbf-summary-buffer ()
  (and (bufferp dbf-summary-buffer)
       (buffer-name dbf-summary-buffer)
       dbf-summary-buffer))

;; Assumes the current buffer is the data display buffer.
;;   "Execute the body in the summary buffer, if it exists."
(defmacro dbf-in-summary-buffer (&rest body)
  (` (if (dbf-summary-buffer)
	 (in-buffer dbf-summary-buffer
	   (progn
	     (,@ body))))))
(put 'dbf-in-summary-buffer 'lisp-indent-hook 0)
(put 'dbf-in-summary-buffer 'edebug-form-spec '(&rest form))

;; Assumes the current buffer is the summary buffer.
;;   "Execute the body in the data display buffer (which always exists)."
(defmacro dbs-in-data-display-buffer (&rest body)
  (` (in-buffer dbs-data-display-buffer
       (,@ body))))
(put 'dbs-in-data-display-buffer 'lisp-indent-hook 0)
(put 'dbs-in-data-display-buffer 'edebug-form-spec '(&rest form))

;;; These assume the current buffer is a database buffer.

(defsubst db-data-display-buffer ()
  "Return the database data display buffer associated with the current buffer,
which must be either a summary buffer or a data display buffer."
  (cond ((db-summary-buffer-p)
	 dbs-data-display-buffer)
	((db-data-display-buffer-p)
	 (current-buffer))
	(t
	 (error "Neither in format nor summary buffer"))))

;;   "Return the database summary buffer associated with the current buffer,
;; which must be either a summary buffer (which is returned) or a data display
;; buffer.  Return nil if there is no associated summary buffer."
(defsubst db-summary-buffer ()
  (cond ((db-summary-buffer-p)
	 (current-buffer))
	((db-data-display-buffer-p)
	 (dbf-summary-buffer))
	(t
	 (error "Neither in format nor summary buffer"))))



(defmacro db-in-data-display-buffer (&rest body)
  (` (in-buffer (db-data-display-buffer)
       (,@ body))))
(put 'db-in-data-display-buffer 'lisp-indent-hook 0)
(put 'db-in-data-display-buffer 'edebug-form-spec '(&rest form))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Creating the summary
;;;

;; Perhaps add a variable which permits, if no changes have been made, even
;; dbf-fill-summary-buffer to be bypassed.

(defun db-summary ()
  "Display a summary (or directory) of all database records according to
the variable `dbf-summary-function', which is set by `dbf-set-summary-format'.
The summary appears in a separate buffer.
When called from the summary buffer, this updates the summary."
  (interactive)

  (db-in-data-display-buffer

    ;; I need to decide whether the buffer should be erased and refilled or
    ;; just displayed without updating at this time.  Probably the former,
    ;; but the latter would be much nicer and is doable.

    (cond ((database-empty-p dbc-database)
	   ;; Maybe give the message, but pop anyway, if the database is empty.
	   (delete-windows-on (dbf-summary-buffer))
	   (db-message "Database is empty."))
	  (t
	   (let ((data-display-buffer (current-buffer)))
	     (if (not dbf-summary-function)
		 (dbf-set-summary-format dbf-summary-format))
	     (if (not (dbf-summary-buffer))
		 (setq dbf-summary-buffer
		       (db-create-summary-buffer data-display-buffer)))
	     ;; Avoid a check by not using dbf-in-summary-buffer
	     (if (in-buffer dbf-summary-buffer (dbs-out-of-date-p))
		 (dbf-fill-summary-buffer))
	     (pop-to-buffer dbf-summary-buffer)
	     (setq dbs-data-display-buffer data-display-buffer)
	     ;; go to proper line
	     (dbs-move-to-proper-record))))))

;; This shouldn't be called if a summary buffer already exists.
(defun db-create-summary-buffer (data-display-buffer)
  (let ((sbuf (generate-new-buffer (concat (buffer-name data-display-buffer)
					   "-summary"))))
    (in-buffer sbuf
      (setq dbs-data-display-buffer data-display-buffer)
      (setq dbc-database (in-buffer data-display-buffer dbc-database))
      (database-summary-mode))

    ;; (make-local-variable 'minor-mode-alist)
    ;; (setq minor-mode-alist (list ": " description))

    ;; return the buffer
    sbuf))


(defvar mode-motion-hook)		; quiet the byte-compiler
;; This is spelled out instead of being db-summary-mode because it's a
;; major mode, while db-edit-mode and db-view-mode are minor modes.  This
;; is a weak rationale.
(defun database-summary-mode ()
  "Summary buffer for database mode.
Most keystrokes perform the same function they do in the data display buffer.

Key bindings:

\\{database-summary-mode-map}"

  ;; Actually mode-line should be hacked the way the others are.
  (setq major-mode 'database-summary-mode)
  (setq mode-name "Database Summary")

  (set-buffer-modified-p nil)
  (setq buffer-read-only t)

  (auto-save-mode 0)
  (setq buffer-file-name nil)

  (use-local-map database-summary-mode-map)

  (setq mode-line-format
	(dbs-in-data-display-buffer
	 (list
	  (format "-----Database: %17s   %%[(Summary" (buffer-name))
	  'minor-mode-alist
	  " "
	  'dbs-index-fraction
	  ")%]---"
	  '(-3 . "%p")
	  "-%-")))

  (if db-running-lucid-emacs
      (progn
	(require 'mode-motion)
	(setq mode-motion-hook 'mode-motion-highlight-line)
	;; (db-lucid-summary-mode-menubar)
	))

  (run-hooks 'database-summary-mode-hooks)

  ;; Force an update.
  (setq dbs-no-of-records -1))

;;   "T if this buffer is a database summary buffer."
(defsubst db-summary-buffer-p ()
  (eq major-mode 'database-summary-mode))


(defun db-summary-subset ()
  "Make EDB summary-listing based on hits of STRING search, of FIELD."
  (interactive)
  (db-mark-searched-records)
  (db-hide-unmarked-records)
  (db-toggle-show-hidden-records 0)
  (db-summary)
  (db-hiding-toggle 0)
  (db-hiding-toggle 1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Filling the summary
;;;

;; This function is currently only called two places, so inline it...
(defsubst dbs-insert-link-summary (link mark-hidden-records-p)
  ;; Instead of having start and end, I could just
  ;; move by lines; which is more efficient?
  (let ((start (point))
	end)
    (insert (link-summary link))
    ;; (indent-rigidly start (point) 2)
    (setq end (point))
    (if (link-markedp link)
	(progn
	  (goto-char start)
	  (delete-char 1)
	  (insert "+")
	  (goto-char end)))
    (if (and mark-hidden-records-p (link-hiddenp link))
	(progn
	  (goto-char start)
	  (forward-char 2)
	  (while (< (point) end)
	    (backward-delete-char 1)
	    (insert "[")
	    (next-line 1))
	  (goto-char end)))))

;; Will I ever call this without pop-to-buffering immediately thereafter?
;; [Does that matter all that much?]

;; The database won't be empty when this is called.

(defun dbf-fill-summary-buffer ()

  ;; (db-debug-message "dbf-f-s-b:  summary-buffer = %s." summary-buffer)

  (let ((summary-function dbf-summary-function)
	(lines dbfs-lines)
	(hide (and (not dbf-summary-show-hidden-records-p)
		   dbc-hide-p))
	(mark-hidden-records-p dbc-hide-p)
	(recompute-all-p dbf-summary-recompute-all-p))
    (db-debug-message "dbf-fill-summary-buffer:  %s %s"
		      hide mark-hidden-records-p)
    (dbf-in-summary-buffer
      (let ((buffer-read-only nil))
	(erase-buffer)
	(db-message "Computing summary...")
	;; Should recompile this loop whenever summary-function changes.
	;; The funcall overhead makes it really slow.
	(maplinks-macro
	  (progn
	    (if (or recompute-all-p (not (link-summary maplinks-link)))
		(progn
		  (db-debug-message "Computing link summary for link %d." maplinks-index)
		  (link-set-summary maplinks-link
			(funcall summary-function (link-record maplinks-link)))))
	    ;; A call to inline here was giving trouble to old-bytecomp users.
	    (dbs-insert-link-summary maplinks-link mark-hidden-records-p))
	  dbc-database hide "Computing summary...%d")
	(db-message "Computing summary...done.")

	;; get rid of last newline.
	(backward-delete-char 1)
	(set-buffer-modified-p nil)
	(setq dbs-no-of-records (database-no-of-records dbc-database))
	;; What is this doing?  lines is bound to dbfs-lines above, and
	;; hasn't been changed (except perhaps dynamically??) since.
	(setq dbfs-lines lines)

	(setq dbs-recompute-p nil)

	;; *** Maybe we should call dbs-move-to-proper-record here.
	(setq dbs-index 0)

        ;;; This is wrong because the first displayed record might not be
        ;;; the first record due to hiding.
	;; (goto-char (point-min))
	;; (dbs-set-index 1)
	))
    (setq dbf-summary-recompute-all-p nil)))

(defun dbf-fill-summary-buffer-and-move-to-proper-record ()
  (if (dbf-summary-buffer)
      (progn
	(dbf-fill-summary-buffer)
	(dbf-in-summary-buffer
	  (dbs-move-to-proper-record)))))

;; Efficient way to update just the marked and hidden summary markings.
(defun dbf-update-summary-marks ()
  (let ((mark-hidden-records-p dbc-hide-p)
	(hidden-records-shown-p (or dbf-summary-show-hidden-records-p
				     (not dbc-hide-p))))
    (dbf-in-summary-buffer
      (let ((buffer-read-only nil)
	    (opoint (point))
	    line)
	(unwind-protect
	    (progn
	      (goto-char (point-min))
	      (maplinks-macro
		(progn
		  (delete-char 1)
		  (insert (if (link-markedp maplinks-link) "+" " "))
		  (backward-char 1)
		  (setq line 0)
		  ;; Each summary item spans exactly dbfs-lines screen lines.
		  (while (< line dbfs-lines)
		    (forward-char 1)
		    (delete-char 1)
		    (insert (if (and mark-hidden-records-p
				     (link-hiddenp maplinks-link))
				"["
			      " "))
		    (forward-line 1)
		    (setq line (1+ line))))
		dbc-database
		(not hidden-records-shown-p)))
	  (goto-char opoint))))))

;; Efficient way to update just changes to one record in the summary.
(defun dbf-update-summary-item (index &optional link)
  (setq link (or link (database-link dbc-database index)))
  (let* ((mark-hidden-records-p dbc-hide-p)
	 (this-record-shown-p (or (not (link-hiddenp link))
				  dbf-summary-show-hidden-records-p
				  (not dbc-hide-p)))
	 (summary-function dbf-summary-function))
    (if this-record-shown-p
	(dbf-in-summary-buffer
	  (let ((buffer-read-only nil)
		(oindex dbs-index)
		line)
	    (unwind-protect
		(progn
		  (or (link-summary link)
		      (link-set-summary
		       link
		       (funcall summary-function (link-record link))))
		  (dbs-move-to-proper-record index)
		  ;; assuming at beginning of line
		  (delete-region (point)
				 (progn
				   (forward-line dbfs-lines)
				   (point)))
		  (dbs-insert-link-summary link mark-hidden-records-p))
	      ;; save old line and column instead.
	      (dbs-move-to-proper-record oindex))))
      ;; *** need to update cached summary, even though we're not going to
      ;; *** display.
      )))

;; If we always show marked records, regardless of hiding, then
;; clearly we should have a similar policy in moving forward in the
;; database proper or everything will get all fouled up.  Maybe have a
;; link-ignored for use when moving forward and here as well; it would be
;; true only if unmarked and hidden and the appropriate variables
;; about how hidden records were treated were set.

;; I think we really want the summary to end with a newline so that all
;; this works.

;; This isn't used any more, and I'm afraid to abstract any more out of
;; dbf-fill-summary-buffer for fear of degraded performance.
(defun dbf-summarize-link (link)
  (if (or dbf-summary-recompute-all-p (not (link-summary link)))
      (progn
	(db-debug-message "Computing link summary.")
	(link-set-summary link
	      (funcall dbf-summary-function (link-record link)))))
  (if (or dbf-summary-show-hidden-records-p
	    (not (link-hiddenp link))
	    ;; always show marked records, regardless of hiding
	    (link-markedp link))
      (dbf-in-summary-buffer
	(let ((start (point))
	      end)
	  (insert (link-summary link))
	  ;; instead of having start and end, I could just
	  ;; move by lines; which is more efficient?
	  (indent-rigidly start (point) 2)
	  (setq end (point))
	  (if (link-markedp link)
	      (progn
		(goto-char start)
		(delete-char 1)
		(insert "*")
		(goto-char end)))
	  (if (link-hiddenp link)
	      (progn
		(goto-char start)
		(forward-char 2)
		(while (< (point) end)
		  (backward-delete-char 1)
		  (insert "[")
		  (next-line 1))
		(goto-char end)))))))






;;; May not need this, depending on how I work the summary buffer.
;;; Perhaps have how often it's updated be an option.
;;; Or maybe it will be fast enough that it won't matter.
;; For newly created records.
; (defun db-insert-summary-info (record)
;   "Insert summary info for the record in the current buffer."
;
;   ;; ...
;
;   )



;; Plan:  make a list of separator strings and displayspecs from the
;; summary-info.  Then create a function that returns a list of the
;; strings and formatted strings.  Complain if, for instance, max-height <>
;; min-height, etc.

;; Don't bother to remember what info is shown in the summary listing;
;; just update the entry when the database record changes.

;; How to compute the number of lines in the summary-format?

;; Sets dbf-summary-function and dbfs-lines.
(defun dbf-make-summary-maker (summary-format database)

  ;; (db-debug-message "db-make-summary-maker: summary-format = %s" summary-format)

  (let ((lasfl (format->lines-and-stringform-list summary-format database 2 t nil)))
    ;; (db-debug-message "db-make-summary-maker:  lasfl = %s" lasfl)

    (setq dbfs-lines (car lasfl))

    ;; I don't see how to fit a call to function around this lambda form.
    (setq dbf-summary-function
	  (` (lambda (formatted-record)
	       (concat (,@ (cdr lasfl))))))))


;; Takes a format and returns a cons of two values:  a number and a list.
;; The list is list of forms which, when evaluated with variable
;; formatted-record bound, evaluate to strings; these can be used as
;; argumentes to concat, insert, etc.  The number is the number of lines
;; occupied by the items when inserted.

;; Signals an error if any displayspec has nonequal min-height and
;; max-height, unless variable-height is non-nil, in which case the number
;; returned is a minimum.

;; I can't decide whether to automatically add a newline at the end; or
;; maybe just check whether there's one there.  Maybe people who care about
;; that sort of thing (like summaries) should make sure for themselves.

(defun format->lines-and-stringform-list (format database indent add-newline variable-height)

  (let (results
	beginning
	end
	this-displayspec
	(backslash-placeholder (and (string-match "\\\\\\\\" format)
				    (db-unused-char-in-string format)))
	(lines 0))
    ;; (db-debug-message "f->lasfl: format = %s" format)

    ;; It would be more efficient to do this to each literal as it's
    ;; extracted.
    (if (and indent (> indent 0))
	(setq format (concat (make-string indent ? )
			     (db-string-substitute-substring-general-case
			      (concat "\n" (make-string indent ? ))
			      "\n"
			      format))))
    (if backslash-placeholder
	(setq format (db-string-substitute-substring-general-case
		      (char-to-string backslash-placeholder)
		      (regexp-quote "\\\\")
		      format)))

    (while (string-match displayspec-regexp format)
      ;; (db-debug-message "f->lasfl: match = %s" (db-match-string 0 format))
      (setq beginning (match-beginning displayspec-regexp-content-beginning)
	    end (or (match-end displayspec-regexp-content-end)
		    (match-end displayspec-regexp-content-end-alt))
	    this-displayspec (make-displayspec-from-string-internal
			      format database))
      ;; This shouldn't be necessary
      (if (not (displayspec-min-height this-displayspec))
	  (displayspec-set-min-height this-displayspec 1))
      (if (not (or variable-height (displayspec-max-height this-displayspec)))
	  (displayspec-set-max-height this-displayspec
		(displayspec-min-height this-displayspec)))
      (if (or variable-height
	      (= (displayspec-min-height this-displayspec)
		 (displayspec-max-height this-displayspec)))
	  (setq lines (+ lines (1- (displayspec-min-height this-displayspec))))
	(error "Min-height %s must equal max-height %s in summary displayspec %s."
	       (displayspec-min-height this-displayspec)
	       (displayspec-max-height this-displayspec)
	       this-displayspec))
      (if (not (zerop beginning))
	  (let ((literal (substring format 0 beginning)))
	    (if backslash-placeholder
		(db-string-substitute ?\\ backslash-placeholder literal))
	    (setq results (cons literal results)
		  lines (+ lines (count-array ?\n literal)))))
      (setq results (cons (make-format-printer this-displayspec) results))
      (setq format (substring format end)))
    (if add-newline (setq format (concat format "\n")))
    (if (not (equal "" format))
	(progn
	  (if backslash-placeholder
	      (db-string-substitute ?\\ backslash-placeholder format))
	  (setq results (cons format results))))
    (setq lines (+ lines (count-array ?\n format)))
    (cons lines (nreverse results))))


;; Is this worth optimizing?  I'd like to, since its result will be called a lot.
;; Differences from displayspec->printed-rep:
;;  * handling of multiple lines (just take the first line)
;;  * can look at the displayspecs only once, at summary-printer creation time;
;;    don't need them around all the time taking up space.
;;  * if min-width and max-width not set, ignore that processing
;; Perhaps abstract lots of stuff away from displayspec->printed-rep so the
;; parts can be reused here.  Or wait until it is very stable and then do
;; the specialization by hand.

;; Note that we assume that dbf-summary-record is bound; this permits fewer
;; function calls.
;;   "Return a lisp form which evaluates to a printed representation for DISPLAYSPEC."
(defun make-format-printer (displayspec)
  (` (displayspec->printed-rep (, displayspec) formatted-record)))

(defun make-summary-initial-indentation ()
  (` (if dbf-sr-markedp "* "
       (, (make-summary-indentation)))))

(defun make-summary-indentation ()
  '(if (and dbf-sr-hiddenp (not dbf-sr-markedp))
       "  [ "
     "  "))

;; ;; Problem:  this will fail (ie, won't insert the right things in the left
;; ;; margin) when a data item (as opposed to a literal) causes a newline.
;; ;; Maybe I need to just do an indent then go to the front and insert the
;; ;; proper items etc for the whole thing after all.  This would also
;; ;; localize the special code a bit.
;;
;; (defun push-literal-printer-results (literal &optional etc)
;;   (while (string-match "\n" literal)
;;     (push (substring literal 0 (1+ (match-beginning 0))) results)
;;     (push (make-summary-indentation) results)
;;     (setq lines (1+ lines))
;;     (setq literal (substring literal (1+ (match-beginning 0)))))
;;   (if etc (setq literal (concat literal etc)))
;;   (if (not (equal "" literal))
;;       (push literal results)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Synching the format and summary buffers
;;;

(defsubst dbs-in-synch-p ()
  (= dbs-index (dbs-in-data-display-buffer dbc-index)))

(defsubst dbs-out-of-date-p ()
  (or dbs-recompute-p
      (not (= dbs-no-of-records (database-no-of-records dbc-database)))))

;; Should perhaps update the summary as well, particularly if it's visible.
;; If I do that, then perhaps this should no longer be inlined.
(defsubst dbf-set-summary-out-of-date-p ()
  (dbf-in-summary-buffer
    (setq dbs-recompute-p t)))

;; When I'm in the summary, trust its variables unless it's out of date.

;; Called by summary movement commands, maybe.
;;   "Ensure that the data display and summary buffers have the same current record."
(defun dbs-synch-format-with-summary ()
  (if (dbs-out-of-date-p)
      (dbs-synch-summary-with-format)
    (if (not (dbs-in-synch-p))
	(dbs-in-data-display-buffer
	  (db-select-record (in-buffer dbf-summary-buffer dbs-index))))))

;; Might want a dbf- version of this too.

(defun dbs-synch-summary-with-format ()
  (if (dbs-out-of-date-p)
      (dbs-in-data-display-buffer
	(dbf-fill-summary-buffer)))
  ;; If we just did the above, it will clearly be out of synch.
  ;; But it might be even if it wasn't out of date.
  (if (not (dbs-in-synch-p))
      ;; Maybe have a better function here.  I don't know whether I want
      ;; to try to make use of the dbs-index info; perhaps I do.  I could
      ;; be way far away, in which case I might as well just go from the
      ;; front; but maybe I'm not.  I don't think that it will be messed
      ;; up, though.
      (dbs-move-to-proper-record)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Moving about
;;;

;; If hidden records aren't shown in the summary (and that should be
;; an option), then this is wrong.  And in that case it's better to do
;; relative than absolute motion.  (Eg if I move down three records, then
;; move down three summaries rather than going to the nth summary.)

;; This moves point and sets dbs-point
;; n is 1-based.
(defsubst dbs-goto-nth-summary (n)
  (goto-line (1+ (* dbfs-lines (1- n))))
  (setq dbs-point (point)))

;; I can't tell if this code is any good.

;; Makes no assumptions about dbs-index.
;; If hidden records aren't shown, this is quite quick.
;; Should perhaps be split into a function that *computes* the mapping from
;; database indices to summary indices, and a function that does the rest.
;;   "Move point to the summary of the record shown in the format or to INDEX."
(defun dbs-move-to-proper-record (&optional index)
  ;; goto-line also moves to the beginning of the line
  (if (dbs-in-data-display-buffer (or (not dbc-hide-p)
				dbf-summary-show-hidden-records-p))
      (let ((index (or index (dbs-in-data-display-buffer dbc-index))))
	(dbs-goto-nth-summary index)
	(dbs-set-index index))
    (let ((previous-displayed-records 0)
	  (last-displayed-record nil)
	  (proper-index (or index (dbs-in-data-display-buffer dbc-index))))
      (maplinks-macro
	(if (<= maplinks-index proper-index)
	    (setq previous-displayed-records
		  (1+ previous-displayed-records)
		  last-displayed-record maplinks-index)
	  ;; If we're past it but still haven't found a nonhidden link.
	  (if (not last-displayed-record)
	      (setq previous-displayed-records 1
		    last-displayed-record maplinks-index)))
	dbc-database
	t)
      ;; If there are no displayed records at all, this will fail.
      ;; But if the database is empty we refuse to make the summary anyway.
      (if (not (= last-displayed-record proper-index))
	  (db-message "Record %s does not appear in the summary buffer."
		   proper-index))
      (dbs-goto-nth-summary previous-displayed-records)
      (dbs-set-index last-displayed-record))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Movement commands
;;;

;; Do I want this to take  &optional ingore-hiding markedp ?

;; Move point forward ARG records in the summary buffer, and set dbs-point.
(defsubst dbs-forward-record (arg)
    (goto-char dbs-point)
    (db-forward-line-wrapping (* dbfs-lines arg))
    (setq dbs-point (point)))

;; Maybe this is the wrong way to implement it and if contrained records
;; aren't shown in the summary buffer I should move forward in the format
;; buffer anyway and give a message in the summary buffer that we're out of
;; synch.

(defun dbs-next-record-ignore-hiding (arg)
  "Go to the ARGth next record, ignoring hiding.
That is, all records, even those which are hidden, are counted."
  (interactive "p")
  (if (not (dbs-in-data-display-buffer dbf-summary-show-hidden-records-p))
      (db-next-record-ignore-hiding arg)
    (progn
      (dbs-synch-format-with-summary)
      (dbs-in-data-display-buffer
	(db-next-record-ignore-hiding arg))
      (dbs-forward-record arg)
      (dbs-set-index (dbs-in-data-display-buffer dbc-index)))))

(defun dbs-previous-record-ignore-hiding (arg)
  "Go to the ARGth previous record, ignoring hiding.
That is, all records, even those which are hidden, are counted."
  (interactive "p")
  (dbs-next-record-ignore-hiding (- arg)))

;; Quite possibly, if I want this to work I'll need to remember where point
;; was before the move; keep around yet another dbs- variable.

;; This permits the body to move to an arbitrary location; it could be used
;; with scroll-*, x-flush-mouse-queue, etc.

(defun dbs-scroll-up ()
  (interactive)
  (scroll-up)
  (db-jump-to-point))

(defun dbs-scroll-down ()
  (interactive)
  (scroll-down)
  (db-jump-to-point))

;; Perhaps someday get rid of this:  merge it directly into
;; db-jump-to-point.  For now, it's called by a lot of functions.  (It
;; probably wouldn't hurt them that much to call db-jump-to-point and pay a
;; smidgen more overhead.)

;; ;; This is wrong in the presence of hidden directory lines.
;; (defun dbs-jump-to-point ()
;;   (interactive)
;;   (beginning-of-line)
;;   (let ((difference (/ (count-lines-signed dbs-point (point)) dbs-lines)))
;;     (goto-char dbs-point)
;;     (dbs-next-record-ignore-hiding difference)))

;; This is the cheating way to do this; fix it later.
;; (defun dbs-first-record ()
;;   (interactive)
;;   (goto-char (point-min))
;;   (dbs-jump-to-point))
;;
;; (defun dbs-last-record ()
;;   (interactive)
;;   (goto-char (point-max))
;;   (dbs-jump-to-point))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Summary Mode commands
;;;

;; These keystrokes should be made to come into line with those of view-mode.

(defvar database-summary-mode-map (make-keymap)
  "Keymap for database summary buffer.")
(suppress-keymap database-summary-mode-map)

;; This map and that of view-mode should be very similar.
;; Mayhap I should move all this map stuff to its own file.  I dunno.

;; Moving around in the database
(define-key database-summary-mode-map "n" 'db-next-record)
(define-key database-summary-mode-map "p" 'db-previous-record)
(define-key database-summary-mode-map "\C-n" 'db-next-record)
(define-key database-summary-mode-map "\C-p" 'db-previous-record)
(define-key database-summary-mode-map "<" 'db-first-record)
(define-key database-summary-mode-map ">" 'db-last-record)
(define-key database-summary-mode-map (db-meta-prefix-ify "<") 'db-first-record)
(define-key database-summary-mode-map (db-meta-prefix-ify ">") 'db-last-record)
(define-key database-summary-mode-map "j" 'db-jump-to-record)
(define-key database-summary-mode-map " " 'db-next-screen-or-record)
(define-key database-summary-mode-map "\177" 'db-previous-screen-or-record)
(define-key database-summary-mode-map (db-meta-prefix-ify "n") 'dbs-next-record-ignore-hiding)
(define-key database-summary-mode-map (db-meta-prefix-ify "p") 'dbs-previous-record-ignore-hiding)
(define-key database-summary-mode-map (db-meta-prefix-ify "\C-n") 'db-next-marked-record)
(define-key database-summary-mode-map (db-meta-prefix-ify "\C-p") 'db-previous-marked-record)


;; Exiting summary mode
(define-key database-summary-mode-map "e" 'dbs-edit)
(define-key database-summary-mode-map "v" 'dbs-view)
(define-key database-summary-mode-map "q" 'dbs-exit)
(define-key database-summary-mode-map "x" 'dbs-exit)

;; Adding and removing records
(define-key database-summary-mode-map "a" 'db-add-record)
(define-key database-summary-mode-map "i" 'db-add-record)
(define-key database-summary-mode-map "d" 'dbs-delete-record)
(define-key database-summary-mode-map "k" 'dbs-delete-record)
(define-key database-summary-mode-map "o" 'dbs-output-record-to-db)
(define-key database-summary-mode-map "c" 'db-copy-record)

;; Searching commands
(define-key database-summary-mode-map "s" 'db-search)
;; (define-key database-summary-mode-map "S" 'db-incremental-search)
(define-key database-summary-mode-map "\C-s" 'db-isearch-forward)
(define-key database-summary-mode-map "\C-r" 'db-isearch-backward)


;; Everything else
(define-key database-summary-mode-map "?" 'describe-mode)
(define-key database-summary-mode-map "O" 'db-hide-record)
(define-key database-summary-mode-map (db-meta-prefix-ify "o") 'db-hiding-toggle)
(define-key database-summary-mode-map (db-meta-prefix-ify "O") 'db-hiding-set)
(define-key database-summary-mode-map (db-meta-prefix-ify "\C-o") 'db-toggle-show-hidden-records)
(define-key database-summary-mode-map "g" 'db-summary)
(define-key database-summary-mode-map "h" 'db-summary)
(define-key database-summary-mode-map "D" 'db-summary)
(define-key database-summary-mode-map "m" 'db-mark-record)
(define-key database-summary-mode-map "r" 'db-report)
(define-key database-summary-mode-map "\C-xr" 'db-revert-database)
(define-key database-summary-mode-map "\C-v" 'dbs-scroll-up)
(define-key database-summary-mode-map (db-meta-prefix-ify "v") 'dbs-scroll-down)

(define-key database-summary-mode-map "\C-x\C-q" 'db-toggle-modifiable-p)

(define-key database-summary-mode-map "\C-x\C-@" 'db-x-jump-to-point)
(define-key database-summary-mode-map "\C-c\C-c" 'dbs-exit)

(define-key database-summary-mode-map "b" 'undefined)
(define-key database-summary-mode-map "f" 'undefined)
(define-key database-summary-mode-map "l" 'undefined)
(define-key database-summary-mode-map "t" 'undefined)
; (define-key database-summary-mode-map "u" 'db-revert-record)
(define-key database-summary-mode-map "w" 'undefined)
(define-key database-summary-mode-map "y" 'undefined)
(define-key database-summary-mode-map "z" 'undefined)


(defun dbs-view ()
  "Manipulate this record in the data display buffer in View mode."
  (interactive)
  (pop-to-buffer dbs-data-display-buffer)
  (db-view-mode))

(defun dbs-edit ()
  "Manipulate this record in the data display buffer in Edit mode."
  (interactive)
  (pop-to-buffer dbs-data-display-buffer)
  (if (eq dbf-minor-mode 'view)
      (db-first-field)))

;; A misstroke in the data display buffer shouldn't exit the database.
(defun dbs-exit ()
  "Exit the summary buffer."
  (interactive)
  ;; This is oh-so-very-crude.
  (let ((data-display-buffer dbs-data-display-buffer))
    (delete-windows-on (current-buffer))
    (switch-to-buffer data-display-buffer)))

(defun dbs-delete-record (&optional force)
  "Delete the current record from the database.
With a prefix argument, doesn't verify."
  (interactive "P")
  (if (or force (y-or-n-p "Delete this record? "))
      (progn
	(dbs-in-data-display-buffer
	  (db-delete-record t))
	;; hope we're at the beginning of the record
	(let ((buffer-read-only nil))
	  (kill-line dbfs-lines)
	  (if (eobp)
	      (goto-char (point-min))))
	(setq dbs-no-of-records (1- dbs-no-of-records))
	(db-message "Record deleted.")
	(dbs-set-index (dbs-in-data-display-buffer dbc-index))
	)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Menus
;;;

(defvar database-summary-mode-menu
  '("Database"
    "SUMMARY Mode:"
    ["Update summary"     db-summary t]
    ["Report"             db-report  t]
    "----"
    ("Motion"
     ["jump  to record"  db-jump-to-record t]
     ["last     record"  db-last-record    t]
     ["first    record"  db-first-record   t]
     "---"
     ["next"               db-next-record                 t]
     ["next (screen)"      db-next-screen-or-record       t]
     ["next (marked)"      db-next-marked-record          t]
     ["next (ingore hiding)" db-next-record-ignore-hiding t]
     "---"
     ["prev"               db-previous-record                 t]
     ["prev (screen) "     db-previous-screen-or-record       t]
     ["prev (marked)"      db-previous-marked-record          t]
     ["prev (ingore hiding)" db-previous-record-ignore-hiding t]
     "---"
     ["Isearch Backward" db-isearch-backward t]
     ["Isearch Forward" db-isearch-forward t]
     )
    "----"
    ["View record" dbs-view t]
    ["Edit record" dbs-edit t]
    ["Delete Record" dbs-delete-record t]
    ["Add Record" db-add-record t]
    ["Mark Record" db-mark-record t]
    ["Hide Record" db-hide-record t]
    "----"
    ("Hiding"
     ["Hiding    on/off" db-hiding-toggle             t]
     ["Hiding hide/show (in summary)" db-toggle-show-hidden-records t]
     ["Un-hide      all" db-unhide-all                  t]
     ["Un-mark      all" db-unmark-all                  t]
     ["Mark   un-hidden" db-mark-unhidden-records      t]
     ["Hide   un-marked" db-hide-unmarked-records       t]
     )
    "----"
    ["Sort     database" db-sort                t]
    ["Revert   database" db-revert-database t]
    ["Save     database" db-save-database       t]
    ["Write    database" db-write-database-file t]
    "----"
    ["Quit" dbs-exit t]
     )
  "Menu for Database Summary mode.")

;; 'ignored for SYMBOL argument was giving me trouble.
;; Does this work in Lucid Emacs?
(easy-menu-define ignored
		  database-summary-mode-map
		  "ignored-doc-string"
		  database-summary-mode-menu)

;; (defun database-summary-mode-menu (e)
;;   (interactive "@e")
;;   (setq zmacs-region-stays 't)
;;   (popup-menu database-summary-mode-menu))

;; (if db-running-lucid-emacs
;;     (defun db-lucid-summary-mode-menubar ()
;;       (if (and current-menubar
;; 	       (not (assoc "DB:Summary" current-menubar)))
;; 	  (progn
;; 	    (set-buffer-menubar (copy-sequence current-menubar))
;; 	    (add-menu nil "DB:Summary"
;; 		      (cdr database-summary-mode-menu))))))

(if db-running-lucid-emacs
    (progn
      (define-key database-summary-mode-map [mouse1] 'db-lucid-mouse-jump-to-point)
      (define-key database-summary-mode-map [mouse2] 'dbs-lucid-mouse-view)
      (define-key database-summary-mode-map [mouse3] 'database-summary-mode-menu)))


;;; db-summary.el ends here
