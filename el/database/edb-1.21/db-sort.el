;;; db-sort.el --- part of EDB, the Emacs database

;; See database.el for copyright notice, distribution conditions, etc.

;; Author: Michael Ernst <mernst@theory.lcs.mit.edu>
;; Keywords: EDB

;;; Commentary:

;;; Code:


(provide 'db-sort)


;;; Variables used dynamically; avoid compiler messages about free variables.
;; Should this be?  Perhaps get rid of it.
(defvar ordering-fields-converted)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; To do
;;;

;; Ordering-fields and field-priorities refer to the same thing.
;; I should think of a third, better name and stick with it.

;; Now, what should the format of ordering-fields be?  Perhaps best is to
;; demand that each pair be integer + other info, but give user a function
;; to produce that from fieldname + other info.  I think I don't want to
;; always demand that every field have a pair (though that will be done by
;; sort-interface) because it could get too long and because I won't be
;; able to count on that anyway.

;; Solution:  require that it always be fieldnumber + other-info (where
;; other-info isn't a function but can be increasing or decreasing or
;; whatever) and provide a function for making arbitrary lists into this
;; format.


;; I work with order functions whenever possible; they're more convenient,
;; particularly because no extra equality test is needed, as it is when
;; only lessp-function is provided.  I do permit sort functions when
;; they're more convenient (or when I only need that much functionality and
;; there's no sense in taking an extra function call just to convert (-1 0
;; 1) to (t nil).)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Variables
;;;

(defvar db-sort-modifies-p nil
  "*If non-nil, then sorting a database marks it as modified too.")

;; Field priorities is a cons of two lists; the first list is those that
;; will be used, and the the second set is the ignored fields.  If the user
;; sets this, he can hide the second list; it is always kept by the
;; program, though, as its order might be worthwhile (for instance, for
;; reminding the user of what the order used to be).  Each list consists of
;; (fieldnumber . order-info) pairs such that every field is accounted for
;; exactly once.  The order-info is 'increasing, 'decreasing, or a cons of
;; (type . value) where type is 'order-function or 'sort-function.

;; Do I want this to be by database or by format?
;; There is probably a natural order (which might not be the order of the
;; fields for any of a number of reasons), and the database ought to be
;; able to tell that to the format.  But there may be natural priorities
;; for particular formats, too.  And might we want to have different ones
;; for different formats?
;; Answer:  put it in the database, but when it's first used, make it
;; format-local.

;;; There isn't a good way to unset these.
(deflocalvar dbf-field-priorities nil
  "The list of field priorities for this database in this data display buffer.
If non-nil, overrides the database's field-priorities slot.")
(deflocalvar dbf-hidden-to-end-p nil
  "The default, local to this data display buffer, for the hidden-to-end-p
database slot.  Only used if  dbf-field-priorities  is non-nil.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Sort
;;;

;; This doesn't now use hidden-records-at-end-p; should it?
;; Does that belong as a database slot or should it be specific to the sorter?
;; [Well, since the default sorter is a database slot, it should be a database
;;  slot too.]

;;;###autoload
(defun database-sort (database &optional sorter hidden-records-at-end-p)
  "Sort and return DATABASE, which is also side-effected.  SORTER is
either a field priorities list or a function which takes two records
as arguments and returns t if r1 < r2."
  (db-debug-message "database-sort:  sorter = %s" sorter)
  (db-message "Sorting...")
  (if (not (database-empty-p database))
      (let (ordering-fields-converted)

	(if (not (db-functionp sorter))
	    (setq ordering-fields-converted (db-convert-ordering-fields
					     sorter database)
		  sorter (function db-record-lessp)))
	(db-debug-message "database-sort:  ordering-fields-converted are %s"
			  ordering-fields-converted)
	(database-set-links-from-list
	 database
	 (sort (database-list-of-links database)
	       (function
		(lambda (l1 l2)
		  (funcall sorter
			   (link-record l1)
			   (link-record l2))))))))
  (if db-sort-modifies-p
      (progn (database-set-modified-p database t)
	     (force-mode-line-update)))
  (db-message "Sorting...done.")
  database)


;; I do NOT want to have a for-sorting option here, for lots of reasons.
;; Mostly because that can produce incorrect results.

;; Gets rid of extraneous fields, and makes sure that every cdr is an
;; ordering function.  I don't want to modify the argument, though.  (Why
;; not?  Well, the user might be surprised by the format.  But that doesn't
;; bother me; he can always munge it up again.  But then again, this isn't
;; so much work compared to the actual sorting, is it?)  I should at least
;; modify it enough to include all fields.  Nah, don't bother, though that
;; is done in sort-interface.

(defun db-convert-ordering-fields (ordering-fields database)
  (let (fieldno sortinfo type value)
    (mapcar (function (lambda (this-cons)
	      (setq fieldno (car this-cons)
		    sortinfo (cdr this-cons))
	      ;; (db-debug-message "db-c-o-f:  this-cons = %s" this-cons)
	      (cons fieldno
		    (if (atom sortinfo)
			;; sortinfo = 'increasing or 'decreasing (or nil)
			;; This is the usual case.
			(recordfieldspec-order-function
			 (database-recordfieldspec database fieldno)
			 (eq sortinfo 'decreasing))
		      (progn
			(setq type (car sortinfo)
			      value (cdr sortinfo))
			(cond ((eq 'order-function type)
			       value)
			      ((eq 'sort-function type)
			       ;; I can do better than this here.
			       (error "Trying to order, but only sort function provided for field %s (%s)." fieldno (fieldnumber->fieldname fieldno database)))
			      (t (error "Unrecognized type in %s" sortinfo))))))))
	    (car (or ordering-fields
		     (database-field-priorities database))))))

;; Do I really want to do check cdrs in this?  I am...

;; Makes sure that every car is a number (not a fieldname) and makes sure
;; that every field is represented.

(defun db-make-ordering-fields-canonical (ordering-fields database)
  (let* ((no-of-fields (database-no-of-fields database))
	 (field-accounted-for (make-vector no-of-fields nil))
	 fno sortinfo value
	 (check-field
	  (function
	   (lambda (this-cons)
	     (setq fno (if (numberp (car this-cons))
			   (car this-cons)
			 (fieldname->fieldnumber (car this-cons) database))
		   sortinfo (cdr this-cons))
	     (cond ((not fno)
		    (db-warning "%s is not a field in the database; ignoring it."
			     (car this-cons))
		    nil)
		   ((aref field-accounted-for fno)
		    (db-warning "%s already appears; ignoring subsequent entries."
			     (fieldnumber->fieldname fno database))
		    nil)
		   (t
		    (aset field-accounted-for fno t)
		    (cons fno
			  (cond ((eq sortinfo 'decreasing)
				 'decreasing)
				((atom sortinfo)
				 (if (and sortinfo (not (eq sortinfo 'increasing)))
				     (db-warning "Unrecognized ordering symbol %s being changed to 'increasing."
					      sortinfo))
				 'increasing)
				;; sortinfo is a list
				((or (eq (car sortinfo) 'order-function)
				     (eq (car sortinfo) 'sort-function))
				 (if (not (db-functionp (cdr sortinfo)))
				     (db-warning "%s is claimed to be a %s but doesn't look like a function to me."
						 (cdr sortinfo) (car sortinfo)))
				 sortinfo)
				(t (error "Unrecognized type in %s" sortinfo)))))))))
	 (sig-fields (mapcar check-field (car ordering-fields)))
	 (nonsig-fields (mapcar check-field (cdr ordering-fields)))
	 missing-fields)
    (setq fno 0)
    (while (< fno no-of-fields)
      (if (not (aref field-accounted-for fno))
	  (progn
	    (db-warning "%s doesn't appear; adding it."
		     (fieldnumber->fieldname fno database))
	    (setq missing-fields
		  (cons (cons fno 'increasing) missing-fields))))
      (setq fno (1+ fno)))
    (cons sig-fields
	  (if missing-fields
	      (append nonsig-fields missing-fields)
	    nonsig-fields))))


;; Uses the ordering-fields-converted dynamic variable
;;   "Return -1, 0, or 1 depending on whether RECORD1 and RECORD2 are <, =, or >.
;; Uses the dynamic variable  ordering-fields-converted  to determine in which
;; order to compare record fields."
(defun db-order-records (record1 record2)
  (let ((ordering-fields ordering-fields-converted)
	this-fieldno
	this-order-fn
	(result 0))
    ;; (db-debug-message "db-order-records: ordering-fields = %s" ordering-fields)
    (while (and ordering-fields (zerop result))
;;       (db-debug-message "db-o-r:  about to call %s on field %s:  %s and %s"
;; 	       (cdr (car ordering-fields))
;; 	       (car (car ordering-fields))
;; 	       (aref record1 (car (car ordering-fields)))
;; 	       (aref record2 (car (car ordering-fields))))
      (setq this-fieldno (car (car ordering-fields))
	    this-order-fn (cdr (car ordering-fields))
	    ordering-fields (cdr ordering-fields)
	    result (funcall this-order-fn
			    (aref record1 this-fieldno)
			    (aref record2 this-fieldno)))
;;       (db-debug-message "called; result = %s" result)
      )
;;     (db-debug-message "db-order-records: %s %s %s" result record1 record2)
    result))

;; In the absense of an explicit equality test in db-record-lessp, using
;; sorting-blah can be less efficient than ordering-blah; it can also be
;; wrong.  If a1 > b1 and a2 < b2, we'll get "true" from the iterated
;; sorter.  So I always want to use an orderer, never a sorter; when I only
;; have a sorter, supplement it with an initial equality test.  And if I've
;; built my sorter from an orderer, this equality test will be done twice
;; (if it fails), which is ridiculous.  Sorting (ie, one bit of infomation)
;; is useful for a whole item (eg a whole record); but for components it's
;; not so great.  (Unless we are sorting ONLY on that field, in which case
;; it's OK; do I want to special-case that or not?)


;;   "Return t if RECORD1 < RECORD2.  Calls `db-order-records'."
(defun db-record-lessp (record1 record2)
  (= -1 (db-order-records record1 record2)))


;; I could also use a sorter, but that's probably the wrong way to go about
;; it.  (And I don't see an application calling for a sorter just now.)

;; Is this really the right way to go about it, or should I use orderer?

;;   "Return t if DATABASE is sorted, nil otherwise.
;; Optional argument SORTER is used as the function."
(defun database-sorted-p (database &optional sorter)
  (with-sorter database
    (let ((sorted t)
	  (first-link-p t))
      ;; Don't do anything for the first link.
      ;; If some other link is less than its predecessor, complain.
      (maplinks-macro
	(if first-link-p
	    (setq first-link-p nil)
	  (if (funcall sorter
		       (link-record maplinks-link)
		       (link-record (link-prev maplinks-link)))
	      (progn
		(setq sorted nil)
		(maplinks-break))))
	database)
      sorted)))

;;   "Return t if DATABASE is ordered, nil otherwise.
;; Optional argument ORDERER is used as the ordering function."
(defun database-ordered-p (database &optional orderer)
  (with-orderer database
    (let ((ordered t)
	  (first-link-p t))
      ;; I expect this will return t most of the time, so don't provide an
      ;; easy escape.
      ;; Don't do anything for the first link.
      ;; If some other link is less than its predecessor, complain.
      (maplinks-macro
	(if first-link-p
	    (setq first-link-p nil)
	  (if (= 1 (funcall orderer
			    (link-record (link-prev maplinks-link))
			    (link-record maplinks-link)))
	      (setq ordered nil)))
	database)
      ordered)))

;; This is something of a hack (and not currently used).
;;   "Return t if no two elements of DATABASE have the same ordering keys, or if
;; user says to proceed even though identical keys have been found.
;; DATABASE should be ordered when this is called.
;; Optional argument ORDERER is used as the ordering function."
(defun database-no-identical-keys-p (database &optional orderer)
  (with-orderer database
    (let ((ordered t)
	  (first-link-p t))
      ;; Don't do anything for the first link.
      ;; If some other link is less than its predecessor, complain.
      (maplinks-macro
	(if first-link-p
	    (setq first-link-p nil)
	  (if (= 0 (funcall orderer
			    (link-record (link-prev maplinks-link))
			    (link-record maplinks-link)))
	      (progn
		(db-warning "Two elements with identical keys found.")
		;; Say what the identical keys were...
		;; I expect this will return t most of the time, so don't
		;; provide an easy escape.
		(setq ordered nil))))
	database)
      (and ordered
	   (not (y-or-n-p "Proceed anyway?"))))))

;; If the variable orderer is nil, then sets orderer and binds
;; ordering-fields-converted.  Otherwise just binds
;; ordering-fields-converted to nil.

;; Where did `orderer' and `sorter' come from, anyway?
;; I think they're dynamically bound, from the lambda list, some of the time.

(defmacro with-orderer (database &rest body)
  (` (let ((ordering-fields-converted))
       (if (not orderer)
	   (progn
	     (setq orderer (function db-order-records)
		   ordering-fields-converted
  		     ;; stupid way to call this function
		     (db-convert-ordering-fields nil (, database))
		   )
	     (db-debug-message "with-orderer:  setting orderer; ofc = %s"
		      ordering-fields-converted)
	     ))
       (,@ body))))
;; (put 'with-orderer 'lisp-indent-hook 0)
(put 'with-orderer 'lisp-indent-hook 1)
(put 'with-orderer 'edebug-form-spec '(&rest form))

(defmacro with-sorter (database &rest body)
  (` (let ((ordering-fields-converted))
       (if (not sorter)
	   (setq sorter (function db-record-lessp)
		 ordering-fields-converted
  		   ;; stupid way to call this function
		   (db-convert-ordering-fields nil (, database))
		 ))
       (,@ body))))
;; (put 'with-sorter 'lisp-indent-hook 0)
(put 'with-sorter 'lisp-indent-hook 1)
(put 'with-sorter 'edebug-form-spec '(&rest form))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Sorting interface
;;;

;; Oh, this code is gross.  Please don't look at it.

;; Have one database-sort-interface function which pops up the list; then,
;; in addition to being able to change the sorting order, have two
;; commands, one to accept the sorting order and one to sort on the field
;; under point.

;; Also be able to toggle hidden-records-at-end-p here.


;; This calls database-sort itself because that's easier than returning values.
;; It also sets database values.

;; Commands:
;;  cursor movement
;;  kill a line
;;  yank a line
;;  sort on this field only
;;  accept this ordering, sort with it, and make it the default
;;  accept this ordering and sort with it, but don't make it the default
;;  toggle ordering direction (increasing vs decreasing) or -to-end-p
;;  input custom ordering info:  a list or a function name

;;; The buffer looks like:
;; ==== Significant fields:
;; foo
;; bar
;; baz
;; ==== Nonsignificant fields:
;; bum
;; but
;; ==== Hidden records to end:  nil

;; I ought to make the modeline, etc. more like the other database submodes.

;; This won't always be called from the data display buffer, but it assumes
;; that it will.  Fix that, maybe.

;;;###autoload
(defun database-sort-interface (database)

  (let ((canonical-ordering-fields (db-make-ordering-fields-canonical
				    (database-field-priorities database)
				    database))
	(data-display-buffer (current-buffer)))
    (switch-to-buffer (get-buffer-create
		       (concat "Ordering info for " (database-print-name database))))
    (database-sort-interface-mode)

    (setq dbsi-data-display-buffer data-display-buffer
	  dbsi-database database
	  dbsi-hidden-to-end-p (database-hidden-to-end-p database)
	  dbsi-killed-fields nil
	  dbsi-sig-fields (cons nil (car canonical-ordering-fields))
	  dbsi-nonsig-fields (cons nil (cdr canonical-ordering-fields)))

    ;; Fill the buffer with info.
    (let ((buffer-read-only nil))
      (erase-buffer)
      (insert "==== Significant fields:\n")
      (mapcar (function dbsi-format) (cdr dbsi-sig-fields))
      (insert "==== Nonsignificant fields:\n")
      (mapcar (function dbsi-format) (cdr dbsi-nonsig-fields))
      (insert "==== Hidden records to end:  "
	      (if dbsi-hidden-to-end-p "Yes" "No ")))
    (goto-char (point-min))
    (db-message "u = use, RET = also make default, ! = this field only, q = abort, ? = help")
    ))

(defun database-sort-interface-mode ()
  "Sort interface buffer for choosing fields upon which to sort a database.
\\<database-sort-mode-map>
Reorder the fields with the following commands:
  \\[dbsi-kill-line]	Kill field under point.
  \\[dbsi-yank-line]	Yank most recently killed field.
Specify how a particular field should be ordered with these commands:
  \\[dbsi-increasing]	Sort the field under point in increasing order.
  \\[dbsi-decreasing]	Sort the field under point in decreasing order.
  \\[dbsi-ordering-function]	Specify an ordering function for the field at point.
  \\[dbsi-sorting-function]	Specify a sorting function for the field at point.
Change whether hidden records go at the end of the sorted order with:
  \\[dbsi-toggle-hidden-to-end]	Toggle the value of  dbsi-hidden-to-end-p.
Exit the sort interface with the following commands:
  \\[dbsi-use-ordering-make-database-default]	Use this ordering, and make it the default for this database.
  \\[dbsi-use-ordering-make-buffer-default]	Use this ordering, and make it the default in this buffer only.
  \\[dbsi-quit-clear-buffer-default]	Clear the default sort in this buffer, and abort the sort.
  \\[dbsi-use-ordering]	Use this ordering, but don't make it the default.
  \\[dbsi-this-field-only]	Sort only according to the field at point.
  \\[dbsi-quit]	Abort the sort; don't change the default ordering criterion.

More specifically:
\\{database-sort-mode-map}"
  (setq major-mode 'database-sort-interface-mode)
  (setq mode-name "Database Sort Interface")

  (set-buffer-modified-p nil)
  (setq buffer-read-only t)

  (auto-save-mode 0)
  (setq buffer-file-name nil)

  (setq truncate-lines t)

  (use-local-map database-sort-mode-map)

  ;; Pared-down mode line.
  ;; Perhaps I should add a change-indicator.
  (setq mode-line-format
	'("---Database Sort: %17b ["
	  (-3 . "%p")
	  "]-%-"))
  )

;; A conundrum:  I would like to pass control back to the data display buffer
;; from whence I started, updating it and all, but something might have
;; happened to it in the meanwhile.  Actually, that doesn't matter; all I
;; really need is to recompute the index and invalidate the summary buffer,
;; right?


(deflocalvar dbsi-data-display-buffer)
(deflocalvar dbsi-database nil)
(deflocalvar dbsi-killed-fields nil)
;; These are lists whose first element is ignored.
(deflocalvar dbsi-sig-fields nil)
(deflocalvar dbsi-nonsig-fields nil)
(deflocalvar dbsi-hidden-to-end-p nil
  "The hidden-to-end slot of the database will be set from this variable.")

;;   "Keymap for database data display buffer in view mode."
(defvar database-sort-mode-map (make-keymap))

(suppress-keymap database-sort-mode-map)

(define-key database-sort-mode-map "\C-k" 'dbsi-kill-line)
(define-key database-sort-mode-map "\C-y" 'dbsi-yank-line)
(define-key database-sort-mode-map "!" 'dbsi-this-field-only)
(define-key database-sort-mode-map "\r" 'dbsi-use-ordering-make-database-default)
(define-key database-sort-mode-map "\C-c\C-c" 'dbsi-use-ordering-make-database-default)
(define-key database-sort-mode-map "A" 'dbsi-use-ordering-make-buffer-default)
(define-key database-sort-mode-map "U" 'dbsi-use-ordering-make-buffer-default)
(define-key database-sort-mode-map "c" 'dbsi-quit-clear-buffer-default)
(define-key database-sort-mode-map "a" 'dbsi-use-ordering)
(define-key database-sort-mode-map "u" 'dbsi-use-ordering)
(define-key database-sort-mode-map "t" 'dbsi-toggle-hidden-to-end)
(define-key database-sort-mode-map "o" 'dbsi-ordering-function)
(define-key database-sort-mode-map "s" 'dbsi-sorting-function)
(define-key database-sort-mode-map "i" 'dbsi-increasing)
(define-key database-sort-mode-map "d" 'dbsi-decreasing)
(define-key database-sort-mode-map "q" 'dbsi-quit)
(define-key database-sort-mode-map "?" 'describe-mode)

;; Need to improve the error messages.

;; PC is previous-cons
(defmacro with-pc (&rest body)
  (` (let ((pc (dbsi-prev-cons))
	   (buffer-read-only nil))
       ;; (db-debug-message "with-pc: %s" pc)
       (if pc
	   (progn (,@ body))
	 (error "Can't insert here.")))))
(put 'with-pc 'lisp-indent-hook 0)
(put 'with-pc 'edebug-form-spec '(&rest form))

;; PC is previous-cons; TI is this-item.
(defmacro with-pc-and-ti (&rest body)
  (` (let* ((pc (dbsi-prev-cons))
	    (ti (car (cdr pc)))
	    (buffer-read-only nil))
       ;; (db-debug-message "with-pc-and-ti: %s, %s" ti pc)
       (if ti
	   (progn (,@ body))
	 (error "No field on current line.")))))
(put 'with-pc-and-ti 'lisp-indent-hook 0)
(put 'with-pc-and-ti 'edebug-form-spec '(&rest form))

;;; Determining where we are.

;; Returns a cons cell whose cdr points to the cons cell holding the
;; current info, or nil if not on an ordering line.

;; For convenience in manipulating the list.

;; I have no idea whether this is correct; it's a crock; just test it 'til
;; it's right.

(defun dbsi-prev-cons ()
  ;; actually 1 less than the line number
  (let ((line-no (save-excursion
		   (beginning-of-line)
		   (count-lines (point-min) (point)))))
    (db-debug-message "dbsi-prev-cons:  line-no = %d" line-no)
    (cond ((zerop line-no)
	   nil)
	  ((<= line-no (length dbsi-sig-fields))
	   (nthcdr (1- line-no) dbsi-sig-fields))
	  (t
	   (setq line-no (- line-no (length dbsi-sig-fields) 1))
	   (db-debug-message "dbsi-prev-cons:  now line-no = %d" line-no)
	   (if (< line-no (length dbsi-nonsig-fields))
	       (nthcdr line-no dbsi-nonsig-fields))))))

;; This isn't quite right; can have a previous cons (for purposes of
;; insertion) without having a this-item.
;; [Well, it's OK; this-item is nil in that case.]

;; (defun dbsi-this-item ()
;;   (car (cdr (dbsi-prev-cons))))

;; Insert item and trailing newline.
(defun dbsi-format (item)
  (insert (format "%-16s    " (fieldnumber->fieldname (car item) dbsi-database)))
  (if (atom (cdr item))
      (insert (format "%s" (cdr item)))
    (insert (format "%s [%s]" (cdr (cdr item)) (car (cdr item)))))
  (insert "\n"))

(defun dbsi-reformat (item)
  (beginning-of-line)
  (kill-line 1)
  (dbsi-format item))

;;; Moving fields around.

(defun dbsi-kill-line ()
  "Kill field on current line, placing it in the sort interface kill stack."
  (interactive)
  (with-pc-and-ti
    (setq dbsi-killed-fields
	  (cons ti dbsi-killed-fields))
    (setcdr pc (cdr (cdr pc)))
    (beginning-of-line)
    (kill-line 1)))

(defun dbsi-yank-line ()
  "Yank most recently killed (lifo ordering) field, inserting it before point.
This removes the field from the sort interface kill stack."
  (interactive)
  (with-pc
    (if dbsi-killed-fields
	(let ((ti (car dbsi-killed-fields)))
	  (beginning-of-line)
	  (setcdr pc (cons ti (cdr pc)))
	  (dbsi-format ti)
	  (previous-line 1)
	  (setq dbsi-killed-fields (cdr dbsi-killed-fields)))
      (error "Nothing to yank."))))

;;; Changing info about hiding.

(defun dbsi-toggle-hidden-to-end ()
  "Toggle the boolean value of `dbsi-hidden-to-end-p'.
This controls whether hidden records should all be placed at the end
of the sorted order or should be sorted according to the same criteria as
non-hidden records."
  (interactive)
  (save-excursion
    (let ((buffer-read-only nil))
      (setq dbsi-hidden-to-end-p (not dbsi-hidden-to-end-p))
      (goto-char (point-max))
      (backward-delete-char 3)
      (insert (if dbsi-hidden-to-end-p "Yes" "No ")))))

;;; Changing

(defun dbsi-increasing ()
  "Specify that the field at point should use an increasing ordering."
  (interactive)
  (with-pc-and-ti
    (setcdr ti 'increasing)
    (dbsi-reformat ti)))

(defun dbsi-decreasing ()
  "Specify that the field at point should use a decreasing ordering."
  (interactive)
  (with-pc-and-ti
    (setcdr ti 'decreasing)
    (dbsi-reformat ti)))

(defun dbsi-ordering-function ()
  "Specify an ordering function for the field at point.
An ``ordering function'' returns -1, 0, or 1 depending on whether its first
argument is less than, equivalent to, or greater than its second argument."
  (interactive)
  (with-pc-and-ti
    (setcdr ti (cons 'order-function
		     (read-from-minibuffer "Ordering function: " nil nil t)))
    (dbsi-reformat ti)))

(defun dbsi-sorting-function ()
  "Specify a sorting function for the field at point.
A ``sorting function'' returns t if its first argument is less
than its second argument and nil otherwise."
  (interactive)
  (with-pc-and-ti
    (setcdr ti (cons 'sort-function
		     (read-from-minibuffer "Sorting function: " nil nil t)))
    (dbsi-reformat ti)))

;;; Quitting

(defun dbsi-quit ()
  "Abort the sort and exit the sort interface."
  (interactive)
  (let ((data-display-buffer dbsi-data-display-buffer))
    (kill-buffer (current-buffer))
    (switch-to-buffer data-display-buffer)
    (db-message "Aborting sort.")))

(defun dbsi-quit-clear-buffer-default ()
  "Clear the default sort order for this buffer and exit the sort interface
without sorting.
In the future, the default sort order will come from the database."
  (interactive)
  (let ((data-display-buffer dbsi-data-display-buffer))
    (kill-buffer (current-buffer))
    (switch-to-buffer data-display-buffer)
    (setq dbf-field-priorities nil)
    (db-message "Reset buffer-local default sort order; didn't sort.")))

;;; Specifying an ordering.

(defun dbsi-killed-fields-to-end-maybe ()
  (if (cdr dbsi-killed-fields)
      (if (y-or-n-p
	   "Put killed but not yanked fields at end of nonsignificant list? ")
	  (nconc dbsi-nonsig-fields (cdr dbsi-killed-fields))
	(error "There are still killed fields."))))

(defun dbsi-use-ordering-make-database-default ()
  "Use the current ordering to sort, and make it the default for future sorts
of this database.
The user is warned if there are killed, non-yanked fields."
  (interactive)
  (dbsi-killed-fields-to-end-maybe)
  (in-buffer dbsi-data-display-buffer
      (setq dbf-field-priorities nil))
  (database-set-field-priorities dbsi-database
	(cons (cdr dbsi-sig-fields) (cdr dbsi-nonsig-fields)))
  (database-set-hidden-to-end-p dbsi-database dbsi-hidden-to-end-p)
  (dbsi-use-ordering (database-field-priorities dbsi-database)))

(defun dbsi-use-ordering-make-buffer-default ()
  "Use the current ordering to sort, and make it the default for future sorts
in this data display buffer only.
The user is warned if there are killed, non-yanked fields."
  (interactive)
  (dbsi-killed-fields-to-end-maybe)
  (let ((ordering (cons (cdr dbsi-sig-fields) (cdr dbsi-nonsig-fields))))
    (in-buffer dbsi-data-display-buffer
      (setq dbf-field-priorities ordering
	    dbf-hidden-to-end-p dbsi-hidden-to-end-p))
    (dbsi-use-ordering ordering)))

(defun dbsi-use-ordering (&optional ordering)
  "Use the current ordering for this sort only."
  (interactive)
  (db-debug-message "(not dbsi-database) = %s" (not dbsi-database))
  (database-sort dbsi-database
		 (or ordering (cons (cdr dbsi-sig-fields) nil))
		 dbsi-hidden-to-end-p)
  (let ((data-display-buffer dbsi-data-display-buffer))
    (kill-buffer (current-buffer))
    (switch-to-buffer data-display-buffer)
    (dbf-finished-sorting)))


;; If sorting on a field, don't even use ordering-fields; get the field's
;; sorting function and call database-sort with just that (with a wrapper
;; so that it takes a record as an argument).  This is why we want to
;; permit (but not encourage, particularly) sort functions in recordfieldspecs.


;; This is similar to, but different from, what's done in
;; db-convert-ordering-fields.

(defun dbsi-this-field-only ()
  "Sort according to only the field at point.
All editing of other fields is ignored."
  (interactive)
  (dbsi-use-ordering
   (with-pc-and-ti
     (let* ((fieldno (car ti))
	    (sortinfo (cdr ti))
	    (sort-function
	     (if (atom sortinfo)
		 (recordfieldspec-sort-function
		  (database-recordfieldspec dbsi-database fieldno)
		  (eq sortinfo 'decreasing))
	       (let ((car-sortinfo (car sortinfo))
		     (cdr-sortinfo (cdr sortinfo)))
		 (cond ((eq car-sortinfo 'order-function)
			(function (lambda (item1 item2)
				    (= -1 (funcall (cdr sortinfo) item1 item2))))
			;; ;; I don't know if this would have worked.
			;; 		  (` (lambda (item1 item2)
			;; 		       (= -1 ((, (cdr sortinfo)) item1 item2))))
			)
		       ((eq car-sortinfo 'sort-function)
			(cdr sortinfo)))))))
       (` (lambda (record1 record2)
	    (funcall (function (, sort-function))
		     (aref record1 (, fieldno))
		     (aref record2 (, fieldno)))))))))

;;; db-sort.el ends here
