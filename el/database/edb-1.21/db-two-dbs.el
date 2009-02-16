;;; db-two-dbs.el --- part of EDB, the Emacs database

;; See database.el for copyright notice, distribution conditions, etc.

;; Author: Michael Ernst <mernst@theory.lcs.mit.edu>
;; Keywords: EDB

;;; Commentary:

;; Support for actions on two databases.

;;; Code:


(provide 'db-two-dbs)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Process two databases
;;;

;;;###autoload
(defun db-process-two-databases (db1 db2
				     process-lone-record-db1
				     process-lone-record-db2
				     process-corresponding-records
				     &optional orderer)
  (db-debug-message "db-process-two-databases:  before with-orderer, dbf-field-priorities = %s" dbf-field-priorities)
  (with-orderer db1
    (db-debug-message "db-process-two-databases:  orderer = %s" orderer)
    (db-debug-message "db-process-two-databases:  process-corresponding-records = %s"
	     process-corresponding-records)
    (if (not (database-ordered-p db1 orderer))
	(progn
	  (db-message "db-process-two-databases:  sorting %s."
		   (database-print-name db1))
	  (database-sort db1 orderer)))
    (if (not (database-ordered-p db2 orderer))
	(progn
	  (db-message "db-process-two-databases:  sorting %s."
		   (database-print-name db2))
	  (database-sort db2 orderer)))

    ;; Perhaps check for identical keys here.

    (db-message "Databases are properly ordered.")
    (let* ((db1-first-link (database-first-link db1))
	   (db1-link db1-first-link)
	   (db2-first-link (database-first-link db2))
	   (db2-link db2-first-link)
	   record1 record2 record-order
	   (done1 (database-empty-p db1))
	   (done2 (database-empty-p db2)))
      (db-debug-message "db-process-two-databases:  entering while loop")
      (while (not (or done1 done2))
	(setq record1 (link-record db1-link)
	      record2 (link-record db2-link)
	      record-order (funcall orderer record1 record2))
	;; (db-debug-message "two records:  %s %s" record1 record2)
	(cond ((= -1 record-order)
	       ;; (db-debug-message "< %s %s" record1 record2)
	       (funcall process-lone-record-db1 record1)
	       (setq db1-link (link-next db1-link))
	       (if (eq db1-link db1-first-link) (setq done1 t)))
	      ((= 1 record-order)
	       ;; (db-debug-message "> %s %s" record1 record2)
	       (funcall process-lone-record-db2 record2)
	       (setq db2-link (link-next db2-link))
	       (if (eq db2-link db2-first-link) (setq done2 t)))
	      ((= 0 record-order)
	       ;; (db-debug-message "= %s %s" record1 record2)
	       (funcall process-corresponding-records record1 record2)
	       (setq db1-link (link-next db1-link)
		     db2-link (link-next db2-link))
	       (if (eq db1-link db1-first-link) (setq done1 t))
	       (if (eq db2-link db2-first-link) (setq done2 t))
	       ;; (db-debug-message "coresponding records processed")
	       )
	      (t
	       (error "Bad result %s from orderer." record-order))))
      (while (not done1)
	(funcall process-lone-record-db1 (link-record db1-link))
	(setq db1-link (link-next db1-link))
	(if (eq db1-link db1-first-link) (setq done1 t)))
      (while (not done2)
	(funcall process-lone-record-db2 (link-record db2-link))
	(setq db2-link (link-next db2-link))
	(if (eq db2-link db2-first-link) (setq done2 t)))
      nil
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Merge
;;;

; Want to display both records in full and, for each differing field, ask how
; to set the merged database:  from one, from the other, or by entering
; something particular.  It should also be possible to edit the merged record
; before proceeding to the next one.

;; Should check for the consistency of the two databases.  Should permit
;; conversion for one or both databases as part of this procedure.

;; Should permit specification of db3 info, or specification of db3 itself.

; I'd like to pass in some buffers (probably three) as well.

;; Ought to pass just two arguments, each a database or format.

;; Return a third database.
(defun db-merge-internal (db1 db2 db3 buffer1 buffer2 buffer3)

  (delete-other-windows)
  (switch-to-buffer buffer1)
  (split-window-vertically (/ (window-height) 2))
  (other-window 1)
  (switch-to-buffer buffer2)
  (split-window-vertically)
  (other-window 1)
  (switch-to-buffer buffer3)
  (set-buffer buffer1)

  (db-process-two-databases
   db1 db2
   (function (lambda (record) (database-add-record record db3)))
   (function (lambda (record) (database-add-record record db3)))
   (function (lambda (record1 record2)
	       (database-add-record (db-merge-records record1 record2 db3
						      buffer1 buffer2 buffer3)
				    db3))))
  )


;;;###autoload
(defun db-merge ()
  "Merge two databases chosen with completion among read-in databases."
  (interactive)

  (let ((databases-alist (mapcar (function (lambda (database)
			           (cons (database-print-name database)
					 database)))
				 db-databases))
	db1 db2 db3 db1-buffer db2-buffer db3-buffer)
    (cond ((< (length databases-alist) 2)
	   (error "db-merge requires there to be at least two read-in databases."))
	  ((= 2 (length databases-alist))
	   (setq db1 (car db-databases)
		 db2 (car (cdr db-databases))))
	  (t
	   ;; Could check that a data display buffer exists.
	   (setq db1 (assoc (completing-read
			     "First database to merge?  [? for options] "
			     databases-alist nil t nil)
			    databases-alist))
	   (setq databases-alist (delq db1 databases-alist))
	   (setq db1 (cdr db1))
	   ;; could check for compatibility, or remove incompatible databases
	   ;; (and warn if none are left).

	   (setq db2 (cdr (assoc (completing-read
				  "Second database to merge?  [? for options] "
				  databases-alist nil t nil)
				 databases-alist)))))
    (setq db1-buffer (car (database-clean-data-display-buffers db1))
	  db2-buffer (car (database-clean-data-display-buffers db2)))
    (db-debug-message "db-merge:  about to create db3")
    (setq db3 (make-similar-database db1)
	  db3-buffer (car (database-clean-data-display-buffers db3)))
    (database-set-print-name db3
	  (concat "Merge of `" (database-print-name db1)
		  "' and `" (database-print-name db2) "'"))
    (db-debug-message "db-merge:  created db3.")
    (let ((db1-index (in-buffer db1-buffer dbc-index))
	  (db2-index (in-buffer db2-buffer dbc-index)))
      (db-merge-internal db1 db2 db3 db1-buffer db2-buffer db3-buffer)
      (in-buffer db1-buffer (db-jump-to-record db1-index))
      (in-buffer db2-buffer (db-jump-to-record db2-index))
      (in-buffer db3-buffer (db-jump-to-record 1))))
  (message "Done merging."))


;; The three buffers should already be visible.

;; The displayspecs should be identical, I think.

(defun db-merge-records (record1 record2 database buffer1 buffer2 buffer3)
  (db-debug-message "db-merge-records called on %s %s" record1 record2)

  (if (equal record1 record2)
      record1
    (progn
      ;; (db-debug-message "db-merge-records:  unequal records")
      (set-buffer buffer1)
      (display-record record1 t)
      (set-buffer buffer2)
      (display-record record2 t)
      (set-buffer buffer3)
      (let ((record3 (make-record database))
	    (fieldno 0)
	    contents1
	    contents2
	    (db1-displayspecs (in-buffer buffer1 dbf-displayspecs))
	    db1-displayspec
	    (db2-displayspecs (in-buffer buffer2 dbf-displayspecs))
	    db2-displayspec
	    recordfieldspec)
	;; (db-debug-message "db-merge-records:  in let")
	(while (< fieldno (database-no-of-fields database))
	  (setq db1-displayspec (aref db1-displayspecs fieldno)
		db2-displayspec (aref db2-displayspecs fieldno)
		recordfieldspec (database-recordfieldspec database fieldno)
		contents1 (db-funcall-maybe (recordfieldspec-common-form-function
					     recordfieldspec)
					    (aref record1 fieldno))
		contents2 (db-funcall-maybe (recordfieldspec-common-form-function
					     recordfieldspec)
					    (aref record2 fieldno)))
	  ;; I should make an attempt at consolidating them.
	  (if (equal contents1 contents2)
	      (progn
		;; (db-debug-message "merge-records equal values %s" contents1)
		;; Does no constraint checking; is this the right thing to do?
		(record-set-field-from-index record3 fieldno contents1 nil))
	    (let ((fieldname (fieldnumber->fieldname fieldno database)))
	      ;; (db-debug-message "merge-records unequal values %s %s" contents1 contents2)
	      ;; Actually, I only want to display the filled-in-so-far fields.
	      ;; Problem is that nil might not be valid in some places.
	      ;; For now, ignore that.
	      (display-record record3 t fieldno)
	      ;; Does no constraint checking; is this the right thing to do?
	      ;; Help for one-char-question is not entirely satisfactory here.
	      (record-set-field-from-index
	       record3 fieldno
	       (choose-value contents1 contents2
			     (fieldnumber->fieldname fieldno database)
			     db1-displayspec db2-displayspec)
	       nil)))
	  (setq fieldno (1+ fieldno)))
	record3
	))))

;; Don't need the whole displayspec here, just actual->display,
;; display->actual, and fieldname.
(defun choose-value (contents1 contents2 fieldname displayspec1 displayspec2)
  (cond ((y-or-n-p (format "Use first value for %s field? [%s] "
			   fieldname
			   (actual->display-call
			    (displayspec-actual->display displayspec1)
			    contents1
			    nil nil)))
	 contents1)
	((y-or-n-p (format "Use second value for %s field? [%s] "
			   fieldname
			   (actual->display-call
			    (displayspec-actual->display displayspec2)
			    contents2
			    nil nil)))
	 contents2)
	(t
	 (if (equal (displayspec-display->actual displayspec1)
		    (displayspec-display->actual displayspec2))
	     (read-fieldvalue-from-minibuffer fieldname displayspec1)
	   (error "displayspecs have different display->actual")))))

(defun read-fieldvalue-from-minibuffer (fieldname displayspec)
  (display->actual-call
   (displayspec-display->actual displayspec)
   (read-from-minibuffer
    (format "Enter value for %s field: " fieldname))
   nil nil nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Compare
;;;

;;;###autoload
(defun databases-compatible (db1 db2)
  "Return t if the database records have the same field names and type, nil otherwise."
  ;; Should eventually check types as well.
  (recordfieldspecs-compatible db1 db2))

;; This can probably be just a call to process-two-databases.  (Should I
;; provide some abstraction as well?  I dunno.  That's why this is called a
;; hack.)

(defun database-compare-hack (db1 db2)
  ;; Check that fieldnames and types are the same.
  (if (not (databases-compatible db1 db2))
      (error "Incompatible databases."))
  (let* ((name1 (or (database-print-name db1) "First Database"))
	 (name2 (or (database-print-name db2) "Second Database"))
	 (loners1 (get-buffer-create (concat "Loners for " name1)))
	 (loners2 (get-buffer-create (concat "Loners for " name2)))
	 (discrep (get-buffer-create (concat "Discrepancies between "
					     name1 "and " name2))))
    (in-buffer loners1
      (erase-buffer))
    (in-buffer loners2
      (erase-buffer))
    (in-buffer discrep
      (erase-buffer))

    (db-process-two-databases
     db1 db2
     (function (lambda (record)
		 (in-buffer loners1
		   (print-record record db1))))
     (function (lambda (record)
		 (in-buffer loners2
		   (print-record record db2))))
     (function (lambda (record1 record2)
		 ;; We already know that order-function considers the two
		 ;; records the same; now we need to check whether any of
		 ;; their fields differ and if so, report it.
		 (if (not (equal record1 record2))
		     (in-buffer discrep
		       (print-compare-records record1 record2 db1))))))))


;;; db-two-dbs.el ends here
