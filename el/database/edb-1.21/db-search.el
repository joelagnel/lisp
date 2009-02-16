;;; db-search.el --- part of EDB, the Emacs database

;; See database.el for copyright notice, distribution conditions, etc.

;; Author: Michael Ernst <mernst@theory.lcs.mit.edu>
;; Keywords: EDB

;;; Commentary:

;;; Code:


(provide 'db-search)

;; db-interfa should have been loaded before this.


(deflocalvar dbf-field-search-defaults nil
  "Vector of defaults for field search.
It is one element longer than the number of fields; the element indexed by
dbf-displayspecs-length is the default for a search over all fields.")

;;   "Keymap for database data display buffer in edit mode."
(defvar database-search-mode-map (copy-keymap database-edit-mode-map))




;; Requisite changes:
;;
;;
;; change dbf-displayspecs:
;;   * displayspec->printed-rep and display->internal
;;   * constraint functions
;;   * add "any field" field.
;;
;; increment dbf-displayspecs-length by 1 to account for "any field" searches.
;;
;; change mode line to reflect that a search is going on.
;;
;; disable or rebind such keys as db-view-mode, db-next-record, etc.
;;
;; Make sure that dbf-this-record is set, never dbf-this-record-original.
;; (Actually, I take it back; the latter can point at the search record.  In
;; addition there will be another search record which remembers defaults from
;; searches on individual fields.)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Searching primitives
;;;

;; These are listed in order of precedence.
;; Perhaps add <= and >=.
(defvar dbm-and-connective "[ \t]+and[ \t]+")
(defvar dbm-or-connective "[ \t]+or[ \t]+")
(defvar dbm-not-prefix "^[ \t]*not[ \t]+")
;; These used to be preceded by [ \t]* and followed by [ \t]+;
;; for instance, (defvar dbm-<-prefix "^[ =t]*<[ \t]+")
(defvar dbm-<-prefix "^<")
(defvar dbm->-prefix "^>")
(defvar dbm-=-prefix "^=")

;; Perhaps demand that after >,<,=, there is a pure string, and don't make
;; the full recursive call.
;;;###autoload
(defun db-parse-match-pattern (string displayspec)
  (cond ((string-match dbm-and-connective string)
	 (let ((and-end (match-end 0)))
	   (list 'db-match-and
		 (db-parse-match-pattern (substring string 0 (match-beginning 0))
					 displayspec)
		 (db-parse-match-pattern (substring string and-end)
					 displayspec))))
	((string-match dbm-or-connective string)
	 (let ((or-end (match-end 0)))
	   (list 'db-match-or
		 (db-parse-match-pattern (substring string 0 (match-beginning 0))
					 displayspec)
		 (db-parse-match-pattern (substring string or-end)
					 displayspec))))
	;; no infix connectives in string
	((string-match dbm-not-prefix string)
	 (list 'db-match-not
	       (db-parse-match-pattern (substring string (match-end 0))
				       displayspec)))
	((string-match dbm-<-prefix string)
	 (list 'db-match-<
	       (db-parse-match-object (substring string (match-end 0))
				       displayspec)))
	((string-match dbm->-prefix string)
	 (list 'db-match->
	       (db-parse-match-object (substring string (match-end 0))
				       displayspec)))
	((string-match dbm-=-prefix string)
	 (list 'db-match-=
	       (db-parse-match-object (substring string (match-end 0))
				       displayspec)))
	(t
	 (display->actual-call
	  (or (displayspec-match-display->actual displayspec)
	      (displayspec-display->actual displayspec))
	  string
	  nil nil nil))))

(defun db-parse-match-object (string displayspec)
  (db-debug-message "db-parse-match-object %s." string)
  (display->actual-call
   (displayspec-display->actual displayspec)
   string
   nil nil nil))

;;;###autoload
(defun db-print-match-pattern (pattern displayspec)
  (let ((pat-type (if (listp pattern) (car pattern))))
    (cond
     ((eq 'db-match-and pat-type)
      (concat (db-print-match-pattern (car (cdr pattern)) displayspec)
	      ;; extra space to emphasize its low precedence
	      "  AND  "
	      (db-print-match-pattern (car (cdr (cdr pattern))) displayspec)))
     ((eq 'db-match-or pat-type)
      (concat (db-print-match-pattern (car (cdr pattern)) displayspec)
	      " OR "
	      (db-print-match-pattern (car (cdr (cdr pattern))) displayspec)))
     ((eq 'db-match-not pat-type)
      (concat "NOT "
	      (db-print-match-pattern (car (cdr pattern)) displayspec)))
     ((eq 'db-match-< pat-type)
      (concat "< "
	      (db-print-match-object (car (cdr pattern)) displayspec)))
     ((eq 'db-match-> pat-type)
      (concat "> "
	      (db-print-match-object (car (cdr pattern)) displayspec)))
     ((eq 'db-match-= pat-type)
      (concat "= "
	      (db-print-match-object (car (cdr pattern)) displayspec)))
     ;; pattern was not a list or the type wasn't recognized
     (t
      (actual->display-call (or (displayspec-match-actual->display displayspec)
				(displayspec-actual->display displayspec))
			    pattern
			    nil nil)))))

(defun db-print-match-object (string displayspec)
  (db-debug-message "db-print-match-object %s." string)
  (actual->display-call (displayspec-actual->display displayspec)
			string
			nil nil))

;;;###autoload
(defun db-match (pattern target recordfieldspec)
  (db-debug-message "db-match: %s %s" pattern target)
  (if (listp pattern)
      (let ((pat-type (car pattern)))
	(cond
	 ((eq 'db-match-and pat-type)
	  (and (db-match (car (cdr pattern)) target recordfieldspec)
	       (db-match (car (cdr (cdr pattern))) target recordfieldspec)))
	 ((eq 'db-match-or pat-type)
	  (or (db-match (car (cdr pattern)) target recordfieldspec)
	      (db-match (car (cdr (cdr pattern))) target recordfieldspec)))
	 ((eq 'db-match-not pat-type)
	  (not (db-match (car (cdr pattern)) target recordfieldspec)))
	 ((eq 'db-match-< pat-type)
	  (= 1 (funcall (recordfieldspec-order-function recordfieldspec)
			(car (cdr pattern)) target)))
	 ((eq 'db-match-> pat-type)
	  (= -1 (funcall (recordfieldspec-order-function recordfieldspec)
			 (car (cdr pattern)) target)))
	 ((eq 'db-match-= pat-type)
	  (= 0 (funcall (recordfieldspec-order-function recordfieldspec)
			(car (cdr pattern)) target)))
	 (t
	  (funcall (recordfieldspec-match-function recordfieldspec)
		   pattern target))))
    (funcall (recordfieldspec-match-function recordfieldspec)
	     pattern target)))

;;; db-search.el ends here
