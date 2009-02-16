;;; msf-abbrev.el --- maintain abbrevs in a directory tree

;; Copyright (C) 2004,2005 Free Software Foundation, Inc.

;; Author: Benjamin Rutt <brutt@bloomington.in.us>
;; Version: 1.0beta3

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This package allows you to place your abbrevs into your filesystem,
;; in a special directory tree.  More information and a demo available at
;; http://www.bloomington.in.us/~brutt/msf-abbrev.html

(require 'cl)

;; xemacs compat
(unless (boundp 'undo-in-progress)
  (defvar undo-in-progress nil
   "Placeholder defvar from msf-abbrev package.")
  (defadvice undo-more (around msf-abbrev-undo-more activate)
     (let ((undo-in-progress t)) (ad-do-it))))

(defgroup msf-abbrev nil
  "Load abbrevs from a filesystem tree."
  :group 'convenience)

;; begin user customizable vars
(defcustom msf-abbrev-root nil
  "*Root directory of user abbreviation files.

This directory should have subdirectories such as c-mode, lisp-mode, etc."
  :group 'msf-abbrev
  :type 'path)

(defcustom msf-abbrev-verbose nil
  "*Whether to be verbose for various msf-abbrev actions."
  :group 'msf-abbrev
  :type 'boolean)

(defcustom msf-abbrev-expand-function 'msf-abbrev-expand-function-default
  "*Which function should be called to expand a abbrev in a file.

The function should take one argument, the filename to expand.
This function will be used for all files except those with .el
extensions, which will be handled by the elisp interpreter
directly."
  :group 'msf-abbrev
  :type 'function)

(defcustom msf-abbrev-expand-hook nil
  "Hook called after expansion of an msf abbrev."
  :group 'msf-abbrev
  :type 'hook)

(defcustom msf-abbrev-indent-after-expansion nil
  "*Whether to indent the region inserted after the abbrev is expanded.

This is only relevant when the default expandsion function is
used (see `msf-abbrev-expand-function')."
  :group 'msf-abbrev
  :type 'boolean)
;; end of user customizable vars

(defvar msf-abbrev-fields-created 0)

;; begin inlined fld.el stuff
(defvar fld-last-group-id nil)
(defvar fld-id-to-group-id (make-hash-table))
(defvar fld-group-id-to-exit-point (make-hash-table))

(defvar fld-keymap (make-sparse-keymap))
(define-key fld-keymap (kbd "M-RET") 'fld-cleanup-form-at-point)
(define-key fld-keymap (kbd "TAB") 'fld-next)
(define-key fld-keymap (kbd "S-TAB") 'fld-prev)
(define-key fld-keymap (kbd "<S-iso-lefttab>") 'fld-prev)
(defvar fld-choose-keymap (copy-keymap fld-keymap))
(define-key fld-choose-keymap (kbd "RET") 'fld-choose)			       
(defvar fld-category-defaults nil)
(setq fld-category-defaults
      `(face highlight front-sticky t rear-sticky t
	     keymap ,fld-keymap))
(setplist 'fld-category fld-category-defaults)

(defvar fld-id-next 0)
(defun fld-nextid ()
  (setq fld-id-next (1+ fld-id-next))
  fld-id-next)

(defvar fld-group-id-next 0)
(defun fld-nextgroupid ()
  (setq fld-group-id-next (1+ fld-group-id-next))
  fld-group-id-next)

(defun fld-currgroupid ()
  (interactive)
  fld-group-id-next)

(defsubst fld-in ()
  (if (get-text-property (point) 'fld-id) t nil))

(defun fld-after ()
  (interactive)
  (and (not (fld-in))
       (not (bobp))
       (save-excursion
	 (forward-char -1)
	 (fld-in))))

(defun fld-id ()
  (assert (or (fld-in) (fld-after)))
  (save-excursion
    (when (fld-after)
      (fld-focus))
    (get-text-property (point) 'fld-id)))

(defun fld-group-id ()
  (interactive)
  (assert (or (fld-in) (fld-after)))
  (save-excursion
    (when (fld-after)
      (fld-focus))
    (get-text-property (point) 'fld-group-id)))

(defun fld-focus ()
  (assert (or (fld-in) (fld-after)))
  (when (fld-after)
    (forward-char -1)))

(defun fld-beginning ()
  (assert (or (fld-in) (fld-after)))
  (let ((pt nil)
	(thisid nil)
	(done nil))
    (save-excursion
      (fld-focus)
      (setq thisid (get-text-property (point) 'fld-id))
      (if (bobp)
	  (setq pt (point-min))
	(while (not done)
	  (if (eq thisid (get-text-property (point) 'fld-id))
	      (progn
		(forward-char -1)
		(when (eq (point) (point-min))
		  (setq pt (point-min))
		  (if (not (eq thisid (get-text-property (point) 'fld-id)))
		      (setq pt (1+ pt)))
		  (setq done t)))
	    (setq pt (1+ (point)))
	    (setq done t)))))
    pt))


(defun fld-end ()
  (assert (or (fld-in) (fld-after)))
  (if (fld-after)
      (point)
    (let ((pt nil))
      (save-excursion
	(setq pt (or (next-single-property-change (point) 'fld-id)
		     (point-max))))
      pt)))

(defun fld-cleanup-form-at-point ( )
  (interactive)
  (when (or (fld-in) (fld-after))
    (fld-cleanup (fld-group-id))))

(defun fld-cleanup (gid)
  (interactive)
  (fld-disable-monitoring)
  (setq fld-ressurection-id nil
	fld-ressurection-now nil
	fld-ressurection-pos nil
	fld-transition-to-typed-id nil
	fld-transition-to-typed-now nil)
  (let ((ids
	 (delq nil
	       (mapcar
		(lambda (ls)
		  (if (eq gid (caddr ls))
		      (car ls)
		    nil))
		(fld-list-in-buffer)))))
    (mapc
     (lambda (id)
       (remhash id fld-id-to-group-id))
     ids)
    (remhash gid fld-group-id-to-exit-point)
    (save-excursion
      (mapcar
       (lambda (id_point_groupid)
	 (when (member (car id_point_groupid) ids)
	   (goto-char (cadr id_point_groupid))
	   (when (get-text-property (point) 'fld-choices)
	     (remove-text-properties
	      (point) (fld-end)
	      '(keymap nil fld-choices nil)))
	   (remove-text-properties
	    (point) (fld-end)
	    '(category nil fld-id nil fld-group-id nil fld-state nil))))
       (fld-list-in-buffer))))
  (fld-enable-monitoring))

(defun fld-find-next-startpos-same-group ()
  (assert (or (fld-in) (fld-after)))
  (let ((done nil)
	(tmp nil)
	(result nil)
	(gid (fld-group-id)))
    (save-excursion
      (goto-char (fld-end))
      (when (and (fld-in)
		 (eq gid (fld-group-id)))
	(setq done t
	      result (point))))
    (if (not done)
	(save-excursion
	  (while (not done)
	    (setq tmp (next-single-property-change (fld-end) 'fld-id))
	    (if (not tmp)
		(setq done t)
	      (if (eq gid (get-text-property tmp 'fld-group-id))
		  (setq done t
			result tmp)
		(goto-char tmp))))))
    result))

(defun fld-find-prev-startpos-same-group ()
  (assert (or (fld-in) (fld-after)))
  (let ((done nil)
	(tmp nil)
	(result nil)
	(gid (fld-group-id)))
    (save-excursion
      (while (not done)
	(setq tmp (previous-single-property-change (fld-beginning) 'fld-id))
	(if (not tmp)
	    (setq done t)
	  (goto-char tmp)
	  (fld-focus)
	  (if (eq gid (get-text-property (point) 'fld-group-id))
	      (setq done t
		    result (fld-beginning))))))
    result))

(defun fld-next ()
  (interactive)
  (assert (or (fld-in) (fld-after)))
  (let ((next (fld-find-next-startpos-same-group))
	(loc nil)
	(id nil)
	(gid nil))
    (if next
	(goto-char next)
      (save-excursion
	(fld-focus)
	(setq gid (get-text-property (point) 'fld-group-id))
	(setq loc (gethash gid fld-group-id-to-exit-point)))
      (when loc
	;; cleanup form, and go to departure location
	(fld-cleanup gid)
	(goto-char loc)))))

(defun fld-prev ()
  (interactive)
  (assert (or (fld-in) (fld-after)))
  (let ((prev (fld-find-prev-startpos-same-group)))
    (when prev
      (goto-char prev))))

(defun fld-make (text group-id)
  (let* ((id (fld-nextid)))
    (puthash id group-id fld-id-to-group-id)
    (add-text-properties 0 (length text)
			 `(category fld-category
				    fld-id ,id
				    fld-group-id ,group-id
				    fld-state untyped)
			 text)
    text))

(defun fld-choose-1 (prompt choices-list existing-choice)
  "Choose an item from a list."
  (let* (i map done o choice-index choice-info out)
    (setq map (make-sparse-keymap))
    (setq i 0)
    (setq choice-index 0)
    (kill-buffer (get-buffer-create " fld-choose"))
    (with-current-buffer (get-buffer-create " fld-choose")
      (erase-buffer)
      (insert "Make your choice, C-g aborts:\n\n")
      (mapc
       (lambda (c)
	 (insert c)
	 (setq choice-info (cons (cons i (list (line-number-at-pos)
						       (length c)))
					 choice-info))
	 (when (equal existing-choice c)
	   (setq choice-index i))
	 (setq i (1+ i))
	 (insert "\n"))
       choices-list)
      (setq choice-info (reverse choice-info))
      (goto-char (point-min))
      (forward-line 2)
      (forward-line choice-index)
      (setq o (make-overlay (line-beginning-position)
			    (+ (line-beginning-position)
			       (cadr (cdr (assoc choice-index choice-info))))))
      (overlay-put o 'face 'bold-italic))
    (save-window-excursion
      (delete-other-windows)
      (let ((buf (get-buffer " fld-choose")))
	(fit-window-to-buffer (display-buffer buf))
	(while (not done)
	  (let (
;; 		(cursor-in-echo-area t)
		(keys nil))
	    (setq keys (read-key-sequence-vector prompt))
;; 	    (message "keys are %s" keys)
	    (cond
	     ((equal keys [up])
	      (when (> choice-index 0)
		(set-buffer buf)
		(setq choice-index (1- choice-index))
		(goto-line (car (cdr (assoc choice-index choice-info))))
		(move-overlay o
			      (line-beginning-position)
			      (+ (line-beginning-position)
				 (cadr (cdr (assoc choice-index
						  choice-info)))))))
	     ((equal keys [down])
	      (when (< choice-index (1- i))
		(set-buffer buf)
		(setq choice-index (1+ choice-index))
		(goto-line (car (cdr (assoc choice-index choice-info))))
		(move-overlay o
			      (line-beginning-position)
			      (+ (line-beginning-position)
				 (cadr (cdr (assoc choice-index
						  choice-info)))))))
	     ((equal keys [13])
	      (setq out (nth choice-index choices-list)
		    done t))
	     ((equal keys [7])
	      (setq done t))
	     (t nil))))))
    (message "")
    out))

;; (fld-choose-1 "Pick a letter" '("a" "b" "cabbie") "b")

(defun fld-choose ()
  (interactive)
  (assert (or (fld-in) (fld-after)))
  (let ((choices nil)
	(choice nil)
	(existing-choice nil))
    (save-excursion
      (fld-focus)
      (setq choices (get-text-property (point) 'fld-choices))
      (setq existing-choice (buffer-substring-no-properties (fld-beginning)
							    (fld-end))))
    (setq choice (fld-choose-1 "choose one" choices existing-choice))
    (when (member choice choices)
;;       (message "replacing with %s" choice)
      (save-excursion
	(let ((props nil)
	      (oldpt nil)
	      (inhibit-modification-hooks t))
	(fld-focus)
	(goto-char (fld-beginning))
	(setq props (text-properties-at (point)))
	(delete-region (fld-beginning) (fld-end))
	(setq oldpt (point))
	(insert choice)
	(add-text-properties oldpt (point) props))))))

(defun fld-insert-choice (default choices group-id)
  (interactive)
  (let ((old-pt (point)))
    (fld-insert default group-id)
    (add-text-properties old-pt (point)
			 `(fld-choices
			   ,(mapcar
			     (lambda (x)
			       (set-text-properties 0 (length x) nil x)
			       x) choices)))
    (put-text-property old-pt (point) 'keymap fld-choose-keymap)))

(defun fld-list-in-buffer ()
  ;; return list of 3-tuples:  (id starting-position group-id)
  (let ((out nil)
	(pt nil)
	(flds (make-hash-table)))
    (save-excursion
      (goto-char (point-min))
      (when (fld-in)
	(puthash (fld-id) (list (point) (fld-group-id)) flds)
	(goto-char (fld-end)))
      (while (not (eobp))
	(if (fld-in)
	    (progn
	      (puthash (fld-id) (list (point) (fld-group-id)) flds)
	      (goto-char (fld-end)))
	  (setq pt (next-single-property-change (point) 'fld-id))
	  (if (not pt)
	      (goto-char (point-max))
	    (goto-char pt)
	    (puthash (fld-id) (list (point) (fld-group-id)) flds)
	    (goto-char (fld-end))))))
    (maphash
     (lambda (k v)
       (setq out (cons (cons k v) out)))
     flds)
    ;; (message "lsout: %s" (reverse out))
    (reverse out)))

(defun fld-kill-replaced-regions ()
  (let ((inhibit-modification-hooks t))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
	(if (get-text-property (point) 'fld-about-to-be-replaced)
	    (delete-region (point) (1+ (point))) 
	  (forward-char 1))))))

(defvar fld-detect-before nil)
(defvar fld-ressurection-pos nil)
(defvar fld-ressurection-id nil)
(defvar fld-ressurection-now nil)
(defvar fld-transition-to-typed-now nil)
(defvar fld-transition-to-typed-id nil)
(defvar fld-transition-to-typed-gid nil)
(defvar fld-during-save nil)
(defun fld-detect-pre (beg end)
  (if undo-in-progress
      nil
    (if (eq beg end) ;; insertion
	(progn
	  (cond
	   ((or (fld-in) (fld-after))
	    (save-excursion
	      (fld-focus)
	      (when (and (eq (get-text-property (point) 'fld-state) 'untyped)
			 (not fld-during-save))	;; avoid
		;; require-final-newline
		;; corner case
		(setq fld-transition-to-typed-id
		      (get-text-property (point) 'fld-id)
		      fld-transition-to-typed-gid
		      (get-text-property (point) 'fld-group-id)
		      fld-transition-to-typed-now t)
		(add-text-properties
		 (fld-beginning) (fld-end)
		 '(fld-about-to-be-replaced t)))))
	   ((eq (point) fld-ressurection-pos)
	    (setq fld-ressurection-now t))
	   (t nil)))
      ;; deletion
      (setq fld-detect-before (fld-list-in-buffer)))))
(defun fld-detect-post (beg end len)
;;   (if
;;       nil
;;       ;; (> beg end)
;;       (message "fld-detect-post: beg < end ?  %s < %s" beg end)
  (if undo-in-progress
      nil
    (if (eq beg end)
	(progn
	  ;; deletion
	  (let ((flds-now (fld-list-in-buffer)))
	    (when (< (length flds-now) (length fld-detect-before))
;; 	      (message "yo, we lost fields: %s"
;; 		       (set-difference (mapcar 'car fld-detect-before)
;; 				       (mapcar 'car flds-now)))
	      (let ((ls (copy-sequence fld-detect-before))
		    (done nil)
		    (id nil))
		(while (and (not done) ls)
		  (if (eq (point) (cadr (car ls)))
		      (progn
			(setq done t)
			(setq id (car (car ls))))
		    (setq ls (cdr ls))))
		(if (not id)
		    (setq fld-ressurection-pos nil)
;; 		  (message "could ressurect id %s" id)
		  (setq fld-ressurection-pos (point))
		  (setq fld-ressurection-id id))))))
      ;; insertion
      (cond
       ((eq fld-transition-to-typed-now t)
	(setq fld-transition-to-typed-now nil)
	(add-text-properties
	 beg end
	 `(category fld-category
		    fld-id ,fld-transition-to-typed-id
		    fld-group-id ,fld-transition-to-typed-gid
		    fld-state typed))
	(remove-text-properties beg end
				'(fld-about-to-be-replaced nil))
	(fld-kill-replaced-regions))
       ((eq fld-ressurection-now t)
	(setq fld-ressurection-now nil)
	(let ((inhibit-modification-hooks t))
	  (add-text-properties
	   beg end
	   `(category fld-category
		      fld-id ,fld-ressurection-id
		      fld-group-id ,(gethash fld-ressurection-id
					     fld-id-to-group-id)
		      fld-state typed))))
       (nil t)))))

(defun fld-before-save ()
  (setq fld-during-save t))
(defun fld-after-save ()
  (setq fld-during-save nil))

(defadvice yank (around fld-handle-yank activate)
  (let ((after nil)
;; 	(debug-on-error t)
	)
    (if (or (fld-in) (fld-after))
	(progn
	  (when (fld-after)
	    (setq after t))
	  (let ((inhibit-modification-hooks t)
		(yank-excluded-properties t)
		(old-point (point))
		(old-id (save-excursion (fld-focus) (fld-id))))
	    (when (eq (get-text-property (point) 'fld-state) 'untyped)
	      (add-text-properties
	       (fld-beginning) (fld-end)
	       '(fld-about-to-be-replaced t)))
	    ad-do-it
	    (add-text-properties
	     old-point (point)
	     `(category fld-category
			fld-id ,old-id
			fld-group-id ,(gethash old-id fld-id-to-group-id)
			fld-state typed))
	    (fld-kill-replaced-regions)))
      ad-do-it)))

;; faster yank advice?
(defadvice yank (after fld-handle-yank-after activate)
  (when mark-active
    (let ((fld-id-at-mark (get-text-property (mark) 'fld-id)))
      (if (or fld-id-at-mark
	      (next-single-property-change (mark) 'fld-id nil (point)))
	  (let ((pos (mark))
		(end (point)))
	    (while (< pos end)
	      (when (get-text-property pos 'fld-id)
		(add-text-properties pos (1+ pos)
				     '(category fld-category)))
	      (setq pos (1+ pos))))))))

;; slower yank advice
;; (defadvice yank (after fld-handle-yank-after activate)
;;   (let ((yanked (copy-sequence (car kill-ring)))
;; 	(s nil)
;; ;; 	(debug-on-error t)
;; 	(preserve-fld-props nil))
;;     (while (> (length yanked) 0)
;;       (setq s (substring yanked 0 1))
;;       (if (get-text-property 0 'fld-id s)
;; 	  (progn
;; 	    (setq preserve-fld-props t)
;; 	    (setq yanked ""))
;; 	(setq yanked (substring yanked 1))))
;;     (if preserve-fld-props
;; 	(let ((pos (mark))
;; 	      (end (point)))
;; 	  (while (< pos end)
;; 	    (when (get-text-property pos 'fld-id)
;; 	      (add-text-properties pos (1+ pos)
;; 				   '(category fld-category)))
;; 	    (setq pos (1+ pos)))))))

(defadvice expand-abbrev (around fld-handle-expand-abbrev activate)
  (let* ((from-fld (or (fld-in) (fld-after)))
	 (from-fld-id (and from-fld (fld-id)))
	 (from-fld-start (and from-fld (fld-beginning)))
	 (fields-before (fld-currgroupid)))
    ad-do-it
    (when from-fld
      (let ((inhibit-modification-hooks t))
	(if (eq (fld-currgroupid) fields-before) ;; didn't make new
	                                         ;; form
	    (add-text-properties
	     from-fld-start (point)
	     `(category fld-category
			fld-id ,from-fld-id
			fld-group-id ,(gethash from-fld-id fld-id-to-group-id)
			fld-state typed))
	  ;; made new form, assimilate into our group
	  (save-excursion
	    (let ((flds (fld-list-in-buffer)))
	      (mapc
	       (lambda (id_point_groupid)
		 (let ((id (car id_point_groupid))
		       (pt (cadr id_point_groupid))
		       (gid (caddr id_point_groupid)))
		   (when (eq gid fld-last-group-id)
		     (goto-char pt)
		     (puthash id (gethash from-fld-id fld-id-to-group-id)
			      fld-id-to-group-id)
		     (add-text-properties
		      (point) (fld-end)
		      `(fld-group-id
			,(gethash from-fld-id fld-id-to-group-id))))))
	       flds))))))))

(defadvice dabbrev-expand (around fld-handle-dabbrev-expand activate)
  (let* ((from-fld (or (fld-in) (fld-after)))
	 (orig-point (point))
	 (from-fld-start (and from-fld (fld-beginning)))
	 (from-fld-id (and from-fld (fld-id))))
    ad-do-it
    (when from-fld
      (add-text-properties
       from-fld-start (point)
       `(category fld-category
		  fld-id ,from-fld-id
		  fld-group-id ,(gethash from-fld-id fld-id-to-group-id)
		  fld-state typed)))))

(defun fld-enable-monitoring ()
  (add-hook 'before-change-functions 'fld-detect-pre nil t)
  (add-hook 'after-change-functions 'fld-detect-post nil t)
  (add-hook 'before-save-hook 'fld-before-save nil t)
  (add-hook 'after-save-hook 'fld-after-save nil t))

(defun fld-disable-monitoring ()
  (remove-hook 'before-change-functions 'fld-detect-pre t)
  (remove-hook 'after-change-functions 'fld-detect-post t)
  (remove-hook 'before-save-hook 'fld-before-save t)
  (remove-hook 'after-save-hook 'fld-after-save t))

(defun fld-insert (text group-id)
  (fld-disable-monitoring)
  (setq fld-last-group-id group-id)
  (let ((fld (fld-make text group-id)))
    (insert fld)))

(defun fld-set-exit-location (point-or-marker)
  (puthash fld-last-group-id point-or-marker fld-group-id-to-exit-point))

(defun fld-activate ()
  (fld-enable-monitoring))

(defun msf-abbrev-expand-function-default (file &optional transform-func)
  (let* ((orig-buffer (current-buffer))
	 (cursor-leave-point nil)
	 (insertion-point-begin (point-marker))
	 (insertion-point-end nil)
	 (trigger-line-opening-whitespace nil)
	 (text-expanded nil)
	 (work-buffer nil)
	 (fields-created-this-abbrev 0)
	 (gid (1+ (fld-currgroupid)))
	 (set-endpoint nil)
	 (first-field-marker nil))
    (setq text-expanded
	  (with-temp-buffer
	    (insert-file-contents file)
	    (buffer-substring-no-properties (point-min) (point-max))))

    ;; replace any <query "Loop iterator: ">-style snippets first before
    ;; insertion
    (setq text-expanded
	  (let ((query-alist nil))
	    (with-temp-buffer
	      (insert text-expanded)
	      (goto-char (point-min))
	      (while (re-search-forward "\\(<\\(QUERY\\|query\\) \"\\(.*?\\)\">\\)" nil t)
		(let ((beginpt (match-beginning 1))
		      (endpt (match-end 1))
		      (key (match-string 3)))
		  (when (not (assoc key query-alist))
		    (setq query-alist
			  (cons (list key (read-from-minibuffer key))
				query-alist)))
		  (goto-char beginpt)
		  (delete-region beginpt endpt)
		  (insert (cadr (assoc key query-alist)))))
	      (buffer-substring-no-properties (point-min) (point-max)))))

    ;; insert the text
    (insert text-expanded)
    (setq insertion-point-end (point-marker))
    (set-marker-insertion-type insertion-point-end t)

    ;; remove all props
    (set-text-properties insertion-point-begin
			 insertion-point-end
			 nil)

    (when transform-func
      (goto-char insertion-point-begin)
      (apply transform-func (list insertion-point-begin insertion-point-end)))

    ;; replace any <varlookup "user-full-name">-style snippets
    (goto-char insertion-point-begin)
    (while (re-search-forward "<\\(VARLOOKUP\\|varlookup\\) \"\\(.*?\\)\">"
			      insertion-point-end t)
      (let ((v (match-string 2)))
	(replace-match (eval (intern v)) nil t)))

    ;; replace any <ELISP "(insert "hi")">-style snippets
    (goto-char insertion-point-begin)
    (while (re-search-forward "<\\(ELISP\\|elisp\\) \"\\(.*?\\)\">" insertion-point-end t)
      (let ((v (match-string 2)))
	(replace-match "")
	(eval (read v))))

    ;; replace any <COMMENT "blah blah"> snippets
    (goto-char insertion-point-begin)
    (while (re-search-forward "^<\\(COMMENT\\|comment\\) \"\\(.*?\\)\">$" insertion-point-end t)
      (replace-match "")
      (let ((kill-ring-old kill-ring))
	(kill-line)
	(setq kill-ring kill-ring-old)))
    (goto-char insertion-point-begin)
    (while (re-search-forward "<\\(COMMENT\\|comment\\) \"\\(.*?\\)\">" insertion-point-end t)
      (replace-match ""))

    ;; calculate the whitespace on the beginning of the trigger line
    ;; and mimic it as a prefix throughout insertions
    (setq trigger-line-opening-whitespace
	  (save-excursion
	    (goto-char insertion-point-begin)
	    (let ((beg nil)
		  (end nil))
	      (beginning-of-line)
	      (setq beg (point))
	      (while (looking-at "[ \t]")
		(forward-char))
	      (setq end (point))
	      (buffer-substring beg end))))

    ;; expand any trigger-line opening whitespace on subsequent lines
    (goto-char insertion-point-begin)
    (forward-line 1)
    (while (< (point) insertion-point-end)
      (insert trigger-line-opening-whitespace)
      (forward-line 1))

    ;; position at end of insertion
    (goto-char insertion-point-end)

    (save-excursion
      (goto-char insertion-point-begin)
      (while (re-search-forward "<\\(FORMJUMP\\|field\\) \"\\(.*?\\)\">"
				(marker-position insertion-point-end) t)
	(let ((txt (match-string 2)))
	  (replace-match "" nil t)
	  (if (not first-field-marker)
	      (setq first-field-marker (point-marker)))
	  (fld-insert txt gid))
	(setq fields-created-this-abbrev
	      (1+ fields-created-this-abbrev))))

    ;; handle <choose><choice "OH"><choice "TX"></choose>
    (save-excursion
      (let ((choice-start nil)
	    (choice-stop nil)
	    (choices nil))
	(goto-char insertion-point-begin)
	(while (re-search-forward "<choose>"
				  (marker-position insertion-point-end) t)
	  (replace-match "" nil t)
	  (setq choice-start (point-marker))
	  (assert (re-search-forward "</choose>"
				     (marker-position insertion-point-end) t))
	  (replace-match "" nil t)
	  (setq choice-stop (point-marker))
	  (goto-char choice-start)
	  (if (or (not first-field-marker)
		  (< (point-marker) first-field-marker))
	      (setq first-field-marker (point-marker)))
	  (setq choices nil)
	  (while (re-search-forward "<choice \"\\(.*?\\)\">"
				    (marker-position choice-stop) t)
	    (setq choices (cons (match-string 1) choices))
	    (replace-match "" nil t))
	  (setq choices (reverse choices))
	  (fld-insert-choice (car choices) choices gid)
	  (setq fields-created-this-abbrev
	      (1+ fields-created-this-abbrev)))))

    (save-excursion
      (goto-char insertion-point-begin)
      (when (re-search-forward "<endpoint>"
			       (marker-position insertion-point-end) t)
	(replace-match "" nil t)
	(fld-set-exit-location (point-marker))
	(setq set-endpoint t)))
    
    (when (> fields-created-this-abbrev 0)
      (when (not set-endpoint)
	(fld-set-exit-location insertion-point-end)
	(setq set-endpoint t))
      (fld-nextgroupid) ;; only use the group id if we added some form fields
      
      (fld-activate)
      
      ;; leave cursor at first field location from this expansion
      (goto-char first-field-marker)
      (set-marker first-field-marker nil))
    
    (save-excursion
      (save-restriction
	(goto-char insertion-point-begin)
	(when (re-search-forward "<\\(CURSOR\\|cursor\\)>"
				 (marker-position insertion-point-end) t)
	  (replace-match "")
	  (setq cursor-leave-point (point)))))
    (when cursor-leave-point
      (goto-char cursor-leave-point))
    
    ;; possibly indent the expanded text
    (when msf-abbrev-indent-after-expansion
      (indent-region insertion-point-begin insertion-point-end))

    (setq msf-abbrev-fields-created
	  (+ msf-abbrev-fields-created fields-created-this-abbrev))))

(defun msf-abbrev-expand-file (file)
  (if (string-match "\\.el$" file)
      ;; if the abbrev is an .el file, just use elisp to evaluate it
      (let ((evalstr
	     (with-temp-buffer
	       (insert-file-contents file)
	       (buffer-substring (point-min) (point-max)))))
	(msf-abbrev-eval (read evalstr)))
    (let ((transform-func nil)
	  (transform-file (concat file "_")))
      (when (file-exists-p transform-file)
	(setq transform-func
	      (read
	       (with-temp-buffer
		 (insert-file-contents transform-file)
		 (buffer-substring (point-min) (point-max))))))
      (apply msf-abbrev-expand-function (list file transform-func))))
  (run-hooks 'msf-abbrev-expand-hook))

(defun msf-abbrev-directory-files (dir)
  (delq nil
	(mapcar
	 (lambda (x)
	   (let ((basenm (file-name-nondirectory x)))
	     (if (or (string-match "^\\." basenm)
		     (string-match "~$" basenm)
		     (string-match "_$" basenm))
		 nil x)))
	 (directory-files dir t))))

(defun msf-abbrev-report-if-verbose (abbr modename)
  (when msf-abbrev-verbose
    (message "defined abbrev %10s for mode %s" abbr modename)))

(defun msf-abbrev-eval (text)
;;   (message "about to eval %s" text)
  (eval text))

(defun msf-abbrev-locate-mode-dir (modename)
  (let ((cpls
	 (file-name-all-completions
	  (concat modename ".aliases.") msf-abbrev-root))
	(dest modename))
    (when cpls
      (assert (string-match "\\(.*\\)\\.aliases\\.\\(.*\\)" (car cpls)))
      (setq dest (match-string 2 (car cpls))))
    (concat (file-name-as-directory msf-abbrev-root) dest)))

(defmacro msf-abbrev-try-require (lib)
  `(let ((succeeded t))
     (condition-case err
         (require ,lib)
       (error (setq succeeded nil)))
     succeeded))
(defvar msf-abbrev-table nil)

(defun msf-abbrev-load ()
  "Load all abbrevs under `msf-abbrev-root'.

`msf-abbrev-root' should have subdirectories like c-mode,
c++-mode, cperl-mode. etc. each of which contain files whose
names will be used as abbreviations, expanding to the file's
contents.  The subdirectory `global' is special and loads up
global-abbrev-table, thus its abbrevs are active in every mode."
  (let* ((modedirs (msf-abbrev-directory-files msf-abbrev-root))
	 (sym nil)
	 (symstr nil)
	 (modename nil)
	 (fetchdir nil)
	 (method nil)
	 (hookname nil)
	 (abbrs-this-mode nil))
    (mapc
     (lambda (modedir)
       (setq modename (file-name-nondirectory modedir))
       (if (string-match "^\\(.*\\)\\.aliases\\.\\(.*\\)$" modename)
	   (setq fetchdir (match-string 2 modename)
		 modename (match-string 1 modename))
		 
	 (setq fetchdir modename))
       (setq abbrs-this-mode nil)
       (cond
	((and (boundp (setq sym (intern (concat modename "-abbrev-table"))))
	      (eval sym))
	 (setq method 'abbrev-table))
	((boundp (setq sym (intern (concat modename "-hook"))))
	 (setq method 'mode-hook))
	(t (progn
	     (message "WARNING from msf-abbrev.el:  no abbrev table %s-abbrev-table and no hook %s-hook, abbrevs for mode %s will not be loaded"
		      modename modename modename)
	     (setq method nil))))
       (setq symstr (symbol-name sym))
       (let ((abbrs (msf-abbrev-directory-files (concat (file-name-as-directory msf-abbrev-root) fetchdir)))
	     (abbr-sans-extension nil))
	 (mapc
	  (lambda (abbr)
	    (setq abbr-sans-extension
		  (file-name-sans-extension abbr))
	    (setq abbrs-this-mode
		  (cons (file-name-nondirectory abbr-sans-extension)
			abbrs-this-mode))
	    ;; define the new function
	    (let ((newfuncnm
		   (format "msf-abbrev-generatedfunc-%s-%s"
			   modename (file-name-nondirectory
				     abbr-sans-extension))))
	      ;; defun it
	      (msf-abbrev-eval
	       `(defun ,(intern newfuncnm) ()
		  (interactive)
		  (msf-abbrev-expand-file ,abbr)
		  'returning-nonnil-here-inhibits-self-insertion))

	      ;; add property to inhibit expansion of trigger (e.g. SPC)
	      (msf-abbrev-eval
	       `(put ',(intern newfuncnm) 'no-self-insert t))

	      (cond
	       ;; abbrev table method
	       ((eq method 'abbrev-table)
		(msf-abbrev-eval
		 `(define-abbrev ,sym ,(file-name-nondirectory
					abbr-sans-extension) ""
		    ',(intern newfuncnm)))
		(msf-abbrev-report-if-verbose
		 (file-name-nondirectory abbr-sans-extension) modename))

	       ;; mode hook method
	       ((eq method 'mode-hook)

		(let ((evtext
		       `(add-hook ',sym
				  (lambda ()
				    (define-abbrev local-abbrev-table
				      ,(file-name-nondirectory
					abbr-sans-extension) ""
				      ',(intern newfuncnm))))))
		  (msf-abbrev-eval evtext))
		(msf-abbrev-report-if-verbose
		 (file-name-nondirectory abbr-sans-extension) modename))

	       ;; do nothing if no <MODE>-abbrev-table or <MODE>-hook exists
	       (t nil))))

	  abbrs)
	 (add-to-list 'msf-abbrev-table
		      (list (file-name-nondirectory modedir)
			    (sort abbrs-this-mode 'string<)))))
     modedirs)))

(defun msf-abbrev-reload-after-save ()
  (let* ((bfn (expand-file-name (buffer-file-name)))
	 (root (expand-file-name msf-abbrev-root)))
    (when (string-match (concat "^" root) bfn)
      ;; we just saved an msf-abbrev file, so reload the tree
      (msf-abbrev-load))))
(add-hook 'after-save-hook 'msf-abbrev-reload-after-save)

(defun msf-abbrev-goto-root ()
  (interactive)
  (let ((current-mode-str (format "%s" major-mode)))
    (if (assoc current-mode-str msf-abbrev-table)
	  (dired (msf-abbrev-locate-mode-dir current-mode-str))
      (dired msf-abbrev-root))))

(defun msf-abbrev-define-new-abbrev-this-mode ()
  (interactive)
  (let* ((current-mode-str
	  (cond
	   ;; create an exception case for AUCTeX
	   ((and
	     (eq major-mode 'latex-mode)
	     (boundp 'AUCTeX-version))
	    "LaTeX-mode")
	   ((and
	     (eq major-mode 'tex-mode)
	     (boundp 'AUCTeX-version))
	    "TeX-mode")
	   (t (format "%s" major-mode))))
	 (d (msf-abbrev-locate-mode-dir current-mode-str)))
    (when (or (file-exists-p d)
	      (and (y-or-n-p
		    (format
		     "Could not find directory %s, create it? " d))
		   (progn
		     (make-directory d)
		     t)))
      (let ((name (read-from-minibuffer "Abbrev name: ")))
	(find-file (concat (file-name-as-directory d) name))))))

(defun msf-abbrev-abbrev-choose ()
  (interactive)
  (let ((tbl (assoc (format "%s" major-mode) msf-abbrev-table))
	(choice nil))
    (when tbl
      (setq choice (completing-read "Choose abbrev: " (cadr tbl) nil t))
      (insert choice)
      (expand-abbrev))))

(defun msf-abbrev-string-no-properties (str)
  (with-temp-buffer
    (insert str)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun msf-abbrev-abbrev-complete ()
  (interactive)
  (let ((tbl (assoc (format "%s" major-mode) msf-abbrev-table))
	(choice nil)
	(thing (thing-at-point 'word))
	(s nil)
	(result nil))
    (when (and thing tbl)
      (setq tbl (cadr tbl))
      (setq s (msf-abbrev-string-no-properties thing))
      (setq result (try-completion s tbl))
      (when result
	(cond
	 ((eq result t)
	  (delete-windows-on (get-buffer-create "*msf-abbrev completions*"))
	  (delete-region (- (point) (length s)) (point))
	  (insert result)
	  (expand-abbrev))
	 ((not (string= result s))
	  (delete-windows-on (get-buffer-create "*msf-abbrev completions*"))
	  (delete-region (- (point) (length s)) (point))
	  (insert result)
	  (when (member result tbl)
	    (expand-abbrev)))
	 (t
	  (with-output-to-temp-buffer "*msf-abbrev completions*"
	    (display-completion-list
	     (all-completions s tbl)))))))))

(provide 'msf-abbrev)
