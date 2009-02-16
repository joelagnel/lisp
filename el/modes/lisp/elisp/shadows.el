;;; shadows.el --- find shadowed emacs lisp files

;; Copyright (C) 1996 by Michelangelo Grigni
;; Author: Michelangelo Grigni <mic@caribou>
;; Version: 0.00
;; Created: Mar  6 1996
;; Keywords: maint, lisp

;; <<<This file could be part of GNU Emacs.>>>

;;; Commentary:
;;
;; The basic goal here is to search a path, and report files that are
;; multiply defined on that path.  An example application is to help
;; root out old versions of files from your Emacs `load-path'.
;;
;; The function `shadows' does the actual work, the two commands
;; `shadows-emacs' and `shadows-exec' are example applications.

;;; History:
;;
;; Several variations of this idea appeared in a thread on
;; gnu.emacs.sources in December 1995, under the Subject:
;;
;;     Find shadowing elisp files...
;;
;; Contributors to the thread included joseph@cis.ohio-state.edu
;; (Sudish Joseph), pot@cnuce.cnr.it (Francesco Potorti`), and
;; terry@santafe.edu (Terry Jones).
;;
;; This version is my own attempt, which includes the idea of Joseph
;; to avoid reporting shadows due to symlink directories.  This
;; version uses hash tables for speed.


;;; Code:

(defun shadows-emacs nil
  "Report shadows on the Emacs `load-path'."
  (interactive) (shadows load-path "\\.elc?\\'" 0))

(defun shadows-exec nil
  "Report shadows on `exec-path'."
  (interactive) (shadows exec-path "\\`[^.]"))

(defsubst shadows-add-hash (table key value &optional del)
  "In TABLE, rebind KEY to (VALUE . old-list), and return old-list.
Here old-list was the old binding, or nil.  If optional DEL is
non-nil, first delete any previous copy of VALUE from oldlist."
  (let* ((sym (intern key table))
	 (old (and (boundp sym) (symbol-value sym))))
    (cdr (set sym (cons value (if del (delete value old) old))))))

(defun shadows (path  &optional match subexp)
  "Report filenames that appear multiple times in PATH.
Optional regexp MATCH restricts the filenames considered.
Optional SUBEXP specifies a subexpression of MATCH which is deleted
to derive a basename from the filename \(0 for the whole pattern\).
The resulting basenames are then used for name collision detection.
Note: shadows due to symlink-equivalent directories are not reported."
  ;; Good sizes are one less than a power of two:
  (let ((basename-table (make-vector 1023 0))
	(truedir-table (make-vector 63 0)))
    (mapcar
     (lambda (dir)
       (setq dir (file-name-as-directory dir))
       (cond
	((setq old (shadows-add-hash
		    truedir-table
		    (file-truename dir)
		    dir))
	 (message "Skipping %s: same as %s\n" dir (car old)))
	((not (file-accessible-directory-p dir))
	 (message "Skipping %s: inaccessible\n" dir))
	(t
	 (let ((files (directory-files dir nil match t)))
	   (if (not files)
	       (message "Skipping %s: no files match \"%s\"\n" dir match)
	     (message "Scanning %s: %d files" dir (length files)))
	   (mapcar
	    (lambda (file)
	      (shadows-add-hash basename-table
				(if subexp
				    (progn
				      (string-match match file)
				      (replace-match "" nil nil file subexp))
				  file)
				dir t))
	    files))
	 )))
     path)
    (message "Scanning: done.")
    ;; Print:
    ;; Ok, now make a report to a temporary buffer:
    (with-output-to-temp-buffer "*Shadows*"
      (princ (format "%sNAME    DIRS ...\n" (if subexp "BASE" "FILE")))
      (mapatoms
       (lambda (sym)
	 (let ((dirs (symbol-value sym)))
	   (if (cdr dirs)		; more than one
	       (progn
		 (princ (format "%-11s" (symbol-name sym)))
		 (mapcar (lambda (dir) (princ " ") (princ dir)) (reverse dirs))
		 (terpri)))))
       basename-table)
      ;; Sort the output buffer: sort-fold-case?
      (save-excursion
	(set-buffer standard-output)
	(goto-char (point-min))
	(forward-line 1)
	(sort-lines nil (point) (point-max))))))

;;; shadows.el ends here
