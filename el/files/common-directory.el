;;;; common-directory.el -- Special handling of my common directory
;; -*- emacs-lisp-mode -*-
;;; Time-stamp: <2006-04-21 19:05:37 jcgs>

;;  This program is free software; you can redistribute it and/or modify it
;;  under the terms of the GNU General Public License as published by the
;;  Free Software Foundation; either version 2 of the License, or (at your
;;  option) any later version.

;;  This program is distributed in the hope that it will be useful, but
;;  WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;  General Public License for more details.

;;  You should have received a copy of the GNU General Public License along
;;  with this program; if not, write to the Free Software Foundation, Inc.,
;;  59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

(provide 'common-directory)

(defun common-file-mark-deleted (file)
  "If FILE is in the directory $COMMON, make a corresponding entry in s:common:uncommon:"
  (when nil
    (let ((file-full-and-true (file-truename (expand-file-name file)))
	(common-full-and-true (file-truename (expand-file-name (getenv "COMMON")))))
    (when (string= (substring file-full-and-true 0 (length common-full-and-true))
		   common-full-and-true)
      (let ((find-file-not-found-hooks nil)
	    (find-file-hooks nil)
	    (file (expand-file-name
		   (substring file-full-and-true
			      (1+ (length common-full-and-true)))
		   (expand-file-name "uncommon"
				     (file-name-directory common-full-and-true)))))
	(make-directory (file-name-directory file) t)
	(save-window-excursion
	  (find-file file)
	  (insert "Deleted at " (current-time-string) "\n")
	  (basic-save-buffer)
	  (kill-buffer nil)))))))

;; (ad-define-subr-args 'delete-file '(filename))

;; (defadvice delete-file (after mark-if-in-common-directory activate)
;;   "Mark that the file has been deleted."
;;   (common-file-mark-deleted filename))

;; (ad-define-subr-args 'rename-file '(file newname &optional ok-if-already-exists))

;; (defadvice rename-file (after mark-if-in-common-directory activate)
;;   "Mark that the file is no longer in its old place."
;;   (common-file-mark-deleted file))

(defvar copy-of-pattern  "Copy\\( ([0-9]+)\\)? of \\(.+\\)$"
  "Pattern matching spurious copy files.")

(defun report-windows-copies (copy)
  "Internal function for scan-directory-for-windows-copies."
  (cond
   ((stringp copy)
    (princ (format "%s\n" copy)))
   ((consp copy)
    (mapc 'report-windows-copies copy))))

(defun get-timestamp (file)
  "Get the timestamp of FILE."
  (let* ((was-visiting (find-buffer-visiting file))
	 (time-stamp (save-window-excursion
		       (if was-visiting
			   (set-buffer was-visiting)
			 (find-file file))
		       (save-excursion
			 (if (zerop time-stamp-line-limit)
			     (goto-char (point-max))
			   (goto-line time-stamp-line-limit))
			 (if (re-search-backward time-stamp-start (point-min) t)
			     (let ((start (match-end 0)))
			       (if (re-search-forward time-stamp-end (point-max) t)
				   (let* ((end (point)))
				     (buffer-substring-no-properties start end))
				 nil))
			   nil))
		       )))
    (unless was-visiting
      (kill-buffer nil))
    time-stamp))

(defun deep-report-windows-copies (dir)
  "Compare \"Copy of\" file time-stamp strings recursively through DIR.
Ask the user about deleting any that are identical."
  (interactive "DReport on directory: ")
  (scan-directory-for-windows-copies dir 'deep-report-windows-copies-function))

(defun deep-report-windows-copies-function (copy)
  "Internals for deep-report-windows-copies."
  ;; (princ (format "Reporting on %S\n" copy))
  (cond
   ((stringp copy)
    (string-match copy-of-pattern copy)
    (let* ((which-copy (match-string-no-properties 1 copy))
	   (of-what (match-string-no-properties 2 copy))
	   (dir (file-name-directory copy))
	   (original (expand-file-name of-what dir))
	   (filename (file-name-nondirectory original))
	   (copy-time-stamp (get-timestamp copy))
	   (original-time-stamp (get-timestamp original))
	   )
      (if (string= original-time-stamp copy-time-stamp)
	  (progn
	    (princ (format "%s: identical\n" copy))
	    (when (yes-or-no-p (format "Delete identical %s? " copy))
		(delete-file copy))
	    )
      (princ (format "%s: original: %s; copy %s: %s\n"
		     original
		     original-time-stamp
		     (if which-copy
			 which-copy
		       "") copy-time-stamp)))))
   ((consp copy)
    (mapc 'deep-report-windows-copies-function copy))))

(defun scan-directory-for-windows-copies (dir &optional command)
  "Scan DIR reporting \"Copy of\" files.
When interactive, or optional COMMAND is given,
Apply optional COMMAND to each -- by default, a reporting function."
  (interactive "DScan directory: ")
  (let ((files (directory-files dir t))
	(copies nil))
    (while files
      (let ((file (car files)))
	(if (file-directory-p file)
	    (unless (string-match "\\.$" file)
	      (message "Subdir %s" file)
	      (setq copies
		    (cons (scan-directory-for-windows-copies file)
			  copies)))
	  (message "file %s" file)
	  (when (string-match copy-of-pattern file)
	    (message "a copy: %s" file)
	    (setq copies
		  (cons file
			copies)))))
      (setq files (cdr files)))
    (when (or command (interactive-p))
      (with-output-to-temp-buffer "*Copies*"
	(mapc (or command 'report-windows-copies) copies)))
    copies))

(defun file-name-library-root ()
  (interactive)
  (when minibuffer-completing-file-name
    (delete-region (point-at-bol) (point-at-eol))
    (let ((name (expand-file-name (getenv "GATHERED"))))
      (insert name)
      (unless (string-match "/$" name)
	(insert "/")))))

(defun file-name-common-root ()
  (interactive)
  (when minibuffer-completing-file-name
    (delete-region (point-at-bol) (point-at-eol))
    (let ((name (expand-file-name (getenv "COMMON"))))
      (insert name)
      (unless (string-match "/$" name)
	(insert "/")))))

(define-key minibuffer-local-completion-map "\M-^" 'file-name-library-root)
(define-key minibuffer-local-completion-map "\M-~" 'file-name-common-root)

;;; end of common-directory.el
