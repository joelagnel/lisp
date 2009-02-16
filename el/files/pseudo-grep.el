;;;; pseudo-grep.el -- approximate replacement for grep, for machines which don't have it
;;; Time-stamp: <2006-03-27 10:26:07 jcgs>

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

(provide 'pseudo-grep)

(unless (fboundp 'file-name-extension)
  (defun file-name-extension (filename)
    "Return the extension of FILENAME, or nil if it has none."
    (if (and filename (string-match ".+\\.\\(.+\\)" filename))
	(substring filename (match-beginning 1) (match-end 1))
      nil)))

(defun pseudo-grep (filter-pattern directory files-pattern &optional number ignore-case)
  "A bit like grep.
Search lines matching FILTER-PATTERN in all files in DIRECTORY whose names match FILES-PATTERN.
The matching lines are displayed in a temporary buffer.
If optional NUMBER is given, number the resulting lines.
If optional IGNORE-CASE is given, make the search ignore case.
As a bonus over running grep in a separate process, the resulting
lines are returned in a list, each element of which is the filename,
the line number, and the line contents."
  ;; Written in frustration at not getting "M-! grep ..." running
  ;; properly on a Windows box! I thought it would be quicker to write
  ;; it in elisp than sort out the Windows stuff. I was probably
  ;; right; it was about half an hour, I think.
  (interactive
   (let* ((filter-pattern (read-from-minibuffer "Grep for: "))
	  (directory (read-file-name "Grep through files in directory: "))
	  (this-file-type
	   (and (stringp buffer-file-name)
		(file-name-extension buffer-file-name)))
	  (files-pattern (read-from-minibuffer "Grep files matching: "
					       (if this-file-type
						   (format "\\.%s$" this-file-type)
						 nil)))
	  (number (y-or-n-p "Show line numbers? "))
	  (ignore-case (y-or-n-p "Ignore case? ")))
     (list filter-pattern (if (file-directory-p directory)
			      directory
			    (file-name-directory directory))
	   files-pattern number ignore-case)))
  (let* ((files (directory-files directory t files-pattern))
	 (visits (mapcar 'find-buffer-visiting files))
	 (max-line-number-width (if number
				    (ceiling
				     (log
				      (apply 'max
					     (mapcar (function
						      (lambda (file)
							;; find them this way, to avoid churning up the buffer list
							(set-buffer (find-file-noselect file))
							(save-restriction
							  (widen)
							  (count-lines (point-min) (point-max)))))
						     files))
				      10))
				  0))
	 (case-fold-search ignore-case)
	 (max-name-length (apply 'max (mapcar 'length (mapcar 'file-name-nondirectory files))))
	 (format-string (format (if number
				    "%%%ds: %%%dd: %%s\n"
				  "%%%ds: %%s\n")
				max-name-length max-line-number-width))
	 (lines nil))
    (message "format string is %s" format-string)
    (with-output-to-temp-buffer "*Pseudo-grep output*"
      (while files
	(let ((line-no 0)
	      (short-name (file-name-nondirectory (car files))))
	  ;; find them this way, to avoid churning up the buffer list
	  (set-buffer (find-file-noselect (car files)))
	  (message "Grepping %s" (car files))
	  (save-excursion
	    (save-restriction
	      (widen)
	      (goto-char (point-min))
	      (while (not (eobp))
		(when (re-search-forward filter-pattern (line-end-position) t)
		  (let ((line (buffer-substring-no-properties (line-beginning-position)
							      (line-end-position))))
		    (princ (if number
			       (format format-string
				       short-name
				       line-no
				       line)
			     (format format-string
				     short-name
				     line)))
		    (setq lines (cons (list (car files) line-no line) lines))))
		(forward-line 2)
		(setq line-no (1+ line-no)))))
	  (unless (car visits) (kill-buffer nil)))
	(setq files (cdr files)
	      visits (cdr visits))))
    (nreverse lines)))

;;; end of pseudo-grep.el
