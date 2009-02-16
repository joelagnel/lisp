;;;; edit-tree.el -- apply editing commands to all files in a tree
;;; Time-stamp: <2005-07-06 18:23:38 jcgs>

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


(provide 'edit-tree)

;;;###autoload
(defun apply-command-to-tree (tree pattern command args)
  "To all files in TREE whose names match PATTERN, apply COMMAND with ARGS.
If the file was already in a buffer, keep the buffer afterwards; otherwise,
destroy the buffer."
  (message "In (apply-command-to-tree %S %S %S %S)" tree pattern command args)
  (save-window-excursion
    (dolist (file (directory-files tree t))
      (message "%s?" file)
      (cond
       ((and (file-directory-p file)
	     (not (string-match "\\.\\.?$" file)))
	(message "%s is a directory, recursing" file)
	(apply-command-to-tree file pattern command args))
       ((string-match pattern file)
	(message "%s matches %s, applying command" file pattern)
	(let ((was-visiting (find-buffer-visiting file)))
	  (find-file file)
	  (save-excursion
	    (goto-char (point-min))
	    (message "Doing (%S %S)" command args)
	    (apply command args)
	    (basic-save-buffer))
	  (unless was-visiting
	    (kill-buffer nil))))
       (t
	(message "skipping %s" file)
	nil)))))

;;;###autoload
(defun query-replace-tree (tree pattern &rest qr-args)
  "To files in TREE that match PATTERN, query-replace OLD with NEW."
  (interactive
   (append
    (list (expand-file-name (read-file-name "Directory tree: "))
	  (read-from-minibuffer "Edit files matching (regexp): "))
    (query-replace-read-args "Query replace" nil)))
  (message "doing (apply-command-to-tree %S %S %S %S)"
	   'apply-command-to-tree tree pattern 'query-replace qr-args)
  (apply-command-to-tree tree pattern 'query-replace qr-args))

;;;###autoload

(defun replace-string-in-tree (old new tree &optional matching)
  "Replace all occurrences of OLD by NEW in files in directory TREE that optionally match MATCHING."
  (interactive "sReplace: 
sReplace %s with: 
DReplace %s with %s in directory: 
sDo it in files matching: ")
  (let ((files (directory-files tree t nil t)))
    (while files
      (let ((file (car files)))
	(if (file-directory-p file)
	    (if (not (string-match "\\.?\\.$" file))
		(replace-string-in-tree old new file))
	  (if (or (null matching)
		  (string-match matching file))
	      (save-window-excursion
		(let ((done 0))
		  (message "Processing %s" file)
		  (find-file file)
		  (save-excursion
		    (goto-char (point-min))
		    (while (search-forward old nil t)
		      (setq done (1+ done))
		      (replace-match new nil t)))
		  (if (not (zerop done))
		      (message "%d in %s" done file))))
	    (message "%s does not match %s" file matching)
	    )))
      (setq files (cdr files)))))
