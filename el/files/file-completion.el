(defvar file-completion-insert-cwd t
  "*`t' if when completing a file name, and the file name doesn't
begin with a \"/\", should insert the current default directory.")

(defun unexpand-file-name (filename)
  (if (string-match (getenv "HOME") filename)
      (concat "~" 
	      (substring filename 
			 (length (getenv "HOME"))
			 (length filename)))
      filename))


(defun complete-file-name (&optional dir)
  "Completes filename before point.  If the filename does not begin
with a slash, the filename is assumed to be relative to the value of
`default-directory' (or optional argument DIR); if
`file-completion-insert-cwd' is non-`nil', then the directory is
inserted with the completed file name."
  (interactive)
  (let (start file new-file home insert-dir)
    (save-excursion
      (setq file (buffer-substring (save-excursion 
				     (if (re-search-backward "[^\\.+~a-z0-9@/_-]"
							     (point-min) t)
					 (setq start (1+ (point)))
					 (setq start (point-min))))
				   (point))
	    home (string-match "\\`~/" file)
	    new-file (file-name-completion
		      (file-name-nondirectory file)
		      (setq dir (cond ((string-match "\\`~?/" file)
				       (setq insert-dir (expand-file-name (file-name-directory file))))
				      (file-completion-insert-cwd
				       (setq insert-dir (concat (or (file-name-directory file) "")
								(or dir default-directory))))
				      (t (concat (or dir default-directory)
						 (or (setq insert-dir (file-name-directory file)) ""))))))))
    (if new-file
	(progn
	  (setq new-file (if home
			     (unexpand-file-name (concat insert-dir new-file))
			     (concat insert-dir new-file)))
	  (if (equal file new-file)
	      (display-file-completions (file-name-nondirectory file) dir)
	      (progn
		(delete-region start (point))
		(insert new-file))))
	(beep t))))

(defun display-file-completions (file dir)
  (message "Making completion list...")
  (save-window-excursion
    (with-output-to-temp-buffer " *Completions*"
      (display-completion-list 
       (sort (file-name-all-completions file dir)
	     'string<)))
    (momentary-string-display "" (point) 32 "Hit SPC to remove window.")))

