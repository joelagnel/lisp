(defun incr-region (beg end cnt)
  "Increments all integers in a region.  Called interactively,
increments numbers by the prefix argument (which can be negative).
From a program, expects BEGIN END COUNT."
  (interactive "r\np")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (re-search-forward "[0-9]+" nil t)
	(replace-match (int-to-string
			(+ cnt
			   (string-to-int (buffer-substring
					 (match-beginning 0)
					 (match-end 0))))))
	))))
