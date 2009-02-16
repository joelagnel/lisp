

(defun count-words-buffer ()
  (let ((count 0))
    (save-excursion
      (goto-char (point-min))
      (while (< (point) (point-max))
	(forward-word 1)
	(setq count (1+ count)))
      (message "buffer contains %d words." count))))

(count-words-buffer)


(defun just-one-space-in-current-line (&optional n)
 (interactive "*p")
 (save-excursion
   (save-restriction
     (narrow-to-region (line-beginning-position)
			(line-end-position))
     (goto-char (point-min))
     (while (re-search-forward "[ \t]" nil t)
	(just-one-space n)))))


;;convert camel cases to underscored stirngs
(defun camel-to-underline ()
  (interactive)
  (let (case-fold-search)
   (while (re-search-forward "[a-z][A-Z]")
       (insert "_")
           (downcase-region (point) (+ 1 (point))))))

