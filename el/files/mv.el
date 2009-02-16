

(defun mv (fname)
  (interactive "FNew filename: ")
  (let ((oldfname (buffer-file-name (current-buffer))))
    (write-file fname)
    (if (and oldfname 
	     (file-exists-p oldfname)
	     (not (string-equal (expand-file-name fname)
				(expand-file-name oldfname))))
	(delete-file oldfname))
    (if (not (file-exists-p fname))
	(write-file fname))))


