
(defun read-only-if-symlink () 
  (if (file-symlink-p buffer-file-name)
      (progn
	(setq buffer-read-only t)
	(message "File is a symlink"))))


(add-hook 'find-file-hooks 
	  '(lambda ()
	     (message "This is a hacker's file")))


	    