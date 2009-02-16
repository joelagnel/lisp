
(defun my-buffer-filename () 
   "Return pretty printed buffer filename."
   (let ((filename (buffer-file-name)))
      (if filename
          (let ((homedir (getenv "HOME"))
                (pwd (concat (getenv "PWD") "/")))
               (cond ((and (> (length pwd) 0) (string-match pwd filename)) 
                      (substring filename (length pwd)))
                     ((and (string-match homedir filename) 
                           (= (string-match homedir filename) 0)) 
                      (concat "~" (substring filename (length homedir))))
                     (t filename)))
          (buffer-name))))


(defun my-save-message () 
   (message (concat "Saved " (number-to-string (buffer-size)) 
                    " bytes to " (my-buffer-filename) "...")))
(add-hook 'after-save-hook 'my-save-message)


