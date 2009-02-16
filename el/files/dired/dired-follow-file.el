(defun dired-follow-file ()
  "In dired, visit the file or directory on this line.
If a directory is on the current line, replace the current Dired buffer
with one containing the contents of the directory. Otherwise, invoke
`dired-find-file' on the file."
  (interactive)
  (let ((filename (dired-get-filename)))
    (if (file-directory-p filename)
        (find-alternate-file filename)
      (dired-find-file))))

(defun dired-setup-follow-file ()
  (substitute-key-definition
   'dired-find-file 'dired-follow-file dired-mode-map)
  (substitute-key-definition
   'dired-advertised-find-file 'dired-follow-file dired-mode-map))

(add-hook 'dired-mode-hook 'dired-setup-follow-file)

(provide 'dired-follow-file)