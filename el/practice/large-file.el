(defun view-large-file-contents (file beg end)
  "View FILE contents from bytes BEG through END, in View mode."
  (interactive "fView file: \nnFrom byte: \nnTo byte: ")
  (switch-to-buffer
   (generate-new-buffer (format "%s[%d,%d]"
				(file-name-nondirectory file) beg end)))
  (insert-file-contents file nil beg end)
  (view-mode 1))


(setenv "PATH" (mapconcat (lambda (dir) (or dir ".")) exec-path ":"))