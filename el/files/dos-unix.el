 
(defun dos-unix ()
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\r" nil t) (replace-match "")))
(defun unix-dos ()
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\n" nil t) (replace-match "\r\n")))

;; M-x dos-unix on a file