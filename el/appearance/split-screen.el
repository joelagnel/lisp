;;; Time-stamp: <2005-05-29 22:02:40 jcgs>
;;; old time stamp: <93/02/03 12:44:47 john>

;;;###autoload
(defun switch-to-buffer-other-window-beside (buffer)
  "Like switch-to-buffer-other-window, but splits using
split-window-horizontally if necessary."
  (interactive "BSwitch to buffer in other window: ")
  (if (one-window-p)
      (split-window-horizontally))
  (let ((pop-up-windows t))
    (pop-to-buffer buffer t)))

;;;###autoload
(defun find-file-other-window-beside (filename)
  "Like find-file-other-window , but splits using
split-window-horizontally if necessary."
  (interactive "FFind file in other window: ")
  (if (one-window-p)
      (split-window-horizontally))
  (switch-to-buffer-other-window (find-file-noselect filename)))

;;;###autoload
(defun find-tag-other-window-beside (tagname &optional next)
  "Like find-tag-other-window, but splits using self-insert-command if
necessary."
  (interactive (if current-prefix-arg
		   '(nil t)
		 (let* ((default (find-tag-default))
			(spec (read-string
			       (if default
				   (format
				    "Find tag other window: (default %s) "
				    default)
				 "Find tag other window: "))))
		   (list (if (equal spec "")
			     default
			   spec)))))
  (if (one-window-p)
      (split-window-horizontally))
  (find-tag-other-window tagname next))

;;;###autoload
(defun rotate-split ()
  "Change from vertical to horizontal split of the screen."
  (interactive)
  (delete-other-windows)
  (split-window-horizontally 40)
  (switch-to-buffer-other-window (other-buffer (current-buffer))))

;;; end of split-screen.el
