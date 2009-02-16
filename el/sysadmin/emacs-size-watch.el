;;;; emacs-size-watch.el

(provide 'emacs-size-watch)

(defun ps-on-emacs ()
  "Run ps on the pid of this emacs"
  (interactive)
  (shell-command-to-string (format "ps  -vp %d" (emacs-pid))))

;;; end of emacs-size-watch.el

