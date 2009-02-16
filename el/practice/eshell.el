;;; Eshell

(let ((path (split-string (getenv "PATH") ":")))
  (add-to-list 'path "/home/alex/bin")
  (setenv "PATH" (mapconcat 'identity path ":")))

(setq eshell-history-size 500)
(global-set-key (kbd "C-c s") 'eshell-here)
(global-set-key (kbd "C-z") 'eshell)

(defalias 'eshell/fg 'bury-buffer)

(defun eshell-here ()
  "Run eshell in the current directory."
  (interactive)
  (let ((dir default-directory))
    (eshell)
    (unless (string= default-directory dir)
      (message "Switching to %s" dir)
      (eshell/cd (list dir))
      (eshell-emit-prompt))))

(defun eshell/info (&rest args)
  (Info-directory)
  (Info-menu (car args)))

(defun eshell/cvs-examine (&rest ignore)
  (cvs-examine "." nil))

(add-hook 'eshell-mode-hook
	  '(lambda ()
	     (local-set-key (kbd "<up>") 'previous-line)
	     (local-set-key (kbd "<down>") 'next-line)
	     (local-set-key (kbd "C-c C-g") 'my-perl-find-error)))