;; danl-tramp-no-auto-save -- Turn off autosave for all tramp files
;; Author: Daniel Lundin <daniel@codefactory.se>
;; Created: Sat Jun 30 21:37:00 CEST 2001

;;; Code:
(defun danl-tramp-no-auto-save ()
    (when (tramp-tramp-file-p (buffer-file-name))
      (make-local-variable 'auto-save-default)
      (setq auto-save-default nil)
      (auto-save-mode -1)))

(add-hook 'find-file-hooks
	  'danl-tramp-no-auto-save)

(provide 'danl-tramp-no-auto-save)
;;; danl-tramp-no-auto-save.el ends here