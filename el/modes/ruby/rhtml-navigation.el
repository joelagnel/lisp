;; TODO -- need a license boiler-plate

;; Handy RHTML functions
;; (C) 2006 Phil Hagelberg

;; Ripped from the previous rhtml-mode, sorry about making it break
;; too :( -- pst

(defun rhtml-controller-name-from-view ()
  (concat (rails-root) 
	  "app/controllers/"
	   (file-name-nondirectory 
	    (expand-file-name "."))
	  "_controller.rb"))

(defun rhtml-find-action ()
  (interactive)
  (let ((action (file-name-sans-extension (file-name-nondirectory buffer-file-name))))
    (find-file (rhtml-controller-name-from-view))
    (beginning-of-buffer)
    (search-forward-regexp (concat "def\s" action))
    (recenter)))

(defun rinari-find-by-context ()
 (interactive)
 (mapc (lambda (rule) (if (string-match (car rule) (current-line))
		      (apply (cdr rule) (match-strings (current-line)))))
       ;; rules (warning; ALL matches will be acted upon, not just first!)
	'((":partial\s=>\s['\":]\\([a-zA-Z_]+\\)['\"\s]" . rhtml-find-partial)
	  (":controller\s=>\s['\":]\\([a-zA-Z_]+\\)['\"\s].*:action\s=>\s['\":]\\([a-zA-Z_]+\\)['\"\s]" . rinari-find-action)
	  (":action\s=>\s['\":]\\([a-zA-Z_]+\\)['\"\s]" . rinari-find-action))))

(defun rhtml-find-partial (partial)
  (interactive "MPartial: ")
  (find-file (concat "_" partial ".rhtml")))

(defun rhtml-find-helper (helper)
  (interactive (list (read-string (concat "Find helper (default " (thing-at-point 'symbol) "): ") 
				  nil nil (thing-at-point 'symbol))))
  (find-file (concat (rails-root) "app/helpers/" (file-name-nondirectory (expand-file-name ".")) "_helper.rb"))
  (unless (search-forward-regexp (concat "def\s" helper) nil t)
    (kill-buffer (current-buffer))
    (find-file (concat (rails-root) "app/helpers/application_helper.rb"))
    (unless (search-forward-regexp (concat "def\s" helper) nil t)
      (kill-buffer (current-buffer))))) ; todo - search controllers for helper_method-ified helpers

;; utility functions

(defun current-line ()
  (save-excursion
    (beginning-of-line)
    (set-mark-command nil)
    (end-of-line)
    (buffer-substring-no-properties (mark) (point))))

(defun match-strings (string &optional n)
  (let* ((n (or n 1))
	 (this-match (match-string n string)))
    (when this-match
      (append (list this-match) (match-strings string (+ 1 n))))))

(provide 'rhtml-navigation)
