;;;
;;; Miscellaneous support functions
;;; Paul Nathan Stickney <pstickne@gmail.com> 2006
;;; GPL License
;;;

(require 'elunit)

;;
;; Misc
;;

(defun truncate-string (string width &optional ellipsis)
  "Like truncate-string-to-width but simplified.  DELIMS defaults to
  \"...\" unless otherwise specified."
  (truncate-string-to-width string width 0 nil (or ellipsis "...")))

(defun resignal (the-error)
  "Re-signal THE-ERROR."
  (signal (car the-error) (cdr the-error)))

;;
;; Buffer features
;;

(defun buffer-puts (buffer string &optional no-smart-newline)
  "If NO-SMART-NEWLINE is non-nil a newline is unconditionally
added. Otherwise a newline is only added if STRING does not end in one."
  (let* ((string (if no-smart-newline
		     (concat string "\n")
		   (add-smart-newline string))))
    (append-string-to-buffer buffer string)))

(defun buffer-puts-tail (buffer string &optional tail-mode no-smart-newline)
  "Appends STRING to the end of BUFFER. If TAIL-MODE is non-nil it
over-rides the value of `buffer-puts-tail-mode'. See `buffer-puts' for an
explanation of no-smart-newline."
  (with-current-buffer buffer
    (let* ((tail-mode ;; merge option
	    (or tail-mode buffer-puts-tail-mode))
	   (windows (and (memq tail-mode '(maybe always))
			 (get-buffer-window-list buffer)))
	   (tail-list ;; windows to move to end
	    (cond ((eq tail-mode 'maybe)
		   (remove-if-not
		    (lambda (x)
		      (eq (window-point x) (point-max)))
		    windows))
		  ((eq tail-mode 'always) windows))))
      (buffer-puts buffer string no-smart-newline)
      (dolist (window tail-list)
	(set-window-point window (point-max)))
      )))


;;;
;;; Functions to provide basic feature reloading. Use with care.
;;;
(defun find-matching-features (regex)
  "Returns a list of features matching REGEX."
  (remove-if-not (lambda (str)
		   (string-match regex (symbol-name str)))
		 features))

(defun remove-matching-features (regex)
  "Removes all features matching REGEX."
  (dolist (feature (find-matching-features regex))
    (remove-feature feature)))

(defun reload-matching-features (regex)
  "Reloads all features matching REGEX."
  (dolist (feature (find-matching-features regex))
    (reload-feature feature)))

(defun remove-feature (feature)
  "Removes FEATURE provided with `provides'.  It does NOT remove any
side-effects loading the feature caused."
  (setq features (delq feature features)))

(defun reload-feature (feature)
  "Reloads FEATURE using `require'."
  (remove-feature feature)
  (require feature))


(provide 'el4r-support)