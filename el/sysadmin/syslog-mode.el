;;; syslog-mode.el --- Mode for viewing system logfiles
;;
;; ~harley/share/emacs/pkg/syslog-mode.el ---
;;
;; $Id: syslog-mode.el,v 1.7 2003/03/17 18:50:12 harley Exp $
;;

;; Author:    Harley Gorrell <harley@mahalito.net>
;; URL:       http://www.mahalito.net/~harley/elisp/syslog-mode.el
;; License:   GPL v2
;; Keywords:  syslog, emacs

;;; Commentary:
;; * Handy functions for looking at system logs.
;; * Fontifys the date and su messages.

;;; History:
;;  2003-03-16 : Updated URL and contact info.

;;; Code:

;; Setup
(defvar syslog-mode-hook nil
  "*Hook to setup `syslog-mode'.")

(defvar syslog-mode-load-hook nil
  "*Hook to run when `syslog-mode' is loaded.")

;;;###autoload
(defvar syslog-setup-on-load nil
  "*If not nil setup syslog mode on load by running syslog-add-hooks.")

;; I also use "Alt" as C-c is too much to type for cursor motions.
(defvar syslog-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Ctrl bindings
    (define-key map [C-down] 'syslog-boot-start)
    ;; XEmacs does not like the Alt bindings
    (if (string-match "XEmacs" (emacs-version))
	t)
    map)
  "The local keymap for `syslog-mode'.")


;;;###autoload
(defun syslog-mode ()
  "Major mode for working with system logs.

\\{syslog-mode-map}"
  (interactive)
  ;;
  (kill-all-local-variables)
  (setq mode-name "syslog")
  (setq major-mode 'syslog-mode)
  (use-local-map syslog-mode-map)
  ;;
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(syslog-font-lock-keywords))
  ;;
  (run-hooks 'syslog-mode-hook)
  )


(defvar syslog-boot-start-regexp "unix: SunOS"
  "Regexp to match the first line of boot sequence.")

(defun syslog-boot-start ()
  "Jump forward in the log to when the system booted."
  (interactive)
  (search-forward-regexp syslog-boot-start-regexp (point-max) t)
  (beginning-of-line))

;; Keywords
;; Todo: Seperate the keywords into a list for each format, rather
;; than one for all.
(defvar syslog-font-lock-keywords
  '(
    ;; Dates: May  9 15:52:34
    ("^\\(...\\s-+[0-9]+\\s-+[0-9:]+\\)" (1 font-lock-type-face))
    ;; Su events
    ("\\(su:.*$\\)" (1 font-lock-comment-face))
    )
  "Expressions to hilight in `syslog-mode'.")


;;; Setup functions
(defun syslog-find-file-func ()
  "Invoke `syslog-mode' if the buffer appears to be a system logfile.
and another mode is not active.
This function is added to `find-file-hooks'."
  (if (and (eq major-mode 'fundamental-mode)
	   (looking-at syslog-sequence-start-regexp))
      (syslog-mode)))

(defun syslog-add-hooks ()
  "Add a default set of syslog-hooks.
These hooks will activate `syslog-mode' when visiting a file
which has a syslog-like name (.fasta or .gb) or whose contents
looks like syslog.  It will also turn enable fontification for `syslog-mode'."
  ;; (add-hook 'find-file-hooks 'syslog-find-file-func)
  (add-to-list
   'auto-mode-alist
   '("\\(messages\\(\\.[0-9]\\)?\\|SYSLOG\\)\\'" . syslog-mode))
  )

;; Setup hooks on request when this mode is loaded.
(if syslog-setup-on-load
    (syslog-add-hooks))

;; done loading
(run-hooks 'syslog-mode-load-hook)
(provide 'syslog-mode)

;;; syslog-mode.el ends here
