;; Trap kill-emacs function
;; Copyright (C) 1998 Joe Keane <jgk@jgk.org>
;; This file is in the public domain.

;; Suggested usage:
;; no key bindings
;; (autoload 'trap-kill-emacs "trap-kill-emacs" nil t)

(defvar kill-emacs-subr nil
  "This variable holds the real kill-emacs function.")

(defun trap-kill-emacs ()
  "Trap the kill-emacs function."
  (interactive)
  (if kill-emacs-subr
      (message "kill-emacs is already trapped")
    (progn
      (setq kill-emacs-subr (symbol-function 'kill-emacs))
      (defalias 'kill-emacs 'kill-emacs-handler))))

(defun untrap-kill-emacs ()
  "Untrap the kill-emacs function."
  (interactive)
  (if (not kill-emacs-subr)
      (message "kill-emacs is not trapped")
    (progn
      (defalias 'kill-emacs kill-emacs-subr)
      (setq kill-emacs-subr nil))))

(defun kill-emacs-handler ()
  "Tell the user that kill-emacs is trapped."
  (error
   (substitute-command-keys
    "kill-emacs is trapped; use \\[untrap-kill-emacs]")))
