;;; always-complete.el: Extensions to completion.el to make
;;; completions display themselves in the minibuffer as you type.

;;; Code by Alan Ruttenberg, Music Group, MIT Media Laboratory.
;;; Original completion.el by Jim Salem, available from think.com.

;;; To use this file, just put a line (load-file "<this-file>") in
;;; your .emacs file.  If you don't like to be reminded of
;;; completions, just load the file completions.el without this file.
;;; The default completion key (bound in completion.el) is M-return.
;;; You can also bind another key to the function 'complete, like
;;; this: (define-key esc-map "c" 'complete)

;; Requires advise.el and completion.el
(require 'advise)
(require 'cl)
(require 'tags)
(load "completion")

(defun maybe-display-next-completion ()
  (when *completep*
    (let ((string (and *completep* (symbol-before-point-for-complete))))
      (if string 
	  (progn
	    (completion-search-reset string)
	    (let ((it (completion-search-next 0)))
	      (if (and (consp it) (zerop (or (minibuffer-depth) 0)))
		  (message "%s" (car it))
		  )))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Key bindings:

(defvar *old-self-insert-command* (symbol-function 'self-insert-command))

;; patch the old one. This doesn't always work
(defun self-insert-command (howmany)
  (interactive "p")
  (prog1 (funcall *old-self-insert-command* howmany)
    (maybe-display-next-completion)))

;; it doesn't work for most of the characters, so make a new function, and 
;; globally-bind it to all the alphabetic keys.

(defun self-insert-command-1 (number)    
  (interactive "p")
  (prog1 (funcall *old-self-insert-command* number)
    (maybe-display-next-completion)))

(defvar all-completing-keys "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ*!@#$%^&*()_-1234567890<>?")
(dotimes (i (length all-completing-keys))
  (global-set-key (format "%c" (aref all-completing-keys i)) 'self-insert-command-1))

(advise previous-line :after (maybe-display-next-completion))
(advise next-line :after (maybe-display-next-completion))
(advise cmpl-forward-sexp :after (maybe-display-next-completion))
(advise cmpl-backward-sexp :after (maybe-display-next-completion))
(advise cmpl-forward-char :after (maybe-display-next-completion))
(advise cmpl-backward-char :after (maybe-display-next-completion))

(defun forward-word-1 (&optional number)
  (interactive "p")
  (prog1
      (forward-word number)
    (maybe-display-next-completion)))

(defun backward-word-1 (&optional number)
  (interactive "p")
  (prog1 
      (backward-word number)
    (maybe-display-next-completion)))

(defun backward-char-1 (&optional number)
  (interactive "p")
  (prog1
      (backward-word number)
    (maybe-display-next-completion)))

(defun forward-char-1 (&optional number)
  (interactive "p")
  (prog1
      (forward-char number)
    (maybe-display-next-completion)))

(defun rebind-function-key-callers (old-function new-function)
  (mapcar '(lambda (key) (global-set-key key new-function))
	  (where-is-internal old-function)))

(rebind-function-key-callers 'forward-word 'forward-word-1)
(rebind-function-key-callers 'backward-word 'backward-word-1)
(rebind-function-key-callers 'forward-char 'forward-char-1)
(rebind-function-key-callers 'backward-char 'backward-char-1)
