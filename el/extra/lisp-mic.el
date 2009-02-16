;;; Mic's lisp-mode hacks.  Updated for GNU Emacs v19.
;; Time-stamp: <97/06/28 14:05:58 mic>

;; Note: I do not use lisp-mode much, just emacs-lisp-mode.
;; My .emacs setup:
;;
;; (autoload 'lisp-mic-hook "lisp-mic")
;; (add-hook 'emacs-lisp-mode-hook 'lisp-mic-hook)

(provide 'lisp-mic)
(require 'lisp-mode)
(autoload 'pp-eval-def "pp" nil t)
(define-key emacs-lisp-mode-map "\e\C-x" 'pp-eval-def) ; replace eval-defun
(define-key emacs-lisp-mode-map "\e\C-f" 'mic-forward-sexp)
(define-key emacs-lisp-mode-map "\e\C-b" 'mic-backward-sexp)
(and (fboundp 'PC-lisp-complete-symbol)
     (define-key emacs-lisp-mode-map "\e\t" 'PC-lisp-complete-symbol))

;;; Todo: get v19 edebug working with pp-eval-def ...
;; (setq edebug-all-defuns (not edebug-all-defuns))

;;; This should be on emacs-lisp-mode-hook, maybe lisp-mode-hook:
(defun lisp-mic-hook nil
  (setq
   ;; the define-key below is more important than this:
   indent-line-function 'lisp-mic-indent-line
   ;; Lines to not reformat at all:
   paragraph-separate "^[ \t\f;]*$\\|^("
   ;; Above lines, plus reformatable first lines:
   paragraph-start (concat paragraph-separate "\\|^[ \t]*[\"(]")
   ;; not really local:
   inferior-lisp-prompt "^[^ \t\n()\"'`]*>+:? *"
   parse-sexp-ignore-comments t		; nil?
   )

  (if (eq window-system 'x)
      (progn
	;; Handled by x-mic: (or (featurep 'hilit19) (font-lock-mode 1))
	(setq show-paren-face 'bold)
	;; (setq show-paren-mismatch-face 'region) ; stig-paren?
	(require 'paren)
	(require 'imenu)		; 19.28
	;; Note: in 19.31, imenu-add-to-menubar affects only the
	;; current buffer, and not other buffers of the same mode.
	(imenu-add-to-menubar "Index")
	;; (lookup-key (current-local-map) [menu-bar index])
	))

  ;; flashparen: needs to be after paren, but hopeless with lazy-lock.
  (if (featurep 'lazy-lock)
      nil
    ;; (require 'flashparen)
    ;; (make-local-variable 'flash-matching-mode)
    ;; (flash-matching-mode)
    )

  ;; paged-mode: now handled for all files by infer package.
  )

;; Would like:
;; (defmacro let-syntax (forms &rest body)
;;  "With syntax temporarily modified by FORMS, evaluate BODY.
;; FORMS is a list of (CHAR SYNTAX), where CHAR is an integer
;; and SYNTAX is an argument to modify-syntax.")

(defun inside-comment-p (&optional on)
  "Is the point inside a comment?
Optional ON means to also count being on a comment start."
  ;; Note: this only handles single-character commenting, as in lisp.
  (or (and on (looking-at "\\s<"))
      (save-excursion
	(skip-syntax-backward "^><")
	(and (not (bobp))
	     (eq (char-syntax (preceding-char)) ?\<)))))
;; Try it: (inside-comment-p)

;; (modify-syntax-entry ?\- "w" emacs-lisp-mode-syntax-table)?
;; Nah, just use forward-sexp and backward-sexp to move over symbols.

(defun mic-forward-sexp (&optional arg)
  "Like forward-sexp, but also works inside a comment block."
  (interactive "p")
  (let ((parse-sexp-ignore-comments (not (inside-comment-p))))
    (forward-sexp arg)))

(defun mic-backward-sexp (&optional arg)
  "Like backward-sexp, but also works inside a comment block."
  (interactive "p")
  (let ((parse-sexp-ignore-comments (not (inside-comment-p))))
    (backward-sexp arg)))

(defun eval-hiding-lisp-comments (form)
  "Evaluate FORM, temporarily setting \";\" syntax to \" \"."
  (let ((old-syntax (char-syntax ?\;))) ; error if not ?\< ?
    (unwind-protect
	(progn
	  (modify-syntax-entry ?\; " ")
	  (eval form))
      (modify-syntax-entry ?\; (char-to-string old-syntax)))))

(define-key emacs-lisp-mode-map "\t" 'lisp-mic-indent-line)
(defun lisp-mic-indent-line (&optional whole-exp)
  "Do lisp-indent-line, or indent in comments, or complete symbols."
  (interactive "P")
  (let ((cc (current-column)) (pt (point)))
    (lisp-indent-line whole-exp)
    (if (and (eq cc (current-column)) (eq pt (point)))
	(if (inside-comment-p t)
	    (progn
	      (if whole-exp
		  (error "whole-exp not working inside comments, sorry"))
	      (setq pt
		    (save-excursion
		      (or (looking-at "\\s<") (skip-syntax-backward "^<"))
		      (skip-syntax-forward "<")
		      (delete-region
		       (point)
		       (save-excursion (skip-syntax-forward " ") (point)))
		      (indent-to-column
		       (let ((col (eval-hiding-lisp-comments
				   '(calculate-lisp-indent))))
			 (if (numberp col) col (car col)))
		       1)
		      (point)))
	      (if (< (point) pt) (goto-char pt)))))
    (if (and (eq cc (current-column)) (eq pt (point)) ; still nothing?
	     (memq (char-syntax (preceding-char)) '(?w ?_)))
	;; Try to complete the preceding symbol:
	(if (fboundp 'PC-lisp-complete-symbol)
	    (progn
	      (skip-syntax-forward "_w")
	      (PC-lisp-complete-symbol))
	  (lisp-complete-symbol)))
    ;; If repeated, ought to list completions.
    ))

;;; Allow split-level comments.         ; Like this
;; Note spaces here, tabs below         ; comment
					; out here ; and this one here.
(define-key emacs-lisp-mode-map "\e;" 'mic-indent-for-comment)
(defun mic-indent-for-comment nil
  "*Like indent-for-comment, with meta-comments."
  (interactive)
  (let ((cc (current-column)))
    (indent-for-comment)
    (if (eq cc (current-column))	; and (not (eolp))?
	(save-restriction
	  (let ((max (save-excursion (end-of-line) (point)))
		;; In some Emacs older than 19.28, (current-column)
		;; ignores restriction.  For those versions, ought to
		;; comment out the next line:
		(comment-column (- comment-column cc))
		(indent-tabs-mode nil))	; avoid creating tabs
	    (narrow-to-region (point) max)
	    (untabify (point) max)
	    (indent-for-comment))))))

(defun lisp-diagnose-parse nil
  "Jump to common lisp errors in buffer, widening if necessary.
Returns nil iff no problems were found."
  (interactive)
  (let (parse pt)
    (save-restriction
      (widen)
      (save-excursion
	(setq parse (parse-partial-sexp (point-min) (point-max) -1)
	      pt (point)))
      (setq pt
	    (cond
	     ((= 1 (nth 0 parse))
	      (message "Found unmatched '\('")
	      (nth 1 parse))
	     ((< 0 (nth 0 parse))
	      (message "Found unmatched '\(' [innermost, %d more previous]"
		       (1- (nth 0 parse)))
	      (nth 1 parse))
	     ((= -1 (nth 0 parse))
	      (message "Found unmatched '\)'")
	      (1- pt))
	     ((nth 3 parse)
	      (message "Buffer ends in a string!  need `%c'" (nth 3 parse))
	      (point-max))
	     ((nth 4 parse)
	      (message "Buffer ends in a comment!")
	      (point-max))
	     (t
	      ;; (message "No problems found.")
	      nil)			; indicates no problem
	     )))
    ;; If no parsing problem found, look for lisp-mnt problems:
    (or pt
	  (if (save-excursion
		(goto-char (point-min))
		(looking-at
		 ;; (concat
		 ;;  "^;;;? +"
		 ;;  (regexp-quote (file-name-nondirectory (buffer-file-name)))
		 ;;  " +---? ")
		 "^;;;? +[-a-z0-9]*\\.el +---? "
		 ))
	      ;; First line looks like lisp-mnt format, so do lm-verify.
	      ;; Even if lm-verify complains, we will not change pt.
	      ;; so that parse-save-compile-buffer does the save-buffer.
	      (progn
		(require 'lisp-mnt)
		(let ((msg (lm-verify)))
		  (if msg (progn (message msg) (sit-for 1)))))))
    (if (stringp pt) (setq pt (point)))
    (if (not pt)
	nil
      ;; Widen only if necessary:
      (if (or (< pt (point-min)) (> pt (point-max))) (widen))
      (goto-char pt))))

(defvar include-dependencies nil)	; see include.el
(defun parse-save-compile-buffer (&optional prefix)
  "If the buffer parses, save it and maybe compile it.
If lisp-diagnose-parse finds an error, do nothing (no save).
Otherwise save, and byte-compile if a compiled version exists.
Also offer to compile any includers \(see `include-dependencies'\).
Single prefix prevents compiling, double prefix forces compiling."
  ;; BUG: does not auto-compile buffer foo.el.gz (with jka-compr)
  (interactive "p")
  (if (lisp-diagnose-parse)
      nil				; found an error
    (save-buffer)
    (if (or (equal prefix 16)
	    (and (not (equal prefix 4))
		 (or
		  (file-exists-p (concat (buffer-file-name) "c"))
		  (assoc (buffer-file-name) include-dependencies))
		 ))
	(progn
	  (sit-for 0)
	  ;; Make sure this file exists on disk:
	  (or (file-exists-p (buffer-file-name))
	      (not (y-or-n-p "File does not exist! write to disk? "))
	      (write-region (point-min) (point-max) (buffer-file-name)))
	  ;; Compile it normally:
	  (if (or (file-exists-p (concat (buffer-file-name) "c")) (eq prefix 16))
	      (byte-compile-file (buffer-file-name)))
	  ;; Also offer to recompile any includers:
	  (let ((alist include-dependencies) elt)
	    (while (setq elt (assoc (buffer-file-name) alist))
	      (and (file-exists-p (concat (cdr elt) "c"))
		   (y-or-n-p (format "Recompile includer `%s' " (cdr elt)))
		   (byte-compile-file (cdr elt)))
	      (setq alist (cdr (memq elt alist)))))
	  ))))

(define-key emacs-lisp-mode-map "\C-x\C-s" 'parse-save-compile-buffer)

;; end of file
