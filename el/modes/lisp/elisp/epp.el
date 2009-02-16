;;; epp.el --- emacs preprocessor (poor cpp emulation)

;; Copyright (C) 1997 by Michelangelo Grigni
;; Author:   Michelangelo Grigni <mic@mathcs.emory.edu>
;; Version:  0.00
;; Created:  Jun 17 1997
;; Keywords: programming

;;; This could be part of GNU Emacs.

;;; Commentary:

;;; History:
;; 6/18/97: created to support multiple versions of ffap.el

;;; Code:

(provide 'epp)

(defun epp-buffer (env)
  "Emulate cpp (the C preprocessor) on the current buffer.
The resulting output appears in the \"*cpp*\" buffer, which is
also the return value.

This function handles the following preprocessor directives:

    \043define SYM [VALUE]
    \043undef SYM
    \043ifdef SYM   // or #ifndef
    ... text ... // #else section optional
    \043endif

This is a poor emulation of cpp.  In particular:
 * no macros
 * no #include, #if, or #elif
 * no comments allowed after a string VALUE (takes the whole line)
 * symbols read with lisp syntax table, not C
 * output has no 'cookies' to track the original line numbers

ENV is a list describing the string value of each defined symbol:
   ENV == ((SYM1 . STRING1) (SYM2 . STRING2) ...)
Any string (even the empty string) counts as true for #ifdef.

To be useful in emacs-lisp, we allow \";;#\" in place of \"#\"."
  (with-output-to-temp-buffer "*cpp*"
    (epp-region-to-stdout (point-min) (point-max) env)
    standard-output))

(defun epp-file (env infile)
  "Emulate cpp on INFILE, writing output to \"*epp*\" buffer."
  (save-excurison
   (set-buffer (find-file-noselect infile))
   (epp-buffer env)))

(defun epp-region-to-stdout (beg end env)
  "Emulate cpp on region using ENV, printing to `standard-output'."
  (setq env (copy-sequence env))	; avoid modifying caller's argument
  (save-excursion
    (goto-char beg)
    (save-restriction
      (narrow-to-region beg end)
      (let ((standard-input (current-buffer))
	    (last-pt (point))
	    stack printing token sym value)
	(while (not (eq token 'eof))
	  ;; Move to next cookie or eof:
	  (re-search-forward "^[ \t]*\\(\;\;#\\|#\\)" nil 'move)
	  ;; If the stack has no nil's, print the previous region:
	  (and (setq printing (not (memq nil stack)))
	       (epp-simple-region
		last-pt
		(save-excursion (or (eobp) (beginning-of-line)) (point))
		env))
	  (if (eobp)			; no cookie, make a pseudo-token
	      (setq token 'eof)		; last-pt?
	    ;; Narrow to this line for parsing arguments:
	    (save-restriction
	      (narrow-to-region
	       (point) (save-excursion (forward-line 1) (point)))
	      ;; Read the token (preprocessor directive keyword):
	      (or (symbolp (setq token (read)))
		  (error "cpp -- bad token `%s'" token))
	      ;; Get SYM argument if necessary:
	      (if (memq token '(ifdef ifndef define undef))
		  (or (symbolp (setq sym (read)))
		      (error "cpp -- bad symbol `%s %s'" token sym)))
	      ;; Get VALUE argument if necessary:
	      (if (eq token 'define)	; include
		  (let (beg)
		    (skip-chars-forward " \t")
		    (setq beg (point))
		    (goto-char (point-max))
		    (skip-chars-backward " \t\n" beg)
		    (setq value (buffer-substring beg (point)))))
	      ;; Remember beginning of next line for the next iteration:
	      (goto-char (setq last-pt (point-max)))))
	  ;; Debugging hack:
	  (and (assoc 'EPPDEBUG env)
	       (princ (format "### EPPDEBUG: %s %s %S\n" token printing env)))
	  ;; All done parsing, now process the directive:
	  (cond
	   ((eq token 'ifdef)		; push t or nil
	    (setq stack (cons (consp (assoc sym env)) stack)))
	   ((eq token 'ifndef)		; push t or nil
	    (setq stack (cons (not (assoc sym env)) stack)))
	   ((eq token 'endif)		; pop
	    (or stack (error "cpp -- unmatched endif"))
	    (setq stack (cdr stack)))
	   ((eq token 'else)		; negate top of stack
	    (setq stack (cons (not (car stack)) (cdr stack))))
	   ;; if, elif -- not implemented, use lisp expressions?
	   ((eq token 'eof)
	    ;; include handling: pop marker from top of stack
	    (and stack (error "cpp -- missing endif's at eof")))
	   ((not printing))		; else ignore remaining directives
	   ((eq token 'undef)		; delete sym association
	    (setq env (delete (assoc sym env) env)))
	   ((eq token 'define)		; add sym association
	    (setq env (cons (cons sym value) (delete (assoc sym env) env))))
	   ;; include -- push point-marker on stack, find-file-noselect
	   (t (error "cpp -- unknown directive `#%s'" token))))))))

(defun epp-simple-region (beg end env)
  ;; Print a region devoid of epp directives, substituting ENV values.
  (save-excursion
    (goto-char beg)
    (save-restriction
      (narrow-to-region beg end)
      (let (case-fold-search
	    (regexp
	     (if (not env)
		 "\\'x"			; impossible regexp
	       (concat "\\<\\("
		       (mapconcat (lambda (p) (symbol-name (car p))) env "\\|")
		       "\\)\\>"))))
	(while (re-search-forward regexp end t)
	  (princ (buffer-substring beg (match-beginning 0)))
	  (princ (cdr (assoc (intern (match-string 0)) env)))
	  (setq beg (point)))
	(princ (buffer-substring (point) end))))))

;; Todo:
;;   * #include
;;   * work with byte-compiler
;;   * rewrite epp-simple-region to avoid big regexp

;;; epp.el ends here
