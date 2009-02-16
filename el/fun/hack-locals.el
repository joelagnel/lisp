;;; hack-locals.el, replacement for `hack-local-variables'

;;; Copyright (C) 1985, 1986, 1987, 1992, 1993 Free Software Foundation, Inc.
;;; Copyright (C) 1993 Noah S. Friedman

;;; Maintainer: Noah Friedman <friedman@prep.ai.mit.edu>
;;; $Id: hack-locals.el,v 1.4 1995/01/10 20:59:55 friedman Exp $

;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, you can either send email to this
;;; program's author (see below) or write to: The Free Software Foundation,
;;; Inc.; 675 Massachusetts Avenue; Cambridge, MA 02139, USA.

;;; Portions of this program are derived from GNU Emacs 19.20 files.el.


(defconst enable-local-variables t
  "*Control use of local-variables lists in files you visit.
The value can be `t', `nil' or something else.
A value of `t' means local-variables lists are obeyed;
`nil' means they are ignored; 
`query-other' means query only if file being visited isn't owned by the user;
anything else means query.

The command \\[normal-mode] always obeys local-variables lists
and ignores this variable.

This feature has been modified by hack-locals.el.")

(defconst enable-local-eval 'query-other
  "*Control processing of the \"variable\" `eval' in a file's local variables.
The value can be `t', `nil' or something else.
A value of `t' means obey `eval' variables;
`nil' means ignore them; 
`query-other' means query only if file being visited isn't owned by the user;
anything else means query.

The command \\[normal-mode] always obeys local-variables lists
and ignores this variable.

This feature has been modified by hack-locals.el.")


(defun hack-local-variables-prop-line (&rest ignored)
  "Set local variables specified in the -*- line.
Ignore any specification for `mode:';
set-auto-mode should already have handled that."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (skip-chars-forward " \t\n\r")
    (let ((result '())
	  (end (save-excursion (end-of-line) (point))))
      ;; Parse the -*- line into the `result' alist.
      (cond ((not (search-forward "-*-" end t))
	     ;; doesn't have one.
	     nil)
	    ((looking-at "[ \t]*\\([^ \t\n\r:;]+\\)\\([ \t]*-\\*-\\)")
	     ;; Simple form: "-*- MODENAME -*-".
	     (setq result
                   (list (cons 'mode
                               (intern (buffer-substring
                                        (match-beginning 1)
                                        (match-end 1)))))))
	    (t
	     ;; Hairy form: '-*-' [ <variable> ':' <value> ';' ]* '-*-'
	     ;; (last ";" is optional).
	     (save-excursion
	       (if (search-forward "-*-" end t)
		   (setq end (- (point) 3))
		 (error "-*- not terminated before end of line")))
	     (while (< (point) end)
	       (or (looking-at "[ \t]*\\([^ \t\n:]+\\)[ \t]*:[ \t]*")
		   (error "malformed -*- line"))
	       (goto-char (match-end 0))
	       (let ((key (intern (downcase (buffer-substring
					     (match-beginning 1)
					     (match-end 1)))))
		     (val (save-restriction
			    (narrow-to-region (point) end)
			    (read (current-buffer)))))
		 (setq result (cons (cons key val) result))
		 (skip-chars-forward " \t;")))
	     (setq result (nreverse result))))
      
      (if (and result
               (let ((buffer-file-name (if (boundp 'buffer-file-truename)
                                           buffer-file-truename
                                         buffer-file-name)))
                 (or (eq enable-local-variables t)
                     (and (eq enable-local-variables 'query-other)
                          buffer-file-name
                          (eq (nth 2 (file-attributes buffer-file-name)) 
                              (user-uid)))
                     (and enable-local-variables
                          (save-window-excursion
                            (switch-to-buffer (current-buffer))
                            (y-or-n-p (format "Set local variables as specified in -*- line of %s? "
                                              (file-name-nondirectory buffer-file-name))))))))
	  (while result
	    (let ((key (car (car result)))
		  (val (cdr (car result))))
	      (or (eq key 'mode)
		  (hack-one-local-variable key val)))
	    (setq result (cdr result)))))))

(defun hack-local-variables (&rest ignored)
  "Parse and put into effect this buffer's local variables spec."
  (interactive)
  (hack-local-variables-prop-line)
  ;; Look for "Local variables:" line in last page.
  (save-excursion
    (goto-char (point-max))
    (search-backward "\n\^L" (max (- (point-max) 3000) (point-min)) 'move)
    (if (let ((case-fold-search t)
              (buffer-file-name (if (boundp 'buffer-file-truename)
                                    buffer-file-truename
                                  buffer-file-name)))
	  (and (search-forward "Local Variables:" nil t)
	       (or (eq enable-local-variables t)
                   (and (eq enable-local-variables 'query-other)
                        buffer-file-name
                        (eq (nth 2 (file-attributes buffer-file-name)) 
                            (user-uid)))
		   (and enable-local-variables
			(save-window-excursion
			  (switch-to-buffer (current-buffer))
			  (save-excursion
			    (beginning-of-line)
			    (set-window-start (selected-window) (point)))
			  (y-or-n-p (format "Set local variables as specified at end of %s? "
 					    (if buffer-file-name
 						(file-name-nondirectory 
 						 buffer-file-name)
 					      (concat "buffer "
 						      (buffer-name))))))))))
	(let ((continue t)
	      prefix prefixlen suffix beg
	      (enable-local-eval enable-local-eval))
	  ;; The prefix is what comes before "local variables:" in its line.
	  ;; The suffix is what comes after "local variables:" in its line.
	  (skip-chars-forward " \t")
	  (or (eolp)
	      (setq suffix (buffer-substring (point)
					     (progn (end-of-line) (point)))))
	  (goto-char (match-beginning 0))
	  (or (bolp)
	      (setq prefix
		    (buffer-substring (point)
				      (progn (beginning-of-line) (point)))))

	  (if prefix (setq prefixlen (length prefix)
			   prefix (regexp-quote prefix)))
	  (if suffix (setq suffix (concat (regexp-quote suffix) "$")))
	  (while continue
	    ;; Look at next local variable spec.
	    (if selective-display (re-search-forward "[\n\C-m]")
	      (forward-line 1))
	    ;; Skip the prefix, if any.
	    (if prefix
		(if (looking-at prefix)
		    (forward-char prefixlen)
		  (error "Local variables entry is missing the prefix")))
	    ;; Find the variable name; strip whitespace.
	    (skip-chars-forward " \t")
	    (setq beg (point))
	    (skip-chars-forward "^:\n")
	    (if (eolp) (error "Missing colon in local variables entry"))
	    (skip-chars-backward " \t")
	    (let* ((str (buffer-substring beg (point)))
		   (var (read str))
                   val)
	      ;; Setting variable named "end" means end of list.
	      (if (string-equal (downcase str) "end")
		  (setq continue nil)
		;; Otherwise read the variable value.
		(skip-chars-forward "^:")
		(forward-char 1)
		(setq val (read (current-buffer)))
		(skip-chars-backward "\n")
		(skip-chars-forward " \t")
		(or (if suffix (looking-at suffix) (eolp))
		    (error "Local variables entry is terminated incorrectly"))
		;; Set the variable.  "Variables" mode and eval are funny.
		(hack-one-local-variable var val))))))))

(defconst ignored-local-variables
  '(enable-local-eval)
  "Variables to be ignored in a file's local variable spec.")

;; "Set" one variable in a local variables spec.
;; A few variable names are treated specially.
(defun hack-one-local-variable (var val)
  (cond ((eq var 'mode)
	 (funcall (intern (concat (downcase (symbol-name val))
				  "-mode"))))
	((memq var ignored-local-variables)
	 nil)
	;; "Setting" eval means either eval it or do nothing.
	;; Likewise for setting hook variables.
	((or (eq var 'eval)
	     (string-match "-hooks?$\\|-functions?$" (symbol-name var)))
	 (if (and (not (string= (user-login-name) "root"))
		  (or (eq enable-local-eval t)
                      (and (eq enable-local-eval 'query-other)
                           buffer-file-name
                           (eq (nth 2 (file-attributes buffer-file-name)) 
                               (user-uid)))
		      (and enable-local-eval
			   (save-window-excursion
			     (switch-to-buffer (current-buffer))
			     (save-excursion
			       (beginning-of-line)
			       (set-window-start (selected-window) (point)))
			     (setq enable-local-eval
				   (y-or-n-p (format "Process `eval' or hook local variables in file %s? "
						     (file-name-nondirectory buffer-file-name))))))))
	     (if (eq var 'eval)
		 (save-excursion (eval val))
	       (make-local-variable var)
	       (set var val))
	   (message "Ignoring `eval:' in file's local variables")))
	;; Ordinary variable, really set it.
	(t (make-local-variable var)
	   (set var val))))


(provide 'hack-locals)

;; End of file hack-locals.el
