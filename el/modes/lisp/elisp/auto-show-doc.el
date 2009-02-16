;;;; auto-show-doc.el -- show documentation as point moves
;;; Time-stamp: <2006-06-26 15:06:25 john>

;;  This program is free software; you can redistribute it and/or modify it
;;  under the terms of the GNU General Public License as published by the
;;  Free Software Foundation; either version 2 of the License, or (at your
;;  option) any later version.

;;  This program is distributed in the hope that it will be useful, but
;;  WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;  General Public License for more details.

;;  You should have received a copy of the GNU General Public License along
;;  with this program; if not, write to the Free Software Foundation, Inc.,
;;  59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

;;; After writing this and using it for a while, I found eldoc.el in
;;; the distribution does something similar, but shows only the
;;; arguments. I tried it for a while, then re-instated this. I might
;;; sometime merge them to produce an improved version of eldoc.el,
;;; and submit that to FSF.

(provide 'auto-show-doc)



(defvar described-thing nil
  "The most recently described thing.")

(make-variable-buffer-local 'described-thing)

(defvar auto-show-full-doc nil
  "*Whether to show the full documentation briefly.
May be a number, in which case it is how long to show it for, in seconds.")

(defvar auto-show-value t
  "*Whether to show the value as well as the first line of the documentation string.")

(defun auto-show-doc ()
  "If point has moved onto a different symbol, display documentation about the symbol.
Meant for use on post-command-hook; set this up in your emacs-lisp-mode-hook."
  (condition-case evar
      (progn
	(when (or (null described-thing)
		  (< (point) (car described-thing))
		  (> (point) (cdr described-thing)))
	  (setq described-thing (bounds-of-thing-at-point 'symbol))
	  (setq described-thing (bounds-of-thing-at-point 'symbol))
	  (let* ((symbol-string (buffer-substring-no-properties (car described-thing) (cdr described-thing)))
		 (symbol (intern-soft symbol-string)))
	    (when symbol
	      (let* ((doc (or (and (functionp symbol) (documentation symbol))
			      (documentation-property symbol 'variable-documentation)))
		     (short-doc (substring doc 0 (string-match "\n" doc)))
		     )
		(when auto-show-full-doc
		  (save-window-excursion
		    (with-output-to-temp-buffer "*Help*"
		      (when (boundp symbol)
			(princ (format "Value: %S\n\n" (symbol-value symbol))))
		      (princ "Documentation:\n")
		      (princ doc))
		    (sit-for (if (numberp auto-show-full-doc)
				 auto-show-full-doc
			       1))))
		(if (and auto-show-value
			 (boundp symbol))
		    (let ((doc-and-value (format "%s: %S" short-doc (symbol-value symbol))))
		      (message "%s" (substring doc-and-value
					       0 (min  (length doc-and-value)
						      (or (string-match "\n" doc-and-value)
							  (1- (frame-width)))
						      (1- (frame-width))))))
		  (message "%s" short-doc)))))))
    (error (setq sdic-error evar))))

;;; end of auto-show-doc.el
