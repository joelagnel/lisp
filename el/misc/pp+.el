;;; pp+.el --- Extensions to `pp.el'.
;;
;; Filename: pp+.el
;; Description: Extensions to `pp.el'.
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 1999-2006, Drew Adams, all rights reserved.
;; Created: Fri Sep  3 13:45:40 1999
;; Version: 21.0
;; Last-Updated: Sun Jan 29 13:16:47 2006 (-28800 Pacific Standard Time)
;;           By: dradams
;;     Update #: 72
;; URL: http://www.emacswiki.org/cgi-bin/wiki/pp+.el
;; Keywords: lisp
;; Compatibility: GNU Emacs 20.x, GNU Emacs 21.x, GNU Emacs 22.x
;;
;; Features that might be required by this library:
;;
;;   `pp'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Extensions to `pp.el'.
;;
;;
;;  ***** NOTE: The following function defined in `pp.el' has
;;              been REDEFINED HERE:
;;
;;  `pp-eval-expression'.
;;
;;
;;  Suggested binding:
;;
;;   (substitute-key-definition 'eval-expression 'pp-eval-expression global-map)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;; 2006/01/29 dadams
;;     pp-eval-expression: Use read-from-minibuffer.
;; 2005/01/04 dadams
;;     Set buffer-read-only to nil.
;; 2004/03/24 dadams
;;     pp-eval-expression: Added call to font-lock-fontify-buffer.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'pp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; REPLACES ORIGINAL in `pp.el':
;; 1. Uses no `emacs-lisp-mode-hook'.
;; 2. Reads with completion, using `read-expression-map'.
;; 3. Call font-lock-fontify-buffer.
;; 4. Progress message added.
;;
;;;###autoload
(defun pp-eval-expression (expression)
  "Evaluate EXPRESSION and pretty-print value into a new display buffer.
If the pretty-printed value fits on one line, the message line is used
instead.  Value is also consed onto front of global list `values'."
  (interactive
   (list (read-from-minibuffer "Eval: " nil read-expression-map t
                               'read-expression-history)))
  (message "Evaluating...")
  (setq values (cons (eval expression) values))
  (let* ((old-show-function temp-buffer-show-function)
	 ;; Use this function to display the buffer.
	 ;; This function either decides not to display it at all
	 ;; or displays it in the usual way.
	 (temp-buffer-show-function
	  (function
	   (lambda (buf)
	     (save-excursion
	       (set-buffer buf)
	       (goto-char (point-min))
	       (end-of-line 1)
	       (if (or (< (1+ (point)) (point-max))
		       (>= (- (point) (point-min)) (frame-width)))
		   (let ((temp-buffer-show-function old-show-function)
			 (old-selected (selected-window))
			 (window (display-buffer buf)))
		     (goto-char (point-min)) ; expected by some hooks ...
		     (make-frame-visible (window-frame window))
		     (unwind-protect
			 (progn
			   (select-window window)
			   (run-hooks 'temp-buffer-show-hook))
		       (select-window old-selected)
                       (message "Evaluating...done.  See buffer *Pp Eval Output*.")))
		 (message "%s" (buffer-substring (point-min) (point)))
		 ))))))
    (with-output-to-temp-buffer "*Pp Eval Output*"
      (pp (car values)))
    (save-excursion
      (set-buffer "*Pp Eval Output*")
      (setq buffer-read-only nil)
      (let ((emacs-lisp-mode-hook nil)
            (change-major-mode-hook nil))
        (emacs-lisp-mode))
      (make-local-variable 'font-lock-verbose)
      (setq font-lock-verbose nil)
      (font-lock-fontify-buffer))))

;;;;;;;;;;;;;;;;;;

(provide 'pp+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; pp+.el ends here
