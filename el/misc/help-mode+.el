;;; help-mode+.el --- Extensions to `help-mode.el'
;;
;; Filename: help-mode+.el
;; Description: Extensions to `help-mode.el'
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 2004-2006, Drew Adams, all rights reserved.
;; Created: Sat Nov 06 15:14:12 2004
;; Version: 21.0
;; Last-Updated: Tue Jul 11 11:38:42 2006 (-25200 Pacific Daylight Time)
;;           By: dradams
;;     Update #: 71
;; URL: http://www.emacswiki.org/cgi-bin/wiki/help-mode+.el
;; Keywords: help
;; Compatibility: GNU Emacs 21.x, GNU Emacs 22.x
;;
;; Features that might be required by this library:
;;
;;   `button', `help-mode', `view'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Extensions to `help-mode.el'
;;
;;
;;  Commands defined here:
;;
;;    `pop-to-help-toggle'.
;;
;;  Internal variables defined here:
;;
;;    `help-origin-buffer'.
;;
;;
;;  ***** NOTE: The following function defined in `help-mode.el'
;;              has been REDEFINED HERE:
;;
;;  `help-xref-on-pp' - Library names are buttonized.
;;
;;
;;  Put this in your initialization file (`~/.emacs'):
;;
;;    (require 'help-mode+)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;; 2006/07/11 dadams
;;     Added: help-origin-buffer, pop-to-help-toggle.  Bound latter to C-h C-o.
;;
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

(require 'help-mode)

;;;;;;;;;;;;;;;;;;;;;;;;


(defvar help-origin-buffer nil "Buffer that we left, to go to *Help*.")

(defun pop-to-help-toggle ()
  "Pop to buffer *Help* or back to the buffer that sent you to *Help*."
  (interactive)
  (let ((orig-buf (and (buffer-live-p help-origin-buffer)
                       (get-buffer help-origin-buffer)))
        (w32-grab-focus-on-raise   t)
        (win32-grab-focus-on-raise t))   ; Older name.
    (if (string-match "*Help*" (buffer-name))
        (cond ((not orig-buf)
               (error "No buffer to return to"))
              ((minibufferp orig-buf)
               (select-frame-set-input-focus
                (window-frame (select-window (minibuffer-window)))))
              (t
               (pop-to-buffer orig-buf)))
      (setq help-origin-buffer (current-buffer))
      (pop-to-buffer "*Help*"))))

(define-key help-map "\C-o" 'pop-to-help-toggle)


;; REPLACES ORIGINAL IN `help-mode.el'.
;; Buttonizes names of libraries also.
;; To see the effect, try `C-h v features', and click on a library name.
;;
;; 2006-01-20: This no longer works, because the call to this function
;; from `describe-variable was commented out in `help-fns.el'.
;;
;;;###autoload
(defun help-xref-on-pp (from to)
  "Add xrefs for symbols in `pp's output between FROM and TO."
  (if (> (- to from) 5000) nil
    (with-syntax-table emacs-lisp-mode-syntax-table
      (save-excursion
	(save-restriction
	  (narrow-to-region from to)
	  (goto-char (point-min))
	  (condition-case nil
	      (while (not (eobp))
		(cond
		 ((looking-at "\"") (forward-sexp 1))
		 ((looking-at "#<") (search-forward ">" nil 'move))
		 ((looking-at "\\(\\(\\sw\\|\\s_\\)+\\)")
		  (let* ((sym (intern-soft (match-string 1)))
			 (type (cond ((fboundp sym) 'help-function)
				     ((or (memq sym '(t nil))
					  (keywordp sym))
				      nil)
				     ((and sym (boundp sym))
				      'help-variable)
                                     ((and sym (locate-library (symbol-name sym)))
                                      'help-library))))
		    (when type (help-xref-button 1 type sym)))
		  (goto-char (match-end 1)))
		 (t (forward-char 1))))
	    (error nil)))))))

(define-button-type 'help-library
  :supertype 'help-xref
  'help-function #'(lambda (x) (find-library (symbol-name x)))
  'help-echo (purecopy "mouse-2, RET: find this library"))

;; $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
;; REPLACES ORIGINAL IN `help-mode.el'.
;; Provide a tooltip for whatever is under the mouse.
;; This can't be done here - the message needs to be done via an idle timer, 
;; whenever mouse is over any name. Perhaps combine with eldoc.
;; 
;; ;;;###autoload
;; (defun help-follow-mouse (click)
;;   "Follow the cross-reference that you CLICK on."
;;   (interactive "e")
;;   (let* ((start (event-start click))
;; 	 (window (car start))
;; 	 (pos (car (cdr start))))
;;     (with-current-buffer (window-buffer window)
;;       (message "Display help on `%s'"
;;                (save-excursion
;;                  (goto-char pos) (skip-syntax-backward "w_")
;;                  (buffer-substring (point)
;;                                    (progn (skip-syntax-forward "w_")
;;                                           (point)))))
;;       (help-follow pos))))

;; After a certain idle time, use function `mouse-position', and pick
;; up the symbol under the pointer. Then display a message that
;; clicking mouse-2 will display help on the symbol.


;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'help-mode+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; help-mode+.el ends here
