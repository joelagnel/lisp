;;; ddutil.el --- Drag'n'Drop utility for integrating kill-ring and selections

;; Copyright (C) 2000, 2001 Katsumi Yamaoka

;; Author: Katsumi Yamaoka  <yamaoka@jpl.org>
;; Created: 2000/11/14
;; Revised: 2001/02/15
;; Keywords: clipboard, drag'n'drop, kill, mouse, selection, yank

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Byte-compile and load (or require) this file, if you have the guts
;; and also you are patient with trifling defects.  Before complaining,
;; try to fix the problems yourself when you aren't satisfied with it,
;; and let me know how did you do that.

;; By the way, it is recommended setting both variables `zmacs-regions'
;; and `mouse-yank-at-point' to `t' for the use of this program.

;;; Code:

(eval-and-compile
  (unless (featurep 'xemacs)
    (error "This program is for only XEmacsen, good-bye.")))

;; Each hook of `zmacs-activate-region-hook', `zmacs-deactivate-region-hook'
;; and `zmacs-update-region-hook' has one compiled lambda form which is
;; predefined by the system.  Especially, `zmacs-deactivate-region-hook'
;; may have the function `disown-selection' or `x-disown-selection' that
;; is an obstacle to use this program.  Because it will vanish the PRIMARY
;; selection just after the region is deactivated, even though the selection
;; has not been yanked.
(setq zmacs-deactivate-region-hook nil)

(eval-when-compile
  (defmacro ddutil-unify-iso646-characters (string)
    "Unify ISO646 characters."
    ;; Note: There are illegal conversion specs for backslash and tilde.
    (let* ((from (char-to-int (make-char 'latin-jisx0201 33)))
	   (to (char-to-int (make-char 'latin-jisx0201 126)))
	   (diff (- from 33)))
      `(let ((string ,string)
	     (regexp (format "[%c-%c]" ,from ,to))
	     index)
	 (while (setq index (string-match regexp string))
	   (aset string index (- (aref string index) ,diff)))
	 string)))
  (defmacro ddutil-defadvice-insert-selection-spec ()
    `'((around unify-iso646-characters activate compile)
       "Unify ISO646 characters."
       (insert
	(with-temp-buffer
	  ad-do-it
	  ,(macroexpand '(ddutil-unify-iso646-characters
			  (buffer-string))))))))

(let ((spec (ddutil-defadvice-insert-selection-spec)))
  (if (fboundp 'insert-selection)
      (eval `(defadvice insert-selection ,@spec))
    ;; Old XEmacsen.
    (eval `(defadvice x-insert-selection ,@spec))))

;; for old XEmacsen.
(defvar ddutil-byte-compile-prop-x-get-selection)
(eval-when-compile
  (setq ddutil-byte-compile-prop-x-get-selection
	(get 'x-get-selection 'byte-compile))
  (put 'x-get-selection 'byte-compile nil))
(eval-and-compile
  (unless (fboundp 'get-selection-no-error)
    (defun get-selection-no-error (&optional type data-type)
      (condition-case nil
	  (x-get-selection type data-type)
	(error nil)))))
(eval-when-compile
  (put 'x-get-selection 'byte-compile
       ddutil-byte-compile-prop-x-get-selection)
  (makunbound 'ddutil-byte-compile-prop-x-get-selection))
(unless (fboundp 'own-selection)
  (defalias 'own-selection 'x-own-selection))

(defvar ddutil-mouse-track-extent nil)

(defun ddutil-mouse-track-cleanup-extents-hook ()
  (remove-hook 'pre-command-hook 'ddutil-mouse-track-cleanup-extents-hook)
  (when ddutil-mouse-track-extent
    (detach-extent ddutil-mouse-track-extent)))

(setq interprogram-cut-function
      (lambda (text &optional push)
	(own-selection text 'PRIMARY)
	(own-selection text 'CLIPBOARD)
	(when (and zmacs-regions zmacs-region-extent)
	  (let ((start (extent-start-position zmacs-region-extent))
		(end (extent-end-position zmacs-region-extent)))
	    (when (and start end (not (eq start end)))
	      (zmacs-deactivate-region)
	      (if ddutil-mouse-track-extent
		  (set-extent-endpoints ddutil-mouse-track-extent
					start end (current-buffer))
		(set-extent-properties
		 (setq ddutil-mouse-track-extent (make-extent start end))
		 '(face primary-selection priority 65535)))
	      (add-hook 'pre-command-hook
			'ddutil-mouse-track-cleanup-extents-hook))))))

(setq interprogram-paste-function
      (lambda ()
	(let ((primary (get-selection-no-error 'PRIMARY))
	      (clipboard (get-selection-no-error 'CLIPBOARD)))
	  (cond ((> (length primary) 0)
		 (ddutil-unify-iso646-characters primary))
		((> (length clipboard) 0)
		 (ddutil-unify-iso646-characters clipboard))))))

(defadvice default-mouse-track-down-hook
  (before save-current-window-and-position activate compile)
  "Save current window and position."
  (put 'ddutil-mouse-track-down-position 'window (selected-window))
  (put 'ddutil-mouse-track-down-position 'position (point)))

(defadvice default-mouse-track-drag-up-hook
  (around restore-last-window-and-position activate compile)
  "Restore last window and position."
  (let (text)
    (let ((region (default-mouse-track-return-dragged-selection
		    (ad-get-arg 0))))
      (unless (eq (car region) (cdr region))
	(setq text (buffer-substring (car region) (cdr region)))))
    ad-do-it
    (when text
      ;; A text will also be stored in both the PRIMARY selection and
      ;; the window system CLIPBOARD, and then the region will be
      ;; deactivated using `interprogram-cut-function'.
      (kill-new text))
    (when (and text
	       (window-live-p (get 'ddutil-mouse-track-down-position
				   'window)))
      (select-window (get 'ddutil-mouse-track-down-position 'window))
      (goto-char (get 'ddutil-mouse-track-down-position 'position)))))

(provide 'ddutil)

;; ddutil.el ends here
