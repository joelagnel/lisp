;;; font-menus.el --- additional font menus for GNU Emacs

;; Copyright (C) 2000 Francis J. Wright

;; Maintainer: Francis J. Wright <F.J.Wright@Maths.QMW.ac.uk>
;; Time-stamp: <06 February 2000>
;; Keywords: font-lock, font, menu

;; This file is not part of GNU Emacs.

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This package is intended for use with GNU Emacs 20 and adds
;; submenus to the Edit menu to control font lock mode and provide
;; font display.

;;; Installation:

;; Put this file somewhere where Emacs can find it (i.e. in one of the
;; directories in your `load-path' such as `site-lisp'), optionally
;; byte-compile it, and put this in your .emacs:
;;
;; (require 'font-menus)

;;; Font Display:

;; Extracted from font-lock.el for GNU Emacs 20.3 and
;; font-lock-menu.el for GNU Emacs 19, both by Simon Marshal
;; <simon@gnu.ai.mit.edu> and revised to use easymenu and run as a
;; stand-alone package by Francis J. Wright.
;; (It would be better put back into font-lock.el!)

;;; Code:

(eval-when-compile
  (require 'easymenu)
  (require 'font-lock))

;; Make the Font Lock menu and add it to the `Edit' menu, before the
;; Text Properties menu:

(easy-menu-add-item			; (map path item &optional before)
 menu-bar-edit-menu nil

 (easy-menu-create-menu			; (menu-name menu-items)
  "Syntax Highlighting"
  '(["In All Buffers" global-font-lock-mode
     :style toggle :selected global-font-lock-mode :active t]
    ["In Current Buffer" font-lock-mode
     :style toggle :selected font-lock-mode :active t]
    "--"
    ["More In Current Buffer" font-lock-fontify-more
     (nth 2 font-lock-fontify-level)]
    ["Less In Current Buffer" font-lock-fontify-less
     (nth 1 font-lock-fontify-level)]
    ))

 'props)

(defvar font-lock-fontify-level nil)	; For less/more fontification.

(defun font-lock-fontify-level (level)
  (let ((font-lock-maximum-decoration level))
    (when font-lock-mode
      (font-lock-mode))
    (font-lock-mode)
    (when font-lock-verbose
      (message "Fontifying %s... level %d" (buffer-name) level))))

(defun font-lock-fontify-less ()
  "Fontify the current buffer with less decoration.
See `font-lock-maximum-decoration'."
  (interactive)
  ;; Check in case we get called interactively.
  (if (nth 1 font-lock-fontify-level)
      (font-lock-fontify-level (1- (car font-lock-fontify-level)))
    (error "No less decoration")))

(defun font-lock-fontify-more ()
  "Fontify the current buffer with more decoration.
See `font-lock-maximum-decoration'."
  (interactive)
  ;; Check in case we get called interactively.
  (if (nth 2 font-lock-fontify-level)
      (font-lock-fontify-level (1+ (car font-lock-fontify-level)))
    (error "No more decoration")))

;; This should be called by `font-lock-set-defaults'.
(defun font-lock-set-menu ()
  ;; Activate less/more fontification entries if there are multiple levels for
  ;; the current buffer.  Sets `font-lock-fontify-level' to be of the form
  ;; (CURRENT-LEVEL IS-LOWER-LEVEL-P IS-HIGHER-LEVEL-P) for menu activation.
  (let ((keywords (or (nth 0 font-lock-defaults)
		      (nth 1 (assq major-mode font-lock-defaults-alist))))
	(level (font-lock-value-in-major-mode font-lock-maximum-decoration)))
    (make-local-variable 'font-lock-fontify-level)
    (if (or (symbolp keywords) (= (length keywords) 1))
	(font-lock-unset-menu)
      (cond ((eq level t)
	     (setq level (1- (length keywords))))
	    ((or (null level) (zerop level))
	     ;; The default level is usually, but not necessarily, level 1.
	     (setq level (- (length keywords)
			    (length (member (eval (car keywords))
					    (mapcar 'eval (cdr keywords))))))))
      (setq font-lock-fontify-level (list level (> level 1)
					  (< level (1- (length keywords))))))))

;; This should be called by `font-lock-unset-defaults'.
(defun font-lock-unset-menu ()
  ;; Deactivate less/more fontification entries.
  (setq font-lock-fontify-level nil))

;; Added by FJW:

(defadvice font-lock-set-defaults
  (after font-lock-set-defaults-advice activate)
  "Font Lock Mode Menu support added."
  (font-lock-set-menu))

(defadvice font-lock-unset-defaults
  (after font-lock-unset-defaults-advice activate)
  "Font Lock Mode Menu support added."
  (font-lock-unset-menu))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Font Display:

;; Based on code by "Daniel, Elijah" <Elijah.Daniel@compaq.com>
;; and `list-faces-display' in `faces.el'.

(defun display-fonts ()
  "Sorted display of all the fonts Emacs knows about."
  (interactive)
  (with-output-to-temp-buffer "*Fonts*"
    (save-excursion
      (set-buffer standard-output)
      (mapcar (lambda (font) (insert font "\n"))
	      (sort (x-list-fonts "*") 'string-lessp)))
    (print-help-return-message)))

(define-key-after facemenu-menu [display-fonts]
  '("Display Fonts" . display-fonts) t)

(provide 'font-menus)

;;; font-menus.el ends here
