;;; wid-xpm-button.el --- put XPM buttons on the widget buttons

;; Copyright (C) 2000 Free Software Foundation, Inc.
;; Copyright (C) 2000 Katsumi Yamaoka

;; Author: Katsumi Yamaoka <yamaoka@jpl.org>
;; Created: 2000/12/08 -- Love and peace to John's soul!
;; Revised:
;; Keywords: button, widget, xpm

;; This file is not part of XEmacs yet.

;; XEmacs is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.

;; XEmacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This program will replace the widget buttons with the XPM buttons.
;; To use this, put the following line in your startup file.
;;
;;	(require 'wid-xpm-button)

;;; Code:

(require 'wid-edit)

(defcustom widget-xpm-button-shadow-thickness 3
  "A number of pixels should be used for the shadows on the edges of
the buttons."
  :group 'widget-button
  :type 'integer)

(defcustom widget-xpm-button-foreground "Yellow"
  "A color used to display the text."
  :group 'widget-button
  :type 'string)

(defcustom widget-xpm-button-background "#a0a0d0"
  "A background color the text will be displayed upon."
  :group 'widget-button
  :type 'string)

(defcustom widget-xpm-button-allowed-widget-type-callback-alist
  '((mime-button . :mime-button-callback) (mime-url-link))
  "Alist of the widget type and the callback property.  If a callback
property is ommited, refer to `:action' by default.   The extent will
be XPM buttonized only if its widget type is the car of the element
of the alist."
  :group 'widget-button
  :type 'string)

(defvar widget-xpm-button-cache nil
  "Cache for XPM buttons.")

(defvar widget-xpm-button-cache-size 32
  "*Maximum length of the cache used for `widget-xpm-button-cache'.")

(defun widget-xpm-put-button (pos)
  "Put an XPM button at point."
  (when (device-on-window-system-p)
    (let* ((prop (get-char-property pos 'button))
	   (extent (when prop
		     (widget-get prop :button-extent)))
	   widget button)
      (when (and extent
		 (setq widget (extent-property extent 'button))
		 (setq prop
		       (assq
			(widget-type widget)
			widget-xpm-button-allowed-widget-type-callback-alist)))
	(let* ((text (extent-string extent))
	       (spec (list
		      (if (string-match
			   "^\\s *\\s(+\\s *\\(.+\\)\\s *\\s)+\\s *$"
			   text)
			  (match-string 1 text)
			text)
		      widget-xpm-button-shadow-thickness
		      widget-xpm-button-foreground
		      widget-xpm-button-background)))
	  (unless (setq button (cdr (assoc spec
					   widget-xpm-button-cache)))
	    (setq button (apply #'xpm-button-create spec))
	    (nbutlast widget-xpm-button-cache
		      (max 0 (- (length widget-xpm-button-cache)
				widget-xpm-button-cache-size)))
	    (push button widget-xpm-button-cache)))
	(set-extent-properties extent '(invisible t start-open t))
	(setq pos (extent-end-position extent)
	      extent (make-extent pos pos))
	(let* ((down-glyph (make-glyph (cadr button)))
	       (up-glyph (make-glyph (car button)))
	       (down-func `(lambda (event)
			     (interactive "e")
			     (set-extent-begin-glyph ,extent ,down-glyph)))
	       (callback (widget-get widget (or (cdr prop) :action)))
	       (up-func `(lambda (event)
			   (interactive "e")
			   (mouse-set-point event)
			   (set-extent-begin-glyph ,extent ,up-glyph)
			   ,(when callback
			      `(funcall #',callback))))
	       (keymap (make-sparse-keymap)))
	  (define-key keymap 'button1 down-func)
	  (define-key keymap 'button2 down-func)
	  (define-key keymap 'button1up up-func)
	  (define-key keymap 'button2up up-func)
	  (set-extent-begin-glyph extent up-glyph)
	  (set-extent-property extent 'keymap keymap))))))

(defadvice widget-convert-button
  (after put-xpm-button-on-the-widget-button activate compile)
  "Put an XPM button on the widget button."
  (widget-xpm-put-button (ad-get-arg 1)))

(provide 'wid-xpm-button)

;; wid-xpm-button.el ends here
