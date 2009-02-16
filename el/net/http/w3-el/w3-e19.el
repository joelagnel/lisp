;;; w3-e19.el --- Emacs 19.xx specific functions for emacs-w3
;; Author: $Author: wmperry $
;; Created: $Date: 1999/12/05 08:36:03 $
;; Version: $Revision: 1.2 $
;; Keywords: faces, help, mouse, hypermedia

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (c) 1993 - 1996 by William M. Perry <wmperry@cs.indiana.edu>
;;; Copyright (c) 1996 - 1999 Free Software Foundation, Inc.
;;;
;;; This file is part of GNU Emacs.
;;;
;;; GNU Emacs is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; GNU Emacs is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to the
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA 02111-1307, USA.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Enhancements For Emacs 19
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(eval-when-compile
  (require 'w3-props))
(require 'w3-forms)
(require 'font)

(defvar w3-e19-window-width nil)
(make-variable-buffer-local 'w3-e19-window-width)

(defun w3-setup-version-specifics ()
  ;; Set up routine for emacs 19
  )

(defun w3-store-in-clipboard (str)
  "Store string STR in the system clipboard"
  (cond
   ((and (boundp 'interprogram-cut-function) interprogram-cut-function)
    (funcall interprogram-cut-function str t))
   (t
    (case (device-type)
      (x (x-select-text str))
      (pm (pm-put-clipboard str))
      (ns (ns-store-pasteboard-internal str))
      (otherwise nil)))))

(defun w3-mode-version-specifics ()
  ;; Emacs 19 specific stuff for w3-mode
  (declare (special w3-face-index w3-display-background-properties))
  (make-local-variable 'track-mouse)
  (setq w3-e19-window-width (window-width))
  (if w3-track-mouse (setq track-mouse t))
  (if w3-display-background-properties
      (let ((face (w3-make-face (intern
				 (format "w3-style-face-%05d" w3-face-index))
				"An Emacs-W3 face... don't edit by hand." t))
	    (fore (car w3-display-background-properties))
	    (inhibit-read-only t)
	    (back (cdr w3-display-background-properties)))
	(setq w3-face-index (1+ w3-face-index))
	(if fore (font-set-face-foreground face fore))
	(if back (font-set-face-background face back))
	(fillin-text-property (point-min) (point-max) 'face 'face face))))

(defun w3-mouse-handler (e)
  "Function to message the url under the mouse cursor"
  (interactive "e")
  (let* ((pt (posn-point (event-start e)))
	 (good (eq (posn-window (event-start e)) (selected-window)))
	 (mouse-events nil))
    (if (not (and good pt (number-or-marker-p pt)))
	nil
      (widget-echo-help pt))))

(defun w3-window-size-change-function (frame)
  (let ((first (frame-first-window frame))
	(cur nil))
    (while (not (eq cur first))
      (setq cur (if cur (next-window cur nil frame) first))
      (save-excursion
	(set-buffer (window-buffer cur))
	(if (and (eq major-mode 'w3-mode)
		 (not (eq (window-width cur) w3-e19-window-width)))
	    (w3-refresh-buffer))))))


(provide 'w3-emacs19)
(provide 'w3-e19)
