;;; w3-e21.el --- Emacs 21.xx specific functions for emacs-w3
;; Author: $Author: wmperry $
;; Created: $Date: 2000/10/16 15:36:56 $
;; Version: $Revision: 1.4 $
;; Keywords: faces, help, mouse, hypermedia

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (c) 1997, 1998 Free Software Foundation, Inc.
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

(require 'w3-e19)

(defadvice w3-setup-version-specifics (after emacs21 pre act)
  (and (featurep 'tool-bar)
       (w3-toolbar-make-buttons)))

(defadvice w3-mode-version-specifics (after emacs21 pre act)
  (w3-add-toolbar-to-buffer))

(defun w3-tooltip-get-tips (event)
  (let (widget pos help start)
    (setq start (event-start event)
	  pos (posn-point start)
	  widget (and pos (widget-at pos))
	  help (and widget (widget-get widget :help-echo)))
    (if (functionp help)
	(setq help (funcall help widget (posn-window start)
			    (window-buffer (posn-window start))
			    (posn-point start))))
    (if (stringp help)
	(tooltip-show help))))

(add-hook 'tooltip-hook 'w3-tooltip-get-tips)

(defvar toolbar-file-icon "new"
  "Lame definition to look a little like XEmacs' toolbar for Emacs/W3")

(defvar toolbar-printer-icon "print"
  "Lame definition to look a little like XEmacs' toolbar for Emacs/W3")

(provide 'w3-e21)
(require 'w3-toolbar)
