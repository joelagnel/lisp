;;; color-file-completion.el --- add colors to file completion

;; $Id: color-file-completion.el,v 1.7 2002/12/29 12:27:19 burton Exp $
;; Copyright (C) 1997-2000 Free Software Foundation, Inc.

;; Author: Kevin A. Burton (burton@openprivacy.org)
;; Maintainer: Kevin A. Burton (burton@openprivacy.org)
;; Location: http://relativity.yi.org
;; Keywords:
;; Version: 1.0.1

;; This file is [not yet] part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 2 of the License, or any later version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along with
;; this program; if not, write to the Free Software Foundation, Inc., 59 Temple
;; Place - Suite 330, Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; This is a hack for Emacs 20.x so that when you are using file completion,
;; directories can show up with a certain color.  This should work under Emacs
;; 21.

;; NOTE:  just prototype for a future hack.

;;; Code:

(defface completion-setup-directory-face  '((t (:foreground "LimeGreen")))
  "Face to use for directories."
  :group 'color-file-completion)

(defcustom color-file-completion-always t "If true, always turn on regexps in
completion buffers."
  :group 'color-file-completion
  :type 'boolean)

(defun completion-setup-directory-face()
  "When we are completing a filename, highlight directories."
  (interactive)

  ;;if this is completing a filename... highlight faces...
  (when (or color-file-completion-always
          (eq minibuffer-completion-table 'read-file-name-internal))

    (let((font-lock-verbose nil))
      (font-lock-mode 1)
        
      (font-lock-add-keywords nil '(("[^ \n]+/" 0 'completion-setup-directory-face keep)))

      (font-lock-fontify-buffer))))

(add-hook 'completion-list-mode-hook 'completion-setup-directory-face)

(provide 'color-file-completion)

;;; color-file-completion.el ends here
