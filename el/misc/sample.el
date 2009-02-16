;;; sample.el --- Sample major mode

;; Copyright (C) 2001, 2004  Free Software Foundation, Inc.

;; Author: Stefan Monnier <monnier@cs.yale.edu>
;; Keywords: extensions

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; 

;;; Code:

(defvar sample-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [foo] 'sample-do-foo)
    map)
  "Keymap for `sample-mode'.")

(defvar sample-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?# "<" st)
    (modify-syntax-entry ?\n ">" st)
    st)
  "Syntax table for `sample-mode'.")

(defvar sample-font-lock-keywords
  '(("function \\(\\sw+\\)" (1 font-lock-function-name-face)))
  "Keyword highlighting specification for `sample-mode'.")

(defvar sample-imenu-generic-expression
  ...)

(defvar sample-outline-regexp
  ...)

;;;###autoload
(define-derived-mode sample-mode fundamental-mode "Sample"
  "A major mode for editing Sample files."
  (set (make-local-variable 'comment-start) "# ")
  (set (make-local-variable 'comment-start-skip) "#+\\s-*")
  (set (make-local-variable 'font-lock-defaults)
       '(sample-font-lock-keywords))
  (set (make-local-variable 'indent-line-function) 'sample-indent-line)
  (set (make-local-variable 'imenu-generic-expression)
       sample-imenu-generic-expression)
  (set (make-local-variable 'outline-regexp) sample-outline-regexp)
  ...)

;;; Indentation

(defun sample-indent-line ()
  "Indent current line of Sample code."
  (interactive)
  (let* ((savep (point))
	 (indent (condition-case nil
		     (save-excursion
		       (forward-line 0)
		       (skip-chars-forward " \t")
		       (if (>= (point) savep) (setq savep nil))
		       (max (sample-calculate-indentation) 0))
		   (error 0))))
    (if savep
	(save-excursion (indent-line-to indent))
      (indent-line-to indent))))

(defun sample-calculate-indentation ()
  "Return the column to which the current line should be indented."
  ...)


(provide 'sample)
;; arch-tag: b5d7a461-b1bc-4f32-b3b7-cad11d95017d
;;; sample.el ends here
