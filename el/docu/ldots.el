;;; ldots.el -- changes three dots to `\ldots' on the fly

;; Filename: ldots.el
;; Copyright (C) 2005 Jesse Rosenthal
;; Author: Jesse Rosenthal <jesse.k.rosenthal@gmail.com>
;; Maintainer: Jesse Rosenthal <jesse.k.rosenthal@gmail.com>
;; Created: 30 Oct 2005
;; Description: On-the-fly rewriting of three dots as `\ldots'.


;; Version 0.1

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 2, or (at your
;; option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This just adds in a feature I can't see to find in AUCTeX: the
;; ability to do on-the-fly rewriting of three dots as `\ldots'. Not a
;; big deal, but I missed the feature.
;;
;;; To use:
;; 
;; Really, this is so short you might as well just copy the whole
;; thing into your ~/.emacs. Alternately, you can add the following
;; line:
;;
;; (load-file "/path/to/this/ldots.el")
;;
;; Note that right now, it's only set up for AUCTeX. For regular emacs
;; mode, you would have to change the "LaTeX" mode bits to
;; "latex". I'll change this when I get a chance.

;;; Code:

(defun test-for-periods()
  (save-excursion
    (let ((first (char-to-string (char-before (- (point) 1))))
	  (second (char-to-string (preceding-char))))
      (concat first second))))

(defun period-to-ldots()
  (interactive)
  (cond ((and (/= (point) 1) (/= (point) 2) (string= (test-for-periods) ".."))
	 (backward-delete-char 2)
	 (insert "\\ldots"))
	(t (insert "."))))

(defun latex-auto-ldots ()
	      (define-key LaTeX-mode-map "." 'period-to-ldots))

(add-hook 'LaTeX-mode-hook 'latex-auto-ldots)
