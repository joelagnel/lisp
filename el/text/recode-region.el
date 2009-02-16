;;; Saved through ges-version 0.3.3dev at 2004-12-07 11:33
;;; From: Magnus Henoch <mange@freemail.hu>
;;; Subject: recode-region.el --- correct incorrectly encoded text
;;; Newsgroups: gmane.emacs.sources
;;; Date: Mon, 29 Nov 2004 18:21:32 +0100

;;; recode-region.el --- correct incorrectly encoded text

;; Copyright (C) 2004  Magnus Henoch

;; Author: Magnus Henoch <mange@freemail.hu>

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

;; Sometimes you end up with text that was really UTF-8 but was
;; interpreted as Latin-1, or some other combination.  I found no
;; function to fix that, so I wrote my own.
;;
;; Only tested with the CVS version of GNU Emacs, so YMMV.

;;; Code:

(defun recode-string (string was-really interpreted-as)
  (decode-coding-string 
   (encode-coding-string (string-make-unibyte string) interpreted-as)
   was-really))

(defun recode-region (start end was-really interpreted-as)
  (interactive "r
zText was really in: 
zBut was interpreted as: ")
  (let ((the-text (buffer-substring start end)))
    (delete-region start end)
    (insert (recode-string the-text was-really interpreted-as))))

(provide 'recode-region)
;;; recode-region.el ends here

