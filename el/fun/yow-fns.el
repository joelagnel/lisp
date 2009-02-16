;;; yow-fns.el --- add-ons to zippy

;; Copyright (C) 1994-1999 Noah S. Friedman

;; Author: Noah Friedman <friedman@splode.com>
;; Maintainer: friedman@splode.com

;; $Id: yow-fns.el,v 1.1 1999/10/06 09:23:33 friedman Exp $

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Some of these functions have been added to official emacs distributions
;; already.a

;;; Code:

;; I added this to Emacs 19.31's yow.el.
(defun apropos-zippy (regexp)
  "Return a list of all Zippy quotes matching REGEXP.
If called interactively, display a list of matches."
  (interactive "sApropos Zippy (regexp): ")
  (require 'yow)
  ;; Make sure yows are loaded
  (cookie yow-file "Am I CONSING yet?..." "I have SEEN the CONSING!!")
  (let* ((case-fold-search t)
         (cookie-table-symbol (intern yow-file cookie-cache))
         (string-table (symbol-value cookie-table-symbol))
         (matches nil)
         (len (length string-table))
         (i 0))
    (save-match-data
      (while (< i len)
        (and (string-match regexp (aref string-table i))
             (setq matches (cons (aref string-table i) matches)))
        (setq i (1+ i))))
    (and (interactive-p)
         (cond ((null matches)
                (message "No matches found."))
               (t
                (let ((l matches))
                  (with-output-to-temp-buffer "*Zippy Apropos*"
                    (while l
                      (princ (car l))
                      (setq l (cdr l))
                      (and l
                           (princ "\n\n"))))))))
    matches))

(defun insert-yow-complete (&optional str)
  "ANN JILLIAN'S HAIR makes LONI ANDERSON'S HAIR look
like RICARDO MONTALBAN'S HAIR!"
  (interactive)
  (if str
      (insert str)
    (require 'yow)
    (require 'cookie1)
    (insert (read-cookie "Zippy quote (with completion): " yow-file
                         "Am I CONSING yet?..." "I have SEEN the CONSING!!"
                         t))))

(provide 'yow-fns)

;;; yow-fns.el ends here.
