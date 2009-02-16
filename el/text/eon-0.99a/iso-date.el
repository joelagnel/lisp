;;; iso-date.el --- tiny iso date utilities

;; Copyright (C) 2007  David O'Toole

;; Author: David O'Toole(defvar iso-date-format "%Y-%m-%dT%H:%M:%S%z"  <dto@monad.lab>
;; Keywords: convenience

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; 

;;; Code:

(defvar iso-date-format "%Y-%m-%dT%H:%M:%S%z" 
  "Format string for ISO dates.")

(defun iso-timestamp (&optional time)
  (format-time-string iso-date-format
		      (or time (current-time))))

(defun insert-iso-timestamp ()
  (interactive)
  (insert (iso-timestamp)))

(defun iso-timestamp-sexp (&optional time)
  (parse-time-string (iso-timestamp)))

(provide 'iso-date)
;;; iso-date.el ends here
