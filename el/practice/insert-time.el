;;; $Id: insert-time.el 291 2007-01-31 03:22:20Z ryan $
;; insert time and date stamps at point

;; Copyright (C) 2001-2007 Ryan McGeary
;; Author: Ryan McGeary
;; Keywords: time date insert format

;; This code is free; you can redistribute it and/or modify it under the
;; terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.

;;; Commentary:
;;
;; Purpose
;; -------
;; insert-time provides an easy way to insert time and date stamps into
;; your files
;;
;; Usage
;; -----
;; Example of lines to be added to your .emacs:
;;
;;     (require 'insert-time)
;;
;;     ; in case you don't like the defaults
;;     ; (see `format-time-string' for format)
;;     (setq insert-date-format "%Y-%m-%d")
;;     (setq insert-time-format "%H:%M:%S")
;;
;;     ; keyboard shortcuts
;;     (define-key global-map [(control c)(d)] 'insert-date-time)
;;     (define-key global-map [(control c)(control v)(d)] 'insert-personal-time-stamp)
;;
;; How it works
;; ------------
;; just a few inserts using the emacs built in format-time-string
;;
;; Limitations
;; -----------
;;

;;; History:
;;
;; 30-Jan-2007 - defvar -> defcustom
;; 25-Mar-2001 - created by
;;               Ryan McGeary

;;; Code:

(defcustom insert-date-format "%Y-%m-%d"
  "*Format for `insert-date' and `insert-date-time' (see
  `format-time-string' for how to format).")

(defcustom insert-time-format "%H:%M:%S %z"
  "*Format for `insert-time' and `insert-date-time' (see
  `format-time-string' for how to format).")

(defun insert-date ()
  "Inserts the current date at point in the format specified by
`insert-date-format'."
  (interactive "*")
  (insert (format-time-string insert-date-format)))

(defun insert-time ()
  "Inserts the current time at point in the format specified by
`insert-time-format'."
  (interactive "*")
  (insert (format-time-string insert-time-format)))

(defun insert-date-time ()
  "Inserts the current date-time combo at point in the formats
specified by `insert-date-format' and `insert-time-format'.  Uses
`insert-date' and `insert-time'."
  (interactive "*")
  (insert-date) (insert " ") (insert-time))

(defun insert-personal-time-stamp ()
  "Inserts the current time-stamp at point for the current user
in the formats specified by `insert-date-format' and
`insert-time-format'.  It inserts a timestamp of
\"(<`user-login-name'> : <`insert-date-format'>
<`insert-time-format'>)\" Uses `insert-date-time'."
  (interactive "*")
  (insert "(" user-login-name " : ") (insert-date-time) (insert ")"))

(provide 'insert-time)
