;;; cups.el -- Interface for setting "default-printer" from cups list
;;; of printers.

;; Filename: cups.el
;; Copyright (C) 2005 Jesse Rosenthal
;; Author: Jesse Rosenthal <jesse.k.rosenthal@gmail.com>
;; Maintainer: Jesse Rosenthal <jesse.k.rosenthal@gmail.com>
;; Created: 30 Oct 2005
;; Description: Allows you to set "printer-name" from the cups 
;;              list of printers.
;; Version 0.2

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

;; If you have cups on your *nix machine, this script will read your
;; conf file (usually "/etc/cups/printers.conf") and allow you to
;; change your default printer to any one, via the included
;; "choose-printer" command. This has tab completion of printer names,
;; so it's useful if you have a lot of printers -- I use it mainly
;; prior to using the dvips command in AUCTeX, but it's also useful
;; for quick printing of plain text.

;; This also has an optional menu entry. It's on by default,
;; but you can turn it off in your .emacs. See below. 
;;
;; UPDATE: I've now added a simple cache (set as ~/.emacs-cups-cache),
;; which just saves the printer list if the cups conf file hasn't
;; changed. This means that this should only slow down your startup
;; (*if* it slows down your startup) when you change your cups
;; settings.

;; To use, put the following in your ~/.emacs:
;;
;; (setq printer-conf-file "/path/to/cups/printers.conf")
;; (setq cups-printer-menu t) ;set to `nil' if you don't want menus.
;; (load-file "/path/to/this/cups.el")
;;
;; and then just run M-x choose-printer.

;; v0.1 - 30 0ct 2005 
;;    * First version 
;;
;; v0.2 - 15 Nov 2005 - 
;;    * Added radio buttons to the menu.
;;    * Cleaned up the menu code
;;    * Added a cache file so it won't slow down startup more than
;;      once.



;;; Code:

(defvar printer-conf-file "/etc/cups/printers.conf")
(defconst cups-cache "~/.emacs-cups-cache")
(defvar cups-printer-menu t)



(defun set-printer-vars (var)
  (setq printer-name var)
  (setq ps-printer-name var))


(defun choose-printer ()
  (interactive)
  (let ((my-printer (printer-prompt)))
    (set-printer-vars my-printer)
    (message (format "Printer %s chosen." my-printer))))

(defun printer-prompt ()
  (let* ((default-printer (if (null printer-name)
			      (car printer-list)
			    printer-name))
	 (prompt (format "Choose printer (default %s): " default-printer))
	 (chosen-printer
	  (completing-read
	   prompt
	   printer-list nil t nil nil default-printer)))
    chosen-printer))

(defun cups-list-update-cache (printer-conf-file)
  "This takes the printer-list stored in parameter file (usual is
/etc/cups/printers.conf) and returns a list of values, with the
default printer first. This makes a new cache."
  (save-excursion
    (with-temp-buffer
      (insert-file-contents printer-conf-file)
      (parse-printer-info)
      (write-region (point-min) (point-max) cups-cache)
      (line-lister))))

(defun cups-list-cached (printer-conf-file)
  "This takes the printer-list stored in parameter file (usual is
/etc/cups/printers.conf) and returns a list of values, with the
default printer first."
  (save-excursion
    (with-temp-buffer
      (insert-file-contents cups-cache)
      (line-lister))))



(defun parse-printer-info ()
  ;; This lists the printers by their cups name on the *printer-list*
  ;; buffer. The default printer is put last, so it will be first in
  ;; the list.
  (save-excursion
    (goto-char (point-min))
    (dotimes (i (count-lines (point-min) (point-max)))
      (cond ((looking-at "^<[^/]*Printer.*>$")
	     (next-line))
	    (t 
	     (kill-line 1))))
    ;; this should put the default last, since it's in reverse, and
    ;; 'D' comes before 'P'
    (sort-lines -1 (point-min) (point-max)) 
    (goto-char (point-min))
    (perform-replace "^\\(.*Printer \\)\\(.*\\)\\(>$\\)" "\\2" nil t nil)))


(defun line-lister()
  (save-excursion
    (let ((x nil) (line-number 1))
      (dotimes (i (count-lines (point-min)(point-max)))
	(goto-line line-number)
	(setf x (cons (current-word) x))
	(incf line-number))
      x)))


;; (defun cups-make-menu-entry (printer)
;;   (vector printer `(setq printer-name ,printer) t))

(defun cups-make-menu-entry (printer)
  (vector printer `(set-printer-vars ,printer)
	  ':style 'radio ':selected `(string= printer-name ,printer)))


(defun cups-menu-generate ()
  (when (not (featurep 'easymenu))
    (require 'easymenu))
  (easy-menu-add-item nil '("file") 
		      (cons "Printer" 
			    (mapcar #'cups-make-menu-entry printer-list))
		      "Print Buffer"))


(if (file-newer-than-file-p cups-cache printer-conf-file)
    (setq printer-list (cups-list-cached printer-conf-file))
  (setq printer-list (cups-list-update-cache printer-conf-file)))
(set-printer-vars (car printer-list))


(when cups-printer-menu
  (cups-menu-generate))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; End cups printer chooser
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;







