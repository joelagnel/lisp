;;; Saved through ges-version 0.3.3dev at 2003-04-17 09:34
;;; ;;; ;;; ;;; From: sandipchitale@yahoo.com (Sandip Chitale)
;;; ;;; ;;; ;;; Subject: launch-program.el --- launch external programs for registered file extensions
;;; ;;; ;;; ;;; Newsgroups: gnu.emacs.sources
;;; ;;; ;;; ;;; Date: 16 Apr 2003 16:39:37 -0700
;;; ;;; ;;; ;;; Organization: http://groups.google.com/

;;; ;;; ;;; launch-program.el --- launch external programs for registered file
;;; ;;; extensions

;;; ;;; ;; Author: Sandip V. Chitale
;;; ;;; ;; Created: Apr 16 2003
;;; ;;; ;; Keywords: launch external programs

;;; ;;; ;; This file is not part of GNU Emacs yet.

;;; ;;; Commentary:
;;; ;;
;;; ;; Many application files (e.g. .pdf) can be opened by Emacs but it
;;; may not be very
;;; ;; useful. It is better to let the external program, associated with
;;; that type of file,
;;; ;; open it. For example .pdf files should be opened by Acrobat Reader.
;;; ;; This package achieves that.
 
;;; ;; To install and use, put the file on your Emacs-Lisp load path and
;;; add the
;;; ;; following into your ~/.emacs startup file:
;;; ;;
;;; ;;  (require 'launch-program)
;;; ;;
;;; ;; Also customize the `launch-program-extensions-list' variable to
;;; register extensions for
;;; ;; which external program should be launched.
;;; ;;
;;; Code:
(require 'dired)
(require 'browse-url)

(defcustom launch-program-extensions-list nil
  "File extensions (without leading period) to check for
dired-find-file-launch-program"
  :group 'dired
  :type '(repeat (string :tag "Extension (without leading period)")))

(defadvice dired-find-file (around dired-find-file-launch-program
activate)
  "Advice function to launch external programs for registered file
extensions."
    (if (not (launch-program (expand-file-name
(file-name-sans-versions (dired-get-filename) t))))
	ad-do-it))

(defadvice find-file (around find-file-launch-program (filename
&optional wildcards) activate)
  "Advice function to launch external programs for registered file
extensions."
  (interactive)
    (if (not (launch-program (expand-file-name filename)))
	ad-do-it))

(defadvice find-file-other-frame (around find-file-launch-program
(filename &optional wildcards) activate)
  "Advice function to launch external programs for registered file
extensions."
  (interactive)
    (if (not (launch-program (expand-file-name filename)))
	ad-do-it))

(defadvice find-file-other-window (around find-file-launch-program
(filename &optional wildcards) activate)
  "Advice function to launch external programs for registered file
extensions."
  (interactive)
    (if (not (launch-program (expand-file-name filename)))
	ad-do-it))

(defun launch-program (file-name)
  "Launch an associated program for registered file extensions."
  (interactive "s")
  (let ((file-extension (file-name-extension file-name)))
    (if (and file-extension
	     (member file-extension launch-program-extensions-list))
	(progn 
	  (browse-url-of-file file-name)
	  t)
	nil)))

(provide 'launch-program)

