;;; Saved through ges-version 0.3.3dev at 2003-06-07 15:49
;;; From: cimosque@free.fr (cimosque)
;;; Subject: Load GNOME 2 list of recently used files
;;; Newsgroups: gnu.emacs.sources
;;; Date: 6 Jun 2003 09:13:21 -0700
;;; Organization: http://groups.google.com/

;;; recently-used.el --- load GNOME 2 list of recently used files

;; Author: Matthias Meulien <cimosque@free.fr> 
;; Created: June 5 2003
;; Keywords: customization

;;; Commentary:

;; When enabled, recentf mode will load GNOME 2 recently used files
;; list.  To install and use, put the file on your Emacs-Lisp load
;; path and add the following into your ~/.emacs startup file:
;; 
;; (require 'recently-used) 
;; (recently-used 1)
;; 
;; Todo:
;; 
;; Next step : update `recently-used-file' when killing emacs.

;;; Code:
(require 'recentf)
(require 'xml)
(require 'url)
(require 'url-parse)
(require 'url-file)

(defgroup recently-used nil
  "Load GNOME 2 list of recently used files."
  :group 'recentf
  :version "21.2"
  :prefix 'recently-used)

;;;###autoload
(defcustom recently-used-mode nil
  "Toggle recently-used-mode.

See the command `recently-used-mode'.

Setting this variable directly does not take effect;
use either \\[customize] or the command `recently-used-mode'."
  :set (lambda (symbol value) (recently-used-mode (or value 0)))
  :initialize 'custom-initialize-default
  :version "21.2"
  :type 'boolean
  :group 'recently-used
  :require 'recently-used)

(defcustom recently-used-file
  (convert-standard-filename "~/.recently-used")
  "File used by GNOME to store its list of recently used files."
  :type 'file
  :group 'recently-used)

(defun recently-used-item-cons (item)
  "Returns a cons (NAME . TIME) where NAME and TIME are the values of
the URI and Timestamp tag of ITEM."
  (let* ((url (caddar (xml-get-children item 'URI)))
	 (time (caddar (xml-get-children item 'Timestamp)))
	 (urlobj (url-generic-parse-url (url-cleanup-file url)))
	 (file (url-unhex-string (url-filename urlobj))))
    (cons file time)))

(defun recently-used-alist ()
  "Returns an alist of ELT where ELT has the form \(NAME . TIME).

See `recently-used-file' to change the name of the file containing the
datas."
  (let* ((entries 
	  (cddar (xml-parse-file recently-used-file))))
    (mapcar 'recently-used-item-cons entries)))

;;:###autoload
(defun recently-used-mode (&optional arg)
  "Toggle recently-used-mode.
With arg, turn recently-used-mode on if arg is positive, off otherwise.

This is a global minor mode which load GNOME 2 list of recently used
files, so it can be used by the `recentf' library."
  (interactive)
  (setq recently-used-mode
	(if (null arg) (not recently-used-mode)
	  (> (prefix-numeric-value arg) 0)))
  (if  (not recentf-mode) 
      (recentf-mode))
  (if recently-used-mode
      (setq recentf-list (mapcar 'car (recently-used-alist))
	    recenrf-update-menu-p t)
    (if (file-readable-p recentf-save-file)
	(load-file recentf-save-file))
    (setq recenrf-update-menu-p t)))

(provide 'recently-used)

;;; recently-used.el ends here

