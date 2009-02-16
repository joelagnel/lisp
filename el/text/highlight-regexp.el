;;; highlight-regexp.el --- temporary highlighting regular expressions.

;; Copyright (C) 2002 Olaf Sylvester

;; Author: Olaf Sylvester <olaf@geekware.de>
;; Maintainer: Olaf Sylvester <olaf@geekware.de>
;; Keywords: convenience
;; Version: 0.11
;; Date: 28.04.2002
;; X-URL: http://www.geekware.de/software/emacs

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
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Provide some functions for highlighting regular expressions 
;; in current buffer and a convenience function for highlighting
;; current word `highlight-regexp-current-word'.
;; 
;; This package offers default key bindings:
;;  C-c h w `highlight-regexp-current-word'
;;  C-c h r `highlight-regexp-regexp'
;;  C-c h s `highlight-regexp-string'
;;  C-c h c `highlight-regexp-clear'
;;  C-c h d `highlight-regexp-delete'
;; You can disable these key bindings.
;; 
;; By recentering the display with `C-l' all highlighted instances will be 
;; deleted. You can disable this feature.

;;; Customization:

;; Use `customize-highlight-regexp' for customization.

;;; Code:

(require 'advice)

(defgroup highlight-regexp nil
  "Functions for highlighting regular expressions."
  :prefix "highlight-regexp-"
  :group 'convenience)

;;; Variables:

(defcustom highlight-regexp-recenter-clears-p t
  "*If t, calling buildin function `recenter' will 
call `highlight-regexp-clear' to delete all highlighted area."
  :group 'highlight-regexp
  :type 'boolean)

(defcustom highlight-regexp-faces '(secondary-selection 
				    show-paren-match-face)
  "*List of face names used one after the other."
  :group 'highlight-regexp
  :type '(repeat face))

(defcustom highlight-regexp-use-bindings-p t
  "*If t, the default key bindings of highlight-regexp will be used."
  :group 'highlight-regexp
  :type 'boolean)


(defvar highlight-regexp--face-index 0
  "Current index for face selection from `highlight-regexp-facess'.")

(defvar highlight-regexp--history nil
  "History of regexps.")

(defvar highlight-regexp--instance-number 0)

;;; Functions:

;;;###autoload
(defun customize-highlight-regexp ()
  "Customize group `highlight-regexp'."
  (interactive)
  (customize-group 'highlight-regexp))

(defun highlight-regexp-get-current-word ()
  "Return word under cursor for `highlight-regexp-current-word'."
  (current-word))

;;;###autoload
(defun highlight-regexp-current-word ()
  "Highlight all occurences of word under cursor."
  (interactive)
  (highlight-regexp-regexp 
   (concat "\\<" 
	   (regexp-quote (highlight-regexp-get-current-word))
	   "\\>")))

;;;###autoload
(defun highlight-regexp-string (string &optional face)
  "Highlight all occurences of STRING."
  (interactive "sString: ")
  (highlight-regexp-regexp (regexp-quote string) face))

;;;###autoload
(defun highlight-regexp-regexp (&optional regexp face)
  "Highlight all occurences of REGEXP in current buffer."
  (interactive 
   (list
    (read-from-minibuffer "Regexp: "
			  (cons "" 1)
			  nil nil 'highlight-regexp--history)))
  (save-excursion
    (let ((n 0)
	  (index (setq highlight-regexp--instance-number
		       (1+ highlight-regexp--instance-number)))
	  (face (or face
		    (prog1 
			(nth highlight-regexp--face-index
			     highlight-regexp-faces)
		      (setq highlight-regexp--face-index
			    (if (< (1+ highlight-regexp--face-index)
				   (length highlight-regexp-faces))
				(1+ highlight-regexp--face-index)
			      0))))))
      (goto-char (point-min))
      (while (search-forward-regexp regexp nil t)
	(let ((ol (make-overlay (match-beginning 0)
				(match-end 0))))
	  (setq n (1+ n))
	  (overlay-put ol 'face face)
	  (overlay-put ol 'highlight-regexp-instance index)))
      (message "Found %s occurences of %s." n regexp))))

;;;###autoload
(defun highlight-regexp-clear (&optional instance-number)
  "Delete all highlighted areas which were generated 
by `highlight-regexp-regexp'."
  (interactive)
  (let ((n 0)
	(overlays-tupel (overlay-lists)))
    (let ((overlays-tupel (list (car overlays-tupel)
				(cdr overlays-tupel))))
      (while overlays-tupel
	(let ((overlays (car overlays-tupel)))
	  (while overlays
	    (let ((instance (overlay-get (car overlays) 'highlight-regexp-instance)))
	      (if (and (not (null instance))
		       (or (null instance-number)
			   (eq instance-number instance)))
		  (progn 
		    (setq n (1+ n))
		    (delete-overlay (car overlays)))))
	    (setq overlays (cdr overlays))))
	(setq overlays-tupel (cdr overlays-tupel))))
    n))

;;;###autoload
(defun highlight-regexp-delete ()
  "Delete all highlighted areas which have the same
instance as the highlighted area at current point."
  (interactive)
  (let ((n 0)
	(overlays (overlays-at (point))))
    (while overlays
      (let ((instance (overlay-get (car overlays) 'highlight-regexp-instance)))
	(if instance
	    (setq n (+ n (highlight-regexp-clear instance)))))
      (setq overlays (cdr overlays)))
    (message "Deleted %s highlights." n)))

;;;###autoload
(defadvice recenter (before hlr first activate)
  (if highlight-regexp-recenter-clears-p
      (highlight-regexp-clear)))

;;;###autoload
(when highlight-regexp-use-bindings-p
  (global-set-key "\C-chw" 'highlight-regexp-current-word)
  (global-set-key "\C-chr" 'highlight-regexp-regexp)
  (global-set-key "\C-chs" 'highlight-regexp-string)
  (global-set-key "\C-chc" 'highlight-regexp-clear)
  (global-set-key "\C-chd" 'highlight-regexp-delete)
  )


(provide 'highlight-regexp)

;;; highlight-regexp.el ends here
