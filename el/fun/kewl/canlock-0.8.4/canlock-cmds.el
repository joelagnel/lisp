;;; canlock-cmds.el --- application commands of Canlock

;; Copyright (C) 1998, 1999, 2001 Katsumi Yamaoka
;; Copyright (C) 1998, 1999, 2001 Yuuichi Teranishi
;; Copyright (C) 1998, 1999, 2001 Hideyuki SHIRAI
;; Copyright (C) 1998, 1999, 2001 Hidekazu Nakamura

;; Author: Katsumi Yamaoka   <yamaoka@jpl.org>
;;         Yuuichi Teranishi <teranisi@gohome.org>
;;         Hideyuki SHIRAI   <shirai@meadowy.org>
;;         Hidekazu Nakamura <u90121@uis-inf.co.jp>
;; Keywords: canlock, cmail, mew, mh-e, rmail, vm, wanderlust

;; This file is part of Canlock.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This file contains the application commands of Canlock to verify
;; Cancel-Lock and/or Cancel-Key header for the various message user
;; agents.  Those commands used to be included in canlock.el, but now
;; canlock.el becomes just a library for Cancel-Lock feature.

;; Note that there is no command for Gnus, since canlock.el and the
;; command `gnus-article-verify-cancel-lock' have already been
;; implemented in the most recent Gnus.  It means that you don't have
;; bother to install Canlock package if you use such a version of Gnus.

;;; Code:

(eval-when-compile
  (if (boundp 'MULE)
      ;; Load special macros for compiling this file.
      (load "canlock-om.el" nil t t)))

(if (boundp 'MULE)
    (require 'canlock-om))

(require 'canlock)

;; Compile warnings.
(eval-when-compile
  (autoload 'cmail-folder-buffer "cmail-misc")
  (autoload 'cmail-get-page-number-from-summary "cmail-misc")
  (autoload 'cmail-n-page "cmail-misc")
  (autoload 'mh-header-display "mh-e" nil t)
  (autoload 'mew-buffer-message "mew")
  (autoload 'mew-summary-display "mew-summary")
  (autoload 'rmail-summary-rmail-update "rmailsum")
  (autoload 'vm-follow-summary-cursor "vm-motion")
  (autoload 'vm-text-of "vm-message")
  (autoload 'wl-message-get-original-buffer "wl-message")
  (autoload 'wl-summary-set-message-buffer-or-redisplay "wl-summary")
  (defvar cmail-current-folder)
  (defvar mh-show-buffer)
  (defvar rmail-buffer)
  (defvar vm-mail-buffer)
  (defvar vm-message-pointer))

;;;###autoload
(defun wl-summary-canlock-verify ()
  "Run `canlock-verify' from Wanderlust summary buffer."
  (interactive)
  (save-excursion
    (wl-summary-set-message-buffer-or-redisplay)
    (canlock-verify (wl-message-get-original-buffer))))

(eval-when-compile
  (autoload 'ad-arglist "advice"))

(eval-when-compile
  (defmacro canlock-mew-summary-display ()
    "Call `mew-summary-display' with the arg t if it is certainly allowed,
otherwise do try&error to call the function with or without arg."
    (if (and
	 (or (and (fboundp 'mew-summary-display)
		  (not (eq (car-safe (symbol-function 'mew-summary-display))
			   'autoload)))
	     (and (condition-case code
		      (load "mew-summary")
		    (error
		     (message "%s (ignored)" (error-message-string code))
		     nil))
		  (fboundp 'mew-summary-display)
		  (not (eq (car-safe (symbol-function 'mew-summary-display))
			   'autoload))))
	 (> (length (ad-arglist (symbol-function 'mew-summary-display)))
	    0))
	(progn
	  ;; Mew 1.94b20 or later
	  (message "\
`mew-summary-canlock-verify' will call `mew-summary-display' with the arg t")
	  '(mew-summary-display t))
      '(condition-case nil
	   (mew-summary-display)
	 (wrong-number-of-arguments
	  (mew-summary-display t))))))

;;;###autoload
(defun mew-summary-canlock-verify ()
  "Run `canlock-verify' from Mew summary buffer."
  (interactive)
  (canlock-mew-summary-display)
  (canlock-verify (mew-buffer-message)))

;;;###autoload
(defun mh-summary-canlock-verify ()
  "Run `canlock-verify' from MH folder buffer."
  (interactive)
  (mh-header-display)
  (canlock-verify mh-show-buffer))

;;;###autoload
(defun vm-summary-canlock-verify ()
  "Run `canlock-verify' from VM summary buffer."
  (interactive)
  (vm-follow-summary-cursor)
  (if (and vm-mail-buffer (buffer-name vm-mail-buffer))
      (save-excursion
	(set-buffer vm-mail-buffer)
	(let* ((mp (car vm-message-pointer))
	       (header (save-restriction
			 (widen)
			 (buffer-substring
			  (aref (aref mp 0) 0) (vm-text-of mp)))))
	  (with-temp-buffer
	    (insert header)
	    (canlock-verify))))
    (or canlock-ignore-errors
	(error "Folder buffer has been killed."))))

;;;###autoload
(defun cmail-summary-canlock-verify ()
  "Run `canlock-verify' from cmail summary buffer."
  (interactive)
  (let* ((page (cmail-get-page-number-from-summary))
	 (header (save-excursion
		   (set-buffer (cmail-folder-buffer cmail-current-folder))
		   (cmail-n-page page)
		   (buffer-substring (point)
				     (if (search-forward "\n\n" nil t)
					 (1- (point))
				       (point-max))))))
    (with-temp-buffer
      (insert header)
      (canlock-verify))))

;;;###autoload
(defun rmail-summary-canlock-verify ()
  "Run `canlock-verify' from RMAIL summary buffer."
  (interactive)
  (rmail-summary-rmail-update)
  (let ((header (save-excursion
		  (set-buffer rmail-buffer)
		  (goto-char (point-min))
		  (save-restriction
		    (widen)
		    (search-backward "\n\C-_\C-l\n") ;; ^_^L
		    (re-search-forward "^[^\t\n ]+:")
		    (buffer-substring
		     (goto-char (match-beginning 0))
		     (progn (search-forward "\n\n")
			    (1- (point))))))))
    (with-temp-buffer
      (insert header)
      (canlock-verify))))

(provide 'canlock-cmds)

;;; canlock-cmds.el ends here
