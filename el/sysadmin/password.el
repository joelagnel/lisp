;;; Saved through ges-version 0.3.3dev at 2004-02-20 13:00
;;; ;;; From: Simon Josefsson <jas@extundo.com>
;;; ;;; Subject: password.el: Read passwords from user, possibly using a password
;;; ;;;  cache
;;; ;;; Newsgroups: gnu.emacs.sources
;;; ;;; Date: Tue, 30 Dec 2003 22:39:41 +0100
;;; ;;; Organization: Dept of Numerical Analysis and Computer Science, KTH

;;; [1. application/emacs-lisp; password.el]

;;; password.el --- Read passwords from user, possibly using a password cache.

;; Copyright (C) 1999, 2000, 2003 Free Software Foundation, Inc.

;; Author: Simon Josefsson <simon@josefsson.org>
;; Created: 2003-12-21
;; Keywords: password cache passphrase key

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Greatly influenced by pgg.el written by Daiki Ueno, with timer
;; fixes for XEmacs by Katsumi Yamaoka.  In fact, this is mostly just
;; a rip-off.
;;
;; (password-read "Password? " "test")
;; ;; Minibuffer prompt for password.
;;  => "foo"
;;
;; (password-cache-add "test" "foo")
;;  => nil
;;
;; (password-read "Password? " "test")
;; ;; No minibuffer prompt
;;  => "foo"
;;
;; (password-read "Password? " "test")
;; ;; No minibuffer prompt
;;  => "foo"
;;
;; ;; Wait `password-cache-expiry' seconds.
;;
;; (password-read "Password? " "test")
;; ;; Minibuffer prompt for password is back.
;;  => "foo"

;;; Code:

(autoload 'run-at-time "timer")

(eval-when-compile
  (require 'cl))

(defcustom password-cache t
  "Whether to cache passwords."
  :group 'password
  :type 'boolean)

(defcustom password-cache-expiry 16
  "How many seconds passwords are cached, or nil to disable expiring.
Whether passwords are cached at all is controlled by `password-cache'."
  :group 'password
  :type '(choice (const :tag "Never" nil)
		 (integer :tag "Seconds")))

(defvar password-data (make-vector 7 0))

(defun password-read (prompt &optional key)
  "Read password, for use with KEY, from user, or from cache if wanted.
KEY indicate the purpose of the password, so the cache can
separate passwords.  The cache is not used if KEY is nil.  It is
typically a string.
The variable `password-cache' control whether the cache is used."
  (or (and password-cache
	   key
	   (symbol-value (intern-soft key password-data)))
      (read-passwd prompt)))

(eval-when-compile
  (defvar itimer-process)
  (defvar itimer-timer)
  (autoload 'delete-itimer "itimer")
  (autoload 'itimer-driver-start "itimer")
  (autoload 'itimer-value "itimer")
  (autoload 'set-itimer-function "itimer")
  (autoload 'set-itimer-function-arguments "itimer")
  (autoload 'set-itimer-restart "itimer")
  (autoload 'start-itimer "itimer"))

(eval-and-compile
  (defalias
    'password-run-at-time
    (if (featurep 'xemacs)
	(if (condition-case nil
		(progn
		  (unless (or itimer-process itimer-timer)
		    (itimer-driver-start))
		  ;; Check whether there is a bug to which the difference of
		  ;; the present time and the time when the itimer driver was
		  ;; woken up is subtracted from the initial itimer value.
		  (let* ((inhibit-quit t)
			 (ctime (current-time))
			 (itimer-timer-last-wakeup
			  (prog1
			      ctime
			    (setcar ctime (1- (car ctime)))))
			 (itimer-list nil)
			 (itimer (start-itimer "password-run-at-time" 'ignore 5)))
		    (sleep-for 0.1) ;; Accept the timeout interrupt.
		    (prog1
			(> (itimer-value itimer) 0)
		      (delete-itimer itimer))))
	      (error nil))
	    (lambda (time repeat function &rest args)
	      "Emulating function run as `run-at-time'.
TIME should be nil meaning now, or a number of seconds from now.
Return an itimer object which can be used in either `delete-itimer'
or `cancel-timer'."
	      (apply #'start-itimer "password-run-at-time"
		     function (if time (max time 1e-9) 1e-9)
		     repeat nil t args))
	  (lambda (time repeat function &rest args)
	    "Emulating function run as `run-at-time' in the right way.
TIME should be nil meaning now, or a number of seconds from now.
Return an itimer object which can be used in either `delete-itimer'
or `cancel-timer'."
	    (let ((itimers (list nil)))
	      (setcar
	       itimers
	       (apply #'start-itimer "password-run-at-time"
		      (lambda (itimers repeat function &rest args)
			(let ((itimer (car itimers)))
			  (if repeat
			      (progn
				(set-itimer-function
				 itimer
				 (lambda (itimer repeat function &rest args)
				   (set-itimer-restart itimer repeat)
				   (set-itimer-function itimer function)
				   (set-itimer-function-arguments itimer args)
				   (apply function args)))
				(set-itimer-function-arguments
				 itimer
				 (append (list itimer repeat function) args)))
			    (set-itimer-function
			     itimer
			     (lambda (itimer function &rest args)
			       (delete-itimer itimer)
			       (apply function args)))
			    (set-itimer-function-arguments
			     itimer
			     (append (list itimer function) args)))))
		      1e-9 (if time (max time 1e-9) 1e-9)
		      nil t itimers repeat function args)))))
      'run-at-time)))

(defun password-cache-remove (key)
  "Remove password indexed by KEY from password cache.
This is typically run be a timer setup from `password-cache-add',
but can be invoked at any time to forcefully remove passwords
from the cache.  This may be useful when it has been detected
that a password is invalid, so that `password-read' query the
user again."
  (let ((password (symbol-value (intern-soft key password-data))))
    (when password
      (fillarray password ?_)
      (unintern key password-data))))

(defun password-cache-add (key password)
  "Add password to cache.
The password is removed by a timer after `password-cache-expiry'
seconds."
  (set (intern key password-data) password)
  (when password-cache-expiry
    (password-run-at-time password-cache-expiry nil
			  #'password-cache-remove
			  key))
  nil)

(provide 'password)

;;; password.el ends here

