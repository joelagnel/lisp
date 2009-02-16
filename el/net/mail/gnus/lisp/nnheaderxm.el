;;; nnheaderxm.el --- making Gnus backends work under XEmacs

;; Copyright (C) 1996, 1997, 1998, 1999, 2000, 2001, 2003
;;      Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: news

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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

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
		 (itimer (start-itimer "nnheader-run-at-time" 'ignore 5)))
	    (sleep-for 0.1) ;; Accept the timeout interrupt.
	    (prog1
		(> (itimer-value itimer) 0)
	      (delete-itimer itimer))))
      (error nil))
    (defun nnheader-xmas-run-at-time (time repeat function &rest args)
      "Emulating function run as `run-at-time'.
TIME should be nil meaning now, or a number of seconds from now.
Return an itimer object which can be used in either `delete-itimer'
or `cancel-timer'."
      (apply #'start-itimer "nnheader-run-at-time"
	     function (if time (max time 1e-9) 1e-9)
	     repeat nil t args))
  (defun nnheader-xmas-run-at-time (time repeat function &rest args)
    "Emulating function run as `run-at-time' in the right way.
TIME should be nil meaning now, or a number of seconds from now.
Return an itimer object which can be used in either `delete-itimer'
or `cancel-timer'."
    (let ((itimers (list nil)))
      (setcar
       itimers
       (apply #'start-itimer "nnheader-run-at-time"
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

(defalias 'nnheader-run-at-time 'nnheader-xmas-run-at-time)
(defalias 'nnheader-cancel-timer 'delete-itimer)
(defalias 'nnheader-cancel-function-timers 'ignore)
(defalias 'nnheader-string-as-multibyte 'identity)

(provide 'nnheaderxm)

;;; arch-tag: ee2b3387-d3ca-4de6-9b64-304d838706dd
;;; nnheaderxm.el ends here
