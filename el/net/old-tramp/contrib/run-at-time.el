;;; run-at-time.el --- A non-buggy version of the run-at-time function

;; Copyright (C) 1999, 2000, 2003 Free Software Foundation, Inc.

;; Author: Katsumi Yamaoka  <yamaoka@jpl.org>

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

;; XEmacs has a buggy version of run-at-time.  This file defines a
;; non-buggy version of the same.

(defalias
  'run-at-time
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
		   (itimer (start-itimer "run-at-time" 'ignore 5)))
	      (sleep-for 0.1) ;; Accept the timeout interrupt.
	      (prog1
		  (> (itimer-value itimer) 0)
		(delete-itimer itimer))))
	(error nil))
      (lambda (time repeat function &rest args)
	"Function emulating the function of the same name of Emacs.
TIME should be nil meaning now, or a number of seconds from now.
Return an itimer object which can be used in either `delete-itimer'
or `cancel-timer'."
	(apply #'start-itimer "run-at-time"
	       function (if time (max time 1e-9) 1e-9)
	       repeat nil t args))
    (lambda (time repeat function &rest args)
      "Function emulating the function of the same name of Emacs.
It works correctly for TIME even if there is a bug in the XEmacs core.
TIME should be nil meaning now, or a number of seconds from now.
Return an itimer object which can be used in either `delete-itimer'
or `cancel-timer'."
      (let ((itimers (list nil)))
	(setcar
	 itimers
	 (apply #'start-itimer "fixed-run-at-time"
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
		nil t itimers repeat function args))))))

(provide 'run-at-time)

;;; run-at-time.el ends here
