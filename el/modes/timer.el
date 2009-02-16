;;; timer.el --- run a function with args at some time in future.

;; Copyright (C) 1996 Free Software Foundation, Inc.

;; Maintainer: FSF

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

;; This package gives you the capability to run Emacs Lisp commands at
;; specified times in the future, either as one-shots or periodically.

;;; Code:

(require 'itimer)

(fset 'timer-create 'make-itimer)

(fset 'timerp 'itimerp)

;(defvar timer-idle-list nil
;  "List of active idle-time timers in order of increasing time")
(defvaralias 'timer-idle-list 'itimer-list)
(defvaralias 'timer-list 'itimer-list)


(defun timer-set-time (timer time &optional delta)
  "Set the trigger time of TIMER to TIME.
TIME must be in the internal format returned by, e.g., `current-time'.
If optional third argument DELTA is a non-zero integer, make the timer
fire repeatedly that many seconds apart."
  (set-itimer-value timer (itimer-time-difference time (current-time)))
  (and delta (check-nonnegative-number delta))
  (and delta (set-itimer-restart timer delta))
  timer)

(defun timer-set-idle-time (timer secs &optional repeat)
  "Set the trigger idle time of TIMER to SECS.
If optional third argument REPEAT is non-nil, make the timer
fire each time Emacs is idle for that many seconds."
  (set-itimer-is-idle timer t)
  (set-itimer-value timer secs)
  (when repeat
    (set-itimer-restart timer secs))
  timer)

(defun timer-relative-time (time secs &optional usecs)
  "Advance TIME by SECS seconds and optionally USECS microseconds.
SECS may be a fraction."
  (let ((high (car time))
	(low (if (consp (cdr time)) (nth 1 time) (cdr time)))
	(micro (if (numberp (car-safe (cdr-safe (cdr time))))
		   (nth 2 time)
		 0)))
    ;; Add
    (if usecs (setq micro (+ micro usecs)))
    (if (floatp secs)
	(setq micro (+ micro (floor (* 1000000 (- secs (floor secs)))))))
    (setq low (+ low (floor secs)))

    ;; Normalize
    (setq low (+ low (/ micro 1000000)))
    (setq micro (mod micro 1000000))
    (setq high (+ high (/ low 65536)))
    (setq low (logand low 65535))

    (list high low (and (/= micro 0) micro))))

(defun timer-inc-time (timer secs &optional usecs)
  "Increment the time set in TIMER by SECS seconds and USECS microseconds.
SECS may be a fraction."
  (let ((time (itimer-value timer)))
    (setq time (+ time secs (if (and usecs (fboundp 'lisp-float-type))
				(/ usecs (float 1000000))
			      0)))
    (set-itimer-value timer time)))

(defun timer-set-time-with-usecs (timer time usecs &optional delta)
  "Set the trigger time of TIMER to TIME.
TIME must be in the internal format returned by, e.g., `current-time'.
If optional third argument DELTA is a non-zero integer, make the timer
fire repeatedly that many seconds apart."
  (let ((list (list nil nil nil)))
    (setcar list (car time))
    (setcar (nthcdr 1 list) (if (consp (cdr time))
				(car (cdr time))
			      (cdr time)))
    (setcar (nthcdr 2 list) usecs)
    (set-itimer-value timer (itimer-time-difference list (current-time)))
    (set-itimer-restart timer delta)
    timer))

(defun timer-set-function (timer function &optional args)
  "Make TIMER call FUNCTION with optional ARGS when triggering."
  (set-itimer-function timer function)
  (set-itimer-function-arguments timer args)
  (set-itimer-uses-arguments timer t)
  timer)

(defun timer-activate (timer)
  "Put TIMER on the list of active timers."
  (activate-itimer timer))

(defun timer-activate-when-idle (timer)
  "Arrange to activate TIMER whenever Emacs is next idle."
  (set-itimer-is-idle timer t)
  (set-itimer-uses-arguments timer nil)
  ;(unless (memq timer timer-idle-list)
    ;(setq timer-idle-list (cons timer timer-idle-list)))
  (activate-itimer timer))

;; can't do this, different kind of timer
;;(defalias 'disable-timeout 'cancel-timer)

(defun cancel-timer (timer)
  "Remove TIMER from the list of active timers."
  ;(setq timer-idle-list (delq timer timer-idle-list))
  (delete-itimer timer))

(defun cancel-function-timers (function)
  "Cancel all timers scheduled by `run-at-time' which would run FUNCTION."
  (interactive "aCancel timers of function: ")
  (let ((p itimer-list))
    (while p
      (if (eq function (itimer-function p))
	  (progn
	    (setq p (cdr p))
	    (delete-itimer (car p)))
	(setq p (cdr p))))))

;;;###autoload
(defun run-at-time (time repeat function &rest args)
  "Perform an action after a delay of SECS seconds.
Repeat the action every REPEAT seconds, if REPEAT is non-nil.
TIME should be a string like \"11:23pm\", nil meaning now, a number of seconds
from now, or a value from `encode-time'.
REPEAT may be an integer or floating point number.
The action is to call FUNCTION with arguments ARGS.

This function returns a timer object which you can use in `cancel-timer'."
  (interactive "sRun at time: \nNRepeat interval: \naFunction: ")

  ;; Special case: nil means "now" and is useful when repeating.
  (if (null time)
      (setq time (current-time)))

  ;; Handle numbers as relative times in seconds.
  (if (numberp time)
      (setq time (timer-relative-time (current-time) time)))

  ;; Handle relative times like "2 hours and 35 minutes"
  (if (stringp time)
      (let ((secs (timer-duration time)))
	(if secs
	    (setq time (timer-relative-time (current-time) secs)))))

  ;; Handle "11:23pm" and the like.  Interpret it as meaning today
  ;; which admittedly is rather stupid if we have passed that time
  ;; already.  (Though only Emacs hackers hack Emacs at that time.)
  (if (stringp time)
      (progn
	(require 'diary-lib)
	(let ((hhmm (diary-entry-time time))
	      (now (decode-time)))
	  (if (>= hhmm 0)
	      (setq time
		    (encode-time 0 (% hhmm 100) (/ hhmm 100) (nth 3 now)
				 (nth 4 now) (nth 5 now) (nth 8 now)))))))

  (or (consp time)
      (error "Invalid time format"))

  (or (null repeat)
      (numberp repeat)
      (error "Invalid repetition interval"))

  (let ((timer (timer-create)))
    (timer-set-time timer time repeat)
    (timer-set-function timer function args)
    (timer-activate timer)
    timer))

;;;###autoload
(defun run-with-timer (secs repeat function &rest args)
  "Perform an action after a delay of SECS seconds.
Repeat the action every REPEAT seconds, if REPEAT is non-nil.
SECS and REPEAT may be integers or floating point numbers.
The action is to call FUNCTION with arguments ARGS.

This function returns a timer object which you can use in `cancel-timer'."
  (interactive "sRun after delay (seconds): \nNRepeat interval: \naFunction: ")
  (apply 'run-at-time secs repeat function args))

;;;###autoload
(defun run-with-idle-timer (secs repeat function &rest args)
  "Perform an action the next time Emacs is idle for SECS seconds.
If REPEAT is non-nil, do this each time Emacs is idle for SECS seconds.
SECS may be an integer or a floating point number.
The action is to call FUNCTION with arguments ARGS.

This function returns a timer object which you can use in `cancel-timer'."
  (interactive
   (list (read-from-minibuffer "Run after idle (seconds): " nil nil t)
	 (y-or-n-p "Repeat each time Emacs is idle? ")
	 (intern (completing-read "Function: " obarray 'fboundp t))))
  (let ((timer (timer-create)))
    (timer-set-function timer function args)
    (timer-set-idle-time timer secs repeat)
    (timer-activate-when-idle timer)
    timer))

(defun with-timeout-handler (tag)
  (throw tag 'timeout))

;;;###autoload (put 'with-timeout 'lisp-indent-function 1)

;;;###autoload
(defmacro with-timeout (list &rest body)
  "Run BODY, but if it doesn't finish in SECONDS seconds, give up.
If we give up, we run the TIMEOUT-FORMS and return the value of the last one.
The call should look like:
 (with-timeout (SECONDS TIMEOUT-FORMS...) BODY...)
The timeout is checked whenever Emacs waits for some kind of external
event \(such as keyboard input, input from subprocesses, or a certain time);
if the program loops without waiting in any way, the timeout will not
be detected."
  (let ((seconds (car list))
	(timeout-forms (cdr list)))
    `(let ((with-timeout-tag (cons nil nil))
	   with-timeout-value with-timeout-timer)
       (if (catch with-timeout-tag
	     (progn
	       (setq with-timeout-timer
		     (run-with-timer ,seconds nil
				      'with-timeout-handler
				      with-timeout-tag))
	       (setq with-timeout-value (progn . ,body))
	       nil))
	   (progn . ,timeout-forms)
	 (cancel-timer with-timeout-timer)
	 with-timeout-value))))

(defun y-or-n-p-with-timeout (prompt seconds default-value)
  "Like (y-or-n-p PROMPT), with a timeout.
If the user does not answer after SECONDS seconds, return DEFAULT-VALUE."
  (with-timeout (seconds default-value)
    (y-or-n-p prompt)))

(defvar timer-duration-words
  (list (cons "microsec" 0.000001)
	(cons "microsecond" 0.000001)
        (cons "millisec" 0.001)
	(cons "millisecond" 0.001)
        (cons "sec" 1)
	(cons "second" 1)
	(cons "min" 60)
	(cons "minute" 60)
	(cons "hour" (* 60 60))
	(cons "day" (* 24 60 60))
	(cons "week" (* 7 24 60 60))
	(cons "fortnight" (* 14 24 60 60))
	(cons "month" (* 30 24 60 60))	  ; Approximation
	(cons "year" (* 365.25 24 60 60)) ; Approximation
	)
  "Alist mapping temporal words to durations in seconds")

(defun timer-duration (string)
  "Return number of seconds specified by STRING, or nil if parsing fails."
  (let ((secs 0)
	(start 0)
	(case-fold-search t))
    (while (string-match
	    "[ \t]*\\([0-9.]+\\)?[ \t]*\\([a-z]+[a-rt-z]\\)s?[ \t]*"
	    string start)
      (let ((count (if (match-beginning 1)
		       (string-to-number (match-string 1 string))
		     1))
	    (itemsize (cdr (assoc (match-string 2 string)
				  timer-duration-words))))
	(if itemsize
	    (setq start (match-end 0)
		  secs (+ secs (* count itemsize)))
	  (setq secs nil
		start (length string)))))
    (if (= start (length string))
	secs
      (if (string-match "\\`[0-9.]+\\'" string)
	  (string-to-number string)))))

(provide 'timer)

;;; timer.el ends here
