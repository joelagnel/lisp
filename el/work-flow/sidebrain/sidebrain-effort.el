;;;; sidebrain-effort.el -- record the "effort" (time and keystrokes) that it takes to do things
;;; Time-stamp: <2006-02-01 12:57:38 jcgs>

;;  This program is free software; you can redistribute it and/or modify it
;;  under the terms of the GNU General Public License as published by the
;;  Free Software Foundation; either version 2 of the License, or (at your
;;  option) any later version.

;;  This program is distributed in the hope that it will be useful, but
;;  WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;  General Public License for more details.

;;  You should have received a copy of the GNU General Public License along
;;  with this program; if not, write to the Free Software Foundation, Inc.,
;;  59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

(provide 'sidebrain-effort)

(defun time-as-seconds (time)
  "Return TIME as seconds, as an integer.
TIME may be an emacs time structure, or an integer."
  (cond
   ((integerp time)
    time)
   ((consp time) (floor (time-to-seconds time)))
   ((null time)
    0)))

(defun informal-format-time (secs &optional inexact)
  (let* ((mins (/ secs 60))
	 (hours (/ mins 60))
	 (days (/ hours 24))
	 (hours-in-day (% hours 24))
	 (weeks (/ days 7))
	 (days-in-week (% days 7))
	 (mins-in-hour (% mins 60))
	 (secs-in-hour (% secs (* 60 60)))
	 (secs-in-minute (% secs 60)))
    (unless (zerop hours) (setq inexact nil)) ; if it's over an hour, don't say a-b minutes
    (let ((weeks-string
	   (cond
	    ((zerop weeks) "")
	    ((= weeks 1) "one week,")
	    (t (format "%d weeks" weeks))))
	  (days-string
	   (cond
	    ((zerop days) "")
	    ((= days 1) "one day")
	    (t (format "%d days" days))))
	  (hour-string
	   (cond
	    ((zerop hours) "")
	    ((= hours 1) "one hour, ")
	    ((> hours 1) (format "%d hours, " hours))
	    (t "something funny about the hours")))
	  (within-hour-string
	   (cond
	    ((= secs-in-hour 61) "1 minute, 1 second")
	    ((and (>= secs-in-hour 62) (<= secs-in-hour 119))
	     (format "1 minute, %d seconds" (- secs-in-hour 60)))
	    ((= mins-in-hour 1) "1 minute")
	    ((> mins-in-hour 1) (if inexact
			    (format "%d-%d minutes" mins-in-hour (1+ mins))
			  (format "%d minutes" mins-in-hour)))
	    ((= secs-in-hour 1) "1 second")
	    (t (format "%d seconds" secs-in-hour)))))
      (concat hour-string within-hour-string))))

(defun add-time (t1 t2)
  "Add two internal times."
  (if (integerp t1) (setq t1 (list 0 t1 0)))
  (if (integerp t2) (setq t2 (list 0 t2 0)))
  (let* ((t1p3 (caddr t1))		; we sometimes get given nil here
	 (t2p3 (caddr t2))		; we sometimes get given nil here
	 (sp3r (+ (if (numberp t1p3) t1p3 0) (if (numberp t2p3) t2p3 0)))
	 (sp2r (+ (cadr t1) (cadr t2) (floor sp3r 1000000)))
	 (sp1r (+ (car t1) (car t2) (floor sp2r 65536))))
    (list sp1r (% sp2r 65536) (% sp3r 1000000))))

(defun zero-time-p (time)
  "Return whether TIME is zero."
  (cond
   ((consp time) (and
		  (zerop (first time))
		  (zerop (second time))
		  (or (null (third time))
		      (zerop (third time)))))
   ((numberp time) (zerop time))))

(defun sidebrain-timestamp (task stamp &optional time)
  "For TASK, set its STAMP property to the current time.
Optional third argument gives a time to use."
  (sidebrain-put-task-property task stamp (or time (current-time))))

(defun sidebrain-start-stopwatch (task timer-attribute started-attribute &optional time)
  "For TASK, start its TIMER-ATTRIBUTE and mark its STARTED-ATTRIBUTE with the start time.
If TIMER-ATTRIBUTE already has a value, the value is not disturbed. If it has no value,
it is set to zero.
The current time is used unless optional TIME argument is given."
  (message "Starting %S from %S" timer-attribute started-attribute)
  (unless (sidebrain-get-task-property task timer-attribute)
    (sidebrain-put-task-property task timer-attribute (list 0 0)))
  (sidebrain-timestamp task started-attribute time))

(defun sidebrain-stop-stopwatch (task timer-attribute started-attribute
				      &optional ended-attribute time)
  "For TASK, update TIMER-ATTRIBUTE with time since STARTED-ATTRIBUTE.
If ENDED-ATTRIBUTE is given, that is set to the end time.
If TIME is given, it is used instead of the current time.
Return the time spent on that stopwatch."
  (message "Stopping %S which has been going from %S%s" timer-attribute started-attribute (if ended-attribute (format " until %S" ended-attribute) ""))
  (let* ((already-spent (sidebrain-get-task-property task timer-attribute))
	 (started (sidebrain-get-task-property task started-attribute))
	 (ended (or time (current-time)))
	 (spent-this-time (if started
			      (subtract-time ended started)
			    nil))
	 (spent (if spent-this-time
		    (if already-spent
			(add-time spent-this-time already-spent)
		      spent-this-time)
		  already-spent)))
    (sidebrain-put-task-property task timer-attribute spent)
    (when ended-attribute
      (sidebrain-timestamp task ended-attribute ended))
    spent))

(defun sidebrain-update-stopwatch (task this-time-attribute started-attribute)
  "For TASK, update the THIS-TIME-ATTRIBUTE to reflect time since STARTED-ATTRIBUTE."
  (let* ((started (sidebrain-get-task-property task started-attribute)))
    (if started
	(sidebrain-put-task-property task
				     this-time-attribute
				     (subtract-time (current-time) started)))))

;;;; recording keys

(defvar sidebrain-keystrokes-count 0
  "The number of keystrokes typed so far.")

(defun sidebrain-pre-command-hook ()
  "Hook to run at the start of each command.
This records keystrokes."
  (setq sidebrain-keystrokes-count (+ sidebrain-keystrokes-count (length (this-command-keys)))))

(add-hook 'pre-command-hook 'sidebrain-pre-command-hook t)

(defun sidebrain-account-keystrokes-to-task (task)
  "Account recent keystrokes to TASK."
  (let ((already (sidebrain-get-task-property task 'keystrokes)))
    (sidebrain-put-task-property task 'keystrokes
				 (if (numberp already)
				     (+ already sidebrain-keystrokes-count)
				   sidebrain-keystrokes-count))
    (setq sidebrain-keystrokes-count 0)))

;;; end of sidebrain-effort.el
