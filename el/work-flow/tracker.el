;; I couldn't find a project tracker that did everything I needed so I wrote one
;;
;; Copyright (C) 1999 Russell Young emacs@young-0.com
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; DESCRIPTION
;; Keeps a list of projects and associated billing entity. A single
;; project can be selected and it will be marked in a file as active
;; until a new command is made to stop it. A project is automatically
;; terminated when emacs exits. If somehow this fails, the tracking
;; data file is touched at regular intervals, so the next time the
;; tracker is started if there is an open project a reasonable guess can
;; be made as to when to mark it as closed. Each session can have multiple
;; comments associated with it, in which you can mark acomplishments or
;; whatever. It will generate informal daily summary reports and a report
;; of total hours for a particular billee.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; VARIABLES
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; First item is project name, second item is billable-to, remaining
;;; are options. Options must begin with a code to be recognized. The
;;; codes currently handled are:
;;;    tracker-init-*: called when the project is selected, after it is
;;;			written in the project file
;;;    tracker-idle-*: called when the project is idle for the timeout
;;;         interval
;;;    tracker-off-*: called when the project is deselected, before it is
;;;         written in the project file
(defvar tracker-projects
  '(("compiler" 		"catharon")
	("administrative" 	"catharon")
	("kernel" 			"catharon")
	("linux" 			"catharon")
	("mail"				"catharon")
	("misc work"		"catharon" 		tracker-init-require-comment)

	("personal mail"	"personal")
	("news" 			"personal" 		tracker-idle-warn)
	("lin" 				"personal")
	("system work"		"personal")
	("emacs"			"personal")
	("nscl"				"personal")
	("job hunting" 		"personal" 		tracker-idle-warn)
	("misc personal"	"personal" 		tracker-init-require-comment))
;;; old values
; 	("vga driver" "catharon")
; 	("x driver" "catharon")
; 	("commands" "catharon")
; 	("libraries" "catharon")
; 	("editor" "catharon")
; 	("misc" "catharon")
; 	("emacs" "personal")
; 	("mail, news" "personal" 'tracker-idle-warn)
; 	("job" "personal")
; 	("lin" "personal")
; 	("debugger" "catharon")
; 	("system" "personal"))
  "projects and billable information for time tracking.
This is a list of all projects, where a project is a list of a project name
and the entity who pays the bills for that project."
  )

(defvar tracker-nag t
  "remind to start tracker if it is not running")

(defvar tracker-default-options '(tracker-idle-stop)
  "default options to use if none are given for an entry in the projects table")

(defvar tracker-on nil
  "the time tracking was started for the current project")

(defvar tracker-file "~/jobs/tracker.data"
  "the default data file for tracker")

(defvar tracker-buffer "*tracker*"
"buffer name to use for tracker")

(defvar tracker-current-project nil
  "the current project for the tracker. May be running or halted")

(defvar tracker-touch-interval 60
  "The number of seconds to wait in between touching the tracker data file")

(defvar tracker-idle-stop 20
  "The number of idle minutes to wait before automatically shutting off tracking")

(defvar tracker-display-in-mode-line t
  "Display the project and the state in the mode line")

(defvar tracker-state nil)

(defconst tracker-search-re
  "^\\([^]*\\)\\([onf]+\\)\\([0-9a-fA-F]+\\)\\([0-9a-fA-F]+\\)\\([^]*\\)\\([0-9]+\\)\\([0-9]+\\)"
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; CHECKING, AUTOMATIC, AND SUPPORT FUNCTIONS
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defadvice switch-to-buffer (before check-tracker activate)
  "Check that you didn't forget to start the tracker by mistake"
  (and tracker-nag (not tracker-on)
	   (message "Did you forget to start the tracker?")))

;;; This belongs in the variable section, but is put here to use as a flag
;; for tracker-warn-stop
(defvar tracker-timer nil)

;;; option functions
;;; These are called as hooks through the use of tracker-map

(defun tracker-idle-stop (&optional time)
  "shuts off tracking after it has been idle for the set interval"
  (tracker-turn nil tracker-current-project)
  (message
   (format "Idle for %d minutes, stopping tracker" (or time tracker-idle-stop))))

(defun tracker-idle-warn ()
  "puts a warning message in the log file after the idle timer expires"
  (save-excursion
	(set-buffer (find-file-noselect tracker-file))
	(end-of-buffer)
	(tracker-make-entry tracker-current-project "idle warning" )))

(defun tracker-idle-ignore ()
  "ignores the idle timer going off")

(defun tracker-init-require-comment ()
  "requires that a comment be added when the tracker is invoked"
  (call-interactively 'tracker-comment))

(defun tracker-map (prefix)
  "runs all the options (hooks) appropriate when it is called"
  (mapcar (lambda (x)
			(let ((func x)
				  args)
			  (if (listp x)
				  (setq args (cdr x)
						func (car x)))
			  (if (string-match prefix (symbol-name func))
				  (apply func args))))
		  (or (cddr tracker-current-project)
			  tracker-default-options)))

(defun tracker-idle ()
  (tracker-map "^tracker-idle-"))

(defun tracker-mark-file ()
  "touch the tracker data file regularly"
  (if tracker-on
	  (start-process-shell-command "touch" nil "/bin/touch" tracker-file)))

(defun tracker-make-entry (project state &optional time buffer)
  "makes an entry to the tracker data file"
  (or time (setq time (current-time)))
  (if buffer (set-buffer tracker-buffer))
  (let ((bill-to (second project))
		(format-string (if (or (equal state "on") (equal state "off"))
						   "%s%s%.4x%.4x%s%.2d%.2d%.2d%.2d%.2d
"
						 "%s%s%.4x%.4x%s%.2d%.2d%.2d%.2d%.2d
")))

	(multiple-value-bind (sec min hour day month year) (decode-time time)
	  (insert
	   (format format-string
			   (car project) state (first time) (second-or-cdr time) bill-to
			   year month day hour min)))))

(defun tracker-turn (on project &optional time)
  "turns the tracker on or off"
  (setq tracker-current-project project)
  (if on (progn
;;; shut off tracking automatically after a fixed time of no activity
		   (or tracker-timer
			   (run-with-idle-timer (* 60 (or time tracker-idle-stop)) t 'tracker-idle))
		   (tracker-check-file)
		   (tracker-switch t)
		   (setq on "on"
				 tracker-on (current-time)))
	(tracker-map "^tracker-off-")
	(setq on "off"
		  tracker-on nil)
	(tracker-switch nil))
  (setq tracker-state (concat "-- " (car project) " " on " "))
  (save-excursion
	(let* ((point (progn (set-buffer (get-buffer-create tracker-buffer))
						 (point)))
		   (bill-to (second project))
		   )
	  (tracker-make-entry project on)
	  (append-to-file point (point) tracker-file))
	(if tracker-on (tracker-map "^tracker-init-"))
	(message (concat "turned " on " tracking for `" (car project) "'"))))

(defun tracker-switch (go)
  "low-level function to start or stop tracker"
  (if (null tracker-timer)
	  (setq tracker-timer
			(run-at-time nil tracker-touch-interval 'tracker-mark-file)))
  (if go
	  (timer-activate tracker-timer)
	(cancel-timer tracker-timer)
	(remove tracker-timer timer-idle-list)
	(setq tracker-timer nil))
  )

(defun second-or-cdr (item)
  (if (listp (cdr item)) (second item)
	(cdr item)))

; load the file and check that it was closed properly. If it wasn't take
; the timestamp from the file
(defun tracker-check-file (&optional file keep)
  "Check the tracker file to make sure it has been closed properly.
Called on startup"
  (let* ((buffer (find-file (or file tracker-file)))
		 (mod-time (visited-file-modtime))
	   )
	(end-of-buffer)
	(if (and (re-search-backward tracker-search-re nil t)
			 (match-string 5))
		(let ((project (match-string 1))
			  (state (match-string 2))
			  (bill-to (match-string 5))
			  )
		  (when (equal state "on")
			(message "guessing that this was not closed right")
			(end-of-buffer)
			(tracker-make-entry project "off" mod-time)
			(tracker-comment "file closed on startup check")
			(save-buffer)))
	  (message "This does not look like a tracking file"))
	(if (not keep) (kill-buffer (current-buffer)))))

(add-hook  'kill-emacs-hook 'tracker-stop)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; VISIBLE FUNCTIONS
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun tracker-format-time (secs)
  (let* ((hours (/ secs 3600))
		 (minutes (/ (mod secs 3600) 60)))
	(concat (if (< 0 hours) (format "%d hours " hours) "")
			(format "%d minutes" minutes))))

(defun tracker ()
  "report on the current state of the tracker"
  (interactive)
  (if (not tracker-current-project)
	  (message "Tracking is off")
	(let ((name (car tracker-current-project)))
	  (if tracker-on
		  (message (concat "Tracking is on for `" name "', "
						   (tracker-format-time (date-subtract (current-time) tracker-on))))
		(message (concat "tracking is off for `" name "'"))
		))))

(defun tracker-comment (comment &optional file)
  "Adds a comment to the tracker log"
  (interactive "MProject description: ")
  (set-buffer (find-file-noselect (or file tracker-file)))
  (end-of-buffer)
  (insert "" comment "
")
  (save-buffer)
  (kill-buffer (current-buffer)))

(defun tracker-start (new-project)
  "starts the tracker on a given project"
  (interactive
   (let* ((name (car tracker-current-project))
		  (prompt
		   (concat "project"
				   (if tracker-current-project
					   (format " (%s): " name)
					 ": "))))
	 (list (assoc
			(completing-read prompt tracker-projects nil t nil nil name)
		   tracker-projects))
	 ))
  (let ((time (second (assoc 'tracker-idle-stop new-project))))
	(if tracker-on
		(when (not (equal new-project tracker-current-project))
		  (tracker-turn nil tracker-current-project)
		  (tracker-turn t new-project time))
	  (tracker-turn t new-project time)))
  )

(defun tracker-stop (&optional project)
  "stops the tracker"
  (interactive)
  (if tracker-on
	  (tracker-turn nil (or project tracker-current-project))))


(global-set-key [?\s-q] 'tracker-start)
(global-set-key [?\s-a] 'tracker-stop)
(global-set-key [?\s-c] 'tracker-comment)
(global-set-key [?\s-t] 'tracker)
(global-set-key [?\s-s] 'my-shell)


(defalias 'daily 'tracker-daily-summary)
(defalias 'start 'tracker-start)
(defalias 'stop 'tracker-stop)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; OUTPUT FUNCTIONS
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun tracker-parse-date (date)
  "get the date internal format out of a dd[/mm[/yy]] timestring"
  (if (not (string-match "^\\([0-9]+\\)/?\\([0-9]*\\)/?\\([0-9]*\\)" date))
	  '(0 . 0)
	(let* ((day (string-to-number (match-string 1 date)))
		   (month (string-to-number (match-string 2 date)))
		   (year (string-to-number (match-string 3 date)))
		   date1 date2
		   start1 start2)
	  (if (or (> month 12)
			  (> day 31))
		  (error "bad date format (dd[/mm[/yy]])"))
 	  (multiple-value-bind (xsec xmin xhour xday xmonth xyear) (decode-time)
 		(progn
 		  (if (= 0 year) (setq year xyear))
 		  (if (< year 1900) (setq year (+ year 1900)))
 		  (if (= 0 month) (setq month xmonth))))
 	  (encode-time 0 0 0 day month year))))

(defun start-of-next-day (time &optional days)
  "Get the internal format for a given day at 00:00:00"
  (or time (setq time (current-time)))
  (or days (setq days 1))
  (multiple-value-bind (sec min hour day month year zone) (decode-time time)
	(encode-time 0 0 0 (+ days day) month year zone)))

(defun tracker-crunch-days (&optional file current-day)
  "parses the tracker file for daily totals"
  (save-excursion
	(set-buffer (find-file-noselect tracker-file))
	(beginning-of-buffer)
	(let ((started '(0 . 0))
		  (day-totals nil)
		  (results '())
		  )
	  (or current-day (setq current-day '(0 . 0)))
	  (while (re-search-forward tracker-search-re nil t)
		(let* ((project (match-string 1))
			   (state (match-string 2))
			   (time1 (string-to-hex (match-string 3)))
			   (time2 (string-to-hex (match-string 4)))
			   (time (cons time1 time2)))
		  (if (equal state "on")
			  (setq started time)
			(when (date< current-day time)
			  (if (< 0 (apply '+ (mapcar 'cdr day-totals)))
				  (setq results (cons (cons current-day day-totals) results)))
			  (setq day-totals (mapcar (lambda (x) (cons (car x) 0)) tracker-projects)
					current-day (start-of-next-day time)))
			(setf (cdr (assoc project day-totals))
				  (+ (date-subtract time started) (cdr (assoc project day-totals)))))
		  ))
	  (kill-buffer (current-buffer))
	  (cons (cons current-day day-totals) results))))

(defun tracker-daily-summary (&optional file)
  "outputs the daily summaries for all projects"
  (interactive)
  (setq start (if current-prefix-arg (start-of-next-day nil current-prefix-arg)
				'(0 . 0)))
  (switch-to-buffer tracker-buffer)
  (or file (setq file tracker-file))
  (erase-buffer)
  (insert "Summary from tracker file " file "
------------------------------------------
")
  (let ((total (apply '+ (mapcar 'tracker-one-day (tracker-crunch-days file start)))))
	(insert "----------------------------------
                         " (format "%6.2f" total))))
(defun tracker-one-day (dailies)
  "Outputs a single day's project totals. Returns the total number of seconds"
  (let ((point (point))
		(total
		 (apply '+
				(mapcar (lambda (x) (let ((hours (/ (cdr x) 3600.0)))
									  (if (< 0 hours)
										  (insert (format "%15s: %8.2f
" (car x) hours)))
									  hours))
						(cdr dailies)))))
	(goto-char point)
	(insert (datify (car dailies)) ":" (format "%22.2f" total) "
")
	(end-of-buffer)
	total))


(defun tracker-bill-tos (&optional projects)
  "Gets a list of all bill-to parties"
  (mapcar 'list
		  (remove-duplicates
		   (mapcar 'second (or projects tracker-projects)) :test 'equal))
  )

(defun tracker-bill (bill-to &optional start end file)
  "Prints an hourly breakdown for a given bill-to"
  (interactive (list (completing-read "company: " (tracker-bill-tos) nil t)
					 (read-from-minibuffer "Start date: ")
					 (read-from-minibuffer "End date: " (datify (current-time) t "/"))))
  (or file (setq file tracker-file))
  (let* ((buffer (current-buffer))
		 (tbuffer (find-file-noselect file))
		 (totals (remove nil (mapcar (lambda (x)
									   (if (equal bill-to (second-or-cdr x))
										   (cons (car x) 0))) tracker-projects)))
		 (current nil)
		 up)
	(set-buffer tbuffer)
	(if start (progn
				(beginning-of-buffer)
				(setq start (tracker-parse-date start)))
	  (setq start '(0 . 0))
	  (end-of-buffer)
	  (if (not (re-search-backward (concat "^-+ billed " bill-to " ") nil t))
		  (beginning-of-buffer)))
	(setq end (if (and (stringp end) (not (equal end "")))
				  (tracker-parse-date end)
				(current-time)))
	(while (re-search-forward tracker-search-re nil t)
	  (let* ((project (match-string 1))
			 (state (match-string 2))
			 (time1 (string-to-hex (match-string 3)))
			 (time2 (string-to-hex (match-string 4)))
			 (time (cons time1 time2)))
		(if (equal state "on")
			(setq current project
				  up time)
		  (if current
			  (if (and (date< start time) (date< time end))
				  (let ((data (assoc project totals)))
					(when data
					  (setf (cdr data) (+ (cdr data) (date-subtract time up)))
					  (setq up nil
							current nil))))
			(beep)
			(message "termination without start")))))
	(end-of-buffer)
	(insert "--------- billed " bill-to " ----------------
")
	(save-buffer)
	(kill-buffer (current-buffer))
	(tracker-write-billfile file bill-to totals)
	totals))

(defun datify (date &optional dmy sep)
  "Writes a date in standard (yy-mm-dd) format"
  (or sep (setq sep "-"))
  (let* ((date-info (decode-time date))
		 (year (mod (sixth date-info) 100))
		 (month (fifth date-info))
		 (day (fourth date-info)))
	(if dmy
		(format "%.2d%s%.2d%s%.2d" day sep month sep year)
	  (format "%.2d%s%.2d%s%.2d" year sep month sep day))))

(defun tracker-write-billfile (file bill-to totals &optional start end)
  "Writes out the tracker bill file"
  (let* ((now (decode-time))
		 (year (mod (sixth now) 100))
		 (month (fifth now))
		 (day (fourth now))
		 (directory (file-name-directory file))
		 (billfile (concat (file-name-directory file)
						   (or bill-to "all") "."
						   (format "%d-%.2d-%.2d" year month day)))
		 total)
	(or end (setq end (current-time)))
	(find-file billfile)
	(erase-buffer)
	(insert "Hours for " (capitalize bill-to) " ")
	(if start (insert "from " (datify start)))
	(insert "to " (datify end) "
-----------------------------------
")
	(setq total (apply '+ (mapcar 'bill-one-line totals)))
	(insert "-----------------------------------
                         " (format "%8.2f" total))
	(save-buffer)
	))

(defun bill-one-line (totals)
  "Writes a single billing line"
  (let ((hours (/ (cdr totals) 3600.0)))
	(insert (format "%20s : %10.2f
" (car totals) hours))
	hours))

(defun string-to-hex (num)
  "Replaces the broken string-to-number for hex translation (try (string-to-number \"12e0\" 16)"
  (let ((total 0)
		digit)
	(while (< 0 (length num))
	  (setq digit (substring num 0 1)
			num (substring num 1)
			total (+ (* 16 total) (string-to-number digit 16))))
	total))

(defun date< (d1 d2)
  "compares 2 dates in internal format"
  (let* ((d11 (car d1))
		 (d12 (second-or-cdr d1))
		 (d21 (car d2))
		 (d22 (second-or-cdr d2)))
	(or (< d11 d21) (and (= d11 d21) (< d12 d22)))))

(defun date-subtract (d1 d2)
  "finds the difference between 2 dates in internal format"
  (let* ((d11 (car d1))
		 (d12 (second-or-cdr d1))
		 (d21 (car d2))
		 (d22 (second-or-cdr d2))
		 (diff1 (- d11 d21))
		 (diff2 (- d12 d22)))
	(if (and (= (car d2) 0) (= (cdr d2) 0))
		(+ (* 256 256 d11) d12)
	  (+ (* 256 256 diff1) diff2))))

(alter-mode-line -1 'tracker-state)

(provide 'tracker)

