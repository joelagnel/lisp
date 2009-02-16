;;; etask-todo.el --- part of EtaskMode (main file: etask.el)

;; Copyright (C) 2004 René Weichselbaum

;; Author: Rene Weichselbaum

;; $Id: etask-todo.el,v 1.35 2004/11/01 21:34:43 rene Exp $

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.


;;; Commentary:

;; This software component implements the EtaskMode todo list.


;; _________________


;;; Code:


(defcustom etask-todo-day-begin-near-now-p t
  "If non-nil the default begin time (full hour) for the Gantt chart
ToDo view is about one hour before current time."
  :type 'boolean
  :group 'finetuning)

(defcustom etask-todo-day-begin 8
  "Default begin time (full hour) for the Gantt chart ToDo view.  Only
used if variable `etask-todo-day-begin-near-now-p' is nil."
  :type 'integer
  :group 'finetuning)

(defcustom etask-todo-trivial-todo 15
  "Default maximum effort in minutes for trivial todo's."
  :type 'integer
  :group 'finetuning)

(defcustom etask-todo-notsotrivial-todo 60
  "Default maximum effort in minutes for not so trivial todo's."
  :type 'integer
  :group 'finetuning)

(defconst etask-todo-time-marker "^"
  "Marker for a time within todo view, e.g. ^12:00.")

(defconst etask-todo-timewidth 8 "Minimum width in characters needed
to insert time and its marker, e.g '^12:00 '.  Should be 8 or a
multiple of 8.")


(defun etask-todo-insert-elements(todocat ixlist)
  "Add a new todo to category TODOCAT at IXLIST.  Return inserted
element or nil if operation failed."
  (when (etask-cat-is-item-p etask-category-todoid todocat)
    (etask-todo-get-initial-elementdata 
     todocat (etask-get-labels) ixlist)))

(defun etask-todo-insert-elementbar-p(element)
  "Return true if ELEMENT's bar is to insert into Gantt chart, nil
otherwise."
  (etask-proj-insert-elementbar-p element))

(defun etask-todo-is-completed-p(todo)
  "Return true if TODO is completed, nil otherwise."
  (let* ((planned-hours (etask-simplify-number
                         (etask-db-get todo etask-db-attr-peffort)))
         (expended-hours (etask-simplify-number
                          (etask-db-get todo etask-db-attr-eeffort))))
    (and (numberp planned-hours)
         (numberp expended-hours)
         (= (- planned-hours expended-hours) 0))))

(defun etask-todo-is-onschedule-p(todo)
  "Return true if TODO's deadline can still be met.  This is
the case when the remaining time to the deadline is  the
still open part."
  (let* ((date
          (etask-datetime-subtract-time
           (etask-make-duedatetime todo)
           (max (- (etask-db-get todo etask-db-attr-peffort)
                   (etask-db-get todo etask-db-attr-eeffort))
                0))))
    (or (etask-datetime-less-p (decode-time (current-time)) date)
        (etask-datetime-equal-p (decode-time (current-time)) date))))

(defun etask-todo-afterdeadline-p(todo)
  "Return true if TODO's deadline is in the past."
  (let* ((date (etask-make-duedatetime todo)))
    (etask-datetime-less-p date (current-time))))

(defun etask-todo-get-head(todo)
  "Return TODO's head as a string."
  (let* ((priostr (number-to-string
                   (etask-db-get todo etask-db-attr-priority))))
    (if (and (not (etask-todo-is-onschedule-p todo))
             (not (etask-todo-is-completed-p todo)))
        (etask-apply-face priostr 'etask-face-status-behindschedule)
      priostr)))

(defun etask-todo-get-bar(todo)
  "Return TODO's bar as a string."
  (let* ((planned-hours (etask-simplify-number
                         (etask-db-get todo etask-db-attr-peffort)))
         (expended-hours (etask-simplify-number
                          (etask-db-get todo etask-db-attr-eeffort)))
         (min (round (* planned-hours 60)))
         (barlen (cond ((<= min etask-todo-no-effort) 1)
                       ((<= min etask-todo-little-effort) 2)
                       ((<= min etask-todo-standard-effort) 3)
                       ((<= min etask-todo-significant-effort) 4)
                       ((<= min etask-todo-huge-effort) 5)
                       (t
                        6))))
    (cond ((etask-todo-is-completed-p todo)
           (make-string barlen ?=))
          ((and (numberp expended-hours) (> expended-hours 0))
           (make-string barlen ?~))
          (t
           (make-string barlen ?-)))))

(defun etask-todo-get-todostr(todo)
  "Return TODO string for Gantt chart."
  (concat (etask-todo-get-bar todo) ">(" (etask-todo-get-head todo) ")"))

(defun etask-todo-get-effort-char(todo)
  "Return TODO's effort indicator as a string."
  (let* ((planned-hours (etask-simplify-number
                         (etask-db-get todo etask-db-attr-peffort)))
         (expended-hours (etask-simplify-number
                          (etask-db-get todo etask-db-attr-eeffort)))
         (min (round (* planned-hours 60))))

    (cond ((< min etask-todo-standard-effort) "<")
          ((= min etask-todo-standard-effort) "")
          ((> min etask-todo-standard-effort) ">"))))

(defun etask-todo-adjust-tododuetime(tododuetime hrwidth)
  "Return number of characters to be added to leading characters if
todo is not due at full hour and hour width is
`etask-todo-timewidth'."
  (let ((duemin (car (cdr tododuetime))))
    (cond ((= duemin 0) 0)
          ((< duemin 8) (floor (* hrwidth 0.125)))
          ((< duemin 16) (floor (* hrwidth 0.25)))
          ((< duemin 23) (floor (* hrwidth 0.375)))
          ((< duemin 31) (floor (* hrwidth 0.5)))
          ((< duemin 46) (floor (* hrwidth 0.75)))
          (t (floor (* hrwidth 0.875))))))

(defun etask-todo-is-visible-day-p(start end day)
  "Return true if DAY is within START and END.  START, END, and DAY
are calendar dates."
  (or (and
       (calendar-date-compare (list start) (list day))
       (calendar-date-compare (list day) (list end)))
      (calendar-date-equal day start)
      (calendar-date-equal day end)))

(defun etask-todo-is-visible-time-p(date time &optional event)
  "Return true if date DATE TIME is within the displayed Gantt chart
]chstarthr chendhr].  The structure of DATE is \(month day year\), and
the structure of TIME is \(hr min sec\).  If optional EVENT is non-nil
then return true if date is between [chstarthr chendhr]."
  (when (and (etask-calendardate-is-legal-p date)
             (etask-time-is-legal-p time))
    (let ((chstart  (etask-state-get etask-stateid-chartstart))
          (chstarthr (etask-state-get etask-stateid-chartstarttime))
          (chend  (etask-state-get etask-stateid-chartend))
          (chendhr (etask-state-get etask-stateid-chartendtime))
          (chstartt (list 0 0 chstarthr))
          (chendt (list 0 0 chendhr))
          (timet (list (nth 2 time) (nth 1 time) (car time))))
      (when (and (etask-calendardate-is-legal-p chstart)
                 (etask-calendardate-is-legal-p chend)
                 (etask-time-is-legal-p (list chstarthr 0 0))
                 (etask-time-is-legal-p (list chendhr 0 0)))
        (and (etask-todo-is-visible-day-p chstart chend date)
             (or (and (etask-time-less-p chstartt timet)
                      (etask-time-less-p timet chendt))
                 (when event
                   (etask-time-equal-p chstartt timet))
                 (etask-time-equal-p timet chendt)))))))

(defun etask-todo-calc-hrwidth(daywidth)
  ""
  ;; etask-todo-timewidth is 8 or a multiple of 8
  ;; therefore:   2**x <= width, where width = (daywidth / hrs)
  ;;          log 2**x <= log width
  ;;                 x <= log width / log 2
  (let ((hrs (etask-todo-get-hrnum)))
    (expt 2 (floor (/ (log (/ daywidth hrs)) (log 2))))))

(defun etask-todo-get-hrwidth()
  "Return width of an hour.  DAYWIDTH is the width of a day in
characters."
  (if (etask-todo-organizer-view-p) etask-todo-timewidth
    (let ((daywidth (etask-state-get etask-stateid-maxbarlen)))
      (etask-todo-calc-hrwidth daywidth))))

(defun etask-todo-get-hrnum()
  "Return number of hours displayed in Gantt chart."
  (let ((daywidth (etask-state-get etask-stateid-maxbarlen)))
    (if (etask-todo-organizer-view-p)
        (if (> etask-todo-timewidth 0)
            (/ daywidth etask-todo-timewidth)
          0)
      (if (> etask-last-workinghour etask-first-workinghour)
          (- etask-last-workinghour etask-first-workinghour)
        0))))

(defun etask-todo-insert-daybar(todo)
  "Insert TODO's bar when time unit = hour."
  (let* ((organizerp (etask-todo-organizer-view-p))
         (chstarthr (etask-state-get etask-stateid-chartstarttime))
         (daywidth (etask-state-get etask-stateid-maxbarlen))
         (chendhr (etask-state-get etask-stateid-chartendtime))
         (hrs (- chendhr chstarthr))
         (tododate (etask-db-get todo etask-db-attr-taskend))
         (todotime (etask-db-get todo etask-db-attr-taskendtime))
         (earlierhrs (- (car todotime) chstarthr)))
    (when (or (and organizerp
                   (etask-todo-is-visible-time-p tododate todotime))
              (not organizerp))
      (let* ((hrwidth (etask-todo-get-hrwidth))
             (todostr (etask-todo-get-todostr todo))
             (todowidth (length todostr))
             (arrowpos (string-match ">" todostr))
             (adjust (etask-todo-adjust-tododuetime todotime hrwidth))
             (space (+ (* (- (car todotime) chstarthr) hrwidth) adjust)))
        (cond ((< space todowidth)
               (insert (store-substring todostr arrowpos "<")))
              ((or (> (car todotime) chendhr)
                   (and (= (car todotime) chendhr)
                        (> (car (cdr todotime)) 0)) ;neglect seconds
                   (< (* hrs hrwidth) todowidth))
               (insert (make-string (- daywidth todowidth) ? ))
               (insert (store-substring todostr arrowpos "<")))
              (t
               (let ((leadingspc (- space todowidth)))
                 (insert (make-string leadingspc ? ))
                 (etask-format-insert
                  todostr (max (round hrwidth) todowidth) "left"))))))))

(defun etask-todo-insert-bar(todo chstart chend daylen)
  "Insert TODO's bar in chart where CHSTART is the chart's start date,
CHEND its end date, and DAYLEN the length of a day in characters."
  (let ((tododate (etask-db-get todo etask-db-attr-taskend)))
    (when (etask-todo-is-visible-day-p chstart chend tododate)
      (if (etask-todo-unit-is-hr-p)
          (etask-todo-insert-daybar todo)
        (let* ((earlierdays
                (if (calendar-date-compare 
                     (list chstart) (list tododate))
                    (etask-days-between chstart tododate)
                  0))
               (todostr (etask-todo-get-todostr todo))
               (todostrlen (length todostr))
               (arrowpos (string-match ">" todostr))
               (leadingspc 
                (if (>= daylen todostrlen)
                    (round (* daylen earlierdays)) 
                  (if (> (* earlierdays daylen) (- todostrlen daylen))
                      (round (- (* earlierdays daylen)
                                (- todostrlen daylen)))
                    0))))
          (when (< leadingspc 0)
            (setq leadingspc 0)
            (setq todostr
                  (store-substring todostr arrowpos "<")))
          (when (and (< daylen todostrlen)
                     (< (* earlierdays daylen) (- todostrlen daylen)))
            (setq todostr
                  (store-substring todostr arrowpos "<")))
          (insert (make-string leadingspc ? ))
          (etask-format-insert 
           todostr (max (round daylen) todostrlen) "left"))))))

(defun etask-todo-show-elementstatus(todo &optional reportingp)
  "Return specific todo status for screen output.  If REPORTINGP is
non-nil, output is formatted for the reporting file instead."
  (let* ((name (etask-db-get todo etask-db-attr-taskname))
         (planned-hours (etask-simplify-number
                         (etask-db-get todo etask-db-attr-peffort)))
         (expended-hours (etask-simplify-number
                          (etask-db-get todo etask-db-attr-eeffort)))
         (open-hours 
          (if (> (- planned-hours expended-hours) 0)
              (etask-simplify-number
               (- planned-hours expended-hours))
            0))
         (statusstr (cond ((= expended-hours 0)
                           (etask-lang-msg 714 etask-language))
                          ((and (> expended-hours 0) (> open-hours 0))
                           (etask-lang-msg 715 etask-language))
                          ((= open-hours 0)
                           (etask-lang-msg 713 etask-language))
                          (t
                           "")))
         (date (calendar-date-string
                (etask-db-get todo etask-db-attr-taskend) t))
         (time (etask-db-get todo etask-db-attr-taskendtime))
         (hr (number-to-string (car time)))
         (min (number-to-string (car (cdr time)))))
    (concat
     (etask-apply-face (concat "'" name "'")
                       'etask-face-statusheader-onscreen)
     "    "
     (etask-lang-msg 451 etask-language)
     ": "
     date ", " hr ":" (if (= (length min) 1)
                          (concat "0" min)
                        min)
     "\n\n"
     statusstr
     "    "
     (etask-lang-msg 121 etask-language)
     ": "
     (etask-get-effort-str planned-hours)
     "    "
     (etask-lang-msg 122 etask-language)
     ": "
     (if (= expended-hours 0)
         "---"
       (etask-get-effort-str expended-hours))
     "    "
     (etask-lang-msg 450 etask-language)
     ": "
     (number-to-string (etask-db-get todo etask-db-attr-priority)))))
           
(defun etask-todo-edit-element(todo)
  "Continue `etask-cat-edit-element' operations."
  (setq todo (etask-todo-edit-priority todo)))

(defun etask-todo-edit-priority(todo)
  "Edit TODO's priority."
  (etask-db-set
   todo etask-db-attr-priority (etask-todo-get-priority todo)))

(defun etask-todo-edit-duedate(todo)
  "Edit TODO's duedate."
  (let ((duedate (etask-todo-get-duedate todo)))
    (setq todo
          (etask-db-set 
           todo etask-db-attr-taskend duedate))
    (etask-db-set 
     todo etask-db-attr-taskbegin
     (etask-todo-get-begin
      duedate
      (etask-db-get todo etask-db-attr-peffort)))))

(defun etask-todo-get-priority(todo)
  "Get TODO priority from minibuffer and return value."
  (let ((prompt
         (concat
          (format "'%s': "
                  (etask-shorten-string 
                   (etask-db-get todo etask-db-attr-taskname)
                   etask-longer-taskname-len-minibuf))
          (etask-lang-msg 450 etask-language)
          " (>= 0): ")))
    (string-to-number
     (etask-read
      prompt
      (lambda (x) (string-match etask-wholenumber-regexp x))))))

(defun etask-todo-get-effort-choicelist(eeffort todocatp evcatp &optional val)
  "Return effort choice list.  EEFFORT is the effort already expended.
If TODOCATP or EVCATP are non-nil, then the corresponding category is
the current one.  If optional VAL is non-nil, then return effort value
list in hrs."
  (let ((noeff 
         (etask-simplify-number (/ etask-todo-no-effort 60.0)))
        (littleeff
         (etask-simplify-number (/ etask-todo-little-effort 60.0)))
        (stdeff
         (etask-simplify-number (/ etask-todo-standard-effort 60.0)))
        (sigeff
         (etask-simplify-number (/ etask-todo-significant-effort 60.0)))
        (hugeeff
         (etask-simplify-number (/ etask-todo-huge-effort 60.0)))
        choicelist)
    (when (and (numberp eeffort) (<= eeffort noeff) todocatp)
      (setq choicelist
            (if val (cons noeff choicelist)
              (cons (etask-get-effort-str noeff) choicelist))))
    (when (and (numberp eeffort) (<= eeffort littleeff) todocatp)
      (setq choicelist
            (if val (cons littleeff choicelist)
              (cons (etask-get-effort-str littleeff) choicelist))))
    (when (and (numberp eeffort) (<= eeffort stdeff))
      (setq choicelist
            (if val (cons stdeff choicelist)
              (cons (etask-get-effort-str stdeff) choicelist))))
    (when (and (numberp eeffort) (<= eeffort sigeff))
      (setq choicelist
            (if val (cons sigeff choicelist)
              (cons (etask-get-effort-str sigeff) choicelist))))
    (when (and (numberp eeffort) (<= eeffort hugeeff))
      (setq choicelist
            (if val (cons hugeeff choicelist)
              (cons (etask-get-effort-str hugeeff) choicelist))))
    (when (and (numberp eeffort)
               (<= eeffort etask-event-little-effort)
               evcatp)
      (setq choicelist
            (if val (cons etask-event-little-effort choicelist)
              (cons
               (etask-get-effort-str etask-event-little-effort)
               choicelist))))
    (when (and (numberp eeffort)
               (<= eeffort etask-event-standard-effort)
               evcatp)
      (setq choicelist
            (if val (cons etask-event-standard-effort choicelist)
              (cons
               (etask-get-effort-str etask-event-standard-effort)
               choicelist))))
    (when (and (numberp eeffort)
               (<= eeffort etask-event-significant-effort)
               evcatp)
      (setq choicelist
            (if val (cons etask-event-significant-effort choicelist)
              (cons
               (etask-get-effort-str etask-event-significant-effort)
               choicelist))))
    (when (and (numberp eeffort)
               (<= eeffort etask-event-huge-effort)
               evcatp)
      (setq choicelist
            (if val (cons etask-event-huge-effort choicelist)
              (cons
               (etask-get-effort-str etask-event-huge-effort)
               choicelist))))
    (when (not val)
      (setq choicelist
            (cons (etask-lang-msg 512 etask-language) choicelist)))
    (nreverse choicelist)))

(defun etask-todo-get-peffort(todo)
  "Get planned TODO effort choice from minibuffer and return value in
hrs."
  (let* ((currcatid (car (etask-cat-get-current-item)))
         (todocatp (= currcatid etask-category-todoid))
         (evcatp (= currcatid etask-category-eventid))
         (eeffort (etask-db-get todo etask-db-attr-eeffort))
         (choicelist
          (etask-todo-get-effort-choicelist eeffort todocatp evcatp))
         (len (length choicelist))
         prompt choice )
    (setq prompt (concat (etask-get-list-prompt choicelist) "  ? "))
    (setq choice
          (string-to-number
           (etask-read 
            prompt 
            (lambda (x) 
              (and (string-match etask-wholenumber-regexp x)
                   (>= (string-to-number x) 1) 
                   (<= (string-to-number x) len))))))
    (cond ((and (natnump choice) (= choice len))
           (etask-get-planned-effort todo))
          ((and (natnump choice) (> choice 0))
           (nth
            (1- choice)
            (etask-todo-get-effort-choicelist
             eeffort todocatp evcatp 'values)))
          (t 0))))

(defun etask-todo-get-time(str)
  "Extract time from STR and return list \(hh mm 0)."
  (let ((hr 0)
        (min 0)
        (s 0)
        (timestr (if (string-match "\\(am\\|pm\\)" str)
                     (substring str 0 (match-beginning 0))
                   str)))
    (if (string-match ":" str)
        (progn
          (setq hr (string-to-int
                    (substring timestr 0 (match-beginning 0))))
          (setq min (string-to-int
                     (substring timestr (1+ (match-beginning 0))))))
      (setq hr (string-to-int str)))
    (cond ((and (string-match "pm" str) (< hr 12))
           (setq hr (+ 12 hr)))
          ((and (string-match "am" str) (= hr 12))
           (setq hr 0)))
    (list hr min 0)))

(defun etask-todo-get-duetime(todo &optional begin)
  "Get TODO begin time from minibuffer and return as list (h m s).  If
optional BEGIN is non-nil, get TODO begin time instead."
  (let* ((prompt (concat
                  (if begin
                      (etask-lang-msg 562 etask-language)
                    (etask-lang-msg 560 etask-language))
                  "  ? "))
         (choice
          (etask-read 
           prompt 
           (lambda (x) (or
                        (string-match etask-time-raw-regexp x)
                        (string-match etask-time-ampm-regexp x))))))
    (etask-todo-get-time choice)))

(defun etask-todo-get-duedate(todo &optional begin)
  "Get TODO due date from minibuffer and return value.  If optional
BEGIN is non-nil, get TODO begin date instead."
  (let* ((listprompt
          (list
           (etask-lang-msg 540 etask-language)
           (etask-lang-msg 542 etask-language)
           (etask-lang-msg 544 etask-language)
           (etask-lang-msg 546 etask-language)
           (etask-lang-msg 512 etask-language)))
         (prompt 
          (concat 
           (if begin
               (etask-lang-msg 305 etask-language)
             (etask-lang-msg 451 etask-language))
           ": "
           (etask-get-list-prompt listprompt)
           "  ? "))
         (choice
          (string-to-number
           (etask-read 
            prompt 
            (lambda (x) 
              (and (string-match etask-wholenumber-regexp x)
                   (>= (string-to-number x) 1) 
                   (<= (string-to-number x) (length listprompt)))))))
         (begindate))
    (cond ((= choice 1)
           (setq begindate (calendar-current-date)))
          ((= choice 2)
           (setq begindate
                 (etask-add-days-to-date (calendar-current-date) 1)))
          ((= choice 3)
           (setq begindate
                 (etask-add-days-to-date (calendar-current-date) 2)))
          ((= choice 4)
           (setq begindate
                 (etask-add-days-to-date (calendar-current-date) 7)))
          ((= choice 5)
           (setq begindate
                 (etask-get-taskdate 
                  (concat 
                   "'" (etask-db-get todo etask-db-attr-taskname) "'"))))
          (t
           (setq begindate (calendar-current-date))))
    (list begindate (etask-todo-get-duetime todo begin))))

(defun etask-todo-get-begin(due peffort)
  "Return ToDo begin date and time.  DUE is a list consisting of due
date and due time list.  PEFFORT is the planned effort in hours."
  (when (<= peffort etask-workinghours-per-day)
    (let* ((date (car due))
           (time (car (cdr due)))
           (hr (car time))
           (min (car (cdr time)))
           (duemin (round (+ (* hr 60) min)))
           (fwdmin (round (* peffort 60)))
           (newmin))
      (if (>= duemin fwdmin)
          (setq newmin (- duemin fwdmin))
        (setq date (etask-add-days-to-date date -1))
        (setq newmin (- (* 24 60) (- fwdmin duemin))))
      (setq hr (/ newmin 60))
      (setq min (% newmin 60))
      (list date (list hr min 0)))))

(defun etask-todo-get-initial-elementdata(todocat todonames ixlist)
  "Get all initial todo data and store it at IXLIST."
  (when todonames
    (let ((todo (etask-cat-generate-default-element 
                 todocat (car todonames)))
          peffort due begin)
      (setq todo
            (etask-db-set todo etask-db-attr-priority 
                          (etask-todo-get-priority todo)))
      (setq peffort (etask-todo-get-peffort todo))
      (setq todo (etask-db-set todo etask-db-attr-peffort peffort))

      (setq due (etask-todo-get-duedate todo))
      (setq todo
            (etask-db-set
             todo etask-db-attr-taskend due))
      (setq todo
            (etask-db-set
             todo etask-db-attr-taskbegin
             (etask-todo-get-begin due peffort)))
      (etask-cat-insert-element todo ixlist 'checkuniqueness))))

(defun etask-todo-link-elements(markedtodos)
  "Link marked todos according to lead or lag time values entered by
the user via minibuffer."
)

(defun etask-todo-one-day-chart-p()
  "Return true if chart's width is one day."
  (let ((start (etask-state-get etask-stateid-chartstart))
        (end (etask-state-get etask-stateid-chartend)))
    (and
     start end
     (calendar-date-is-legal-p start)
     (calendar-date-is-legal-p end)
     (calendar-date-equal start end))))

(defun etask-todo-organizer-view-p()
  "Return true if day view with scrolling enabled is active."
  (and (etask-todo-one-day-chart-p)
       (string= (etask-state-get etask-stateid-zoommodus) "p")))

(defun etask-todo-unit-is-hr-p()
  "Return true if chart's time unit is hour."
  (let ((zoommod (etask-state-get etask-stateid-zoommodus)))
    (and (etask-todo-one-day-chart-p)
         (or (string= zoommod "p")
             (string= zoommod "d")
             (string= zoommod "a")))))

(defun etask-is-todo-item-p(item)
  "Return true if ITEM is a ToDo item.  ToDo items start and end at
the same day and have a priority."
  (and (etask-element-is-valid-p item)
       (etask-is-milestone-p item)
       (etask-db-get item etask-db-attr-priority)))

(defun etask-todo-current-hour()
  "Scroll todo view to or near current hour."
  (interactive)
  (etask-todo-scroll-hours "current"))

(defun etask-todo-first-hour()
  "Scroll todo view to the first hour."
  (interactive)
  (etask-todo-scroll-hours "first"))

(defun etask-todo-last-hour()
  "Scroll todo view to the last hour."
  (interactive)
  (etask-todo-scroll-hours "last"))
  
(defun etask-todo-previous-hour()
  "Scroll todo view one hour to the right."
  (interactive)
  (etask-todo-scroll-hours "previous"))

(defun etask-todo-next-hour()
  "Scroll todo view one hour to the left."
  (interactive)
  (etask-todo-scroll-hours "next"))

(defun etask-todo-scroll-hours(cmd)
  "Scroll according to CMD."
  (interactive)
  (when (etask-todo-organizer-view-p)
    (let* ((currbeg (etask-state-get etask-stateid-chartstarttime))
           (currend (etask-state-get etask-stateid-chartendtime))
           (dist (etask-todo-get-hrnum))
           (newbeg (cond ((string= cmd "current")
                          (etask-todo-validate-begintime
                           (etask-todo-get-current-hour)))
                         ((string= cmd "first")
                          (etask-todo-validate-begintime 0))
                         ((string= cmd "last")
                          (etask-todo-validate-begintime 23))
                         ((string= cmd "previous")
                          (max (1- (etask-state-get
                                    etask-stateid-chartstarttime))
                               0))
                         ((string= cmd "next")
                          (min (1+ (etask-state-get
                                    etask-stateid-chartstarttime))
                               (- 24 dist)))
                         (t
                          currbeg)))
           (newend (if (> newbeg currbeg)
                       (+ currend (- newbeg currbeg))
                     (- currend (- currbeg newbeg)))))
      (etask-state-set etask-stateid-chartstarttime newbeg)
      (etask-state-set etask-stateid-chartendtime (+ newbeg dist))
      (etask-cat-show-elements))))

(defun etask-todo-validate-begintime(time)
  "Return Todo view begin TIME as full hour if it is valid, otherwise
the closest valid begin time."
  (let ((hours (if (> etask-todo-timewidth 0)
                   (min 24
                        (/ (etask-get-maxbarlen) etask-todo-timewidth))
                 2)))
    (if (and (natnump time) (<= time (- 24 hours)))
        time
      (- 24 hours))))

(defun etask-todo-get-current-hour()
  (nth 2 (decode-time (current-time))))

(defun etask-todo-insert-timelinelabels(date)
  "Insert todo timeline for day DATE."
  (let* ((fixedp (not (etask-todo-organizer-view-p)))
         (daylen (etask-state-get etask-stateid-maxbarlen))
         (hrsnum (when fixedp
                   (max 3
                        (- etask-last-workinghour
                           etask-first-workinghour))))
         (hrwidth (when fixedp
                    (/ daylen hrsnum)))
         (step (if (and fixedp (natnump hrwidth) (> hrwidth 0))
                   (max 1
                        (ceiling (/ (float etask-todo-timewidth) hrwidth)))
                 1))
         (currhour (etask-todo-get-current-hour))
         (cstime (etask-state-get etask-stateid-chartstarttime))
         (time (if (natnump cstime) cstime etask-first-workinghour))
         (lasthr (min (+ time (etask-todo-get-hrnum)) 24))
         (hint (if fixedp "" "<jk>"))
         (leadingspclen (+ 1 (etask-state-get etask-stateid-maxtasklen)))
         (leadingspc (make-string leadingspclen ? ))
         marker currtimestr timestr)
    (when (not fixedp)
      (etask-state-clear etask-stateid-todostate))
    (while (<= time lasthr)
      (setq marker (concat marker 
                           etask-todo-time-marker
                           (when (< time lasthr)
                             (make-string
                              (- etask-todo-timewidth 
                                 (length etask-todo-time-marker))
                              ? ))))
      (when (< time lasthr)
        (setq currtimestr (concat (when (< time 10)
                                    "0")
                                  (number-to-string time)
                                  ":00 "))
        (setq timestr (concat timestr
                              (if (or (= currhour time)
                                      (= (1+ currhour) time))
                                  (etask-apply-face
                                   currtimestr
                                   'etask-face-today)
                                currtimestr)
                              (make-string
                               (max 0
                                    (- etask-todo-timewidth 
                                       (length currtimestr)))
                               ? ))))
      (setq time (+ time step)))
    (insert leadingspc)
    (insert marker)
    (insert "\n")
    (etask-format-insert hint leadingspclen "center")
    (insert timestr)
    (insert "\n")
    (insert leadingspc)
    (etask-format-insert
     (if (calendar-date-equal (calendar-current-date) date)
         (etask-apply-face
          (calendar-date-string date) 
          'etask-face-today)
       (calendar-date-string date))
     daylen
     "center")
    (insert "\n")))


;;; Module Initialization

(setq etask-todo-loaded-p t)
(provide 'etask-todo)


;;; etask-todo.el  end of file