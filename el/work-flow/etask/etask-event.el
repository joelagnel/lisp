;;; etask-event.el --- part of EtaskMode (main file: etask.el)

;; Copyright (C) 2004 René Weichselbaum

;; Author: Rene Weichselbaum

;; $Id: etask-event.el,v 1.8 2004/11/01 21:34:43 rene Exp $

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

;; This software component implements the EtaskMode events part.


;; _________________


;;; Code:


(defun etask-ev-insert-elements(evcat ixlist)
  "Add a new event to event category EVCAT."
  (when (etask-cat-is-item-p etask-category-eventid evcat)
    (etask-ev-get-initial-elementdata
     evcat (etask-get-labels) ixlist)))

(defun etask-ev-insert-elementbar-p(element)
  "Return true if ELEMENT's bar is to insert into Gantt chart, nil
otherwise."
  (etask-proj-insert-elementbar-p element))

(defun etask-ev-insert-head(begintime adjustbeg)
  "Insert '[----' part of an event including leading spaces.
BEGINTIME is the start of the event in \(hr min sec\).  ADJUSTBEG is a
non-negative integer representing the width in characters that is to
add to the full hour position of the event's start when displaying it
in the Gantt chart."
 (let* ((chstarthr (etask-state-get etask-stateid-chartstarttime))
        (hrs (etask-todo-get-hrnum))
        (hrwidth (etask-todo-get-hrwidth))
        (daywidth (* hrs hrwidth))
        (leadingspc (+ adjustbeg
                       (* hrwidth (- (car begintime) chstarthr)))))
   (when (> daywidth leadingspc)
     (insert (make-string leadingspc ? ))
     (insert "[")
     (insert (make-string
              (min (- daywidth leadingspc 1)
                   evwidth)
              ?-)))))

(defun etask-ev-insert-tail(endtime adjustend)
  (let* ((chstarthr (etask-state-get etask-stateid-chartstarttime))
         (dashnum (max
                   1
                   (1-                  ;']' replaces last dash
                    (+ adjustend
                       (* hrwidth
                          (- (car endtime) chstarthr)))))))
    (insert (make-string dashnum ?-))
    (insert "]")))

(defun etask-ev-insert-daybar(ev)
  "Insert EV's bar when time unit = hour."
  (let* ((chstart  (etask-state-get etask-stateid-chartstart))
         (chstarthr (etask-state-get etask-stateid-chartstarttime))
         (daywidth (etask-state-get etask-stateid-maxbarlen))
         (hrs (etask-todo-get-hrnum))
         (maxbarlen (etask-state-get etask-stateid-maxbarlen))
         (chendhr (etask-state-get etask-stateid-chartendtime))
         (begindate (etask-db-get ev etask-db-attr-taskbegin))
         (enddate (etask-db-get ev etask-db-attr-taskend))
         (currdate-is-first-day-p (calendar-date-equal chstart begindate))
         (currdate-is-last-day-p (calendar-date-equal chstart enddate))
         (rawbegintime (etask-db-get ev etask-db-attr-taskbegintime))
         (begintime (if currdate-is-first-day-p rawbegintime
                      (list etask-first-workinghour 0 0)))
         (rawendtime (etask-db-get ev etask-db-attr-taskendtime))
         (endtime (if currdate-is-last-day-p
                      rawendtime
                    (list (min 24 (+ (car begintime )
                                     etask-workinghours-per-day))
                          0 0)))
         (earlierhrs (- (car begintime) chstarthr))
         (visbegintimep (etask-todo-is-visible-time-p
                         begindate begintime 'event))
         (visendtimep (etask-todo-is-visible-time-p
                       enddate endtime 'event))
         (hrwidth (etask-todo-get-hrwidth))
         (evstr (etask-ev-get-eventstr ev hrwidth))
         (evwidth (min (length evstr) maxbarlen))
         (adjustbeg (etask-todo-adjust-tododuetime begintime hrwidth))
         (adjustend (etask-todo-adjust-tododuetime endtime hrwidth)))
    (cond ((and currdate-is-first-day-p visbegintimep 
                currdate-is-last-day-p visendtimep)
           ;;     | ... [--------] ... |
           (let* ((leadingspc (+ (* earlierhrs hrwidth)
                                 adjustbeg)))
             (insert (make-string leadingspc ? ))
             (etask-format-insert
              evstr (max (round hrwidth) evwidth) "left")))
          ((and (not visbegintimep)
                currdate-is-last-day-p
                visendtimep)
           ;;     | ----]          ... |
           (etask-ev-insert-tail endtime adjustend))
          ((and currdate-is-first-day-p visbegintimep (not visendtimep))
           ;;     | ...          [---- |
           (etask-ev-insert-head begintime adjustbeg))
          ((and (not visbegintimep) (not visendtimep)
                (or
                 (and (calendar-date-equal chstart begindate)
                      (< (car begintime) chstarthr))
                 (calendar-date-compare (list chstart) (list begindate)))
                (or
                 (and (calendar-date-equal chstart enddate)
                      (< chendhr (car endtime)))
                 (calendar-date-compare (list chstart) (list enddate))))
           ;; ... | ------------------ |
           (insert(make-string (* hrs hrwidth) ?-)))
          ((and (not visbegintimep) (not visendtimep)
                (calendar-date-equal chstart enddate)
                (< (car begintime) chstarthr)
                (< (car endtime) chendhr))
           ;; ... | <eff]          ... |
           (insert (etask-ev-get-eventstr ev nil 'effort "<")))
          ((and (not visbegintimep) (not visendtimep)
                (calendar-date-equal chstart begindate)
                (> (car begintime) chstarthr)
                (> (car endtime) chendhr))
           ;; ... | ...          [eff> |
           (let ((str (etask-ev-get-eventstr ev nil 'effort nil ">")))
             (insert (make-string (max 0 (- daywidth (length str))) ? ))
             (insert str)))
          (t
           ()))))
  
(defun etask-ev-get-bar(ev hrwidth)
  "Return EV's bar as a string.  HRWIDTH is the width of an hour in
characters."
  (when (and (natnump hrwidth) (> hrwidth 0))
    (let* ((planned-hours (etask-simplify-number
                           (etask-db-get ev etask-db-attr-peffort)))
           (expended-hours (etask-simplify-number
                            (etask-db-get ev etask-db-attr-eeffort)))
           (barlen
            (max
             1
             (-
              (round (etask-simplify-number (* planned-hours hrwidth)))
              2))))                     ;-2 for '[' and ']'
      (cond ((etask-todo-is-completed-p ev)
             (make-string barlen ?=))
            ((and (numberp expended-hours) (> expended-hours 0))
             (make-string barlen ?~))
            (t
             (make-string barlen ?-))))))

(defun etask-ev-get-eventstr(ev &optional hrwidth effort strbeg strend)
  "Return EV string for Gantt chart.  Optional HRWIDTH is the width of
an hour in characters.  If optional EFFORT is non-nil, HRWIDTH is not
used and EV's effort is inserted instead of a bar.  If optional
STRBEG or STREND is non-nil, this character is used to define the
string's begin and end, respectively."
  (concat
   (if strbeg strbeg "[")
   (if effort
       (etask-get-effort-str (etask-db-get ev etask-db-attr-peffort))
     (etask-ev-get-bar ev hrwidth))
   (if strend strend "]")))

(defun etask-ev-is-visible-day-p(ev start end)
  "Return true if EV is at least partially visible between calendar
dates START and END."
  (let ((evstart (etask-db-get ev etask-db-attr-taskbegin))
        (evend (etask-db-get ev etask-db-attr-taskend)))
    (or (calendar-date-equal evstart start)
        (calendar-date-equal evend start)
        (calendar-date-equal evstart end)
        (calendar-date-equal evend end)
        (and (calendar-date-compare (list start) (list evstart))
             (calendar-date-compare (list evstart) (list end)))
        (and (calendar-date-compare (list evstart) (list end))
             (calendar-date-compare (list end) (list evend))))))

(defun etask-ev-insert-bar(ev chstart chend daylen)
  "Insert EV's bar in chart where CHSTART is the chart's start date,
CHEND its end date, and DAYLEN the length of a day in characters."
  (when (etask-ev-is-visible-day-p ev chstart chend)
    (if (etask-todo-unit-is-hr-p)
        (etask-ev-insert-daybar ev)
      (let* ((evdate (etask-db-get ev etask-db-attr-taskbegin))
             (earlierdays
              (if (calendar-date-compare 
                   (list chstart) (list evdate))
                  (etask-days-between chstart evdate)
                0))
             (evstr (etask-ev-get-eventstr ev (/ daylen 24.0) 'effort))
             (evstrlen (length evstr))
             (leadingspc 
              (if (>= daylen evstrlen)
                  (round (* daylen earlierdays)) 
                (round (- (* earlierdays daylen) (- evstrlen daylen))))))
        (when (< leadingspc 0)
          (setq leadingspc 0)
          (setq evstr (store-substring evstr 0 "<")))
        (insert (make-string leadingspc ? ))
        (etask-format-insert 
         evstr (max (round daylen) evstrlen) "left")))))

(defun etask-ev-show-elementstatus(ev &optional reportingp)
  "Print event status after `etask-statusheader'.  If REPORTINGP is
non-nil, output goes to the reporting file instead."
  (let* ((name (etask-db-get ev etask-db-attr-taskname))
         (bdate (etask-db-get ev etask-db-attr-taskbegin))
         (begindate (calendar-date-string bdate t))
         (begintime (etask-db-get ev etask-db-attr-taskbegintime))
         (beginhr (number-to-string (car begintime)))
         (beginmin (number-to-string (car (cdr begintime))))
         (edate (etask-db-get ev etask-db-attr-taskend))
         (enddate (calendar-date-string edate t))
         (endtime (etask-db-get ev etask-db-attr-taskendtime))
         (endhr (number-to-string (car endtime)))
         (endmin (number-to-string (car (cdr endtime)))))
    (when (= (length beginmin) 1)
      (setq beginmin (concat "0" beginmin)))
    (when (= (length endmin) 1)
      (setq endmin (concat "0" endmin)))
    (concat
     (etask-apply-face (concat "'" name "'")
                       'etask-face-statusheader-onscreen)
     "    "
     (if (calendar-date-equal bdate edate)
         (concat
          begindate ", " beginhr ":" beginmin 
          " - " 
          endhr ":" endmin)
       (concat
        begindate ", " beginhr ":" beginmin 
        " - " 
        enddate ", " endhr ":" endmin)))))

(defun etask-ev-edit-element(ev)
  "Edit EV data not covered by etask-cat-edit-element."
  ev)

(defun etask-ev-get-peffort(ev)
  "Get EV's planned effort from minibuffer and return changed EV."
  (let* ((peffort (etask-todo-get-peffort ev))
         (ev (etask-db-set ev etask-db-attr-peffort peffort))
         (due (etask-ev-get-due ev peffort)))
    (etask-db-set ev etask-db-attr-taskend due)))

(defun etask-ev-get-begindate(ev)
  "Get EV begin date from minibuffer and return value."
  (etask-todo-get-duedate ev 'begin))

(defun etask-ev-get-due(ev peffort)
  "Return event due date and time.  BEGIN is a list consisting of
begin date and begin time list.  PEFFORT is the planned effort in
hours."
  (let* ((begin (etask-make-begindatetime ev))
         (end (etask-datetime-add-time begin peffort)))
;;          (manipulatep (and (= (etask-extract-min end) 0)
;;                            (= (etask-extract-sec end) 0))))
;;     (when manipulatep
;;       (setq end (etask-datetime-subtract-time 
;;                  end 
;;                  (etask-simplify-number (/ 1.0 60)))))
    (list (etask-extract-date end) (etask-extract-time end))))

(defun etask-ev-edit-begindate(ev)
  "Edit EV's begin date via minibuffer.  Change its end date if
necessary."
  (let* ((ev (etask-db-set
              ev etask-db-attr-taskbegin
              (etask-ev-get-begindate ev)))
         (peffort (etask-db-get ev etask-db-attr-peffort))
         (due (etask-ev-get-due ev peffort)))
    (etask-db-set ev etask-db-attr-taskend due)))

(defun etask-ev-get-initial-elementdata(evcat evnames ixlist)
  "Get all initial event data and store it at IXLIST."
  (when evnames
    (let ((event (etask-cat-generate-default-element 
                  evcat (car evnames))))
      (setq event
            (etask-db-set
             event etask-db-attr-taskbegin
             (etask-ev-get-begindate event)))
      (setq event (etask-ev-get-peffort event))
      (etask-cat-insert-element event ixlist 'checkuniqueness))))

(defun etask-ev-link-elements(markedevings)
  "Link marked events according to lead or lag time values entered by
the user via minibuffer."
)


;;; Module Initialization

(setq etask-ev-loaded-p t)
(provide 'etask-event)


;;; etask-event.el  end of file