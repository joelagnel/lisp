;;; etask-db.el --- part of etask

;; Copyright (C) 2004 René Weichselbaum

;; Author: Rene Weichselbaum <rene (at) reneweichselbaum (dot) com>

;; $Id: etask-db.el,v 1.20 2004/10/29 11:05:44 rene Exp $

;; Keywords: calendar 

;; Human-Keywords: task management, bar chart, Gantt chart, project
;; management, todo list, personal information management

;; See `etask.el' to find out which software components you need to
;; run etask.

;; URL: http://www.reneweichselbaum.com/etask.html


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

;; This software component implements low level data access functions
;; for etask.el


;; _________________


;;; Code:

(defconst etask-db-attr-taskname 100 "Task name ID.")
(defconst etask-db-ix-taskname 0 "Index of task name within task
structure.")

(defconst etask-db-attr-projname 110 "Project name ID.")
(defconst etask-db-ix-projname 1 "Index of project name within task
structure.")

(defconst etask-db-attr-tracking 120 "Tracking algorithm ID.")
(defconst etask-db-ix-tracking 2 "Index of tracking algorithm within
task structure.")

(defconst etask-db-attr-tasktype 130  "Task type ID.")
(defconst etask-db-ix-tasktype 3 "Index of task type within task
structure.")

(defconst etask-db-attr-peffort 140 "Planned effort ID.")
(defconst etask-db-ix-peffort 4 "Index of planned effort within
task structure.")

(defconst etask-db-attr-eeffort 150 "Expended effort ID.")
(defconst etask-db-ix-eeffort 5 "Index of expended effort within
task structure.")

(defconst etask-db-attr-taskbegin 160 "Task begin ID.")
(defconst etask-db-attr-taskbegintime 161 "Task begin time ID.")
(defconst etask-db-ix-taskbegin 6 "Index of task begin within task
structure.")

(defconst etask-db-attr-taskend 170 "Task end ID.")
(defconst etask-db-attr-taskendtime 171 "Task end time ID.")
(defconst etask-db-ix-taskend 7 "Index of task end within task
structure.")

(defconst etask-db-attr-mark 180 "Mark ID.")
(defconst etask-db-ix-mark 8 "Index of mark within task structure.")

(defconst etask-db-attr-priority 190 "Priority ID.")
(defconst etask-db-ix-priority 9 "Index of priority within task structure.")

(defconst etask-db-attr-category 200 "Category ID.")
(defconst etask-db-ix-category 10 "Index of category within task structure.")

(defconst etask-db-attr-policy 210 "Policy ID.")
(defconst etask-db-ix-policy 11 "Index of policy within task structure.
The policy deals with the issue of missing the deadline.")

(defconst etask-db-attr-people 220 "People ID.")
(defconst etask-db-ix-people 12 "Index of people within task structure.")

(defconst etask-db-attr-arglist 230 "Argument list ID.")
(defconst etask-db-ix-arglist 13 "Index of list with other task
attributes within task structure.")

(defconst etask-db-attr-subtasklist 240 "Sub task ID.")
(defconst etask-db-ix-subtasklist 14 "Index of sub task list within
task structure.")

(defun etask-element-is-valid-p(element)
  "Return true if ELEMENT's structure and content seems to be ok, nil
otherwise.  Checks just the most important things..."
  (let ((begindate (etask-db-get element etask-db-attr-taskbegin))
        (enddate (etask-db-get element etask-db-attr-taskend))
        (peffort (etask-db-get element etask-db-attr-peffort))
        (eeffort (etask-db-get element etask-db-attr-eeffort)))
    (and (listp element)
         element
         (stringp (etask-db-get element etask-db-attr-taskname))
         (stringp (etask-db-get element etask-db-attr-projname))
         (stringp (etask-db-get element etask-db-attr-tracking))
         (stringp (etask-db-get element etask-db-attr-tasktype))
         (and (numberp peffort) (>= peffort 0))
         (and (numberp eeffort) (>= eeffort 0))
         begindate
         (etask-calendardate-is-legal-p begindate)
         enddate
         (etask-calendardate-is-legal-p enddate)
         (stringp (etask-db-get element etask-db-attr-mark))
         (listp (etask-db-get element etask-db-attr-subtasklist)))))

(defun etask-write-elements(elements &optional catid item erase)
  "Update existing or store new ELEMENTS in current item buffer.  If
optional CATID and ITEM define a valid item its buffer is used
instead.  If optional ERASE is non-nil erase the buffer before storing
ELEMENTS."
  (let ((buf (cond ((and catid item)
                    (etask-cat-get-itembuffer catid item))
                   (t
                    (etask-cat-get-current-itembuffer)))))
    (save-current-buffer
      (set-buffer buf)
      (cond (erase
             (erase-buffer)
             (while elements
               (goto-char (point-max))
               (when (not (bolp))
                 (insert "\n"))
               (etask-write-op (car elements) buf)
               (setq elements (cdr elements))))
            (t
             (while elements
               (etask-write-kill-and-goto-pos (car elements))
               (etask-write-op (car elements) buf)
               (setq elements (cdr elements)))))
      (save-buffer))))

(defun etask-write-element(element &optional index catid item buf)
  "Update an existing or store a new ELEMENT.

If the element name in ELEMENT already exists its data is updated.  If
INDEX is non-nil ELEMENT is stored at INDEX.

If optional CATID and ITEM are non-nil, this specified item file is
used instead of the current item file.

If optional CATID and ITEM are nil but optional BUF is non-nil then
buffer BUF is the target of this write operation.

ELEMENT is a list containing all information regarding this
element. Here is its structure:

  0  ELEMENTNAME...string
  1  ITEMNAME...string
  2  TRACKING...string in `etask-tracking-algorithms-alist'
  3  ELEMENTTYPE...string in {`etask-normaltask-string', 
                       `etask-highrisktask-string',
                       `etask-criticaltask-string'}
  4  PLANNED_EFFORT...the planned effort in hours, a nonnegative integer
  5  EXPENDED_EFFORT...the already expended effort in hours, a nn integer
  6  BEGIN...date (mm dd yyyy), optional time (h m s)
             Examples:  (6 16 2004)  or  ((6 16 2004) (14 0 0))
  7  END...date (mm dd yyyy), optional time (h m s)
  8  MARK...string in {'u', 'm'}, where 'u' means unmarked and 'm'
            stands for marked
  9  PRIORITY...non-negative integer
 10  CATEGORY...string
 11  POLICY...string
 12  PEOPLE...string list
 13  ARGLIST...other attributes in a list
 14  SUBELEMENTLIST...list of subelements"
  
  (let ((buf (cond ((and catid item)
                    (etask-cat-get-itembuffer catid item))
                   ((and (not catid) (not item) buf)
                    buf)
                   (t
                    (etask-cat-get-current-itembuffer)))))
    (save-current-buffer
      (set-buffer buf)
      (etask-write-kill-and-goto-pos element index)
      (etask-write-op element buf)
      (save-buffer)
      t)))

(defun etask-write-kill-and-goto-pos(element &optional index)
  "Delete old element if necessary and go to correct position."
  (let ((name (etask-db-get element etask-db-attr-taskname)))
    (goto-char (point-min))
    (cond ((search-forward (etask-get-elementregexp name) nil t) 
           ;; element name already exists => element has just been
           ;;   edited without changing its name
           ;; => update by 1) killing old data and 2) inserting new data
           (forward-line 0)
           (kill-line))
          (index
           (goto-char (point-min))
           (forward-line (1- index))
           ;; If point is already in the last line then another
           ;; `forward-line' moves pos to end of last line - but
           ;; then this element would share the same line with
           ;; another element. We need to create a new line to
           ;; insert the element in the following line
           (if (< (etask-current-line) index)
               (insert "\n")))
          (t
           (goto-char (point-max))
           (when (not (bolp)) (insert "\n"))))))

(defun etask-write-op(element buffer)
  "Write ELEMENT into BUFFER."
  (prin1 element buffer)
  (if (not (eolp))                      ;one line, one element
      (insert "\n")))

(defun etask-extract-effort-days(inputstring)
  "Return INPUTSTRINGs effort in days for units hours, days, and weeks."
  (/ (etask-extract-effort-hours inputstring) etask-workinghours-per-day))

(defun etask-extract-effort-raw(inputstring)
  "Return INPUTSTRINGs effort for days, weeks, and months raw,
i.e. without the unit symbol."
  (when (string-match etask-effort-dwm-regexp inputstring)
    (string-to-number
     (substring inputstring
                (string-match etask-effort-num-regexp inputstring)
                (match-end 0)))))

(defun etask-extract-effort-hours(inputstring)
  "Return INPUTSTRINGs effort in hours for units m, h, d, and w,
i.e. minutes, hours, days, and weeks, respectively."
  (let* ((effort 
          (string-to-number 
           (substring inputstring
                      (string-match etask-effort-num-regexp 
                                    inputstring)
                      (match-end 0))))
         (unit (when (string-match etask-effort-regexp inputstring)
                 (substring inputstring
                            (string-match etask-effort-units-regexp
                                          inputstring)
                            (match-end 0)))))
    (cond ((string= unit etask-effort-unit-week)
           (etask-simplify-number
            (* etask-workinghours-per-day 
               etask-workingdays-per-week
               effort)))
          ((string= unit etask-effort-unit-day)
           (etask-simplify-number
            (* etask-workinghours-per-day
               effort)))
          ((string= unit etask-effort-unit-hour)
           (etask-simplify-number effort))
          ((string= unit etask-effort-unit-minute)
           (etask-simplify-number (/ effort 60.0)))
          (t
           0))))

(defun etask-get-elementregexp(taskname)
  "Return a regexp string for searching for the task named TASKNAME.
TASKNAME is a string."
  (concat "\"" taskname "\""))

(defun etask-db-set(task attribute &optional value)
  "Set ATTRIBUTE in TASK to VALUE.  If attribute is invalid return
nil.  If optional value is nil the attribute is to toggle."
  (cond ((= attribute etask-db-attr-taskname)
         (nconc
          (etask-get-sublist task 1 etask-db-ix-taskname)
          (list value)
          (nthcdr (1+ etask-db-ix-taskname) task)))
        ((= attribute etask-db-attr-projname)
         (nconc
          (etask-get-sublist task 1 etask-db-ix-projname)
          (list value)
          (nthcdr (1+ etask-db-ix-projname) task)))
        
        ((= attribute etask-db-attr-tracking)
         (nconc
          (etask-get-sublist task 1 etask-db-ix-tracking)
          (list value)
          (nthcdr (1+ etask-db-ix-tracking) task)))
        ((= attribute etask-db-attr-tasktype)
         (nconc
          (etask-get-sublist task 1 etask-db-ix-tasktype)
          (if (string= (etask-db-get task etask-db-attr-tasktype)
                       etask-normaltask-string)
              (list value)
            (list etask-normaltask-string))
          (nthcdr (1+ etask-db-ix-tasktype) task)))
        ((= attribute etask-db-attr-peffort)
         (nconc
          (etask-get-sublist task 1 etask-db-ix-peffort)
          (list value)
          (nthcdr (1+ etask-db-ix-peffort) task)))
        ((= attribute etask-db-attr-eeffort)
         (nconc
          (etask-get-sublist task 1 etask-db-ix-eeffort)
          (list value)
          (nthcdr (1+ etask-db-ix-eeffort) task)))
        ((= attribute etask-db-attr-taskbegin)
         (let ((old (etask-db-get task etask-db-attr-taskbegintime)))
           (when (and (listp old) (> (length old) 0) 
                      (not (listp (car value))))
             (setq value (list value old)))
           (nconc
            (etask-get-sublist task 1 etask-db-ix-taskbegin)
            (list value)
            (nthcdr (1+ etask-db-ix-taskbegin) task))))
        ((= attribute etask-db-attr-taskend)
         (let ((old (etask-db-get task etask-db-attr-taskendtime)))
           (when (and (listp old) (> (length old) 0)
                      (not (listp (car value))))
             (setq value (list value old)))
           (nconc
            (etask-get-sublist task 1 etask-db-ix-taskend)
            (list value)
            (nthcdr (1+ etask-db-ix-taskend) task))))
        ((= attribute etask-db-attr-mark)
         (nconc
          (etask-get-sublist task 1 etask-db-ix-mark)
          (if (string= (etask-db-get task etask-db-attr-mark)
                       etask-unmarkedtask-string)
              (list etask-markedtask-string)
            (list etask-unmarkedtask-string))
          (nthcdr (1+ etask-db-ix-mark) task)))
        ((= attribute etask-db-attr-priority)
         (nconc
          (etask-get-sublist task 1 etask-db-ix-priority)
          (list value)
          (nthcdr (1+ etask-db-ix-priority) task)))
        ((= attribute etask-db-attr-category)
         (nconc
          (etask-get-sublist task 1 etask-db-ix-category)
          (list value)
          (nthcdr (1+ etask-db-ix-category) task)))
        ((= attribute etask-db-attr-policy)
         (nconc
          (etask-get-sublist task 1 etask-db-ix-policy)
          (list value)
          (nthcdr (1+ etask-db-ix-policy) task)))
        ((= attribute etask-db-attr-people)
         (nconc
          (etask-get-sublist task 1 etask-db-ix-people)
          (list value)
          (nthcdr (1+ etask-db-ix-people) task)))
        ((= attribute etask-db-attr-arglist)
         (nconc
          (etask-get-sublist task 1 etask-db-ix-arglist)
          (list value)
          (nthcdr (1+ etask-db-ix-arglist) task)))
        ((= attribute etask-db-attr-subtasklist)
         (nconc
          (etask-get-sublist task 1 etask-db-ix-subtasklist)
          (list value)
          (nthcdr (1+ etask-db-ix-subtasklist) task)))
        (t
         ())))

(defun etask-db-get(task attribute)
  "Get ATTRIBUTE of TASK.  If attribute is invalid return nil."
  (cond ((= attribute etask-db-attr-taskname)
         (nth etask-db-ix-taskname task))

        ((= attribute etask-db-attr-projname)
         (nth etask-db-ix-projname task))

        ((= attribute etask-db-attr-tracking)
         (nth etask-db-ix-tracking task))

        ((= attribute etask-db-attr-tasktype)
         (nth etask-db-ix-tasktype task))

        ((= attribute etask-db-attr-peffort)
         (nth etask-db-ix-peffort task))

        ((= attribute etask-db-attr-eeffort)
         (nth etask-db-ix-eeffort task))

        ((= attribute etask-db-attr-taskbegin)
         (let ((datetime (nth etask-db-ix-taskbegin task)))
           (if (listp (car datetime))
               (car datetime)
             datetime)))

        ((= attribute etask-db-attr-taskbegintime)
         (let ((datetime (nth etask-db-ix-taskbegin task)))
           (if (listp (car datetime))
               (car (cdr datetime))
             nil)))

        ((= attribute etask-db-attr-taskend)
         (let ((datetime (nth etask-db-ix-taskend task)))
           (if (listp (car datetime))
               (car datetime)
             datetime)))

        ((= attribute etask-db-attr-taskendtime)
         (let ((datetime (nth etask-db-ix-taskend task)))
           (if (listp (car datetime))
               (car (cdr datetime))
             nil)))

        ((= attribute etask-db-attr-mark)
         (nth etask-db-ix-mark task))

        ((= attribute etask-db-attr-priority)
         (nth etask-db-ix-priority task))

        ((= attribute etask-db-attr-category)
         (nth etask-db-ix-category task))

        ((= attribute etask-db-attr-policy)
         (nth etask-db-ix-policy task))

        ((= attribute etask-db-attr-people)
         (nth etask-db-ix-people task))

        ((= attribute etask-db-attr-arglist)
         (nth etask-db-ix-arglist task))

        ((= attribute etask-db-attr-subtasklist)
         (nth etask-db-ix-subtasklist task))
        (t
         ())))


;;; Initialization

(setq etask-db-loaded-p t)
(provide 'etask-db)


;;; etask-db.el  end of file