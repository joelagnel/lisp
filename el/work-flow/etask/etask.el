;;; etask.el --- EtaskMode: GNU Emacs extension for managing your projects

;; Copyright (C) 2004 René Weichselbaum

;; Author: Rene Weichselbaum

;; $Id: etask.el,v 1.80 2004/12/12 21:46:14 rene Exp $

;; Keywords: calendar

;; Human-Keywords: task management, project management, todo list,
;; Gantt chart

;; See the accompanying README file for detailed system requirements.

;; URL: http://members.chello.at/rene.weichselbaum/etask.html

;; Developed under GNU Emacs 21.3.1 (Distribution: Debian/testing)


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

;; This software is accompanied by a tutorial in pdf format and a
;; README file containing important setup information.

;; To start, do `M-x etask'.


;; -------------------------------------------------------------------
;;   Get the latest version at http://members.chello.at/rene.weichselbaum/

;;   EtaskMode is free software -- free as in freedom, and also free
;;   as in beer.

;;   I would like to ask you, dear EtaskMode user, for your
;;   collaboration.  I kindly invite you to send me your comments,
;;   suggestions, or remarks -- just do `C-c C-f' within EtaskMode or
;;   go to www.reneweichselbaum.com/contact.

;;   If you encounter failures, please report them by doing `C-c C-b'
;;   within EtaskMode.
;; -------------------------------------------------------------------


;; HAVE FUN!


;;; Code:

(require 'calendar)
(require 'diary-lib)
(require 'timezone)
(require 'cal-iso)
(require 'etask-tex)
(require 'etask-db) 
(require 'etask-port)
(require 'etask-lang)
(require 'etask-cat)
(require 'etask-proj)
(require 'etask-todo)
(require 'etask-event)

(defconst etask-release "0.3.91"
  "EtaskMode release number." )

(defgroup etask nil
  "Main EtaskMode Customization (Project Management and LaTeX Gantt
Charts)."
  :group 'applications)

(defgroup finetuning nil
  "Customize Various EtaskMode Details."
  :prefix "etask-"
  :group 'etask)

(defgroup screen nil
  "Customize Screen Layout for Emacs' EtaskMode Window."
  :prefix "etask-"
  :group 'etask)

(defgroup etaskfaces nil
  "Customize Faces for EtaskMode."
  :prefix "etask-"
  :group 'faces)

(defgroup latex nil
  "Customize all LaTeX related stuff for Gantt Chart Generation."
  :prefix "etask-tex-"
  :group 'etask)

(defcustom etask-language "english"
  "Default language for EtaskMode output.

Available languages so far: english, german

Ensure that you have configured your LaTeX system according to the
language specified here.

See also variable `etask-set-calendar-language-p' in the finetuning
group.

You can also specify a language not supported by EtaskMode.  Then this
setting is applied to the LaTeX Gantt chart and everything else is
displayed in English.

If your language is not yet supported, you can easily add it (see file
`etask-lang.el'.

Note that this customization procedure as well as logging activities
are not affected by this language variable."
  :type 'string
  :group 'etask)

(defcustom etask-organization ""
  "The name of your company to be put at the top left corner of the
Gantt chart pages.

Example: Ren\\'e Weichselbaum"
  :type 'string
  :group 'etask)

(defcustom etask-organization-slogan ""
  "A short slogan to be put at the top right corner of the Gantt chart pages.

Example: Sound Advice. Reliable Software."
  :type 'string
  :group 'etask)

(defcustom etask-max-taskname-len-chart 20
  "Maximum task name length (characters) in draft Gantt chart.

Also used for some minibuffer messages.

If you use longer names than defined here they are trimmed to fit.
However, nothing gets lost if a task name is longer than specified by
this variable.  The whole name (and not the trimmed one) is stored and
all status reports contain the entire name.  

Note that this variable affects only the draft Gantt chart, the chart
you can edit within Emacs.  It does not apply to the high-quality
LaTeX Gantt chart."
  :type 'integer
  :group 'screen)

(defcustom etask-normal-taskname-len-minibuf 12
  "Normal task name length in characters in minibuffer.

If necessary, longer task names are shortened within the minibuffer to
prevent line breaking.  For example the string 'Very loooooooooooong'
is displayed like 'Very loooo..'.

See also `etask-longer-taskname-len-minibuf'."
  :type 'integer
  :group 'screen)

(defcustom etask-longer-taskname-len-minibuf 22
  "Task name length in characters in minibuffer for short messages.
For example the string 'Very looooooooooooooooooooong' is displayed
like 'Very loooooooooooooo..'.

See also `etask-normal-taskname-len-minibuf'."
  :type 'integer
  :group 'screen)

(defcustom etask-earliest-taskstart-year 2002
  "Tasks must start at this year or later."
  :type 'integer
  :group 'finetuning)

(defconst etask-earliest-catdate '(1 1 1000)
  "Full date for earliest possible task")

(defconst etask-latest-catdate '(1 1 9999)
  "Full date for latest possible task")

(defconst etask-buffer "*EtaskMode*"
  "Name of the buffer used for task management.")

(defconst etask-report-buffer "*EtaskMode REPORTS*"
  "Name of the buffer used for reports (in a new frame).")

(defconst etask-unmarkedtask-string "u"
  "Specify an unmarked task in file `etask-tasks-filename'.")

(defconst etask-markedtask-string "m"
  "Specify a marked task in file `etask-tasks-filename'.")

(defconst etask-highrisktask-string "high-risk"
  "Specify a high risk task in file `etask-tasks-filename'.")

(defconst etask-normaltask-string "normal"
  "Specify a normal task in file `etask-tasks-filename'.")

(defconst etask-criticaltask-string "critical"
  "Specify a critical task in file `etask-tasks-filename'.")

(defconst etask-delimiter-marker "|"   ; -> etask-delimiter-regexp
  "String that delimits the name of an unmarked task from its bar.")

(defconst etask-delimiter-markedtask-marker "#" ; -> etask-delimiter-regexp
  "String that delimits the name of a marked task from its bar.")

(defconst etask-time-raw-regexp
  "^\\(\\(\\(1?[0-9]\\)\\|\\(2[0-3]\\)\\)\\(:[0-5][0-9]\\)?\\)$"
  "Regular expression for an element's start or end time without am/pm.")

(defconst etask-time-ampm-regexp
  "^\\(1[01]\\|[0-9]\\)\\(:[0-5][0-9]\\)?\\(am\\|\\pm\\)$"
  "Regular expression for an element's start or end time with am/pm.")

(defcustom etask-workinghours-per-day 8
  "Number of working hours per day."
  :type 'integer
  :group 'finetuning)

(defcustom etask-workingdays-per-week 5
  "Number of working days per week."
  :type 'integer
  :group 'finetuning)

(defcustom etask-first-workinghour 8
  "The earliest begin of your workday."
  :type 'integer
  :group 'finetuning)

(defcustom etask-last-workinghour 20
  "The latest end of your workday."
  :type 'integer
  :group 'finetuning)

(defcustom etask-use-bizdays t
  "If true then business days are used to calculate distance between
date, otherwise calendar days are used."
  :type 'boolean
  :group 'finetuning)

(defconst etask-effort-regexp "^[0-9]*\\.?[0-9]+[wdhm]$"
  "Regular expression for effort in weeks, days, hrs, or minutes.")

(defconst etask-effort-units-regexp "[wdhm]$"
  "Regular expression for effort units week, day, hr, and min.")

(defconst etask-effort-unit-hour "h" "Symbol for unit 'hour'.")

(defconst etask-effort-unit-day "d" "Symbol for unit 'day'.")

(defconst etask-effort-unit-week "w" "Symbol for unit 'week'.")

(defconst etask-effort-unit-minute "m" "Symbol for unit 'minute'.")

(defconst etask-todo-no-effort 5
  "Effort in minutes for trivial ToDo items.")

(defconst etask-todo-little-effort 15
  "Effort in minutes for little Todo items.")

(defconst etask-todo-standard-effort 30
  "Effort in minutes for standard ToDo items.")

(defconst etask-todo-significant-effort 60
  "Effort in minutes for more complex ToDo items.")

(defconst etask-todo-huge-effort 120
  "Effort in minutes for really complex todo items.")

(defconst etask-event-little-effort 4
  "Effort in hours for standard event items.")

(defconst etask-event-standard-effort etask-workinghours-per-day
  "Effort in hours for standard event items.")

(defconst etask-event-significant-effort
  (* etask-workinghours-per-day etask-workingdays-per-week)
  "Effort in hours for more complex event items.")

(defconst etask-event-huge-effort
  (* etask-workinghours-per-day etask-workingdays-per-week 2)
  "Effort in hours for really complex event items.")

(defconst etask-effort-dwm-regexp "^[0-9]*\\.?[0-9]+[dwm]$"
  "Regular expression for effort units day, week, and month.")

(defconst etask-effort-d-regexp "^[0-9]*\\.?[0-9]+d$"
  "Regular expression for valid efforts in days.")

(defconst etask-effort-w-regexp "^[0-9]*\\.?[0-9]+w$"
  "Regular expression for valid efforts in weeks.")

(defconst etask-effort-m-regexp "^[0-9]*\\.?[0-9]+m$"
  "Regular expression for valid efforts in months.")

(defconst etask-effort-numonly-regexp "^[0-9]*\\.?[0-9]*$"
  "Regular expression for effort without unit symbols.")

(defconst etask-effort-num-regexp "^[0-9]*\\.?[0-9]*"
  "Regular expression for effort with or without unit symbols.")

(defconst etask-wholenumber-regexp "^[0-9]+$"
  "Regular expression for valid whole number minibuffer inputs.")

(defconst etask-integernumber-regexp "^-*[0-9]+$"
  "Regular expression for valid integer minibuffer inputs.")

(defconst etask-elementfind-regexp "^(\""
  "Regular expression for an element and all its subelements in files
`etask-tasks-filename', `etask-todos-filename', or
`etask-events-filename'.")

(defconst etask-delimiter-regexp
  (concat "[" etask-delimiter-marker etask-delimiter-markedtask-marker
"]")
  "Regular expression to find the delimiter between element names and
its bars.")

(defconst etask-tracking-algorithms-alist '(("linear" 1)
("s-shape-65" 2) ("s-shape-70" 3))
  "Associated list containing the names of all algorithms available
for calculating a task status.")

(defvar etask-statusheader nil "Header for the current element's
status information.")

(defconst etask-subelements-prefix "'-"
  "Prefix identifying a sublevel item.")

(defcustom etask-holiday-regexp ""
  "Define the 'true' holidays within `calendar-holidays'.

This variable is used for calculating the business days between dates.
Sometimes holidays are working days.  The holiday names in
`calendar-holidays' that are matched by this regular expression are
the days on which you don't work.

Background:
Within etask free days are Saturdays, Sundays, and days in
`calendar-holidays' that match the regular expression specified here.
All other days are business days.

For example, consider the following holiday names in
`calendar-holidays' and assume that `etask-holiday-regexp' is set to
\"(free)\":
  \"holiday1 (free)\"
  \"holiday2\"

'holiday1' indicates a 'true' holiday for you and your staff.
'holiday2' may be a holiday for many people but not for you and your
staff."
  :type 'string
  :group 'etask)

(defcustom etask-default-tracking "s-shape-65"
  "Default tracking method applied for new tasks.

The following methods are available:
'linear', 's-shape-65', and 's-shape-75'.

You can easily change the method for each task individually within the
program.

The question of how to track a task or a whole project is a tough one
to answer.  But there is little doubt that the rate of progress is not
always linear.  It may even vary significantly in different phases of
a task or project.  Often, its real shape is typically of an
increasing and then decreasing slope, which vaguely looks like an S
shape.

That's why you can choose between three alternatives:

   - 'Linear' (50% completion after 50% of the time): 
     When, for example, half of the time for the task has elapsed, 
     it must be 50 percent completed.  After 70 percent of the time 
     you need 70 percent completion.

   - 'S-Shape-65' (65% completion after 50% of the time): 
     Take into account that progress is slow at the start, then reaches 
     its peak, and then slows again during the completion phase. In other
     words, when assessing the task status, an S-shape curve is used.
     For example, after 20 percent of the time the task must be 15
     percent completed, and after 50 percent you need at least 65 percent
     completion.

   - 'S-Shape-70' (70% completion after 50% of the time): 
     Till about 30 percent of the task's time there are very much the same
     completion requirements in place. But in order to have more flexibility 
     towards the planned task end the requirements for the middle phase are 
     tougher. 
     For example, after 70 percent of the time the task must be 90 percent
     completed."
  :type 'string
  :group 'finetuning)

(defcustom etask-default-type etask-normaltask-string
  "Default task type assigned to a task when newly inserted.

The following types are available:
`etask-normaltask-string' for a normal task,
`etask-highrisktask-string' for a high-risk task, and
`etask-criticaltask-string' for a critical task

High risk tasks contain very risky activities in terms of cost,
schedule, quality etc.

Critical tasks are tasks on the critical path. Therefore, they cannot
be delayed or take longer than its estimate without impacting the
whole project - at least in theory."
  :type 'string
  :group 'finetuning)

(defcustom etask-default-priority 0
  "Default priority assigned to tasks, todos and other elements when
newly inserted.

Priority range: from 0 to n>0
0...No priority, 1...Highest priority, n>1...lowest priority,
but still higher than zero"
  :type 'natnum
  :group 'finetuning)

(defcustom etask-show-projectdates-p t
  "If non-nil, project begin and end date are shown in draft bar
chart."
  :type 'boolean
  :group 'screen)

(defcustom etask-max-dataitems 60
  "Maximum number of elements in an EtaskMode category."
  :type 'integer
  :group 'finetuning)

(defcustom etask-max-fte 20
  "Maximum full-time equivalents you can assign to a task.

Staff members who work full-time on a task have a full-time
equivalence of 1.0.  Thus, a person who spends 30 percent of his or
her time on a task and the rest on other activities should be
considered as 0.3 FTE for this task."
  :type 'integer
  :group 'finetuning)

(defcustom etask-set-calendar-language-p nil
  "Set calendar language too when switching language."
  :type 'boolean
  :group 'finetuning)

(defcustom etask-element-killring-size 20
  "Specify size of the element kill ring."
  :type 'integer
  :group 'finetuning)

(if (boundp 'facemenu-unlisted-faces)
    (add-to-list 'facemenu-unlisted-faces 'etask-face-flag-delete))
(defface etask-face-flag-delete
  '((((class color))
     (:foreground "darkred" :weight bold))
    (t ()))
  "Face for 'delete' flag."
  :group 'etaskfaces)

(if (boundp 'facemenu-unlisted-faces)
    (add-to-list 'facemenu-unlisted-faces 'etask-face-flag-archive))
(defface etask-face-flag-archive
  '((((class color))
     (:foreground "darkred" :weight bold))
    (t ()))
  "Face for 'archive' flag."
  :group 'etaskfaces)

(if (boundp 'facemenu-unlisted-faces)
    (add-to-list 'facemenu-unlisted-faces 'etask-face-flag-post-elt))
(defface etask-face-flag-post-elt
  '((((class color))
     (:foreground "darkred" :weight bold))
    (t ()))
  "Face for 'postpone whole element' flag."
  :group 'etaskfaces)

(if (boundp 'facemenu-unlisted-faces)
    (add-to-list 'facemenu-unlisted-faces 'etask-face-flag-post-beg))
(defface etask-face-flag-post-beg
  '((((class color))
     (:foreground "darkred" :weight bold))
    (t ()))
  "Face for 'postpone begin date' flag."
  :group 'etaskfaces)

(if (boundp 'facemenu-unlisted-faces)
    (add-to-list 'facemenu-unlisted-faces 'etask-face-flag-post-dd))
(defface etask-face-flag-post-dd
  '((((class color))
     (:foreground "darkred" :weight bold))
    (t ()))
  "Face for 'postpone due date' flag."
  :group 'etaskfaces)

(if (boundp 'facemenu-unlisted-faces)
    (add-to-list 'facemenu-unlisted-faces 'etask-face-flag-fwd-elt))
(defface etask-face-flag-fwd-elt
  '((((class color))
     (:foreground "darkred" :weight bold))
    (t ()))
  "Face for 'forward whole element' flag."
  :group 'etaskfaces)

(if (boundp 'facemenu-unlisted-faces)
    (add-to-list 'facemenu-unlisted-faces 'etask-face-flag-fwd-beg))
(defface etask-face-flag-fwd-beg
  '((((class color))
     (:foreground "darkred" :weight bold))
    (t ()))
  "Face for 'forward begin date' flag."
  :group 'etaskfaces)

(if (boundp 'facemenu-unlisted-faces)
    (add-to-list 'facemenu-unlisted-faces 'etask-face-flag-fwd-dd))
(defface etask-face-flag-fwd-dd
  '((((class color))
     (:foreground "darkred" :weight bold))
    (t ()))
  "Face for 'forward due date' flag."
  :group 'etaskfaces)

(if (boundp 'facemenu-unlisted-faces)
    (add-to-list 'facemenu-unlisted-faces 'etask-face-flag-link))
(defface etask-face-flag-link
  '((((class color))
     (:foreground "darkred" :weight bold))
    (t ()))
  "Face for 'link elements' flag."
  :group 'etaskfaces)

(if (boundp 'facemenu-unlisted-faces)
    (add-to-list 'facemenu-unlisted-faces 'etask-face-flag-mark))
(defface etask-face-flag-mark
  '((((class color))
     (:foreground "darkred" :weight bold))
    (t ()))
  "Face for 'mark elements' flag."
  :group 'etaskfaces)

(if (boundp 'facemenu-unlisted-faces)
    (add-to-list 'facemenu-unlisted-faces 'etask-face-holiday))
(defface etask-face-holiday
  '((((class color))
     (:foreground "darkred" :weight bold))
    (t ()))
  "Face for holidays on the project timeline."
  :group 'etaskfaces)

(if (boundp 'facemenu-unlisted-faces)
    (add-to-list 'facemenu-unlisted-faces 'etask-face-today))
(defface etask-face-today
  '((((class color))
     (:foreground "red"))
    (t ()))
  "Face for current date and its marker on the project timeline."
  :group 'etaskfaces)

(if (boundp 'facemenu-unlisted-faces)
    (add-to-list 'facemenu-unlisted-faces 'etask-face-day-marker))
(defface etask-face-day-marker
  '((((class color))
     (:foreground "black"))
    (t ()))
  "Face for day markers on the project timeline."
  :group 'etaskfaces)

(if (boundp 'facemenu-unlisted-faces)
    (add-to-list 'facemenu-unlisted-faces 'etask-face-specialdelimiter))
(defface etask-face-specialdelimiter
  '((((class color))
     (:foreground "red"))
    (t ()))
  "Face for delimiters of elements 5 and 9 for quick navigation."
  :group 'etaskfaces)

(if (boundp 'facemenu-unlisted-faces)
    (add-to-list 'facemenu-unlisted-faces 'etask-face-planned))
(defface etask-face-planned
  '((((class color))
     (:foreground "gray60"))
    (t ()))
  "Task bar face for planned, i.e. not yet completed, part of a normal
task."
  :group 'etaskfaces)

(if (boundp 'facemenu-unlisted-faces)
    (add-to-list 'facemenu-unlisted-faces 'etask-face-criticalplanned))
(defface etask-face-criticalplanned
  '((((class color))
     (:foreground "gray60"))
    (t ()))
  "Task bar face for planned part of a task on the critical path."
  :group 'etaskfaces)

(if (boundp 'facemenu-unlisted-faces)
    (add-to-list 'facemenu-unlisted-faces 'etask-face-highriskplanned))
(defface etask-face-highriskplanned
  '((((class color))
     (:foreground "gray60"))
    (t ()))
  "Task bar face for planned part of a high risk task."
  :group 'etaskfaces)

(if (boundp 'facemenu-unlisted-faces)
    (add-to-list 'facemenu-unlisted-faces 'etask-face-completed))
(defface etask-face-completed
  '((((class color))
     (:family "Courier" :foreground "darkgreen" :weight bold))
    (t ()))
  "Task bar face for the completed part of a normal task."
  :group 'etaskfaces)

(if (boundp 'facemenu-unlisted-faces)
    (add-to-list 'facemenu-unlisted-faces 'etask-face-criticalcompleted))
(defface etask-face-criticalcompleted
  '((((class color))
     (:foreground "darkred" :weight bold))
    (t ()))
  "Task bar face for the completed part of a task on the critical
path."
  :group 'etaskfaces)

(if (boundp 'facemenu-unlisted-faces)
    (add-to-list 'facemenu-unlisted-faces 'etask-face-highriskcompleted))
(defface etask-face-highriskcompleted
  '((((class color))
     (:foreground "darkblue" :weight bold))
    (t ()))
  "Task bar face for completed part of a high risk task."
  :group 'etaskfaces)

(if (boundp 'facemenu-unlisted-faces)
    (add-to-list 'facemenu-unlisted-faces 'etask-face-criticaltask))
(defface etask-face-criticaltask
  '((((class color))
     (:foreground "darkred"))
    (t ()))
  "Face for critical task names displayed next to their task bar."
  :group 'etaskfaces)

(if (boundp 'facemenu-unlisted-faces)
    (add-to-list 'facemenu-unlisted-faces
                 'etask-face-criticaltaskname-behind-schedule))
(defface etask-face-criticaltaskname-behind-schedule
  '((((class color))
     (:foreground "darkred" :weight bold))
    (t ()))
  "Face for those critical task names displayed next to their task bar
that are behind schedule.

Keep in mind that this face should, for consistency, add one or more
additional face attributes to `etask-face-criticaltask'."
  :group 'etaskfaces)

(if (boundp 'facemenu-unlisted-faces)
    (add-to-list 'facemenu-unlisted-faces
                 'etask-face-criticaltaskname-completed))
(defface etask-face-criticaltaskname-completed
  '((((class color))
     (:foreground "darkred" :strike-through t))
    (t ()))
  "Face for those critical task names displayed next to their task bar
that are already completed.

Keep in mind that this face should, for consistency, add one or more
additional face attributes to `etask-face-criticaltask'."
  :group 'etaskfaces)

(if (boundp 'facemenu-unlisted-faces)
    (add-to-list 'facemenu-unlisted-faces 'etask-face-highrisktask))
(defface etask-face-highrisktask
  '((((class color))
     (:foreground "darkblue"))
    (t ()))
  "Face for high risk task names displayed next to their task bar."
  :group 'etaskfaces)

(if (boundp 'facemenu-unlisted-faces)
    (add-to-list 'facemenu-unlisted-faces
                 'etask-face-highrisktaskname-behind-schedule))
(defface etask-face-highrisktaskname-behind-schedule
  '((((class color))
     (:foreground "darkblue" :weight bold))
    (t ()))
  "Face for those high risk task names displayed next to their task
bar that are behind schedule.

Keep in mind that this face should, for consistency, add one or more
additional face attributes to `etask-face-highrisktask'."
  :group 'etaskfaces)

(if (boundp 'facemenu-unlisted-faces)
    (add-to-list 'facemenu-unlisted-faces
                 'etask-face-highrisktaskname-completed))
(defface etask-face-highrisktaskname-completed
  '((((class color))
     (:foreground "darkblue" :strike-through t))
    (t ()))
  "Face for those high risk task names displayed next to their task
bar that arealready completed.

Keep in mind that this face should, for consistency, add one or more
additional face attributes to `etask-face-highrisktask'."
  :group 'etaskfaces)

(if (boundp 'facemenu-unlisted-faces)
    (add-to-list 'facemenu-unlisted-faces 'etask-face-normaltask))
(defface etask-face-normaltask
  '((((class color))
     (:foreground "black"))
    (t ()))
  "Face for normal task names displayed next to their task bar."
  :group 'etaskfaces)

(if (boundp 'facemenu-unlisted-faces)
    (add-to-list 'facemenu-unlisted-faces
                 'etask-face-normaltaskname-behind-schedule))
(defface etask-face-normaltaskname-behind-schedule
  '((((class color))
     (:foreground "black" :weight bold))
    (t ()))
  "Face for those normal task names displayed next to their task bar
that are behind schedule.

Keep in mind that this face should, for consistency, add one or more
additional face attributes to `etask-face-normaltask'."
  :group 'etaskfaces)

(if (boundp 'facemenu-unlisted-faces)
    (add-to-list 'facemenu-unlisted-faces
                 'etask-face-normaltaskname-completed))
(defface etask-face-normaltaskname-completed
  '((((class color))
     (:foreground "black" :strike-through t))
    (t ()))
  "Face for those normal task names displayed next to their task bar
that are already completed.

Keep in mind that this face should, for consistency, add one or more
additional face attributes to `etask-face-normaltask'."
  :group 'etaskfaces)

(if (boundp 'facemenu-unlisted-faces)
    (add-to-list 'facemenu-unlisted-faces
                 'etask-face-status-behindschedule))
(defface etask-face-status-behindschedule
  '((((class color))
     (:foreground "red" :weight bold))
    (t ()))
  "Face for status message indicating that a task is behind schedule."
  :group 'etaskfaces)

(if (boundp 'facemenu-unlisted-faces)
    (add-to-list 'facemenu-unlisted-faces 'etask-face-status-ok))
(defface etask-face-status-ok
  '((((class color))
     (:foreground "black" :weight bold))
    (t ()))
  "Face for the status message indicating that a task is on time."
  :group 'etaskfaces)

(if (boundp 'facemenu-unlisted-faces)
    (add-to-list 'facemenu-unlisted-faces 'etask-face-statusheader))
(defface etask-face-statusheader-onscreen
  '((((class color))
     (:background "darkseagreen2" :foreground "black" :weight bold))
    (t ()))
  "Face for the task status header below the bar chart.

The task status header consists of the prefix `etask-statusheader' and
the task's name."
  :group 'etaskfaces)

(if (boundp 'facemenu-unlisted-faces)
    (add-to-list 'facemenu-unlisted-faces 'etask-face-task-on-report))
(defface etask-face-task-on-report
  '((((class color))
     (:background "darkseagreen2" :foreground "black" :weight bold))
    (t ()))
  "Face for task name and its prefix in a report document."
  :group 'etaskfaces)

(defconst etask-flag-delete
  (etask-propertize "D" 'face 'etask-face-flag-delete)
  "The 'delete' flag.")
(defconst etask-flag-archive
  (etask-propertize "A" 'face 'etask-face-flag-archive)
  "The 'archive' flag.")
(defconst etask-flag-post-elt
  (etask-propertize ">>" 'face 'etask-face-flag-post-elt)
  "The 'postpone whole element' flag.")
(defconst etask-flag-post-beg
  (etask-propertize ">" 'face 'etask-face-flag-post-beg)
  "The 'postpone begin date' flag.")
(defconst etask-flag-post-dd
  (etask-propertize " >" 'face 'etask-face-flag-post-dd)
  "The 'postpone due date' flag.")
(defconst etask-flag-fwd-elt
  (etask-propertize "<<" 'face 'etask-face-flag-fwd-elt)
  "The 'forward whole element' flag.")
(defconst etask-flag-fwd-beg
  (etask-propertize "<" 'face 'etask-face-flag-fwd-beg)
  "The 'forward begin date' flag.")
(defconst etask-flag-fwd-dd
  (etask-propertize " <" 'face 'etask-face-flag-fwd-dd)
  "The 'forward due date' flag.")
(defconst etask-flag-link
  (etask-propertize "L" 'face 'etask-face-flag-link)
  "The 'link elements' flag.")
(defconst etask-flag-mark
  (etask-propertize "*" 'face 'etask-face-flag-mark)
  "The 'mark' flag.")
(defconst etask-flag-empty ""  "The 'empty' flag.")

(defvar etask-current-flags nil "List of current flags")

(defconst etask-today-symbol
  (etask-propertize "^" 'face 'etask-face-today)
  "The symbol used to point at the current date.")

(defconst etask-day-marker
  (etask-propertize "'" 'face 'etask-face-day-marker)
  "The symbol used to point at the end of a day.")

(defcustom etask-planned-marker
  (etask-propertize "-" 'face 'etask-face-planned)
  "Task bar symbol for planned (i.e. still open) parts of a normal
task."
  :group 'etaskfaces)

(defcustom etask-criticalplanned-marker
  (etask-propertize "-" 'face 'etask-face-criticalplanned)
  "Task bar symbol for planned parts of a task on the critical path."
  :group 'etaskfaces)

(defcustom etask-highriskplanned-marker
  (etask-propertize "-" 'face 'etask-face-highriskplanned)
  "Task bar symbol for planned parts of a high risk task."
  :group 'etaskfaces)

(defcustom etask-completed-marker
  (etask-propertize "=" 'face 'etask-face-completed)
  "Task bar symbol for completed parts of a normal task."
  :group 'etaskfaces)

(defcustom etask-criticalcompleted-marker
  (etask-propertize "*" 'face 'etask-face-criticalcompleted)
  "Task bar symbol for completed parts of a task on the critical
path."
  :group 'etaskfaces)

(defcustom etask-highriskcompleted-marker
  (etask-propertize "*" 'face 'etask-face-highriskcompleted)
  "Task bar symbol for completed parts of a high risk task."
  :group 'etaskfaces)

(defcustom etask-working-dir ""
  "Full path name of etask's working directory.

Example: /home/rene/etaskmode/"
  :type 'string
  :group 'etask)

(defconst etask-tex-filename-prefix "gantt"
  "Prefix of LaTeX Gantt chart file name.

Note: Use `etask-working-dir' to specify the working directory.

See `etask-reports-filename-prefix' for details about the file naming
approach.")

(defconst etask-reports-filename-prefix "reports"
  "Prefix of file that holds your status reports.

Note: Use `etask-working-dir' to specify the working directory.

When you request a task status report the first time of that day the
system creates a new file named
`etask-reports-filename-prefix'.day.mon.year'.  This file is used for
all reports requested on that day.")

(defcustom etask-write-logfile-p nil
  "If non-nil, EtaskMode logs debug information in file \"etasklog\"."
  :type 'boolean
  :group 'etask)

(defcustom etask-allow-popup-frames-p nil
  "If non-nil, reports and LaTeX Gantt charts pop up in new frames."
  :type 'boolean
  :group 'etask)

(defconst etask-logfile "etasklog"
  "File name for log output.

Note: Use `etask-working-dir' to specify the working directory.  See
also `etask-write-logfile-p'.")

(defconst etask-controlfile "etaskctrl"
  "File name for the main control file.

Use `etask-working-dir' to specify the working directory.")

(defconst etask-ctrl-chartbegindate "chartbegindate"
  "The draft Gantt charts begin date id in the control file.")

(defconst etask-ctrl-chartenddate "chartenddate"
  "The draft Gantt charts end date id in the control file.")

(defconst etask-ctrl-todoview-begintime "todobegintime"
  "The draft Gantt charts begin time id in the todo view.")

(defconst etask-ctrl-todoview-endtime "todoendtime"
  "The draft Gantt charts end time id in the todo view.")

(defconst etask-ctrl-currcategory "currcategory"
  "The current category id.")

(defconst etask-ctrl-curritem "curritem"
  "The current item id.")

(defconst etask-ctrl-onlinehelp "onlinehelp"
  "The online help flag id.")

(defconst etask-ctrl-zoomfactor "zoomfactor"
  "The draft Gantt charts zoom factor id in the control file.")

(defconst etask-ctrl-zoommodus "zoommodus"
  "The draft Gantt charts zoom modus id in the control file.")

(defconst etask-current-category-id "***"
  "The id for the current item in a category.  Example: ***Cat***")

(defconst etask-tasks-filename "projects"
  "Prefix of files in `etask-working-dir' that store task related
data.")

(defconst etask-todos-filename "todos"
  "Prefix of files in `etask-working-dir' that store todo related
data.")

(defconst etask-events-filename "events"
  "Prefix of files in `etask-working-dir' that store meeting related
data.")

(defconst etask-tasklist-filename "tasklist"
  "File name in `etask-working-dir' for task name suggestions.")

(defconst etask-lines-taskstatus 17
  "Number of lines needed to display task status information.")

(defconst etask-lines-help 17
  "Number of lines needed to display EtaskMode online help.")

(defconst etask-lines-except-tasklines
  (+ etask-lines-taskstatus etask-lines-help)
  "Number of lines needed to display task status information and
online help.")

(defvar etask-mode-map nil "Local keymap for etask mode.")

(defvar etask-helprequest-p t "*Initial online help.")

(defvar etask-state-var nil "EtaskMode state communication.")

(defvar etask-stateid-currcat "currcat"
  "Current EtaskMode category.")

(defvar etask-stateid-curritem "currcatitem"
  "Current EtaskMode category item.")

(defvar etask-stateid-archive "archive"
  "Flag for active archive view.")

(defvar etask-stateid-todostate "todoinit"
  "Initial todo state.")

(defvar etask-stateid-zoomfactor "zoomfactor"
  "Zoom factor.")

(defvar etask-stateid-zoommodus "zoommodus"
  "Zoom modus.")

(defvar etask-stateid-level "level"
  "Display items up to specified level.")

(defvar etask-stateid-maxbarlen "maxbarlen"
  "Maximum bar length.")

(defvar etask-stateid-maxtasklen "maxtasklen"
  "Maximum task name length in draft Gantt chart.")

(defvar etask-stateid-chartstart "chartstart"
  "The chart's start date.")

(defvar etask-stateid-chartend "chartend"
  "The chart's end date.")

(defvar etask-stateid-chartstarttime "chartstarttime"
  "The chart's start time.")

(defvar etask-stateid-chartendtime "chartendtime"
  "The chart's end time.")

(defvar etask-stateid-businessdays "bizdaysop"
  "If non-nil use business days, otherwise calendar days.")

(defvar etask-stateid-last-flag "lastflag"
  "Remember last flag.")

(defvar etask-element-killring nil "Kill ring for killed elements.")


(if etask-mode-map
    nil
  (setq etask-mode-map (make-keymap))
  (suppress-keymap etask-mode-map t)

  (define-key etask-mode-map "i" 'etask-cat-insert-elements)
  (define-key etask-mode-map "r" 'etask-set-last-flag-again)
  (define-key etask-mode-map "X" 'etask-cat-show-elements)
  (define-key etask-mode-map "\C-hm" 'etask-cat-show-elements-with-help)
  (define-key etask-mode-map "E" 'etask-cat-edit-element)

  (define-key etask-mode-map "d" 'etask-set-flag-delete)
  (define-key etask-mode-map "\C-k" 'etask-kill-element)
  (define-key etask-mode-map "\C-y" 'etask-yank-element)
  (define-key etask-mode-map "a" 'etask-set-flag-archive)

  (define-key etask-mode-map "\C-cd" 'etask-toggle-dayunit)
  (define-key etask-mode-map "\C-c\C-d" 'etask-print-dayunit)

  (define-key etask-mode-map "\C-cpt" 'etask-set-flag-postpone-element)
  (define-key etask-mode-map "\C-cpb" 'etask-set-flag-postpone-begin)
  (define-key etask-mode-map "\C-cpd" 'etask-set-flag-postpone-duedate)
  (define-key etask-mode-map "\C-cft" 'etask-set-flag-fwd-element)
  (define-key etask-mode-map "\C-cfb" 'etask-set-flag-fwd-begin)
  (define-key etask-mode-map "\C-cfd" 'etask-set-flag-fwd-duedate)

  (define-key etask-mode-map "l" 'etask-set-flag-link-elements)

  (define-key etask-mode-map "m" 'etask-set-mark)
  (define-key etask-mode-map " " 'etask-set-mark)
  (define-key etask-mode-map "u" 'etask-unset-flag-and-mark)

  (define-key etask-mode-map "x" 'etask-execute-flagcmd)
  (define-key etask-mode-map "I" 'etask-cat-elementnotes)

  (define-key etask-mode-map "\C-cs" 'etask-cat-switch-item)
  (define-key etask-mode-map "\C-ca" 'etask-cat-admin-categories)
  (define-key etask-mode-map "A" 'etask-cat-toggle-archive)

  (define-key etask-mode-map "B" 'etask-set-new-chartbegindate)
  (define-key etask-mode-map "z" 'etask-zoom)
  (define-key etask-mode-map "\C-f" 'etask-scroll-chart-left-days)
  (define-key etask-mode-map "\C-b" 'etask-scroll-chart-right-days)
  (define-key etask-mode-map "\M-f" 'etask-scroll-chart-left-weeks)
  (define-key etask-mode-map "\M-b" 'etask-scroll-chart-right-weeks)
  (define-key etask-mode-map "\C-\M-f" 'etask-scroll-chart-left-months)
  (define-key etask-mode-map "\C-\M-b" 'etask-scroll-chart-right-months)
  (define-key etask-mode-map ">" 'etask-scroll-chart-end)
  (define-key etask-mode-map "<" 'etask-scroll-chart-begin)
  (define-key etask-mode-map "j" 'etask-todo-previous-hour)
  (define-key etask-mode-map "k" 'etask-todo-next-hour)
  (define-key etask-mode-map "J" 'etask-todo-first-hour)
  (define-key etask-mode-map "K" 'etask-todo-last-hour)
  (define-key etask-mode-map "." 'etask-scroll-to-current)
  (define-key etask-mode-map "," 'etask-scroll-to-taskdate)

  (define-key etask-mode-map "\C-\M-u" 'etask-move-element-up)
  (define-key etask-mode-map "\C-\M-d" 'etask-move-element-down)

  (define-key etask-mode-map "e" 'etask-cat-set-planned-effort)
  (define-key etask-mode-map "f" 'etask-set-fulltime-equivalence)
  (define-key etask-mode-map "T" 'etask-set-tracking-algorithm)
  (define-key etask-mode-map "b" 'etask-set-plannedbegin)


  (define-key etask-mode-map "%" 'etask-set-expended-effort-percent)
  (define-key etask-mode-map "=" 'etask-set-expended-effort-timeunits)
  (define-key etask-mode-map "+" 'etask-add-expended-effort-timeunits)
  (define-key etask-mode-map "-" 'etask-subtract-expended-effort-timeunits)

  (define-key etask-mode-map "" 'etask-forward-begindate)
  (define-key etask-mode-map "" 'etask-postpone-begindate)
  (define-key etask-mode-map "" 'etask-forward-duedate)
  (define-key etask-mode-map "" 'etask-postpone-duedate)
  (define-key etask-mode-map "" 'etask-forward-whole-task)
  (define-key etask-mode-map "" 'etask-postpone-whole-task)

  (define-key etask-mode-map "M" 'etask-toggle-all-marks)

  (define-key etask-mode-map "c" 'etask-toggle-critical-normal)
  (define-key etask-mode-map "h" 'etask-toggle-highrisk-normal)

  (define-key etask-mode-map "R" 'etask-generate-report)
  (define-key etask-mode-map "C" 'etask-generate-latex-gantt-chart)

  (define-key etask-mode-map "S" 'etask-cat-sort-elements)
  (define-key etask-mode-map "\C-xu" 'etask-undo)

  (define-key etask-mode-map "p" 'etask-previous-line)
  (define-key etask-mode-map "n" 'etask-next-line)
  (define-key etask-mode-map "P" 'etask-previous-line-samelevel)
  (define-key etask-mode-map "N" 'etask-next-line-samelevel)
  (define-key etask-mode-map "t" 'etask-goto-toplevel)
  (define-key etask-mode-map "\C-p" 'etask-previous-line)
  (define-key etask-mode-map "\C-n" 'etask-next-line)
  (define-key etask-mode-map "\M-p" 'etask-goto-first-delimiter)
  (define-key etask-mode-map "\M-n" 'etask-goto-last-delimiter)
  (define-key etask-mode-map "g" 'etask-goto-elementline)
  (define-key etask-mode-map "1" 'etask-goto-elementline-1)
  (define-key etask-mode-map "2" 'etask-goto-elementline-2)
  (define-key etask-mode-map "3" 'etask-goto-elementline-3)
  (define-key etask-mode-map "4" 'etask-goto-elementline-4)
  (define-key etask-mode-map "5" 'etask-goto-elementline-5)
  (define-key etask-mode-map "6" 'etask-goto-elementline-6)
  (define-key etask-mode-map "7" 'etask-goto-elementline-7)
  (define-key etask-mode-map "8" 'etask-goto-elementline-8)
  (define-key etask-mode-map "9" 'etask-goto-elementline-9)

  (define-key etask-mode-map "L" 'etask-set-language)
  (define-key etask-mode-map "w" 'etask-warranty)
  (define-key etask-mode-map "v" 'etask-version)
  (define-key etask-mode-map "?" 'etask-help)
  (define-key etask-mode-map "q" 'etask-quit)
  (define-key etask-mode-map "\C-c\C-b" 'etask-report-failure)
  (define-key etask-mode-map "\C-c\C-f" 'etask-submit-feedback)
  (define-key etask-mode-map "^" 'etask-adjust-frame-height)

  (define-key etask-mode-map [prior] 'etask-previous-line)
  (define-key etask-mode-map [next] 'etask-next-line)
  (define-key etask-mode-map [left] 'etask-disabled-key)
  (define-key etask-mode-map [up] 'etask-previous-line)
  (define-key etask-mode-map [right] 'etask-disabled-key)
  (define-key etask-mode-map [down] 'etask-next-line)
  (define-key etask-mode-map "\M-j" 'etask-disabled-key)
  (define-key etask-mode-map "" 'etask-disabled-key)
  (define-key etask-mode-map "\M-a" 'etask-disabled-key)
  (define-key etask-mode-map "\M-e" 'etask-disabled-key)
  (define-key etask-mode-map "\C-a" 'etask-disabled-key)
  (define-key etask-mode-map "\C-e" 'etask-disabled-key)
  (define-key etask-mode-map "\C-\M-a" 'etask-disabled-key)
  (define-key etask-mode-map "\C-\M-e" 'etask-disabled-key))

  ;; ''Rescue'' keys if frame is smaller than needed.
  ;;   (define-key etask-mode-map "\C-v" 'etask-disabled-key)
  ;;   (define-key etask-mode-map "\M-v" 'etask-disabled-key)


(put 'etask-mode 'mode-class 'special)

(defun etask-disabled-key()
  "Tell user that key is disabled."
  (error "%s" (etask-lang-msg 1004 etask-language)))

(defun etask-mode()
  "Major mode for managing your projects.  Online Help: `C-h m'"
  (kill-all-local-variables)
  (etask-lang-ix etask-language 'init)
  (setq etask-state-var nil)
  (etask-cat-create-cat-alist)
  (etask-proj-create-tasks-alist)
  (setq major-mode 'etask-mode)
  (setq mode-name "etask")
  (use-local-map etask-mode-map)
  (setq buffer-read-only t)
  (setq etask-statusheader
        (concat ">> " (etask-lang-msg 775 etask-language) ": "))

  (when (and (featurep 'highlight-current-line)
             (not highlight-current-line-minor-mode))
    (highlight-current-line-minor-mode))

  (etask-state-set
   etask-stateid-currcat
   (etask-get-control-item etask-ctrl-currcategory 'main))
  (etask-state-set
   etask-stateid-curritem
   (etask-get-control-item etask-ctrl-curritem 'main))
  (etask-state-set etask-stateid-todostate "todoinit")
  (etask-set-control-item 
   etask-ctrl-onlinehelp etask-helprequest-p 'main)
  (run-hooks 'etask-mode-hook))


;;; Interactive Functions

(defun etask-version()
  "Print EtaskMode's version including copyright and warranty hint."
  (interactive)
  (princ (etask-lang-msg 900 etask-language))
  (princ etask-release)
  (princ (etask-lang-msg 901 etask-language)))

(defun etask-warranty()
  "Print (no) warranty to buffer '*No Warranty*'."
  (interactive)
  (with-output-to-temp-buffer "*No Warranty*"
    (princ (etask-lang-msg 905 etask-language))))

(defun etask-help()
  "Create help buffer with a brief description of EtaskMode."
  (interactive)
  (with-output-to-temp-buffer "*Help*"
    (princ (etask-lang-msg 910 etask-language))
    (princ (etask-lang-msg 911 etask-language))
    (princ (etask-lang-msg 912 etask-language))
    (princ (etask-lang-msg 913 etask-language))
    (princ (etask-lang-msg 914 etask-language))
    (princ (etask-lang-msg 915 etask-language))
    (princ
     (format "\n\n\nEtaskMode: \n\n%s\n" 
             (substitute-command-keys "\\{etask-mode-map}")))
    (save-excursion
      (set-buffer standard-output)
      (help-mode))
    (print-help-return-message)))

(defun etask()
  "Manage your projects within Emacs."
  (interactive)
  (let ((input))
    (etask-log 
     (concat "EtaskMode started: " (current-time-string) "\n") 'init)
    (set-buffer (get-buffer-create etask-buffer))
    (setq buffer-read-only nil)
    (erase-buffer)
    (setq buffer-read-only t)
    (etask-mode)
    (buffer-disable-undo etask-buffer)
    (let* ((pop-up-windows t)
           (currcatid (car (etask-cat-get-current-item)))
           (curritem (car (cdr (etask-cat-get-current-item))))
           (itemp (etask-cat-is-item-p currcatid curritem))
           (activecats (etask-cat-get-active-categories)))
      (etask-all-states-update)
      (pop-to-buffer etask-buffer)
      (cond ((and itemp
                  (not (etask-cat-is-current-elementlist-empty-p)))
             (etask-cat-show-elements))
            (activecats
             (etask-cat-switch-item
              (etask-lang-msg 375 etask-language)))
            (t
             (etask-cat-admin-categories
              (etask-lang-msg 376 etask-language)))))))

(defun etask-insert-all-files()
  "Insert all element related files in a single buffer."
  (let* ((catlist (etask-cat-get-active-categories))
         (file
          (concat
           etask-working-dir
           "failure-report"
           "."
           (number-to-string 
            (extract-calendar-day (calendar-current-date)))
           "."
           (calendar-month-name 
            (extract-calendar-month (calendar-current-date)) 3)
           "."
           (number-to-string 
            (extract-calendar-year (calendar-current-date)))))
         (buf (find-file-noselect file))
         (itemlist)
         (catid)
         (item)
         (file))
    (save-excursion
      (set-buffer buf)
      (erase-buffer)
      (goto-char (point-max))
      (forward-line)
      (insert
       (format "\n\n==> Main Ctrl:\nFile \"%s\"\n\n"
               (etask-get-main-ctrl-filename)))
      (goto-char (point-max))
      (forward-line)
      (setq file (etask-get-main-ctrl-filename))
      (when (and (stringp file) (> (length file) 0)
                 (file-readable-p file))
        (insert-file-contents file))
      (while catlist
        (setq catid (car catlist))
        (goto-char (point-max))
        (forward-line)
        (insert (format "\n\n\n==> Category '%s':\nFile \"%s\"\n\n"
                        (etask-cat-get-catname catid)
                        (concat etask-working-dir
                                (etask-cat-get-catfilename catid))))
        (goto-char (point-max))
        (forward-line)
        (setq file (concat etask-working-dir 
                 (etask-cat-get-catfilename catid)))
        (when (and (stringp file) (> (length file) 0)
                   (file-readable-p file))
          (insert-file-contents file))
        (setq itemlist (etask-cat-get-itemlist catid))
        (while itemlist
          (setq item (etask-cat-trim-current-id (car itemlist)))
          (goto-char (point-max))
          (forward-line)
          (insert (format "\n\n\n==> Item '%s':\nFile \"%s\"\n\n" 
                          item
                          (concat 
                           etask-working-dir
                           (etask-cat-get-itemfilename catid item))))
          (goto-char (point-max))
          (forward-line)
          (setq file (concat etask-working-dir
                             (etask-cat-get-itemfilename catid item)))
          (when (and (stringp file) (> (length file) 0)
                     (file-readable-p file))
            (insert-file-contents file))
          (goto-char (point-max))
          (forward-line)
          (insert (format "\n\n==> '%s' Ctrl:\nFile \"%s\"\n\n" 
                          item
                          (etask-get-item-ctrl-filename catid item)))
          (goto-char (point-max))
          (forward-line)
          (setq file (etask-get-item-ctrl-filename catid item))
          (when (and (stringp file) (> (length file) 0)
                     (file-readable-p file))
            (insert-file-contents file))
          (setq itemlist (cdr itemlist)))
        (setq catlist (cdr catlist)))
      (goto-char (point-max))
      (forward-line)
      (insert "\n\n")
      (forward-line)
      (setq file (concat etask-working-dir etask-logfile))
      (when (and etask-write-logfile-p
                 (file-readable-p file))
        (insert-file-contents file))
      (save-buffer))
    (goto-char (point-max))
    (insert-buffer-substring buf)))

(defun etask-submit-feedback()
  "Submit feedback on EtaskMode."
  (interactive)
  (require 'reporter)
  (delete-other-windows)
  (reporter-submit-bug-report
   "rene.mas@chello.at"
   nil
   nil
   nil
   nil
   (etask-lang-msg 994 etask-language))
  (goto-char (point-min))
  (mail-position-on-field "Subject")
  (insert "EtaskMode: "))

(defun etask-report-failure()
  "Submit EtaskMode failures."
  (interactive)
  (require 'reporter)
  (delete-other-windows)
  (reporter-submit-bug-report
   "rene.mas@chello.at"
   (format "EtaskMode %s" etask-release)
   (list
    'emacs-version 
    'system-type
    'window-system 
    'window-system-version
    'features)
   nil
   nil
   (etask-lang-msg 990 etask-language))

;; insert the backtrace buffer content if present
  (let ((backtrace (get-buffer-create "*Backtrace*")))
    (when backtrace
      (goto-char (point-max))
      (insert "\n\n")
      (insert-buffer-substring backtrace)))
  
  (etask-insert-all-files)

  (goto-char (point-min))
  (mail-position-on-field "Subject")
  (insert "EtaskMode: "))

(defun etask-adjust-frame-height-gnuemacs(&optional lines)
  "Increase or decrease current frame height."
  (interactive "P")
  ;; -2: minibuffer + modeline
  (when (= (- (frame-height) 2) (etask-get-window-currheight))
    (let* ((default 
             (if (etask-window-too-small-p)
                 (- (etask-get-window-minheight)
                    (etask-get-window-currheight))
               (- (etask-get-window-currheight)
                  (etask-get-window-minheight))))
           (amount 
            (if (etask-window-too-small-p)
                (+ (frame-height) 
                   (if (natnump lines) (min lines default) default))
              (- (frame-height)
                 (if (natnump lines)
                     (min lines (etask-get-window-currheight))
                   default)))))
      (set-frame-height 
       (window-frame (get-buffer-window (current-buffer)))
       amount))))

(defun etask-set-language()
  "Set the language for EtaskMode."
  (interactive)
  (let ((prompt (etask-get-list-prompt etasklang t)))
    (setq etask-language
          (nth (1- (string-to-number
                    (etask-read
                     (concat prompt "  ? ")
                     (lambda (x) 
                       (and (>= (string-to-number x) 1) 
                            (<= (string-to-number x) 
                                (length etasklang)))))))
               etasklang))
    (etask-cat-create-cat-alist)
    (etask-set-calendar-language etask-language)
    (etask-cat-show-elements)))

(defun etask-scroll-to-current()
  "Scroll to the current day or hour."
  (interactive)
  (if (etask-todo-organizer-view-p)
      (etask-todo-current-hour)
    (etask-scroll-chart-to-day (calendar-current-date))))

(defun etask-scroll-to-taskdate(&optional endp)
  "Scroll to begin or, if ENDP is non-nil, to end of current element."
  (interactive "P")
  (let* ((catid (car (etask-cat-get-current-item)))
         (element (etask-cat-get-current-element))
         (date (when element
                 (if endp
                     (etask-db-get element etask-db-attr-taskend)
                   (etask-db-get element etask-db-attr-taskbegin)))))
    (when (etask-cat-is-valid-catid-p catid)
      (when (and (= catid etask-category-todoid)
                 (not (etask-todo-organizer-view-p)))
        (let* ((todowidth (length (etask-todo-get-todostr element)))
               (daywidth (etask-get-daylen))
               (corr (1- (ceiling (/ (float todowidth) daywidth)))))
          (when (and (natnump corr) (> corr 0))
            (setq date (etask-add-days-to-date date (- 0 corr))))))
      (when (etask-calendardate-is-legal-p date)
        (etask-scroll-chart-to-day date)))))

(defun etask-scroll-chart-to-day(date)
  "Scroll near to current date."
  (let* ((currbegin (etask-state-get etask-stateid-chartstart))
         (dist (etask-days-between date currbegin))
         (leftp (calendar-date-compare (list currbegin) (list date))))
    (if leftp
        (etask-scroll-chart-left-days dist)
      (etask-scroll-chart-right-days dist))))

(defun etask-scroll-chart-left-days(&optional days)
  "Scroll one day or, if DAYS is non-nil, DAYS days to the left."
  (interactive "p")
  (etask-scroll-chart-left days))

(defun etask-scroll-chart-right-days(&optional days)
  "Scroll one day or, if DAYS is non-nil, DAYS days to the right."
  (interactive "p")
  (etask-scroll-chart-right days))

(defun etask-scroll-chart-left-weeks(&optional weeks)
  "Scroll one week or, if WEEKS is non-nil, WEEKS weeks to the left."
  (interactive "p")
  (etask-scroll-chart-left (* weeks 7)))

(defun etask-scroll-chart-right-weeks(&optional weeks)
  "Scroll one week or, if WEEKS is non-nil, WEEKS weeks to the right."
  (interactive "p")
  (etask-scroll-chart-right (* weeks 7)))

(defun etask-scroll-chart-left-months(&optional months)
  "Scroll one month or, if MONTH is non-nil, MONTHS months to the
left."
  (interactive "p")
  (let* ((currentbegin (etask-state-get etask-stateid-chartstart))
         (day (extract-calendar-day currentbegin))
         (mon (extract-calendar-month currentbegin))
         (year (extract-calendar-year currentbegin)))
    (increment-calendar-month mon year months)
    (etask-scroll-chart-left
     (etask-days-between currentbegin (list mon day year)))))

(defun etask-scroll-chart-right-months(&optional months)
  "Scroll one month or, if MONTH is non-nil, MONTHS months to the
right."
  (interactive "p")
  (let* ((currentbegin (etask-get-control-item
                        etask-ctrl-chartbegindate))
         (day (extract-calendar-day currentbegin))
         (mon (extract-calendar-month currentbegin))
         (year (extract-calendar-year currentbegin)))
    (increment-calendar-month mon year (- 0 months))
    (etask-scroll-chart-right
     (etask-days-between currentbegin (list mon day year)))))

(defun etask-scroll-chart-beginend(ctrl)
  "Scroll to begin if CTRL>1 or to end if CTRL<1."
  (let* ((currentbegin (etask-get-control-item etask-stateid-chartstart))
         (day (extract-calendar-day currentbegin))
         (mon (extract-calendar-month currentbegin))
         (year (extract-calendar-year currentbegin)))
    (when (integerp ctrl)
      (cond ((> ctrl 0)
             (etask-scroll-chart-left
              (etask-gantt-validate-scrolling ctrl 'max)))
            ((< ctrl 0)
             (etask-scroll-chart-right
              (etask-gantt-validate-scrolling ctrl 'max)))
            (t
             ())))))

(defun etask-scroll-chart-end()
  "Scroll to the last category item."
  (interactive)
  (etask-scroll-chart-beginend 1))

(defun etask-scroll-chart-begin()
  "Scroll to the first category item."
  (interactive)
  (etask-scroll-chart-beginend -1))

(defun etask-gantt-validate-scrolling(days &optional max)
  "Return max number of days between zero and DAYS that can be
scrolled within the current category time frame.  If DAYS is a
negative integer scroll the chart to the right if possible.  If MAX is
non-nil, return max number of days not limited by DAYS.  Then DAYS
just controls the scrolling direction."
  (let ((catid (car (etask-cat-get-current-item))))
    (when (etask-cat-is-valid-catid-p catid)
      (let ((elements (etask-cat-get-current-elts)))
        (cond ((or (= catid etask-category-todoid)
                   (= catid etask-category-eventid))
               (cond ((and max (> days 0))
                      (let* ((chartend
                              (etask-state-get etask-stateid-chartend))
                             (projend 
                              (etask-calculate-lastend elements)))
                        (etask-days-between chartend projend)))
                     ((and max (< days 0))
                      (let* ((chartbegin 
                              (etask-state-get etask-stateid-chartstart))
                             (projbegin 
                              (etask-calculate-earliestbegin elements)))
                        (etask-days-between projbegin chartbegin)))
                     (t
                      (abs days))))
              (t
               (cond ((> days 0)
                      (let* ((chartend
                              (etask-state-get etask-stateid-chartend))
                             (projend 
                              (etask-calculate-lastend elements)))
                        (if (calendar-date-compare 
                             (list chartend) (list projend))
                            (if max
                                (etask-days-between chartend projend)
                              (min days 
                                   (etask-days-between chartend projend)))
                          0)))
                     ((< days 0)
                      (let* ((chartbegin 
                              (etask-state-get etask-stateid-chartstart))
                             (projbegin 
                              (etask-calculate-earliestbegin elements)))
                        (if (calendar-date-compare
                             (list projbegin) (list chartbegin))
                            (if max
                                (etask-days-between projbegin chartbegin)
                              (min (abs days)
                                   (etask-days-between projbegin chartbegin)))
                          0)))
                     (t
                      0))))))))
         
(defun etask-scroll-chart-left(sldays)
  "Scroll to the left by SLDAYS.  If SLDAYS is after the end of the
last category item scroll to this last item instead."
  (when (etask-interactive-preconditions)
    (let* ((catid (car (etask-cat-get-current-item)))
           (elements (etask-cat-get-current-elts))
           (projbegin (etask-calculate-earliestbegin elements))
           (projend (etask-calculate-lastend elements))
           (dist (etask-days-between 
                  (etask-state-get etask-stateid-chartstart)
                  projend)))
      (when (etask-cat-is-valid-catid-p catid)
        (if (or (integerp sldays) (> dist 0))
            (let* ((days (when (integerp sldays)
                           (etask-gantt-validate-scrolling sldays)))
                   newbegin newend)
              (when (and (natnump days) (> days 0))
                (setq newbegin
                      (etask-add-days-to-date
                       (etask-state-get etask-stateid-chartstart)
                       days))
                (setq newend
                      (etask-add-days-to-date
                       (etask-state-get etask-stateid-chartend)
                       days))
                (etask-state-set etask-stateid-chartstart newbegin)
                (etask-state-set etask-stateid-chartend newend)
                (etask-cat-show-elements)))
          (error "%s" (etask-lang-msg 1009 etask-language)))))))

(defun etask-scroll-chart-right(&optional sldays)
  "Scroll the draft Gantt chart to the right by SLDAYS.  If SLDAYS
earlier is before the first category item scroll to this first item
instead."
  (when (etask-interactive-preconditions)
    (let* ((catid (car (etask-cat-get-current-item)))
           (elements (etask-cat-get-current-elts))
           (projbegin (etask-calculate-earliestbegin elements))
           (projend (etask-calculate-lastend elements))
           (dist (etask-days-between 
                  (etask-state-get etask-stateid-chartstart)
                  projbegin)))
      (if (or (integerp sldays) (> dist 0))
          (let* ((days (when (integerp sldays)
                           (etask-gantt-validate-scrolling (- 0 sldays))))
                 newbegin newend)
            (when (and (natnump days) (> days 0))
              (setq newbegin
                    (etask-add-days-to-date
                     (etask-state-get etask-stateid-chartstart)
                     (- 0 days)))
              (setq newend
                    (etask-add-days-to-date
                     (etask-state-get etask-stateid-chartend)
                     (- 0 days)))
              (when (or (etask-project-date-p newbegin)
                        (and (etask-cat-is-valid-catid-p catid)
                             (/= catid etask-category-projectid)))
                (etask-state-set etask-stateid-chartstart newbegin)
                (etask-state-set etask-stateid-chartend newend)
                (etask-cat-show-elements))))
        (error "%s" (etask-lang-msg 1010 etask-language))))))

(defun etask-set-new-chartbegindate()
  "Set the begin date of the draft Gantt chart."
  (interactive)
  (when (etask-interactive-preconditions)
    (progn
      (etask-state-set
       etask-stateid-chartstart
       (etask-get-taskdate (etask-lang-msg 290 etask-language)))
      (etask-cat-show-elements))))

(defun etask-get-flag(line &optional properties)
  "Return flag in LINE.  If optional PROPERTIES is non-nil, then its
text properties are included."
  (let ((pos (point))
        flag)
    (etask-goto-line line)
    (forward-line 0)
    (setq flag (buffer-substring (point) (+ 2 (point))))
    (goto-char pos)
    (etask-string-trim flag)))

(defun etask-store-flags(&optional ignorelist)
  "Store current flags in list \(\(line flag\)\(line flag\)...\).  If
optional IGNORELIST, a list of positive integers, is non-nil then all line
numbers in IGNORELIST are omitted."
  (let ((l (etask-cat-get-elementnum))
        flaglist)
    (while (> l 0)
      (when (not (member l ignorelist))
        (setq flaglist 
              (cons (list l (etask-get-flag l 'properties)) flaglist)))
      (setq l (1- l)))
    flaglist))

(defun etask-restore-flags(flaglist)
  ""
  (let ((pos (point))
        flag flaglen line)
    (setq buffer-read-only nil)
    (while flaglist
      (setq flag (car (cdr (car flaglist)))
            flaglen (length flag)
            line (car (car flaglist)))
      (etask-goto-line line)
      (forward-line 0)
      (delete-char flaglen)
      (insert flag)
      (setq flaglist (cdr flaglist)))
    (setq buffer-read-only t)
    (goto-char pos)))

(defun etask-is-flag-unique(flag)
  "Return true if FLAG is the only flag in `etask-flag-archive'."
  (let ((li etask-current-flags)
        (otherflags 0))
    (while li
      (when (not (string= (car li) flag))
        (setq otherflags (1+ otherflags)))
      (setq li (cdr li)))
    (= otherflags 0)))
 
(defun etask-is-flag-allowed(flag)
  "Return true if FLAG can be set."
  (cond ((or (etask-is-flag-archive-p flag)
             (etask-is-flag-delete-p flag)
             (etask-is-flag-link-p flag))
         (etask-is-flag-unique flag))
        (t
         (and
          (not (member etask-flag-archive etask-current-flags))
          (not (member etask-flag-delete etask-current-flags))
          (not (member etask-flag-link etask-current-flags))))))

(defun etask-is-flag-delete-p(flag)
  "Return true if FLAG is the string `etask-flag-delete'."
  (string= flag etask-flag-delete))

(defun etask-is-flag-archive-p(flag)
  "Return true if FLAG is the string `etask-flag-archive'."
  (string= flag etask-flag-archive))

(defun etask-is-flag-post-elt-p(flag)
  "Return true if FLAG is the string `etask-flag-post-elt'."
  (string= flag etask-flag-post-elt))

(defun etask-is-flag-post-beg-p(flag)
  "Return true if FLAG is the string `etask-flag-post-beg'."
  (string= flag etask-flag-post-beg))

(defun etask-is-flag-post-dd-p(flag)
  "Return true if FLAG is the string `etask-flag-post-dd'."
  (string= flag etask-flag-post-dd))

(defun etask-is-flag-fwd-elt-p(flag)
  "Return true if FLAG is the string `etask-flag-fwd-elt'."
  (string= flag etask-flag-fwd-elt))

(defun etask-is-flag-fwd-beg-p(flag)
  "Return true if FLAG is the string `etask-flag-fwd-beg'."
  (string= flag etask-flag-fwd-beg))

(defun etask-is-flag-fwd-dd-p(flag)
  "Return true if FLAG is the string `etask-flag-fwd-dd'."
  (string= flag etask-flag-fwd-dd))

(defun etask-is-flag-link-p(flag)
  "Return true if FLAG is the string `etask-flag-link'."
  (string= flag etask-flag-link))

(defun etask-is-flag-mark-p(flag)
  "Return true if FLAG is the string `etask-flag-mark'."
  (string= flag etask-flag-mark))

(defun etask-is-flag-empty-p(flag)
  "Return true if FLAG is the string `etask-flag-empty'."
  (string= flag etask-flag-empty))
    
(defun etask-set-last-flag-again()
  ""
  (interactive)
  (let ((flag (etask-state-get etask-stateid-last-flag)))
    (when flag
      (cond ((etask-is-flag-delete-p flag)
             (etask-set-flag-delete))
            ((etask-is-flag-archive-p flag)
             (etask-set-flag-archive))
            ((etask-is-flag-post-elt-p flag)
             (etask-set-flag-postpone-element))
            ((etask-is-flag-post-beg-p flag)
             (etask-set-flag-postpone-begin))
            ((etask-is-flag-post-dd-p flag)
             (etask-set-flag-postpone-duedate))
            ((etask-is-flag-fwd-elt-p flag)
             (etask-set-flag-fwd-element))
            ((etask-is-flag-fwd-beg-p flag)
             (etask-set-flag-fwd-begin))
            ((etask-is-flag-fwd-dd-p flag)
             (etask-set-flag-fwd-duedate))
            ((etask-is-flag-link-p flag)
             (etask-set-flag-link-elements))
            ((etask-is-flag-mark-p flag)
             (etask-set-mark))
            (t
             ())))))
            
(defun etask-execute-flagcmd()
  "Execute commands according to current flags for all elements in
buffer."
  (interactive)
  (let ((currline (etask-current-line))
        (l 1)
        elnum tmp cmd flaglist)
    (setq elnum (etask-cat-get-elementnum)
          tmp elnum)
    (etask-goto-first-delimiter)
    (while (> tmp 0)
      (setq cmd (etask-get-flag l))
      (when (not (etask-is-flag-empty-p cmd))
        (cond ((etask-is-flag-delete-p cmd)
               (etask-execute-flagdelete elnum)
               ;; `etask-execute-flagdelete' deletes all elements at once
               (setq tmp 0))
              ((etask-is-flag-archive-p cmd)
               (etask-execute-flagarchive elnum)
               ;; `etask-execute-flagarchive' archives all elements at once
               (setq tmp 0))
              ((etask-is-flag-link-p cmd)
               (etask-cat-link-elements)
               ;; `etask-cat-link-elements' links all elements at once
               (setq tmp 0))
              ((etask-is-flag-mark-p cmd)
               )
              (t
               (setq flaglist (etask-store-flags (list l)))
               (cond ((etask-is-flag-post-elt-p cmd)
                      (etask-postpone-whole-task
                       (etask-state-get etask-stateid-businessdays)))
                     ((etask-is-flag-post-beg-p cmd)
                      (etask-postpone-begindate
                       (etask-state-get etask-stateid-businessdays)))
                     ((etask-is-flag-post-dd-p cmd)
                      (etask-postpone-duedate
                       (etask-state-get etask-stateid-businessdays)))
                     ((etask-is-flag-fwd-elt-p cmd)
                      (etask-forward-whole-task
                       (etask-state-get etask-stateid-businessdays)))
                     ((etask-is-flag-fwd-beg-p cmd)
                      (etask-forward-begindate
                       (etask-state-get etask-stateid-businessdays)))
                     ((etask-is-flag-fwd-dd-p cmd)
                      (etask-forward-duedate
                       (etask-state-get etask-stateid-businessdays))))
               (etask-forward-line 1)
               (when (> (1- tmp) 0) (etask-cat-show-elements))
               (etask-restore-flags flaglist))))
      (setq l (1+ l))
      (setq tmp (1- tmp)))
    (etask-goto-line currline)
    (etask-cat-show-elements)
    (setq etask-current-flags nil)))

(defun etask-execute-flagdelete(num)
  "Delete elements in lines 1 to NUM with its delete flag set and
return number of deleted elements."
  (let ((deleted 0))
    (goto-char (point-min))
    (etask-forward-line (1- num))
    (while (> num 0)
      (when (etask-is-flag-delete-p (etask-get-flag (etask-current-line)))
        (setq deleted (1+ deleted))
        (etask-cat-delete-element
         (etask-cat-get-current-elementindex)))
      (forward-line -1)
      (setq num (1- num)))
    deleted))

(defun etask-execute-flagarchive(num)
  "Archive all elements in lines 1 to NUM with archive flag set."
  (let ((archived 0))
    (goto-char (point-min))
    (forward-line (1- num))
    (while (> num 0)
      (when (etask-is-flag-archive-p (etask-get-flag (etask-current-line)))
        (setq archived (1+ archived))
        (etask-goto-toplevel)
        (etask-cat-archive-element
         (etask-cat-get-current-elementindex)))
      (forward-line -1)
      (setq num (1- num)))
    archived))

(defun etask-set-flag-delete()
  "Set 'delete' flag for current element and all subelements at once."
  (interactive)
  (when (and (etask-interactive-preconditions)
             (etask-is-flag-allowed etask-flag-delete))
    (let* ((subnum (etask-cat-get-subelement-num-after))
           (pos (point)))
      (etask-set-flag etask-flag-delete 'ctrl)
      (cons etask-flag-delete etask-current-flags)
      (etask-state-set etask-stateid-last-flag etask-flag-delete)
      (forward-line 1)
      (while (> subnum 0)
        (etask-set-flag etask-flag-delete 'ctrl)
        (setq subnum (1- subnum))
        (forward-line 1))
      (etask-goto-delimiter))))

(defun etask-set-flag-archive()
  "Set 'archive' flag for current top level element and all its
subelements at once."
  (interactive)
  (when (and (etask-interactive-preconditions)
             (not (etask-state-get etask-stateid-archive))
             (etask-is-flag-allowed etask-flag-archive))
    (let* (subnum)
      (etask-goto-toplevel)
      (setq subnum (etask-cat-get-subelement-num-after))
      (etask-set-flag etask-flag-archive 'ctrl)
      (cons etask-flag-archive etask-current-flags)
      (etask-state-set etask-stateid-last-flag etask-flag-archive)
      (forward-line 1)
      (while (> subnum 0)
        (etask-set-flag etask-flag-archive 'ctrl)
        (setq subnum (1- subnum))
        (forward-line 1))
      (etask-goto-delimiter))))

(defun etask-kill-element()
  "Kill current element and all its subelements."
  (interactive)
  (let ((currel (etask-cat-get-current-element)))
    (etask-cat-delete-element (etask-cat-get-current-elementindex))
    (when (> (length etask-element-killring) etask-element-killring-size)
      (setq etask-element-killring (butlast etask-element-killring)))
    (setq etask-element-killring (cons currel etask-element-killring))
    (etask-cat-show-elements)))

(defun etask-yank-element(&optional num)
  "Insert the last killed element.  If optional NUM is non-nil, insert
the NUMth most recently killed element, where 1 is the last killed
element."
  (interactive "P")
  (when (not (etask-cat-insert-element
              (if (and (natnump num) (> num 0))
                  (nth num etask-element-killring)
                (car etask-element-killring))
              (etask-cat-get-current-elementindex)
              'checkuniqueness))
    (error "%s" (etask-lang-msg 1015 etask-language)))
  (etask-cat-show-elements))

(defun etask-set-flag-postpone-element()
  ""
  (interactive)
  (when (and (etask-interactive-preconditions)
             (not (etask-state-get etask-stateid-archive))
             (etask-is-flag-allowed etask-flag-post-elt))
    (etask-set-flag etask-flag-post-elt)
    (etask-state-set etask-stateid-last-flag etask-flag-post-elt)))

(defun etask-set-flag-postpone-begin()
  ""
  (interactive)
  (when (and (etask-interactive-preconditions)
             (not (etask-state-get etask-stateid-archive))
             (etask-is-flag-allowed etask-flag-post-beg))
    (etask-set-flag etask-flag-post-beg)
    (etask-state-set etask-stateid-last-flag etask-flag-post-beg)))

(defun etask-set-flag-postpone-duedate()
  ""
  (interactive)
  (when (and (etask-interactive-preconditions)
             (not (etask-state-get etask-stateid-archive))
             (etask-is-flag-allowed etask-flag-post-dd))
    (etask-set-flag etask-flag-post-dd)
    (etask-state-set etask-stateid-last-flag etask-flag-post-dd)))

(defun etask-set-flag-fwd-element()
  ""
  (interactive)
  (when (and (etask-interactive-preconditions)
             (not (etask-state-get etask-stateid-archive))
             (etask-is-flag-allowed etask-flag-fwd-elt))
    (etask-set-flag etask-flag-fwd-elt)
    (etask-state-set etask-stateid-last-flag etask-flag-fwd-elt)))

(defun etask-set-flag-fwd-begin()
  ""
  (interactive)
  (when (and (etask-interactive-preconditions)
             (not (etask-state-get etask-stateid-archive))
             (etask-is-flag-allowed etask-flag-fwd-beg))
    (etask-set-flag etask-flag-fwd-beg)
    (etask-state-set etask-stateid-last-flag etask-flag-fwd-beg)))

(defun etask-set-flag-fwd-duedate()
  ""
  (interactive)
  (when (and (etask-interactive-preconditions)
             (not (etask-state-get etask-stateid-archive))
             (etask-is-flag-allowed etask-flag-fwd-dd))
    (etask-set-flag etask-flag-fwd-dd)
    (etask-state-set etask-stateid-last-flag etask-flag-fwd-dd)))

(defun etask-set-flag-link-elements()
  ""
  (interactive)
  (when (and (etask-interactive-preconditions)
             (not (etask-state-get etask-stateid-archive))
             (etask-is-flag-allowed etask-flag-link))
    (etask-set-flag etask-flag-link)
    (etask-state-set etask-stateid-last-flag etask-flag-link)))

(defun etask-set-mark()
  ""
  (interactive)
  (when (etask-interactive-preconditions)
    (etask-set-flag etask-flag-mark)
    (etask-state-set etask-stateid-last-flag etask-flag-mark)))

(defun etask-delete-current-flag(flag)
  "Delete FLAG in `etask-current-flags'"
  (setq etask-current-flags (delete flag etask-current-flags)))

(defun etask-unset-flag-and-mark()
  "Delete both the mark of current element and its flag."
  (interactive)
  (when (etask-interactive-preconditions)
    (let* ((currline (etask-current-line))
           (currflag (etask-get-flag currline))
           (len (length (etask-get-flagstring)))
           pos linewidth)
      (etask-delete-current-flag currflag)
      (forward-line 0)
      (setq pos (point))
      (setq buffer-read-only nil)
      (forward-line 0)
      (delete-char len)
      (insert (etask-get-flagstring))
      (etask-goto-delimiter)
      (delete-char 1)
      (etask-insert-delimiter)
      (etask-forward-line 1)
      (setq buffer-read-only t))))

(defun etask-set-flag(flag &optional ctrl)
  "Set both the mark of current element and FLAG.  If optional CTRL is
non-nil, then the caller takes care to put point at the correct
position after setting flag and mark."
  (when (and (etask-interactive-preconditions)
             (stringp flag) (> (length flag) 0))
    (let ((currline (etask-current-line))
          (len (length (etask-get-flagstring)))
          pos)
      (forward-line 0)
      (setq pos (point))
      (setq buffer-read-only nil)
      (forward-line 0)
      (delete-char len)
      (etask-format-insert flag len "left")
      (setq etask-current-flags (cons flag etask-current-flags))
      (etask-goto-delimiter)
      (delete-char 1)
      (etask-insert-delimiter 'mark)
      (when (not ctrl)
        (etask-forward-line 1))
      (setq buffer-read-only t))))

(defun etask-toggle-all-marks()
  "Toggle the mark of all tasks."
  (interactive)
  (when (etask-interactive-preconditions)
    (let ((pos (point))
          (lines (etask-cat-get-elementnum))
          (l 1))
      (goto-char (point-min))
      (while (<= l lines)
        (if (etask-is-flag-empty-p (etask-get-flag l))
            (etask-set-mark)
          (etask-unset-flag-and-mark))
        (setq l (1+ l)))
      (goto-char pos))))

(defun etask-toggle-dayunit()
  "Toggle flag that decides whether business days or calendar days are
used for operations."
  (interactive)
  (if (etask-state-get etask-stateid-businessdays)
      (etask-state-set etask-stateid-businessdays nil)
    (etask-state-set etask-stateid-businessdays t))
  (etask-print-dayunit))

(defun etask-print-dayunit()
  "Display message in minibuffer indicating whether business days or
calendar days are used for operations."
  (interactive)
  (if (etask-state-get etask-stateid-businessdays)
      (princ (etask-lang-msg 705 etask-language))
    (princ (etask-lang-msg 708 etask-language))))

(defun etask-set-expended-effort-percent()
  "Set the status of the current task or, if there are marked tasks,
the status of all marked tasks to 'PERCENT completed' according to
values collected via minibuffer."
  (interactive)
  (when (and (etask-interactive-preconditions)
             (not (etask-state-get etask-stateid-archive)))
    (let* ((editlist (etask-get-ixlist-to-be-edited))
           plannedeffort percent currix)
      (while editlist
        (setq element (etask-cat-get-element (car editlist)))
        (setq currix (car editlist))
        (setq plannedeffort 
              (etask-db-get element etask-db-attr-peffort))
        (if (> plannedeffort 0)
            (progn
              (setq percent
                    (calendar-read
                     (concat
                      (format "'%s': "
                              (etask-shorten-string 
                               (etask-db-get 
                                element etask-db-attr-taskname)
                               etask-longer-taskname-len-minibuf))
                      (etask-lang-msg 110 etask-language)
                      " (0-100): ")
                     (lambda (x) (and
                                  (string-match 
                                   etask-wholenumber-regexp 
                                   (prin1-to-string x))
                                  (<= x 100)))))
              (etask-cat-store-element
               (etask-db-set element etask-db-attr-eeffort
                             (etask-simplify-number
                              (* (etask-db-get 
                                  element etask-db-attr-peffort)
                                 (/ (float percent) 100))))
               currix))
          (error "%s"
                 (concat
                  (format "'%s' -- "
                          (etask-db-get 
                           element etask-db-attr-taskname))
                  (etask-lang-msg 1000 etask-language))))
        (setq editlist (cdr editlist)))
      (etask-cat-show-elements))))

(defun etask-set-expended-effort-timeunits()
  "Set the status of the current task or, if there are marked tasks,
the status of all marked tasks, to 'TIME expended' according to values
collected via minibuffer.

TIME is a duration specified by the regexp `etask-effort-regexp'.  For
example, '12h' sets the expended effort to 12 hours, and '16d' sets it
to 16 days."
  (interactive)
  (when (and (etask-interactive-preconditions)
             (not (etask-state-get etask-stateid-archive)))
    (let* ((editlist (etask-get-ixlist-to-be-edited))
           plannedeffort time currix)
      (while editlist
        (setq element (etask-cat-get-element (car editlist)))
        (setq currix (car editlist))
        (setq plannedeffort 
              (etask-db-get element etask-db-attr-peffort))
        (if (> plannedeffort 0)
            (progn
              (setq time
                    (calendar-read
                     (etask-get-prompt-effort 
                      element
                      plannedeffort
                      (etask-lang-msg 140 etask-language))
                     (lambda (x) (and
                                  (string-match 
                                   etask-effort-regexp 
                                   (prin1-to-string x t))
                                  (<= (etask-extract-effort-hours 
                                       (prin1-to-string x t))
                                      plannedeffort)))))
              (etask-çat-store-element
               (etask-db-set element etask-db-attr-eeffort
                             (etask-simplify-number
                              (etask-extract-effort-hours 
                               (prin1-to-string time t))))
               currix))
          (error "%s"
                 (concat
                  (format "'%s' -- "
                          (etask-db-get 
                           element etask-db-attr-taskname))
                  (etask-lang-msg 1000 etask-language))))
        (setq editlist (cdr editlist)))
      (etask-cat-show-elements))))

(defun etask-add-expended-effort-timeunits()
  "Add EFFORT to expended effort of current element or, if there are
marked elements, of all marked elements according to values collected
via minibuffer.

EFFORT is the effort in business hours or days, specified by the
regexp `etask-effort-regexp'.  For example, the inputs '12h' or '16d'
add 12 hours or 16 days to the expended effort, respectively."
  (interactive)
  (when (and (etask-interactive-preconditions)
             (not (etask-state-get etask-stateid-archive)))
    (let* ((editlist (etask-get-ixlist-to-be-edited))
           neweffort plannedeffort expendedeffort maxneweffort currix)
      (while editlist
        (setq element (etask-cat-get-element (car editlist)))
        (setq currix (car editlist))
        (setq plannedeffort (etask-db-get 
                             element etask-db-attr-peffort))
        (setq expendedeffort (etask-db-get 
                              element etask-db-attr-eeffort))
        (setq maxneweffort (etask-simplify-number 
                            (- plannedeffort expendedeffort)))
        (setq neweffort
              (if (= plannedeffort expendedeffort)
                  0
                (calendar-read
                 (etask-get-prompt-effort 
                  element
                  maxneweffort 
                  (etask-lang-msg 150 etask-language))
                 (lambda (x) (and
                              (string-match 
                               etask-effort-regexp
                               (prin1-to-string x t))
                              (<= (etask-extract-effort-hours 
                                   (prin1-to-string x t))
                                  (etask-simplify-number
                                   (- plannedeffort expendedeffort))))))))
        (etask-cat-store-element
         (etask-db-set element etask-db-attr-eeffort
                       (etask-simplify-number
                        (+ (etask-extract-effort-hours 
                            (prin1-to-string neweffort t))
                           expendedeffort)))
         currix)
        (setq editlist (cdr editlist)))
      (etask-cat-show-elements))))

(defun etask-subtract-expended-effort-timeunits()
  "Subtract EFFORT from expended effort of current element or, if
there are marked elements, from the expended efforts of all marked
elements according to values collected via minibuffer.

EFFORT is the effort in business hours or days, specified by the
regexp `etask-effort-regexp'.  For example, the inputs '12h' or '16d'
reduce the expended effort by 12 hours or 16 days, respectively."
  (interactive)
  (when (and (etask-interactive-preconditions)
             (not (etask-state-get etask-stateid-archive)))
    (let* ((editlist (etask-get-ixlist-to-be-edited))
           eeffort plannedeffort expendedeffort)
      (while editlist
        (setq element (etask-cat-get-element (car editlist)))
        (setq currix (car editlist))
        (setq plannedeffort (etask-db-get 
                             element etask-db-attr-peffort))
        (setq expendedeffort (etask-db-get
                              element etask-db-attr-eeffort))
        (cond ((> expendedeffort 0)
               (setq eeffort
                     (if (= plannedeffort 0)
                         0
                       (calendar-read
                        (etask-get-prompt-effort 
                         element
                         expendedeffort
                         (etask-lang-msg 160 etask-language))
                        (lambda (x) (and
                                     (string-match 
                                      etask-effort-regexp 
                                      (prin1-to-string x t))
                                     (<= (etask-extract-effort-hours 
                                          (prin1-to-string x t))
                                         expendedeffort))))))
               (etask-cat-store-element
                (etask-db-set element etask-db-attr-eeffort
                              (etask-simplify-number
                               (- expendedeffort
                                  (etask-extract-effort-hours 
                                   (prin1-to-string 
                                    eeffort t)))))
                currix))
              (t
               ))
        (setq editlist (cdr editlist)))
      (etask-cat-show-elements))))

(defun etask-set-fulltime-equivalence()
  "Adjust the planned end date of the current task or, if there are
marked tasks, the planned end date of all marked tasks according to
FTE values collected via minibuffer."
  (interactive)
  (when (and (etask-interactive-preconditions)
             (not (etask-state-get etask-stateid-archive)))
    (let* ((catid (car (etask-cat-get-current-item)))
           (editlist (etask-get-ixlist-to-be-edited))
           peffort currix)
      (when (and (etask-cat-is-valid-catid-p catid)
                 (= catid etask-category-projectid))
        (while editlist
          (setq element (etask-cat-get-element (car editlist)))
          (setq currix (car editlist))
          (when (not (etask-is-milestone-p element))
            (let* ((fte (string-to-number
                         (etask-read 
                          (concat
                           (format "'%s': "
                                   (etask-db-get
                                    element etask-db-attr-taskname))
                           (etask-lang-msg 130 etask-language)
                           "  ? ")
                          (lambda (x) 
                            (and
                             (string-match 
                              etask-effort-numonly-regexp x)
                             (> (string-to-number x) 
                                0)
                             (<= (string-to-number x) 
                                 etask-max-fte))))))
                   (peffort-in-days-new
                    (ceiling
                     (/
                      (/ (float (etask-db-get
                                 element etask-db-attr-peffort))
                         fte)
                      etask-workinghours-per-day)))

                   ;; maybe initial begin is a holiday or a weekend day
                   (beginoffset
                    (etask-days-till-business-day
                     (etask-db-get element etask-db-attr-taskbegin)))
                   (begindate
                    (etask-add-days-to-date 
                     (etask-db-get element etask-db-attr-taskbegin)
                     beginoffset))
                   (newtaskend
                    (etask-add-businessdays-to-date 
                     begindate 
                     (cond ((and (etask-businessday-p
                                  begindate)
                                 (< peffort-in-days-new 1))
                            0)          ;effort fits into first day
                           ((and (not (etask-businessday-p
                                       begindate))
                                 (< peffort-in-days-new 1)
                                 (> peffort-in-days-new 0))
                            1)
                           ((etask-businessday-p begindate)
                            (1- peffort-in-days-new))
                           (t
                            peffort-in-days-new)))))
              (etask-cat-store-element 
               (etask-db-set
                (etask-db-set element etask-db-attr-taskend newtaskend)
                etask-db-attr-taskbegin
                begindate)
               currix)
              (etask-cat-show-elements)))
          (setq editlist (cdr editlist)))))))

(defun etask-generate-report()
  "Generate a detailed status report for all tasks or, if there are
marked tasks, for all marked tasks."
  (interactive)
  (let ((tasklist (etask-cat-get-current-elts)))
    (when tasklist
      (let ((markedtasks (etask-cat-get-marked-elements))
            (num 0)
            (task))

        (if (and (etask-multi-frame-p) etask-allow-popup-frames-p)
            (let ((frame-alist))
              (setq frame-alist
                    (cons (cons 'name
                                (concat "EtaskMode: --- "
                                        (etask-lang-msg 700 etask-language)
                                        " --- "
                                        (calendar-date-string 
                                         (calendar-current-date))))
                          frame-alist))
              (select-frame (make-frame frame-alist))
              (set-buffer (get-buffer-create etask-report-buffer))
              (find-file (etask-get-reportfile))
              (goto-char (point-max))
              (if (> (point) (point-min))
                  (insert "\n\n\n\n\n"))
              (if markedtasks
                  (etask-insert-report-string markedtasks)
                (etask-insert-report-string tasklist))
              (save-buffer))

          (progn
            (set-buffer (get-buffer-create etask-report-buffer))
            (save-excursion
              (find-file (etask-get-reportfile))
              (goto-char (point-max))
              (if (> (point) (point-min))
                  (insert "\n\n\n\n\n"))
              (if markedtasks
                  (etask-insert-report-string markedtasks)
                (etask-insert-report-string tasklist))
              (save-buffer))))))))

(defun etask-set-plannedbegin()
  "Set begin of current element to a date provided via minibuffer.  If
the current element is a todo, then its due date is changed."
  (interactive)
  (when (and (etask-interactive-preconditions)
             (not (etask-state-get etask-stateid-archive)))
    (let* ((catid (car (etask-cat-get-current-item)))
           (item (car (cdr (etask-cat-get-current-item))))
           (currix (etask-cat-get-current-elementindex))
           (elt (etask-cat-get-current-element))
           (eltname (etask-db-get elt etask-db-attr-taskname)))

      (when (and elt (etask-cat-civalid-p catid item))

        (cond ((= catid etask-category-todoid)
               (setq elt (etask-todo-edit-duedate elt)))
              ((= catid etask-category-eventid)
               (setq elt (etask-ev-edit-begindate elt)))
              ((= catid etask-category-projectid)
               (let* ((oldbegin
                       (etask-db-get elt etask-db-attr-taskbegin))
                      (begin
                       (etask-get-taskdate
                        (concat
                         (format "'%s'"
                                 (etask-shorten-string
                                  eltname
                                  etask-longer-taskname-len-minibuf))
                         ": "
                         (etask-lang-msg 305 etask-language))
                        elt))
                      (postponep
                       (calendar-date-compare (list oldbegin) (list begin)))
                      (diff
                       (etask-business-days-between oldbegin begin)))
                 (if postponep
                     (setq elt 
                           (etask-start-n-days-later 
                            diff
                            (etask-finish-n-days-later diff elt 'business)
                            'business))
                   (setq elt
                         (etask-finish-n-days-earlier
                          diff
                          (etask-start-n-days-earlier diff elt 'business)
                          'business))))))
      
        (etask-cat-store-element elt currix)
        (etask-cat-show-elements)))))

(defun etask-forward-begindate(alldaysp)
  "Set begin of current task or, if there are marked tasks, the begin
of all marked tasks to a date N days earlier according to values
collected via minibuffer.

If ALLDAYSP is nil, all days are valid days for this operation.
Otherwise only business days are used."
  (interactive "P")
  (when (and (etask-interactive-preconditions)
             (not (etask-state-get etask-stateid-archive)))
    (let* ((catid (car (etask-cat-get-current-item)))
           (item (car (cdr (etask-cat-get-current-item))))
           (editlist (etask-get-ixlist-to-be-edited 1))
           (daysearlier 0)
           (businessp alldaysp))
      (when (and (etask-cat-civalid-p catid item)
                 (= catid etask-category-projectid))
        (while editlist
          (setq element (etask-cat-get-element (car editlist)))
          (setq daysearlierstr
                (etask-read 
                 (etask-get-prompt-fwdrew
                  element
                  (etask-lang-msg 190 etask-language)
                  businessp)
                 (lambda (x) (or (string-match 
                                  etask-wholenumber-regexp x)
                                 (string-match
                                  etask-effort-dwm-regexp x)))
                 (if (> daysearlier 0) ;default for successive changes
                     (number-to-string daysearlier))))
          (etask-write-element-to-fwd-date 
           element 'begin daysearlierstr businessp)
          (setq editlist (cdr editlist)))))))

(defun etask-postpone-begindate(alldaysp)
  "Set begin of current task or, if there are marked tasks, of all
marked tasks to a date N days later according to values collected via
minibuffer.

If ALLDAYSP is nil, all days are valid days for this operation.
Otherwise only business days are used."
  (interactive "P")
  (when (etask-interactive-preconditions)
    (let* ((catid (car (etask-cat-get-current-item)))
           (item (car (cdr (etask-cat-get-current-item))))
           (editlist (etask-get-ixlist-to-be-edited 1))
           (dayslater 0)
           (businessp alldaysp))
      (when (and (etask-cat-civalid-p catid item)
                 (= catid etask-category-projectid))
        (while editlist
          (setq element (etask-cat-get-element (car editlist)))
          (if (etask-postpone-begindate-opvalid-p 
               element 1 businessp)
              (progn
                (setq dayslaterstr
                      (etask-read
                       (etask-get-prompt-fwdrew
                        element
                        (etask-lang-msg 200 etask-language)
                        businessp)
                       (lambda (x)
                         (and
                          (or (string-match 
                               etask-wholenumber-regexp x)
                              (string-match
                               etask-effort-dwm-regexp x))
                          (etask-postpone-begindate-opvalid-p
                           element
                           (string-to-number x)
                           businessp)))
                       (if (> dayslater 0) ;default for successive changes
                           (number-to-string dayslater))))
                (etask-write-element-to-postp-date
                 element 'begin dayslaterstr businessp))
            (error (etask-postpone-begindate-errormsg businessp)))
          (setq editlist (cdr editlist)))))))

(defun etask-forward-duedate(alldaysp)
  "Set end of current task or, if there are marked tasks, of all
marked tasks to a date N days before its current end according to
values collected via minibuffer.

If ALLDAYSP is nil, all days are valid days for this operation.
Otherwise only business days are used."
  (interactive "P")
  (when (etask-interactive-preconditions)
    (let* ((catid (car (etask-cat-get-current-item)))
           (item (car (cdr (etask-cat-get-current-item))))
           (editlist (etask-get-ixlist-to-be-edited 1))
           (opvalidp)
           (daysearlier 0)
           (businessp alldaysp))
      (when (and (etask-cat-civalid-p catid item)
                 (= catid etask-category-projectid))
        (while editlist
          (setq element (etask-cat-get-element (car editlist)))
          (if (etask-forward-duedate-opvalid-p
               element 1 businessp)
              (progn
                (setq daysearlierstr
                      (etask-read
                       (etask-get-prompt-fwdrew
                        element
                        (etask-lang-msg 210 etask-language)
                        businessp)
                       (lambda (x) 
                         (and
                          (or (string-match 
                               etask-wholenumber-regexp x)
                              (string-match
                               etask-effort-dwm-regexp x))
                          (etask-forward-duedate-opvalid-p 
                           element
                           (string-to-number x)
                           businessp)))
                       (if (> daysearlier 0) ;default for successive changes
                           (number-to-string daysearlier))))
                (etask-write-element-to-fwd-date 
                 element nil daysearlierstr businessp))
            (error (etask-forward-duedate-errormsg businessp)))
          (setq editlist (cdr editlist)))))))
  
(defun etask-postpone-duedate(alldaysp)
  "Set end of current task or, if there are marked tasks, of all
marked tasks to a date N days later according to values collected via
minibuffer.

If ALLDAYSP is nil, all days are valid days for this operation.
Otherwise only business days are used."
  (interactive "P")
  (when (etask-interactive-preconditions)
    (let* ((catid (car (etask-cat-get-current-item)))
           (item (car (cdr (etask-cat-get-current-item))))
           (editlist (etask-get-ixlist-to-be-edited 1))
           (dayslater 0)
           (businessp alldaysp))
      (when (and (etask-cat-civalid-p catid item)
                 (= catid etask-category-projectid))
        (while editlist
          (setq element (etask-cat-get-element (car editlist)))
          (setq dayslaterstr
                (etask-read
                 (etask-get-prompt-fwdrew
                  element
                  (etask-lang-msg 220 etask-language)
                  businessp)
                 (lambda (x) (or (string-match 
                                  etask-wholenumber-regexp x)
                                 (string-match
                                  etask-effort-dwm-regexp x)))
                 (if (> dayslater 0)   ;default for successive changes
                     (number-to-string dayslater))))

          (etask-write-element-to-postp-date
           element nil dayslaterstr businessp)
          (setq editlist (cdr editlist)))))))

(defun etask-forward-whole-task(alldaysp)
  "Forward current task or, if there are marked tasks, all marked
tasks by N days according to values collected via minibuffer.

If ALLDAYSP is nil, all days are valid days for this operation.
Otherwise only business days are used."
  (interactive "P")
  (when (etask-interactive-preconditions)
    (let* ((editlist (etask-get-ixlist-to-be-edited 1))
           (daysearlier 0)
           (businessp alldaysp))
      (while editlist
        (setq element (etask-cat-get-element (car editlist)))
        (setq daysearlierstr
              (etask-read
               (etask-get-prompt-fwdrew
                element
                (etask-lang-msg 230 etask-language)
                businessp)
               (lambda (x) (or (string-match 
                                etask-wholenumber-regexp x)
                               (string-match
                                etask-effort-dwm-regexp x)))
               (if (> daysearlier 0)   ;default for successive changes
                   (number-to-string daysearlier))))
        (etask-write-element-to-forward element daysearlierstr businessp)
        (setq editlist (cdr editlist))))))

(defun etask-postpone-whole-task(alldaysp)
  "Postpone current task or, if there are marked tasks, all marked
tasks by N days according to values collected via minibuffer.

If ALLDAYSP is nil, all days are valid days for this operation.
Otherwise only business days are used."
  (interactive "P")
  (when (etask-interactive-preconditions)
    (let* ((editlist (etask-get-ixlist-to-be-edited 1))
           (dayslater 0)
           (businessp alldaysp))
      (while editlist
        (setq element (etask-cat-get-element (car editlist)))
        (setq dayslaterstr
               (etask-read
                (etask-get-prompt-fwdrew
                 element
                 (etask-lang-msg 240 etask-language)
                 businessp)
                (lambda (x) (or (string-match 
                                 etask-wholenumber-regexp x)
                                (string-match
                                 etask-effort-dwm-regexp x)))
                (if (> dayslater 0)    ;default for successive changes
                    (number-to-string dayslater))))
        (etask-write-element-to-postpone element dayslaterstr businessp)
        (setq editlist (cdr editlist))))))

(defun etask-set-tracking-algorithm()
  "Set tracking algorithm for current task or, if there are marked
tasks, for all marked tasks according to values collected via
minibuffer.

See `etask-tracking-algorithms' for more information about available
algorithms."
  (interactive)
  (let ((catid (car (etask-cat-get-current-item)))
        (item (car (cdr (etask-cat-get-current-item)))))
    (when (and (etask-interactive-preconditions)
               (not (etask-state-get etask-stateid-archive))
               (etask-cat-civalid-p catid item)
               (= catid etask-category-projectid))
      (let* ((algorithm 
              (completing-read
               (concat
                (etask-lang-msg 250 etask-language)
                "  ? ")
               etask-tracking-algorithms-alist
               nil t))
             (editlist (etask-get-ixlist-to-be-edited))
             currix)
        (while editlist
          (setq element (etask-cat-get-element (car editlist)))
          (setq currix (car editlist))
          (etask-cat-store-element
           (etask-db-set element etask-db-attr-tracking algorithm)
           currix)
          (setq editlist (cdr editlist)))
        (etask-cat-show-elements)))))

(defun etask-toggle-critical-normal()
  "Toggle critical flag of current task or, if there are marked tasks, of all
marked tasks."
  (interactive)
  (when (and (etask-interactive-preconditions)
             (not (etask-state-get etask-stateid-archive)))
    (etask-toggle-attribute 'etask-db-set
                            etask-db-attr-tasktype
                            etask-criticaltask-string
                            (lambda(task)t))))

(defun etask-toggle-highrisk-normal()
  "Toggle high risk flag of current task or, if there are marked tasks, of all
marked tasks."
  (interactive)
  (when (and (etask-interactive-preconditions)
             (not (etask-state-get etask-stateid-archive)))
    (etask-toggle-attribute 'etask-db-set
                            etask-db-attr-tasktype
                            etask-highrisktask-string
                            (lambda(task)t))))

(defun etask-move-element-up(&optional down)
  "Moves the current top element and all its subelements one line up
-- if it is not already at the top -- or, if DOWN is non-nil, one line
down -- if it is not already at the bottom."
  (interactive)
  (when (etask-interactive-preconditions)
    (let* ((movepos (car (etask-cat-get-current-elementindex)))
           (moveel (etask-cat-get-element (list movepos)))
           moveix)
      (when (or (and (null down) (> movepos 1))
                (and down (< movepos (etask-cat-get-topelementnum))))
        (if down
            (setq moveix (list (1+ movepos)))
          (setq moveix (list (1- movepos))))
        (etask-cat-delete-element (list movepos))
        (etask-cat-store-element moveel moveix))
      ;; very slow - improve!
      (etask-cat-show-elements)
      (etask-cat-goto-elementindex moveix)
      (etask-cat-show-elements))))
    
(defun etask-move-element-down()
  "Move the task under cursor one line down -- if it is not already at
the bottom."
  (interactive)
  (etask-move-element-up 'down))

(defun etask-set-chartdates(&optional all)
  "Set chart's begin date and its end date.  If optional ALL is
non-nil all elements of current category are to be displayed.  If
optional ALL is nil, then the caller ensures that the chart's begin
date and its zoom factor are set correctly."
  (if all
      (let ((elts (etask-cat-get-current-elts)))
        (etask-state-set
         etask-stateid-chartstart (etask-calculate-earliestbegin elts))
        (etask-state-set
         etask-stateid-chartend (etask-calculate-lastend elts)))
    (let* ((zoom (etask-state-get etask-stateid-zoomfactor))
           (maxbarlen (etask-state-get etask-stateid-maxbarlen))
           (chartstart (etask-state-get etask-stateid-chartstart))
           (chartend (etask-get-chartdate-end chartstart zoom maxbarlen)))
      (etask-state-set etask-stateid-chartend chartend))))

(defun etask-set-charttimes()
  "Set chart's begin time and its end time."
  (cond ((etask-todo-organizer-view-p)
         (if etask-todo-day-begin-near-now-p
             (etask-state-set
              etask-stateid-chartstarttime
              (etask-todo-validate-begintime
               (etask-todo-get-current-hour)))
           (etask-state-set
            etask-stateid-chartstarttime etask-first-workinghour))
         (etask-state-set
          etask-stateid-chartendtime
          (+ (etask-state-get etask-stateid-chartstarttime)
             (etask-todo-get-hrnum))))
        ((etask-todo-unit-is-hr-p)
         (etask-state-set
          etask-stateid-chartstarttime etask-first-workinghour)
         (etask-state-set
          etask-stateid-chartendtime etask-last-workinghour))
        (t
         (etask-state-clear etask-stateid-chartstarttime)
         (etask-state-clear etask-stateid-chartendtime))))

(defun etask-zoom-readminibuffer()
  ""
  (etask-read
   (concat
    (etask-lang-msg 312 etask-language)
    ": ["
    "a=" (etask-lang-msg 308 etask-language) ", "
    "w=" (etask-lang-msg 311 etask-language) ", "
    "d=" (etask-lang-msg 309 etask-language) ", "
    "p=" (etask-lang-msg 310 etask-language) ", "
    "2=2 " (etask-lang-msg 706 etask-language) ", "
    "3=3 " (etask-lang-msg 706 etask-language) ", ..."
    "]  ? ")
   (lambda (x) 
     (or (string= x "a")
         (string= x "w")
         (string= x "d")
         (string= x "p")
         (and (string-match etask-wholenumber-regexp x))))))

(defun etask-zoom(&optional zoomfactor)
  "Zoom draft Gantt chart.  If optional ZOOMFACTOR is non-nil it
controls the width of a day in characters."
  (interactive)
  (when (> (buffer-size) 0)
    (let* ((allowed (etask-zoom-allowed))
           (zoom (cond ((and (natnump zoomfactor) (= zoomfactor 0))
                        (floor allowed))
                       ((and (natnump zoomfactor) (>= zoomfactor allowed))
                        zoomfactor)
                       ((and (stringp zoomfactor)
                             (or (string= zoom "a")
                                 (string= zoom "w")
                                 (string= zoom "d")
                                 (string= zoom "p")))
                        zoomfactor)
                       (t
                        (etask-zoom-readminibuffer)))))
      (when (stringp zoom)
        (let ((len (etask-get-maxbarlen)))
          (cond ((string= zoom "a")
                 (setq zoom allowed)
                 (etask-state-set etask-stateid-zoommodus "a")
                 (etask-set-control-item etask-ctrl-zoommodus "a"))
                ((string= zoom "w")
                 (setq zoom (/ len 7))
                 (etask-state-set etask-stateid-zoommodus "w")
                 (etask-set-control-item etask-ctrl-zoommodus "w"))
                ((string= zoom "d")
                 (setq zoom len)
                 (etask-state-set etask-stateid-zoommodus "d")
                 (etask-set-control-item etask-ctrl-zoommodus "d"))
                ((string= zoom "p")
                 (setq zoom len)
                 (etask-state-set etask-stateid-zoommodus "p")
                 (etask-state-set etask-stateid-todostate "todoinit")
                 (etask-set-control-item etask-ctrl-zoommodus "p"))
                (t
                 (setq zoom (/ len (string-to-number zoom)))
                 ;; `etask-get-chartdate-end' may cause EtaskMode to
                 ;; display one additional day due to rounding error
                 ;; (example: len = 52, days to display = 9)
                 (etask-state-clear etask-stateid-zoommodus)
                 (etask-set-control-item etask-ctrl-zoommodus "")))))
      (etask-state-set etask-stateid-zoomfactor zoom)
      (etask-set-control-item etask-ctrl-zoomfactor zoom)
      (if (or (= zoom 0) (= zoom allowed))
          (etask-set-chartdates 'displayall)
        (etask-set-chartdates))
      (etask-set-charttimes)
      (etask-cat-show-elements))))

(defun etask-quit()
  "Quit etask and delete its window."
  (interactive)
  (etask-states-save)
  (etask-log (concat "etask exit: " (current-time-string) "\n") nil 'exit)
  (bury-buffer)
  (if (one-window-p)
      (delete-windows-on etask-buffer)
    (delete-window)))

(defun etask-migrate-01-02()
  "Migrate tasks file (release 0.1.xx to project file (rel. 0.2.xx)"
  (interactive)
  (let ((catid etask-category-projectid)
        (tfile (etask-string-trim
                (etask-read
                 (concat
                  (etask-lang-msg 790 etask-language)
                  "  ? ")
                 (lambda (x) (string-match "^[ \\/.~a-zA-Z0-9_?!-]+$" x)))))
        (projname (etask-string-trim
                   (etask-read
                    (concat
                     (etask-lang-msg 410 etask-language)
                     "  ? ")
                    (lambda (x) (string-match "^[ a-zA-Z0-9_?!-]+$" x))))))
    (if (and (stringp tfile) (> (length tfile) 0)
             (stringp projname) (> (length projname) 0)
             (file-readable-p tfile))
        (let ((pfile (concat
                      etask-working-dir
                      (etask-cat-get-itemfilename catid projname))))
          (if (and (stringp pfile) (> (length pfile) 0))
              (let ((oldprojname "no project"))
                (copy-file tfile pfile 1)
                (let ((elements (etask-cat-get-elementlist catid projname))
                      (newelements))
                  (while elements
                    (setq newelements (cons (etask-db-set 
                                             (car elements)
                                             etask-db-attr-projname
                                             projname)
                                            newelements))
                    (setq elements (cdr elements)))
                  (etask-write-elements 
                   (nreverse newelements) catid projname 'erase))) 
            (error "%s" (etask-lang-msg 1014 etask-language))))
      (when (not (file-readable-p tfile))
        (error "%s" (etask-lang-msg 1013 etask-language)))
      (error "%s" (etask-lang-msg 1014 etask-language)))))


;;; Moving Functions

(defun etask-forward-line(num)
  "Move point forward according to `forward-line' and goto delimiter."
  (forward-line num)
  (etask-goto-delimiter))

(defun etask-goto-line(num)
  "Move point forward from \(point-min\) according to `forward-line'
and goto delimiter."
  (when (and (natnump num) (> num 0))
    (goto-char (point-min))
    (etask-forward-line (1- num))))

(defun etask-goto-elementline(&optional num)
  "Set cursor position to the delimiter of the element queried via
minibuffer, or, if NUM is non-nil, of the NUMth element."
  (interactive)
  (when (> (buffer-size) 0)
    (let* ((elementnum (etask-cat-get-elementnum))
           (catid (car (etask-cat-get-current-item)))
           (elementname (etask-cat-get-catelementname catid)))
      (when (and (interactive-p)
                 (or
                  (not (natnump num))
                  (< num 1) (> num elementnum)))
        (setq num (calendar-read
                   (concat 
                    (etask-lang-msg 280 etask-language)
                    " "
                    elementname
                    " (0 < "
                    elementname
                    (format " < %d): " (1+ elementnum)))
                   (lambda (x) (and (natnump x)
                                    (> x 0) 
                                    (< x (1+ elementnum)))))))
      (when (and (> num 0) (<= num elementnum))
        (save-restriction
          (widen)
          (etask-goto-line num)
          (when (interactive-p)
            (etask-cat-show-elementstatus
             (etask-cat-get-current-elementindex))))))))

(defun etask-goto-elementline-1()
  "Set cursor position to pos `taskname-taskbar-delimiter' of the 1st
task."
  (interactive)
  (when (> (buffer-size) 0)
    (etask-goto-elementline 1)))

(defun etask-goto-elementline-2()
  "Set cursor position to pos `taskname-taskbar-delimiter' of the 2nd
task."
  (interactive)
  (when (> (buffer-size) 0)
    (etask-goto-elementline 2)))

(defun etask-goto-elementline-3()
  "Set cursor position to pos `taskname-taskbar-delimiter' of the 3th
task."
  (interactive)
  (when (> (buffer-size) 0)
    (etask-goto-elementline 3)))

(defun etask-goto-elementline-4()
  "Set cursor position to pos `taskname-taskbar-delimiter' of the 4th
task."
  (interactive)
  (when (> (buffer-size) 0)
    (etask-goto-elementline 4)))

(defun etask-goto-elementline-5()
  "Set cursor position to pos `taskname-taskbar-delimiter' of the 5th
task."
  (interactive)
  (when (> (buffer-size) 0)
    (etask-goto-elementline 5)))

(defun etask-goto-elementline-6()
  "Set cursor position to pos `taskname-taskbar-delimiter' of the 6th
task."
  (interactive)
  (when (> (buffer-size) 0)
    (etask-goto-elementline 6)))

(defun etask-goto-elementline-7()
  "Set cursor position to pos `taskname-taskbar-delimiter' of the 7th
task."
  (interactive)
  (when (> (buffer-size) 0)
    (etask-goto-elementline 7)))

(defun etask-goto-elementline-8()
  "Set cursor position to pos `taskname-taskbar-delimiter' of the 8th
task."
  (interactive)
  (when (> (buffer-size) 0)
    (etask-goto-elementline 8)))

(defun etask-goto-elementline-9()
  "Set cursor position to pos `taskname-taskbar-delimiter' of the 9th
task."
  (interactive)
  (when (> (buffer-size) 0)
    (etask-goto-elementline 9)))

(defun etask-goto-delimiter()
  "Set cursor position to next delimiter pos
`taskname-taskbar-delimiter'.  If there is no delimiter after current
position go to the last delimiter."
  (when (> (buffer-size) 0)
    (forward-line 0)
    (if (re-search-forward etask-delimiter-regexp nil t)
        (goto-char (1- (point)))
      (etask-goto-last-delimiter))))

(defun etask-goto-first-delimiter()
  "Set cursor position to the first `taskname-taskbar-delimiter' pos."
  (interactive)
  (when (> (buffer-size) 0)
    (while (re-search-backward etask-delimiter-regexp nil t))
    (when (interactive-p)
      (etask-cat-show-elementstatus '(1)))
    (etask-goto-delimiter)))

(defun etask-goto-last-delimiter()
  "Set cursor position to the last `taskname-taskbar-delimiter' pos.

Used to circle when navigating with 'p'.  Returns number of
line-forwards.  If current position is after the last delimiter,
function sets cursor over last delimiter and returns -1."
  (interactive)
  (when (> (buffer-size) 0)
    (let ((stepcounter 0)
          (elnum (etask-cat-get-elementnum))
          (currline (etask-current-line)))
      (cond ((< currline elnum)
             (progn
               (while (re-search-forward etask-delimiter-regexp nil t)
                 (setq stepcounter (1+ stepcounter)))
               (goto-char (1- (point)))))
            ((> currline elnum)
             (re-search-backward etask-delimiter-regexp nil t)
             (setq stepcounter -1))
            (t
             (etask-goto-delimiter)))
      (when (interactive-p)
        (etask-cat-show-elementstatus
         (etask-cat-get-current-elementindex)))
      stepcounter)))

(defun etask-previous-line()
  "Move cursor to previous line over `taskname-taskbar-delimiter'.

If point is already at the beginning, i.e. over the first delimiter,
the function sets the cursor over the last delimiter - but if one or
more tasks at the beginning are ivsisible because of narrowing the
cursor's position stays the same."
  (interactive)
  (when (> (buffer-size) 0)
    (save-restriction
      (widen)
      (let ((currline (etask-current-line)))
        (cond ((and (> currline 1)
                    (<= currline (etask-cat-get-elementnum)))
               (forward-line -1)
               (etask-goto-delimiter)
               (etask-cat-show-elementstatus
                (etask-cat-get-current-elementindex)))
              (t
               (etask-goto-last-delimiter)
               (etask-cat-show-elementstatus
                (etask-cat-get-current-elementindex))))))))

(defun etask-next-line()
  "Move cursor to the next line over `taskname-taskbar-delimiter'.

If point is already at the bottom, i.e. over the last delimiter, the
function sets the cursor over the first delimiter - but if one or more
tasks at the bottom are ivsisible because of narrowing the cursor's
position stays the same."
  (interactive)
  (when (> (buffer-size) 0)
    (etask-goto-delimiter)
    (save-restriction
      (widen)
      (if (< (etask-current-line) (etask-cat-get-elementnum))
          (progn
            (forward-line 1)
            (etask-goto-delimiter)
            (etask-cat-show-elementstatus
             (etask-cat-get-current-elementindex)))
        (etask-goto-first-delimiter)
        (etask-cat-show-elementstatus '(1))))))

(defun etask-previous-line-samelevel()
  "Move point to the previous \(sub\)element with the same level
within the current main element.  Do not move if there is no such
other element."
  (interactive)
  (let ((lines (etask-cat-get-elementnum-samelevel-before)))
    (while (> lines 0)
      (etask-previous-line)
      (setq lines (1- lines)))))

(defun etask-next-line-samelevel()
  "Move point to the next \(sub\)element with the same level
within the current main element.  Do not move if there is no such
other element."
  (interactive)
  (let ((lines (etask-cat-get-elementnum-samelevel-after)))
    (while (> lines 0)
      (etask-next-line)
      (setq lines (1- lines)))))

(defun etask-goto-toplevel()
  "Move point to the current element's top level.  Do not move if
point is already at the top level."
  (interactive)
  (let* ((level (etask-cat-get-current-elementindex))
         (lines (if (= (length level) 1)
                    0
                  (etask-cat-get-elementnum-currenttop))))
    (while (> lines 0)
      (etask-previous-line)
      (setq lines (1- lines)))))

(defun etask-undo(number)
  "Undo the last NUMBER operations."
  (interactive "P")
  (when (> (buffer-size) 0)
    (let ((taskbuffer (etask-cat-get-current-itembuffer)))
      (save-current-buffer
        (set-buffer taskbuffer)
        (undo number))
      (etask-cat-show-elements))))


;;; Utilities Functions

(defun etask-get-prompt-fwdrew(task msg businessp)
  "Return string for minibuffer query.  TASK begin or end date is forwarded or postponed."
  (concat
   (format "'%s': "
           (etask-shorten-string 
            (etask-db-get task etask-db-attr-taskname)
            etask-longer-taskname-len-minibuf))
   msg
   (when businessp
     (concat
      " ("
      (etask-lang-msg 705 etask-language)
      ")"))
   ": "))

(defun etask-get-prompt-effort(task max msg)
  "Return string for minibuffer query.  Effort entered for TASK must not exceed
MAX hours.  Constraints included in string.

'NAME': MSG (constraints)   Constraints example: (<= 140h, 20d, 4w)"
  (concat
   (format "'%s': "
           (etask-shorten-string
            (etask-db-get task etask-db-attr-taskname)
            etask-longer-taskname-len-minibuf))
   msg
   (format " (<= %sh" (number-to-string max))
   (when (>= max etask-workinghours-per-day)
     (concat
      ", "
      (format "%sd"
              (number-to-string 
               (etask-simplify-number 
                (/ (float max)
                   etask-workinghours-per-day))))))
   (when (>= max etask-workinghours-per-day)
     (concat
      ", "
      (format "%sw"
              (number-to-string 
               (etask-simplify-number 
                (/ (float max)
                   (* etask-workinghours-per-day
                      etask-workingdays-per-week)))))))
   "): "))

(defun etask-get-maxtasklen()
  "Return maximum task name length in draft Gantt chart."
  (let* ((level (etask-state-get etask-stateid-level))
         (w (if (natnump level)
                (+ level (length etask-subelements-prefix))
              0)))
    (min etask-max-taskname-len-chart
         (+ (length (etask-get-infostring))
            (length (etask-get-subelstring))
            (etask-longest-string-in-list
             (etask-cat-get-current-elts 'name))
            w))))

(defun etask-get-maxbarlen()
  "Return maximum task bar length in [0..n]."
  (max
   (- (window-width) 
      (length (etask-get-flagstring))
      (length (etask-get-infostring))
      (length (etask-get-subelstring))
      (etask-get-maxtasklen) 
      2)
   0))

(defun etask-get-tasknames-list(tasklist)
  "Return a list with all tasknames in TASKLIST."
  (when tasklist
    (cons (etask-db-get (car tasklist) etask-db-attr-taskname)
          (etask-get-tasknames-list (cdr tasklist)))))

(defun etask-get-main-ctrl-buffer()
  "Return the EtaskMode control buffer."
  (find-file-noselect (etask-get-main-ctrl-filename)))

(defun etask-get-main-ctrl-filename()
  "Return the EtaskMode control file name."
  (concat etask-working-dir etask-controlfile))

(defun etask-get-item-ctrl-buffer(&optional catid item)
  "Return the current item control file buffer or, if CATID and ITEM
are valid, return ITEM's control file buffer."
  (let ((filename (etask-get-item-ctrl-filename catid item)))
    (when (and (stringp filename) (> (length filename) 0))
      (find-file-noselect filename))))

(defun etask-get-item-ctrl-filename(&optional catid item)
  "Return the current item control file name or, if CATID and ITEM
are valid, return ITEM's control file name."
  (let* ((catid (if catid
                    catid
                  (car (etask-cat-get-current-item))))
         (item (if item
                   (etask-cat-trim-current-id item)
                 (etask-cat-trim-current-id
                  (car (cdr (etask-cat-get-current-item))))))
         (filename (etask-cat-get-itemfilename catid item)))
    (concat etask-working-dir filename "." etask-controlfile)))

(defun etask-set-control-item(ctrl value &optional main)
  "Set CTRL item in current project control file to VALUE.  If optional MAIN is non-nil then CTRL is set to VALUE in EtaskMod control file."
  (when (stringp ctrl)
    (let ((buffer (if main
                      (etask-get-main-ctrl-buffer)
                    (etask-get-item-ctrl-buffer))))
      (when buffer
        (save-current-buffer
          (set-buffer buffer)
          (goto-char (point-min))
          (flush-lines ctrl)
          (goto-char (point-min))
          (prin1 (list ctrl value) (current-buffer))
          (if (not (eolp))
              (insert "\n"))
          (save-buffer))))))

(defun etask-get-control-item(ctrl &optional main)
  "Return the value of CTRL in current project control file.

If VALUE not available return the following default values:
etask-ctrl-chartbegindate: project begin date or, if nil, today
etask-ctrl-chartenddate: project end date or, if nil, 
                         etask-ctrl-chartbegindate
etask-ctrl-zoomfactor: 0 \( Note: Maximum = \(etask-get-maxbarlen\) \)
etask-ctrl-todoview-begintime: etask-todo-day-begin
etask-ctrl-todoview-endtime: nil
etask-ctrl-currcategory: nil
etask-ctrl-curritem: nil
etask-ctrl-zoommodus: nil
etask-ctrl-onlinehelp: nil
default: nil"
  (when (stringp ctrl)
    (let ((value)
          (buffer (if main
                        (etask-get-main-ctrl-buffer)
                      (etask-get-item-ctrl-buffer))))
      (when buffer
        (save-current-buffer
          (set-buffer buffer)
          (goto-char (point-min))
          (if (search-forward ctrl nil t)
              (let ((line (buffer-substring-no-properties 
                           (- (match-beginning 0) 2) ;include (" prefix
                           (search-forward-regexp "$"))))
                (setq value (car (cdr (car (read-from-string line))))))))
        (cond ((string= ctrl etask-ctrl-chartbegindate)
               (let ((tasklist (etask-cat-get-current-elts)))
                 (if (> (length value) 0)
                     value
                   (let ((date (etask-calculate-earliestbegin tasklist)))
                     (if (etask-calendardate-is-legal-p date)
                         date
                       (calendar-current-date))))))
              ((string= ctrl etask-ctrl-chartenddate)
               (let ((tasklist (etask-cat-get-current-elts)))
                 (if (> (length value) 0)
                     value
                   (let ((date (etask-calculate-lastend tasklist)))
                     (if (etask-calendardate-is-legal-p date)
                         date
                       (etask-get-control-item
                        etask-ctrl-chartbegindate main))))))
              ((string= ctrl etask-ctrl-todoview-begintime)
               (if (natnump value)
                   value
                 etask-todo-day-begin))
              ((string= ctrl etask-ctrl-todoview-endtime)
               (if (natnump value)
                   value
                 nil))
              ((string= ctrl etask-ctrl-currcategory)
               (if (natnump value)
                   value
                 nil))
              ((string= ctrl etask-ctrl-curritem)
               (if (stringp value)
                   value
                 nil))
              ((string= ctrl etask-ctrl-onlinehelp)
               (if value
                   value
                 nil))
              ((string= ctrl etask-ctrl-zoommodus)
               (if (stringp value)
                   value
                 nil))
              ((string= ctrl etask-ctrl-zoomfactor)
               (if (natnump value)
                   (min value (etask-get-maxbarlen))
                 0))
              (t
               ()))))))

(defun etask-get-chosen-list-element(list &optional str)
  "Return element of LIST chosen by user in minibuffer.  Optional STR
is an additional prompt string."
  (let ((len (length list))
        (input)
        (prompt (concat str (etask-get-list-prompt list 'capitalize))))
    (when (and (stringp prompt) (> len 0))
      (setq input (if (= (length list) 1)
                      (car list)
                    (nth
                     (1- (string-to-number
                          (etask-read
                           (concat prompt "  ? ")
                           (lambda (x) 
                             (and (>= (string-to-number x) 1) 
                                  (<= (string-to-number x) len))))))
                     list)))
      input)))

(defun etask-get-list-prompt(list &optional capitalizep)
  "Return prompt for choosing an element of LIST or nil if list is
nil.  If CAPITALIZEP is non-nil each list element's first character is
upper case.  Modifies LIST."
  (let ((prompt "")
        (count 1))
    (while list
      (when (car list)
        (setq prompt
              (concat prompt
                      "(" (number-to-string count) ") " 
                      (if capitalizep
                          (capitalize (car list))
                        (car list))
                      (when (cdr list) "  "))))
      (setq count (1+ count))
      (setq list (cdr list)))
    (if (> (length prompt) 0)
        prompt
      nil)))

(defun etask-get-chartdate-end(begin zoom maxbarlen)
  "Return the end date of the draft Gantt chart or nil if no valid end
date available.  BEGIN is the chart's begin date.  ZOOM, a
non-negative integer, controls the width of a day in character.
ZOOM=0 => draw whole project.  MAXBARLEN is the maximum bar length in
characters."
  (when (and (etask-calendardate-is-legal-p begin) 
             (natnump zoom) (natnump maxbarlen) (> maxbarlen 0))
    (cond ((= zoom 0)
           (etask-calculate-lastend (etask-cat-get-current-elts)))
          ((> zoom 0)
           (let ((days (/ maxbarlen zoom)))
             (cond ((>= days 1)
                    (etask-add-days-to-date begin (1- days)))
                   ((= days 0)
                    begin)
                   (t
                    begin)))))))

(defun etask-zoom-allowed()
  "Return the smallest zoom level in [1..n] that can be selected.  If
zoom level n shows the whole project but \(n+1\) doesn't then return n.
If n < 1 return the exact real value instead of zero."
  (let* ((maxbarlen (etask-get-maxbarlen))
         (tasklist (etask-cat-get-current-elts))
         (projbegin (etask-calculate-earliestbegin tasklist))
         (projend (etask-calculate-lastend tasklist))
         (projdays (1+ (etask-days-between projbegin projend)))
         (factor (/ (float maxbarlen) projdays))
         (zoom (if (< factor 1)
                   (etask-simplify-number factor)
                 (floor factor))))
    zoom))

(defun etask-interactive-preconditions()
  "Return TRUE if interactive operation is permitted or FALSE
otherwise."
  (and (> (buffer-size) 0)
       (<= (etask-current-line) (etask-cat-get-elementnum))))

(defun etask-get-ixlist-to-be-edited(&optional num)
  "Return list of index lists to be edited - either the current index
or, if there are marked elements, all index lists of the marked
elements.  If optional NUM is an integer > 0 just return an index list
of length NUM."
  (let ((pos (point))
        ixlist)
    (goto-char (point-min))
    (while (and (search-forward etask-delimiter-markedtask-marker nil t)
                (or (null num)
                    (and (natnump num) (> num 0))))
      (setq ixlist (cons 
                    (etask-cat-get-current-elementindex)
                    ixlist))
      (when (and (natnump num) (> num 0))
        (setq num (1- num))))
    (goto-char pos)
    (if ixlist (nreverse ixlist)
      (list (etask-cat-get-current-elementindex)))))

(defun etask-log(string &optional init exit)
  "Write STRING in logfile if `etask-write-logfile-p' is non-nil.

If INIT is non-nil the logfile contents are deleted first.

If EXIT is non-nil the logfile buffer is killed."
  (when etask-write-logfile-p
    (save-current-buffer
      (set-buffer (find-file-noselect 
                   (concat etask-working-dir etask-logfile)))
      (if init
          (progn
            (goto-char (point-min))
            (erase-buffer))
        (goto-char (point-max)))
      (insert (concat string " (" (current-time-string) ")\n"))
      (save-buffer)
      (when exit
          (bury-buffer))))
  "")                                   ;for use within concat

(defun etask-is-milestone-p(task)
  "Return t if TASK is in fact a milestone rather than a task, nil
otherwise.

A milestone is a task whose planned effort is zero."
  (= (etask-db-get task etask-db-attr-peffort) 0))

(defun etask-num-plural-s(num)
  "Return t if NUM requires plural, nil otherwise."
  (not (= num 1)))

(defun etask-get-alignment-string(msgid1 msgid2)
  "Return string containing SPCs needed to align MSGID1 and MSGID2."
  (make-string 
   (abs (- (length (etask-lang-msg msgid1 etask-language))
           (length (etask-lang-msg msgid2 etask-language))))
   ? ))

(defun etask-time-is-legal-p(time)
  "Return true if TIME is legal, nil otherwise."
  (when (and (listp time) (= (length time) 3))
    (let ((hr (car time))
          (min (car (cdr time)))
          (sec (car (cdr (cdr time)))))
      (and (natnump hr) (natnump min) (natnump sec)
           (or (and (< hr 24) (< min 60) (< sec 60))
               (and (= hr 24) (= min 0) (= sec 0)))))))

(defun etask-calendardate-is-legal-p(date)
  "Return true if DATE is legal, nil otherwise."
  (and (listp date)
       (= (length date) 3)
       (natnump (car date))
       (natnump (nth 1 date))
       (natnump (nth 2 date))
       (calendar-date-is-legal-p date)
       (>= (extract-calendar-year date) etask-earliest-taskstart-year)))

(defun etask-get-daylen()
  "Return length of a Gantt chart day in characters."
  (cond ((etask-todo-unit-is-hr-p)
         (* (etask-todo-get-hrnum) (etask-todo-get-hrwidth)))
        (t
         (let* ((chartstart (etask-state-get etask-stateid-chartstart))
                (chartend (etask-state-get etask-stateid-chartend))
                (maxbarlen (etask-state-get etask-stateid-maxbarlen))
                (zoom (etask-state-get etask-stateid-zoomfactor))
                (daylen
                 (if (and (etask-calendardate-is-legal-p chartstart)
                          (etask-calendardate-is-legal-p chartend))
                     (etask-simplify-number
                      (/ maxbarlen
                         (float 
                          (1+ (etask-days-between chartstart chartend)))))
                   0)))
           (if (> daylen 1) (floor daylen) daylen)))))

(defun etask-project-date-p(date)
  "Return true if DATE is within the project time frame or nil
otherwise."
  (let* ((tasklist (etask-cat-get-current-elts))
         (projbegin (etask-calculate-earliestbegin tasklist))
         (projend (etask-calculate-lastend tasklist)))
    (and (etask-calendardate-is-legal-p date)
         (or
          (and 
           (calendar-date-compare (list projbegin) (list date))
           (calendar-date-compare (list date) (list projend)))
          (= (etask-days-between projbegin date) 0)
          (= (etask-days-between date projend) 0)))))

(defun etask-projdays-in-year(projbegin projend year)
  "Return the number of project days in YEAR.

Holidays are included.  PROJBEGIN and PROJEND are dates and define the
project time frame."
  (let ((yearbegin (extract-calendar-year projbegin))
        (yearend (extract-calendar-year projend))
        (days))
    (cond ((and (= yearbegin year)
                (= year yearend))
           (setq days
                 (1+ (etask-days-between projbegin projend))))
          ((and (< yearbegin year)
                (< year yearend))
           (if (calendar-leap-year-p year)
               (setq days 366)
             (setq days 365)))
          ((and (= yearbegin year)
                (< year yearend))
           (setq days
                 (1+ (etask-days-between projbegin (list 12 31 year)))))
          ((and (< yearbegin year)
                (= year yearend))
           (setq days
                 (1+ (etask-days-between (list 1 1 year) projend))))
          (t
           0))
    days))

(defun etask-projdays-in-month(num month year projbegin projend total)
  "Return the number of project days including holidays of the NUMth
project MONTH in YEAR.

TOTAL is the number of different months including at least one project
day.  PROJBEGIN and PROJEND are dates and define the project time
frame."
  (cond ((and (= num total)
              (> total 1))
         (extract-calendar-day projend))
        ((and (= num 1)
              (= total 1))
         (1+ (etask-days-between projbegin projend)))
        ((and (= num 1)
              (> total 1))
         (1+
          (- (calendar-last-day-of-month month year) 
             (extract-calendar-day projbegin))))
        (t
         (calendar-last-day-of-month month year))))

(defun etask-toggle-attribute(func ctrl1 ctrl2 condition &optional alltasks)
  "Toggle a task attribute specified by CTRL for one or more tasks if
CONDITION evaluates to t.

Normally this function calls FUNC for the current task if CONDITION is
t for that task.  But if there are marked tasks FUNC is applied to all
marked tasks whose CONDITION is t instead.  However, if optional
ALLTASKS is non-nil, then FUNC is applied to all tasks whose CONDITION
is t, no matter if there are marked tasks or not."
  (let* ((pos (point))
         (tasklist (etask-cat-get-current-elementlist))
         (markedtasks (etask-cat-get-marked-elements))
         (toggletasks
          (cond (alltasks tasklist)
                (markedtasks markedtasks)
                (t
                 (list (etask-cat-get-current-element))))))
    (while toggletasks
      (when (funcall condition (car toggletasks))
        (etask-cat-store-element
         (apply func (car toggletasks) ctrl1 ctrl2 nil)
         (etask-cat-get-elementindex (car toggletasks))))
      (setq toggletasks (cdr toggletasks)))
    (etask-cat-show-elements)
    (goto-char pos)))

(defun etask-postpone-begindate-opvalid-p(task num &optional businessp)
  "Return true if a TASK's begin date can be postponed by at least NUM
days.  If optional BUSINESSP is non-nil business days are used instead of
calendar days."
  (or
   (etask-is-milestone-p task)
   (calendar-date-compare
    (if businessp
        (list 
         (etask-add-businessdays-to-date
          (etask-db-get task etask-db-attr-taskbegin)
          num))
      (list 
       (etask-add-days-to-date
        (etask-db-get task etask-db-attr-taskbegin)
        num)))
    (list 
     (etask-db-get task etask-db-attr-taskend)))
   (calendar-date-equal
    (if businessp
        (etask-add-businessdays-to-date
         (etask-db-get task etask-db-attr-taskbegin)
         num)
      (etask-add-days-to-date
       (etask-db-get task etask-db-attr-taskbegin)
       num))
    (etask-db-get task etask-db-attr-taskend))))

(defun etask-forward-duedate-opvalid-p(task num &optional businessp)
  "Return true if TASK's duedate can be forwarded by at least NUM
days.  If optional BUSINESSP is non-nil business days are used instead
of calendar days."
  (or
   (etask-is-milestone-p task)
   (calendar-date-compare
    (list 
     (etask-db-get task etask-db-attr-taskbegin))
    (if businessp
        (list 
         (etask-add-businessdays-to-date
          (etask-db-get task etask-db-attr-taskend)
          (- 0 num)))
      (list 
       (etask-add-days-to-date
        (etask-db-get task etask-db-attr-taskend)
        (- 0 num)))))
   (calendar-date-equal
    (etask-db-get task etask-db-attr-taskbegin)
    (if businessp
        (etask-add-businessdays-to-date
         (etask-db-get task etask-db-attr-taskend)
         (- 0 num))
      (etask-add-days-to-date
       (etask-db-get task etask-db-attr-taskend)
       (- 0 num))))))

(defun etask-postpone-begindate-errormsg(businessp)
  "Return error string for function `etask-postpone-begindate'.
BUSINESSP indicates if business days are used by the caller."
  (if businessp
      (etask-lang-msg 1002 etask-language)
    (etask-lang-msg 1001 etask-language)))

(defun etask-forward-duedate-errormsg(businessp)
  "Return error string for function `etask-forward-duedate'.

BUSINESSP indicates if business days are used by the caller."
  (if businessp
      (etask-lang-msg 1002 etask-language)
    (etask-lang-msg 1001 etask-language)))

(defun etask-shorten-string(longstring &optional max)
  "Return LONGSTRING shortened to
`etask-normal-taskname-len-minibuf' or MAX.

If string LONGSTRING is longer than
`etask-normal-taskname-len-minibuf' it is shortened. For example,
'A very looooooooooooooooong word' becomes something similar to 'A
very looo..'.

Optional MAX overrides `etask-normal-taskname-len-minibuf'."
  (let ((maxlen
         (if (natnump max)
             max
           etask-normal-taskname-len-minibuf)))
    (if (and (stringp longstring)
             (> (length longstring) maxlen))
        (concat (substring longstring 
                           0 
                           (- maxlen (length "..")))
                "..")
      longstring)))

(defun etask-calculate-earliestbegin(tasklist)
  "Return the project begin date.  The project begin date is the
earliest begin date of all tasks in TASKLIST."
  (if tasklist
      (let ((earliestbegin etask-latest-catdate)
            date)
        (while tasklist
          (setq date (etask-db-get (car tasklist) etask-db-attr-taskbegin))
          (when (calendar-date-compare (list date) (list earliestbegin))
            (setq earliestbegin date))
          (setq tasklist (cdr tasklist)))
        ;; (etask-state-set etask-stateid-chartstart earliestbegin)
        earliestbegin)
    (etask-state-set etask-stateid-chartstart etask-earliest-catdate)
    etask-earliest-catdate))

(defun etask-calculate-lastend(tasklist)
  "Return the project end date.  The project end date is the latest
end date of all tasks in TASKLIST."
  (if tasklist
      (let ((lastend etask-earliest-catdate)
            date)
        (while tasklist
          (setq date (etask-db-get (car tasklist) etask-db-attr-taskend))
          (when (calendar-date-compare (list lastend) (list date))
            (setq lastend date))
          (setq tasklist (cdr tasklist)))
        ;; (etask-state-set etask-stateid-chartend lastend)
        lastend)
    (etask-state-set etask-stateid-chartend etask-latest-catdate)
    etask-latest-catdate))

(defun etask-add-days-to-date(date days)
  "Return date DATE + DAYS.  If DAYS is negative the return value is
DATE - DAYS."
  (if (etask-calendardate-is-legal-p date)
      (let ((days
             (+ (timezone-absolute-from-gregorian 
                 (extract-calendar-month date)
                 (extract-calendar-day date)
                 (extract-calendar-year date))
                days)))
        (calendar-gregorian-from-absolute days))
    (list 1 1 etask-earliest-taskstart-year)))

(defun etask-add-weeks-to-date(date weeks)
  "Return date DATE + WEEKS.  If WEEKS is negative the return value is
DATE - WEEKS."
  (if (integerp weeks)
      (etask-add-days-to-date date (* weeks 7))
    (if (etask-calendardate-is-legal-p date)
        date
      (list 1 1 etask-earliest-taskstart-year))))

(defun etask-add-months-to-date(date months)
  "Return date DATE + MONTHS.  If MONTHS is negative the return value
is DATE - MONTH."
  (if (and (etask-calendardate-is-legal-p date)
           (integerp months))
      (let ((month (extract-calendar-month date))
            (day (extract-calendar-day date))
            (year (extract-calendar-year date)))
        (increment-calendar-month month year months)
        (let ((last (calendar-last-day-of-month month year)))
          (if (< last day)
              (setq day last)))
        (list month day year))
    (if (etask-calendardate-is-legal-p date)
        date
      (list 1 1 etask-earliest-taskstart-year))))

(defun etask-write-element-to-forward(task str businessp)
  "Store TASK that is to forward according to STR.  Examples for str:
3d, 5w, 8m for days, weeks, months, accordingly.  If BUSINESSP is
non-nil and STR indicates days then only business days count."
  (etask-write-element-to-postpone task str businessp 'fwd))

(defun etask-write-element-to-postpone(task str businessp &optional fwd)
  "Store TASK that is to postpone according to STR.  Examples for str:
3d, 5w, 8m for days, weeks, months, accordingly.  If BUSINESSP is
non-nil and STR indicates days then only business days count.  If
optional FWD is non-nil, TASK is to forward according to STR."
  (let ((oldbegin (etask-db-get task etask-db-attr-taskbegin))
        (oldend (etask-db-get task etask-db-attr-taskend))
        (currix (etask-cat-get-elementindex task))
        newbegin newend)
    (cond ((or (string-match etask-wholenumber-regexp str)
               (string-match etask-effort-d-regexp str))
           (let ((pdays (etask-extract-effort-raw
                         (if (string-match etask-wholenumber-regexp str)
                             (concat str "d")
                           str))))
             (when fwd
               (setq pdays (- 0 pdays)))
             (setq newbegin (if businessp
                                (etask-add-businessdays-to-date oldbegin pdays)
                              (etask-add-days-to-date oldbegin pdays)))
             (setq newend (if businessp
                              (etask-add-businessdays-to-date oldend pdays)
                            (etask-add-days-to-date oldend pdays)))))
          ((string-match etask-effort-w-regexp str)
           (let ((pweeks (etask-extract-effort-raw str)))
             (when fwd
               (setq pweeks (- 0 pweeks)))
             (setq newbegin (etask-add-weeks-to-date oldbegin pweeks))
             (setq newend (etask-add-weeks-to-date oldend pweeks))))
          ((string-match etask-effort-m-regexp str)
           (let ((pmonths (etask-extract-effort-raw str)))
             (when fwd
               (setq pmonths (- 0 pmonths)))
             (setq newbegin (etask-add-months-to-date oldbegin pmonths))
             (setq newend (etask-add-months-to-date oldend pmonths)))))
    (etask-cat-store-element 
     (etask-db-set
      (etask-db-set task etask-db-attr-taskend newend)
      etask-db-attr-taskbegin
      newbegin)
     currix)))

(defun etask-write-element-to-fwd-date(task begp str businessp)
  "Store TASK that's - when BEGP is true - begin or - when BEGP is
false - end date is to forward according to STR.  Examples for str:
3d, 5w, 8m for days, weeks, months, accordingly.  If BUSINESSP is
non-nil and STR indicates days then only business days count."
  (if (etask-is-milestone-p task)
      (etask-write-element-to-forward task str businessp)
    (etask-write-element-to-postp-date task begp str businessp 'fwd)))

(defun etask-write-element-to-postp-date(task begp str businessp &optional fwd)
  "Store TASK that's - when BEGP is true - begin or - when BEGP is
false - end date is to postpone according to STR.  Examples for str:
3d, 5w, 8m for days, weeks, months, accordingly.  If BUSINESSP is
non-nil and STR indicates days then only business days count.  If
optional FWD is non-nil, the date is to forward according to STR."
  (let ((olddate (if begp 
                     (etask-db-get task etask-db-attr-taskbegin)
                   (etask-db-get task etask-db-attr-taskend)))
        (newdate))
    (cond ((etask-is-milestone-p task)
           (etask-write-element-to-postpone task str businessp))
          ((or (string-match etask-wholenumber-regexp str)
               (string-match etask-effort-d-regexp str))
           (let ((pdays (etask-extract-effort-raw
                         (if (string-match etask-wholenumber-regexp str)
                             (concat str "d")
                           str))))
             (when fwd
               (setq pdays (- 0 pdays)))
             (setq newdate (if businessp
                               (etask-add-businessdays-to-date olddate pdays)
                             (etask-add-days-to-date olddate pdays)))))
          ((string-match etask-effort-w-regexp str)
           (let ((pweeks (etask-extract-effort-raw str)))
             (when fwd
               (setq pweeks (- 0 pweeks)))
             (setq newdate (etask-add-weeks-to-date olddate pweeks))))
          ((string-match etask-effort-m-regexp str)
           (let ((pmonths (etask-extract-effort-raw str)))
             (when fwd
               (setq pmonths (- 0 pmonths)))
             (setq newdate (etask-add-months-to-date olddate pmonths)))))
    (when (not (etask-is-milestone-p task))
      (let ((currix (etask-cat-get-current-elementindex)))
        (if begp
            (etask-cat-store-element 
             (etask-db-set task etask-db-attr-taskbegin newdate) currix)
          (etask-cat-store-element 
           (etask-db-set task etask-db-attr-taskend newdate) currix))))))

(defun etask-days-till-business-day(date)
  "Return number of days from DATE till next business day."
  (if (etask-calendardate-is-legal-p date)
      (let ((days 0))
        (while (not (etask-businessday-p date))
          (setq days (1+ days))
          (setq date (etask-add-days-to-date date 1)))
        days)
    0))

(defun etask-days-till-business-day-before(date)
  "Return number of days from DATE till last business day."
  (if (etask-calendardate-is-legal-p date)
      (let ((days 0))
        (while (not (etask-businessday-p date))
          (setq days (1+ days))
          (setq date (etask-add-days-to-date date -1)))
        days)
    0))

(defun etask-businessday-p(date)
  "Return t if date is a business day, nil otherwise."
  (when (etask-calendardate-is-legal-p date)
      (cond ((and (check-calendar-holidays date)
                  (string-match etask-holiday-regexp
                                (car (check-calendar-holidays date))))
             nil)
            ((or (= (calendar-day-of-week date) 6) ;Saturday
                 (= (calendar-day-of-week date) 0)) ;Sunday
             nil)
            (t
             t))))

(defun etask-add-businessdays-to-date(date days)
  "Return date DATE + DAYS business days.  If DAYS is negative the
return value is DATE - DAYS."
  (if (and (etask-calendardate-is-legal-p date)
           (integerp days))
      (let ((currentday 0)
            (daysadded 0)
            (bizdaysadded 0))
        (while (< bizdaysadded (abs days))
          (setq daysadded 
                (if (> days 0)
                    (1+ daysadded)
                  (1- daysadded)))
          (when (etask-businessday-p (etask-add-days-to-date date daysadded))
            (setq bizdaysadded (1+ bizdaysadded))))
        (etask-add-days-to-date date daysadded))
    (if (etask-calendardate-is-legal-p date)
        date
      (list 1 1 etask-earliest-taskstart-year))))

(defun etask-days-between(date1 date2)
  "Calculate the number of days between DATE1 and DATE2.  The order of
variables doesn't matter."
  (if (and (etask-calendardate-is-legal-p date1)
           (etask-calendardate-is-legal-p date2))
      (let ((days1 
             (timezone-absolute-from-gregorian 
              (car date1)
              (car (cdr date1))
              (car(cdr(cdr date1)))))
            (days2 
             (timezone-absolute-from-gregorian 
              (car date2) 
              (car (cdr date2))
              (car(cdr(cdr date2))))))
        (abs (- days1 days2)))
    0))

(defun etask-business-days-between(date1 date2)
  "Return the number of business days in interval ]DATE1 DATE2].  The
order of variables doesn't matter."
  (if (and (etask-calendardate-is-legal-p date1)
           (etask-calendardate-is-legal-p date2))
      (let* ((d1 (calendar-absolute-from-gregorian date1))
             (d2 (calendar-absolute-from-gregorian date2))
             (du (max d1 d2))
             (d1 (min d1 d2))
             (bizdays 0)
             (d))
        (while (< d1 du)
          (when (etask-businessday-p (calendar-gregorian-from-absolute d1))
            (setq bizdays (1+ bizdays)))
          (setq d1 (1+ d1)))
        bizdays)
    0))

(defun etask-business-days-inclusive(date1 date2)
  "Return the number of business days in interval [DATE1 DATE2].  The
order of variables doesn't matter.  The interval is inclusive."
  (if (and (etask-calendardate-is-legal-p date1)
           (etask-calendardate-is-legal-p date2))
      (let* ((d1 (calendar-absolute-from-gregorian date1))
             (d2 (calendar-absolute-from-gregorian date2))
             (du (max d1 d2))
             (d1 (min d1 d2))
             (bizdays 0)
             (d))
        (while (<= d1 du)
          (if (etask-businessday-p (calendar-gregorian-from-absolute d1))
              (setq bizdays (1+ bizdays)))
          (setq d1 (1+ d1)))
        bizdays)
    0))

(defun etask-calculate-business-days-string(date1 date2 &optional between)
  "Return a string indicating the number of business days between
DATE1 and DATE2.  If optional BETWEEN is nil, day DATE1 also counts.
Otherwise, if BETWEEN is non-nil, the first day doesn't count."
  (if (and (etask-calendardate-is-legal-p date1)
           (etask-calendardate-is-legal-p date2))
      (let ((bizdays (if between
                         (etask-business-days-between date1 date2)
                       (etask-business-days-inclusive date1 date2))))
        (concat
         (number-to-string bizdays)
         " "
         (if (/= bizdays 1)
             (etask-lang-msg 705 etask-language)
           (etask-lang-msg 721 etask-language))))
    ""))

(defun etask-calculate-fte-string(task)
  "Return a string indicating the full time equivalence number of
TASK."
  (when (listp task)
    (let* ((plannedhours (etask-db-get task etask-db-attr-peffort))
           (planneddays (/ (float plannedhours) etask-workinghours-per-day))
           (bizdays (etask-business-days-inclusive
                     (etask-db-get task etask-db-attr-taskbegin)
                     (etask-db-get task etask-db-attr-taskend)))
           (fte (if (> bizdays 0)
                    (etask-simplify-number
                     (/ planneddays bizdays))
                  0)))
      (concat
       (number-to-string fte)
       " FTE"
       (when (/= fte 1)
           "s")))))

(defun etask-calculate-tasktime-expended(task)
  "Return portion of tasktime that is already in the past.  For
example, if 20% of the planned time is already expended the function
returns 0.2."
  (let* ((start (etask-db-get task etask-db-attr-taskbegin))
         (end (etask-db-get task etask-db-attr-taskend))
         (current (calendar-current-date))
         (total (1+ (etask-days-between start end)))
         (burnt (cond ((or
                        (and 
                         (calendar-date-compare (list start) (list current))
                         (calendar-date-compare (list current) (list end)))
                        (= (etask-days-between current end) 0))
                       (etask-days-between start current))
                      ((calendar-date-compare (list end) (list current))
                       total)
                      (t
                       0))))
    (/ (float burnt) total)))

(defun etask-calculate-timetoplannedend(task)
  "Return the number of days from today (exclusive) to planned end of
TASK.  If TASK is already completed the function returns a negative
number."
  (when (listp task)
    (let ((enddate (etask-db-get task etask-db-attr-taskend))
          (today (calendar-current-date)))
      (if (calendar-date-compare (list today) (list enddate))
          (etask-days-between today enddate)
        (- 0 (etask-days-between today enddate))))))

(defun etask-make-begindatetime(element)
  "Return ELEMENT's begin date and time: \(sec min hr day mon year\)"
  (etask-make-duedatetime element 'begin))

(defun etask-make-duedatetime(element &optional begin)
  "Return ELEMENT's due date and time: \(sec min hr day mon year\).
If optional BEGIN is non-nil, return ELEMENT's begin date and time."
  (let* ((eltdate (if begin
                      (etask-db-get element etask-db-attr-taskbegin)
                    (etask-db-get element etask-db-attr-taskend)))
         (elttime (if begin
                      (etask-db-get element etask-db-attr-taskbegintime)
                    (etask-db-get element etask-db-attr-taskendtime)))
         (sec (car (cdr (cdr elttime))))
         (min (car (cdr elttime)))
         (hr (car elttime))
         (day (extract-calendar-day eltdate))
         (month (extract-calendar-month eltdate))
         (year (extract-calendar-year eltdate)))
    (list
     (if (natnump sec) sec 0)
     (if (natnump min) min 0)
     (if (natnump hr) hr 0)
     day
     month
     year)))

(defun etask-extract-year(date)
  "Extract the year part of DATE.  DATE structure: \(sec min hr day
mon year\)"
  (nth 5 date))

(defun etask-extract-month(date)
  "Extract the month part of DATE.  DATE structure: \(sec min hr day
mon year\)"
  (nth 4 date))

(defun etask-extract-day(date)
  "Extract the day part of DATE.  DATE structure: \(sec min hr day
mon year\)"
  (nth 3 date))

(defun etask-extract-hour(date)
  "Extract the hour part of DATE.  DATE structure: \(sec min hr day
mon year\)"
  (nth 2 date))

(defun etask-extract-min(date)
  "Extract the min part of DATE.  DATE structure: \(sec min hr day
mon year\)"
  (nth 1 date))

(defun etask-extract-sec(date)
  "Extract the second part of DATE.  DATE structure: \(sec min hr day
mon year\)"
  (car date))

(defun etask-extract-date(date)
  "Extract the calendar date part of DATE.  DATE structure: \(sec min
hr day mon year\).  Calendar date structure: \(mon day year\)."
  (list (etask-extract-month date)
        (etask-extract-day date)
        (etask-extract-year date)))

(defun etask-extract-time(date)
  "Extract the EtaskMode time part of DATE.  DATE structure: \(sec min
hr day mon year\).  EtaskMode time structure: \(hr min sec\)."
  (list (etask-extract-hour date)
        (etask-extract-min date)
        (etask-extract-sec date)))

(defun etask-datetime-add-time(date effort)
  "Return DATE + EFFORT, given in hours, as new DATE structure: \(sec
min hr day mon year\)"
  (let* ((year (etask-extract-year date))
         (month (etask-extract-month date))
         (day (etask-extract-day date))
         (hr (etask-extract-hour date))
         (min (etask-extract-min date))
         (sec (etask-extract-sec date))
         (daystoadd (floor (/ effort 24)))
         (hrstoadd (floor (- effort (* daystoadd 24))))
         (mintoadd 
          (floor (* (etask-simplify-number
                     (- effort 
                        (+ hrstoadd 
                           (* daystoadd 24))))
                    60)))
         (sectoadd
          (floor (* (etask-simplify-number
                     (- effort 
                        (+ (/ mintoadd 60.0) 
                           hrstoadd 
                           (* daystoadd 24))))
                    60 60)))
         (newdate)
         (newsec (% (+ sec sectoadd) 60))
         (newmin (% (+ min mintoadd (/ (+ sec sectoadd) 60)) 60))
         (newhr (% (+ hr hrstoadd (/ (+ min mintoadd) 60)) 24)))
    (setq daystoadd (+ (/ (+ hr hrstoadd) 24)
                       daystoadd))
    (when (and (> daystoadd 0) (= newhr 0) (= newmin 0) (= newsec 0))
      (setq daystoadd (1- daystoadd))
      (setq newhr 24))
    (setq newdate
          (etask-add-days-to-date (list month day year) daystoadd))
    (list newsec newmin newhr 
          (extract-calendar-day newdate)
          (extract-calendar-month newdate)
          (extract-calendar-year newdate))))

(defun etask-datetime-subtract-time(date effort)
  "Return DATE - EFFORT, given in hours, as new DATE structure: \(sec
min hr day mon year\)"
  (let* ((year (etask-extract-year date))
         (month (etask-extract-month date))
         (day (etask-extract-day date))
         (hr (etask-extract-hour date))
         (min (etask-extract-min date))
         (sec (etask-extract-sec date))
         (daystosub (floor (/ effort 24)))
         (hrstosub (floor (- effort (* daystosub 24))))
         (mintosub 
          (floor (* (etask-simplify-number
                     (- effort 
                        (+ hrstosub 
                           (* daystosub 24))))
                    60)))
         (sectosub
          (floor (* (etask-simplify-number
                     (- effort 
                        (+ (/ mintosub 60.0) 
                           hrstosub 
                           (* daystosub 24))))
                    60 60)))
         (newdate)
         (newsec (if (>= sec sectosub)
                     (- sec sectosub)
                   (- 60 (- sectosub sec))))
         (newmin (if (>= min mintosub)
                     (- min (+ mintosub
                               (if (>= sec sectosub) 0 1)))
                   (- 60 (- (+ mintosub 
                               (if (>= sec sectosub) 0 1))
                            min))))
         (newhr (if (>= hr hrstosub)
                    (- hr (+ hrstosub
                             (if (>= min mintosub) 0 1)))
                  (- 24 (- (+ hrstosub 
                              (if (>= min mintosub) 0 1))
                           hr)))))
    (setq daystosub (if (< hr hrstosub)
                        (1+ daystosub)
                      daystosub))
    (setq newdate
          (etask-add-days-to-date (list month day year) (- 0 daystosub)))
    (list newsec newmin newhr 
          (extract-calendar-day newdate)
          (extract-calendar-month newdate)
          (extract-calendar-year newdate))))

(defun etask-datetime-less-p(d1 d2)
  "Return true if date D1 is before date D2.  DATE structure: \(sec
min hr day mon year\)"
  (cond ((etask-date-less-p d1 d2) t)
        ((and (etask-date-equal-p d1 d2) (etask-time-less-p d1 d2)) t)
        (t nil)))
  
(defun etask-date-less-p(d1 d2)
  "Return true if date D1's date part is before date D2's date part,
nil otherwise.  DATE structure: \(sec min hr day mon year\)"
  (or (< (etask-extract-year d1) (etask-extract-year d2))
      (and (= (etask-extract-year d1) (etask-extract-year d2))
           (< (etask-extract-month d1) (etask-extract-month d2)))
      (and (= (etask-extract-year d1) (etask-extract-year d2))
           (= (etask-extract-month d1) (etask-extract-month d2))
           (< (etask-extract-day d1) (etask-extract-day d2)))))

(defun etask-time-less-p(d1 d2)
  "Return true if date D1's time is before date D2's time, nil
otherwise.  DATE structure: \(sec min hr day mon year\)"
  (or (< (etask-extract-hour d1) (etask-extract-hour d2))
      (and (= (etask-extract-hour d1) (etask-extract-hour d2))
           (< (etask-extract-min d1) (etask-extract-min d2)))
      (and (= (etask-extract-hour d1) (etask-extract-hour d2))
           (= (etask-extract-min d1) (etask-extract-min d2))
           (< (etask-extract-sec d1) (etask-extract-sec d2)))))

(defun etask-datetime-equal-p(d1 d2)
  "Return true if date D1 is equal date D2.  DATE structure: \(sec min
hr day mon year\)"
  (and (etask-date-equal-p d1 d2) (etask-time-equal-p d1 d2)))

(defun etask-date-equal-p(d1 d2)
  "Return true if date D1's date part is equal to date D2's date part,
nil otherwise.  DATE structure: \(sec min hr day mon year\)"
  (and (= (etask-extract-year d1) (etask-extract-year d2))
       (= (etask-extract-month d1) (etask-extract-month d2))
       (= (etask-extract-day d1) (etask-extract-day d2))))

(defun etask-time-equal-p(d1 d2)
  "Return true if D1's time part is equal to D2's time part, nil
otherwise.  DATE structure: \(sec min hr day mon year\)"
  (and (= (etask-extract-hour d1) (etask-extract-hour d2))
       (= (etask-extract-min d1) (etask-extract-min d2))
       (= (etask-extract-sec d1) (etask-extract-sec d2))))

(defun etask-current-line()
  "Return the current buffer line number starting with 1."
  (save-excursion
    (save-restriction
      (widen)
      (forward-line 0)
      (1+ (count-lines 1 (point))))))

(defun etask-get-sublist(list from to)
  "Return sublist of LIST with elements FROM to TO.  FROM=1 refers to
the first LIST element.  If TO exceeds the list length then the rest
of the list is returned."
  (when (listp list)
    (let ((len (safe-length list)))
      (when (> to len)
        (setq to len))
      (when (and (natnump from) (natnump to) 
                 (> from 0) (> to 0) (<= from to) (<= from len))
        (butlast (nthcdr (1- from) list) (- len to))))))

(defun etask-apply-face(string stringface)
  "Apply face STRINGFACE to string STRING."
  (let ((newstring (concat string)))
    (etask-propertize string 'face stringface)))

(defun etask-longest-string-in-list(list)
  "Return the length of the longest string in the list of strings
LIST."
  (cond (list
         (max (length (car list)) (etask-longest-string-in-list(cdr list))))
        (t
         0)))

(defun etask-is-task-completed-p(task)
  "Return t if task TASK is already completed, nil otherwise.  Note
that when TASK's planned effort is 0 it is on time but not completed.
Because its effort is not even planned, the task cannot be completed."
  (let* ((peffort (etask-db-get task etask-db-attr-peffort))
         (ret
          (if (= peffort 0)
              nil
            (cond ((= 1.0 (/ (float 
                              (etask-db-get task etask-db-attr-eeffort))
                             peffort))
                   t)
                  (t
                   nil)))))
    ret))

(defun etask-is-task-behind-schedule-p(task)
  "Return true if task TASK is behind schedule, nil otherwise."
  (let ((tracking (etask-db-get task etask-db-attr-tracking)))
    (cond ((string= tracking "s-shape-65")
           (if (string-match 
                (etask-lang-msg 710 etask-language)
                (etask-calculate-taskstatus-s-shape-65 task))
               t
             nil))
          ((string= tracking "s-shape-70")
           (if (string-match 
                (etask-lang-msg 710 etask-language)
                (etask-calculate-taskstatus-s-shape-70 task))
               t
             nil))
          (t
           (if (string-match 
                (etask-lang-msg 710 etask-language)
                (etask-calculate-taskstatus-linear task))
               t
             nil)))))

(defun etask-calculate-percentcompleted(task)
  "Return TASK's percent completed value or -1 if plannedeffort = 0."
  (let ((expendedeffort (etask-db-get task etask-db-attr-eeffort))
        (plannedeffort (etask-db-get task etask-db-attr-peffort)))
    (if (= plannedeffort 0)
        -1
      (round
       (* 100 
          (/ (float expendedeffort) plannedeffort))))))

(defun etask-calculate-taskstatus-s-shape-70(task)
  "Return status of TASK according to S-Shape-70.

The function returns the string 'ON TIME' if progress is according to or better
than the following s-shape-curve:

 time  |  sample of completion requirements
-------|-------------------------------------
  0%   |              0%
 10%   |              5%
 20%   |             15%
 30%   |             30%
 40%   |             55%
 50%   |             70%
 60%   |             82%
 70%   |             90%
 80%   |             95%
 90%   |             98%
100%   |            100%

Otherwise it returns the string 'BEHIND SCHEDULE (target: y%, actual: x%)' or
'NOT STARTED' or 'COMPLETED'."
  (let* ((percentcompleted (etask-calculate-percentcompleted task))
         (plannedeffort (etask-db-get task etask-db-attr-peffort))
         (burnt (etask-calculate-tasktime-expended task))
         (scaledburnt (* 10 burnt))
         target)
    
    (cond ((or
            (= burnt 0)
            (= plannedeffort 0))
           (setq target 0))

          ;; [0..2[  5(x-0) + 2.5(x-0)(x-1)
          ((< burnt 0.2)
           (setq target (+ (* 5 scaledburnt)
                           (* 2.5 scaledburnt (- scaledburnt 1)))))

          ;; [2..4[  15 + 15(x-2) + 5(x-2)(x-3)
          ((and (>= burnt 0.2) (< burnt 0.4))
           (setq target (+ 15
                           (* 15 (- scaledburnt 2))
                           (* 5 (- scaledburnt 2) (- scaledburnt 3)))))

          ;; [4..6[  55 + 15(x-4) - 1.5(x-4)(x-5)
          ((and (>= burnt 0.4) (< burnt 0.6)) 
           (setq target (+ 55
                           (* 15 (- scaledburnt 4))
                           (* -1.5 (- scaledburnt 4) (- scaledburnt 5)))))

          ;; [6..8[  82 + 8(x-6) - 1.5(x-6)(x-7)
          ((and (>= burnt 0.6) (< burnt 0.8))
           (setq target (+ 82
                           (* 8 (- scaledburnt 6))
                           (* -1.5 (- scaledburnt 6) (- scaledburnt 7)))))

          ;; [8..10] 95 + 3(x-8) - 0.5(x-8)(x-9)
          (t
           (setq target (+ 95
                           (* 3 (- scaledburnt 8))
                           (* -0.5 (- scaledburnt 8) (- scaledburnt 9))))))
    (cond ((and (> percentcompleted 0)
                (< percentcompleted 100)
                (>= percentcompleted target)
                (>= target 0))
           (concat
            (format "%s ("
                    (etask-apply-face 
                     (etask-lang-msg 711 etask-language) ;on time
                     'etask-face-status-ok))
            (etask-lang-msg 730 etask-language)
            (format ": %d%%, " target)
            (etask-lang-msg 731 etask-language)
            (format ": %d%%) " percentcompleted)))
          ((= plannedeffort 0)
           (format "%s" 
                   (etask-apply-face 
                    (etask-lang-msg 712 etask-language) ;milestone
                    'etask-face-status-ok)))
          ((= percentcompleted 100.0)
           (format "%s" 
                   (etask-apply-face 
                    (etask-lang-msg 713 etask-language) ;completed
                    'etask-face-status-ok)))
          ((= percentcompleted 0.0)
           (format "%s" 
                   (etask-apply-face
                    (etask-lang-msg 714 etask-language) ;not started
                    'etask-face-status-ok)))
          (t
           (concat
            (format "%s ("
                    (etask-apply-face 
                     (etask-lang-msg 710 etask-language) ;behind schedule
                     'etask-face-status-behindschedule))
            (etask-lang-msg 730 etask-language)
            (format ": %d%%, " target)
            (etask-lang-msg 731 etask-language)
            (format ": %d%%) " percentcompleted))))))

(defun etask-calculate-taskstatus-s-shape-65(task)
  "Return status of TASK according to S-Shape-65.

The function returns the string 'ON TIME' if progress is according to or better
than the following s-shape-curve:

 time  |  sample of completion requirements
-------|-------------------------------------
  0%   |              0%
 10%   |              5%
 20%   |             15%
 30%   |             30%
 40%   |             50%
 50%   |             65%
 60%   |             77%
 70%   |             85%
 80%   |             91%
 90%   |             96%
100%   |            100%

Otherwise it returns the string 'BEHIND SCHEDULE (target: y%, actual: x%)' or
'NOT STARTED' or 'COMPLETED'."
  (let* ((percentcompleted (etask-calculate-percentcompleted task))
         (plannedeffort (etask-db-get task etask-db-attr-peffort))
         (burnt (etask-calculate-tasktime-expended task))
         (scaledburnt (* 10 burnt))
         target)

    (cond ((or
            (= burnt 0)
            (= plannedeffort 0))
           (setq target 0))

          ;; [0..2[  5(x-0) + 2.5(x-0)(x-1)
          ((< burnt 0.2)
           (setq target (- (* 5 scaledburnt)
                           (* 2.5 scaledburnt (- scaledburnt 1)))))

          ;; [2..4[  15 + 15(x-2) + 2.5(x-2)(x-3)
          ((and (>= burnt 0.2) (< burnt 0.4)) 
           (setq target (+ 15
                           (* 15 (- scaledburnt 2))
                           (* 2.5 (- scaledburnt 2) (- scaledburnt 3)))))

          ;; [4..6[  50 + 15(x-4) - 1.5(x-4)(x-5)
          ((and (>= burnt 0.4) (< burnt 0.6))
           (setq target (+ 50
                           (* 15 (- scaledburnt 4))
                           (* -1.5 (- scaledburnt 4) (- scaledburnt 5)))))

          ;; [6..8[  77 + 8(x-6) - 1(x-6)(x-7)
          ((and (>= burnt 0.6) (< burnt 0.8))
           (setq target (+ 77
                           (* 8 (- scaledburnt 6))
                           (* -1 (- scaledburnt 6) (- scaledburnt 7)))))

          ;; [8..10] 91 + 5*(x-8) -0.5(x-8)(x-9)
          (t
           (setq target 
                 (round
                  (+ 91
                     (* 5 (- scaledburnt 8))
                     (* -0.5 (- scaledburnt 8) (- scaledburnt 9)))))))
    (cond ((and (> percentcompleted 0)
                (< percentcompleted 100)
                (>= percentcompleted target)
                (>= target 0))
           (concat
            (format "%s ("
                    (etask-apply-face 
                     (etask-lang-msg 711 etask-language) ;on time
                     'etask-face-status-ok))
            (etask-lang-msg 730 etask-language)
            (format ": %d%%, " target)
            (etask-lang-msg 731 etask-language)
            (format ": %d%%) " percentcompleted)))
          ((= plannedeffort 0)
           (format "%s" 
                   (etask-apply-face    ;milestone
                    (etask-lang-msg 712 etask-language)
                    'etask-face-status-ok)))
          ((= percentcompleted 100.0)
           (format "%s" 
                   (etask-apply-face    ;completed
                    (etask-lang-msg 713 etask-language)
                    'etask-face-status-ok)))
          ((= percentcompleted 0.0)
           (format "%s" 
                   (etask-apply-face    ;not started
                    (etask-lang-msg 714 etask-language)
                    'etask-face-status-ok)))
          (t
           (concat
            (format "%s ("
                    (etask-apply-face 
                     (etask-lang-msg 710 etask-language) ;behind schedule
                     'etask-face-status-behindschedule))
            (etask-lang-msg 730 etask-language)
            (format ": %d%%, " target)
            (etask-lang-msg 731 etask-language)
            (format ": %d%%) " percentcompleted))))))

(defun etask-calculate-taskstatus-linear(task)
  "Return status of TASK according to linear progress model.

Calculates the task status and returns the string 'ON TIME' if
progress is according to or better than the following linear
requirements:

 time  |  sample of completion requirements
-------|-------------------------------------
  0%   |              0%
 10%   |             10%
 20%   |             20%
 30%   |             30%
 40%   |             40%
 50%   |             50%
 60%   |             60%
 70%   |             70%
 80%   |             80%
 90%   |             90%
100%   |            100%

Otherwise it returns the string 'BEHIND SCHEDULE (target: y%, actual: x%)' or
'NOT STARTED' or 'COMPLETED'."
  (let* ((percentcompleted (etask-calculate-percentcompleted task))
         (plannedeffort (etask-db-get task etask-db-attr-peffort))
         (burnt (etask-calculate-tasktime-expended task))
         (target (if (= plannedeffort 0)
                     0
                   (* 100 burnt))))
    (cond ((and (> percentcompleted 0)
                (< percentcompleted 100)
                (>= percentcompleted target)
                (>= target 0))
           (concat
            (format "%s ("
                    (etask-apply-face 
                     (etask-lang-msg 711 etask-language) ;on time
                     'etask-face-status-ok))
            (etask-lang-msg 730 etask-language)
            (format ": %d%%, " target)
            (etask-lang-msg 731 etask-language)
            (format ": %d%%) " percentcompleted)))
          ((= plannedeffort 0)
           (format "%s" 
                   (etask-apply-face    ;milestone
                    (etask-lang-msg 712 etask-language)
                    'etask-face-status-ok)))
          ((= percentcompleted 100.0)
           (format "%s" (etask-apply-face ;completed
                         (etask-lang-msg 713 etask-language)
                         'etask-face-status-ok)))
          ((= percentcompleted 0.0)
           (format "%s" (etask-apply-face ;not started
                         (etask-lang-msg 714 etask-language)
                         'etask-face-status-ok)))
          (t
           (concat
            (format "%s ("
                    (etask-apply-face 
                     (etask-lang-msg 710 etask-language) ;behind schedule
                     'etask-face-status-behindschedule))
            (etask-lang-msg 730 etask-language)
            (format ": %d%%, " target)
            (etask-lang-msg 731 etask-language)
            (format ": %d%%) " percentcompleted))))))

(defun etask-get-reportfile()
  "Return filename for requested status report."
  (concat
   etask-working-dir
   etask-reports-filename-prefix
   "."
   (number-to-string (extract-calendar-day (calendar-current-date)))
   "."
   (calendar-month-name (extract-calendar-month (calendar-current-date)) 3)
   "."
   (number-to-string (extract-calendar-year (calendar-current-date)))))

(defun etask-insert-report-string(tasklist)
  "Insert status report of TASKLIST in reporting buffer."
  (let ((task))
    (insert (concat (etask-lang-msg 701 etask-language) "\n"))
    (insert (concat (etask-lang-msg 702 etask-language) "\n\n"))
    (insert (current-time-string))
    (while tasklist
       (setq task (car tasklist))
       (insert "\n\n\n\n")
       (insert (etask-cat-show-elementstatus 
                (etask-cat-get-elementindex task) 
                'reporting))
       (setq tasklist (cdr tasklist)))))

(defun etask-simplify-number(num)
  "Truncate or cut NUM if possible.

If NUM is a floating point number with just '0's after the decimal point return
the corresponding integer value. If NUM is a floating point number with more
than 2 digits after the decimal point round to its hundredth decimal place. All
other numbers are returned unchanged."
  (if (numberp num)
      (cond ((string-match 
              "[0-9]+\.0+$" 
              (number-to-string num))
             (truncate num))
            ((string-match 
              "[0-9]+\.[0-9][0-9][0-9]+$" 
              (number-to-string num))
             ;; see round documentation for rounding errors
;;           in XEmacs only (round NUMBER)
;;           (/ (round num 0.01) (float 100)))
             (/ (round (* num 100)) (float 100)))
            (t
             num))))

(defun etask-start-n-days-earlier(days task &optional businessp)
  "Return TASK with its begin date set to DAYS days earlier.  DAYS is
a whole number.  If optional BUSINESSP is non-nil business days are
used instead of days."
  (let ((newstart
         (if businessp
             (etask-add-businessdays-to-date
              (etask-db-get task etask-db-attr-taskbegin)
              (- 0 days))
           (calendar-gregorian-from-absolute
            (- (calendar-absolute-from-gregorian
                (etask-db-get task etask-db-attr-taskbegin))
               days)))))
    (etask-db-set task etask-db-attr-taskbegin newstart)))

(defun etask-start-n-days-later(days task &optional businessp)
  "Return TASK with its begin date set to DAYS days later.  DAYS is a
whole number.  If optional BUSINESSP is non-nil business days are used
instead of days."
  (let* ((newstart
          (if businessp
              (etask-add-businessdays-to-date
               (etask-db-get task etask-db-attr-taskbegin)
               days)
            (calendar-gregorian-from-absolute
             (+ (calendar-absolute-from-gregorian
                 (etask-db-get task etask-db-attr-taskbegin))
                days)))))
    (etask-db-set task etask-db-attr-taskbegin newstart)))

(defun etask-finish-n-days-earlier(days task &optional businessp)
  "Return task TASK with a new end date: DAYS days before current end.
DAYS is a whole number.  If optional BUSINESSP is non-nil business
days are used instead of days."
  (let ((newend
         (if businessp
             (etask-add-businessdays-to-date
              (etask-db-get task etask-db-attr-taskend)
              (- 0 days))
           (calendar-gregorian-from-absolute
            (- (calendar-absolute-from-gregorian
                (etask-db-get task etask-db-attr-taskend))
               days)))))
    (etask-db-set task etask-db-attr-taskend newend)))

(defun etask-finish-n-days-later(days task &optional businessp)
  "Return task TASK with a new end date: DAYS days later.  DAYS is a
whole number.  If optional BUSINESSP is non-nil business days are used
instead of days."
  (let ((newend
         (if businessp
             (etask-add-businessdays-to-date
              (etask-db-get task etask-db-attr-taskend)
              days)
           (calendar-gregorian-from-absolute
            (+ (calendar-absolute-from-gregorian
                (etask-db-get task etask-db-attr-taskend))
               days)))))
    (etask-db-set task etask-db-attr-taskend newend)))

(defun etask-read(prompt acceptable &optional initial default)
  "Return a string read from the minibuffer or DEFAULT if RET is
entered.

Prompt with the string PROMPT and use the function ACCEPTABLE to
decide if entered item is acceptable.

If non-nil, optional third arg INITIAL is a string to insert in the
minibuffer before reading according to `read-from-minibuffer'.  If
non-nil, optional forth arg DEFAULT is the default value according to
`read-from-minibuffer'.

If the user enters just RET then `read-from-minibuffer' returns the
empty string. This function replaces the empty string with the default
value if it is a string.

Written because calendar-read fails when just typing RET and no inital
value is entered -> reason in read-minibuffer, read-from-minibuffer
can handle this."
  (let ((value (read-from-minibuffer prompt initial nil nil nil default)))
    (while (not (funcall acceptable value))
      (setq value (read-from-minibuffer prompt initial nil nil nil default)))
    (if (and (string= value "")
             (stringp default))
        default
      value)))

(defun etask-string-trim(string)
  "Lose leading and trailing whitespace but keep properties."
  (if (string-match "\\`[ \t\n]+" string)
      (setq string (substring string (match-end 0))))
  (if (string-match "[ \t\n]+\\'" string)
      (setq string (substring string 0 (match-beginning 0))))
  string)

(defun etask-assoc-delete-all (key alist)
  "Delete from ALIST all elements whose car is equal to KEY.  Return the
modified alist."
  (let ((tail alist))
    (while tail
      (if (equal (car (car tail)) key)
	  (setq alist (delq (car tail) alist)))
      (setq tail (cdr tail)))
    alist))

(defun etask-states-update()
  "Update selected state data: file > memory"
  (let* ((filemod (etask-get-control-item etask-ctrl-zoommodus))
         (memmod (etask-state-get etask-stateid-zoommodus))
         (modp (or (and (stringp filemod)
                            (not (stringp memmod)))
                       (and (stringp filemod)
                            (stringp memmod)
                            (not (string= filemod memmod)))))
         (zoomfactor (etask-get-control-item etask-ctrl-zoomfactor))
         (memfact (etask-state-get etask-stateid-zoomfactor))
         (factp (or (and (natnump zoomfactor)
                            (not (natnump memfact)))
                       (and (natnump zoomfactor)
                            (natnump memfact)
                            (not (= zoomfactor memfact)))))
         (maxbarlen (etask-get-maxbarlen))
         (maxtasklen (etask-get-maxtasklen)))
    (when modp
      (etask-state-set etask-stateid-zoommodus filemod))
    (when factp
      (etask-state-set etask-stateid-zoomfactor zoomfactor))
    (etask-state-set etask-stateid-maxbarlen maxbarlen)
    (etask-state-set etask-stateid-maxtasklen maxtasklen)))

(defun etask-all-states-update()
  "Update state data: file > memory"
  (etask-states-update)
  (let* ((chstart (etask-get-control-item etask-ctrl-chartbegindate))
         (chend (etask-get-control-item etask-ctrl-chartenddate))
         (chstarthr (etask-get-control-item etask-ctrl-todoview-begintime))
         (chendhr (etask-get-control-item etask-ctrl-todoview-endtime)))
    (etask-state-set etask-stateid-last-flag nil)
    (etask-state-set etask-stateid-businessdays etask-use-bizdays)
    (etask-state-set etask-stateid-chartstart chstart)
    (etask-state-set etask-stateid-chartend chend)
    (etask-state-set etask-stateid-chartstarttime chstarthr)
    (etask-state-set etask-stateid-chartendtime chendhr)))

(defun etask-states-save()
  "Update state data: memory > file"
  (let ((chartstart (etask-state-get etask-stateid-chartstart))
        (chartend (etask-state-get etask-stateid-chartend))
        (chartstarttime (etask-state-get etask-stateid-chartstarttime))
        (chartendtime (etask-state-get etask-stateid-chartendtime)))
  (when chartstart
    (etask-set-control-item etask-ctrl-chartbegindate chartstart))
  (when chartend
    (etask-set-control-item etask-ctrl-chartenddate chartend))
  (when chartstarttime
    (etask-set-control-item etask-ctrl-todoview-begintime chartstarttime))
  (when chartendtime
    (etask-set-control-item etask-ctrl-todoview-endtime chartendtime))))

(defun etask-state-clear(statestr)
  "Remove STATE from EtaskMode state."
  (when (stringp statestr)
    (setq etask-state-var
          (etask-assoc-delete-all statestr etask-state-var))))

(defun etask-state-set(statestr value)
  "Add STATE to EtaskMode state."
  (when (and (stringp statestr) 
             (or (stringp value) 
                 (natnump value)
                 (boolean-p value)
                 (etask-calendardate-is-legal-p value)))
    (etask-state-clear statestr)
    (setq etask-state-var
          (cons (list statestr value) etask-state-var))))

(defun etask-state-get(statestr)
  "Return true if STATE is set, nil otherwise."
  (when (stringp statestr)
    (car (cdr (assoc statestr etask-state-var)))))

(defun etask-state-p(statestr value)
  "Return true if state STATESTR is set to VALUE, nil otherwise."
  (when (stringp statestr)
    (let ((val (etask-state-get statestr)))
      (cond ((listp val)
             (member value val))
            ((and (natnump value) (natnump val))
             (= value val))
            ((and (stringp value) (stringp val))
             (string= value val))
            ((and (boolean-p value) (boolean-p val))
             value)
            (t
             ())))))


;;; Screen Related Functions

(defun etask-get-window-minheight()
  "Return minimum window height to view all data."
  (+ (etask-cat-get-elementnum)
     (if (etask-get-control-item etask-ctrl-onlinehelp 'main)
         etask-lines-except-tasklines
       etask-lines-taskstatus)))

(defun etask-get-window-currheight()
  "Return current window height."
  ;;subtract modeline
  (1- (window-height)))

(defun etask-window-too-small-p()
  "Return true if current window's height must be increased to view all data."
  (< (etask-get-window-currheight) (etask-get-window-minheight)))

(defun etask-fit-window()
  "Fit window to buffer content and print a message if window's height
cannot be increased."
  (etask-fit-window-port)
  (when (etask-window-too-small-p)
    (message "^")))

(defun etask-format-insert(string &optional len form)
  "Insert STRING according to its format string FORM.

STRING may have a face.  If LEN is non-nil, print exactly LEN
characters wide, otherwise print the string.  FORM is the alignment
string - in left, center, right.

Originally written because XEmacs 21.4 patch 6 couldn't do complex
insert operations - lost faces."
  (let* ((orilen (length string))
         (len (if (integerp len) len orilen))
         (str (if (> (length string) len)
                  (substring string 0 len)
                string))
         (numspcbef
          (cond ((and form 
                      (string= form "left"))
                 0)

                ((and form 
                      (string= form "center")
                      (< orilen len))
                 (ceiling (/ (float (- len orilen)) 2)))
                ((and form
                      (string= form "right")
                      (< orilen len))
                 (- len orilen))
                (t
                 0)))
         (numspcaft
          (cond ((and form 
                      (string= form "left")
                      (< orilen len))
                 (- len orilen))
                ((and form 
                      (string= form "center")
                      (< orilen len))
                 (- len numspcbef orilen))
                ((and form
                      (string= form "right"))
                 0)
                (t
                 0))))
    (if (> numspcbef 0)
        (insert (make-string numspcbef ? )))
    (insert str)
    (if (> numspcaft 0)
        (insert (make-string numspcaft ? )))))

(defun etask-generate-chart(elements &optional sublevel maxsublevel)
  "Generate Gantt chart within Emacs for all elements in ELEMENTS.  If
optional SUBLEVEL and MAXSUBLEVEL are provided, include only elements
with sublevel zero to maxsublevel."
  ;; Used variables defined in `etask-cat-show-elements': catid daylen
  (when (and (listp elements) (> (length elements) 0))
    (let* ((len 0)
           (chartstart (etask-state-get etask-stateid-chartstart))
           (chartend (etask-state-get etask-stateid-chartend))
           (maxnamelen (etask-state-get etask-stateid-maxtasklen))
           (sublevel 
            (cond ((and (natnump sublevel) (not (natnump maxsublevel)))
                   sublevel)
                  ((and (natnump sublevel) (natnump maxsublevel))
                   (if (<= sublevel maxsublevel)
                       sublevel
                     -1))
                  ((not (natnump sublevel))
                   0)))
           element subelements infostr)
      (when (and (etask-cat-is-valid-catid-p catid)
                 elements
                 (natnump sublevel))
        (while elements
          (setq element (car elements))
          (setq elementname 
                (etask-db-get (car elements) etask-db-attr-taskname))
          (setq subelements
                (etask-db-get (car elements) etask-db-attr-subtasklist))

          ;; To calculate the real infostring the task name must begin
          ;; at the correct column
          (insert (etask-get-infostring))

          (etask-insert-elementname
           element
           (- maxnamelen
              (length (etask-get-infostring))
              (length (etask-get-subelstring)))
           sublevel)

          ;; Delimiter also needed for infostr calculation
          (etask-insert-delimiter)

          (setq infostr
                (etask-get-infostring
                 (etask-cat-elementnotes-p)
                 subelements
                 (and (natnump sublevel) (natnump maxsublevel)
                      (< sublevel maxsublevel))))
          (forward-line 0)
          (insert infostr)
          (delete-char (length (etask-get-infostring)))
          (etask-goto-delimiter)
          (goto-char (1+ (point)))

          (when (etask-cat-insert-elementbar-p catid element)
            (etask-insert-taskbar 
             element chartstart chartend daylen maxnamelen))
          (insert "\n")
          (when (and (listp subelements) (> (length subelements) 0))
            (while subelements
              (etask-generate-chart
               (list (car subelements))
               (1+ sublevel) 
               (when (natnump maxsublevel) maxsublevel))
              (setq subelements (cdr subelements))))
          (setq elements (cdr elements)))))))

(defun etask-generate-helpshortcut(helpp)
  "Return help shortcut."
  (if helpp
      (concat (etask-lang-msg 780 etask-language) ": X")
    (concat (etask-lang-msg 779 etask-language) ": C-h m")))

(defun etask-generate-item-info()
  "Return item information including name and relative position within
the item's timeframe."
  (let* ((elements (etask-cat-get-current-elts))
         (projbegin (etask-calculate-earliestbegin elements))
         (projend (etask-calculate-lastend elements))
         (chartstart (etask-state-get etask-stateid-chartstart))
         (chartend (etask-state-get etask-stateid-chartend))
         (daysbefore
          (cond ((and (etask-calendardate-is-legal-p projbegin)
                      (etask-calendardate-is-legal-p chartstart)
                      (calendar-date-equal
                       chartstart etask-earliest-catdate))
                 0)
                ((and (etask-calendardate-is-legal-p projbegin)
                      (etask-calendardate-is-legal-p chartstart)
                      (calendar-date-compare 
                       (list projbegin) (list chartstart)))
                 (etask-days-between projbegin chartstart))
                (t
                 0)))
         (daysafter
          (cond ((and (etask-calendardate-is-legal-p projend)
                      (etask-calendardate-is-legal-p chartend)
                      (calendar-date-equal
                       chartend etask-latest-catdate))
                 0)
                ((and (etask-calendardate-is-legal-p projend)
                      (etask-calendardate-is-legal-p chartend)
                      (calendar-date-compare 
                       (list chartend) (list projend)))
                 (etask-days-between chartend projend))
                (t
                 0)))
         (catid (etask-state-get etask-stateid-currcat))
         (catitemname))
    (when (etask-cat-is-valid-catid-p catid)
      (setq catitemname (capitalize (etask-cat-get-catitemname catid)))
      (concat 
       (when (> daysbefore 0)
         (concat 
          (number-to-string daysbefore)
          "d << "))
       "["
       catitemname
       ": "
       (car (cdr (etask-cat-get-current-item)))
       "]"
       (when (> daysafter 0)
         (concat 
          " >> "
          (number-to-string daysafter)
          "d"))))))

(defun etask-get-flagstring()
  "Return empty flag string."
  "  ")

(defun etask-get-infostring(&optional notesp subelements followersp)
  "Return infostring consisting of 2 characters and a following spc.
If optional NOTESP is non-nil, the element has a non-empty notes file.
Optional SUBELEMENTS is a list with all subelements of the current
element.  If optional FOLLOWERSP is non-nil, then the current
\(sub\)level is not the last to be displayed."
  (concat
   (etask-get-flagstring)
   (if notesp "i" " ")
   (if (and subelements (not followersp)) "+" " ")
   " "))

(defun etask-get-subelstring(&optional element sublevel)
  "Return string that shows the subelement level of ELEMENT."
  (if (and element (natnump sublevel) (> sublevel 0))
      (concat
       (make-string sublevel ? )
       etask-subelements-prefix)
    ""))

(defun etask-insert-elementname(element len &optional sublevel)
  "Label ELEMENT's bar with ELEMENT's name, if necessary trimmed to
LEN letters.  The faces applied to the inserted string are determined
by the ELEMENT's status and its criticality (customizable).  Add an
infostring and a sublevel description if applicable."
  (let* ((rawname (etask-db-get element etask-db-attr-taskname))
         (elementname
          (substring rawname 0 (min (length rawname) len)))
         str
         (subelements (etask-db-get element etask-db-attr-subtasklist)))
    (cond ((string= (etask-db-get element etask-db-attr-tasktype)
                    etask-normaltask-string)
           (cond ((etask-cat-is-element-behind-schedule-p element)
                  (setq elementname
                        (etask-apply-face
                         elementname
                         'etask-face-normaltaskname-behind-schedule)))
                 ((etask-is-task-completed-p element)
                  (setq elementname
                        (etask-apply-face 
                         elementname
                         'etask-face-normaltaskname-completed)))
                 (t
                  (setq elementname
                        (etask-apply-face
                         elementname 
                         'etask-face-normaltask)))))
          ((string= (etask-db-get element etask-db-attr-tasktype)
                    etask-highrisktask-string)
           (cond ((etask-cat-is-element-behind-schedule-p element)
                  (setq elementname
                        (etask-apply-face
                         elementname
                         'etask-face-highrisktaskname-behind-schedule)))
                 ((etask-is-task-completed-p element)
                  (setq elementname
                        (etask-apply-face 
                         elementname
                         'etask-face-highrisktaskname-completed)))
                 (t
                  (setq elementname
                        (etask-apply-face
                         elementname
                         'etask-face-highrisktask)))))
          ((string= (etask-db-get element etask-db-attr-tasktype)
                    etask-criticaltask-string)
           (cond ((etask-cat-is-element-behind-schedule-p element)
                  (setq elementname
                        (etask-apply-face 
                         elementname
                         'etask-face-criticaltaskname-behind-schedule)))
                 ((etask-is-task-completed-p element)
                  (setq elementname
                        (etask-apply-face 
                         elementname
                         'etask-face-criticaltaskname-completed)))
                 (t
                  (setq elementname
                        (etask-apply-face
                         elementname
                         'etask-face-criticaltask))))))

    (setq str (concat 
               (etask-get-subelstring element sublevel) elementname))
    (etask-format-insert str len "left")))

(defun etask-insert-delimiter(&optional markp)
  "Insert `taskname-taskbar-delimiter'.  There are different
delimiters for marked and unmarked elements.  The 5th and 9th symbol
are highlighted."
  (let* ((line (etask-current-line))
         (navp (if (or (= line 5) (= line 9)) t nil)))
    (cond ((and (not markp) (not navp))
           (insert etask-delimiter-marker))
          ((and (not markp) navp)
           (insert (etask-apply-face
                    etask-delimiter-marker
                    'etask-face-specialdelimiter)))
          ((and markp (not navp))
           (insert etask-delimiter-markedtask-marker))
          ((and markp navp)
           (insert (etask-apply-face 
                    etask-delimiter-markedtask-marker
                    'etask-face-specialdelimiter))))))

(defun etask-insert-status(status)
  "Insert task STATUS in current buffer."
  (setq buffer-read-only nil)
  (goto-char (point-min))
  (delete-region
   (search-forward etask-statusheader nil t)
   (point-max))
  (insert status)
  (when (etask-get-control-item etask-ctrl-onlinehelp 'main)
    (etask-insert-help))
  (etask-fit-window)
  (setq buffer-read-only t))
  
(defun etask-insert-help()
  "Insert help, ie various shortcuts, in current buffer."
  (setq buffer-read-only nil)
  (insert
   (concat
    "\n\n"
    (make-string (- (window-width) 2) ?=)
    "\n"
    (etask-lang-msg 800 etask-language)
    "\n"
    (make-string (- (window-width) 2) ?=)
    "\n"
    (etask-lang-msg 801 etask-language)
    "\n"
    (etask-lang-msg 802 etask-language)
    "\n"
    (etask-lang-msg 803 etask-language)
    "\n"
    (etask-lang-msg 804 etask-language)
    "\n"
    (etask-lang-msg 805 etask-language)
    "\n"
    (etask-lang-msg 806 etask-language)
    "\n"
    (etask-lang-msg 807 etask-language)
    "\n"
    (etask-lang-msg 808 etask-language)
    "\n"
    (etask-lang-msg 809 etask-language)
    "\n"
    (etask-lang-msg 810 etask-language)
    "\n"
    (etask-lang-msg 811 etask-language)
    "\n"
    (etask-lang-msg 812 etask-language)
    "\n"
    (etask-lang-msg 813 etask-language)))
  (setq buffer-read-only t))

(defun etask-calculate-taskbar(tasktype open completed)
  "Return a task bar string.

The function generates a taskbar for tasks with type TASKTYPE.
Every bar consists of two parts: the still OPEN and the already 
COMPLETED part, given as nonnegative integers.
The faces of these parts can be customized for every TASKTYPE.
TASKTYPE is in {`etask-highrisktask-string', `etask-criticaltask-string',
`etask-normaltask-string'}."
  (let ((taskbar))
    (while (> open 0)
      (cond ((string= tasktype etask-highrisktask-string)
             (setq taskbar (concat etask-highriskplanned-marker taskbar)))
            ((string= tasktype etask-criticaltask-string)
             (setq taskbar (concat etask-criticalplanned-marker taskbar)))
            (t
             (setq taskbar (concat etask-planned-marker taskbar))))
      (setq open (1- open)))
    (while (> completed 0)
      (cond ((string= tasktype etask-highrisktask-string)
             (setq taskbar (concat etask-highriskcompleted-marker taskbar)))
            ((string= tasktype etask-criticaltask-string)
             (setq taskbar (concat etask-criticalcompleted-marker taskbar)))
            (t
             (setq taskbar (concat etask-completed-marker taskbar))))
      (setq completed (1- completed)))
    taskbar))

(defun etask-insert-taskbar(task start end daylen maxtasklen)
  "Insert TASK's taskbar into the draft Gantt chart.

START and END are the start and end dates of the draft Gantt chart.

DAYLEN is the width of a day in characters.

MAXTASKLEN is the maximum task name length in characters.

Example: '     ----------------   '   = inserted string
          1    2              3  4

          1...START, here begins our chart, may or may not be the
              project begin
               2...here starts our task
                              3...END, here ends our task
                                 4...here ends our chart, 
                                     may or may not be the projectend

Example:       '----------------        '   = inserted string
          2     1              3       4

          2-1...this part is outside the chart
"
  ;; Used variables defined in `etask-cat-show-elements': catid
  (cond ((= catid etask-category-todoid)
         (etask-todo-insert-bar task start end daylen))
        ((= catid etask-category-eventid)
         (etask-ev-insert-bar task start end daylen))
        ((= catid etask-category-projectid)
         (if (etask-is-milestone-p task)
             (let* ((taskstart 
                     (etask-db-get task etask-db-attr-taskbegin))
                    (earlierdays
                     (if (calendar-date-compare 
                          (list start) (list taskstart))
                         (etask-days-between 
                          start taskstart)
                       0))
                    (leadingspaces (round (* daylen earlierdays))))
               (insert
                (make-string 
                 (if (>= leadingspaces maxtasklen)
                     (1- leadingspaces)
                   leadingspaces)
                 ? ))
               (if (> daylen 1)
                   (etask-format-insert 
                    (make-string 
                     (cond ((< daylen 6)
                            1)
                           ((< daylen 9)
                            2)
                           (t
                            3))
                     ?M)
                    (round daylen)
                    "center")
                 (insert "M")))
           (let* ((taskstart (etask-db-get task etask-db-attr-taskbegin))
                  (taskend (etask-db-get task etask-db-attr-taskend))
                  (taskdays (1+ (etask-days-between taskstart taskend)))
                  (tasktype (etask-db-get task etask-db-attr-tasktype))
                  (chartdays (1+ (etask-days-between start end)))
                  (chartlen 
                   (min
                    (etask-state-get etask-stateid-maxbarlen)
                    (round (etask-simplify-number (* chartdays daylen)))))
                  (reltaskstart
                   (if (calendar-date-compare 
                        (list start)
                        (list taskstart))
                       (etask-days-between start taskstart)
                     ;; these task days are before chart window begin date
                     (- 0 (etask-days-between start taskstart))))
                  (reltaskend
                   (if (calendar-date-compare 
                        (list taskend)
                        (list end))
                       (etask-days-between taskend end)
                     (- 0 (etask-days-between taskend end))))

                  (pefforthours (etask-db-get task etask-db-attr-peffort))
                  (peffortdays
                   (if (> etask-workinghours-per-day 0)
                       (etask-simplify-number
                        (/ (float pefforthours) etask-workinghours-per-day))
                     0))

                  (eefforthours (etask-db-get task etask-db-attr-eeffort))
                  (eeffortdays
                   (if (and (> pefforthours 0)
                            (> etask-workinghours-per-day 0))
                       (etask-simplify-number
                        (/ (float eefforthours) etask-workinghours-per-day))
                     0))

                  (taskdaysvisible (+ taskdays
                                      (if (< reltaskstart 0)
                                          reltaskstart
                                        0)
                                      (if (< reltaskend 0)
                                          reltaskend
                                        0)))

                  (oefforthours
                   (if (> pefforthours eefforthours)
                       (- pefforthours eefforthours)
                     0))
                  (oeffortdays
                   (if (> peffortdays eeffortdays)
                       (- peffortdays eeffortdays)
                     0))

                  (leadingspclen
                   (if (> reltaskstart 0)
                       (max
                        (round
                         (etask-simplify-number
                          (* (float reltaskstart) daylen)))
                        1)
                     0))
                  (peffortbarlen
                   ;; We need `pefforthours' here because
                   ;; `etask-simplify-number' may round small numbers to
                   ;; zero when converting hours to days above
                   (if (and (> pefforthours 0) (> daylen 0))
                       ;; don't round to increase precision below
                       (etask-simplify-number
                        (* (float taskdays) daylen))
                     0))
                  (peffortbarlenvisible
                   (if (and (> pefforthours 0) (> daylen 0))
                       (etask-simplify-number
                        (* (float taskdaysvisible) daylen))
                     0))

                  (eeffortbarlen
                   (if (and (> eefforthours 0) (> peffortbarlen 0))
                       ;; unit = characters, hence round
                       (round
                        (etask-simplify-number
                         (max
                          (+
                           (* (/ (float eefforthours)
                                 pefforthours)
                              peffortbarlen)
                           (if (< reltaskstart 0) 
                               ;; subtract invisible days
                               (* (float reltaskstart) daylen)
                             0))
                          1)))
                     0))

                  (oeffortbarlen
                   (if (and (> oefforthours 0) (> peffortbarlen 0))
                       (round
                        (etask-simplify-number
                         (max
                          (- peffortbarlenvisible eeffortbarlen)
                          1)))
                     0))
                  (deviation
                   (or
                    (> (+ eeffortbarlen oeffortbarlen) 
                       (round peffortbarlenvisible))
                    (> (+ leadingspclen eeffortbarlen oeffortbarlen) 
                       chartlen))))
        
             (when deviation
               (let ((amount 
                      (max 1
                           (round
                            (abs (- (+ eeffortbarlen oeffortbarlen)
                                    peffortbarlenvisible))))))
                 (if (and (> eeffortbarlen oeffortbarlen)
                          (> eeffortbarlen 2))
                     (setq eeffortbarlen (max 1 (- eeffortbarlen amount)))
                   (when (> oeffortbarlen 2)
                     (setq oeffortbarlen (max 1 (- oeffortbarlen amount)))
                     (setq deviation nil))
                   (when deviation
                     (setq leadingspclen (max (- leadingspclen amount) 0))
                     (setq deviation nil)))))
        
             (when (> leadingspclen 0)
               (insert (make-string leadingspclen ? )))
             (when (> eeffortbarlen 0)
               (insert (etask-calculate-taskbar 
                        tasktype 0 eeffortbarlen)))
             (when (> oeffortbarlen 0)
               (insert (etask-calculate-taskbar 
                        tasktype oeffortbarlen 0))))))))

(defun etask-insert-currentdatestring(scaledprojday scaledmarkerlen)
  "Return string '<spaces><current date>, DAY X/Y', omit the DAY X/Y
part for all categories but project.

X is the nth project day from project begin till today and Y is the
sum of all project days.  SCALEDPROJDAY is the character number from
task bar begin to current date.  SCALEDMARKERLEN defines the length of
one day in characters."
  (let* ((catid (car (etask-cat-get-current-item)))
         (item (car (cdr (etask-cat-get-current-item))))
         (tasklist (etask-cat-get-current-elts))
         (maxtasklen (etask-state-get etask-stateid-maxtasklen))
         (maxbarlen (etask-state-get etask-stateid-maxbarlen))
         (projbegin (etask-calculate-earliestbegin tasklist))
         (projend (etask-calculate-lastend tasklist))
         (projday (1+ (etask-days-between 
                       projbegin
                       (calendar-current-date))))
         (projlen (1+ (etask-days-between
                       projbegin
                       projend)))
         (len-daycounter 
          (+ (length 
              (concat
               (etask-lang-msg 772 etask-language)
               " /"))
             ;; The x in 'DAY x/y'
             (length (number-to-string projday))
             ;; The y
             (length (number-to-string projlen)))))
    (when (etask-cat-civalid-p catid item)
      (cond ((or
              (calendar-date-equal
               (calendar-current-date)
               (etask-state-get etask-stateid-chartstart))
              (calendar-date-equal
               (calendar-current-date)
               (etask-state-get etask-stateid-chartend))
              (>= scaledmarkerlen 14))
             (let ((leadingspc
                    (round
                     (max
                      (1+ maxtasklen)
                      (min
                       (- (+ maxtasklen scaledprojday (/ scaledmarkerlen 2))
                          (/ len-daycounter 2))
                       (- (+ maxtasklen 1 maxbarlen)
                          len-daycounter))))))
               (insert (make-string leadingspc ? ))
               (when (= catid etask-category-projectid)
                 (insert
                  (etask-apply-face
                   (concat
                    (etask-lang-msg 772 etask-language)
                    " "
                    (number-to-string projday)
                    "/"
                    (number-to-string projlen))
                   'etask-face-today)))))
            ((>= scaledmarkerlen 9)
             (let* ((date (calendar-day-name (calendar-current-date)))
                    (len (+ len-daycounter (length date) 2)) ;", "
                    (leadingspc
                     (round
                      (max
                       (1+ maxtasklen)
                       (min
                        (- (+ maxtasklen scaledprojday (/ scaledmarkerlen 2))
                           (/ len-daycounter 2))
                        (- (+ maxtasklen 1 maxbarlen)
                           len))))))
               (insert (make-string leadingspc ? ))
               (insert
                (etask-apply-face
                 (concat
                  date
                  (when (= catid etask-category-projectid)
                    (concat
                     ", "
                     (etask-lang-msg 772 etask-language)
                     " "
                     (number-to-string projday)
                     "/"
                     (number-to-string projlen))))
                 'etask-face-today))))
            (t
             (let* ((date (calendar-date-string (calendar-current-date)))
                    (len 
                     ;; (round x.5) may be x on some systems => ceiling
                     (ceiling 
                      (/ (+ (float (length date))
                            ;; Center current date string below middle of
                            ;; `etask-today-symbol's
                            (length ", ")
                            len-daycounter)
                         2)))
                    ;; scaled day can be wider than 1 column; if so: center
                    (datestring-middle-pos 
                     (+ scaledprojday (/ scaledmarkerlen 2)))
                    (numofblanks
                     (floor       ;ensure wholenumberp for make-string
                      (+ maxtasklen     ;Leading blanks
                         1              ;Delimiter
                         (cond ((<= datestring-middle-pos len)
                                0)
                               ((> (+ datestring-middle-pos len) maxbarlen)
                                (- maxbarlen (* len 2 )))
                               (t
                                (- datestring-middle-pos len)))))))
               (insert (make-string numofblanks ? ))
               (insert
                (etask-apply-face
                 (concat
                  date
                  (when (= catid etask-category-projectid)
                    (concat
                     ", "
                     (etask-lang-msg 772 etask-language)
                     " "
                     (number-to-string projday)
                     "/"
                     (number-to-string projlen))))
                 'etask-face-today))))))))

(defun etask-insert-day-in-timeline(daylen date)
  "Insert DATE in timeline.  There are DAYLEN chars available."
  (if (> daylen 2)
      (let ((str (cond ((< daylen 9)
                        (number-to-string (extract-calendar-day date)))
                       ((and (>= daylen 9) (< daylen 14))
                        (substring (calendar-date-string date t t) 0 -5))
                       ((and (>= daylen 14) (< daylen 19))
                        (calendar-date-string date t t))
                       ((and (>= daylen 19) (< daylen 29))
                        (calendar-date-string date t))
                       (t
                        (calendar-date-string date)))))
        (etask-format-insert
         (cond ((calendar-date-equal (calendar-current-date) date)
                (etask-apply-face str 'etask-face-today))
               ((etask-businessday-p date)
                str)
               (t
                (etask-apply-face str 'etask-face-holiday)))
         (1- daylen) "center")
        (setq date (etask-add-days-to-date date 1)))
    (insert (make-string (1- daylen) ? )))
  (insert etask-day-marker))

(defun etask-insert-daymarkers(chars daylen date)
  "Insert day markers - width of all markers is CHARS, width of one
marker is DAYLEN.  If there is enough space available print the days
as well, starting with DATE."
  (when (and (natnump chars) (natnump daylen))
    (if (>= daylen 1)
        (while (> chars 0)
          (etask-insert-day-in-timeline daylen date)
          (setq date (etask-add-days-to-date date 1))
          (setq chars (- chars daylen)))
      (insert (make-string (* chars daylen) ? )))))

(defun etask-insert-projbeginendmarker(daylen date)
  "Insert project begin/end marker and, if there is enough space, also
day markers in timeline."
  (if etask-show-projectdates-p
      (cond ((and (natnump daylen) (> daylen 2))
             (etask-insert-day-in-timeline daylen date))
            ((and (natnump daylen) (= daylen 2))
             (insert "^")
             (insert etask-day-marker))
            ((<= daylen 1)
             (insert "^"))
            (t
             ()))))

(defun etask-insert-todaymarker(daylen)
  "Insert today marker in timeline.

DAYLEN is the width of today marker.  If <1 it is enlarged to 1."
  (if etask-show-projectdates-p
      (cond ((and (natnump daylen) (> daylen 2))
             (etask-insert-day-in-timeline daylen (calendar-current-date)))
            ((and (natnump daylen) (= daylen 2))
             (insert etask-today-symbol)
             (insert (etask-apply-face etask-day-marker 'etask-face-today)))
            ((<= daylen 1)
             (insert (etask-apply-face "^" 'etask-face-today)))
            (t
             ()))))

(defun etask-insert-timelinesymbols(scaledprojectlen
                                    scaledoffsetbefore 
                                    scaledoffsetafter
                                    scaledmarkerlen)
  "Insert timeline markers \(chart begin, chart end, today, and day
markers\) if there is enough space available.

SCALEDPROJECTLEN is a nonnegative integer defining the project length
in columns.  SCALEDOFFSETBEFORE is a nonnegative integer defining the
days from chart begin till the current date \(inclusive\) in
columns. If a scaled day is > 1 column, it holds the number of columns
till the first column of the current day \(inclusive\).
SCALEDOFFSETAFTER is a nonnegative integer defining the days after the
current date till the project end.  SCALEDMARKERLEN is a nonnegative
integer defining the length of one day in columns \(characters\)."
  (let ((chartbegin (etask-state-get etask-stateid-chartstart))
        (chartend (etask-state-get etask-stateid-chartend))
        (maxtasklen (etask-state-get etask-stateid-maxtasklen)))
    (when (calendar-date-compare 
           (list chartbegin) 
           (list chartend))
      
      ;; leading blanks for element name column including delimiter
      (insert (make-string (1+ maxtasklen) ? ))
    
      (cond ((and (> scaledoffsetbefore scaledmarkerlen) ;'>': inclusive
                  (>= scaledoffsetafter scaledmarkerlen)
                  (calendar-date-compare 
                   (list chartbegin) 
                   (list (calendar-current-date)))
                  (calendar-date-compare
                   (list (calendar-current-date)) 
                   (list chartend)))
             ;; today is visible and enough space for markers

             (etask-insert-projbeginendmarker scaledmarkerlen chartbegin)

             ;; days between chart begin and current date
             (if (and (natnump scaledmarkerlen) (>= scaledmarkerlen 1))
                 (etask-insert-daymarkers (- scaledoffsetbefore 
                                             (* 2 scaledmarkerlen))
                                          scaledmarkerlen
                                          (etask-add-days-to-date
                                           chartbegin 1))
               (insert (make-string 
                        (max
                         ;; subtract start and today marker
                         (round (- scaledoffsetbefore 2))
                         0)
                        ? )))

             ;;today marker
             (etask-insert-todaymarker scaledmarkerlen)

             ;; days between current date and chart end
             (if (and (natnump scaledmarkerlen) (>= scaledmarkerlen 1))
                 (etask-insert-daymarkers (- scaledoffsetafter
                                             scaledmarkerlen)
                                          scaledmarkerlen
                                          (etask-add-days-to-date
                                           (calendar-current-date) 1))
               (insert (make-string 
                        (max
                         ;; subtract already inserted characters
                         (round (- scaledprojectlen 
                                   (max
                                    (round (- scaledoffsetbefore 2))
                                    0)        
                                   3))
                         0) 
                        ? )))

             ;; chart end marker is always on the right of the timeline
             (etask-insert-projbeginendmarker scaledmarkerlen chartend))
          
            ((and (= scaledoffsetbefore 1)
                  (>= scaledprojectlen (* 2 scaledmarkerlen)))
             ;; => chart begin is current date
             (etask-insert-todaymarker scaledmarkerlen)
             (if (>= scaledmarkerlen 1)
                 (etask-insert-daymarkers (- scaledoffsetafter
                                             scaledmarkerlen)
                                          scaledmarkerlen
                                          (etask-add-days-to-date
                                           (calendar-current-date) 1))
               (insert (make-string (- scaledprojectlen 2) ? )))
             (etask-insert-projbeginendmarker scaledmarkerlen chartend))

            ((and (= scaledoffsetafter 0)
                  (>= scaledprojectlen (* 2 scaledmarkerlen)))
             ;; => chart ends today
             (etask-insert-projbeginendmarker scaledmarkerlen chartbegin)
             (if (>= scaledmarkerlen 1)
                 (etask-insert-daymarkers (- scaledoffsetbefore 
                                             (* 2 scaledmarkerlen))
                                          scaledmarkerlen
                                          (etask-add-days-to-date
                                           chartbegin 1))
               (insert (make-string (- scaledprojectlen 2) ? )))
             (etask-insert-todaymarker scaledmarkerlen))
           
            ((or
              (calendar-date-compare (list chartend) 
                                     (list (calendar-current-date)))
              (calendar-date-compare (list (calendar-current-date))
                                     (list chartbegin)))
             ;; => current date not visible
             (etask-insert-projbeginendmarker scaledmarkerlen chartbegin)
             (if (>= scaledmarkerlen 1)
                 (etask-insert-daymarkers
                  (* (1- (etask-days-between chartbegin chartend))
                     scaledmarkerlen)
                  scaledmarkerlen
                  (etask-add-days-to-date chartbegin 1))
               (insert (make-string (- scaledprojectlen 2) ? )))
             (etask-insert-projbeginendmarker scaledmarkerlen chartend)))

      (insert "\n"))))

(defun etask-insert-datestring(scaledmarkerlen scaledprojlen)
  "Insert chart timeline dates.  SCALEDMARKERLEN and SCALEDPROJLEN are
non-negative integers representing the length of a day and the project
length in chars, respectively."
  (let ((chartbegin (etask-state-get etask-stateid-chartstart))
        (chartend (etask-state-get etask-stateid-chartend))
        (maxtasklen (etask-state-get etask-stateid-maxtasklen)))

    ;; blanks for task name column and delimiter
    (insert (make-string (+ maxtasklen 1) ? ))

    (cond ((etask-todo-organizer-view-p)
           (let ((datestr
                  (if (calendar-date-equal 
                       (calendar-current-date) chartbegin)
                      (etask-apply-face
                       (calendar-date-string chartend) 'etask-face-today)
                    (calendar-date-string chartbegin))))
             (etask-format-insert datestr scaledmarkerlen "center")))
          ((and (>= scaledmarkerlen 19)
                (calendar-date-equal chartbegin chartend))
           (let ((date
                  (if (calendar-date-equal 
                       (calendar-current-date) chartbegin)
                      (etask-apply-face
                       (calendar-date-string chartbegin)
                       'etask-face-today)
                    (calendar-date-string chartbegin))))
             (etask-format-insert date scaledprojlen "center")))
          ((>= scaledmarkerlen 19)
           ())
          ((and (>= scaledmarkerlen 14) (< scaledmarkerlen 19))
           (let* ((start 
                   (if (calendar-date-equal 
                        (calendar-current-date) chartbegin)
                       (etask-apply-face
                        (calendar-day-name chartbegin) 'etask-face-today)
                     (calendar-day-name chartbegin)))
                  (end 
                   (if (calendar-date-equal 
                        (calendar-current-date) chartend)
                       (etask-apply-face
                        (calendar-day-name chartend) 'etask-face-today)
                     (calendar-day-name chartend))))
             (etask-format-insert start scaledmarkerlen "center")
             (when (>= scaledprojlen (* 2 scaledmarkerlen))
               (insert (make-string (- scaledprojlen 
                                       (* 2 scaledmarkerlen)) 
                                    ? ))
               (etask-format-insert end scaledmarkerlen "center"))))
          (t
           (let ((len1 (length (calendar-date-string chartbegin t)))
                 (len2 (length (calendar-date-string chartend t))))
             (insert (calendar-date-string chartbegin t))
             (if (> scaledprojlen (+ len1 len2 (length " - ")))
                 (insert (make-string (- scaledprojlen len1 len2) ? ))
               (insert " - "))
             (insert (calendar-date-string chartend t)))))
    (insert "\n")))

(defun etask-insert-projectdates-pointers(scaledmarkerlen scaledprojlen)
  "Insert characters that point to chart begin and chart end dates."
  (let ((chartbegin (etask-state-get etask-stateid-chartstart))
        (chartend (etask-state-get etask-stateid-chartend))
        (maxtasklen (etask-state-get etask-stateid-maxtasklen)))
    (cond ((>= scaledmarkerlen (/ scaledprojlen 2))
           ())
          (t
           (let ((strstart
                  (if (calendar-date-equal start (calendar-current-date))
                      (etask-apply-face "*" 'etask-face-today)
                    "*"))
                 (strend
                  (if (calendar-date-equal end (calendar-current-date))
                      (etask-apply-face "*" 'etask-face-today)
                    "*")))
             (insert (make-string (+ maxtasklen 1) ? ))
             (insert strstart)
             (if (> scaledmarkerlen 1)
                 (progn
                   (insert (make-string (- scaledmarkerlen 2) ? ))
                   (insert strstart)
                   (insert (make-string (- scaledprojlen 
                                           (* 2 scaledmarkerlen))
                                        ? )))
               (insert (make-string (- scaledprojlen 2) ? )))
             (insert strend)
             (when (> scaledmarkerlen 1)
               (insert (make-string (- scaledmarkerlen 2) ? ))
               (insert strend)))))
    (insert "\n")))

(defun etask-insert-datelabels(daylen)
  "Insert project timeline markers for chart start, chart end, current
date, and end of days, if applicable."
  (let* ((start (etask-state-get etask-stateid-chartstart))
         (end (etask-state-get etask-stateid-chartend))
         (maxtasklen (etask-state-get etask-stateid-maxtasklen))
         (chartlen (1+ (etask-days-between start end)))
         (projday 
          (if (or
               (calendar-date-compare 
                (list start) (list (calendar-current-date)))
               (calendar-date-equal start (calendar-current-date)))
              (1+ (etask-days-between start (calendar-current-date)))
            (- 0 (etask-days-between start (calendar-current-date)))))
         (projdays-tomorrow-end 
          (if (calendar-date-compare 
               (list (calendar-current-date)) (list end))
              (etask-days-between (calendar-current-date) end)
            (- 0 (etask-days-between (calendar-current-date) end))))
         (insert-today-p
          (and
           (or
            (calendar-date-compare (list start) 
                                   (list (calendar-current-date)))
            (calendar-date-equal start 
                                 (calendar-current-date)))
           (or
            (calendar-date-compare (list (calendar-current-date)) 
                                   (list end))
            (calendar-date-equal (calendar-current-date) 
                                 end))))
         (scaledprojday
          (if (> daylen 1)
              ;; scaled day > 1 column => set scaledprojday 
              ;; at first column
              ;; |proj start|         |today    |         |proj end  |
              ;;                       ^
              ;;                       |-- this column is scaledprojday
              (round (1+ (- (* daylen projday)
                            daylen)))
            (round (* daylen projday))))
         (maxbarlen (etask-state-get etask-stateid-maxbarlen))
         (scaledprojlen 
          (min
           maxbarlen
           (round (etask-simplify-number
                   (* chartlen daylen)))))
         (scaledmarkerlen         ;proj begin/end, today, days (if >1)
          (if (> daylen 1)
              (round daylen)
            daylen)))               ;just insert proj begin/end, today
    (cond ((calendar-date-compare (list start) (list end))
           (etask-insert-timelinesymbols
            scaledprojlen 
            scaledprojday
            (round (* daylen projdays-tomorrow-end))
            scaledmarkerlen)            ;needed if daylen > 1
           (when etask-show-projectdates-p
             (etask-insert-projectdates-pointers
              scaledmarkerlen scaledprojlen)
             (etask-insert-datestring 
              scaledmarkerlen scaledprojlen))
           (when insert-today-p
             (etask-insert-currentdatestring
              scaledprojday scaledmarkerlen)))
          ((calendar-date-equal start end)
           (etask-todo-insert-timelinelabels start))
          (t
           ()))))


;;; User input handling

(defun etask-get-effort-str(effort)
  "Return human readable string of EFFORT, given in hours.  Examples:
3m, 1w, 5d, 40h for 3 minutes, 1 week, 5 days, and 40 hours,
respectively.  If EFFORT is not a valid effort return nil."
  (when (numberp effort)
    (cond ((and (natnump effort)
                (natnump etask-workinghours-per-day)
                (natnump etask-workingdays-per-week)
                (> etask-workinghours-per-day 0)
                (> etask-workingdays-per-week 0)
                (= (% effort (* etask-workinghours-per-day
                                etask-workingdays-per-week))
                   0))
           (concat
            (number-to-string 
             (/ effort 
                (* etask-workinghours-per-day
                   etask-workingdays-per-week)))
            "w"))
          ((and (natnump effort)
                (natnump etask-workinghours-per-day)
                (> etask-workinghours-per-day 0)
                (= (% effort etask-workinghours-per-day)
                   0))
           (concat
            (number-to-string 
             (/ effort 
                etask-workinghours-per-day))
            "d"))
          ((< effort 1)
           (concat
            (number-to-string
             (round
              (etask-simplify-number
               (* effort 60))))
            "min"))
          (t
           (concat
            (number-to-string effort)
            "h")))))

(defun etask-get-planned-effort(task &optional oldpeffort)
  "Get planned effort for TASK from minibuffer.  Optional OLDPEFFORT
serves as default value."
  (let* ((eeffort (etask-db-get task etask-db-attr-eeffort)))
    (etask-extract-effort-hours
     (etask-read
      (concat 
       (format "'%s': "
               (etask-shorten-string 
                (etask-db-get task etask-db-attr-taskname)
                etask-longer-taskname-len-minibuf))
       (etask-lang-msg 120 etask-language)
       (when (and (numberp eeffort) (> eeffort 0))
         (concat " (>= " (etask-get-effort-str eeffort) ")"))
       ": ")
      (lambda (x) (and (string-match etask-effort-regexp x)
                       (>= (etask-extract-effort-hours x) eeffort)))
      (etask-get-effort-str oldpeffort)))))

(defun etask-get-labels()
  "Return a list of element names entered by the user via minibuffer.

Collect task names from minibuffer, and return these names as a list
if the entered number of tasks is in 1..`etask-max-dataitems' or nil
if this number is 0."
  (let* ((catid (car (etask-cat-get-current-item)))
         (labellist)
         (labelnum 
          (if (= catid etask-category-projectid)
              (string-to-number
               (etask-read
                (concat
                 (car (cdr (etask-cat-get-current-item)))
                 ": "
                 (etask-lang-msg 100 etask-language)
                 (format " (0-%d)? " etask-max-dataitems))
                (lambda (x) (and
                             (string-match 
                              etask-wholenumber-regexp x)
                             (>= (string-to-number x) 
                                 0)
                             (<= (string-to-number x) 
                                 etask-max-dataitems)))
                (if (= (etask-cat-get-elementnum) 0)
                    (number-to-string 10)
                  (number-to-string 1))))
            1))
         (temp labelnum))
    (while (> labelnum 0)
      (setq labellist
            (cons
             (cond ((= catid etask-category-projectid)
                     (etask-get-taskname nil 
                                         (+ 1 (- temp labelnum))
                                         temp
                                         labellist))
                   (t
                    (etask-get-taskname)))
             labellist))
      (setq labelnum (1- labelnum)))
    (nreverse labellist)))

(defun etask-get-taskdate(prompt &optional task endoftaskp begindate)
  "Get a task date via minibuffer and return list (month day year).

PROMPT is a string containing the prefix prompt for the minibuffer
query.

TASK (optional) is a list with all the task-specific information; for
detailed structure go to `etask-write-element-p'.  If used its current
begin or - if optional ENDOFTASKP is non-nil - end date is the default
value in the minibuffer.

BEGINDATE is the task's begin date when ENDOFTASKP is non-nil."
  (let* ((safyear (if endoftaskp 
                         (extract-calendar-year begindate)
                       etask-earliest-taskstart-year))
         (year (string-to-number
                (etask-read
                 (concat
                  (format "%s" prompt)
                  " - "
                  (etask-lang-msg 776 etask-language)
                  "  ? ")
                 (lambda (x) (and
                              (string-match 
                               etask-wholenumber-regexp x)
                              (>= (string-to-number x) safyear)))
                 (if task
                     (int-to-string
                      (if endoftaskp
                          (extract-calendar-year 
                           (etask-db-get task etask-db-attr-taskend))
                        (extract-calendar-year 
                         (etask-db-get task etask-db-attr-taskbegin))))
                   (int-to-string
                    (extract-calendar-year 
                     (calendar-current-date)))))))
         (month-array calendar-month-name-array)
         (completion-ignore-case t)
         (month (cdr (assoc-ignore-case
                      (completing-read
                       (concat
                        (format "%s" prompt)
                        " - "
                        (etask-lang-msg 777 etask-language)
                        "  ? ")
                       (mapcar 'list (append month-array nil))
                       nil
                       t
                       (cond ((and task endoftaskp)
                              (calendar-month-name 
                               (extract-calendar-month 
                                (etask-db-get task etask-db-attr-taskend))))
                             (task
                              (calendar-month-name 
                               (extract-calendar-month 
                                (etask-db-get task etask-db-attr-taskbegin))))
                             (t
                              ())))
                      (calendar-make-alist month-array 1))))
         (safendday (if (and endoftaskp
                             (= year (extract-calendar-year begindate))
                             (= month (extract-calendar-month begindate)))
                        (+
                         (extract-calendar-day begindate)
                         (etask-days-till-business-day begindate))
                      (1+ (etask-days-till-business-day 
                           (list month 1 year)))))
         (last (calendar-last-day-of-month month year))
         (lastbizday (if (etask-businessday-p (list month last year))
                         last
                       (-
                        last
                        (etask-days-till-business-day-before
                         (list month last year))))))
    (if (> safendday lastbizday)
        (list 1 1 1)
      (list month
            (string-to-number
             (etask-read 
              (concat
               (format "%s" prompt)
               " - "
               (etask-lang-msg 721 etask-language)
               " "
               (format "(%s-%d): " 
                       (number-to-string safendday)
                       lastbizday))
              (lambda (x) (and 
                           (string-match 
                            etask-wholenumber-regexp x)
                           (<= safendday (string-to-number x)) 
                           (<= (string-to-number x) lastbizday)
                           (etask-businessday-p
                            (list  
                             month
                             (string-to-number x)
                             year))))
              (cond ((and task 
                          endoftaskp
                          (> safendday 1))
                     (number-to-string 
                      (max
                       safendday
                       (extract-calendar-day 
                        (etask-db-get task etask-db-attr-taskend)))))
                    ((and task
                          endoftaskp)
                     (number-to-string 
                      (extract-calendar-day 
                       (etask-db-get task etask-db-attr-taskend))))
                    (task
                     (number-to-string 
                      (extract-calendar-day 
                       (etask-db-get task etask-db-attr-taskbegin))))
                    (t
                     nil))))
            year))))

(defun etask-get-taskenddate(taskname prompt date &optional task)
  "Get TASKNAME's planned end date and ensure that it is at or after
DATE.

The function calls `etask-get-taskdate' to get TASKNAME's planned end
date until the returned date is after DATE.  PROMPT and optional TASK
are processed within `etask-get-taskdate'."
  (let ((illegaldatep t)
        (tmp))
    (while illegaldatep
      (setq tmp (etask-get-taskdate prompt task 'gettaskend date))
      (if (or
           (calendar-date-compare (list date)
                                  (list tmp))
           (calendar-date-equal tmp 
                                date))
          (setq illegaldatep nil)
        (error "%s" (etask-lang-msg 1003 etask-language))))
    tmp))

(defun etask-get-taskname(&optional oldname num total names)
  "Get task name from minibuffer and delete leading and trailing
whitespaces.  The empty string is not a valid taskname.  Uniqueness is
checked against current element names unless NAMES is non-nil - see
below.

If optional OLDNAME is non-nil it serves as the default task name.
If optional NUM and TOTAL are non-nil the user enters more than one
task at once.  The current task name is the NUMth out of TOTAL task
names.  If optional NAMES is non-nil, uniqueness is checked against
this list,too."
  (let ((invalid t)
        (newname)
        (catid (car (etask-cat-get-current-item)))
        (element (capitalize (etask-cat-get-catelementname catid))))
    (while invalid
      (setq newname
            (etask-string-trim
             ;; XEmacs `completing-read' has only 7 parameters
             (completing-read
              (if oldname
                  (concat
                   (etask-lang-msg 299 etask-language)
                   (format ": '%s': " 
                           (etask-shorten-string
                            oldname
                            etask-longer-taskname-len-minibuf)))
                (if (and num total)
                    (concat
                     (etask-lang-msg 781 etask-language)
                     " ("
                     element
                     (format " %d/%d): " num total))
                  (concat
                   element ": "
                   (etask-lang-msg 781 etask-language)
                   ": ")))
              etask-task-alist
              nil nil nil nil oldname)))
      (if (or
           (and (stringp oldname) (string= newname oldname))
           (and (not (string-equal newname ""))
               (or (and names
                        (not (member newname names))
                        (not (member
                              newname
                              (etask-cat-get-current-elts 'names))))
                   (and (not names)
                        (not (member
                              newname
                              (etask-cat-get-current-elts 'names)))))))
          (setq invalid nil)))
    newname))


;;; Other useful stuff

(defun boolean-p (bool)
  "Return non-nil if BOOL is nil or t.  ---> Source: eieio.el"
  (or (null bool) (eq bool t)))


;;; Initialization

(setq etask-loaded-p t)
(provide 'etask)


;;; etask.el  end of file
