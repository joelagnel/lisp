;;;; sidebrain-vars.el -- global variables for sidebrain
;;; Time-stamp: <2006-04-11 12:46:23 john>

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

(provide 'sidebrain-vars)

(defstruct sidebrain-task-stack
  "Sidebrain data relating to a particular task and its subtasks."
  tasks
  observations
  priority
  link-group
  link-project
  link-task
  )

(defstruct sidebrain-task
  "Sidebrain data relating to a particular task."
  text
  file
  line-number

  time-started
  time-ended
  time-this-time			; gets added sporadically to time-spent
  time-spent

  time-started-current
  time-ended-current
  time-current

  keystrokes

  subtasks
  suspensions

  abandoned

  display-start
  display-end
)

;;;; big global variables

(defvar sidebrain-project-groups nil
  "alist of project group names to project groups.
a group is an alist of projects.
this is the top-level data structure for sidebrain; the other globals point
to parts of this structure. the main such variables are:
  sidebrain-current-project-group
  sidebrain-current-project
  sidebrain-current-stack")

(defvar sidebrain-current-project-group nil
  "a pair representing the current project group.
the car is the name, and the cdr is the alist of project names to projects.
this pair is a member of sidebrain-project-groups.")

(defvar sidebrain-current-project nil
  "a pair representing the current project.
the car is the project name, and the cdr is the alist of task names to tasks.
this pair is a member of sidebrain-current-project-group.")

(defvar sidebrain-current-stack nil
  "the task (and subtasks) that the user is currently working on;
and also the observations.
these are stored collectively as cons of the stack name and a structure
type, sidebrain-task-stack.")

(defvar sidebrain-history nil
  "The history of completed tasks.")

(defvar sidebrain-suspension-history nil
  "The list of suspended tasks.")

;;; end of sidebrain-vars.el
