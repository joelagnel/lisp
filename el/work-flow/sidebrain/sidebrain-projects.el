;;;; sidebrain-projects.el -- sidebrain project and task data structures
;;; Time-stamp: <2006-04-25 09:42:28 jcgs>

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

(provide 'sidebrain-projects)
(require 'sidebrain-macros)

(defun sidebrain-get-project-group (group)
  "Return the project group called GROUP.
It is returned as a cons of the name and the projects in the group."
  (cond
   ((null group)
    sidebrain-current-project-group)
   ((stringp group)
    (assoc group sidebrain-project-groups))
   ((consp group)
    group)
   (t (error "Bad type for sidebrain-get-project-group: %S" group))))

(defun sidebrain-get-project (project group)
  "Return the project called PROJECT from GROUP.
It is returned as a cons of the name and the tasks in the project."
  (let ((group (sidebrain-get-project-group group)))
    (if (stringp project)
	(assoc project (cdr group))
      (if (consp project)
	  project
	(error "Bad type for sidebrain-get-project: %S" project)))))

(defun sidebrain-set-project-group (group &optional auto-create)
  "Set the current project group to GROUP.
GROUP may be a name, or a pair of name . projectlist.
With optional second argument, and a string for the first argument,
create the group if it does not exist."
  (interactive
   (list (sidebrain-completing-read-project-group)
	 t))
  (cond
   ((null group)
    (setq sidebrain-current-project-group nil
	  sidebrain-current-project nil))
   ((stringp group)
    (let ((found (sidebrain-get-project-group group)))
      (if found
	  (setq sidebrain-current-project-group found)
	(if (or auto-create
		sidebrain-create-groups-on-demand
		(member group sidebrain-always-available-groups))
	    (progn
	      (message "Creating project group \"%s\" on demand" group)
	      (sidebrain-new-project-group group))
	  (error "No such project group: %S" group)))))
   ((listp group)
    (unless (sidebrain-get-project-group (car group))
      (error "Trying to make an unknown group %s current" (car group)))
    (setq sidebrain-current-project-group group)))
  (unless (memq sidebrain-current-project
		sidebrain-current-project-group)
    (setq sidebrain-current-project nil)))

(defvar sidebrain-completing-read-project-group-history nil
  "History variable for sidebrain-completing-read-project-group.")

(defvar sidebrain-completing-read-project-group-default nil
  "The default project group, when reading a project group from the user.")

(defun sidebrain-completing-read-project-group (&optional prompt default nonexistent-ok)
  "Prompt the user for a project group name, and return the name as a string.
If PROMPT is given, use it.
If DEFAULT is given, and is a project group or project group name, use it when prompting.
If DEFAULT is t, use the current project group name as the default.
If NONEXISTENT-OK is non-nil, allow the user to enter a name that is not currently
naming a project group."
  ;; (with-output-to-temp-buffer (format "*Backtrace for group %S*" (gensym)) (backtrace))
  (setq sidebrain-completing-read-project-group-history
	(mapcar 'car sidebrain-project-groups))
  (let* ((default-input (cond
			 ((eq default t) (car sidebrain-current-project-group))
			 ((stringp default) default)
			 ((consp default) (car default))
			 (sidebrain-completing-read-project-group-default
			  sidebrain-completing-read-project-group-default)
			 (t (car sidebrain-current-project-group))))
	 (history-position (position default-input
				     sidebrain-completing-read-project-group-history)))
    (message "history is %S; default-input is %S; history-position is %S" sidebrain-completing-read-project-group-history default-input history-position)
    (let ((read (completing-read (format (or prompt "Project group (default %s): ") default-input)
				 sidebrain-project-groups
				 nil	; predicate
				 (not nonexistent-ok) ; require-match
				 nil	; initial-input
				 (if (integerp history-position)
				     (cons 'sidebrain-completing-read-project-group-history
					   (1+ history-position))
				   'sidebrain-completing-read-project-group-history))))
      (if (string= read "")
	  default-input
	read))))

(defun sidebrain-new-project-group (name)
  "Create and select a new project group."
  (interactive "sNew group name: ")
  (when (sidebrain-get-project-group name)
    (error "Group %s already exists!" name))
  (message "Creating project group \"%s\"" name)
  (setq sidebrain-current-project-group (cons name nil)
	sidebrain-current-project nil
	sidebrain-current-stack nil)
  (push sidebrain-current-project-group
	sidebrain-project-groups)
  (when (interactive-p) (sidebrain-display)))

(defun sidebrain-delete-project-group (name)
  "Delete the project group called NAME.
If this is the current group, there is then no current group."
  (interactive (list (sidebrain-completing-read-project-group "Delete project group: ")))
  (let* ((pair (assoc name sidebrain-project-groups))
	 (was-current (eq pair sidebrain-current-project-group)))
    (setq sidebrain-project-groups (delete pair sidebrain-project-groups))
    (when was-current
      (setq sidebrain-current-project-group nil
	    sidebrain-current-project nil)))
  ;; re-display if we were displaying (approximately -- only checks current buffer)
  (when (eq major-mode 'sidebrain-browse-tasks-mode)
    (sidebrain-browse-tasks sidebrain-browse-tasks-showing-all)))

(defun sidebrain-set-project (project &optional auto-create)
  "Make PROJECT be the current project.
PROJECT may be the name of a project, or a pair of name.taskqueue
With a string as the first argument, and an optional second argument
is given, the project is created, in the current group, if there is
none of that name in the current group; otherwise, it is an error for
the current group not to have one of that name."
  (interactive
   (progn
     (unless sidebrain-current-project-group
       (call-interactively 'sidebrain-set-project-group))
     (list (sidebrain-completing-read-project nil nil t)
	   t)))
  (unless sidebrain-current-project-group
    (error "No project group selected"))
  (cond
   ((stringp project)
    (when (string= project "") (setq project "General"))
    (let ((found (assoc project sidebrain-current-project-group)))
      (if found
	  (setq sidebrain-current-project found)
	(if (or auto-create
		(yes-or-no-p (format "Create project \"%s\" in group \"%s\"? "
				     project
				     (car sidebrain-current-project-group))))
	    (sidebrain-new-project project)
	  (error "No project called \"%s\" in current group, which is \"%s\""
		 project
		 (car sidebrain-current-project-group))))))
   ((listp project)
    (unless (assoc (car project) sidebrain-current-project-group)
      (error "Trying to make \"%s\" the current project, but it is not part of the current group" (car project)))
    (setq sidebrain-current-project project))))

(defvar sidebrain-completing-read-project-history nil
  "History variable for sidebrain-completing-read-project.")

(defun sidebrain-completing-read-project (&optional group prompt auto-create default)
  "Prompt the user for a project from the given GROUP, or the current if none given.
PROMPT may also be specified, as may AUTO-CREATE and DEFAULT.
The result is the name of the selected project, as a string."
  ;; (with-output-to-temp-buffer (format "*Backtrace for project %S*" (gensym)) (backtrace))
  (let ((use-group (or group sidebrain-current-project-group)))
    (setq sidebrain-completing-read-project-history (mapcar 'car (cdr use-group)))
    (message "Using given group %S, using %S for completion and %S for history" group (cdr use-group) sidebrain-completing-read-project-history)
    (completing-read (if (stringp default)
			 (format (or prompt "Project (default %s) : ") default)
		       "Project: ")
		     (cdr use-group) 	; table
		     nil		; predicate
		     (not auto-create)	; require-match
		     nil		; initial-input
		     'sidebrain-completing-read-project-history	; history
		     default)))

(defun sidebrain-new-project (name)
  "Create and select a new project within the current group."
  (interactive "sProject name: ")
  (when (null sidebrain-current-project-group)
    (sidebrain-select-project-group))
  (when (assoc name (cdr sidebrain-current-project-group))
    (error "There is already a project called %s in project group %s!"
	   name (car sidebrain-current-project-group)))
  (message "Creating project \"%s\"" name)
  (setq sidebrain-current-project (cons name nil)
	sidebrain-current-stack nil)
  (rplacd sidebrain-current-project-group
	  (cons sidebrain-current-project
		(cdr sidebrain-current-project-group)))
  (when (interactive-p) (sidebrain-display)))

(defun sidebrain-delete-project (group project-name)
  "In project group GROUP, delete PROJECT-NAME.
If this was the current project, then no project will be current."
  (interactive
   (let* ((group (sidebrain-completing-read-project-group "Delete project from group: "))
	  (project (sidebrain-completing-read-project group "Delete project: ")))
     (list group project)))
  ;; todo: write internals of sidebrain-delete-project
  ;; what happens if this is the current one?
  ;; re-display if we were displaying (approximately -- only checks current buffer)

  (setq group (sidebrain-get-project-group group))

  (when (eq major-mode 'sidebrain-browse-tasks-mode)
    (sidebrain-browse-tasks sidebrain-browse-tasks-showing-all)))

(defmacro sidebrain-save-project-excursion (&rest body)
  "Run BODY, preserving which project and group we are in."
  `(let ((old-sidebrain-current-project-group sidebrain-current-project-group)
	(old-sidebrain-current-project sidebrain-current-project))
    (unwind-protect
	(progn
	  ,@body)
      (setq sidebrain-current-project old-sidebrain-current-project
	    sidebrain-current-project-group old-sidebrain-current-project-group))))

;;;; Quick reference to tasks as (project-group project task)
;;; also useful for fixed tasks for special purposes, such as coffee, lunch and tea!

(defun sidebrain-current-task-triplet ()
  "Return a list of the names of the current project group, project and task."
  (sidebrain-ensure-task-stack)
  (list (car sidebrain-current-project-group)
	(car sidebrain-current-project)
	(sidebrain-task-stack-name)))

(defun sidebrain-set-task-triplet (triplet)
  "Select a task given as (project-group project task).
Returns the previous task as a triplet."
  (let ((group (first triplet))
	(project (second triplet))
	(task (third triplet)))
    ;; (message "sidebrain-set-task-triplet called from: %s" (with-output-to-string (backtrace)))
    (message "Switching to %s in %s:%s" task group project)
    (let ((old (sidebrain-current-task-triplet)))
      (unless group
	(if sidebrain-switch-self-repair 
	    (setq group "General")
	  (error "No group specified")))
      ;; force the existence of the group and project
      (sidebrain-set-project-group group 'auto-create)
      (unless project
	(if sidebrain-switch-self-repair 
	    (setq project group)
	  (error "No project specified")))
      (sidebrain-set-project project 'auto-create)
      (unless task
	(if sidebrain-switch-self-repair
	    (setq task project)
	  (error "No task specified")))
      (if (assoc task (cdr sidebrain-current-project))
	  (progn
	    (message "task %s existed, resuming it" task)
	    (sidebrain-resume-task task t group project))
	(progn
	  (message "Task %s did non exist; suspending old task ready to create new task" task)
	  (sidebrain-suspend-task 'no-edit 'resuming-another)
	  (if (and (stringp task) (not (string= task "")))
	      (progn
		(message "Creating task %S" task)
		(sidebrain-begin-task task t))
	    (message "Non-string or empty string given as task to sidebrain-set-task-triplet"))))
      old)))

;;; end of sidebrain-projects.el
