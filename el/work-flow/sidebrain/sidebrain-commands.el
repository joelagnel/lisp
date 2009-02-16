;;;; sidebrain-commands.el -- main commands for sidebrain
;;; Time-stamp: <2006-05-05 12:15:14 john>

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

(provide 'sidebrain-commands)

(defun sidebrain-command-tidyup ()
  "Do things that may be necessary at the end of a command."
  (when (sidebrain-save-after-command)
    (sidebrain-save-to-file))
  (sidebrain-display)
  (sidebrain-make-visible))

;;;###autoload
(defun sidebrain-begin-task (task-string &optional same-project)
  "Begin a task described as TASK-STRING."
  (interactive "sTask: ")
  (sidebrain-ensure-task-stack)
  ;; (message "beginning task; stack is %S" (sidebrain-task-stack))
  (when (and (null (sidebrain-task-stack))
	     (not same-project))
    (let* ((group (sidebrain-completing-read-project-group "Project group (default %s): " t t))
	   (project (sidebrain-completing-read-project (assoc group sidebrain-project-groups)
						       "Project (default %s): " t
						       (if (eq group (car sidebrain-current-project-group))
							   (car sidebrain-current-project)
							 "General"))))
      (sidebrain-set-project-group group t)
      (sidebrain-set-project
project)))
  (unless (stringp task-string) (error "About to push non-string %S as a task" task-string))
  (let* ((file-name (sidebrain-ok-file-name (buffer-file-name)))
	 (line-number (count-lines (point-min) (point)))
	 (now (current-time))
	 (task-data (make-sidebrain-task :text task-string
					 :time-started (current-time)
					 :file  file-name
					 :line-number line-number))
	 (old-current (car (sidebrain-task-stack))))
    (if old-current
	(progn
	  ;; the old one is no longer current, so stop accounting current time to it
	  (sidebrain-stop-stopwatch old-current
				    'time-current
				    'time-started-current
				    'time-ended-current)
	  (sidebrain-increment-counter old-current 'subtasks)
	  (sidebrain-account-keystrokes-to-task old-current)
	  ;; remember where we were at the time
	  (setf (sidebrain-task-file old-current) (buffer-file-name)
		(sidebrain-task-line-number old-current) line-number)
	  )
      ;; we are beginning from an empty stack
      (setf (sidebrain-task-stack-name) task-string)
      (let ((stack (cdr sidebrain-current-stack)))
	(setf (sidebrain-task-stack-link-group stack) (first sidebrain-latest-non-special-task)
	      (sidebrain-task-stack-link-project stack) (second sidebrain-latest-non-special-task)
	      (sidebrain-task-stack-link-task stack) (third sidebrain-latest-non-special-task))))
    (sidebrain-start-stopwatch task-data 'time-spent 'time-started now)
    (sidebrain-start-stopwatch task-data 'time-current 'time-started-current now)
    (sidebrain-push-task task-data)
    (if (not (get-buffer sidebrain-buffer))
	(sidebrain-display)
      (set-buffer sidebrain-buffer)
      (goto-char (point-min))
      (let ((buffer-read-only nil))
	(sidebrain-put-task-property task-data 'display-start (point-marker))
	(insert (format sidebrain-task-format
			(sidebrain-task-text task-data)
			(sidebrain-task-extra-text task-data)))
	(sidebrain-put-task-property task-data 'display-end (point-marker)))
      (sidebrain-make-visible)))
  (sidebrain-command-tidyup))

;;;###autoload
(defun sidebrain-end-task (&optional no-display filter-functions)
  "End the topmost task on the stack.
With optional arg non-nil, don't update the display.
Second optional is functions to use instead of those on sidebrain-record-task-hook."
  (interactive)
  (let* ((task (car (sidebrain-task-stack))))
    (if task
	(let ((linked-triplet nil))
	  (sidebrain-stop-stopwatch task 'time-spent 'time-started 'time-ended)
	  ;; This is a bit like run-hook-with-args-until-failure, but each function
	  ;; after the first is called on the result of the previous one.
	  (let ((recorded task)
		(functions (or filter-functions
			       sidebrain-record-task-hook)))
	    (while (and functions recorded)
	      (setq recorded (funcall (car functions) recorded)
		    functions (cdr functions)))
	    (when recorded
	      (push recorded sidebrain-history)))
	  ;; now remove the task
	  (sidebrain-pop-task)
	  (let ((new-current (car (sidebrain-task-stack))))
	    ;; is there still anything on this task stack?
	    (if new-current
		(progn
		  ;; Yes, stack not empty yet. Start the timer on the new task.
		  (incf (sidebrain-task-subtasks new-current))
		  (sidebrain-start-stopwatch new-current 'time-current 'time-started-current))
	      ;; The stack is now empty.
	      ;; We usually want to clear the observations now, as the
	      ;; work they apply to has finished
	      (when (and sidebrain-clear-observations-on-emptying-stack
			 (sidebrain-task-stack-p (cdr sidebrain-current-stack))
			 (sidebrain-observations)
			 (or (eq sidebrain-clear-observations-on-emptying-stack t)
			     (and (not (eq sidebrain-clear-observations-on-emptying-stack nil))
				  (y-or-n-p "Clear observations? "))))
		(setf (sidebrain-task-stack-observations (cdr sidebrain-current-stack))
		      nil))
	      ;; If this stack was "linked" from another, offer to resume the linked stack.
	      (let ((task-struct (cdr sidebrain-current-stack)))
		(when (sidebrain-task-stack-p task-struct)
		  (let ((link-group (sidebrain-task-stack-link-group task-struct))
			(link-project (sidebrain-task-stack-link-project task-struct))
			(link-task (sidebrain-task-stack-link-task task-struct)))
		    (when (and (stringp link-group)
			       (stringp link-project)
			       (stringp link-task)
			       (yes-or-no-p
				(format "Resume %s:%s:%s? "
					link-group link-project link-task)))
		      (setq linked-triplet (list link-group link-project link-task))))))
	      ;; remove it from its project
	      (sidebrain-delete-current-task-stack)))
	  ;; if the task came from a todo comment in a file, remove it from the file
	  (let ((file (sidebrain-ok-file-name (sidebrain-get-task-property task 'file))))
	    ;; (message "origin file is %S" file)
	    (when (and (stringp file) (file-writable-p file))
	      (sidebrain-file-comment-mark-done task file)))
	  ;; update the task stack display
	  (if (not (get-buffer sidebrain-buffer))
	      (sidebrain-display)
	    (set-buffer sidebrain-buffer)
	    ;; todo: ought to be able to use the new one unconditionally, I hope!
	    (if (and (sidebrain-get-task-property task 'display-start)
		     (sidebrain-get-task-property task 'display-end)) 
		(let ((buffer-read-only nil))
		  (delete-region (sidebrain-get-task-property task 'display-start)
				 (sidebrain-get-task-property task 'display-end)))
	      (progn
		(error "No display region defined for task")
		;; old code (from before display-start, display-end markers) follows...
		;; leave it for now in case it's useful later
		(goto-char (point-min))
		(beginning-of-line 2)
		(let ((buffer-read-only nil))
		  (delete-region (point-min) (point)))))
	    (sidebrain-make-visible))
	  ;; If it was an information-gathering task (has question
	  ;; syntax) as the user for the information found:
	  (let ((task-text (sidebrain-task-text task)))
	    (if (and (stringp sidebrain-auto-ask-info-gathering-results)
		     (string-match sidebrain-auto-ask-info-gathering-results task-text))
		(sidebrain-observe
		 (read-from-minibuffer
		  (format "What did you find about %s%s "
			  task-text
			  (if (string-match "\\?[ ]*$" task-text) "" "?"))))))
	  ;; If there was a linked task, we now switch to it:
	  (if linked-triplet
	      (sidebrain-set-task-triplet linked-triplet)
	    (sidebrain-command-tidyup)))
      (message "No task to end"))
    task))

;;;###autoload
(defun sidebrain-abandon-task (&optional no-display)
  "End the topmost task on the stack.
With optional arg non-nil, don't update the display."
  (interactive)
  (when (car (sidebrain-task-stack))
    (rplaca (sidebrain-task-stack)
	    (propertize (car (sidebrain-task-stack))
			'abandoned t)))
  (sidebrain-end-task no-display sidebrain-record-abandon-task-hook))

(defvar sidebrain-reminder-latest-project-group-name nil
  "The name of the latest project group into which a reminder has been entered.")

(defvar sidebrain-reminder-latest-project-name nil
  "The name of the latest project into which a reminder has been entered.")

(defvar sidebrain-latest-non-special-task nil
  "The most recent task triplet that was not in the project group \"special\".")

;;;###autoload
(defun sidebrain-reminder (reminder-string &optional file line project-group project)
  "Enter a reminder described as REMINDER-STRING.
If FILE and LINE are given, mark the reminder as having come from there.
If PROJECT-GROUP and PROJECT are given, enter the reminder in that project.
These may be given as names to look up in the appropriate lists,
or conses of name and list, for efficiency if entering reminders in bulk
from a program."
  (interactive "sReminder: ")
  ;; todo: do we need to protect against quotes in strings for reminders? check that we can read and write them!
  ;; (if (string-match "\"" reminder-string) (error "Quote not allowed here"))
  (sidebrain-save-project-excursion
   (cond
    ((null project-group)
     (sidebrain-set-project-group
      (sidebrain-completing-read-project-group
       (if (stringp sidebrain-reminder-latest-project-group-name)
	   (format "Project group to put reminder \"%s\" in (default %s) : "
	       reminder-string
	       sidebrain-reminder-latest-project-group-name)
	 (format "Project group to put reminder \"%s\" in: "
	       reminder-string))
       sidebrain-reminder-latest-project-group-name
       t)
      t))
    ((stringp project-group)
     (setq sidebrain-current-project-group (sidebrain-get-project-group project-group))
     (when (null sidebrain-current-project-group)
       (sidebrain-new-project-group project-group)))
    ((consp project-group)
     (setq sidebrain-current-project-group project-group)))
   (setq sidebrain-reminder-latest-project-group-name (car sidebrain-current-project-group))
   (message "Sidebrain-reminder has temporarily set project group name to %s and project group to %S"
	    sidebrain-reminder-latest-project-group-name
	    (car sidebrain-current-project-group))
   (cond
    ((null project)
     (sidebrain-set-project
      (sidebrain-completing-read-project
       nil				; group -- use current
       (if (and (stringp sidebrain-reminder-latest-project-name)
		(assoc sidebrain-reminder-latest-project-name (cdr sidebrain-current-project-group)))
	   (format "Project (in group %s) to put reminder \"%s\" in (default %s) : "
		   (car sidebrain-current-project-group)
		   reminder-string
		   sidebrain-reminder-latest-project-name)
	 (format "Project (in group %s) to put reminder \"%s\" in: "
		 (car sidebrain-current-project-group)
		 reminder-string))
       t
       sidebrain-reminder-latest-project-name)
      t))
    ((stringp project)
     (setq sidebrain-current-project (assoc project sidebrain-current-project-group))
     (when (null sidebrain-current-project)
       (sidebrain-new-project project)))
    ((consp project)
     (setq sidebrain-current-project project)))
   (setq sidebrain-reminder-latest-project-name (car sidebrain-current-project))
   (message "Reminder %S will go into %S:%S"
	    reminder-string
	    (car sidebrain-current-project-group)
	    (car sidebrain-current-project))
   (string-match sidebrain-nested-tasks-pattern reminder-string)
   (let* ((base (match-string-no-properties 1 reminder-string)))
     (if (assoc base (cdr sidebrain-current-project))
	 (message "Already got reminder for %s" reminder-string)
       (push (cons base
		   (make-sidebrain-task-stack
		    :tasks (sidebrain-nested-tasks reminder-string file line)))
	     (cdr sidebrain-current-project))
       (message "Added reminder \"%s\" to project \"%s:%s\""
		base
		(car sidebrain-current-project-group)
		(car sidebrain-current-project)))))
  (sidebrain-command-tidyup))

;;;###autoload
(defun sidebrain-suspend-task (&optional no-edit resuming-another)
  "Suspend the current task stack.

With optional argument non-nil, don't ask for a name under which to suspend it,
but use the base task of the stack.

Second optional argument warns that we are being called from
sidebrain-resume-task, or similar.

Otherwise, suspending a \"special\" task (one whose project-group is
called \"special\") will resume the task that was active before it.

Returns the name under which it was suspended."
  (interactive)
  (let ((initial-label (car sidebrain-current-stack)))
    ;; (message "Existing label for task being suspended is \"%S\"" initial-label)
    (let ((top-task (car (sidebrain-task-stack)))
	  (group (car sidebrain-current-project-group))
	  (label (if no-edit
		     initial-label
		   (rplaca sidebrain-current-stack
			   (read-from-minibuffer "Label for suspended task: "
						 initial-label)))))
      ;; update the "which file we were in" field of the task
      (when top-task
	(setf (sidebrain-task-file top-task) buffer-file-name
	      (sidebrain-task-line-number top-task) (count-lines (point-min) (point))))
      (run-hook-with-args 'sidebrain-suspend-hook (cdr sidebrain-current-stack))
      (push sidebrain-current-stack sidebrain-suspension-history)
      (setq sidebrain-current-stack nil)
      (if (equal group "special")
	  (unless resuming-another
	    (sidebrain-set-task-triplet sidebrain-latest-non-special-task))
	(setq sidebrain-latest-non-special-task (sidebrain-current-task-triplet)))
      (sidebrain-command-tidyup)
      label)))

(defun sidebrain-read-resumable-task ()
  "Read the arguments needed for sidebrain-resume-task."
  (let* ((completion-ignore-case t)
	 (group-name (sidebrain-completing-read-project-group "Project group to resume task from (default %s) : " t))
	 (group (sidebrain-get-project-group group-name))
	 (project-name (sidebrain-completing-read-project
			group
			"Project to resume task from (default %s) : "
			nil 
			;; make a default if possible
			(if (eq group sidebrain-current-project-group)
			    (car sidebrain-current-project)
			  nil)))
	 (project (assoc project-name group))
	 (choices-alist (cdr project))
	 (labels (mapcar 'car choices-alist))
	 (nearest (sidebrain-nearest-task-comment))
	 (nearest-position (position nearest labels :test 'string=))
	 (resume-task-history-hack labels))
    (unless (assoc nearest (cdr project))
      (setq nearest nil))
    (list (completing-read "Resume task: "
			   choices-alist ; table
			   nil		; predicate
			   t		; require-match
			   nearest	; initial-input
			   (if (and nearest nearest-position)
			       (cons 'resume-task-history-hack nearest-position)
			     'resume-task-history-hack) ; hist
			   )
	  t group project)))

;;;###autoload
(defun sidebrain-resume-task (task-name &optional no-edit project-group project)
  "Resume the stack described as TASK-NAME.
If there was a task active, it is suspended.
Returns the label of the suspended task, if there was one.
With optional second argument non-nil, don't ask about the name under which to suspend
the suspended task, but use the base task of that stack.
If TASK-NAME is empty, the task stack is left empty."
  (interactive (sidebrain-read-resumable-task))
  (message "Resuming \"%s\" (in \"%s:%s\")" task-name (or project-group "<no group>") (or project "<no project>"))
  (let ((previous-group (car sidebrain-current-project-group)))
    (unless (equal previous-group "special")
      (setq sidebrain-latest-non-special-task (sidebrain-current-task-triplet)))
    (let ((suspended-task (if (sidebrain-valid-stack)
			      (progn
				(message "implicitly suspending %s" (car sidebrain-current-stack))
				(sidebrain-suspend-task no-edit t))
			    nil)))
      (when project-group (sidebrain-set-project-group project-group))
      (when project (sidebrain-set-project project))
      (let* ((found (assoc task-name sidebrain-current-project))
	     (resuming-label (car found))
	     (resuming-task-stack (cdr found))
	     (resuming-current-task (if (sidebrain-task-stack-p resuming-task-stack)
					(car (sidebrain-task-stack-tasks resuming-task-stack))
				      nil))
	     (file (if resuming-current-task
		       (sidebrain-ok-file-name
			(sidebrain-task-file resuming-current-task))
		     nil))
	     (line-number (if resuming-current-task
			      (sidebrain-task-line-number resuming-current-task)
			    nil)))
	(if (string= task-name "")
	    (message "No new task")
	  (setq sidebrain-current-stack found)
	  (when resuming-current-task
	    (sidebrain-start-stopwatch resuming-current-task 'time-current 'time-started-current))
	  (dolist (task (sidebrain-task-stack-tasks resuming-task-stack))
	    (sidebrain-start-stopwatch task 'time-this-time 'time-started))
	  (when (and (stringp file) (file-readable-p file))
	    (find-file file)
	    (if (numberp line-number)
		(goto-line line-number)
	      ;; if we don't know a line number, look for the task name as a string, in case
	      ;; we got this task from a reminder comment in the code
	      (let ((was (point)))
		(goto-char (point-min))
		(unless (search-forward task-name (point-max) t)
		  (goto-char was)))))
	  (run-hook-with-args 'sidebrain-resume-hook (car found))
	  (sidebrain-command-tidyup))
	suspended-task))))

(defun sidebrain-new-task-stack (task-string)
  "Begin a new task stack, based on TASK-STRING.
This suspends the old task stack, and begins the new one.
On ending the new one, the old one is offered for resumption."
  (interactive "sTask: ")
  (sidebrain-suspend-task t t)
  (sidebrain-begin-task task-string))

;;;; Remove a task from a project

;;;###autoload
(defun sidebrain-delete-task-stack (task project)
  "Delete TASK stack from PROJECT.
TASK may be a task label, task defstruct, or a cons of the two."
  (interactive
   (list (completing-read "Delete task stack: " (cdr sidebrain-current-project) t)
	 sidebrain-current-project))
  (let* ((project-tasks (cdr project))
	 (trailer project))
    (while project-tasks
      (if (or (and (stringp task) (string= task (caar project-tasks)))
	      (and (consp task) (equal task (car project-tasks)))
	      (and (sidebrain-task-stack-p task) (eq task (cdr project-tasks))))
	  (progn
	    (rplacd trailer (cdr project-tasks))
	    (setq project-tasks nil)
	    (sidebrain-command-tidyup))
	(setq trailer project-tasks
	      project-tasks (cdr project-tasks))))))

;;;###autoload
(defun sidebrain-delete-current-task-stack ()
  "Delete the current task stack."
  (interactive)
  (sidebrain-delete-task-stack sidebrain-current-stack
			       sidebrain-current-project))

;;;###autoload
(defun sidebrain-observe (observation)
  "Observe OBSERVATION."
  (interactive "sObservation: ")
  (sidebrain-add-observation observation)
  (sidebrain-command-tidyup))

;;; end of sidebrain-commands.el
