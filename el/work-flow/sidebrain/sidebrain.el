;;;; sidebrain.el -- programmer's memory aide
;;; Time-stamp: <2006-05-05 18:06:06 john>
;;; see also sidebrain-browse.el

;; Author: John Sturdy
;; Original body of code written 2005-01-06
;; Original work: all my own work; some ideas contributed by Chris Exton

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

;;; Acknowledgement: Part of the B4STEP project of the SFI (Science
;;; Foundation Ireland) at University of Limerick

(provide 'sidebrain)

(require 'cl)
(require 'time-date)
(require 'xml)

(defconst sidebrain-version "1.0"
  "The version number for this version of sidebrain.
This is written into the saved data, but not currently used when the data is loaded.
It could later be used to determine compatibility between implementations and data.")

(autoload 'sidebrain-display "sidebrain-display"
  "Display the current task stack, etc.
Creates the buffer as needed." t)

(require 'sidebrain-custom)
(require 'sidebrain-vars)
(require 'sidebrain-macros)
(require 'sidebrain-projects)
(require 'sidebrain-xml)
(require 'sidebrain-effort)
(require 'sidebrain-display)
(require 'sidebrain-commands)
(require 'sidebrain-todo)
(require 'sidebrain-menu)

;; todo: deletion and editing of observations
;; todo: sort out order of entries in saved stacks
;; todo: write manual for sidebrain, in TeXinfo
;; todo: priorities in task queue
;; todo: count the time each task spends on the stack, and the time it spends at the top of the stack
;; todo: integrate with timeclock.el in the emacs distribution?
;; todo: suspend and resume to be able to splice part way up the current stack
;; todo: buttons, mouseover etc
;; todo: use Info-file-list-for-emacs

(defun sidebrain-ensure-task-stack (&optional ask)
  "Ensure there is a task stack data structure."
  (when (null sidebrain-current-stack)
    (save-window-excursion		; is this really needed?
      (when (or ask
		(null sidebrain-current-project-group))
	(if ask
	    (sidebrain-set-project-group
	     (sidebrain-completing-read-project-group "Project group for new task: ")
	     t)
	  (sidebrain-set-project-group "General" t)))
      (when (or ask
		(null sidebrain-current-project))
	(if ask
	    (sidebrain-set-project
	     (sidebrain-completing-read-project "Project for new task: ")
	     t)
	  (sidebrain-set-project (car sidebrain-current-project-group) ; use group name as default project name
				 t)))
      (let* ((new-name (car sidebrain-current-project))
	     (new-pair (assoc new-name (cdr sidebrain-current-project))))
	(when (null new-pair)
	  (setq new-pair (cons new-name	; use project name as default task name
			       (make-sidebrain-task-stack)))
	  (pushnew new-pair
		   (cdr sidebrain-current-project) :test 'equal)
	  ;; (with-output-to-temp-buffer (symbol-name (gensym "*Backtrace for new task ")) (backtrace))
	  ;; The stack can look like this:
	  ;;   sidebrain-ensure-task-stack()
	  ;;   sidebrain-current-task-triplet()
	  ;;   sidebrain-suspend-task(t t)
	  ;;   sidebrain-resume-task("versor" t "research writing" "other paper ...
	  ;;   sidebrain-set-task-triplet(("research writing" "other papers" "v ...
	  ;;   (progn (message "Typing break is on task stack, suspending it")  ...
	  ;;   (if (equal top-task-text sidebrain-type-break-string) (progn (me ...
	  ;;   (when (equal top-task-text sidebrain-type-break-string) (message ...
	  ;;   (let* ((top-task ...) (top-task-text ...)) (message "comparing % ...
	  ;;   sidebrain-end-type-break()
	  ;;   run-hooks(type-break-end-break-hook)

	  )
	(setq sidebrain-current-stack new-pair)))))

(defun sidebrain-choose-task (&optional prompt)
  "Choose and return a task from within the current project, prompting with PROMPT if given."
  (completing-read (or prompt "Task: ")
		   (cdr sidebrain-current-project)
		   nil
		   t))

;;;; task properties

(defun sidebrain-put-task-property (task key value)
  "For TASK, set property KEY to VALUE."
  (if (stringp task)
      (put-text-property 0 (1- (length task))
			 key value
			 task)
    (eval (list 'setf
		(list (intern (concat "sidebrain-task-" (symbol-name key)))
		      task)
		'value))))

(defun sidebrain-get-task-property (task key)
  "For TASK, get the value for property KEY."
  (if (stringp task)
      (get-text-property 0 key task)
    (eval (list (intern (concat "sidebrain-task-" (symbol-name key)))
		task))))

(defun sidebrain-increment-counter (task counter)
  "On TASK, increment the COUNTER property.
Return its new value."
  (let* ((old-value (sidebrain-get-task-property task counter))
	 (new-value (if old-value
			(1+ old-value)
		      1)))
    (sidebrain-put-task-property task counter new-value)
    new-value))

(defun sidebrain-ok-file-name (filename)
  "Return FILENAME unless it is one we prefer not to record...
i.e. do not make the sidebrain data file be the file for any task."
  ;; (message "(sidebrain-ok-file-name %S)" filename)
  (if (and (stringp filename)
	   (not (string= filename ""))
	   (not (string-match "[\n\r]" filename)))
      (let ((truename (file-truename filename)))
	(if (or (string= truename (file-truename sidebrain-file-name)))
	    nil
	  filename))
    nil))

(defun sidebrain-nested-tasks (string &optional file line-number)
  "Make STRING into a nested tasks list.
If FILE and LINE-NUMBER are given, indicate that is where the tasks belong."
  (string-match sidebrain-nested-tasks-pattern string)
  (let ((further (match-string 3 string)))
    (if further
	(cons (make-sidebrain-task :text (match-string 1 string)
				   :file file
				   :line-number line-number)
	      (sidebrain-nested-tasks further))
      (list (make-sidebrain-task :text string
			   :file file
			   :line-number line-number)))))

(defun uniquify-against-alist (key alist)
  "Return KEY modified to make it different from any keys of ALIST.
KEY must be a string."
  (let ((i 1)
	(newkey key))
    (while (assoc newkey alist)
      (incf i)
      (setq newkey (format "%s<%d>" key i)))
    newkey))

(autoload 'sidebrain-browse-tasks "sidebrain-browse"
  "Browse the task queue, and perhaps select a task." t)

;; Uncomment this line, or put the same into your .emacs, to turn on the Sidebrain menu
;; (define-key global-map [menu-bar sidebrain] '(menu-item "Sidebrain" sidebrain-menu))

;;; end of sidebrain.el
