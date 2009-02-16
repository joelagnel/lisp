;;;; sidebrain-browse.el -- browser for sidebrain.el
;;; Time-stamp: <2006-05-05 15:48:13 john>

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

(provide 'sidebrain-browse)
(require 'sidebrain)

;;;; browse tasks (and major mode thereunto)

(defvar sidebrain-browse-tasks-mode-keymap (make-keymap "Browse tasks")
  "Major mode for browsing tasks.")

(suppress-keymap sidebrain-browse-tasks-mode-keymap)

(defun sidebrain-browse-tasks-move-to-label ()
  "Move to the label of the current task, and return it."
  (let* ((pattern "^    \\([^ ].+\\)$")
	 (whole-line (if (or (looking-at pattern)
			     (re-search-backward pattern (point-min) t))
			 (match-string-no-properties 1)
		       nil)))
    (if (string-match "\\(.+\\)\\( ([^()]+)\\)" whole-line)
	(match-string 1 whole-line)
      whole-line)))

(defvar sidebrain-browse-tasks-project-group-regexp "^Project group \"\\(.+\\)\".*:"
  "Pattern for finding the start of a project group.")

(defvar sidebrain-browse-tasks-project-group-start nil
  "Start of the project group.")

(defvar sidebrain-browse-tasks-project-group-end nil
  "End of the project group.")

(defun sidebrain-browse-tasks-get-project-group ()
  "Find the project group that the current task is part of."
  (save-excursion
    (if (re-search-backward sidebrain-browse-tasks-project-group-regexp (point-min) t)
	;; found the start of it
	(let* ((start (match-beginning 0))
	       (group-name (match-string-no-properties 1)))
	  (forward-char 1)
	  ;; now find the end of it
	  (if (re-search-forward sidebrain-browse-tasks-project-group-regexp (point-max) t)
	      ;; not the last one
	      (setq sidebrain-browse-tasks-project-group-start start
		    sidebrain-browse-tasks-project-group-end
		    (progn
		      (goto-char (match-beginning 0))
		      (when sidebrain-browse-tasks-double-spaced
			(beginning-of-line -2))
		      (point)))
	    ;; the last one in the buffer
	    (setq sidebrain-browse-tasks-project-group-start start
		  sidebrain-browse-tasks-project-group-end (point-max)))
	  group-name)
      nil)))

(defvar sidebrain-browse-tasks-project-regexp "^  Project \"\\(.+\\)\".*:"
  "Pattern for finding the start of a project.")

(defvar sidebrain-browse-tasks-project-start nil
  "Start of the project group.")

(defvar sidebrain-browse-tasks-project-end nil
  "End of the project.")

(defun sidebrain-browse-tasks-get-project ()
  "Find the project that the current task is part of."
  (save-excursion
    (if (re-search-backward sidebrain-browse-tasks-project-regexp (point-min) t)
	;; found the start of it
	(let* ((start (match-beginning 0))
	       (project-name (match-string-no-properties 1)))
	  (forward-char 1)
	  ;; now look for the end of it
	  (if (re-search-forward sidebrain-browse-tasks-project-regexp (point-max) t)
	      ;; not the last one
	      (setq sidebrain-browse-tasks-project-start start
		    sidebrain-browse-tasks-project-end
		    (progn
		      (goto-char (match-beginning 0))
		      (when sidebrain-browse-tasks-double-spaced
			(beginning-of-line -1))
		      (point)))
	    ;; the last one in the buffer
	    (setq sidebrain-browse-tasks-project-start start
		  sidebrain-browse-tasks-project-end (point-max)))
	  (if (> sidebrain-browse-tasks-project-end
		 sidebrain-browse-tasks-project-group-end)
	      (setq sidebrain-browse-tasks-project-end sidebrain-browse-tasks-project-group-end))
	  project-name)
      nil)))

(defun sidebrain-browse-tasks-select ()
  "Resume the selected task."
  (interactive)
  (let ((label (sidebrain-browse-tasks-move-to-label))
	(group (sidebrain-browse-tasks-get-project-group))
	(project (sidebrain-browse-tasks-get-project)))
    (message "Selecting %S in %S:%S" label group project)
    (when (and label group project)
      (bury-buffer)
      (sidebrain-resume-task label t group project))))

(defvar sidebrain-browse-tasks-project-overlay nil
  "Overlay for showing the current project selection.")

(defvar sidebrain-browse-tasks-project-group-overlay nil
  "Overlay for showing the current project group selection.")

(defvar sidebrain-browse-tasks-stack-overlay nil
  "Overlay for showing the current stack selection.")

(defun sidebrain-browse-tasks-update-highlight ()
  "Update the highlighting, if any, for sidebrain-browse-tasks."
  (sidebrain-browse-tasks-get-project-group)
  (if (and sidebrain-browse-tasks-project-group-start
	   sidebrain-browse-tasks-project-group-end)
      (if sidebrain-browse-tasks-project-group-overlay
	  (move-overlay sidebrain-browse-tasks-project-group-overlay
			sidebrain-browse-tasks-project-group-start
			sidebrain-browse-tasks-project-group-end)
	(setq sidebrain-browse-tasks-project-group-overlay
	      (make-overlay sidebrain-browse-tasks-project-group-start
			    sidebrain-browse-tasks-project-group-end))
	(overlay-put sidebrain-browse-tasks-project-group-overlay
		     'face
		     (cons 'background-color "light goldenrod")))
    (delete-overlay sidebrain-browse-tasks-project-group-overlay)
    (setq sidebrain-browse-tasks-project-group-overlay nil))
  (sidebrain-browse-tasks-get-project)
  (if (and sidebrain-browse-tasks-project-start
	   sidebrain-browse-tasks-project-end)
      (if sidebrain-browse-tasks-project-overlay
	  (move-overlay sidebrain-browse-tasks-project-overlay
			sidebrain-browse-tasks-project-start
			sidebrain-browse-tasks-project-end)
	(setq sidebrain-browse-tasks-project-overlay
	      (make-overlay sidebrain-browse-tasks-project-start
			    sidebrain-browse-tasks-project-end))
	(overlay-put sidebrain-browse-tasks-project-overlay
		     'face
		     (cons 'background-color "goldenrod")))
    (delete-overlay sidebrain-browse-tasks-project-overlay)
    (setq sidebrain-browse-tasks-project-overlay nil))
  (save-excursion
    (let* ((stack-start (point))
	   (stack-end (if (eobp)
			  (point)
			(save-excursion
			  (forward-char 1)
			  ;; look for the end of this task
			  (if (re-search-forward sidebrain-browse-tasks-stack-start-regexp
						 (point-max) t)
			      ;; not the last one in the buffer
			      (progn
				(match-beginning 0)
				(if sidebrain-browse-tasks-double-spaced
				    (beginning-of-line 0)
				  (beginning-of-line 1))
				(point))
			    ;; the last one in the buffer
			    (progn
			      (goto-char (point-max))
			      (when sidebrain-browse-tasks-double-spaced
				(beginning-of-line -2))
			      (point))))))
	   )
      ;; (message "start=%S end=%S" stack-start stack-end)
      (when (and stack-start stack-end)
	(when (> stack-end sidebrain-browse-tasks-project-end)
	  (setq stack-end sidebrain-browse-tasks-project-end))
	(if (overlayp sidebrain-browse-tasks-stack-overlay)
	    (move-overlay sidebrain-browse-tasks-stack-overlay stack-start stack-end)
	  (setq sidebrain-browse-tasks-stack-overlay
		(make-overlay stack-start stack-end))
	  (overlay-put sidebrain-browse-tasks-stack-overlay
		       'face (cons 'background-color "yellow")))))))

(defvar sidebrain-browse-tasks-stack-start-regexp "^    [^ ]"
  "Pattern for recognizing the start of a task stack.")

(defun sidebrain-browse-tasks-next ()
  "Select the next task."
  (interactive)
  (let ((next (save-excursion
		(end-of-line 1)
		(re-search-forward sidebrain-browse-tasks-stack-start-regexp
				   (point-max) t))))
    (while (and next
		(get-text-property next 'invisible))
      (end-of-line 1)
      (setq next (re-search-forward sidebrain-browse-tasks-stack-start-regexp
				    (point-max) t)))
    (if next
	(progn
	  (goto-char next)
	  (beginning-of-line 1)
	  (message "In project %s:%s"
		   (sidebrain-browse-tasks-get-project-group)
		   (sidebrain-browse-tasks-get-project)))
      (message "No further tasks")))
  (sidebrain-browse-tasks-update-highlight))

(defun sidebrain-browse-tasks-previous ()
  "Select the previous task."
  (interactive)
  (let ((previous (save-excursion
		    (beginning-of-line 0)
		    (re-search-backward sidebrain-browse-tasks-stack-start-regexp (point-min) t))))
    (while (and previous
		(get-text-property previous 'invisible))
      (beginning-of-line 0)
      (setq previous (re-search-backward sidebrain-browse-tasks-stack-start-regexp
					 (point-min) t)))
    (if previous
	(progn
	  (goto-char previous)
	  (beginning-of-line 1)
	  (message "In project %s:%s" (sidebrain-browse-tasks-get-project-group) (sidebrain-browse-tasks-get-project)))
      (message "No previous tasks")))
  (sidebrain-browse-tasks-update-highlight))

(defun sidebrain-browse-tasks-next-project ()
  "Select the next project."
  (interactive)
  (if (re-search-forward "^  Project \"" (point-max) t)
      (sidebrain-browse-tasks-next)
    (message "No further projects")))

(defun sidebrain-browse-tasks-previous-project ()
  "Select the previous project."
  (interactive)
  (if (and (re-search-backward "^  Project \"" (point-min) t)
	   (re-search-backward "^  Project \"" (point-min) t))
      (sidebrain-browse-tasks-next)
    (message "No previous projects")))

(defun sidebrain-browse-tasks-new-project (name)
  "Make a new project group, and update the browser display."
  (interactive "sNew project name: ")
  (let ((current-browsing-group (sidebrain-browse-tasks-get-project-group)))
    (message "New project is in group %S" current-browsing-group)
    (sidebrain-set-project-group current-browsing-group)
    (sidebrain-new-project name)
    (sidebrain-reminder name nil nil current-browsing-group name)
    (sidebrain-browse-tasks sidebrain-browse-tasks-showing-all
			    sidebrain-browse-tasks-specific-groups
			    sidebrain-browse-tasks-specific-projects
			    nil name nil)))

(defun sidebrain-browse-tasks-next-project-group ()
  "Select the next project group."
  (interactive)
  (if (re-search-forward "^Project group \"" (point-max) t)
      (sidebrain-browse-tasks-next)
    (message "No further project groups")))

(defun sidebrain-browse-tasks-previous-project-group ()
  "Select the previous project."
  (interactive)
  (if (and (re-search-backward "^Project group \"" (point-min) t)
	   (re-search-backward "^Project group \"" (point-min) t))
      (sidebrain-browse-tasks-next)
    (message "No previous project groups")))

(defun sidebrain-browse-tasks-new-project-group (name)
  "Make a new project group, and update the browser display."
  (interactive "sNew project group name: ")
  (sidebrain-new-project-group name)
  (sidebrain-new-project name)
  (sidebrain-reminder name nil nil name name)
  (sidebrain-browse-tasks sidebrain-browse-tasks-showing-all
			  sidebrain-browse-tasks-specific-groups
			  sidebrain-browse-tasks-specific-projects
			  name nil nil))

(defun sidebrain-browse-tasks-delete ()
  "Delete the current task."
  (interactive)
  (let ((label (sidebrain-browse-tasks-move-to-label))
	(group (sidebrain-browse-tasks-get-project-group))
	(project (sidebrain-browse-tasks-get-project)))
    ;; (message "got %S" label)
    (when label
      (let ((where (point)))
	(sidebrain-set-project-group group)
	(sidebrain-set-project project)
	(sidebrain-remove-tasks-labelled label)
	(sidebrain-browse-tasks)
	(when (<= where (point-max))
	  (goto-char where))
	(sidebrain-browse-tasks-update-highlight)))))

;;;; grep-like filter for tasks

(defun sidebrain-grep-tasks (pattern)
  "Restrict the display of tasks to those matching PATTERN."
  (interactive "sDisplay only tasks matching regexp: ")
  (let ((buffer-read-only nil))
    (goto-char (point-min))
    (let ((full-pattern (format "^    .*\\(%s\\).*$" pattern))
	  (wanted nil))
      (while (not (eobp))
	(cond
	 ((looking-at sidebrain-browse-tasks-stack-start-regexp)
	  (setq wanted (looking-at full-pattern))
	  (unless wanted
	    (put-text-property (point-at-bol) (1+ (point-at-eol)) 'invisible t)))
	 ((looking-at "^      ")
	  (unless wanted
	    (put-text-property (point-at-bol) (1+ (point-at-eol)) 'invisible t))))
	(beginning-of-line 2)))))

(defun sidebrain-ungrep-tasks ()
  "Undo the effect of sidebrain-grep-tasks."
  (interactive)
  (let ((buffer-read-only nil))
    (put-text-property (point-min) (point-max) 'invisible nil)))

;;;; marking tasks

(defvar sidebrain-browse-tasks-marked nil
  "The currently marked tasks.")

(defun sidebrain-browse-tasks-marked ()
  "Return the list of marked tasks."
  (nreverse
   (mapcar (lambda (label)
	     (assoc label (cdr sidebrain-current-project)))
	   sidebrain-browse-tasks-marked)))

(defun sidebrain-browse-tasks-mark ()
  "Mark the selected task."
  (interactive)
  (save-excursion
    (let* ((label (sidebrain-browse-tasks-move-to-label))
	   (here (match-beginning 0)))
      (when label
	(pushnew label sidebrain-browse-tasks-marked :test 'string=)
	(re-search-forward "^$" (point-max) t)
	(put-text-property here (point) 'face (cons 'background-color "yellow")))))
  (message (substitute-command-keys
    "\\<sidebrain-browse-tasks-mode-keymap>\\[sidebrain-browse-tasks-unmark] to unmark, \\[sidebrain-mail-tasks] to mail marked tasks")))
  
(defun sidebrain-browse-tasks-unmark ()
  "Unmark the selected task."
  (interactive)
  (save-excursion
    (let ((label (sidebrain-browse-tasks-move-to-label))
	  (here (match-beginning 0)))
      (when label
	(setq sidebrain-browse-tasks-marked
	      (delete-if (lambda (task) (string= task label))
			 sidebrain-browse-tasks-marked))
	(re-search-forward "^$" (point-max) t)
	(put-text-property here (point) 'face (cons 'background-color nil))))))

(define-key sidebrain-browse-tasks-mode-keymap "" 'sidebrain-browse-tasks-select)
(define-key sidebrain-browse-tasks-mode-keymap "n" 'sidebrain-browse-tasks-next)
(define-key sidebrain-browse-tasks-mode-keymap "p" 'sidebrain-browse-tasks-previous)
(define-key sidebrain-browse-tasks-mode-keymap "N" 'sidebrain-browse-tasks-next-project)
(define-key sidebrain-browse-tasks-mode-keymap "P" 'sidebrain-browse-tasks-previous-project)
(define-key sidebrain-browse-tasks-mode-keymap "\M-n" 'sidebrain-browse-tasks-next-project-group)
(define-key sidebrain-browse-tasks-mode-keymap "\M-p" 'sidebrain-browse-tasks-previous-project-group)
(define-key sidebrain-browse-tasks-mode-keymap "J" 'sidebrain-browse-tasks-new-project)
(define-key sidebrain-browse-tasks-mode-keymap "G" 'sidebrain-browse-tasks-new-project-group)
(define-key sidebrain-browse-tasks-mode-keymap "g" 'sidebrain-grep-tasks)
(define-key sidebrain-browse-tasks-mode-keymap "f" 'sidebrain-ungrep-tasks)
(define-key sidebrain-browse-tasks-mode-keymap "q" 'bury-buffer)
(define-key sidebrain-browse-tasks-mode-keymap "d" 'sidebrain-browse-tasks-delete)
(define-key sidebrain-browse-tasks-mode-keymap "m" 'sidebrain-browse-tasks-mark)
(define-key sidebrain-browse-tasks-mode-keymap "u" 'sidebrain-browse-tasks-unmark)
(define-key sidebrain-browse-tasks-mode-keymap "M" 'sidebrain-mail-tasks)

(defvar sidebrain-browse-tasks-showing-all nil
  "Whether we are showing all task information.
Used to make re-display re-display the same way.")

(defvar sidebrain-browse-tasks-specific-groups nil
  "The specific groups list we are browsing, if any.
Used to make re-display re-display the same way.")

(defvar sidebrain-browse-tasks-specific-projects nil
  "The specific groups list we are browsing, if any.
Used to make re-display re-display the same way.")

;;;###autoload
(defun sidebrain-browse-tasks (&optional show-all
					 specific-groups specific-projects
					 start-at-group start-at-project start-at-task)
  "Browse the task queue, and perhaps select a task.
With optional argument, display all groups and projects, even when empty.
Further args are SPECIFIC-GROUPS SPECIFIC-PROJECTS and
START-AT-GROUP START-AT-PROJECT START-AT-TASK.
The SPECIFIC- arguments give alternative lists of groups and projects,
and if you specify SPECIFIC-PROJECTS there should only be one group
in SPECIFIC-GROUPS, and that group should contain those projects.
The START-AT- arguments are used to set the initial position of the
cursor; if they are not given, it starts on the first task."
  (interactive "P")
  (switch-to-buffer (get-buffer-create "*Browse tasks*"))
  (toggle-read-only -1)
  (erase-buffer)
  (setq sidebrain-browse-tasks-marked nil
	sidebrain-browse-tasks-showing-all show-all
	sidebrain-browse-tasks-specific-groups specific-groups
	sidebrain-browse-tasks-specific-projects specific-projects)
  (dolist (project-group (or specific-groups
			     sidebrain-project-groups))
    ;; (message "listing project group %S with cdr %S" (car project-group) (cdr project-group))
    ;; (message "contents are %S" (mapcar 'cddr (cdr project-group)))
    (when (or show-all
	      (equal start-at-group (car project-group))
	      (assoc start-at-project (cdr project-group))
	      (and (cdr project-group)
		   (reduce (lambda (a b) (or a b)) ; skip empty groups
			   (mapcar 'cddr (cdr project-group)))))
      (insert "Project group \""(car project-group) "\"")
      (when (eq project-group sidebrain-current-project-group)
	(insert " (current)"))
      (if sidebrain-browse-tasks-double-spaced 
	  (insert ":\n\n")
	(insert ":\n"))
      (dolist (project (or specific-projects
			   (cdr project-group)))
	;; (message "listing project %S with cdr %S" (car project) (cdr project))
	(when (or show-all
		  (equal start-at-project (car project))
		  (cdr project)) ; skip empty projects
	  (insert "  Project \"" (car project) "\"")
	  (when (eq project sidebrain-current-project)
	    (insert " (current)"))
	  (if sidebrain-browse-tasks-double-spaced 
	      (insert ":\n\n")
	    (insert ":\n"))
	  (dolist (task (cdr project))
	    (message "Adding task %S to browse display" task)
	    (unless (equal (car task) (car project))
	      (let* ((task-label (car task))
		     (task-data (cdr task)))
		;; (message "Label is %S, task-data is %S" task-label task-data)
		(insert "    " task-label)
		(let ((file (sidebrain-ok-file-name (get-text-property 0 'file task-label))))
		  (when file
		    (insert " (" (file-name-nondirectory file) ")")))
		(insert "\n")
		(let ((subtasks (sidebrain-task-stack-tasks task-data)))
		  (while subtasks
		    ;; don't put the last one in if it is the same as the label
		    (when (or show-all
			      (cdr subtasks)
			      (not (string= (sidebrain-task-text (car subtasks))
					    task-label)))
		      (insert "      " (sidebrain-task-text (car subtasks)))
		      (when show-all
			(insert " " (sidebrain-task-extra-text (car subtasks) nil)))
		      (insert "\n"))
		    (setq subtasks (cdr subtasks))))
		(when show-all
		  (let ((observations (sidebrain-task-stack-observations (cdr task))))
		    ;; (when observations (message "Displaying observations %S" observations))
		    (dolist (observation observations)
		      (insert "       :" observation ":\n"))))
		(when sidebrain-browse-tasks-double-spaced
		  (insert "\n"))))))
	(when sidebrain-browse-tasks-double-spaced
	  (insert "\n"))))
    (when sidebrain-browse-tasks-double-spaced
      (insert "\n")))
  (toggle-read-only 1)
  (goto-char (point-min))
  (when start-at-group
    (re-search-forward (format "^Project group \"%s\"" start-at-group)
		       (point-max) t))
  (when start-at-project
    (re-search-forward (format "^  Project \"%s\"" start-at-project)
		       (point-max) t))
  (if start-at-task
      (re-search-forward (format "^    %s" start-at-task)
			 (point-max) t)
    (sidebrain-browse-tasks-next))
  (sidebrain-browse-tasks-mode)
  (message
   (substitute-command-keys
    "\\<sidebrain-browse-tasks-mode-keymap>\\[sidebrain-browse-tasks-select] to select, \\[sidebrain-browse-tasks-mark] to mark, \\[bury-buffer] to quit")))

(defun sidebrain-browse-tasks-mode ()
  "Major mode for browsing the task queue.
Special commands available are:
\\{sidebrain-browse-tasks-mode-keymap}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'sidebrain-browse-tasks-mode
	mode-name "Browse tasks")
  (use-local-map sidebrain-browse-tasks-mode-keymap)
  (run-hooks 'sidebrain-browse-mode-hook))

;;;; browsing parts of the collection

(defun sidebrain-browse-project-group (group)
  "Like sidebrain-browse-tasks, but only shows projects and tasks in GROUP."
  (interactive
   (list
    (sidebrain-completing-read-project-group "Browse projects in group: ")))
  (sidebrain-browse-tasks nil (list (assoc group sidebrain-project-groups))))

(defun sidebrain-browse-project (group project)
  "Like sidebrain-browse-tasks, but only shows projects and tasks in GROUP and PROJECT."
  (interactive
   (let* ((group-name (sidebrain-completing-read-project-group "Browse a project in group: "))
	  (group (assoc group-name sidebrain-project-groups))
	  (project (sidebrain-completing-read-project group "Browse project: ")))
     (list group project)))
  (sidebrain-browse-tasks nil (list group) (list (assoc project (cdr group)))))

;;;; mailing tasks

;;;###autoload
(defun sidebrain-mail-tasks (recipient)
  "Send selected tasks in the mail.
Should normally be used from sidebrain-browse-tasks-mode."
  (interactive "sMail tasks to: ")
  (unless (or (eq major-mode 'sidebrain-browse-tasks-mode)
	      (yes-or-no-p "Not in task browser; mail selected tasks anyway? "))
    (error "Not in tasks browser"))
  (compose-mail recipient "Tasks")
  (insert "<mailed_tasks>\n")
  (sidebrain-save-task-queue (sidebrain-browse-tasks-marked))
  (insert "</mailed_tasks>\n")
  (mail-subject))

;;;###autoload
(defun sidebrain-extract-tasks-from-mail ()
  "Parse the current buffer as a mail message, and pick up any tasks described in it."
  (interactive)
  (goto-char (point-min))
  (let (begin end)
    (if (and (setq end (search-forward "</mailed_tasks>" (point-max) t))
	     (setq begin (search-backward "<mailed_tasks>" (point-min) t)))
	(sidebrain-restore-queue (xml-parse-region begin end)))))

;;; end of sidebrain-browse.el
