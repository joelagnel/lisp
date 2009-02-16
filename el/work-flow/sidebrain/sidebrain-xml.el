;;;; sidebrain-xml.el -- save and reload sidebrain data using an XML file
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

(provide 'sidebrain-xml)

(defvar sidebrain-potential-save-count 8
  "Counter for potential saves.")

(defun sidebrain-save-after-command ()
  "Return whether to save after this command.
Controlled by sidebrain-save-after-commands."
  (or (eq sidebrain-save-after-commands t)
      (and (integerp sidebrain-save-after-commands)
	   (zerop (% (setq sidebrain-potential-save-count
			   (1+ sidebrain-potential-save-count))
		     sidebrain-save-after-commands)))))

(defun sidebrain-insert-task-to-xml (task prefix)
  "Insert a representation of TASK."
  (unless (sidebrain-task-p task) (error "Cannot save %S as a task, as it is not a task." task))
  (let* ((copied (sidebrain-task-text task))
	 (start-time (sidebrain-task-time-started task))
	 (end-time (sidebrain-task-time-ended task))
	 (time-spent (sidebrain-task-time-spent task))
	 (time-current (sidebrain-task-time-current task))
	 (time-this-time (sidebrain-task-time-this-time task))
	 (subtasks (sidebrain-task-subtasks task))
	 (suspensions (sidebrain-task-suspensions task))
	 (abandoned (sidebrain-task-abandoned task))
	 (keystrokes (sidebrain-task-keystrokes task))
	 (raw-file (sidebrain-task-file task))
	 (file (sidebrain-ok-file-name raw-file))
	 (modified-file-name (and file
				  (run-hook-with-args-until-success
				   'sidebrain-filename-save-hooks
				   file)))
	 (line-number (sidebrain-task-line-number task))
	 (spacer (if sidebrain-save-more-readable
		     (concat "\n" prefix "      ")
		   " "))
	 (attributes nil)
	 )
    (when (null modified-file-name) (setq modified-file-name file))
    (when (and start-time (not (zero-time-p start-time)))
      (push (format "time-started=\"%s\"" (current-time-string start-time)) attributes))
    (when (and end-time (not (zero-time-p end-time)))
      (push (format "time-ended=\"%s\"" (current-time-string end-time)) attributes))
    (when (and time-spent (not (zero-time-p time-spent)))
      (push (format "time-spent=\"%d\"" (time-as-seconds time-spent)) attributes))
    (when (and time-current (not (zero-time-p time-current)))
      (push (format "time-current=\"%d\"" (time-as-seconds time-current)) attributes))
    (when (and time-this-time (not (zero-time-p time-this-time)))
      (push (format "time-this-time=\"%d\"" (time-as-seconds time-this-time)) attributes))
    (when abandoned (push "abandoned=\"yes\"" attributes))
    (when (stringp modified-file-name)
      (push (format "file=\"%s\"" modified-file-name) attributes))
    (when (numberp line-number)
      (push (format "line-number=\"%d\"" line-number) attributes))
    (when (and (numberp subtasks) (not (zerop subtasks)))
      (push (format "subtasks=\"%d\"" subtasks) attributes))
    (when (and (numberp suspensions) (not (zerop suspensions)))
      (push (format "suspensions=\"%d\"" suspensions) attributes))
    (when (and (numberp keystrokes) (not (zerop keystrokes)))
      (push (format "keystrokes=\"%d\"" keystrokes) attributes))
    (insert (concat
	     prefix
	     "<task "
	     (mapconcat 'identity attributes spacer)
	     ">"
	     copied
	     "</task>\n"))))

(defun sidebrain-save-task-stack-contents (stack prefix)
  "Save a stack. Assumes the <stack></stack> will be written by our caller."
  (dolist (task (reverse stack))
    (sidebrain-update-stopwatch task 'time-this-time 'time-started)
    (sidebrain-insert-task-to-xml task prefix)))

(defun sidebrain-save-observations (observations prefix)
  "Insert an XML representation of OBSERVATIONS, with PREFIX before each line."
  (insert prefix "<observations>\n")
  (dolist (observation observations)
    (insert prefix "  <observation>" observation "</observation>\n"))
  (insert prefix "</observations>\n\n"))

(defun quote-quotes (string &optional quoted-quote unquoted-quote)
  "Quote any quotes in STRING.
If QUOTED-QUOTE is given, it is the representation to use for a quote.
If UNQUOTED-QUOTE is given, it is the representation to expect for a quote."
  (if (not (stringp string))
      string
    (unless quoted-quote (setq quoted-quote "&quot;"))
    (unless unquoted-quote (setq unquoted-quote "\""))
    (let ((match 0)
	  (repl-len (length quoted-quote))
	  (handled 0))
      (while (setq match (string-match unquoted-quote string handled))
	(setq handled (+ match repl-len)
	      string (concat 
		      (substring string 0 match)
		      quoted-quote
		      (substring string (match-end 0)))))
      string)))

(defun unquote-quotes (string)
  "Reverse the effect of quote-quotes."
  (quote-quotes string "\"" "&quot;"))

(defun sidebrain-save-task-stack (task label prefix)
  "Save TASK to file, using LABEL and putting PREFIX at the start of each line."
  (when (and (sidebrain-task-stack-p task)
	     (stringp label))
    (let* ((stack (sidebrain-task-stack-tasks task))
	   (observations (sidebrain-task-stack-observations task))
	   (priority (sidebrain-task-stack-priority task))
	   (raw-file (if stack (sidebrain-task-file (car stack)) nil))
	   (file (sidebrain-ok-file-name raw-file))
	   (modified-file (run-hook-with-args-until-success
			   'sidebrain-filename-save-hooks
			   file))
	   (link-group (sidebrain-task-stack-link-group task))
	   (link-project (sidebrain-task-stack-link-project task))
	   (link-task (sidebrain-task-stack-link-task task)))
      (when modified-file (setq file modified-file))
      (insert prefix "    <task-queue-element label=\"" label "\"")
      (when file
	(insert " file=\"" file "\""))
      (when priority
	(insert (format " priority=\"%S\"" priority)))
      (run-hook-with-args 'sidebrain-save-label-hook label)
      (insert ">\n")

      (when stack
	(insert prefix "      <task-stack")
	(when (and link-group link-project link-task)
	  (insert " link-group=\"" link-group
		  "\" link-project=\"" link-project
		  "\" link-task=\"" link-task
		  "\""))
	(insert ">\n")
	(sidebrain-save-task-stack-contents stack (concat prefix "        "))
	(insert prefix "      </task-stack>\n"))
      (when observations
	(sidebrain-save-observations observations (concat prefix "    ")))
      (insert prefix "    </task-queue-element>\n"))))

(defun sidebrain-save-task-queue (this-queue &optional prefix)
  "Save THIS-QUEUE. Optionally output PREFIX at the start of each line."
  (unless prefix (setq prefix ""))
  (when sidebrain-xml-verbose
    (message "Saving task queue %S" this-queue))
  (insert prefix "  <task-queue>\n")
  (dolist (task (reverse this-queue))
    (when task ; (and task (stringp (first task))) ; should always be string
      (when sidebrain-xml-verbose
	(message "Saving task %S" task))
      (unless (stringp (first task)) (error "Trying to save a non-string task %S" task))
      (sidebrain-save-task-stack (cdr task)
			   (quote-quotes (copy-sequence (car task)))
			   prefix)))
  (insert prefix "  </task-queue>\n"))

(defun sidebrain-prepare-file (file-name)
  "Set up a file for writing sidebrain data to."
  (find-file file-name)
  (erase-buffer)
  (insert "<?xml version=\"1.0\"?>\n")
  (insert "<sidebrain version=\"" sidebrain-version "\">\n")
  (insert "  <saved-at timezone=\"" (second (current-time-zone)) "\">" sidebrain-saved-at "</saved-at>\n")
  (insert "  <saved-by address=\"" user-mail-address "\">"
	  user-full-name "</saved-by>\n")
  (insert "  <saved-on systemtype=\"" (symbol-name system-type) "\""">" (system-name) "</saved-on>\n")
  (when (stringp sidebrain-extra-file-header)
    (insert sidebrain-extra-file-header)))

;;;###autoload
(defun sidebrain-save-to-file ()
  "Save the sidebrain data to the file named in sidebrain-file-name."
  (interactive)
  (if (or (and sidebrain-current-stack
	       (sidebrain-task-stack))
	  sidebrain-project-groups
	  sidebrain-current-project-group
	  sidebrain-current-project
	  (and sidebrain-current-stack
	       (sidebrain-observations))
	  sidebrain-history)
      (save-window-excursion
	(let* ((visiting (find-buffer-visiting sidebrain-file-name))
	       (viewing (eq (current-buffer) visiting)))
	  (setq sidebrain-saved-at (current-time-string))
	  (sidebrain-prepare-file sidebrain-file-name)
	  (insert "  <project-groups")
	  (when sidebrain-current-project-group
	    (insert " current=\"" (car sidebrain-current-project-group) "\""))
	  (insert ">\n")
	  (dolist (project-group (reverse sidebrain-project-groups))
	    (when (and (stringp (car project-group)) (not (string= "" (car project-group))))
	      (when sidebrain-xml-verbose
		(message "Saving project group %S" project-group))
	      (insert "    <project-group name=\"" (car project-group) "\"")
	      (when (and (eq project-group sidebrain-current-project-group)
			 sidebrain-current-project)
		(insert " current=\"" (car sidebrain-current-project) "\""))
	      (insert ">\n")
	      (dolist (project (reverse (cdr project-group)))
		(when (and (stringp (car project)) (not (string= "" (car project))))
		  (when sidebrain-xml-verbose
		    (message "Saving project %S" project))
		  (insert "      <project name=\"" (car project) "\"")
		  (when (and sidebrain-current-stack
			     (eq project-group sidebrain-current-project-group)
			     (eq project sidebrain-current-project))
		    (insert " current=\"" (sidebrain-task-stack-name) "\""))
		  (insert ">\n")
		  (when sidebrain-xml-verbose
		    (message "Saving project task queue %S" (cdr project)))
		  (sidebrain-save-task-queue (cdr project) "      ")
		  (insert "      </project>\n")))
	      (insert "    </project-group>\n")))
	  (insert "  </project-groups>\n")

	  ;; (sidebrain-save-observations (sidebrain-observations) "      ")
	  (insert "</sidebrain>\n")
	  (basic-save-buffer)
	  (if visiting
	      (progn
		(goto-char (point-min))	; just for tidiness, if we are viewing it
		(unless viewing
		  (bury-buffer nil)))
	    (kill-buffer nil)))
	(let* ((visiting (find-buffer-visiting sidebrain-history-file-name))
	       (viewing (eq (current-buffer) visiting)))
	  (sidebrain-prepare-file sidebrain-history-file-name)
	  (insert "  <history>\n")
	  (insert "    <tasks-ended>\n")
	  (dolist (task sidebrain-history)
	    (sidebrain-insert-task-to-xml task "    "))
	  (insert "    </tasks-ended>\n")
	  (insert "    <tasks-suspended>\n")
	  (dolist (task-stack-pair sidebrain-suspension-history)
	    (sidebrain-save-task-stack (cdr task-stack-pair)
				       (car task-stack-pair)
				       "      "))
	  (insert "    </tasks-suspended>\n")
	  (insert "  </history>\n\n")
	  
	  (insert "</sidebrain>\n")
	  (basic-save-buffer)
	  (if visiting
	      (progn
		(goto-char (point-min))	; just for tidiness, if we are viewing it
		(unless viewing
		  (bury-buffer nil)))
	    (kill-buffer nil))))
    (message "No sidebrain data to save -- leaving file undisturbed")))

(defun sidebrain-extract-task-from-xml (task)
  "Turn TASK (in the format we get from the XML reader) into a task structure."
  (unless (eq (first task) 'task) (error "%S is not a task" task))
  (let* ((attributes (second task))
	 (start-time-pair (assoc 'time-started attributes))
	 (start-time (if start-time-pair
			 (date-to-time (cdr start-time-pair))
		       nil))
	 (end-time-pair (assoc 'time-ended attributes))
	 (end-time (if end-time-pair
		       (date-to-time (cdr end-time-pair))
		     nil))
	 (time-spent-pair (assoc 'time-spent attributes))
	 (time-spent (seconds-to-time
		      (if time-spent-pair 
			  (string-to-int (cdr time-spent-pair))
			0)))
	 (time-current-pair (assoc 'time-current attributes))
	 (time-current (seconds-to-time
			(if time-current-pair 
			    (string-to-int (cdr time-current-pair))
			  0)))
	 (subtasks-pair (assoc 'subtasks attributes))
	 (subtasks (if subtasks-pair
		       (string-to-int (cdr subtasks-pair))
		     0))
	 (suspensions-pair (assoc 'suspensions attributes))
	 (suspensions (if suspensions-pair
			  (string-to-int (cdr suspensions-pair))
			0))
	 (file-pair (assoc 'file attributes))
	 (raw-file (cdr file-pair))
	 (file (sidebrain-ok-file-name
		(if (stringp raw-file)
		    (let ((modified
			   (run-hook-with-args-until-success 'sidebrain-filename-load-hooks raw-file)))
		      (or modified raw-file))
		  raw-file)))
	 (line-pair (assoc 'line-number attributes))
	 (line-number (cond
		       ((numberp (cdr line-pair)) (cdr line-pair))
		       ((stringp (cdr line-pair)) (string-to-int (cdr line-pair)))
		       (t nil)))
	 (text (third task)))
    (when sidebrain-xml-verbose
      (message "Making task from %S, raw-file=%S file=%S start-time=%S end-time=%S time-spent=%S time-current=%S line-pair=%S"
	       task raw-file file start-time end-time time-spent time-current line-pair))
    (make-sidebrain-task :text text
			 :file file
			 :line-number line-number
			 :time-started start-time
			 :time-ended end-time
			 :time-spent time-spent
			 :subtasks subtasks
			 :suspensions suspensions
			 ;; todo: keystrokes too (and in writer code)
			 )
    ))

(defun sidebrain-forget-all (&optional force)
  "Used when about to re-load all the sidebrain data. Also Useful for debugging."
  (interactive "P")
  (if (or force (yes-or-no-p "Forget all sidebrain data? "))
      (setq sidebrain-task-stack nil
	    sidebrain-project-groups nil
	    sidebrain-current-project-group nil
	    sidebrain-current-project nil
	    sidebrain-current-stack nil
	    sidebrain-observations nil
	    sidebrain-history nil
	    sidebrain-suspension-history nil))
  (sidebrain-display))

(defun sidebrain-build-queue-entry (queue-entry)
  "Construct a queue entry from TASK-QUEUE."
  (let* ((queue-data (second queue-entry))
	 (file (sidebrain-ok-file-name
		(cdr (assoc 'file queue-data))))
	 (label (cdr (assoc 'label queue-data)))
	 (raw-priority (cdr (assoc 'priority queue-data)))
	 (priority (if (stringp raw-priority)
		       (if (string-match "^[0-9]+$" raw-priority)
			   (string-to-int raw-priority)
			 raw-priority)
		     nil))
	 (raw-stack (cdr (assoc 'task-stack (cddr queue-entry))))
	 (raw-stack-attributes (car raw-stack))
	 (raw-stack-contents (reverse (cdr raw-stack)))
	 (stack nil)
	 (raw-observations (cdr (assoc 'observations (cddr queue-entry))))
	 (raw-observations-contents (reverse (cdr raw-observations)))
	 (observations nil)
	 (link-group (cdr  (assoc 'link-group queue-data)))
	 (link-project (cdr (assoc 'link-project queue-data)))
	 (link-task (cdr (assoc 'link-task queue-data))))
    (run-hook-with-args 'sidebrain-load-label-hook label queue-entry)
    (message "label %S, file %S, stack %S, observations %S" label file raw-stack raw-observations)
    (dolist (task (reverse raw-stack-contents))
      (push (sidebrain-extract-task-from-xml task) stack))
    (dolist (obs raw-observations-contents)
      ;; (message "  Restoring observation %S" obs)
      (push (third obs) observations))
    (cons (unquote-quotes label)
	  (make-sidebrain-task-stack
	   :tasks stack
	   :observations observations
	   :priority priority
	   :link-group link-group
	   :link-project link-project
	   :link-task link-task))))

(defun sidebrain-restore-queue (task-queue)
  "From the xml list TASK-QUEUE, restore task queue data."
  (dolist (queue-entry task-queue)
    (when sidebrain-xml-verbose (message "Restoring queue entry %S" queue-entry))
    (push (sidebrain-build-queue-entry queue-entry)
	  (cdr sidebrain-current-project))))

(defvar sidebrain-saved-at ""
  "When we last saved the sidebrain data.")

;;;###autoload
(defun sidebrain-load-from-file (&optional force)
  "Load the sidebrain data from the file named in sidebrain-file-name.
If it has not been saved since we saved it, don't do it, so as not to
disturb the existing data structures, unless optional argument non-nil."
  ;; todo: look for "done" task comments, and delete them from the task queue
  ;; todo: make this set the current everything, and display it properly
  (interactive "P")
  (let* ((sidebrain-create-groups-on-demand t)
	 (raw-data (xml-parse-file sidebrain-file-name))
	 (raw-history-file-data (xml-parse-file sidebrain-history-file-name))
	 (sidebrain (cddr (assoc 'sidebrain raw-data)))
	 (sidebrain-history-holder-holder (cddr (assoc 'sidebrain raw-history-file-data)))
	 (sidebrain-history-holder (cddr (assoc 'history sidebrain-history-holder-holder)))
	 (ended (cddr (assoc 'tasks-ended sidebrain-history-holder)))
	 (suspended (cddr (assoc 'tasks-suspended sidebrain-history-holder)))
	 (saved-at (caddr (assoc 'saved-at sidebrain)))
	 (history-saved-at (caddr (assoc 'saved-at sidebrain-history-holder)))
	 ;; (task-stack (cddr (assoc 'task-stack sidebrain)))
	 ;; (task-queue (cddr (assoc 'task-queue sidebrain)))
	 (project-groups-holder (assoc 'project-groups sidebrain))
	 (project-groups (cddr project-groups-holder))
	 (current-project-group (cdr (assoc 'current (cadr project-groups-holder))))
	 ;; (observations (cddr (assoc 'observations sidebrain)))
	 ;; (history (cddr (assoc 'history sidebrain-history-holder)))
	 )
    (if (or force (not (string= saved-at sidebrain-saved-at)))
	(progn
	  (sidebrain-forget-all t)
	  (dolist (project-group project-groups)
	    (let ((group-name (cdr (assoc 'name (cadr project-group)))))
	      (when sidebrain-xml-verbose
		(message "restoring project group called %S: %S" group-name project-group))
	      (sidebrain-set-project-group group-name)
	      (dolist (project-holder (cddr project-group))
		(when (eq (car project-holder) 'project)
		  (let ((project-name (cdr (assoc 'name (cadr project-holder))))
			(task-queue (cddr (assoc 'task-queue (cddr project-holder))))
			)
		    (sidebrain-set-project project-name t)
		    (when sidebrain-xml-verbose
		      (message "restoring project called %S: queue is %S" project-name task-queue))
		    (sidebrain-restore-queue task-queue)
		    )))
	      ))
	  ;; todo: now restore history
	  (when sidebrain-xml-verbose (message "sidebrain-history-holder is %S" sidebrain-history-holder))
	  ;; xml tasks-ended goes to sidebrain-history
	  (when sidebrain-xml-verbose (message "Ended tasks were %S" ended))
	  (dolist (ended-task ended)
	    (push (sidebrain-extract-task-from-xml ended-task)
		  sidebrain-history))
	  ;; xml tasks-suspended goes to sidebrain-suspension-history
	  (when sidebrain-xml-verbose (message "Suspended tasks were %S" suspended))
	  (dolist (suspended-task suspended)
	    (push (sidebrain-build-queue-entry suspended-task)
		  sidebrain-suspension-history))
	  (sidebrain-display))
      (message "Did not update sidebrain data from %s, as it appears to be the one we wrote"
	       sidebrain-file-name))))

;;; end of sidebrain-xml.el
