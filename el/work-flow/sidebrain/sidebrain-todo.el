;;;; sidebrain-todo.el -- sidebrain interface to traditional comments in file
;;; Time-stamp: <2006-04-11 10:18:53 john>

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

(provide 'sidebrain-todo)

(defun nearest (back forward)
  "Return whichever of BACK and FORWARD is nearer to point.
Behaves sensibly if either or both are null."
  (if (null back)
      forward
    (if (null forward)
	back
      (message "comparing %S %S" (- (point) back) (- forward (point)))
      (if (> (- (point) back)
	     (- forward (point)))
	  forward
	back))))

(defun sidebrain-nearest-task-comment ()
  "Return the to-do comment nearest to point."
  (save-excursion
    (let ((back (save-excursion
		  (re-search-backward to-do-comment-pattern (point-min) t)))
	  (forward (save-excursion
		     (and (re-search-forward to-do-comment-pattern (point-max) t)
			  (match-beginning 0)))))
      (when (or back forward)
	(goto-char (nearest back forward))
	(if (looking-at to-do-comment-pattern)
	    (match-string-no-properties 1)
	  nil)))))

(defun sidebrain-determine-project ()
  "Determine, if possible, the group and project to use.
Returns t if it did so, nil otherwise."
  (let* ((group-and-project (run-hook-with-args-until-success
			     'sidebrain-determine-project-hook
			     buffer-file-name)))
    (when (null group-and-project)
      ;; (message "Looking for group and project for %s" buffer-file-name)
      (let ((elts sidebrain-file-projects))
	(while (and elts (null group-and-project))
	  ;; (message "Trying %s as possible match for %s" (caar elts) buffer-file-name)
	  (if (string-match (caar elts) buffer-file-name)
	      (setq group-and-project (cdar elts))
	    (setq elts (cdr elts))))))
      ;; (message "Got %S and group and project for %s" group-and-project buffer-file-name))
    (if group-and-project
	(if (equal group-and-project '(nil nil))
	    nil
	  (sidebrain-set-project-group (first group-and-project))
	  (message "Setting project to %S" (second group-and-project))
	  (sidebrain-set-project (second group-and-project) t)
	  (message "Set project to %S" (second group-and-project))
	  t)
      (if sidebrain-use-default-project
	  (progn
	    (sidebrain-set-project-group "other" t)
	    (sidebrain-set-project "other" t)
	    t)
	(if (y-or-n-p "Assign to do comments to a project? ")
	    (progn
	      (sidebrain-set-project-group (format "Project group %s belongs to: "
						      buffer-file-name))
	      (sidebrain-set-project (format "Project %s belongs to: "
						buffer-file-name))
	      t)
	  nil)))))

;;;###autoload
(defun sidebrain-read-todo-from-comments ()
  "Make sidebrain reminders from todo comments in the current buffer.
They become part of the project (and project group) determined thus:
  First, sidebrain-determine-project-hook is tried (which see)
  If none of those match, sidebrain-file-projects is scanned (which see)
  If none of those match:
    if sidebrain-use-default-project is set, a default is used;
    otherwise, the user is prompted for them."
  (interactive)
  (catch 'done
    (save-excursion
      (let ((assigned-project nil))
	;; first, find all the comments for things still to be done, and merge them into our collection:
	(goto-char (point-min))
	(while (re-search-forward to-do-comment-pattern (point-max) t)
	  (let ((reminder (match-string-no-properties 1)))
	    (unless assigned-project
	      (if (sidebrain-determine-project)
		  (setq assigned-project t)
		(throw 'done 'no-project))
	      (message "Putting \"%s\" into \"%s:%s\""
		       reminder
		       (car sidebrain-current-project-group)
		       (car sidebrain-current-project)))
	    (sidebrain-reminder reminder
				(buffer-file-name)
				(count-lines (point-min) (point))
				sidebrain-current-project-group
				sidebrain-current-project)))
	(goto-char (point-min))
	;; next, find all the comments for things that have already been
	;; done, and take them out of our collection if they're in there
	(while (re-search-forward to-do-done-comment-pattern (point-max) t)
	  ;; todo: probably should strip comment trailers from end of line
	  (sidebrain-remove-tasks-labelled (match-string-no-properties 1)))))))

(defun sidebrain-remove-tasks-labelled (string)
  "Remove from the queue all tasks whose label is STRING."
  (rplacd sidebrain-current-project
	  (delete-if (lambda (item)
		       (string= (first item) string))
		     (cdr sidebrain-current-project))))

(defun sidebrain-file-comment-mark-done (task file)
  "Mark TASK as being done, in source comment in FILE."
  (when sidebrain-mark-done-comments
  (save-window-excursion
    (find-file file)
    (save-excursion
      ;; todo: fix this regular expression, and give the option of deleting the whole comment
      (let ((pattern (copy-sequence	; why is this here?
		      (format (concat
			       "\\(" comment-start-skip "\\)"
			       "\\(to" "do\\): +\\(%s\\)\\( *{[^}]}+\\)?"
			       comment-end
			       )
		       (cond
			((stringp task) task)
			((sidebrain-task-p task) (sidebrain-task-text task))
			(t (error "Bad task data for sidebrain-file-comment-mark-done: %S" task)))
		       ))))
	(message "Looking for string to remove or update: %s" pattern)
	(goto-char (point-min))
	;; todo: this is a test of the done thing
	(if (re-search-forward pattern (point-max) t)
	    (progn
	      (message "Found it: %S" (match-data))
	      (if nil
		  (delete-region (match-beginning 0) (match-end 0))
		(replace-match "done" t t nil 1)))
	  (message "Could not find comment to update")))))))

;;; end of sidebrain-todo.el
