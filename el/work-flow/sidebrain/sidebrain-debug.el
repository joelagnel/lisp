;;;; sidebrain-debug.el -- debugging stuff for sidebrain
;;; Time-stamp: <2006-05-05 17:48:43 john>

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

(defvar sidebrain-debug-stop-after-each nil
  "Debugging control")

(defmacro sidebrain-with-browsing (&rest forms)
  "Execute FORMS, running the sidebrain browser after each."
  (let ((i 1))
    (let ((sidebrain-file-name sidebrain-safe-data-file))
      (sidebrain-save-to-file))
    (while forms
      (message "doing test %d:%S" i (car forms))
      (eval (car forms))
      (sidebrain-browse-tasks t)
      (message "done test %d:%S" i (car forms))
      (if sidebrain-debug-stop-after-each
	  (save-excursion
	    (recursive-edit)
	    (message (substitute-command-keys "\\[exit-recursive-edit] to continue")))
	(sit-for 2))
      (let ((sidebrain-file-name (format "~/tmp/sidebrain-test-%d.xml" i))
	    (sidebrain-history-file-name (format "~/tmp/sidebrain-history-test-%d.xml" i))
	    (sidebrain-extra-file-header (format "<debugstep number=\"%d\">%S</debugstep>\n"
						 i (car forms))))
	(message "Saved to file %s" sidebrain-file-name)
	(sidebrain-save-to-file))
      (setq forms (cdr forms)
	    i (1+ i)))
    (sidebrain-forget-all)
    (let ((sidebrain-file-name sidebrain-safe-data-file))
      (sidebrain-load-from-file t))))

(defvar sidebrain-safe-data-file "~/tmp/sidebrain-safe.xml"
  "Where to put the real sidebrain data during experiments.")

(defun sidebrain-debug-structures (&optional arg)
  "Debugging run of sidebrain data structures"
  (interactive "P")
  (let* ((stack-trace-on-error t)
	 (sidebrain-debug-stop-after-each arg))
    (sidebrain-with-browsing

     (sidebrain-forget-all t)

     ;; set up some sample tasks

     (sidebrain-new-project-group "research programming")
     (sidebrain-new-project "sidebrain")
     (sidebrain-begin-task "release sidebrain" t)
     (sidebrain-begin-task "refactor sidebrain" t)
     (sidebrain-begin-task "fix data storage" t)
     (sidebrain-observe "Using defstruct, since what I am handling are structures")
     (sidebrain-observe "It seems to make a lot more sense this way")
     (sidebrain-suspend-task t)
     (sidebrain-begin-task "review sidebrain effort logging" t)
     (sidebrain-observe "Quite what is the time spent on something?")
     (sidebrain-suspend-task t)
     (sidebrain-begin-task "re-write sidebrain interface to activities" t)
     (sidebrain-resume-task "review sidebrain effort logging" t)
     (sidebrain-begin-task "Think about what this really means" t)
     (sidebrain-new-project "versor")
     (sidebrain-begin-task "recenter display for both directions of movement" t)
     (sidebrain-reminder "continue work on languide" nil nil "research programming" "versor")
     (sidebrain-new-project-group "research writing")
     (sidebrain-new-project "sidebrain writing")
     (sidebrain-begin-task "write sidebrain manual" t)
     (sidebrain-reminder "write paper about interruptions" nil nil
			 "research writing" "sidebrain writing")
     (sidebrain-reminder "write paper about attention focussing{explain the problem{refer to 5+-2 paper}}"
			 nil nil
			 "research writing" "sidebrain writing")
     (sidebrain-reminder "write paper about how programmers think of program changes" nil nil
			 "research writing" "versor papers")

     ;; try saving and re-loading

      (let ((sidebrain-file-name  "~/tmp/sidebrain-test-reloading.xml")) (sidebrain-save-to-file) (sidebrain-forget-all t) (sidebrain-load-from-file t))

     ;; now try switching tasks

     

     )
))

(defun sidebrain-test-load ()
  (interactive)
  (let ((sidebrain-file-name  "~/tmp/sidebrain-test-reloading.xml")) (sidebrain-load-from-file t)))

(defun sidebrain-preen-data-structures ()
  "Undo some damage that has been occurring."
  (interactive)
  (dolist (project-group sidebrain-project-groups)
    (message "  Preening group \"%s\""
	     (car project-group))
    (dolist (project (cdr project-group))
      (message "    Preening \"%s:%s\""
	       (car project-group)
	       (car project))
      (dolist (task-stack (cdr project))
	(message "    Preening \"%s:%s:%s\""
		 (car project-group)
		 (car project)
		 (car task-stack))
	(dolist (task (sidebrain-task-stack-tasks (cdr task-stack)))
	  (message  "      Preening \"%s\" (in \"%s:%s\")"
		    (sidebrain-task-text task)
		    (car project-group)
		    (car project))
	  (when (and (stringp (sidebrain-task-file task))
		     (not (string-match "[-a-z0-9]" (sidebrain-task-file task))))
	    (message "        Bad filename: \"%s\"" (sidebrain-task-file task))))
	))
    )
  )

;;; end of sidebrain-debug.el
