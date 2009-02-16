;;;; sidebrain-macros.el -- macros for sidebrain
;;; Time-stamp: <2006-03-08 11:40:22 jcgs>

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

(provide 'sidebrain-macros)

(defmacro sidebrain-task-stack-name ()
  "Return the name of the current task stack."
  '(car sidebrain-current-stack))

(defmacro sidebrain-valid-stack ()
  "Return whether there is currently a stack."
  '(and sidebrain-current-stack
       (sidebrain-task-stack)))

(defmacro sidebrain-task-stack ()
  "Return the current task stack."
  '(and (sidebrain-task-stack-p (cdr sidebrain-current-stack))
	(sidebrain-task-stack-tasks (cdr sidebrain-current-stack))))

(defmacro sidebrain-push-task (task)
  "Push TASK onto the current task stack"
  `(if (sidebrain-task-stack-p (cdr sidebrain-current-stack))
       (push ,task
	     (sidebrain-task-stack-tasks (cdr sidebrain-current-stack)))
     (error "No current task stack")))

(defmacro sidebrain-pop-task ()
  "Pop a task from the current task stack."
  '(if (sidebrain-task-stack-p (cdr sidebrain-current-stack))
       (pop (sidebrain-task-stack-tasks (cdr sidebrain-current-stack)))
     (error "No current task stack")))

(defmacro sidebrain-observations ()
  "Return the current collection of observations."
  '(and (sidebrain-task-stack-p (cdr sidebrain-current-stack))
	(sidebrain-task-stack-observations (cdr sidebrain-current-stack))))

(defmacro sidebrain-add-observation (observation)
  "Add OBSERVATION to the current observation list."
  `(if (sidebrain-task-stack-p (cdr sidebrain-current-stack))
      (push ,observation
	    (sidebrain-task-stack-observations (cdr sidebrain-current-stack)))
    (error "No current task stack")))

(defmacro sidebrain-set-observations (obs)
  "Set the current selection of observations to OBS."
    `(if (sidebrain-task-stack-p (cdr sidebrain-current-stack))
      (setf (sidebrain-task-stack-observations (cdr sidebrain-current-stack))
	    ,obs)
    (error "No current task stack")))

;; (defmacro sidebrain-task-stack ()
;;   "Return the current task stack."
;;   '(let ((stack sidebrain-current-stack))
;;      (if stack
;; 	 (sidebrain-task-stack-tasks (cdr stack))
;;        nil)))

;; (defmacro sidebrain-observations ()
;;   "Return the current collection of observations."
;;   '(let ((stack sidebrain-current-stack))
;;      (if stack
;; 	 (sidebrain-task-stack-observations (cdr stack))
;;        nil)))

;;; end of sidebrain-macros.el
