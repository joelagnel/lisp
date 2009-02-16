;;;; sidebrain-menu.el -- menu for sidebrain
;;; Time-stamp: <2006-03-24 18:49:18 jcgs>

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

(provide 'sidebrain-menu)

;;;; sidebrain menu setup

(defvar sidebrain-menu (make-sparse-keymap "Sidebrain")
  "Menu for main sidebrain operations.")

(fset 'sidebrain-menu sidebrain-menu)

(define-key sidebrain-menu [ sidebrain-read-comments ] '(menu-item "Read comments" sidebrain-read-todo-from-comments))
(define-key sidebrain-menu [ sidebrain-sep-c ] '(menu-item "--single-line"))
;; (define-key sidebrain-menu [ sidebrain-mail-tasks ] '(menu-item "Mail tasks" sidebrain-mail-tasks))
;; (define-key sidebrain-menu [ sidebrain-extract-tasks-from-mail ] '(menu-item "Extract tasks from mail" sidebrain-extract-tasks-from-mail))
(define-key sidebrain-menu [ sidebrain-resume-task ] '(menu-item "Resume task" sidebrain-resume-task))
(define-key sidebrain-menu [ sidebrain-suspend-task ] '(menu-item "Suspend task" sidebrain-suspend-task))
(define-key sidebrain-menu [ sidebrain-new-task ] '(menu-item "New task" sidebrain-new-task))
(define-key sidebrain-menu [ sidebrain-sep-1 ] '(menu-item "--single-line"))
(define-key sidebrain-menu [ sidebrain-set-project-group ] '(menu-item "Set project group" sidebrain-set-project-group))
(define-key sidebrain-menu [ sidebrain-set-project ] '(menu-item "Set project" sidebrain-set-project))
(define-key sidebrain-menu [ sidebrain-sep-2 ] '(menu-item "--single-line"))
(define-key sidebrain-menu [ sidebrain-new-project-group ] '(menu-item "New project group" sidebrain-new-project-group))
(define-key sidebrain-menu [ sidebrain-new-project ] '(menu-item "New project" sidebrain-new-project))
(define-key sidebrain-menu [ sidebrain-sep-3 ] '(menu-item "--single-line"))
(define-key sidebrain-menu [ sidebrain-reminder ] '(menu-item "Reminder" sidebrain-reminder))
(define-key sidebrain-menu [ sidebrain-observe ] '(menu-item "Observation" sidebrain-observe))
(define-key sidebrain-menu [ sidebrain-sep-4 ] '(menu-item "--single-line"))
(define-key sidebrain-menu [ sidebrain-browse-tasks ] '(menu-item "Browse tasks" sidebrain-browse-tasks))
(define-key sidebrain-menu [ sidebrain-sep-5 ] '(menu-item "--single-line"))
(define-key sidebrain-menu [ sidebrain-end-task ] '(menu-item "End task" sidebrain-end-task))
(define-key sidebrain-menu [ sidebrain-begin-task ] '(menu-item "Begin task" sidebrain-begin-task))

;;; end of sidebrain-menu.el
