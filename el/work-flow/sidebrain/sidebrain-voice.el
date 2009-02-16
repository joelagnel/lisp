;;;; sidebrain-voice.el -- voice commands for sidebrain
;;; Time-stamp: <2006-01-25 15:48:33 jcgs>

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

(provide 'sidebrain-voice)

(defvar vr-sidebrain-commands
  '(("begin task" . sidebrain-begin-task)
    ("end task" . sidebrain-end-task)
    ("abandon task" . sidebrain-abandon-task)
    ("suspend task" . sidebrain-suspend-task)
    ("resume task" . sidebrain-resume-task)
    ("task reminder" . sidebrain-reminder)
    ("browse tasks" . sidebrain-browse-tasks)
    ("make observation" . sidebrain-observe)
    ("read comments" . sidebrain-read-todo-from-comments)
    ("save tasks" . sidebrain-save-to-file)    
    ("load tasks" . sidebrain-load-from-file)
    ;; ("" . sidebrain-)


    )
  "Voice commands for the sidebrain package.")

;;; end of sidebrain-voice.el
