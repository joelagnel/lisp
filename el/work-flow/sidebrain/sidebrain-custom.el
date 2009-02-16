;;;; sidebrain-custom.el -- customization for sidebrain
;;; Time-stamp: <2006-04-12 11:52:21 john>

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

(provide 'sidebrain-custom)

(defgroup sidebrain
  nil
  "Customization for sidebrain, a programmer's memory aide."
  :group 'applications
  :prefix "sidebrain-")

(defgroup sidebrain-tasks
  nil
  "Sidebrain task handling"
  :group 'sidebrain
  :prefix "sidebrain-")

(defcustom sidebrain-auto-ask-info-gathering-results ".+\\?$"
  "If a string, when you end a task that that string matches as a pattern, you are asked to make an observation.
If not a string, should be nil.
This lets you use a convention of marking information-gathering tasks in a particular way --
the default is ending them in a question mark -- and getting prompted to store the result
of the investigative task."
  :type 'regexp
  :group 'sidebrain-tasks
  )

(defcustom sidebrain-clear-observations-on-emptying-stack 'ask
  "*Whether to clear the observations list on ending the last task.
If nil, this is never done; if t, it is always done; otherwise, the
user is prompted."
  :group 'sidebrain-tasks
  :type 'boolean)

(defcustom sidebrain-mark-done-comments t
  "*Whether to change \"todo\" comments to \"done\" when ending tasks generated from them."
  :group 'sidebrain-tasks
  :type 'boolean)

(defcustom sidebrain-edit-labels t
  "*Whether to let the user edit the label on suspending a task."
  :group 'sidebrain-tasks
  :type 'boolean)

(defcustom sidebrain-always-available-groups
  '("special" "other")
  "*Groups which always get created if they do not exist,
and thus appear always to exist."
  :group 'sidebrain-tasks
  :type 'boolean)

(defcustom sidebrain-create-groups-on-demand nil
  "*Whether to make all groups spring into existence as required."
  :group 'sidebrain-tasks
  :type 'boolean)

(defcustom sidebrain-switch-self-repair t
  "*Whether sidebrain-set-task-triplet should fill in defaults if given an incomplete triplet."
  :group 'sidebrain-tasks
  :type 'boolean)

;; Chris Exton suggested that it would be good to record what the user
;; had been doing just before suspending a task, and play it back to
;; them when they resume that task. Rather than hard coding that
;; specific feature, I have put in this family of hooks, to allow
;; various such things to be added.

(defcustom sidebrain-suspend-hook nil
  "Functions to be applied to the task stack structure on suspending the task.
You could save extra information here, using properties."
  :type 'hook
   :group 'sidebrain)

(defcustom sidebrain-resume-hook nil
  "Functions to be applied to the task stack structure on resuming the task.
You could use extra information stored as properties by sidebrain-suspend-hook functions."
  :type 'hook
   :group 'sidebrain)

;;;;;;;;;;;;;;;;;;;;;
;;;; persistence ;;;;
;;;;;;;;;;;;;;;;;;;;;

(defgroup sidebrain-save
   nil
   "Sidebrain data save and load to XML."
   :group 'sidebrain)

(defcustom sidebrain-file-name "~/.sidebrain.xml"
  "File in which we save the sidebrain data, apart from the history -- see sidebrain-history-file-name for that."
  :type 'file
  :group 'sidebrain-save)

(defcustom sidebrain-history-file-name "~/.sidebrain-history.xml"
  "File in which we save the sidebrain history."
  :type 'file
   :group 'sidebrain-save)

(defcustom sidebrain-record-task-hook nil
  "Functions to filter tasks before putting them on the history list.
These can modify their argument, or return nil to stop it going onto the history list."
  :type 'hook
  :group 'sidebrain-save)

(defcustom sidebrain-record-abandon-task-hook nil
  "Functions to filter tasks before putting them on the history list when the task is abandoned.
These can modify their argument, or return nil to stop it going onto the history list."
  :type 'hook
  :group 'sidebrain-save)

(defcustom sidebrain-save-after-commands t
  "Whether to save sidebrain data after every user-level operation that modifies it.
Nil means don't do this, t means do this every time, an integer means once every n times."
  :type 'boolean
  :group 'sidebrain-save)

(defcustom sidebrain-nested-tasks-pattern "\\([^{]+\\) *\\({\\(.+\\)}\\)?$"
  "Pattern describing our notation for nested tasks."
  :type 'regexp
  :group 'sidebrain-save)

(defcustom sidebrain-filename-save-hooks nil
  "Functions to modify filenames before writing them into sidebrain-file-name.
This lets you put in environment variables to substitute, etc, so you can make
the filenames relative to a project directory, for example, and have them make
sense on machines where the project directory is in a different place."
  :type 'hook
  :group 'sidebrain-save)

(defcustom sidebrain-filename-load-hooks nil
  "Functions to modify filenames after reading them from sidebrain-file-name.
This lets you substitute in environment variables, etc -- see sidebrain-filename-save-hooks."
  :type 'hook
  :group 'sidebrain-save)

(defcustom sidebrain-save-more-readable t
  "Whether to make the XML file more readable, rather than more compact."
  :type 'boolean
  :group 'sidebrain-save)

(defcustom sidebrain-xml-verbose nil
  "*Whether sidebrain should report loading and saving in great detail."
  :type 'boolean
   :group 'sidebrain-save)

(defcustom sidebrain-extra-file-header nil
  "Either a string to include at the end of the XML header, or nil."
  :type '(choice (const :tag none nil)
		 (string))
  :group 'sidebrain-save)

(defcustom sidebrain-save-label-hook nil
  "Functions to be applied to the task stack label on saving the stack to file.
You could use this to save information stored by sidebrain-suspend-hook."
  :type 'hook
  :group 'sidebrain-save)

(defcustom sidebrain-load-label-hook nil
  "Functions to be applied on loading the stack from file.
Arguments are the task stack label and the XML structure from which it was created.
You could use this to restoreinformation stored by sidebrain-save-label-hook."
  :type 'hook
   :group 'sidebrain-save)

;;;;;;;;;;;;;;;;;;;;;;;;
;;;; to do comments ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup sidebrain-to-do nil
  "Sidebrain handling of to-do comments."
  :group 'sidebrain)

(defcustom to-do-comment-pattern
  ;; Finding oneself is normally seen as good (in terms of personal
  ;; growth), but not here: so split the string literal so it's not
  ;; found when we search the buffer using it as the search string!
  (concat "to"
	  "do: \\(.+\\)") ; todo: probably should strip comment trailers from end of line
  "*Pattern for recognizing comments marking things to do."
  :type 'regexp
  :group 'sidebrain-to-do)

(defcustom to-do-done-comment-pattern
  (concat "do" ;; see comment in to-do-comment-pattern definition
	  "ne: \\(.+\\)")
  "*Pattern for recognizing comments marking things that have been done."
  :type 'regexp
  :group 'sidebrain-to-do)

(defcustom sidebrain-file-projects nil
  "List describing which projects certain files belong in.
Each element is a list of three strings:
  a regular expression to match against the filename
  the project group name
  the project name
The first one to match is used.
If the group and the project are nil, the comments are not read from the file.
The functions on sidebrain-determine-project-hook are used first."
  :type '(repeat (list regexp string string))
  :group 'sidebrain-to-do)

(defcustom sidebrain-determine-project-hook nil
  "Functions to find which project and project group a file belongs in.
    Functions on this list should return
      a list of (group project) if matched,
      a list of (nil nil) if they want to say this file's comments should be ignored
      nil if they do not recognize the filename
The first one to answer gets it.
If none of these functions answers, sidebrain-file-projects are tried."
  :type 'hook
  :group 'sidebrain-to-do)

(defcustom sidebrain-use-default-project nil
  "*Whether a default project should be used when the project file cannot be determined.
Otherwise, sidebrain-read-todo-from-comments will prompt for a group and project.
Set this non-nil to make it work silently."
  :type 'boolean
  :group 'sidebrain-to-do)

;;;;;;;;;;;;;;;;;;;;;;;
;;;; tasks browser ;;;;
;;;;;;;;;;;;;;;;;;;;;;;

(defgroup sidebrain-browser nil
  "The sidebrain task list browser."
  :group 'sidebrain)

(defcustom sidebrain-browse-mode-hook nil
  "Functions to run on entering sidebrain-browse-mode."
  :type 'hook
  :group 'sidebrain-browser
)

(defcustom sidebrain-browse-tasks-double-spaced t
  "*Whether to put blank lines between entries in the tasks browser."
  :type 'boolean
  :group 'sidebrain-browser)

;;;;;;;;;;;;;;;;;
;;;; display ;;;;
;;;;;;;;;;;;;;;;;

(defgroup sidebrain-display nil
  "Control of the main sidebrain display."
  :group 'sidebrain)

(defcustom sidebrain-buffer "*Task stack*"
  "The buffer for displaying the task stack."
  :group 'sidebrain-display
  :type 'string)

(defcustom sidebrain-task-format "%s\n"
  "Format string for displaying tasks."
  :group 'sidebrain-display
  :type 'string)

(defcustom sidebrain-observation-format "%s\n"
  "Format string for displaying observations."
  :group 'sidebrain-display
  :type 'string)

(defcustom sidebrain-popup-frame nil
  "*Whether to use a popup frame for sidebrain.
It might be better to remove this variable, and couple of others, and
let the user set special-display-buffer-names to include
sidebrain-buffer."
  :group 'sidebrain-display
  :type 'boolean)

(defcustom sidebrain-frame-parameters 
  '((width . 80) (height . 8)
    (auto-raise t)
    (top . 0)
    (left . 0)
    (menu-bar-lines 1)
    (tool-bar-lines 1)
    (title . "Sidebrain")
    (modeline))
  "*Parameters for popup frame for sidebrain."
  :group 'sidebrain-display
  :type '(alist :key-type 'symbol)
  :options '((width integer) (height integer)
	     (auto-raise boolean)
	     (top integer)
	     (left integer)
	     (menu-bar-lines integer)
	     (tool-bar-lines integer)
	     (title string)
	     (modeline boolean)))

;;; end of sidebrain-custom.el
