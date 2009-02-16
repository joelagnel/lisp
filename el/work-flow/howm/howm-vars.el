;;; howm-vars.el --- Wiki-like note-taking tool
;;; Copyright (c) 2005, 2006
;;;   by HIRAOKA Kazuyuki <khi@users.sourceforge.jp>
;;; $Id: howm-vars.el,v 1.31 2006/05/15 13:34:32 hira Exp $
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 1, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; The GNU General Public License is available by anonymouse ftp from
;;; prep.ai.mit.edu in pub/gnu/COPYING.  Alternately, you can write to
;;; the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139,
;;; USA.
;;--------------------------------------------------------------------

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Util

;; ;; Should I use this?
;; (defmacro howm-boundp-q (var)
;;   `(prog1
;;        (boundp ,var)
;;      (howm-dont-warn-free-variable ,var)))
(defmacro howm-dont-warn-free-variable (var)
  "No effect except for inhibition of warning in byte-compilation.
Without this trick, compiler says 'reference to free variable' even when
we have checked availability like (if (boundp xxx) ...)."
  `(when (boundp (quote ,var))
     (defvar ,var nil)))

(put 'howm-funcall-if-defined 'lisp-indent-hook 1)
(defmacro howm-funcall-if-defined (call &rest not-defined)
  "Execute CALL if its car is defined as a function.
Otherwise, execute expressions in NOT-DEFINED.
This is cheat to avoid warning while byte-compilation.
Byte-compiler says \"not known to be defined\" even for codes like
  (if (fboundp 'foo) (foo bar)).

(macroexpand '(howm-funcall-if-defined (migemo-get-pattern roma) nil))
==> (if (fboundp 'migemo-get-pattern)
        (let ((howm-funcall-if-defined-f 'migemo-get-pattern))
          (funcall howm-funcall-if-defined-f roma))
      nil)
"
  (let ((func (car call))
        (args (cdr call)))
    `(if (fboundp (quote ,func))
         (let ((howm-funcall-if-defined-f (quote ,func)))
           (funcall howm-funcall-if-defined-f ,@args))
       ,@not-defined)))

;; copied and modified from mule-cmds.el
;; snap:///usr/share/emacs/21.2/lisp/international/mule-cmds.el#1870:(defun set-locale-environment (locale-name)
(defun howm-get-locale ()
  (let ((vars '("LC_ALL" "LC_CTYPE" "LANG"))
        (locale nil))
    (while (and vars (not (setq locale (getenv (car vars)))))
      (setq vars (cdr vars)))
    (or locale "")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Top

(defgroup howm nil
  "Wiki-like note-taking tool."
  :group 'applications)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Files

(defgroup howm-files nil
  "Names of files and directories."
  :group 'howm)

(defcustom howm-directory "~/howm/"
  "*All files under this directory are scanned recursively."
  :type 'directory
  :group 'howm-files)

(defcustom howm-file-name-format "%Y/%m/%Y-%m-%d-%H%M%S.howm"
  "Name of new file. See `format-time-string'.
For example, set as \"%Y/%m/%Y-%m-%d-%H%M%S.howm\" to separate each entry
to its own file. You must guarantee (string< oldfile newfile)."
  :type '(radio (const :tag "One file for one entry"
                       "%Y/%m/%Y-%m-%d-%H%M%S.howm")
                (const :tag "One file for one day" "%Y/%m/%Y-%m-%d.howm")
                (const :tag "One file for one month" "%Y/%Y-%m.howm")
                (const :tag "One file for one year" "%Y.howm")
                string)
  :group 'howm-efficiency
  :group 'howm-files)

(defcustom howm-keyword-file "~/.howm-keys"
  "*Keywords (WikiNames) are stored in this file."
  :type 'file
  :group 'howm-files)

;; inhibit warning in compilation.
(howm-dont-warn-free-variable image-file-name-regexps)
(defvar howm-image-file-name-regexps
  (let ((exts-regexp "\\.\\(GIF\\|JP\\(?:E?G\\)\\|P\\(?:BM\\|GM\\|NG\\|PM\\)\\|TIFF?\\|X\\(?:[BP]M\\)\\|gif\\|jp\\(?:e?g\\)\\|p\\(?:bm\\|gm\\|ng\\|pm\\)\\|tiff?\\|x\\(?:[bp]m\\)\\)\\'")
        (image-file-name-regexps (and (boundp 'image-file-name-regexps)
                                      image-file-name-regexps)))
    ;; copied from image-file-name-regexp.
    (if image-file-name-regexps
        (mapconcat 'identity
                   (if exts-regexp
                       (cons exts-regexp image-file-name-regexps)
                     image-file-name-regexps)
                   "\\|")
      exts-regexp))
  "Regular expression that matches image-file filenames.
Default value is equal to the result of `image-file-name-regexp'
on GNU Emacs 21.2.1.

In order to use `image-file-name-regexp' on Meadow 2.10 (ASAGAO),
max-specpdl-size must be increased from the default value 600.
Otherwise, an error occurs both in byte-compilation and in run time.
To avoid such troubles, this variable is prepared as a fixed string.")

(defvar howm-excluded-file-regexp-common-list
  (list "[~#]$"
        "\\.\\(bak\\|elc\\|gz\\|aux\\|toc\\|idx\\|dvi\\)$"
        howm-image-file-name-regexps))
(defvar howm-excluded-file-regexp-dir-sep
  (if (let ((case-fold-search t))
        (string-match "windows" (symbol-name system-type)))
      "[/\\\\]" ;; / or \ for win
    "/")) ;; / otherwise
(let ((dir-head (concat "\\(^\\|" howm-excluded-file-regexp-dir-sep "\\)"))
      (cvs (concat "CVS" howm-excluded-file-regexp-dir-sep)))
  (defvar howm-excluded-file-regexp-dots-ok
    (mapconcat #'identity
               `(,(concat dir-head cvs) ;; "\\(^\\|/\\)CVS/"
                 "^[.][.]"
                 ,@howm-excluded-file-regexp-common-list)
               "\\|"))
  (defvar howm-excluded-file-regexp-dots-ng
    (mapconcat #'identity
               `(,(concat dir-head "\\([.]\\|" cvs "\\)")
                 ;; "\\(^\\|/\\)\\([.]\\|CVS/\\)"
                 ,@howm-excluded-file-regexp-common-list)
               "\\|")))
;;   "\\(^\\|/\\)\\([.]\\|CVS/\\)\\|[~#]$\\|\\.\\(bak\\|elc\\|gz\\|aux\\|toc\\|idx\\|dvi\\|jpg\\|gif\\|png\\)$"

(defcustom howm-excluded-file-regexp howm-excluded-file-regexp-dots-ng
  "Regexp for excluded files.
It is checked for relative paths from howm-directory and howm-search-path.
A file is excluded iff this regexp matches with all the relative paths."
  :type `(radio (const :tag "Don't search dot files"
                       ,howm-excluded-file-regexp-dots-ng)
                (const :tag "Search dot files"
                       ,howm-excluded-file-regexp-dots-ok)
                regexp)
  :group 'howm-files
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Menu

(defgroup howm-menu nil
  "Menu."
  :group 'howm)

(defcustom howm-menu-lang
  (if (or (and (boundp 'current-language-environment)
               (string= current-language-environment "Japanese"))
          (string-match "^ja" (howm-get-locale)))
      'ja
    'en)
  "*Language of menu."
  :type '(radio (const en) (const ja))
  :group 'howm-menu)

(defcustom howm-menu-file nil
  "*Specify menu file explicitly, or set as nil to search every time."
  :type '(radio (const :tag "Search every time" nil)
                (const "0000-00-00-000000.howm")
                file)
  :group 'howm-files
  :group 'howm-efficiency
  :group 'howm-menu)

(defcustom howm-menu-expiry-hours 0
  "*Cache menu contents for this number of hours."
  :type 'number
  :group 'howm-efficiency
  :group 'howm-menu)

(defcustom howm-menu-refresh-after-save t
  "*If non-nil, refresh menu contents after you save howm note."
  :type 'boolean
  :group 'howm-efficiency
  :group 'howm-menu)

(defcustom howm-menu-name-format "*howmM:%s*"
  "*Name format of menu buffer."
  :type '(radio (const :tag "Never show in normal buffer list" " *howmM:%s*")
                string)
  :group 'howm-menu)

(defcustom howm-menu-footer nil
  "Footer string for each menu. Nil means no footer."
  :type '(radio (const :tag "Off" nil)
                string)
  :group 'howm-menu)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reminder

(defgroup howm-reminder nil
  "Schedule and todo list."
  :group 'howm)

(defvar howm-reminder-old-format nil)

(defvar howm-reminder-marks
  ;; Be careful to order of characters.
  ;; "-" must be first so that regexp "[-+~!@.]" makes sense.
  (if howm-reminder-old-format "-+~!@. " "-+~!@."))
(defvar howm-reminder-types
  (format "[%s]" howm-reminder-marks))

(defun howm-custom-reminder-get-types (symbol)
  (let ((reg (default-value symbol))
        (default-types (split-string howm-reminder-marks "")))
    ;; return list of types for standard cases
    ;; and string itself for nonstandard cases
    (if (not (string-match "^\\[\\(.*\\)\\]" reg))
        reg
      (let ((types (split-string (match-string-no-properties 1 reg) "")))
        (if (howm-cl-find-if-not (lambda (x) (member x default-types))
                                 types)
            reg
          (howm-cl-remove-if-not (lambda (x) (member x types))
                                 default-types))))))
(defun howm-custom-reminder-set-types (symbol types)
  (when (listp types)
    (setq types (apply #'concat `("[" ,@types "]"))))
  (set-default symbol types))
(defun howm-custom-reminder-list-types ()
  `(radio (set ,@(mapcar (lambda (ty) (list 'const ty))
                         (split-string howm-reminder-marks "")))
          string))

(defcustom howm-schedule-types "[!@.]"
  "*Regular expression of reminder types which are listed as schedule."
  :get #'howm-custom-reminder-get-types
  :set #'howm-custom-reminder-set-types
  :type (howm-custom-reminder-list-types)
  :group 'howm-efficiency
  :group 'howm-reminder)

(defcustom howm-todo-types
  (if howm-reminder-old-format "[-+~! .]" "[-+~!.]")
  "*Regular expression of reminder types which are listed as todo."
  :get #'howm-custom-reminder-get-types
  :set #'howm-custom-reminder-set-types
  :type (howm-custom-reminder-list-types)
  :group 'howm-efficiency
  :group 'howm-reminder)

(defcustom howm-congrats-format '("Finished %s tasks!")
  "List of format strings to generate message when a reminder is finished.
One of elements is chosen randomly every time."
  :type '(repeat string)
  :group 'howm-reminder)

(defcustom howm-reminder-cancel-string "cancel"
  "*This string is inserted automatically when a reminder is canceled."
  :type 'string
  :group 'howm-reminder)

(defcustom howm-action-lock-forward-save-buffer nil
  "*Non nil if direct manipulation on reminder list should cause auto-save."
  :type 'boolean
  :group 'howm-reminder)

(defcustom howm-action-lock-forward-kill-buffer nil
  "*Non nil if direct manipulation on reminder list should cause kill-buffer.
Be careful that you cannot undo the result of action-lock after kill-buffer."
  :type 'boolean
  :group 'howm-reminder)

;;;
;;; Menu reminder
;;;

(defgroup howm-menu-reminder nil
  "Reminders shown in menu."
  :group 'howm-menu
  :group 'howm-reminder)

(defcustom howm-schedule-menu-types "[!@]"
  "*Regular expression of reminder types which are shown in menu as schedule."
  :get #'howm-custom-reminder-get-types
  :set #'howm-custom-reminder-set-types
  :type (howm-custom-reminder-list-types)
  :group 'howm-efficiency
  :group 'howm-menu-reminder)

(defcustom howm-todo-menu-types
  (if howm-reminder-old-format "[-+~! .]" "[-+~!.]")
  "*Regular expression of reminder types which are shown in menu as todo."
  :get #'howm-custom-reminder-get-types
  :set #'howm-custom-reminder-set-types
  :type (howm-custom-reminder-list-types)
  :group 'howm-efficiency
  :group 'howm-menu-reminder)

(defcustom howm-menu-schedule-days 7
  "*Show schedule in menu until this number of days from now."
  :type 'number
  :group 'howm-menu-reminder)

(defcustom howm-menu-schedule-days-before 0
  "*Show schedule in menu from this number of days ago."
  :type 'number
  :group 'howm-menu-reminder)

(defcustom howm-menu-todo-num 50
  "*Maximum number of todo items shown in menu."
  :type 'number
  :group 'howm-menu-reminder)

(defvar howm-huge- 66666)
(defvar howm-huge 77777)
(defvar howm-huge+ 88888)

(defcustom howm-menu-todo-priority (- howm-huge+)
  "*Limit priority for elimination of reminders in menu."
  :type `(radio (const :tag "Show sleeping reminders",(- howm-huge+))
                (const :tag "Hide sleeping reminders" ,(- howm-huge-))
                number)
  :group 'howm-menu-reminder)

(defcustom howm-todo-priority-done-bottom (- howm-huge+)
  "*Priority of done reminder."
  :type `(radio (const :tag "Deeper than sleeping reminders" ,(- howm-huge+))
                (const :tag "Shallower than sleeping reminders"
                       ,(- howm-huge-))
                number)
  :group 'howm-menu-reminder)

(defcustom howm-menu-recent-num 20
  "*Maximum number of recent items shown in menu."
  :type 'number
  :group 'howm-menu-reminder)

(defcustom howm-menu-recent-regexp nil
  "Regexp which is regarded as title line in recent list in menu.
When it is nil, `howm-view-title-regexp' is used."
  :type '(radio (const :tag "Default" nil)
                regexp)
  :group 'howm-title
  :group 'howm-menu-reminder)

(defcustom howm-menu-todo-priority-format nil
  "*Format for priority display in todo list in menu, or nil for no display."
  :type '(radio (const :tag "Off" nil)
                (const "(%8.1f)")
                string)
  :group 'howm-devel
  :group 'howm-menu-reminder)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; List

(defgroup howm-list nil
  "Style of list view."
  :group 'howm)

(defcustom howm-view-contents-limit 10000
  "*Max length for howm-view-contents. Nil means no limit."
  :type '(radio (const :tag "No limit" nil)
                integer)
  :group 'howm-list)

(defcustom howm-view-summary-keep-cursor t
  "*If non-nil, keep cursor position when you open a note from summary list."
  :type 'boolean
  :group 'howm-list)

(defcustom howm-view-summary-omit-same-name t
  "*If non-nil, same name is not written repeatedly in summary list."
  :type 'boolean
  :group 'howm-list)

(defcustom howm-list-recent-days 7
  "*This number of days are listed by `howm-list-recent'."
  :type 'integer
  :group 'howm-list)

;;
;; Sort
;;

(defgroup howm-sort nil
  "Sorting and filtering of matched entries."
  :group 'howm-list)

(defcustom howm-list-normalizer 'howm-view-sort-by-mtime
  "*Default method to list matched notes."
  :type '(radio (function-item :tag "Sort by edit-time" howm-view-sort-by-mtime)
                (function-item :tag "Sort by create-time"
                               howm-view-sort-by-reverse-date)
                function)
  :group 'howm-sort)

(defcustom howm-list-prefer-word nil
  "*Matches to whole word are listed first in summary buffer."
  :type 'boolean
  :group 'howm-sort)

(defcustom howm-list-prefer-wiki t
  "*Matches to wiki tags are listed first in summary buffer."
  :type 'boolean
  :group 'howm-sort)

;;
;; Title
;;

(defgroup howm-title nil
  "Title of each entry."
  :group 'howm-list)

;; I don't know the way to generate this list automatically. Sigh...
(defvar howm-custom-command-list
  `(set ,@(mapcar (lambda (com) (list 'const com))
                  '(howm-list-all
                    howm-list-recent
                    howm-list-around
                    howm-keyword-search
                    howm-list-grep
                    howm-list-grep-fixed
                    howm-list-migemo
                    howm-list-related
                    howm-action-lock-date-search
                    ))))

(defcustom howm-list-title
  '(
    howm-list-all
    howm-list-recent
    howm-list-around
    ; howm-keyword-search
    ; howm-list-grep howm-list-grep-fixed howm-list-migemo
    ; howm-list-related
    howm-action-lock-date-search
    )
  "List of commands in which titles are listed instead of matched lines.
T means 'always'.
If it is a function, the evaluated value is used instead of itself."
  :type `(radio (const :tag "Always" t)
                (const :tag "Never" nil)
                ,howm-custom-command-list
;;                 (set (const howm-list-all)
;;                      (const howm-list-recent)
;;                      (const howm-list-around)
;;                      (const howm-keyword-search)
;;                      (const howm-list-grep)
;;                      (const howm-list-grep-fixed)
;;                      (const howm-list-migemo)
;;                      (const howm-list-related))
                function)
  :group 'howm-efficiency
  :group 'howm-title)

(defcustom howm-list-title-regexp nil
  "Regexp which is regarded as title line in summary buffer.
When it is nil, `howm-view-title-regexp' is used."
  :type '(radio (const :tag "Default" nil)
                regexp)
  :group 'howm-title)

(defcustom howm-list-title-undo t
  "*Non-nil if `howm-list-title' should toggle whether title is shown or not."
  :type 'boolean
  :group 'howm-efficiency
  :group 'howm-title)

;;
;; BufWin
;;

(defgroup howm-list-bufwin nil
  "Buffers and windows for listing search result."
  :group 'howm-list)

(defcustom howm-view-summary-name "*howmS*"
  "Format string of buffer name for summary.
%s is replaced with searched string. See `format'."
  :type '(radio (const :tag "Use one common buffer" "*howmS*")
                (const :tag "Make new buffer for each search" "*howmS:%s*")
                string)
  :group 'howm-list-bufwin)

(defcustom howm-view-contents-name "*howmC*"
  "Format string of buffer name for contents.
%s is replaced with searched string. See `format'."
  :type '(radio (const :tag "Use one common buffer" "*howmC*")
                (const :tag "Make new buffer for each search" "*howmC:%s*")
                string)
  :group 'howm-list-bufwin)

(defcustom howm-view-summary-persistent t
  "*If non-nil, keep summary buffer on howm-view-summary-open by default.
If it is a function, the evaluated value is used instead of itself."
  :type 'boolean
  :group 'howm-list-bufwin)

(defcustom howm-view-contents-persistent t
  "*If non-nil, keep contents buffer on howm-view-contents-open by default.
If it is a function, the evaluated value is used instead of itself."
  :type 'boolean
  :group 'howm-list-bufwin)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Search

(defgroup howm-search nil
  "Search methods."
  :group 'howm)

(defcustom howm-keyword-case-fold-search nil
  "*Non-nil if searches of come-from keywords should ignore case."
  :type 'boolean
  :group 'howm-search)

(defcustom howm-view-update-search-ring nil
  "*Non-nil if search-ring should be updated in howm search."
  :type 'boolean
  :group 'howm-search)

(defcustom howm-message-time nil
  "*Non nil if search etc. should show took time."
  :type 'boolean
  :group 'howm-devel
  :group 'howm-search)

(defcustom howm-history-file "~/.howm-history"
  "*Search history is recorded to that file."
  :type 'file
  :group 'howm-files
  :group 'howm-search)

(defcustom howm-history-limit 50
  "*Limit number of recorded search history, or nil for no limit.
Set 0 to inhibit recording."
  :type '(radio (const :tag "No limit" nil)
                integer)
  :group 'howm-search)

(defcustom howm-history-unique t
  "*If non-nil, duplicated entries are removed from search history."
  :type 'boolean
  :group 'howm-search)

(defcustom howm-keyword-list-alias-sep "\t"
  "*Separator string for alias keywords in the keyword file `howm-keyword-file'.
If it is nil, alias of come-from keyword is disabled."
  :type '(radio (const :tag "Disable aliases" nil)
                (const :tag "Tab" "\t")
                string)
  :group 'howm-search)

(defcustom howm-keyword-aliases-recursive t
  "*Non nil if aliases of come-from keywords should be expanded recursively."
  :type 'boolean
  :group 'howm-search)

;;;
;;; grep
;;;

(defgroup howm-grep nil
  "Use external grep command for fast search."
  :group 'howm-efficiency
  :group 'howm-search)

(defcustom howm-view-use-grep nil
  "*If non-nil, use external grep command for search.
Performance must be improved greatly if you set this.
When the value is elisp function, it is used instead of `howm-view-fake-grep'."
  :type '(radio (const :tag "On" t)
                (const :tag "Off" nil)
                function)
  :group 'howm-grep)

;; These variables should be renamed: howm-view-xxx ==> howm-xxx.
(defcustom howm-view-grep-command "grep"
  "*Command name for grep."
  :type 'string
  :group 'howm-grep)
(defvar howm-view-fgrep-command nil
  "*Command name for fgrep.
This variable is obsolete and may be removed in future.")
(defcustom howm-view-grep-option "-Hnr"
  "*Common grep option for howm."
  :type 'string
  :group 'howm-grep)
(defcustom howm-view-grep-extended-option "-E"
  "*Grep option for extended regular expression."
  :type 'string
  :group 'howm-grep)
(defcustom howm-view-grep-fixed-option "-F"
  "*Grep option to search fixed strings."
  :type 'string
  :group 'howm-grep)
(defcustom howm-view-grep-ignore-case-option "-i"
  "*Grep option for ignoring case distinctions."
  :type 'string
  :group 'howm-grep)
(defcustom howm-view-grep-expr-option "-e"
  "*Grep option for pattern."
  :type 'string
  :group 'howm-grep)
(defcustom howm-view-grep-file-stdin-option "-f -"
  "*Grep option for receiving patterns from standard input.
If this is nil, pattern is received as command line argument."
  :type '(radio (const :tag "Off" nil)
                string)
  :group 'howm-grep)
;; (defvar howm-view-grep-command "egrep")
;; (defvar howm-view-fgrep-command "fgrep")
;; (defvar howm-view-grep-extended-option nil)
;; (defvar howm-view-grep-fixed-option nil)
;; (defvar howm-view-grep-file-stdin-option "-f -")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc

(defgroup howm-misc nil
  "Miscellaneous customization."
  :group 'howm)

(defvar howm-prefix "\C-c,"
  "Howm commands are invoked by this prefix + some keys.")

(defcustom howm-random-walk-wait 2
  "*Seconds of wait in `howm-random-walk'."
  :type 'number
  :group 'howm-misc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Create

(defgroup howm-create nil
  "Create new note."
  :group 'howm-misc)

(defcustom howm-prepend nil
  "*Non nil if new entries should be prepended to previous entries.
Otherwise, they are appended."
  :type '(radio (const :tag "Append" nil)
                (const :tag "Prepend" t))
  :group 'howm-create)

(defcustom howm-content-from-region nil
  "*When the value non-nil, selected string is inserted as default content.
Unless the value is t, single-line selection is inserted as title instead.
This variable is ignored when `transient-mark-mode' is nil."
  :type '(radio (const :tag "Off" nil)
                (const :tag "Single line selection is copied as title" 1)
                (const :tag "Any selection is copied as content" t))
  :group 'howm-create)

(defcustom howm-title-from-search nil
  "*Non nil if searched keyword is inserted as default title
when `howm-create' is called on summary buffer."
  :type 'boolean
  :group 'howm-create)

(defcustom howm-create-here-just nil
  "*Non nil if `howm-create-here' should insert new entry into cursor position
rather than append or prepend."
  :type '(radio (const :tag "Append or prepend" nil)
                (const :tag "Just here" t))
  :group 'howm-create)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Viewer

(defgroup howm-viewer nil
  "External viewers for images, movies, sounds, etc."
  :group 'howm-misc)

(defun howm-try-require (feature)
  (and (locate-library (symbol-name feature))
       (require feature)))

;; These variables should be renamed.

(defcustom howm-view-external-viewer-assoc nil
  "List of viewer specifications.
Each specification must be a cons pair of type and format.
Type is a regular expression of file names.
Format is a command string in which %s is replaced with file name.
This setting is prior to mailcap.

Example:
  (setq howm-view-external-viewer-assoc
        '(
          (\"[.]\\(jpg\\|gif\\|png\\)$\" . \"display %s\")
          (\"[.]dvi$\" . \"xdvi %s\")
         ))
"
  :type '(alist :key-type regexp :value-type string)
  :group 'howm-viewer)

(defcustom howm-view-use-mailcap
  (and (howm-try-require 'mailcap)
       (fboundp 'mailcap-parse-mailcaps)
       (fboundp 'mailcap-parse-mimetypes))
  "*Non nil if external viewers should be selected according to mailcap.
Mailcap processing depends on gnus/mailcap, and old FLIM library may
cause conflicts."
  :type 'boolean
  :group 'howm-viewer)

(defcustom howm-view-open-by-myself '("text/.*" "application/emacs-lisp")
  "List of regular expressions for mime types which should be opened normally."
  :type '(repeat regexp)
  :group 'howm-viewer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Narrow

(defgroup howm-narrow nil
  "Narrowing to each entry."
  :group 'howm-misc)

(defcustom howm-auto-narrow t
  "List of commands after which the function `howm-auto-narrow' can work.
If the value is t, it means 'always'."
  :type `(radio (const :tag "Never" nil)
                (const :tag "Always" t)
                ,howm-custom-command-list)
  :group 'howm-narrow)

(mapc (lambda (hook) (custom-add-option hook 'howm-auto-narrow))
      '(howm-view-open-hook howm-create-hook))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Efficiency

(defgroup howm-efficiency nil
  "To improve performance, use grep and turn off expensive options."
  :group 'howm)

(defcustom howm-refresh-after-save t
  "*Redraw links after you save howm note."
  :type 'boolean
  :group 'howm-efficiency)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Face

(defgroup howm-faces nil
  "Colors and fonts."
  :group 'faces
  :group 'howm)

(defcustom howm-use-color t
  "*If non-nil, highlight tags in howm-mode verbosely."
  :type 'boolean
  :group 'howm-faces)

(defface howm-view-hilit-face
  '((((class color)) (:foreground "red"))
    (t ()))
  "*Face for matched word."
  :group 'howm-faces)

(defface howm-view-name-face
  '((((class color)) (:foreground "white" :background "blue"))
    (t ()))
  "*Face for file name in summary buffer."
  :group 'howm-faces)

(defface howm-view-empty-face
  '((((class color)) (:background "midnight blue"))
    (t ()))
  "*Face for empty field in summary buffer."
  :group 'howm-faces)

(defface howm-mode-title-face ;; =
  '((((class color)) (:foreground "RoyalBlue"))
    (t ()))
  "*Face for title."
  :group 'howm-faces)
(defface howm-mode-ref-face ;; >>>
  '((((class color) (background light)) (:foreground "blue"))
    (((class color) (background dark)) (:foreground "cyan"))
    (t ()))
  "*Face for goto link."
  :group 'howm-faces)
(defface howm-mode-keyword-face ;; <<<
  '((((class color)) (:foreground "white" :background "blue"))
    (t ()))
  "*Face for come-from link."
  :group 'howm-faces)
(defface howm-mode-wiki-face ;; [[]]
  '((((class color) (background light)) (:foreground "blue"))
    (((class color) (background dark)) (:foreground "cyan"))
    (t ()))
  "*Face for wiki link."
  :group 'howm-faces)

(defface howm-reminder-normal-face
  '((((class color)) (:foreground "blue"))
    (t ()))
  "*Face for normal reminder."
  :group 'howm-faces)
(defface howm-reminder-todo-face
  '((((class color) (background light)) (:foreground "purple"))
    (((class color) (background dark)) (:foreground "yellow"))
    (t ()))
  "*Face for todo."
  :group 'howm-faces)
(defface howm-reminder-defer-face
  '((((class color)) (:foreground "magenta"))
    (t ()))
  "*Face for defer."
  :group 'howm-faces)
(defface howm-reminder-deadline-face
  '((((class color)) (:foreground "red"))
    (t ()))
  "*Face for deadline."
  :group 'howm-faces)
(defface howm-reminder-schedule-face
  '((((class color) (background light)) (:foreground "dark green"))
    (((class color) (background dark)) (:foreground "green"))
    (t ()))
  "*Face for schedule."
  :group 'howm-faces)
(defface howm-reminder-done-face
  '((((class color) (background light)) ())
    (((class color) (background dark)) (:foreground "gray"))
    (t ()))
  "*Face for done reminder."
  :group 'howm-faces)
(defface howm-reminder-today-face
  '((((class color)) (:foreground "black" :background "orange"))
    (t ()))
  "*Face for today."
  :group 'howm-faces)
(defface howm-reminder-tomorrow-face
  '((((class color)) (:foreground "black" :background "pink"))
    (t ()))
  "*Face for tommorow."
  :group 'howm-faces)

(defface howm-menu-list-face ;; item header in menu-mode list (schedule, todo)
  '((t ()))
  "*Face for list in menu."
  :group 'howm-faces)
(defface howm-menu-key-face ;; shortcut key in menu-mode
  '((((class color) (background light)) (:foreground "dark red"))
    (((class color) (background dark)) (:foreground "orange"))
    (t ()))
  "*Face for key binding in menu."
  :group 'howm-faces)

(defvar howm-view-hilit-face 'howm-view-hilit-face
  "*Face for matched word.")
(defvar howm-view-name-face  'howm-view-name-face
  "*Face for file name in summary buffer.")
(defvar howm-view-empty-face 'howm-view-empty-face
  "*Face for empty field in summary buffer.")
(defvar howm-mode-title-face   'howm-mode-title-face
  "*Face for title.")
(defvar howm-mode-ref-face     'howm-mode-ref-face
  "*Face for goto link.")
(defvar howm-mode-keyword-face 'howm-mode-keyword-face
  "*Face for come-from link.")
(defvar howm-mode-wiki-face    'howm-mode-wiki-face
  "*Face for wiki link.")
(defvar howm-reminder-normal-face   'howm-reminder-normal-face
  "*Face for normal reminder.")
(defvar howm-reminder-todo-face     'howm-reminder-todo-face
  "*Face for todo.")
(defvar howm-reminder-defer-face    'howm-reminder-defer-face
  "*Face for defer.")
(defvar howm-reminder-deadline-face 'howm-reminder-deadline-face
  "*Face for deadline.")
(defvar howm-reminder-schedule-face 'howm-reminder-schedule-face
  "*Face for schedule.")
(defvar howm-reminder-done-face     'howm-reminder-done-face
  "*Face for done reminder.")
(defvar howm-reminder-today-face    'howm-reminder-today-face
  "*Face for today.")
(defvar howm-reminder-tomorrow-face 'howm-reminder-tomorrow-face
  "*Face for tommorow.")
(defvar howm-menu-list-face 'howm-menu-list-face
  "*Face for list in menu.")
(defvar howm-menu-key-face  'howm-menu-key-face
  "*Face for key binding in menu.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hook

(defgroup howm-hooks nil
  "Hooks."
  :group 'howm)

(defcustom howm-mode-hook nil
  "Hook run at the end of function `howm-mode'"
  :type 'hook
  :group 'howm-hooks)

(defcustom howm-mode-on-hook nil
  "Hook run when `howm-mode' is turned on."
  :type 'hook
  :group 'howm-hooks)

(defcustom howm-mode-off-hook nil
  "Hook run when `howm-mode' is turned off."
  :type 'hook
  :group 'howm-hooks)

(defcustom howm-view-open-hook nil
  "Hook run when open a note from summary/contents buffer."
  :type 'hook
  :group 'howm-narrow
  :group 'howm-hooks)

(defcustom howm-view-before-open-hook nil
  "Hook run before open something from summary or contents buffer."
  :type 'hook
  :group 'howm-hooks)

(defcustom howm-create-file-hook nil
  "Hook run when buffer for new note is created."
  :type 'hook
  :group 'howm-hooks)

(defcustom howm-create-hook nil
  "Hook run after new note is created and set up."
  :type 'hook
  :group 'howm-narrow
  :group 'howm-hooks)

(defcustom howm-menu-hook nil
  "Hook run at the end of `howm-menu-refresh'."
  :type 'hook
  :group 'howm-hooks)

(defcustom howm-congrats-hook nil
  "Hook run at the end of `howm-congrats'."
  :type 'hook
  :group 'howm-hooks)

(defcustom howm-after-save-hook nil
  "Hook run at the end of `howm-after-save'."
  :type 'hook
  :group 'howm-hooks)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Devel

(defgroup howm-devel nil
  "Developers' diagnoses."
  :group 'howm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Experimental

(defgroup howm-experimental nil
  "Test of experimental features."
  :group 'howm)

(defcustom howm-reminder-menu-types
  (if howm-reminder-old-format "[-+~!@ ]" "[-+~!@]")
  "*Regular expression of reminder types which are shown in menu."
  :get #'howm-custom-reminder-get-types
  :set #'howm-custom-reminder-set-types
  :type (howm-custom-reminder-list-types)
  :group 'howm-experimental)

(defcustom howm-schedule-sort-by-time nil
  "Non nil if `howm-schedule-sort-converter' should consider time part."
  :type 'boolean
  :group 'howm-experimental)

(defcustom howm-user-font-lock-keywords nil
  "Font lock keywords for all howm-related buffers.
See help of `font-lock-keywords' for details."
;;   :type '(repeat (radio (cons regexp (list (const quote) face))
;;                         sexp))
  :type 'sexp
  :group 'howm-experimental)

(let ((sep "-------------------------------------"))
  (defcustom howm-menu-reminder-separators nil
    "Assoc list to specify positions and strings of separators in reminder
in menu. For each element, car is days from now, and cdr is separator string.
If car is nil, it means the boarder between schedule and todo.
This option is prepared for `howm-menu-reminder'."
    :type `(radio (const :tag "No separators" nil)
                  (const :tag "Standard separators"
                         ((-1 . ,sep)
                          (0 . ,sep)
                          (nil . ,sep)))
                  (alist :key-type
                         (radio number
                                (const :tag "Between schedule and todo"))
                         :value-type string))
    :group 'howm-experimental))

(defcustom howm-process-coding-system nil
  "*Default coding system for grep command in howm.
If the value is a symbol, it is used for both read and write.
If the value is a cons pair, its car and cdr are used for read and write,
respectively.

Example:
 (setq howm-process-coding-system 'euc-japan-unix)
 (setq howm-process-coding-system '(utf-8-unix . sjis-unix))"
  :type '(radio (const :tag "Off" nil)
                coding-system
                (cons coding-system coding-system))
;;   :group 'howm-grep
  :group 'howm-experimental
  )

(defcustom howm-view-dired-keep-cursor nil
  "*Non nil if `howm-view-dired' should keep cursor position."
  :type 'boolean
  :group 'howm-experimental)

;;;

(provide 'howm-vars)

;;; howm-vars.el ends here
