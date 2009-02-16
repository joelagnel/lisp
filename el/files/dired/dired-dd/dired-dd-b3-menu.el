;;; dired-dd-b3-menu.el --- dired-dd's mouse button 3 menu example.

;; This file was split from dired-dd.el to make this part a customizable
;; module.  Always required by dired-dd.

;; Authour: Seiichi Namba <sn@asahi-net.email.ne.jp>
;; Copyright (C) 1997,1998,1999,2000,2001 Seiichi Namba <sn@asahi-net.email.ne.jp>

;;  This program is free software; you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published by
;;  the Free Software Foundation; either version 2 of the License, or
;;  (at your option) any later version.
;;
;;  This program is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU General Public License for more details.
;;
;;  You should have received a copy of the GNU General Public License
;;  along with this program; if not, write to the Free Software
;;  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

;; Test version:        Wed Dec 17 03:57:03 1997
;; Adopted:             Fri Dec 19 01:48:22 1997
;;
;; Sun Dec 20 02:21:46 1998 
;;     Defining default of dired-dd-terminal-command:
;;         Sets dummy string if no $LANG is set (getenv => nil).
;;         The fix suggested by Yuji Yamano <yyamano@kt.rim.or.jp>.
;; Sun Jan  3 02:13:34 1999
;;     dired-dd-shell-command and dired-dd-shell-command2 were rewritten for
;;     shell meta-character and space-in-filename quoting. Now they are
;;     fully-dired-x-compatibe.  dired-dd-shell-command2's argument format
;;     is changed.

(eval-when-compile
  (require 'dired) ;; probably only this is necessary to make working elc.
  (require 'dired-aux))

(defvar dired-dd-b3-keymap
;;(defconst dired-dd-b3-keymap
  '(keymap
    (get-term "Launch a Terminal" (nil)
	      dired-dd-exec-async-shell-command
	      dired-dd-terminal-command nil)
    (separator-mymap "--" (nil))
    (copy "Copy" (nil) dired-do-copy arg)
    (copyrecursive "Copy Recursive" (nil) dired-dd-do-copy-recursive arg)
    (move "Move" (nil) dired-do-rename arg)
    (separator-mymap "--" (nil))
    (up-tree "Up Tree" (nil) dired-up-directory arg)
    (newdir "New directory" (nil) dired-create-directory
	    (read-file-name "Create directory: "
			    (dired-current-directory)))
    (separator-mymap "--" (nil))
    (delete "Delete" (nil) dired-do-delete arg)
    (flagged-delete "Delete Flagged" (nil) dired-do-flagged-delete)
    ;; Sat Apr  7 16:36:48 2001, `dired-dd-shell-rm-R' deprecated.
    ;; (rm-R "Delete Recursive" (nil) dired-dd-shell-rm-R fn-list)
    (rm-R "Delete Recursive" (nil) dired-dd-delete-recursive fn-list)
    (separator-mymap "--" (nil))
    ;; shortname -> dired-get-marked-files
    ;; This entry is now used from M-double-down-mouse-1
    (shellcmd "Execute Command On" (nil) dired-do-shell-command
	      (dired-read-shell-command
	       (concat "! on " "%s: ") current-prefix-arg
	       ;;shortnames
	       (dired-get-marked-files t arg)
	       ) arg)
;;;	   (shellcmd "Execute Command On" (nil) dired-do-shell-command
;;;		     (dired-read-shell-command
;;;		      (concat "! on " "%s: ") current-prefix-arg
;;;		      (dired-dd-get-marked-files t current-prefix-arg)) arg)
    (separator-mymap "--" (nil))
    (file-strip "File, Strip"
		(nil)
		keymap
		(file "File" (nil)
		      progn (dired-dd-shell-command "file" shortnames))
		(strip "strip -v" (nil)
		       progn (dired-dd-shell-command "strip -v" shortnames)
		       (revert-buffer))
		"File, Strip")
    (separator-mymap "--" (nil))
    (arc "Tar, Zip"
	 (nil)
	 keymap
	 (zvtf "tar zvtf" (nil) dired-dd-do-tar-zvtf shortnames)
	 (zvxf "tar zvxf" (nil)
	       progn (dired-dd-do-tar-zvxf shortnames)
	       (revert-buffer))
	 (separator-mymap "--" (nil))
	 (gzip "gzip -9" (nil)
	       progn (dired-dd-shell-command "gzip" shortnames)
	       (revert-buffer))
	 (gunzip "gunzip" (nil) 
		 progn (dired-dd-shell-command "gunzip" shortnames)
		 (revert-buffer))
	 (separator-mymap "--" (nil))
	 (unzip-l "unzip -l" (nil) 
		progn (dired-dd-do-unzip-l shortnames)
		(revert-buffer))
	 (unzip "unzip" (nil) 
		progn (dired-dd-do-unzip shortnames)
		(revert-buffer))
	 (lha-v "lha v"  (nil)  dired-dd-do-lha-v shortnames)
	 (lha-x "lha x" (nil) 
		progn (dired-dd-do-lha-x shortnames) (revert-buffer))
	 "Tar, Zips")
;;; Maybe handy, but the menu looks ugly Thu Apr 20 13:01:18 2000
;;;    (separator-mymap "--" (nil))
;;;    (html "Open local HTML" (nil) dired-dd-w3-open-local shortnames)
    (separator-mymap "--" (nil))
    (doc "Man, Info, HTML"
	 (nil)
	 keymap
	 (mandoc "groff -mandoc" (nil) dired-dd-do-mandoc shortnames)
	 (w3openlocal "w3-open-local" (nil) dired-dd-w3-open-local shortnames)
	 ;; Pass `shortnames' and it expands to URL.
	 (w3mopenlocal "w3m-open-local" (nil) dired-dd-w3m-open-local shortnames)
	 ;; Just open cursor (pointer) is on.  No multiple file handling.
	 (info "Read This Info" (nil)  call-interactively 'dired-info)
	 "Man, Info, HTML")
    (separator-mymap "--" (nil))
    (link "Link"
	  (nil)
	  keymap
	  (symlink "Symlink" (nil) dired-do-symlink arg)
	  (hardlink "Hardlink" (nil) dired-do-hardlink arg)
	  (relsymlink "Relative Symlink" (nil) dired-do-relsymlink arg)
	  "Link")
    ;;	(separator-mymap "--" (nil))
    (perm "Permission"
	  (nil)
	  keymap
	  (chmod "Change File Mode" (nil) dired-do-chmod arg)
	  (chgrp "Change File Group" (nil) dired-do-chgrp arg)
	  (chown "Change File Owner" (nil) dired-do-chownrp arg)
	  "Permission")
    (separator-mymap "--" (nil))
    (file "File Open"
	  (nil)
	  keymap
	  (open "Open" (nil) . find-file)
	  (view "View" (nil) dired-view-file)
	  (o-other-win
	   "Open Other Window" (nil) . find-file-other-window)
	  (o-other-frame
	   "Open Other Frame" (nil) . find-file-other-frame)
	  (o-multi-win "Open Multi Window" (nil)
		       dired-do-find-marked-files arg)
	  "File Open")
    (separator-mymap "--" (nil))
;;; (elisp "Elisp"
;;;   (nil)
;;;	 keymap
    (load "Load Into Emacs" (nil) dired-do-load arg)
    (bytecompile "Byte Compile" (nil) dired-do-byte-compile arg)
    (byte-recompile-dir
     "Byte Recompile Dir"
     (nil)  . dired-dd-byte-recompile-directory)
;;;		  "Elisp")
    (separator-mymap "--" (nil))
    (mark "Mark"
	  (nil)
	  keymap
	  (xsel "X-select" (nil) dired-copy-filename-as-kill nil)
	  (xsel-full "X-select Full" (nil)
		     dired-copy-filename-as-kill 0)
	  (toggle-sort "Toggle Sorting" (nil)
		       dired-sort-toggle-or-edit arg)
	  (mark-all
	   "Mark All" (nil)
	   save-excursion (dired-mark-subdir-files) (revert-buffer))
	  (umark-all
	   "Unmark All" (nil) progn (dired-unmark-all-marks)
	   (revert-buffer))
	  "Mark Ops")
    (separator-mymap "--" (nil))
    ;; Promote up from submenu, Sun Jan 11 13:13:05 1998
    ;;	   (system "File System Stat"
    ;;		   (nil)
    ;;		   keymap
    (du "Disk Usage (du)" (nil) dired-dd-shell-du shortnames)
    (df "Disk Free (df)" (nil)  dired-dd-shell-df)
    ;;		   "System")
    ;;	   (separator-mymap "--" (nil))
    ;;	   (cancel "Cancel" (nil) . cancel)
    (separator-mymap "--" (nil))
    (refresh "Refresh Listing" (nil) revert-buffer arg)
    (separator-mymap "--" (nil))
    (quit "Bury Buffer" (nil)  bury-buffer)
    (separator-mymap "--" (nil))
    (version "About dired-dd" (nil) dired-dd-version nil)
    ;;	   (separator-mymap "--" (nil))
    ;;	   (kill "Kill Buffer" (nil)  kill-this-buffer)
;;; This sort of binding is more convenient:
;;;	  ;;(global-set-key [mode-line C-mouse-3] 'kill-this-buffer)
;;;	  ;;(global-set-key [mode-line S-mouse-3] 'bury-buffer)
    "Dired-dd File Menu")
  "A data for menu raised by mouse-3 of dired-dd.
Structured as a emacs keymap, so you can use define-key/define-key-after
to customize the menu as normal keymap.

In dired-dd, the use of keymap is expanded (may be an `abuse').
That is, you can not only define a normal menu keymap entry:

   (define-key-after (lookup-key dired-dd-b3-keymap [file])
     [open2] '(\"Open2\" . find-file) [o-multi-win])

but also even an eval'able form ('(dired-view-file)):

   (define-key-after (lookup-key dired-dd-b3-keymap [file])
     [view2] '(\"View2\" . (dired-view-file)) [o-multi-win])

Note that the second example above contains a non-command object as
`definition' ('(dired-view-file)).

In dired-dd, if the key definition is a symbol ('find-file, in the
first example above), the symbol is called as command with feeding
marked filename(s) as a 1st arg (repeatedly), while if the key
definition is not a symbol ('(dired-view-file), in the second example
above), it is just evaluated once.

Both the binding above can only be handled special function
dired-dd-x-popup-menu-out-of-keymap, which acts like
x-popup-menu when x-popup-menu is fed with non-keymap object as 2nd arg.

Of course, you can undefine the unnecessary menu entry:

   (define-key dired-dd-b3-keymap [arc lha-x] 'undefined)
   (define-key dired-dd-b3-keymap [arc lha-v] 'undefined)

Updated dired-dd-b3-keymap can be inspected with evaluating
   (x-popup-menu               t dired-dd-b3-keymap)
or
   (dired-dd-x-popup-menu-out-of-keymap t dired-dd-b3-keymap)
in *scratch* buffer.

Do not bind this variable directly to a key/mouse event, which is useless.")
;; Keymap Tester:
;;(dired-dd-x-popup-menu-out-of-keymap t dired-dd-b3-keymap)

;;
;; dired-shell-command has been changed in emacs-20.2
;;

(if (>= (string-to-int emacs-version) 20)
    (define-key
      dired-dd-b3-keymap [shellcmd]
      '("Execute Command On" . (dired-do-shell-command
				(dired-read-shell-command
				 (concat "! on " "%s: ")
				 arg
				 (dired-get-marked-files
				  t arg))
				arg
				(dired-get-marked-files
				 t arg)))))
;;(lookup-key dired-dd-b3-keymap [shellcmd])

;;(defun dired-dd-do-shell-command (a1 a2)
;;  (if (>= (string-to-int emacs-version) 20)
;;      ;; emacs-20 requires fn-list.
;;      (dired-do-shell-command a1 a2 
;;			      (dired-get-marked-files t nil))
;;    (dired-do-shell-command a1 a2)))

;;
;; Handler definitions to be registered in dired-dd-b3-keymap.
;; Currently there are two interfaces to invoke shell commands:
;;   dired-dd-shell-command, and dired-dd-shell-command2.
;; dired-dd-shell-command2 is `on-each' method (command is invoked on each file).
;; The argument to the two functions are:
;;   (COMMANDLINE-FORMAT &optional FN-LIST ARG)
;;
;; dired-dd-shell-command, and dired-dd-shell-command2 were rewritten
;; in 0.9.1.15, to support dired-guess-shell-alist-user's grammar
;; to specify filename position in command line format.
;;

;; Code for environment where $LANG is not set
;; by courtesy of Yuji Yamano <yyamano@kt.rim.or.jp>
;; (string-match barfs if bare getenv returns nil => byte-compile fails)

(defconst dired-dd-japanese-environment
  (if (string-match "^ja" (or (getenv "LANG") "NO-LANG-set")) t nil)
  "Automatically set on loading dired-dd-b3-menu. Dired-dd internal use only. 
Setting this variable on running emacs means nothing.")

(defvar dired-dd-terminal-command
  (cond (dired-dd-japanese-environment "kterm")
	(t "xterm"))
  "*Terminal emulator command launched from dired-dd menu.
Set your favorite terminal emulator name.  Option may be included such as:

\(setq dired-dd-terminal-command \"rxvt -fn vga\"\)

Default is `xterm', or probably `kterm' depending upon environment
variable `LANG'.")

;; For one command only for bunch of files (not on-each).
;; Rewritten for 0.9.1.15 Sun Jan  3 13:54:48 1999
;; "*" grammar as in dired-guess-shell-alist-user supported.
;; More dired-compatible way. `on-each' (3rd arg for dired-shell-stuff-it)
;; is nil, so that one command is invoked for the files in fn-list.
;; If no output from subshell, no buffer raised.

(defun dired-dd-shell-command (cmd &optional fn-list sync)
  "Invokes one SHELL-COMMAND over FILENAME-LIST.
SHELL-COMMAND can be formatted as in dired-guess-shell-alist-user
using \"*\" grammar to specify FILENAME-LIST position.
SHELL-COMMAND is invoked just once for the files in FILENAME-LIST."
;;  (shell-command (dired-shell-stuff-it cmd fn-list nil nil)))
  (shell-command (format "%s%s" (dired-shell-stuff-it cmd fn-list nil nil)
			 (if sync " &" ""))))

;; (dired-shell-stuff-it "groff -Tnippon -mandoc *" '("a.1" "b.1") nil)
;; (dired-shell-stuff-it "groff -Tnippon -mandoc *" '("a.1" "b.1") t)

(defun dired-dd-shell-command2 (cmd &optional fn-list sync)
  "Invokes a SHELL-COMMAND for each file in FILENAME-LIST.
SHELL-COMMAND can be formatted as in dired-guess-shell-alist-user
using \"*\" grammar to specify a filename in FILENAME-LIST position.
A SHELL-COMMAND is invoked one by one for each files in FILENAME-LIST."
;;  (shell-command (dired-shell-stuff-it cmd fn-list t nil)))
  (shell-command  (format "%s%s%s"
			  (if sync "( " "")
			  (dired-shell-stuff-it cmd fn-list t nil)
			  (if sync " ) &" ""))))

;; From 0.9.1.15, filename position should be specified by "*" as in
;; dired-guess-shell-alist-user.
;; "&" SHOULD NOT be used in functions calling dired-dd-shell-command2.
(defun dired-dd-do-tar-zvtf (fn-list)
  (dired-dd-shell-command2 "zcat * | tar vt" fn-list))

(defun dired-dd-do-tar-zvxf (fn-list)
  (dired-dd-shell-command2 "zcat * | tar vx" fn-list)
  (revert-buffer))

(defun dired-dd-do-lha-v (fn-list)
  (dired-dd-shell-command2 "lha v *" fn-list))

(defun dired-dd-do-lha-x (fn-list)
  (dired-dd-shell-command2 "lha x *" fn-list))

(defun dired-dd-do-unzip-l (fn-list)
  (dired-dd-shell-command2 "unzip -l *" fn-list))

(defun dired-dd-do-unzip (fn-list)
  (dired-dd-shell-command2 "unzip *" fn-list))

(defvar dired-dd-groff-command
  (cond (dired-dd-japanese-environment "groff -Tnippon -mandoc * | jcol.pl -xbf")
	(t "groff -Tascii -mandoc * | col -xbf"))
  "*Shell command string to invoke groff command.
In japanese environment, you might have to install jcol.pl to remove backspace etc.")

(defvar dired-dd-shell-df-command "df"
  "*Shell command string to invoke disk-usage command. Default is \"df\".
You can use same \"*\" grammer to specify marked file position as in
dired-guess-shell-alist-user.")

(defvar dired-dd-shell-du-command "du"
  "*Shell command string to invoke disk-usage command. Default is \"du\".
You can use same \"*\" grammer to specify marked file position as in
dired-guess-shell-alist-user.

For example, maybe you will find \"du|sort\" is useful \(like tkdesk default\).
If so, 

  \(setq dired-dd-shell-du-command \"du * | sort -nr\")")

(defun dired-dd-do-mandoc (fn-list)
  (dired-dd-shell-command2 dired-dd-groff-command fn-list))

;; Normally user specifies one HTML here but `mapc' for integrity.

(defun dired-dd-w3-open-local (fn-list)
  (mapc (lambda (fn) (w3-open-local fn)) fn-list))

(autoload 'w3m "w3m" nil t)	;; Should be done by user ?

(defun dired-dd-w3m-open-local (fn-list)
  (mapc (lambda (fn)
	  ;; concat("file:") not required. A full-path is okay.
	  (w3m (expand-file-name fn)
	       ;; dired-dd-w3m.el (including `dired-dd-w3m-new-session')
	       ;; is not installed by default, so must do boundp().
	       (if (and (boundp 'dired-dd-w3m-new-session)
			dired-dd-w3m-new-session) arg))
	  ) (nreverse fn-list)))
  
(defun dired-dd-shell-df (&optional fn-list)
  (dired-dd-shell-command dired-dd-shell-df-command fn-list))

(defun dired-dd-shell-du (&optional fn-list)
  (dired-dd-shell-command dired-dd-shell-du-command fn-list))

;;(defvar dired-dd-byte-recompile-directory-query t)
(defun dired-dd-byte-recompile-directory (fname)
  ;; "s" is good enough
  (interactive "s")
  (if (null fname)			; perhaps `No file' line...
      (setq fname (dired-current-directory)))
  (if (file-accessible-directory-p fname)
      (progn
	(byte-recompile-directory fname 
				  ;;dired-dd-byte-recompile-directory-query
				  )
	(revert-buffer))
    (message "%s inaccessible as directory" fname)))

;Keymap version.  Last line enables du/df everywhere.
;(defvar dired-dd-shell-map (make-sparse-keymap "Disk Commands"))
;(fset 'dired-dd-disk-shell-map dired-dd-shell-map)
;(define-key dired-dd-shell-map [df]
;  '("Disk Free Space (df)" . dired-dd-shell-df))
;(define-key dired-dd-shell-map [du]
;  '("Disk Usage (du)" . dired-dd-shell-du))
;(x-popup-menu nil dired-dd-shell-map)
;(define-key dired-mode-map [M-S-mouse-3] 'dired-dd-disk-shell-map)
;(define-key global-map [C-M-S-mouse-3] 'dired-dd-disk-shell-map)

(provide 'dired-dd-b3-menu)
