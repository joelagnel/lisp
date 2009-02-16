;;; after-save-commands.el --- Run a shell command after saving a file

;; Copyright (C) 1997,98 by  Karl M. Hegbloom

;; $Id: after-save-commands.el,v 1.6 1998/08/25 19:56:58 karlheg Exp $
;; Author: Karl M. Hegbloom <karl...@inetarena.com>
;; Keywords: processes,unix

;; This file is part of XEmacs.

;; XEmacs is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; XEmacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Commentary:

;;; Set up a list of file-name matching regular expressions associated
;;; with shell commands to run after saving the file.

;;; This is good for things like running `newaliases(1)' on
;;; "/etc/aliases", or `xrdb(1)' on "~/.Xresources", as well as
;;; sending signals to daemons whos configuration files you've just
;;; finished editting.
;;; 
;;; It is much safer than using exec statements in "Local Variables"
;;; sections, and can safely be used by root for system administration
;;; tasks.  The shell command can run about anything you can think of.
;;;
;;; See: `After-save-alist' for more information.

;;; Code:
;;;-----------------------------------------------------

(require 'cl)

(defcustom After-save-alist
'(("/\\.X\\(resource\\|default\\)s$"	; File Regexp
   (("aft_save_Blahb" . "/path/to/blahb_data") ; environment additions
    ("aft_sav_Foo" . "$Blahb:/another/strange/path/")
    ("aft_sav_Unset_Me"))		; ... and deletions
   t					; confirm-exec-flag
   "echo xrdb -merge $f"		; shell commandline specification
  ("/etc/aliases" nil t "echo newaliases")))
    "*This option is for associating file name patterns to shell commands.

These are shell commands you would like XEmacs to run for you after a
file with a name that matches a regexp has been editted and saved.

You may also specify whether you want to be asked for confirmation
each time you save the file, prior to running that command, on a per
command basis.

While you are visiting a file that has an `after-save-command'
associated with it, the modeline will display \"AScmd\" in the minor
mode list, and moving the mouse over that indicator will cause the
buffer's associated shell command to be displayed in the minibuffer.
Clicking button 2 there will disable the command for this visit of the
file.

This facility can be very handy for doing things like running
`newaliases\(1)' after you've editted the `sendmail\(8)' daemon's
\"/etc/aliases\" file, running `xrdb\(1)' after you've hand tweaked
your X resource settings, or sending a signal to a system daemon whos
configuration file you've just modified.

You may change these settings while you are visiting a file, prior to
saving it, and thus change the command you've attached to it before
you actually save it.

Take care to specify the regular expressions carefully.  If you
encounter a `wrong type argument, stringp nil' when saving a file,
check that you've not changed the regexp for the file you are visiting
to one that no longer matches it's `buffer-file-name'.  The file has
already been saved when the error is signalled, so you have probably
not lost any work.

The command you specify will be run in a subshell, out of the
`after-save-hook', using the lisp function `shell-command'.  You can
cause it to background by suffixing the command with the usual \"&\".
It will inherit the `process-environment' of your XEmacs session,
along with the specified environment additions or undefines, as well
as the following automaticly defined variables:

   $f -- The full path and filename of the file, `buffer-file-name'
   $d -- The directory, with a trailing \"/\" where the file was saved.

The `Var=\"Value pair\"' environment variables will be defined in the
context the shell command will be run in.  You may reference
previously defined environment variables within the `Value' fields,
since they are expanded sequentially, from top to bottom, using
`substitute-in-file-name', just before the command is run.  $f and $d
are set first, and so may be used for expansion within your
environment specifications, as well as in the commandline.  Note that
no shell processing will be done until the commandline is fed to your
shell; that is, you cannot expect brace expansions and things to work
in your environment specification.

If you use `write-file' (`C-x C-w') to write the visited buffer to a
different filename, the `after-save-command' will not be run, and that
property will be removed from the buffer."
  :require 'after-save-commands
  :type '(repeat
	  (list :tag "------------------------------------------------------------"
		:indent 2
		(regexp :tag "File name regexp " "")
		(repeat :tag "Environment"
			:indent 1
			(cons :tag "Var=\"Value\" pairs "
			      (string :tag "Variable " "")
			      (choice (string :tag "Value " "")
				      (const :tag "Undefine" nil))))
		(boolean :tag "Confirm before execution? " t)
		(string :tag "Shell Command line " "")))
  :group 'files)


(defun After-save--entry-lookup (buf-fn)
  "Lookup BUF-FN in `After-save-alist', and return that record."
  (dolist (elt After-save-alist)
    (when (string-match (expand-file-name (car elt)) buf-fn)
      (return elt))))

(defun After-save--after-save-hook ()
  "An `after-save-hook' to run a shell-command.
This gets hung on the buffer-local `after-save-hook', by
`After-save--find-file-hook', in buffers whos `buffer-file-name' match
one of the regular expressions in `After-save-alist'."
  ;; The `copy-sequence' is important!
  ;; We want a copy of `process-environment' to get bound here, so
  ;; the more global one, outside of this `let*' block, doesn't get
  ;; its elements modified.
  (let* ((process-environment	(copy-sequence process-environment))
	 (file->cmd-entry	(After-save--entry-lookup (buffer-file-name)))
	 (environ-alist		(second file->cmd-entry))
	 (confirm-exec-flag	(third  file->cmd-entry))
	 (command		(fourth file->cmd-entry)))
    ;; Here, the bound `process-environment' is modified, not the
    ;; more global one.  The current dynamic value of it will get
    ;; passed to the command's shell.
    (setenv "f" (buffer-file-name))
    (setenv "d" (default-directory))
    (dolist (env-pair environ-alist)
      (setenv (car env-pair) (if (cdr env-pair)
				 ;; does not expand tilde's...
				 ;; there is no `expand-environ-in-string'
				 (substitute-in-file-name (cdr env-pair))
			       nil)))
    (if confirm-exec-flag
	(when (y-or-n-p-maybe-dialog-box
	       (substitute-in-file-name
		(format "Run:%S ? " command)))
	  (shell-command command))
      (shell-command command))))

;; autoload this just in case it gets stuck on the hook before the
;; setting of `After-save-alist' brings this in with its :require.  I
;; think that could happen if the `find-file-hooks' gets saved in
;; `options.el' after this has been installed on it.  That variable
;; might come before the `defcustom'ed variables at the top of this
;; program.  Comments?
;;;###autoload
(defun After-save--find-file-hook ()
  "Look up the name of this file in `After-save-alist', and if it has
an entry, install `After-save--after-save-hook' on a buffer local
`after-save-hook' for it, and set `After-save-commands--enable-locally'."
  (let ((file->cmd-entry (After-save--entry-lookup (buffer-file-name))))
    (when file->cmd-entry
      (make-local-hook 'after-save-hook)
      (add-hook 'after-save-hook 'After-save--after-save-hook nil t)
      ;; I encourage someone to port this to GNU Emacs. :-)
      (let ((ext (make-extent nil nil))
	    (km (make-sparse-keymap)))
	(set-extent-face ext 'modeline-mousable-minor-mode)
	(set-extent-property ext 'help-echo (fourth (After-save--entry-lookup (buffer-file-name))))
	(define-key km [(button2)] (lambda ()
				     (interactive)
				     (remove-hook 'after-save-hook 'After-save--after-save-hook t)
				     (setq modeline-process nil)))
	(set-extent-keymap ext km)
	(setq modeline-process (cons ext " AScmd"))))))

(defadvice write-file (before After-save activate)
  (when (string= (cdr modeline-process) " AScmd")
    (setq modeline-process nil)))

;; -------------------------------------------------------------------------
;; I shouldn't have to do this...  Or should I?  I think maybe I ought
;; to be able to use just a straight out `add-hook' here.  But when
;; `find-file-hooks' has been customized, it's value can get set to an
;; arbitrary list after this `add-hook' is run, thus wiping it out. So
;; I have to install it on the `after-init-hook' like this.  It might
;; be nice if the hook type would be initialized by custom with an
;; add-hook... Then again, maybe sticking an add-hook onto the
;; `after-init-hook' like this is really just the standard way of
;; getting a function onto the list?  Perhaps by makeing
;; `custom-set-variables' set the hook to an absolute value, we make
;; it possible to know for certain what it's startup time value will
;; be...  minus additions by packages like this one.  YTMAWBK
(add-hook 'after-init-hook
	  #'(lambda ()
	      (add-hook 'find-file-hooks 'After-save--find-file-hook)))

;;;-------------------------------------------------------------------------
(provide 'after-save-commands)
