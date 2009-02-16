;;; @(#) hobo.el -- remote editting via SCP
;;; @(#) $Id: hobo.el,v 1.2 2002/01/20 02:30:26 jcasa Exp $

;; This file is not part of Emacs

;; Copyright (C) 2001 by Joseph L. Casadonte Jr.
;; Author:          Joe Casadonte (emacs@northbound-train.com)
;; Maintainer:      Joe Casadonte (emacs@northbound-train.com)
;; Created:         November 27, 2001
;; Keywords:        remote scp tramp comm
;; Latest Version:  http://www.northbound-train.com/emacs.html

;; COPYRIGHT NOTICE

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;;  Hobo facilitates editing remote files by automating file transfers
;;  via SCP, relying on SSH-AGENT (or RSA/DSA) to authenticate.  Hobo
;;  stores files locally in a temporary directory structure and can
;;  automatically clean up the temporary files and directories when
;;  you're done.  In addition, it makes accessing the remote files a
;;  tad easier using aliases/shortcuts with all of the connection info
;;  already set up.  See 'Existential Background' below for
;;  information on alternatives to Hobo.

;;; Installation:
;;
;;  Put this file on your Emacs-Lisp load path, then add one of the
;;  following to your ~/.emacs startup file.  You can load hobo
;;  every time you start Emacs:
;;
;;     (require 'hobo)
;;
;;  or you can load the package on first use via autoload:
;;
;;     (autoload 'hobo-register-agent "hobo" "" t)
;;     (autoload 'hobo-register-agent-from-file "hobo" "" t)
;;     (autoload 'hobo-find-file "hobo" "" t)
;;
;;  To add a directory to your load-path, use something like the following:
;;
;;      (add-to-list 'load-path (expand-file-name "/some/load/path"))

;;; Usage:
;;
;;  Using hobo requires that SCP be installed and configured to run
;;  with SSH-AGENT, so that no password needs to be given when SCP is
;;  run.  Further, SSH-AGENT must already be running when hobo is
;;  asked to retrieve a file.
;;
;;  Alternately, if you use RSA/DSA authentication without a
;;  passphrase (or if you use ASK-PASS), you can tell Hobo that you
;;  don't need the agent, by setting `hobo-use-agent' to nil.  If so,
;;  skip the first two steps below.
;;
;;  1. Start SSH-AGENT.
;;  2. Register SSH-AGENT with hobo (see `hobo-register-agent' and
;;     `hobo-register-agent-from-file')
;;  3. M-x hobo-find-file
;;     - provide connection-info (e.g. alias OR user@remote-host.com)
;;     - provide file info (e.g. /some/remote/path/to/file.txt
;;  4. Edit file locally
;;  5. Save file locally
;;     - when saving, hobo can be configured to:
;;       o automatically transmit the file back to the remote host
;;       o prompt you to transmit the file back to the remote host
;;       o do nothing, in which case you must transmit the file
;;         manually
;;       [see `hobo-after-save-transmit' for more details]
;;  6. M-x hobo-save-buffer
;;     - manually transmits the file back to the remote host (only
;;       needed when `hobo-after-save-transmit' is set to nil)
;;  7. Kill local buffer
;;     - when the buffer is killed, hobo can be configured to:
;;       o automatically transmit the file back to the remote host
;;       o prompt you to transmit the file back to the remote host
;;       o do nothing
;;       [see `hobo-kill-buffer-transmit' for more details]
;;     - when the buffer is killed, hobo can be configured to:
;;       o automatically delete local copy of the file
;;       o above plus delete temp directory structure
;;       o do nothing
;;       [see `hobo-kill-buffer-cleanup' for more details]
;;
;;  For more info on SSH/SCP:
;;
;;     http://www.openssh.org

;;; Customization:
;;
;;  Hobo can be used as-is without any customizations.  Common things
;;  that you can customize, should you need them, are:
;;
;;  o SCP path/executable name [`hobo-scp'] - if you need to run
;;    something other than 'scp', or if 'scp' is not in your path
;;  o Aliases [`hobo-alias-alist'] - stores default connection info
;;    for later retrieval;;
;;  o Temporary directory name [`hobo-temp-dir'] - determines where
;;    files are put locally when editing
;;  o Various after-save and kill-buffer hook customizations
;;    [`hobo-after-save-transmit', `hobo-kill-buffer-transmit',
;;    `hobo-kill-buffer-cleanup']
;;
;;  To see all of the customization options, do one of the following:
;;
;;     M-x customize-group RET hobo RET
;;
;;  or
;;
;;     M-x hobo-customize RET
;;
;;  Both of them do the same thing.

;;; Existential Background:
;;
;;  Hobo's raison d'être can best be defined in terms of what it's
;;  not, and that can best be accomplished by looking at TRAMP
;;  (http://tramp.sf.net).  Hobo is the antithesis of what TRAMP
;;  stands for (literally): "Transparent Remote Access, Multiple
;;  Protocol".  From the TRAMP homepage, TRAMP seeks to provide a way
;;
;;      to access files on remote machines as though they were
;;      local. Access to the remote file system for editing files,
;;      version control, and dired are transparently enabled.  Your
;;      access to the remote machine can be with the rsh, rlogin,
;;      telnet programs or with any similar connection method.
;;
;;  Hobo does not provide a transparent interface, nor does it support
;;  multiple protocols.  It has no support for version control (and
;;  none planned) and no support for dired (though that may come
;;  someday).  It has no facility for file completion, does not
;;  support any protocols other then 'scp', cannot tell when the file
;;  you're visiting has changed on the remote system and cannot be
;;  used to edit local files as a different user via 'su'.  If you
;;  must have dired, must know when a file changes on the remote
;;  system and update the local buffer automatically, need to use rcp
;;  and so on, then Hobo is not for you.
;;
;;  So why use Hobo at all?  A couple of reasons.  With the power and
;;  flexibility of TRAMP comes a certain amount of complexity and
;;  overhead.  For some uses/users, TRAMP setup and configuration is
;;  very straight-forward, but for others it can be quite complex.  In
;;  my case the complexity was caused and/or compounded by the
;;  protocol I chose (ssh/scp) and the limitations of the available
;;  tools for the Win32 platform (the Cygwin version of SSH has ptty
;;  problems).
;;
;;  When I tried & failed recently to get TRAMP to work (my fourth or
;;  fifth try), I came to the conclusion that part of the problem, for
;;  me, had to do with stuff that I didn't need.  The only thing I
;;  wanted was a nice easy way to bring a file located on a remote
;;  machine into my local Emacs session, work on it and, when I save
;;  it locally, have it automatically go back to the remote system.
;;  Everything else that TRAMP does would have been nice, but I could
;;  live without it.
;;
;;  So I wrote Hobo (playing on the vagabond definition of tramp; I'm
;;  sure I could have had more fun with some other definitions :).
;;  It's basic and straight-forward, there's very little setup or
;;  configuration and it works.  And that for me was the most
;;  important thing.
;;
;;  There are a couple of things that Hobo has that TRAMP doesn't (at
;;  least that I know of).  One is the concept of aliases, where
;;  instead of typing in "user@some.host.com:/some/default/path", you
;;  can store all of that in an alias called "foo" and then just
;;  provide "foo" as the connection info.  See `hobo-alias-alist' and
;;  `hobo-find-file' for more info.
;;
;;  The second feature that Hobo has that TRAMP does not is the
;;  ability to save the file locally and NOT transmit it back to the
;;  remote site.  Ok, it's not earth-shattering, but it is something.

;;; Statement of intent re: TRAMP
;;
;;  Just to be clear, Hobo will never be anything more than a
;;  utilitarian replacement for TRAMP.  Hobo is the bottem-end
;;  sub-compact to TRAMP's ultra-high-end SUV.  The sub-compact is
;;  cheap, easy to use, and reliable, but lacking in comfort features.
;;  The SUV is powerful, fun, and much more feature-rich, but requires
;;  lots of resources and some TLC.  Both have their place.  Sure, the
;;  sub-compact will get you where you need to be if you're desperate
;;  and can't get the SUV started because it's out of gas or in the
;;  shop.  But honestly, wouldn't you rather be driving around in the
;;  SUV?

;;; Known Bugs:
;;
;;  o no local file security other than what is already provided for
;;    by the underlying filesystem (may never be fixed)
;;  o hobo-connect-get-local-filename does not work if real target
;;    fname is dos-ish (i.e. real target is c:/foo/bar)
;;  o when deleting directories during clean-up, unpredictable things
;;    may occur if the OS is using the directory being deleted (may
;;    never be handled well)
;;  o cannot reliably determine if SCP was successful or not (I
;;    believe this might be an artifact of the port of SCP I'm using,
;;    so this may not be a Hobo bug, and so may never be 'fixed').
;;    `hobo-assume-scp-output-is-error' is an attempt at addressing
;;    this issue.
;;  o might not act gracefully when remote file is read-only

;;; To Do (possibly sometime soon):
;;
;;  o Allow local temp directory to be overridden on a file-by-file
;;    basis
;;  o Maybe have a hobo-register-check-if-agent-is-running function
;;  o If a dirty hobo buffer exists when exiting emacs (i.e. the
;;    buffer has been saved locally but not tranmitted back to the
;;    remote host), no opportunity to transmit is given, since
;;    `kill-buffer' is not called; could be fixed with a fancy
;;    `kill-emacs' hook

;;; To Do (maybe never):
;;
;;  o minor-mode indicator
;;  o some semblance of dired
;;  o hobo & eshell (allows use of aliases)
;;  o interface for agent from within emacs
;;  o have Hobo track remote timestamp to see if it has changed
;;  o support for root@foobar.com:/some/file/path

;;; Comments:
;;
;;  Any comments, suggestions, bug reports or upgrade requests are welcome.
;;  Please send them to Joe Casadonte (emacs@northbound-train.com).
;;
;;  This version of hobo was developed and tested with NTEmacs
;;  21.1.1 under Windows 2000.  Please, let me know if it works
;;  with other OS and versions of Emacs.

;;; Change Log:
;;
;;  see http://www.northbound-train.com/emacs/hobo.log

;;; **************************************************************************
;;; **************************************************************************
;;; **************************************************************************
;;; **************************************************************************
;;; **************************************************************************
;;; Code:
(eval-when-compile
  ;; silence the old byte-compiler
  (defvar byte-compile-dynamic)
  (set (make-local-variable 'byte-compile-dynamic) t)
  (defvar window-system)
  (defvar system-type)
  )

;;; **************************************************************************
;;; ***** customization routines
;;; **************************************************************************
(defgroup hobo nil
  "Remote editing via SCP."
  :group 'message)

;; ---------------------------------------------------------------------------
(defun hobo-customize ()
  "Customization of the group 'hobo'."
  (interactive)
  (customize-group "hobo"))

;; ---------------------------------------------------------------------------
(defcustom hobo-scp '("scp" "" "-P")
  "Used to set the SCP executable name and command line arguments.

You would set this when 'scp' is not the executable you want to run,
when 'scp' is not in your PATH, or when you want to pass some
arguments to it (e.g -c <cipher-name>).

The Port Number command line argument prefix is specified separately,
as it's use is more host-dependent \(it's only used when using a
non-standard remote port number).  Specify just the command line
argument to use \(including the '-', '+' or '--'), not the actual port
number.  The actual port number will be stored along with the
connection info in `hobo-alias-alist'.  If you want to *always* use
the same remote, non-standard port number, you should specify it under
SCP Arguments, instead \(though doing it this way is not recommended)."
  :type '(list
		  (string :tag "SCP Executable name")
		  (string :tag "SCP Arguments")
		  (string :tag "SCP PortNum Argument Prefix"))
  :group 'hobo)

;; ---------------------------------------------------------------------------
(defcustom hobo-use-agent t
  "Require that the SSH agent be registered with Hobo before use.

If set to nil, then no check for agent registration is made.

Regardless of the value of this flag, Hobo and SCP/SSH must be
configured to run without interaction (i.e. SCP/SSH must be set up to
do something like RSA/DSA authentication)."
  :type 'boolean
  :group 'hobo)

;; ---------------------------------------------------------------------------
(defcustom hobo-assume-scp-output-is-error nil
  "Assume any output from SCP is an error.

This is useful for when the SCP executable does not return reliable
exit codes.  The assumption here is that the SCP executable produces
no output when it's successful."
  :type 'boolean
  :group 'hobo)

;; ---------------------------------------------------------------------------
(defcustom hobo-alias-alist ()
  "Alist of pre-defined connection data for use in `hobo-find-file'.

ALIAS is the name under which all of the other data is grouped.
USER NAME is the remote user name.
HOSTNAME is the name of the remote host.
PORT is the remote port to use (default is 22 if left blank).
DEFAULT PATH is the path that the file will be searched for, assuming
none is given.

For example, if you often edit remote web files, you might have the
following entry:

  ALIAS: web
  -USER NAME: bob
  -HOSTNAME: scp.bobs-host.com
  -PORT:
  -DEFAULT PATH: /usr/local/http/web_root

When `hobo-find-file' is called and you are prompted for the
connection info, you can type:

  Connection Info: web
  File name: index.html

instead of:

  Connection Info: bob@scp.bobs-host.com
  File name: /usr/local/http/web_root/index.html

The DEFAULT PATH can be ignored/overridden by supplying an absolute
path name to `hobo-find-file':

  Connection Info: web
  File name: /temp/index.html"
  :type '(repeat
		  (cons :tag "Connection String"
				(string :tag "Alias")
				(list :tag "Parameters (please fill in one or more of the following:"
				 (string :tag "-User Name")
				 (string :tag "-Hostname")
				 (string :tag "-Port")
				 (string :tag "-Default Path")
				 )))
  :group 'hobo)

;; ---------------------------------------------------------------------------
(defcustom hobo-default-alias ""
  "The default alias to use if no connection-info is given.

Used when nothing is provided to `hobo-find-file' as connection info."
  :type '(string :tag "Default alias")
  :group 'hobo)

;; ---------------------------------------------------------------------------
(defcustom hobo-temp-dir "~/.hobo"
  "Local directory where remote files are stored while being worked on.

See `hobo-kill-buffer-cleanup' for ways to keep this temporary
directory from getting too cluttered.

NOTE: the data in the files is only as secure as the underlying
filesystem, so choose the temp location wisely, and protect it well!"
  :type '(string :tag "Hobo temp directory")
  :group 'hobo)

;; ---------------------------------------------------------------------------
(defcustom hobo-mangle-ala-cygwin-under-win32 t
  "Boolean indicating whether the SCP needs Cygwin mount points.

Under Windows, the Cygwin port of SCP needs to talk in terms of mount
points, not drive letters (e.g. c:/temp is really /cygdrive/c/temp).
When running under Windows, this flag indicates whether or not to
provide that mapping/mangling.  It has no effect on non-Windows
platforms."
  :type 'boolean
  :group 'hobo)

;; ---------------------------------------------------------------------------
(defcustom hobo-debug-buffer-name "*hobo debug*"
  "Name of debug buffer."
  :type 'string
  :group 'hobo)

;; ---------------------------------------------------------------------------
(defcustom hobo-after-save-transmit 'prompt
  "Default transmit action to perform after `save-file' is called.

Available options are:
  o Prompt for transmit - 'hobo' will prompt user to see if the local
    file should be transmitted to the remote system
  o Auto-transmit - local file will automatically be transmitted to
    the remote system
  o Do nothing - nothing happens

This setting can be ovverriden on a file-by-file basis via the
`hobo-set-local-after-save-transmit' command (which see)."
  :type '(choice
		  (const :tag "Prompt for transmit" prompt)
		  (const :tag "Auto-transmit" auto)
		  (const :tag "Do nothing" nil))
  :group 'hobo)

;; ---------------------------------------------------------------------------
(defcustom hobo-kill-buffer-transmit 'prompt
  "Default transmit action to perform after `kill-buffer' is called.

Available options are:
  o Prompt for transmit - 'hobo' will prompt user to see if the local
    file should be transmitted to the remote system
  o Auto-transmit - local file will automatically be transmitted to
    the remote system
  o Do nothing - nothing happens

This setting can be ovverriden on a file-by-file basis via the
`hobo-set-local-kill-buffer-transmit' command (which see)."
  :type '(choice
		  (const :tag "Prompt for transmit" 'prompt)
		  (const :tag "Auto-transmit" 'auto)
		  (const :tag "Do nothing" nil))
  :group 'hobo)

;; ---------------------------------------------------------------------------
(defcustom hobo-kill-buffer-cleanup 'all
  "Default cleanup action to perform after `kill-buffer' is called.

Available options are:
  o Delete local file - deletes just the local copy of the file,
    leaving any directory structure in-tact
  o Delete local file & remove directire tree - deletes the local copy
    of the file and as much of the directory tree as it can
  o Do nothing - nothing happens

This setting can be ovverriden on a file-by-file basis via the
`hobo-set-local-kill-buffer-cleanup' command (which see)."
  :type '(choice
		  (const :tag "Delete local file" 'file)
		  (const :tag "Delete local file & remove directory tree" 'all)
		  (const :tag "Do nothing" nil))
  :group 'hobo)

;; ---------------------------------------------------------------------------
(defcustom hobo-after-save-hook nil
  "Hook run when a 'hobo' file is saved."
  :type 'hook
  :group 'hobo)

;; ---------------------------------------------------------------------------
(defcustom hobo-kill-buffer-hook nil
  "Hook run when a 'hobo' file is killed."
  :type 'hook
  :group 'hobo)

;; ---------------------------------------------------------------------------
(defcustom hobo-load-hook nil
  "Hook run when 'hobo' is first loaded."
  :type 'hook
  :group 'hobo)

;;; **************************************************************************
;;; ***** version related routines
;;; **************************************************************************
(defconst hobo-version
  "$Revision: 1.2 $"
  "Version number for 'hobo' package.")

;; ---------------------------------------------------------------------------
(defun hobo-version-number ()
  "Return 'hobo' version number."
  (let ((version))
	(save-match-data
	  (string-match "[0123456789.]+" hobo-version)
	  (setq version (match-string 0 hobo-version)))
	version))

;; ---------------------------------------------------------------------------
(defun hobo-version ()
  "Display 'hobo' version."
  (interactive)
  (message "hobo version <%s>." (hobo-version-number)))

;;; **************************************************************************
;;; ***** internal variables
;;; **************************************************************************
(defconst hobo-is-win32
  (if (or (eq window-system 'w32)
		  (eq system-type 'windows-nt)) t nil)
  "Boolean indicating whether 'hobo' is running under Win32.")

;;; **************************************************************************
(defvar hobo-agent-is-registered nil
  "[Internal] Boolean indicating whether or not the agent is registered.

Should not be set directly.  Registration is handled via
`hobo-register-agent' and `hobo-register-agent-from-file'.")

;;; **************************************************************************
(defvar hobo-connection-info-history nil
  "[Internal] Minibuffer history variable for connection info.")

;;; **************************************************************************
(defvar hobo-file-info-history nil
  "[Internal] Minibuffer history variable for file info.")

;;; **************************************************************************
;;; ***** buffer local variables
;;; **************************************************************************
(defvar hobo-connect-params nil
  "[Internal] Used to store connection parameters.

This variable is always buffer local.")

(make-variable-buffer-local 'hobo-connect-params)

;;; **************************************************************************
(defvar hobo-buffer-is-dirty nil
  "[Internal] Indicates a need to transmit changes back to the remote host.

This variable is always buffer local.")

(make-variable-buffer-local 'hobo-buffer-is-dirty)

;;; **************************************************************************
(defvar hobo-inhibit-after-save-actions nil
  "[Internal] Used to prevent needless recursion.

This variable is always buffer local.")

(make-variable-buffer-local 'hobo-inhibit-after-save-actions)

;;; **************************************************************************
;;; ***** interactive functions
;;; **************************************************************************
;;;###autoload
(defun hobo-register-agent (pid socket)
  "Registers the SSH-AGENT process (for use with SCP authentication).

PID is the process ID of the SSH-AGENT process.
SOCKET is the communications socket of the SSH-AGENT process.

This function adds two variables to the Emacs process environment:
SSH_AGENT_PID and SSH_AUTH_SOCK.

See also `hobo-register-agent-from-file' and `hobo-use-agent'."
  (interactive "nPID: \nsSocket: ")

  (setenv "SSH_AGENT_PID" pid)
  (setenv "SSH_AUTH_SOCK" socket)

  (setq hobo-agent-is-registered t))

;;; **************************************************************************
;;;###autoload
(defun hobo-register-agent-from-file (filename)
  "Registers the SSH-AGENT process (for use with SCP authentication).

FILENAME is the file to be loaded (via `load').  This file should have
two calls to `setenv', setting the SSH_AGENT_PID and SSH_AUTH_SOCK
environment variables.  Example:

  \(setenv \"SSH_AUTH_SOCK\" \"/tmp/ssh-GqBl1324/agent.1324\")
  \(setenv \"SSH_AGENT_PID\" \"1464\")

You could call this from the load hook like so:

   \(defun my-hobo-init ()
     \(hobo-register-agent-from-file \"~/.sshagent\")
     \(hobo-set-prefix-keymap (read-kbd-macro \"<f3> h\")))

   \(add-hook 'hobo-load-hook 'my-hobo-init)

See also `hobo-register-agent' and `hobo-use-agent'."
  (interactive "fFilename: ")
  (load filename nil nil t)
  (setq hobo-agent-is-registered t))

;;; **************************************************************************
;;;###autoload
(defun hobo-register-set-registered ()
  "Indicates that the AGENT is already registered.

This can be used when the AGENT is already registered in the Emacs
environment or if the AGENT is not needed at all \(e.g. when
ssh-askpass is being used)."
  (interactive)
  (setq hobo-agent-is-registered t))

;;; **************************************************************************
(defun hobo-get-args-from-minibuffer ()
  "[Internal] Used by `hobo-find-file' in call to `interactive'."
  (hobo-check-init-status)
  (let ((ci (read-string "Connection info: " nil 'hobo-connection-info-history))
		(fi (read-string "File name/info: " nil 'hobo-file-info-history)))
	(list ci fi)))

;;; --------------------------------------------------------------------------
;;;###autoload
(defun hobo-find-file (connection-info file-info)
  "Copy remote file to local directory and visit the local file.

CONNECTION-INFO is either an alias (defined in `hobo-alias-alist') or
a string of the form:

     username@remote-host-name[:portnum]

where ':portnum' is optional.

FILE-INFO is the name of the file to edit.  If the file name given is
a relative file name, it is relative to the home directory of the user
specified in the CONNECTION-INFO.  If an alias is used for
the CONNECTION-INFO, then the file name may be relative to some
default path given in the CONNECTION-INFO definition."
  (interactive (hobo-get-args-from-minibuffer))

  (let ((connect-params (hobo-init-connect-params connection-info file-info)))
	(if (= (hobo-get-file connect-params) 0)
		(progn
		  (save-excursion
			(set-buffer hobo-debug-buffer-name)
			(goto-char (point-max))
			(insert "Success!\n"))

		  (find-file (hobo-connect-get-local-filename connect-params nil))
		  (rename-buffer (hobo-connect-get-buffername connect-params))

		  (setq hobo-connect-params connect-params)
		  (setq hobo-buffer-is-dirty nil)
		  )
	  (switch-to-buffer-other-window hobo-debug-buffer-name)
	  (error "Error getting file -- see error buffer"))
	))

;;; **************************************************************************
;;;###autoload
(defun hobo-save-buffer ()
  "Save current buffer locally and copy back to remote machine.

If the current buffer has been modified, it is saved first.  The local
file is then transmitted back to the remote host using the connection
info that was provided when the file was first visited.

If the current buffer is not a hobo buffer, an error is generated."
  (interactive)

  (unless (hobo-is-hobo-buffer)
	(error "Not a valid HOBO buffer"))

  ;; save it first if needed
  (when (buffer-modified-p)
	;; prevent recursion
	(let ((hobo-inhibit-after-save-actions t))
	  (save-buffer)))

  ;; send it and report
  (message "Transmitting file...")

  (if (= (hobo-put-file hobo-connect-params) 0)
	  (progn
		(save-excursion
		  (set-buffer hobo-debug-buffer-name)
		  (goto-char (point-max))
		  (insert "Success!\n"))
		(setq hobo-buffer-is-dirty nil)
		(message "File successfully transmitted"))
	(switch-to-buffer-other-window hobo-debug-buffer-name)
	(error "Error putting file -- see error buffer"))
  )

;;; **************************************************************************
;;; ***** after-save & kill-buffer hook functions
;;; **************************************************************************
(defun hobo-hook-set-dirty-flag ()
  "[Internal] Set `hobo-buffer-is-dirty' to t.

Called by `hobo-after-save-check'."
  (when (hobo-is-hobo-buffer)
	(setq hobo-buffer-is-dirty t)))

;;; **************************************************************************
(defun hobo-hook-prompt-for-transmit ()
  "[Internal] Prompt user to transmit the file.

Called by `hobo-after-save-check' and `hobo-kill-buffer-check'."
  (when (and hobo-buffer-is-dirty
			 (y-or-n-p "[HOBO] Transmit file back to host? "))
	(hobo-save-buffer)))

;;; **************************************************************************
(defun hobo-hook-auto-transmit ()
  "[Internal] Automatically transmits the file.

Called by `hobo-after-save-check' and `hobo-kill-buffer-check'."
  (when hobo-buffer-is-dirty
	(hobo-save-buffer)))

;;; **************************************************************************
(defun hobo-join (join-string elements)
  "[Internal] Function to mimic Perl's join() function.

JOIN-STRING is the string to use to join them together.
ELEMENTS is a list of what to join.

Example:

  \(hobo-join \"/\" (list \"some\" \"file\" \"path\")

would result in the string:

  some/file/path

Note that no leading slash is put in; JOIN-STRING is only put in
between the joined elements."
  (let (rc active subsequent-pass)
	(while elements
	  (setq active (car elements))
	  (setq elements (cdr elements))

 	  (if (not subsequent-pass)
 		  (setq subsequent-pass t)
		(setq rc (concat rc join-string))
  )

	  (setq rc (concat rc active)))
	rc))

;;; --------------------------------------------------------------------------
(defun hobo-hook-cleanup (dirs)
  "[Internal] Clean up temporary files after hobo buffer is killed.

DIRS is a boolean indicating whether or not the temporary directory
structure should be cleaned up as well.

Called by `hobo-kill-buffer-check'."
  ;; delete file always
  (delete-file (hobo-connect-get-local-filename hobo-connect-params nil))

  ;; remove directory structure, maybe
  (when dirs
	(let ((elements (nreverse
					 (split-string
					  (file-name-directory
					   (hobo-connect-get-local-filename hobo-connect-params nil)) "/")))
		  (full-temp-dir (expand-file-name hobo-temp-dir))
		  current-dir files done)
	  (while (and elements (not done))
		(setq current-dir (hobo-join "/" (reverse elements)))

		(save-match-data
		  (when (not (string-match "^[a-zA-Z]:" current-dir))
			(setq current-dir (concat "/" current-dir))))

		(setq files (directory-files current-dir nil nil t))

		(if (or (not (= (length files) 2))
				(string= full-temp-dir current-dir))
			(setq done t)
		  (delete-directory current-dir)
		  (setq elements (cdr elements))))
	  )))

;;; **************************************************************************
(defun hobo-after-save-check ()
  "Added to `after-save-hook' to run after-save activities on hobo buffers.

Has no effect if the buffer is not a hobo buffer.

Should not be run directly, nor should it be modified.  See
`hobo-after-save-transmit' and `hobo-after-save-hook' for more info on
how to affect after-save activities."
  (when (hobo-is-hobo-buffer)
	(when (not hobo-inhibit-after-save-actions)
	  ;; mark as needing to be sent
	  (hobo-hook-set-dirty-flag)

	  ;; send-check: prompt or auto
	  (cond ((eq hobo-after-save-transmit 'prompt)
			 (hobo-hook-prompt-for-transmit))
			((eq hobo-after-save-transmit 'auto)
			 (hobo-hook-auto-transmit)))
	  )

	;; run other hooks
	(run-hooks 'hobo-after-save-hook)))

;;; --------------------------------------------------------------------------
(add-hook 'after-save-hook 'hobo-after-save-check)

;;; **************************************************************************
(defun hobo-kill-buffer-check ()
  "Added to `kill-buffer-hook' to run `kill-buffer' activities on hobo buffers.

Has no effect if the buffer is not a hobo buffer.

Should not be run directly, nor should it be modified.  See
`hobo-kill-buffer-transmit', `hobo-kill-buffer-cleanup' and
`hobo-after-save-hook' for more info on how to affect `kill-buffer'
activities."
  (when (hobo-is-hobo-buffer)
	;; send-check: prompt or auto
	(when hobo-buffer-is-dirty
	  (cond ((eq hobo-kill-buffer-transmit 'prompt)
			 (hobo-hook-prompt-for-transmit))
			((eq hobo-kill-buffer-transmit 'auto)
			 (hobo-hook-auto-transmit))))

	;; clean-up check: file, all or none
	(cond ((eq hobo-kill-buffer-cleanup 'file)
		   (hobo-hook-cleanup nil))
		  ((eq hobo-kill-buffer-cleanup 'all)
		   (hobo-hook-cleanup t)))

	;; run other hooks
	(run-hooks 'hobo-kill-buffer-hook)))

;;; --------------------------------------------------------------------------
(add-hook 'kill-buffer-hook 'hobo-kill-buffer-check)

;;; **************************************************************************
(defun hobo-set-local-after-save-transmit (new-val)
  "Function to set local value of `hobo-after-save-transmit'.

Valid values for NEW-VAL are: prompt, auto or nil.

Gives `hobo-after-save-transmit' a buffer-local copy (thereby
overriding the global value of the variable).  Best used when called
non-interactively; for example:

   (define-key some-hobo-keymap [(p)]
     (function (lambda ()
			     (interactive)
			     (hobo-set-local-after-save-transmit 'prompt))))"
  (interactive "S")
  (unless (or (eq new-val 'prompt)
			  (eq new-val 'auto)
			  (not new-val))
	(error "Unknown value for hobo-after-save-transmit"))
  (set (make-local-variable 'hobo-after-save-transmit) new-val))

;;; **************************************************************************
(defun hobo-set-local-kill-buffer-transmit (new-val)
  "Function to set local value of `hobo-kill-buffer-transmit'.

Valid values for NEW-VAL are: prompt, auto or nil.

Gives `hobo-kill-buffer-transmit' a buffer-local copy (thereby
overriding the global value of the variable).  Best used when called
non-interactively; for example:

   (define-key some-hobo-keymap [(p)]
     (function (lambda ()
			     (interactive)
			     (hobo-set-local-kill-buffer-transmit 'prompt))))"
  (unless (or (eq new-val 'prompt)
			  (eq new-val 'auto)
			  (not new-val))
	(error "Unknown value for hobo-kill-buffer-transmit"))
  (set (make-local-variable 'hobo-kill-buffer-transmit) new-val))

;;; **************************************************************************
(defun hobo-set-local-kill-buffer-cleanup (new-val)
  "Function to set local value of `hobo-kill-buffer-cleanup'.

Valid values for NEW-VAL are: file, all or nil.

Gives `hobo-kill-buffer-cleanup' a buffer-local copy (thereby
overriding the global value of the variable).  Best used when called
non-interactively; for example:

   (define-key some-hobo-keymap [(a)]
     (function (lambda ()
			     (interactive)
			     (hobo-set-local-kill-buffer-cleanup 'all))))"
  (unless (or (eq new-val 'file)
			  (eq new-val 'all)
			  (not new-val))
	(error "Unknown value for hobo-kill-buffer-cleanup"))
  (set (make-local-variable 'hobo-kill-buffer-cleanup) new-val))

;;; **************************************************************************
;;; ***** internal functions
;;; **************************************************************************
(defun hobo-is-hobo-buffer ()
  "[Internal] Used to determine if current buffer is a hobo buffer."
(and (boundp 'hobo-connect-params)
	   hobo-connect-params))

;;; **************************************************************************
(defun hobo-check-init-status ()
  "[Internal] Used to determine whether SSH-AGENT info has been registered."
  (unless (or (not hobo-use-agent) hobo-agent-is-registered)
	(error "Hobo not initialized -- see `hobo-register-agent' or `hobo-register-agent-from-file'")))

;;; **************************************************************************
;;; ***** connect-param functions
;;; **************************************************************************
(defun hobo-connect-get-username (connect-params)
  "[Internal] Extracts the user name from CONNECT-PARAMS."
  (nth 0 connect-params))

;;; **************************************************************************
(defun hobo-connect-get-hostname (connect-params)
  "[Internal] Extracts the host name from CONNECT-PARAMS."
  (nth 1 connect-params))

;;; **************************************************************************
(defun hobo-connect-get-portnum (connect-params)
  "[Internal] Extracts the remote port number from CONNECT-PARAMS."
  (nth 2 connect-params))

;;; **************************************************************************
(defun hobo-connect-get-fullpath (connect-params)
  "[Internal] Extracts the remote file path from CONNECT-PARAMS."
  (nth 3 connect-params))

;;; **************************************************************************
(defun hobo-connect-get-remote-filename (connect-params)
  "[Internal] Extracts the remote file name from CONNECT-PARAMS."
  (nth 4 connect-params))

;;; **************************************************************************
(defun hobo-connect-get-connection-info (connect-params &optional strip-colons)
  "[Internal] Extracts the connection info from CONNECT-PARAMS.

Optional STRIP-COLONS will substitue an underscore \('_') for any
colon found in the connection info."
  (let ((rc (nth 5 connect-params)))
	(when strip-colons
	  (save-match-data
		(while (string-match ":" rc)
		  (setq rc (replace-match "-" nil nil rc)))
		))
	rc))

;;; **************************************************************************
(defun hobo-connect-get-local-filename (connect-params check-mangle)
  "[Internal] Constructs the local file name from CONNECT-PARAMS.

CHECK-MANGLE is a boolean indicating whether or not to check for
Cygwin mangling.  This should be t when constructing the local file
name in order to pass it to SCP, and nil otherwise.

Regardless, it has no affect on non-Win32 platforms.  See
`hobo-mangle-ala-cygwin-under-win32' for more info."
  (save-match-data
	(let* ((connection-info (hobo-connect-get-connection-info connect-params t))
		   (fullpath (hobo-connect-get-fullpath connect-params))
		   (filename (hobo-connect-get-remote-filename connect-params))
		   (rc (concat hobo-temp-dir
					   "/" connection-info
					   (if (and fullpath (not (string-match "^/" fullpath))) "/")
					   fullpath)))
	  ;; make sure directory exists
	  (if (and rc (not (file-directory-p rc)))
		(make-directory rc t))

	  ;; tack on filename
	  (setq rc
			(expand-file-name
			 (concat rc
					 (if (or (not fullpath) (not (string-match "^/" fullpath))) "/")
					 filename)))

	  ;; do something with ports here?

	  ;; mangle (maybe)
	  (when (and hobo-mangle-ala-cygwin-under-win32 hobo-is-win32 check-mangle
				 (string-match "^\\([a-zA-Z]\\):" rc))
		(let* ((drive-letter (downcase (match-string-no-properties 1 rc)))
			   (cygdrive (concat "/cygdrive/" drive-letter)))
		  (setq rc (replace-match cygdrive t t rc))))

	  ;; return new temp file name
	  rc)))

;;; **************************************************************************
(defun hobo-connect-get-buffername (connect-params)
  "[Internal] Constructs a buffer name from CONNECT-PARAMS."
  (concat (hobo-connect-get-connection-info connect-params)
		  "::"
		  (hobo-connect-get-remote-filename connect-params)))

;;; **************************************************************************
(defun hobo-init-connect-params (connection-info file-info)
  "[Internal] Parse arguments into connect-params.

CONNECTION-INFO is either an alias defined in `hobo-alias-alist' or a
string in the form:

    \"remote-login-name@remote.host.com[:port-number]\"

where ':port-number' is optional.

FILE-INFO is the name of the remote file to be visited.  It can be an
absolute filename or a relative filename.  If relative &
CONNECTION-INFO is an alias, the value of the alias' Default Path (as
defined in `hobo-alias-alist') is pre-pended to the filename before it
is parsed.

Return value is a list comprised of the following elements:

   \(username hostname portnum fullpath filename raw-connection-info)"
  (let (alias username hostname portnum def-path fullpath filename)
	;; use default if none given
	(unless (and connection-info (> (length connection-info) 0))
	  (setq connection-info hobo-default-alias))

	;; look it up in alist
	(setq alias (cdr-safe (assoc-ignore-case connection-info hobo-alias-alist)))

	;; if not there, assume it itself has all needed info
	(unless alias
	  (save-match-data
		(unless (string-match "@" connection-info)
		  (setq connection-info (concat "@" connection-info))))
	  (setq alias (split-string connection-info "[@:]")))

	;; don't go on unless we have *something*
	(unless alias
	  (error "No connection data found"))

	;; get some pieces
	(setq username (nth 0 alias))
	(setq hostname (nth 1 alias))
	(setq portnum (nth 2 alias))
	(setq def-path (nth 3 alias))

	;; massage def-path a little
	(save-match-data
	  ;; kill it entirely if not needed
	  (when (and def-path (or (string-match "^/" file-info)
							  (string-match "^[a-zA-Z]:" file-info)
							  (= (length def-path) 0)))
		(setq def-path nil))
	  ;; make sure it ends in a slash if it exists at all
	  (when (and def-path (not (string-match "/$" def-path)))
		(setq def-path (concat def-path "/")))
	  )

	;; combine (possibly massaged) def-path with file-info
	;; to get the fullpath and filename
	(setq fullpath (concat def-path file-info))
	(setq filename (file-name-nondirectory fullpath))
	(setq fullpath (file-name-directory fullpath))

	;; return the list
	(list username hostname portnum fullpath filename connection-info)
	))

;;; **************************************************************************
;;; ***** transfer functions
;;; **************************************************************************
(defun hobo-get-file (connect-params)
  "[Internal] Low-level call to retrieve remote file.

Uses CONNECT-PARAMS to determine local and remote names.  See
`hobo-call-scp' for more info."
  (hobo-call-scp connect-params t))

;;; **************************************************************************
(defun hobo-put-file (connect-params)
  "[Internal] Low-level call to transmit local file to remote host.

Uses CONNECT-PARAMS to determine local and remote names.  See
`hobo-call-scp' for more info."
  (hobo-call-scp connect-params nil))

;;; **************************************************************************
(defun hobo-call-scp (connect-params get)
  "[Internal] Very low-level call, just above call to SCP.

CONNECT-PARAMS is used to get the file names on either end, and remote
system name.

GET is a boolean indicating direction (t == get, nil == put)."
  ;; have we init-ed yet?
  (hobo-check-init-status)

  ;; make sure temp dir exists
  (unless (file-directory-p hobo-temp-dir)
	(make-directory hobo-temp-dir t))

  (let* ((scp-prog-name (nth 0 hobo-scp))
		 (scp-prog-args (nth 1 hobo-scp))
		 (scp-prog-port-cla (nth 2 hobo-scp))

		 ;; connection pieces
		 (username (hobo-connect-get-username connect-params))
		 (hostname (hobo-connect-get-hostname connect-params))
		 (portnum (hobo-connect-get-portnum connect-params))
		 (fullpath (hobo-connect-get-fullpath connect-params))
		 (filename (hobo-connect-get-remote-filename connect-params))
		 (temp-fname (hobo-connect-get-local-filename connect-params t))

		 ;; build connect string
		 (connect-string (concat
						  (when username (concat username "@"))
						  hostname ":" fullpath filename))
;;						  (when fullpath (concat fullpath "/"))

		 ;; determine first and second args to scp
		 (arg1 (if get connect-string temp-fname))
		 (arg2 (if get temp-fname connect-string))
		 current-hobo-debug-max rc)

	;; put separator into debug buffer
	(save-excursion
	  (set-buffer (get-buffer-create hobo-debug-buffer-name))
	  (goto-char (point-max))
	  (insert "\n\n---------------\n")
	  (setq current-hobo-debug-max (point-max)))

	;; allow for -p <portnum>
	;; add a hobo-scp-portnum-prefix (-p)

	;; call SCP (finally)
	(setq rc (if (> (length portnum) 0)
				 (call-process scp-prog-name nil (list hobo-debug-buffer-name t) nil
							   scp-prog-port-cla portnum arg1 arg2)
			   (call-process scp-prog-name nil (list hobo-debug-buffer-name t) nil
							 arg1 arg2)))

	(when (and (= rc 0)
			   hobo-assume-scp-output-is-error)
	  (save-excursion
		(set-buffer (get-buffer-create hobo-debug-buffer-name))
		(setq rc (- (point-max) current-hobo-debug-max))))

	rc))

;;; **************************************************************************
;;; ***** keymap functions
;;; **************************************************************************
;;;###autoload
(defun hobo-set-prefix-keymap (hobo-prefix)
  "Utility function to assign key bindings for various hobo commands.

It's only really useful if you like my bindings.  HOBO-PREFIX is the
prefix to bind all of these keys into.  It should be in `kbd' format.
Example:

   \(hobo-set-prefix-keymap (read-kbd-macro \"<f12> H\"))

This would bind the various functions to <f12>-H-<whatever>.  You
could call this from the load hook like so:

   \(defun my-hobo-init ()
     \(hobo-register-agent-from-file \"~/.sshagent\")
     \(hobo-set-prefix-keymap (read-kbd-macro \"<f3> h\")))

   \(add-hook 'hobo-load-hook 'my-hobo-init)

Bindings are as follows:

  r - `hobo-register-agent'
  R - `hobo-register-agent-from-file'

  f - `hobo-find-file'
  s - `hobo-save-buffer'

  a p - `hobo-set-local-after-save-transmit' with 'prompt as argument
  a a - `hobo-set-local-after-save-transmit' with 'auto as argument
  a n - `hobo-set-local-after-save-transmit' with nil as argument

  k p - `hobo-set-local-kill-buffer-transmit' with 'prompt as argument
  k a - `hobo-set-local-kill-buffer-transmit' with 'auto as argument
  k n - `hobo-set-local-kill-buffer-transmit' with nil as argument

  K f - `hobo-set-local-kill-buffer-cleanup' with 'file as argument
  K a - `hobo-set-local-kill-buffer-cleanup' with 'all as argument
  K n - `hobo-set-local-kill-buffer-cleanup' with nil as argument"
  (interactive "KEnter new HOBO prefix key: ")
  (let ((hobo-keymap (make-sparse-keymap))
		(hobo-a-keymap (make-sparse-keymap))
		(hobo-k-keymap (make-sparse-keymap))
		(hobo-K-keymap (make-sparse-keymap)))
	(global-set-key hobo-prefix hobo-keymap)

	(define-key hobo-keymap [(r)] 'hobo-register-agent)
	(define-key hobo-keymap [(R)] 'hobo-register-agent-from-file)
	(define-key hobo-keymap [(f)] 'hobo-find-file)
	(define-key hobo-keymap [(s)] 'hobo-save-buffer)

	(define-key hobo-keymap [(a)] hobo-a-keymap)
	(define-key hobo-a-keymap [(p)] (function (lambda () (interactive) (hobo-set-local-after-save-transmit 'prompt))))
	(define-key hobo-a-keymap [(a)] (function (lambda () (interactive) (hobo-set-local-after-save-transmit 'auto))))
	(define-key hobo-a-keymap [(n)] (function (lambda () (interactive) (hobo-set-local-after-save-transmit nil))))

	(define-key hobo-keymap [(k)] hobo-k-keymap)
	(define-key hobo-k-keymap [(p)] (function (lambda () (interactive) (hobo-set-local-kill-buffer-transmit 'prompt))))
	(define-key hobo-k-keymap [(a)] (function (lambda () (interactive) (hobo-set-local-kill-buffer-transmit 'auto))))
	(define-key hobo-k-keymap [(n)] (function (lambda () (interactive) (hobo-set-local-kill-buffer-transmit nil))))

	(define-key hobo-keymap [(K)] hobo-K-keymap)
	(define-key hobo-K-keymap [(f)] (function (lambda () (interactive) (hobo-set-local-kill-buffer-cleanup 'file))))
	(define-key hobo-K-keymap [(a)] (function (lambda () (interactive) (hobo-set-local-kill-buffer-cleanup 'all))))
	(define-key hobo-K-keymap [(n)] (function (lambda () (interactive) (hobo-set-local-kill-buffer-cleanup nil))))
	))

;;; **************************************************************************
;;; ***** we're done
;;; **************************************************************************
(provide 'hobo)
(run-hooks 'hobo-load-hook)

;;; hobo.el ends here
;;; **************************************************************************
;;;; *****  EOF  *****  EOF  *****  EOF  *****  EOF  *****  EOF  *************
