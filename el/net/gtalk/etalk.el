;;; etalk --- otalk/ntalk/gtalk compatible talk client.
;;
;; Copyright (C) 1994, 1995, 1996, 1997, 1998, 1999 Free Software Foundation
;;
;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Version: 0.99
;; Keywords: talk, comm, games
;;
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
;; along with this program; if not, you can either send email to this
;; program's author (see below) or write to:
;;
;;              The Free Software Foundation, Inc.
;;              675 Mass Ave.
;;              Cambridge, MA 02139, USA.
;;
;; Please send bug reports, etc. to zappo@gnu.org
;;

;;; Commentary:
;;   etalk is an Emacs interface to GNU talk, or Gtalk.  It runs gtalk
;;   as a subprocess, and uses buffers to display your conversation.
;;   GNU talk is an emulation and enhancement to the standard BSD talk
;;   system version 0 and 1.  It provides a talk daemon and client
;;   with additional interfaces.
;;
;;   General supported features are: Multiple connections, multiple
;;   entry points with different connection methods, several
;;   communication features (for extended editing), game playing, with
;;   a large library of games, and emacs19/20 X support.
;;
;;   The major entry point is the command `etalk' which querys for a
;;   user to talk to.  It also supports a new GTALK protocol which
;;   adds the entry point `etalk-reply' which is only useful if your
;;   system has the GTALK daemon running.

;;; $Id: etalk.el,v 1.34 1999/11/29 16:57:42 zappo Exp $
;; History:
;; <joe@opus.ohio-state.edu> 9/10/94
;; Added option allowing local buffer to be on top or bottom.
;;
;; zappo@choochoo 10/1/94 (Just after season premier of DS9 ... Wow.)
;; Re-added section to identify and recycle buffers in etalk-mode-remote.
;; Also added start-one-process to etalk startup command so things
;; are ready to go the instant you hit RET
;;
;; zappo@choochoo 11/19/94
;; Added etalk-reply and updated comments.  Reply is a gtalkdaemon
;; only option which queries for you just announced you, and then
;; connects to them.
;;
;; See ChangeLog for other history elements.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Q&D instructions:  Load this file, type "M-x etalk RET SPC"
;;                    to get a list of users.  Type/complete one,
;;                    and hit RET.  Type to them.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General features.
;; O Connection features
;;   O Seven connection generation entry points:
;;     etalk, etalk-reply, etalk-batch, etalk-mail, etalk-mail-reply.
;;     The sixth is started with etalk-ringer-on-command, which
;;     starts the etalk process to answer talk requests.  While you
;;     work etalk then waits for requets, and asks if you wish to
;;     connect to the people talking to you, and the seventh provides for
;;     unsolicited connections from other users.
;;   O Minibuffer completion for usernames and host names including
;;     parsing routines for finger version of several types,
;;     including VMS
;;   O Local config file containing host names and daemon types to
;;     speed up connecting routines, and provide completion on a
;;     system and personal basis.
;;   O Multiple daemon support for GTALK, NTALK and OTALK (sun talk)
;;   O Multiple talk connection protocols including vanilla talk (no
;;     features) and emacs talk (tons o features.)
;;   O Preferred name transfer for more personable messages.
;;   O For users with completely incompatible daemons (or no daemon
;;     at all), etalk-mail can still let users connect over TCP.
;;
;; O Interface features
;;   O Infinite conversation scrollback.
;;   O Multiple simultaneous connections (all can be started with
;;     different methods.)
;;   O Peer to peer connection propagation allowing user 1 to
;;     generate connections to everyone user 2 is talking to.
;;   O Copy/Yank from/into etalk conversation buffers.
;;   O Buffer logging facility to always save your conversations.
;;   O auto-wrap minor mode compatible (even with vanilla talk)
;;   O Make a file popup in a window on both sides of a connection.
;;   O Insert a file into the conversation (you may wish to stick to
;;     small files. ;)
;;   O Extended editing capabilities to go backwards over lines when
;;     everyone uses emacs.
;;   O Minibuffer message over connection.
;;   O Two person games over auxiliary connection with a wide selection of
;;     games to choose from.  Regular conversation is not interrupted.
;;   O Some game(s) can be used with tyrant-ai to be played against
;;     the computer.
;;   O Etalk window management commands.
;;   O Remote talk version in mode line
;;
;; O Other features
;;   O Bundled finger mode, allows browsing of finger information
;;     which also allows you to do dired style actions to users on a
;;     given line such as mail, more-finger-info, and etalk.
;;   O sformat, a super-format extender I wrote to aid in
;;     writing the multiple *-format functions through lists of
;;     extension characters and their values and types.
;;   O etalk-mini can be loaded stand-alone and used in other
;;     emacs applications to complete on usernames.
;;   O Extended X support in games allows colors to spice things up,
;;     in addition to mouse support which allows users to "click" on
;;     a square to make a move.
;;

;;; Code:
(defvar etalk-version-number '(0 . 99)
  "The current version of etalk in a float notation.")

(defvar etalk-program-name "ETALK"
  "The name of this package.")

(defvar etalk-emacs-version emacs-version
  "Version of Emacs this was loaded/byte compiled under.")

(defvar etalk-version (format "%s %d.%d"
			      etalk-program-name
			      (car etalk-version-number)
			      (cdr etalk-version-number))
  "Current working version of the etalk program.")

(defun etalk-version ()
  "Return a string which verbally describes this version of Emacs."
  (interactive)
  (message "Emacs Talk [%s] parsed under emacs V %s"
	   etalk-version etalk-emacs-version)
)

;; From custom web page for compatibility between versions of custom:
(eval-and-compile
  (condition-case ()
      (require 'custom)
    (error nil))
  (if (and (featurep 'custom) (fboundp 'custom-declare-variable))
      nil ;; We've got what we needed
    ;; We have the old custom-library, hack around it!
    (defmacro defgroup (&rest args)
      nil)
    (defmacro custom-add-option (&rest args)
      nil)
    (defmacro defface (var values doc &rest args)
      (` (make-face ` (, var))))
    (defmacro defcustom (var value doc &rest args)
      (` (defvar (, var) (, value) (, doc))))))

(defvar etalk-edit-characters-mine ""
  "*The edit characters used with a remote talk session.
This variable must be changed if you wish to change key bindings.")

;; Since I have never used abbrev, I havn't the slightest notion
;; of what to do with this now that I have one.
(defvar etalk-mode-abbrev-table nil
  "Abbreviation table.")

(define-abbrev-table 'etalk-mode-abbrev-table ())

(defun etalk-superbind-alpha (keymap fn)
  "In KEYMAP bind all occurrences of alphanumeric keys to FN.
An alphanumeric key is any value between 0 and 128"
  (let ((key "\00"))
    (aset key 0 0)
    (while (< (aref key 0) 128)
      (define-key keymap key fn)
      (aset key 0 (1+ (aref key 0))))))

(defvar etalk-log-mode-map nil
  "Keymap used within the log buffer...")

(if etalk-log-mode-map
    ()
  (setq etalk-log-mode-map (make-keymap))
  (suppress-keymap etalk-log-mode-map)
  (etalk-superbind-alpha etalk-log-mode-map 'etalk-send-command-key)
  (define-key etalk-log-mode-map "\C-c" nil)
  (define-key etalk-log-mode-map "\C-x" nil)
  (define-key etalk-log-mode-map "\C-f" nil)
  (define-key etalk-log-mode-map "\C-n" nil)
  (define-key etalk-log-mode-map "\C-b" nil)
  (define-key etalk-log-mode-map "\C-p" nil)
  (define-key etalk-log-mode-map "\C-v" nil)
  (define-key etalk-log-mode-map "\C-g" nil)
  (define-key etalk-log-mode-map "\C-l" nil)
  (define-key etalk-log-mode-map "\C-e" nil)
  (define-key etalk-log-mode-map "\C-a" nil)
  (define-key etalk-log-mode-map "\C-s" nil)
  (define-key etalk-log-mode-map "\e" nil)
  (define-key etalk-log-mode-map "\C-c\C-c" 'etalk-kill-process)
  (define-key etalk-log-mode-map "\C-r" 'etalk-setup-windows-with-log)
  (define-key etalk-log-mode-map "\r" 'etalk-setup-windows-with-log)
  )

(defvar etalk-mode-c-map nil
  "Keymap used on control-c in talk mode.")

(if etalk-mode-c-map
    ()
  (setq etalk-mode-c-map (make-sparse-keymap))
  (define-key etalk-mode-c-map "c" (lookup-key global-map "\C-c"))
  (define-key etalk-mode-c-map "\C-c" 'etalk-nuke-connection)
  (define-key etalk-mode-c-map "h"    'etalk-hug-remote)
  (define-key etalk-mode-c-map "g"    'etalk-initiate-special-function)
  (define-key etalk-mode-c-map "m"    'etalk-send-minibuffer-message)
  ;(define-key etalk-mode-c-map "o"    'etalk-remote-multilist)
  (define-key etalk-mode-c-map "r"    'etalk-setup-windows-plus)
  (define-key etalk-mode-c-map "\C-r" 'etalk-setup-windows-with-log)
  (define-key etalk-mode-c-map "F"    'etalk-enable-user-filter)
  (define-key etalk-mode-c-map "s"    'etalk-start-shared-app)
  )

(defvar etalk-ctl-x-map nil
  "Keymap used to hold Control-x definitions in etalk.")

(if etalk-ctl-x-map
    ()
  (setq etalk-ctl-x-map (make-sparse-keymap))
  ;; the following is to allow files to be inserted just as yank works.
  (define-key etalk-ctl-x-map "i" 'etalk-insert-file)
  (define-key etalk-ctl-x-map "I" 'etalk-insert-file-buffer)
  ;; protect ourselves from deleting the buffers unkindly...
  (define-key etalk-ctl-x-map "k" 'etalk-nuke-connection-buffer)
  (define-key etalk-ctl-x-map "\C-k" 'etalk-nuke-connection-buffer)
)

(defvar etalk-mode-map nil
  "Keymap used in talk mode.")

(if etalk-mode-map
    ()
  (if (not (fboundp 'etalk-fancy-keymap))
      ;; the simple way of defining a keymap
      (progn
	(setq etalk-mode-map (make-keymap))
	(suppress-keymap etalk-mode-map)
	(etalk-superbind-alpha etalk-mode-map 'etalk-insert-char))
    ;; The following lines are a clever way of leting etalk grab fancy
    ;; keys which don't show up on US keyboards.  As the author of
    ;; this package, I don't really know what it's doing for real, but
    ;; Andre Spiegel <spiegel@bruessel.informatik.uni-stuttgart.de>
    ;; does.  Since neither of us know the most efficient way of doing
    ;; this, suggestions are welcome.  Unfortunatly it also nukes
    ;; use of other keymaps, otherwise I'd make it the default
    ;;
    ;; I suspect this won't work for LUCID/XEMACS
    (setq etalk-mode-map
	  (list 'keymap
		(vconcat
		 (mapcar (lambda (f) (if (eq f 'self-insert-command)
					 'etalk-insert-char
				       'undefined))
			 (append (nth 1 global-map) nil))))))
  (define-key etalk-mode-map "\C-@" 'set-mark-command)
  (define-key etalk-mode-map "\C-b" nil) ;move-back
  (define-key etalk-mode-map "\C-c"  etalk-mode-c-map)
  (define-key etalk-mode-map "\C-f" nil) ;move forward
  (define-key etalk-mode-map "\C-g" 'etalk-beep)
  (define-key etalk-mode-map "\C-l" 'etalk-clear-window)
  (define-key etalk-mode-map "\C-m" 'etalk-RET)
  (define-key etalk-mode-map "\C-n" nil) ;next line
  (define-key etalk-mode-map "\C-p" nil) ;prev line
  (define-key etalk-mode-map "\C-q" 'etalk-quoted-insert)
  (define-key etalk-mode-map "\C-r" 'etalk-setup-windows)
  (define-key etalk-mode-map "\C-x" etalk-ctl-x-map)
  (define-key etalk-mode-map "\C-y" 'etalk-yank-text)
  (define-key etalk-mode-map "\C-z" 'suspend-emacs)
  (define-key etalk-mode-map "\e" nil)

  ;; The edit keys used by talk (delete-char, delete-line, and delete-word)
  (define-key etalk-mode-map (substring etalk-edit-characters-mine 0 1)
    'etalk-delete-backwards)
  (define-key etalk-mode-map (substring etalk-edit-characters-mine 1 2)
    'etalk-delete-line)
  (define-key etalk-mode-map (substring etalk-edit-characters-mine 2)
    'etalk-delete-word-backwards)

  ;; Now define some emacs Meta keys that behave like talk edit keys to
  ;; translate themselves
  (define-key etalk-mode-map "\e\C-?" (substring etalk-edit-characters-mine 2))

  ;; Now we must overide all "enhanced" keys which can do weird things.
  (if (not window-system)
      nil		  ; window systems let mice do weird stuff
    (if (string-match "XEmacs" emacs-version)
	(progn
	  ;; We need to have a special yank to actually send that info over
	  ;; the talk connection line.
	  (define-key etalk-mode-map 'button2 'etalk-mouse-yank)
	  ;; Just set the region, never kill it...
	  (define-key etalk-mode-map 'button3 'mouse-set-region)
	  ;; Ok, ditto for secondary selections...
	  (define-key etalk-mode-map '(meta button2) 'etalk-mouse-yank-secondary)
	  (define-key etalk-mode-map '(meta button3) 'mouse-set-secondary)
	  )
      ;; We need to have a special yank to actually send that info over
      ;; the talk connection line.
      (define-key etalk-mode-map [mouse-2] 'etalk-mouse-yank)
      ;; Just set the region, never kill it...
      (define-key etalk-mode-map [mouse-3] 'mouse-set-region)
      ;; Ok, ditto for secondary selections...
      (define-key etalk-mode-map [M-mouse-2] 'etalk-mouse-yank-secondary)
      (define-key etalk-mode-map [M-mouse-3] 'mouse-set-secondary)
      ))
  )

(defcustom etalk-mode-local-hooks '(lambda () (auto-fill-mode 1))
  "Default hook to set auto fill on a talk buffer through hooks..."
  :group 'etalk
  :type 'sexp)

(defcustom etalk-hangup-redo-windows t
  "*Redo the windows whenever there is a change in process status."
  :group 'etalk
  :type 'boolean)

(defcustom etalk-process-file "gnutalk"
  "*Location of the talk program executable.
A path name, or just file name if on your path."
  :group 'etalk
  :type 'string)

(defvar etalk-process-parameters '("-s")
  "*List of strings to use as arguments to the etalk binary.
Example arguments would be: '(\"-s\" \"-v\").
Refer the the etalk binary for more examples.")

(defvar etalk-log-buffer-name "*ETALK LOG*"
  "*Buffer name of the talk log buffer.
Etalk logs all process transactions here.")

(defvar etalk-log-mode-name "Etalk-Log"
  "Mode name used in mode line when describing an etalk log buffer.")

(defvar etalk-local-buffer-name "%P"
  "*The name given to a local talk buffer.  Follows rules for `etalk-format'.")

(defvar etalk-remote-buffer-name "%u@%:1m%:1t"
  "*The name given to a local talk buffer.  Follows rules for `etalk-format'.")

(defvar etalk-remote-display-preferred-name " {%:2p}"
  "*Non-nil, display the remote's preferred name in the minibuffer.
This string is simply appended to `etalk-remote-buffer-name' and follows the
rules for `etalk-format'.")

(defvar etalk-inhibit-startup-message nil
  "*If non-nil, don't print startup message about etalk.")

(defvar etalk-goofy-message-delay 5
  "*Initial length of time between goofy messages.  0 means turn off.")

(defcustom etalk-buffer-logging-directory ""
  "*Where conversations are logged.
Nil is no logging, a string logs files in that directory where the filename
is the username of the person you are talking to."
  :group 'etalk
  :type 'directory)

(defcustom etalk-myself-on-top t
  "*If non-nil, put local buffer first in the frame."
  :group 'etalk
  :type 'boolean)

(defcustom etalk-clear-buffer-on-call nil
  "*Clear buffers if non-nil for remote."
  :group 'etalk
  :type 'boolean)

(defvar etalk-announce-as (user-login-name)
  "*The name passed to the remote talk daemon as your username.
Changing this means you announce yourself with a different name.
Most system administrators would probably be upset if you did this
to unknowing users.")

(defcustom etalk-preferred-name (user-full-name)
  "*The user name you wish to use as your preferred name."
  :group 'etalk
  :type 'string)

(defvar etalk-C-x4t-to-global-map t
  "*Non-nil to add `etalk-setup-windows' to the keystroke C- x 4 t.")

;; Ok! here is a brave thing.  Modify the global-key-map so that the
;; keystroke C-x 4 t redoes the talk windows. ;)
(if etalk-C-x4t-to-global-map
    (define-key global-map "\C-x4t" 'etalk-setup-windows))

(defvar etalk-remote-mode-string "ETALK-remote"
  "The mode name of a buffer attached to a talk process.")

(defvar etalk-local-mode-string "ETALK"
  "Mode name of the local talk buffer.")

(defvar etalk-message-to-minibuffer t
  "Dictates if messages from the inferior process are displayed in minibuffer.
nil means to insert the message into the talk buffer.")

(defvar etalk-log-all-minibuffer-messages t
  "*Dictate whether message from TCP processes are logged in the log buffer.")

(defvar etalk-remote-process-list nil
  "The list of active talk buffers for local talk.
Input to talk window is sent to all remote windows.")

(require 'etalk-mini)			;minibuffer support, address parse
(require 'etalk-proc)			;process control
(require 'etalk-tcp)			;tcp specific things
(require 'etalk-edit)			;editing control
(require 'etalk-spec)                   ;special things
(require 'etalk-sinit)			;initializing special things (AUXlinks)
(require 'etalk-yorn)			;yes or no queries
(if (equal window-system 'x)
    (require 'etalk-x)		;x specific menuy things
  )

(defgroup etalk nil
  "Talk to other users with the talk protocol."
  :prefix "etalk"
  :group 'applications)

(autoload 'etalk-tyrannical-mode "etalk-tyrn"
  "tyrant mode setup for minor mode." nil)

(autoload 'etalk-mail-portnumber "etalk-mail"
  "contact that individual directly via port number" nil)

(autoload 'etalk-mail-extract-portnumber "etalk-mail"
  "Take the mail buffer, and read in parameters to create an ETALK connection."
  nil)

;; Define all local variables globally, otherwise, when we byte compile
;; we will get lots of errors about free variables.
(defvar etalk-remote-who nil
  "User name of who we are talking as per remote etalk buffer.")

(defvar etalk-remote-where nil
  "Machine name of who we are talking as per remote etalk buffer.")

(defvar etalk-remote-tty nil
  "Tty of who we are talking as per remote etalk buffer.")

(defvar etalk-remote-preferred-name nil
  "Preferred name of who we are talking as per remote etalk buffer.")

(defvar etalk-user-filter-type nil
  "Cons cell describing the currently enabled user filter type.
Of the form (INBOUND . OUTBOUND) where each is a nil or a string describing
the filter method used for inbound or outbound text.")

;; TODO: not used, remove all references
(defvar etalk-remote-connect-port nil
  "In remote talk buffers, the port used to make new connections to them.")

(defvar etalk-edit-chars nil
  "Edit characters of who we are talking as per remote etalk buffer.")

(defvar etalk-filter-message nil
  "Local out of band data for a specified connection.
This is text between the escape character (ASCII 3) and the terminator (\\n).
Once this occurs, the value is used and this variable is nil.")

(defvar etalk-filter-message-type nil
  "Identifier code which follows the the out-of-band data escape character.
This determines how the message is parsed")

(defvar etalk-special-request-function nil
  "Function to run synchronously if a remote agrees.
Use `etalk-initiate-special-function' to begin such a program.")

(defvar etalk-remote-type nil
  "The ETALK version of remote talk program.  NIL is vanilla talk.")

(defvar etalk-point nil
  "Marker which points to the end of the talk buffer.")

(defvar etalk-tag nil
  "Set to t for any buffer which is related to etalk.
These buffers are being tagged so etalk can delete all buffers
related to itself")

(defvar etalk-remote-is-emacs nil
  "Non-nil when this remote user is also running Emacs talk.
In the `local' buffer, it means that all remote connections are using Emacs.
Emacs talk buffers allow delete around line terminators.")

;; ------------------------------------------------------------------ ;;
;; Ok, now that the preliminaries are over, define the major modes.   ;;
;; This is broken down into 6 functions.                              ;;
;;                                                                    ;;
;; etalk             : find/start local talk buffer and remote.       ;;
;; etalk-reply       : query local daemon for caller, then call etalk ;;
;; etalk-batch       : read command line arguments to get talk args   ;;
;; etalk-mail        : intiate a talk conversation using mail         ;;
;; etalk-mail-reply  : reply to a talk-mail message to connect        ;;
;; -Modes-                                                            ;;
;; etalk-mode-local: : setup the local talk buffer with marker.       ;;
;; etalk-mode-remote:: setup names, marker, process on remote.        ;;
;; etalk-mode:       : setup keymaps etc common to both buffers.      ;;
;; ------------------------------------------------------------------ ;;

;;;###autoload
(defun etalk (somebody-else &optional socket)
  "Initiate a conversation using standard talk protocols.
SOMEBODY-ELSE is a talk style address of the form:
   joe@some.big.computer.edu tty01   or
   some.big.computer.edu!joe tty01
Optional argument SOCKET is used by other etalk functions to
create connections to a known port."
  (interactive (list (save-window-excursion
		       (etalk-blurb-buffer)
		       ;; Make sure things are ready to go!
		       (etalk-start-one-process)
		       (etalk-read-username))))

  ;; make sure its a username always at least 1 letter... right?
  (if (not (string-match "[A-Za-z]" somebody-else))
      (error "No name specified"))

  ;; check for duplicate calls local buffers
  (if (not (get-buffer (etalk-format etalk-local-buffer-name)))
      (progn
	(set-buffer (get-buffer-create (etalk-format etalk-local-buffer-name)))
	(etalk-mode-local)))
  ;; build new talk buffer, talk-mode-remote will recycle
  (set-buffer (get-buffer-create "etalk-temp"))
  (etalk-mode-remote somebody-else socket)
  (etalk-setup-windows)
  (run-hooks 'etalk-hooks))

(defun etalk-debug ()
  "Effectivly call `etalk' w/ the debug parameter set."
  (interactive)
  (etalk-debug-process t)
  (call-interactively 'etalk))

;;;###autoload
(defun etalk-batch ()
  "Filler procedure to allow etalk to run from a single command line."
  (interactive)
  (etalk-blurb-buffer)
  (let ((ind 0)
	(tname ""))
    (while (not (equal (nth ind command-line-args) "etalk-batch"))
      (setq ind (+ ind 1)))
    (setq ind (+ ind 1))
    (if (nth ind command-line-args)
	(setq tname (concat tname (nth ind command-line-args)))
      (error "Out of arguments at key point!"))
    (setq ind (+ ind 1))
    (if (nth ind command-line-args)
	(setq tname (concat tname " " (nth ind command-line-args))))
    (etalk tname))
  (error "Canceling rest of loadup to preserve etalk windows"))

;;;###autoload
(defun etalk-reply ()
  "Queries local talk daemon for name of last caller and responds."
  (interactive)
  (etalk-blurb-buffer)
  ;; Get things ready for action.
  (etalk-start-one-process)
  ;; Use let to make sure this is reset when we are all done.
  (etalk-send-command "reply"))

;;;###autoload
(defun etalk-mail (somebody-else)
  "Call SOMEBODY-ELSE with socket set to 0.
This sets us up as a server waiting for someone to use `etalk-mail-reply'"
  (interactive (cons (save-window-excursion
		       (etalk-blurb-buffer)
		       (etalk-read-username)) '()))
  (etalk somebody-else 0)		;start the wait...
  (let ((namestuff (etalk-parse-address somebody-else)))
    (etalk-mail-portnumber (etalk-format "%u@%m"
					 (nth 0 namestuff)
					 (nth 1 namestuff)
					 (nth 2 namestuff))))
  (run-hooks 'etalk-mail-hooks))

;;;###autoload
(defun etalk-mail-reply (somebody-else socket)
  "Reply to mail talk request and require socket parameter too!
The response is to SOMEBODY-ELSE.  SOCKET is the socket they have advertised."
  (interactive (save-window-excursion
		 (let ((tmp 0))
		   (if (setq tmp (etalk-mail-extract-portnumber))
		       tmp
		     (cons (progn
			     (etalk-read-username))
			   (cons (string-to-int
				  (read-string "Enter port number: "))
				 '()))))))
  (etalk somebody-else socket)
  (run-hooks 'etalk-mail-hooks))

(defun etalk-mode-local ()
  "Create and set up a local talk buffer.  See `etalk-mode' for details."

  (etalk-mode 'local)
  (setq mode-name etalk-local-mode-string)

  (aset etalk-remote-type 0 "ETALK")
  (aset etalk-remote-type 1 (car etalk-version-number))
  (aset etalk-remote-type 2 (cdr etalk-version-number))

  ;; setup our modeline
  (etalk-setup-modeline (current-buffer))
  (run-hooks 'etalk-mode-local-hooks))

(defun etalk-mode-remote (address &optional socket)
  "Initialize a buffer as a remote etalk window.
This buffer will communicate with the user at ADDRESS and connects to them
via SOCKET."

  ;; parse the command line parameter.
  (let* ((addresslist (etalk-parse-address address))
	 (somebody-else (nth 0 addresslist))
	 (somewhere-else (nth 1 addresslist))
	 (sometty-else (nth 2 addresslist))
	 (tname nil))

    (if (not sometty-else)
	(setq sometty-else ""))

    (setq tname (etalk-format etalk-remote-buffer-name somebody-else
			      somewhere-else sometty-else))

    ;; If the buffer of this name already exists, then kill the one we
    ;; have right now, and use the other one.  Otherwise, turn the
    ;; buffer we have now (which should be called etalk-temp) and
    ;; rename it to the right thing.
    (if (get-buffer tname)
	(progn
	  (kill-buffer (current-buffer))
	  (set-buffer (get-buffer tname)))
      (rename-buffer tname))

    ;; Clear if set.
    (if etalk-clear-buffer-on-call
	(delete-region (point-min) (point-max)))

    (etalk-mode)

    (setq mode-name etalk-remote-mode-string)

    (make-local-variable 'etalk-remote-who)
    (setq etalk-remote-who somebody-else)

    (make-local-variable 'etalk-remote-where)
    (setq etalk-remote-where somewhere-else)

    (make-local-variable 'etalk-remote-tty)
    (setq etalk-remote-tty sometty-else))

  (make-local-variable 'etalk-remote-preferred-name)
  (setq etalk-remote-preferred-name nil)

  (make-local-variable 'etalk-remote-connect-port)
  (setq etalk-remote-connect-port nil)

  (make-local-variable 'etalk-edit-chars)
  (setq etalk-edit-chars "")

  (make-local-variable 'etalk-user-filter-type)
  (setq etalk-user-filter-type (cons nil nil))

  (make-local-variable 'etalk-filter-message)
  (setq etalk-filter-message nil)

  (make-local-variable 'etalk-filter-message-type)
  (setq etalk-filter-message-type 0)

  (make-local-variable 'etalk-special-request-function)
  (setq etalk-special-request-function nil)

  ;; Setup our modeline
  (etalk-setup-modeline (current-buffer))

  ;; upon closer inspection, I couldn't figure out what these were for.
  ;;(make-local-variable 'etalk-special-activation-function)
  ;;(setq etalk-special-activation-function nil)

  ;; (make-local-variable 'etalk-inserted-file)
  ;; (setq etalk-inserted-file nil)

  ;; socket is null if using normal methods, a process if there is a process
  ;; if we want to get that info from another user, and an integer if gotten
  ;; from a mail message

  (etalk-startup-tcp-connection socket)
  (run-hooks 'etalk-mod-eremote-hooks))

(defun etalk-setup-modeline (&optional buffer)
  "Initialize mode-line to display the etalk version information for BUFFER.
This version information is specific to the person you are talking to."
  (save-excursion
    (if buffer (set-buffer buffer))
    (setq mode-line-buffer-identification
	  (list (format "%s %d.%d" (aref etalk-remote-type 0)
			(aref etalk-remote-type 1) (aref etalk-remote-type 2))
		": %15b "
		(if etalk-remote-preferred-name
		    (etalk-format etalk-remote-display-preferred-name))
		(if (cdr etalk-user-filter-type)
		    (concat ">" (cdr etalk-user-filter-type)))
		(if (car etalk-user-filter-type)
		    (concat "<" (car etalk-user-filter-type)))
		))
    (set-buffer-modified-p (buffer-modified-p))))

(defun etalk-mode (&optional local)
  "Major mode for using the standard TALK protocols via Emacs buffers.
The LOCAL talk buffer collects text typed in any talk window.  A
REMOTE talk buffer displays information sent by some other talk client.
 
Editing in a talk buffer is similar to using other talk clients.
When talking other users of etalk, additional behaviors are allowed.

See the etalk info manual for a complete list of additional
functionality of etalk over other talk clients.

Key bindings:
\\{etalk-mode-map}"
  (kill-all-local-variables)


  ;; set the visited file name for logging purposes.
  (if (and etalk-buffer-logging-directory
	   (not (string= etalk-buffer-logging-directory "")))
      (let ((obn (buffer-name)))
	(set-visited-file-name
	 (concat (expand-file-name
		  (concat etalk-buffer-logging-directory
			  (if (string-match "/$" etalk-buffer-logging-directory)
			      "" "/" )
			  (if local
			      (concat (getenv "USER") ".local")
			    (if (string-match "\\( \\)" obn)
				(substring obn 0 (match-beginning 1))
			      obn))))))
	(rename-buffer obn)))

  ;; set the talk-point marker if it exists, or make one.
  (if (not (markerp 'etalk-point))
      (progn
	(make-local-variable 'etalk-point)
	(setq etalk-point (point-max-marker))))

  (set-marker etalk-point (point-max))
  ;; this is a tag saying that the buffer is related to talk
  (make-local-variable 'etalk-tag)
  (setq etalk-tag t)

  (use-local-map etalk-mode-map)
  (setq major-mode 'etalk-mode)

  (set (make-local-variable 'font-lock-defaults)
       '((etalk-font-lock-keywords) t t nil))

  (setq local-abbrev-table etalk-mode-abbrev-table)

  (make-local-variable 'etalk-remote-type)
  (setq etalk-remote-type (copy-sequence ["talk" 1 0]))

  (make-local-variable 'etalk-remote-is-emacs) ;for local too for ease of use.
  (setq etalk-remote-is-emacs nil)	;set to nil till we really know
  (run-hooks 'etalk-mode-hooks))

(defun etalk-log-buffer ()
  "Return the current etalk log buffer.  If it doesn't exist, it is created."

  (if (get-buffer etalk-log-buffer-name)
      ;; just return the buffer if it exists..
      (get-buffer etalk-log-buffer-name)
    ;; set up the buffer so it is usable...
    (set-buffer (get-buffer-create etalk-log-buffer-name))
    (kill-all-local-variables)

    (use-local-map etalk-log-mode-map)

    (setq major-mode 'etalk-log)
    (setq mode-name etalk-log-mode-name)

    (make-local-variable 'etalk-point)
    (setq etalk-point (point-max-marker))

    (make-local-variable 'etalk-tag)
    (setq etalk-tag t)

    (make-local-variable 'etalk-filter-message)
    (setq etalk-filter-message nil)

    (make-local-variable 'etalk-filter-message-type)
    (setq etalk-filter-message-type 0)

    (make-local-variable 'font-lock-defaults)
    (setq font-lock-defaults '(etalk-log-font-lock-keywords))

    (run-hooks 'etalk-log-mode-hooks)

    (current-buffer))
  )

(defun etalk-setup-windows (&optional extrabuff)
  "Reorganize all active etalk windows evenly.
Set `etalk-myself-on-top' to control the position of the local buffer.
Optional EXTRABUFF is a non-talk buffer that should also be displayed."
  (interactive)

  ;; clean out all tuples which are dead.  Prevents weird stuff from happening.
  (etalk-zorch-dead-processes)

  ;; If the debugger is active, we always want an extrabuff!
  (if (and (not extrabuff)
	   (get-buffer "*gud-etalk*")
	   (get-buffer-process (get-buffer "*gud-etalk*")))
      (setq extrabuff (get-buffer "*gud-etalk*")))

  ;; get window data type stuff.
  (let ((nw (+ 1 (length etalk-tcp-list))))
    (if (and extrabuff (bufferp (get-buffer extrabuff)))
	(progn
	  (switch-to-buffer extrabuff)
	  (delete-other-windows (selected-window))
	  (split-window (selected-window) (/ (window-height) (+ nw 1)))
	  (other-window 1)
	  (switch-to-buffer (etalk-format etalk-local-buffer-name))
	  )
      ;; If local was deleted, make sure it has the right mode parts
      (if (not (get-buffer (etalk-format etalk-local-buffer-name)))
	  (progn
	    (set-buffer (get-buffer-create (etalk-format etalk-local-buffer-name)))
	    (etalk-mode-local)))

      (switch-to-buffer (etalk-format etalk-local-buffer-name))
      (delete-other-windows (selected-window)))

    (let ((l etalk-tcp-list))
      (while (> nw 1)
	(split-window (selected-window) (/ (window-height) nw))
	(if etalk-myself-on-top (other-window 1))
	(switch-to-buffer (process-buffer (car (car l))))
	(setq nw (- nw 1))
	(setq l (cdr l))
	(if (not etalk-myself-on-top) (other-window 1)))
      (if etalk-myself-on-top (other-window 1))
      (select-window (get-buffer-window
		      (get-buffer (etalk-format etalk-local-buffer-name)))))))

(defun etalk-setup-windows-with-log ()
  "Reorganize talk windows with the log buffer on the top of the screen."
  (interactive)
  (etalk-setup-windows etalk-log-buffer-name))

(defun etalk-setup-windows-plus (extrabuff)
  "Reorganize talk windows with EXTRABUFF on top."
  (interactive "BOther buffer:")
  (etalk-setup-windows extrabuff))

(defun etalk-pop-kill-ring ()
  "Remove the first element from the kill ring."
  ;;doesn't seem to always work.  Will redo someday...
  (interactive)
  (setq kill-ring-yank-pointer (cdr kill-ring-yank-pointer)))

(defun etalk-blurb-buffer ()
  "You have the right to remain silent.
Anything you say can and will be type-cast against you."
  (or etalk-inhibit-startup-message
      (save-excursion
	(setq etalk-inhibit-startup-message t)
	(get-buffer-create "Your Rights with ETALK")
	(set-buffer (get-buffer "Your Rights with ETALK"))
	(delete-region (point-min) (point-max))
	(insert "
     Thanks for using Emacs talk!  [" etalk-version "]

     This program is free under the GNU general public license.
     See the GNU COPYING file for more details.

     Set `etalk-inhibit-startup-message' to t inhibit this message.

     Please report Bugs/Problems/Suggestions to: Eric Ludlam via
                                                 zappo@gnu.org")
    (display-buffer "Your Rights with ETALK"))))

(provide 'etalk)
;;; etalk ends here
