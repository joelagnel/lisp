;;; etalk-tcp ---  TCP support, including log buffer
;;
;; Copyright (C) 1994, 1995, 1996, 1999 Free Software Foundation
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
;; Please send bug reports, etc. to zappo@gnu.ai.mit.edu.

;;; Commentary:
;;  This bit of Lisp will do basic process control on the etalk binary,
;; including such drudgery as sending commands to an existing process,
;; and creating TCP connections to said binary (which are treated as
;; processes...)
;;

;;; $Id: etalk-proc.el,v 1.24 1999/11/29 16:56:01 zappo Exp $

;;; Code:
(defvar etalk-process-string-internal "etalk-process"
  "*String used to identify the single running version of etalk.")

(defvar etalk-local-host (system-name)
  "*String/function used to identify the machine to connect to.")

(defvar etalk-process nil
  "The current etalk process.")

(defvar etalk-waiting-process nil
  "The process waiting for a user identifier.")

;; Removed TTY from name because unsolicited connects can mess up the
;; search for duplicate names
(defvar etalk-tcp-string-internal "talk-tcp-%u-%M"
  "*String used to identify multiple TCP connections to the talk binary.
It follows the rules of talk-format.")

(defvar etalk-tcp-list nil
  "List of all open TCP connections.")

(defvar etalk-local-socket nil
  "Integer representing the socket to which we will attach ourselves...")

(defvar etalk-remote-socket nil
  "Integer representing the socket to which others will attach themselves...")

(defvar etalk-returned-username nil
  "String representing the username of a person who sent us an announcement.")

(defvar etalk-text-end-regexp "\\([%s\03\07\14]\\)"
  "Regular expression used to find the end end normal inserted text.")

(defvar etalk-log-end-regexp "\\([\03\07]\\)"
  "Regular expression used to find the end normal inserted log text.")

(defvar etalk-message-end-regexp "\\(\n\\)"
  "Regular expression used to find the end of a message string.")

(defconst etalk-max-message-types 13
  "The maximum number of message types sent between talk programs..")

(defun etalk-char-sendable-string (c)
  "Convert char such as ^C into the string ^C."
  (if (equal c ?\C-?)
      "^?"
    (if (<= c ?\C-Z)
	(concat "^" (char-to-string (+ c (- ?A 1))))
      (char-to-string c))))

(defun etalk-start-one-process ()
  "Startup the etalk subprocess.
Only one process is allowed to be in this state at a time."

  (interactive)
  ;; do nothing if the process already exists...
  (if etalk-process
      (if (or (eq (process-status etalk-process) 'exit)
	      (eq (process-status etalk-process) 'signal))
	  (progn
	    (setq etalk-process nil)
	    (etalk-zorch-all-processes)
	    (message "The main talk process was dead, replacing."))))
  ;; ok, try again just in case.
  (if etalk-process
      ()
    (save-excursion
      (set-buffer (etalk-log-buffer))
      (setq etalk-process (eval (append
				 '(start-process
				   etalk-process-string-internal
				   (etalk-log-buffer)
				   etalk-process-file)
				 etalk-process-parameters)))
      (set-process-filter etalk-process 'etalk-log-filter)
      (set-process-sentinel etalk-process 'etalk-log-sentinel)
      (setq mode-line-process '(" %s!")))
    ;; This allows us time to read in the socket identifiers.
    (message "Waiting for talk process to initialize...")
    (sit-for 3)
    ;; don't forget to tell it all about our edit caracters!
    (etalk-send-command
     (concat "set editchar "
	     (etalk-char-sendable-string (aref etalk-edit-characters-mine 0))
	     (etalk-char-sendable-string (aref etalk-edit-characters-mine 1))
	     (etalk-char-sendable-string (aref etalk-edit-characters-mine 2))))
    ;; Now set your announcement name to subprocess.
    (etalk-send-command (concat "set name " etalk-announce-as))
    ;; And finally, make sure we say what our application name is...
    (etalk-send-command "set application etalk")
    (message "Waiting for talk process to initialize...done")))

(defun etalk-debug-process (&optional auto-continue)
  "Start GDB using GUD on the currently active etalk process.
This depends on `etalk-process-file' and the current pid of
`etalk-process'.  If AUTO-CONTINUE is non-nil, then do not actually
stop the currently running etalk process."
  (interactive)
  (etalk-start-one-process)
  (gdb (concat "gdb " etalk-process-file))
  (gud-basic-call (concat "attach " (process-id etalk-process)))
  (if auto-continue (gud-basic-call "cont")))
  
(defun etalk-startup-tcp-connection (socket)
  "Create a new connection to SOCKET."
  
  ;; WARNING : Do not use EDEBUG to debug this function.  The result
  ;;           is that filters are not sent when io first gets parsed!

  ;; Always call this just to make sure that we have the necessary
  ;; process
  (etalk-start-one-process)
  
  (if (not (stringp etalk-remote-who))
      (error "Required local variables not found in buffer %s"
	     (current-buffer)))

  (let ((tp (get-process (etalk-format etalk-tcp-string-internal
				       etalk-remote-who
				       etalk-remote-where
				       etalk-remote-tty))))
    (if tp
	(if (/= (process-exit-status tp) 0)
	    (etalk-nuke-connection (process-buffer))
	  (if (/= (process-exit-status tp) 0)
	      (delete-process tp)
	    (error "You are already talking to them!")))))
  
  ;; if we are restarting a process, get a new edit char list.
  (setq etalk-edit-chars "")
  
  (let ((newtcp nil)
	;; We are sometimes called from IN a filter, so we must set these
	;; locally here or else or stored message will be messed up,
	;; and new messages will be confused.
	(etalk-filter-message-type 0)
	(etalk-filter-message nil))
    (setq newtcp (open-network-stream (etalk-format etalk-tcp-string-internal
						    etalk-remote-who
						    etalk-remote-where
						    etalk-remote-tty)
				      (current-buffer)
				      ;; This string is the standard loopback
				      ;; Used to use the number "127.0.0.1"
				      ;; but some emacses didn't like
				      ;; that version at all. :(
				      etalk-local-host
				      etalk-local-socket))
    (set-process-filter newtcp 'etalk-tcp-filter)
    (set-process-sentinel newtcp 'etalk-sentinel)
    (setq etalk-waiting-process newtcp)
    (setq mode-line-process '(" %s!"))
    (if (stringp socket)		;strings already have a UID encoded in them
	(setq etalk-tcp-list (cons (cons newtcp (string-to-int socket)) etalk-tcp-list))
      (setq etalk-tcp-list (cons (cons newtcp 0) etalk-tcp-list))
      (etalk-log "Starting wait for user id value\n")
      (message "Waiting for new user id ...")
      (while (and (etalk-process-tuple) (= (cdr (etalk-process-tuple)) 0))
	(accept-process-output etalk-process 1))
      (message "Waiting for new user id ... done")))
  (if (not (etalk-process-tuple))
      (error "Lisp Error generating connection")
    (if socket
	(if (numberp socket)
	    (if (> socket 0)
		(etalk-send-command (format "connect %d %d %s@%s %s"
					    (cdr (etalk-process-tuple))
					    socket
					    etalk-remote-who
					    etalk-remote-where
					    etalk-remote-tty))
	      (etalk-send-command (format "wait %s %s@%s %s"
					  (cdr (etalk-process-tuple))
					  etalk-remote-who
					  etalk-remote-where
					  etalk-remote-tty)))
	  ;; else clause is that a symbol means that we are connecting
	  ;; to a pre-connected user struct in the talk process
	  )
      (etalk-send-command (format "call %d %s@%s %s"
				  (cdr (etalk-process-tuple))
				  etalk-remote-who
				  etalk-remote-where
				  etalk-remote-tty))))
  (setq etalk-waiting-process nil)
  )

(defun etalk-log-filter (process output)
  "Filter PROCESS' OUTPUT for commands and information."

  (let ((oldbuffer (current-buffer))	; the buffer we were in
	(op
	 (save-excursion
	   (set-buffer (process-buffer process))
	   (goto-char etalk-point)
	   (beginning-of-line)
	   (point)))
	(initiate-reply nil)		;this is set when a ringer goes off
	(redo-windows nil)
	(output output)
	)

    (set-buffer (process-buffer process))

    (while (> (length output) 0)
      (let ((oldmatch (match-data)))
	;; First, check if we are building a message for the minibuffer.
	(if etalk-filter-message
	    (if (not (string-match etalk-message-end-regexp output))
		(progn
		  (setq etalk-filter-message
			(concat etalk-filter-message output))
		  (setq output ""))
	      (setq etalk-filter-message
		    (concat etalk-filter-message
			    (substring output 0 (match-beginning 1))))
	      (setq output (substring output (match-end 1)))
	      ;; place the message into the log, unless it is flagged.
	      (if (/= etalk-filter-message-type 5)
		  (save-excursion
		    (goto-char etalk-point)
		    (insert "MESSAGE : \"" etalk-filter-message "\"\n")
		    (move-marker etalk-point (point-max))))
	      (unwind-protect
		  (cond
		   ;; unlabeled messages get a 0
		   ((equal etalk-filter-message-type 0)
		    ;; Only msg if not in a prompt
		    (if (window-minibuffer-p (selected-window))
			nil
		      (message etalk-filter-message)))
		   ;; reporting socket we will be connecting to
		   ((equal etalk-filter-message-type 1)
		    (setq etalk-local-socket
			  (string-to-int etalk-filter-message))
		    (save-excursion
		      (goto-char etalk-point)
		      (insert (format " ** local socket number is %d\n"
				      etalk-local-socket))
		      (move-marker etalk-point (point-max))))
		   ;; reporting socket others will be connecting to
		   ((equal etalk-filter-message-type 2)
		    (setq etalk-remote-socket
			  (string-to-int etalk-filter-message))
		    (save-excursion
		      (goto-char etalk-point)
		      (insert (format " ** remote socket number is %d\n"
				      etalk-remote-socket))
		      (move-marker etalk-point (point-max))))
		   ;; reporting a user id struct within the binary.
		   ((equal etalk-filter-message-type 3)
		    (if (listp
			 (etalk-modify-socket (string-to-int etalk-filter-message)
					      etalk-waiting-process))
			(save-excursion
			  (goto-char etalk-point)
			  (insert (format " ** new user process tuple is %s\n"
					  (etalk-process-tuple
					   etalk-waiting-process)))
			  (move-marker etalk-point (point-max)))
		      (save-excursion
			(goto-char etalk-point)
			(insert (format " ** new sinit process number is %s\n"
					etalk-filter-message))
			(move-marker etalk-point (point-max)))))
		   ;; if we get a message that a connection is closed.
		   ((equal etalk-filter-message-type 4)
		    (let* ((omd (match-data))
			   (id (string-to-int etalk-filter-message))
			   (prc (etalk-process-tuple id)))
		      (if prc
			  (progn
			    (etalk-nuke-connection (car prc))
			    (save-excursion
			      (goto-char etalk-point)
			      (insert (format " ** deletion of tuple %d\n"
					      (cdr prc)))
			      (move-marker etalk-point (point-max)))))))
		   ;; what abount very common messages?  Ignore special ones
		   ((equal etalk-filter-message-type 5)
		    ;; Only msg if not in a prompt, ignore otherwise
		    (if (window-minibuffer-p (selected-window))
			nil
		      (message etalk-filter-message)))
		   ;; Ok, what about LOOK_HERE message from daemon?
		   ((equal etalk-filter-message-type 8)
		    ;; Now, go to that buffer, and fix the address!
		    (etalk-proc-look-here))
		   ;; Unsolicited request for a new user window
		   ((equal etalk-filter-message-type 9)
		    (setq redo-windows (etalk-proc-unsolicited-user)))
		   ;; New data link (like new user id)
		   ((equal etalk-filter-message-type 10)
		    (etalk-proc-unsolicited-aux))
		   ;; Show data file request
		   ((equal etalk-filter-message-type 11)
		    (setq redo-windows
			  (find-file-noselect etalk-filter-message)))
		   ;; binary needs y/n answer to a query
		   ((equal etalk-filter-message-type 12)
		    (if (etalk-yorn-p etalk-filter-message)
			(etalk-send-command "y")
		      (etalk-send-command "n")))
		   ;; Filter a connection to a given user
		   ((equal etalk-filter-message-type 13)
		    (let* ((type (prog1
				     (string-to-char etalk-filter-message)
				   (setq etalk-filter-message
					 (substring etalk-filter-message 1))))
			   (jnk (string-match "\\([^:]+\\):\\(.*\\)$"
					      etalk-filter-message))
			   (user (string-to-int
				  (match-string 1 etalk-filter-message)))
			   (style (match-string 2 etalk-filter-message))
			   (tuple (etalk-process-tuple user)))
		      (if (string= style "None") (setq style nil))
		      (save-excursion
			(set-buffer (process-buffer (car tuple)))
			(setq etalk-user-filter-type
			      (if (= type 1)
				  (cons (car etalk-user-filter-type) style)
				(cons style (cdr etalk-user-filter-type))))
			(etalk-setup-modeline))))
		   ;; other cases
		   (t
		    (message "Unknown filter command....")))
		;; These commands are protected by and unwind
		;; in case of an error in the implementation of an error
		;; we guarantee that we clean up after ourselves.
		;; In addition, results may push us into a new buffer.
		;; This is bad, so make sure we nil out the correct
		;; variables in the correct buffer
		(save-excursion
		  (set-buffer (process-buffer process))
		  (setq etalk-filter-message-type 0)
		  (setq etalk-filter-message nil))))
	  ;; When there is no filter message yet...
	  (if (not (string-match etalk-log-end-regexp output))
	      ;; this case, no special message formatting.
	      (save-excursion
		(goto-char etalk-point)
		(insert output)
		(setq output "")
		(move-marker etalk-point (point-max)))
	    ;; this case, we have something to parse around..
	    (save-excursion
	      (goto-char etalk-point)
	      (insert (substring output 0 (match-beginning 1)))
	      (move-marker etalk-point (point-max))
	      (setq output (substring output (match-beginning 1))))
	    ;; Now, look at the output to see what's there.
	    (let ((tchar (string-to-char output)))
	      (setq output (substring output 1))
	      (cond
	       ;; if it is a special filter...
	       ((= tchar 3)
		(setq etalk-filter-message "")
		(if (<= (string-to-char output) etalk-max-message-types)
		    (progn
		      (setq etalk-filter-message-type
			    (string-to-char output))
		      (setq output (substring output 1)))
		  (setq etalk-filter-message-type 0)))
	       ;; if we get a little bell
	       ((= tchar 7)
		(ding t)
		(setq output (substring output 1)))
	       (t
		(message "Weird thing happened in log filter..."))))))
	(store-match-data oldmatch)))

    (etalk-move-realpoint (process-buffer process)
			  (save-excursion
			    (set-buffer (process-buffer process))
			    etalk-point))

    ;; If we got info relating to a new tcp connection, redoodle windows
    (cond ((bufferp redo-windows)
	   (etalk-setup-windows redo-windows))
	  (redo-windows
	   (etalk-setup-windows)))

    ;; If we have a call waiting, then do it.
    (if initiate-reply (etalk-reply))

    ;; if the current buffer is no longer the buffer associated with
    ;; this process, then this indicates a desire to keep that buffer
    ;; setting as it is (new connections, tyrant games, etc).  If the
    ;; current buffer is the same, then switch back to wherever we
    ;; just came from.

    (if (eq (current-buffer) (process-buffer process))
	(set-buffer oldbuffer)
      (message "Leaving CB as %s and not switching back to %s"
	       (current-buffer) oldbuffer)))
  )

(defun etalk-proc-look-here ()
  "Handle look-here message for talk process filter.
Uses localvariables from the fileter."
  (string-match "\\([0-9]+\\)\\( \\)" etalk-filter-message)
  (let* ((uid (substring etalk-filter-message
			 (match-beginning 1)
			 (match-end 1)))
	 (newaddr (substring etalk-filter-message
			     (match-end 2)))
	 (tup (etalk-process-tuple (string-to-int uid))))
    (if (y-or-n-p (format "That user appears to be on %s: call there? "
			  newaddr))
	(save-excursion
	  (set-buffer (process-buffer (car tup)))
	  (setq etalk-remote-where newaddr)
	  ;; I'll assume tty is now invalid...
	  (setq etalk-remote-tty "")
	  ;; and send a new version of the command.....
	  (etalk-send-command (format "call %d %s@%s %s"
				      (cdr (etalk-process-tuple))
				      etalk-remote-who
				      etalk-remote-where
				      etalk-remote-tty)))
      )))

(defun etalk-proc-unsolicited-user ()
  "Handle the advent of an unsolicited user attaching to our process.
Ringers now also use this service, because the binary asks us if we wish
to connect ahead of time."
  ;; The symbol in the socket field means that when the TCP connection
  ;; is made, that TCP will be auto-attached to a user struct
  (save-excursion
    (let ((junk (string-match "\\( \\)" etalk-filter-message))
	  (uid (substring etalk-filter-message 0 (match-beginning 1)))
	  (userhost (substring etalk-filter-message (match-end 1))))
      ;; Create a remote buffer...
      (set-buffer (get-buffer-create "etalk-temp"))
      (etalk-mode-remote userhost uid)))
  t)

(defun etalk-proc-unsolicited-aux ()
  "Handle the advent of an unsolicited auxiliary connection to our process.
This requires we peek at the message to see what they want to do with us."
  ;; The symbol in the socket field means that when the TCP connection
  ;; is made, that TCP will be auto-attached to a user struct
  (let ((md (match-data))
	(junk (string-match "\\( \\)" etalk-filter-message))
	(uid (string-to-int
	      (substring etalk-filter-message 0 (match-beginning 1))))
	(func (substring etalk-filter-message (match-end 1))))
      (etalk-sinit-unsolicited uid func)
      (set-match-data md)))

(defun etalk-log (string)
  "Add STRING to the end of the current log."
  (interactive "sString: ")
  (let ((op (save-excursion (set-buffer (etalk-log-buffer))
			    (marker-position etalk-point))))
    (save-excursion
      (set-buffer (etalk-log-buffer))
      (goto-char etalk-point)
      (insert "lisp: " string)
      (let ((omd (match-data)))
	(if (not (string-match "\n" string)) (insert "\n"))
	(store-match-data omd))
      (move-marker etalk-point (point))
      (if (featurep 'hilit19)
	  (hilit-highlight-region op etalk-point nil t)))
    (etalk-move-realpoint (etalk-log-buffer)
			  (save-excursion
			    (set-buffer (etalk-log-buffer))
			    etalk-point))))

(defun etalk-kill-process ()
  "Last ditch effort to kill a rampant etalk process!!"
  (interactive)
  (delete-process etalk-process))

(defun etalk-send-command-key ()
  "Read in a command for the etalk log buffer.
This function will be called from a keypress, and include that onto
the prompt string by suppressing it.  This is done differently between
the emacsen."
  (interactive)
  (let ((unread-command-char (if (boundp 'last-input-char) last-input-char))
	(unread-command-events (if (and (boundp 'last-input-event)
					(not (boundp 'last-input-char)))
				   (cons last-input-event
					 unread-command-events)
				 nil)))
    (call-interactively 'etalk-send-command)))

(defun etalk-send-quit-command ()
  "Send a quit command to the etalk process..."
  (interactive)
  (etalk-send-command "quit"))

(defun etalk-send-host-command ()
  "Send a host command to the etalk process..."
  (interactive)
  (etalk-send-command "show host"))

(defun etalk-send-clean-command ()
  "Send a clean command to the etalk process..."
  (interactive)
  (etalk-send-command "clean"))

(defun etalk-send-help-command ()
  "Send a help command to the etalk process..."
  (interactive)
  (etalk-send-command "help"))

(defun etalk-send-abort-command ()
  "Send a abort command to the etalk process..."
  (interactive)
  (etalk-send-command "abort"))

(defun etalk-send-users-command ()
  "Send a users command to the etalk process..."
  (interactive)
  (etalk-send-command "show users"))

(defun etalk-send-device-command ()
  "Send a device command to the etalk process..."
  (interactive)
  (etalk-send-command "show device"))

(defun etalk-ringer-on-command ()
  "Send a set ringer on command to the etalk process..."
  (interactive)
  (etalk-start-one-process)
  (etalk-send-command "set ringer on"))

(defun etalk-ringer-off-command ()
  "Send a set ringer off command to the etalk process..."
  (interactive)
  (etalk-send-command "set ringer off"))

(defun etalk-tcpaccept-always-command ()
  "Send a set accepttcp always command to the etalk process..."
  (interactive)
  (etalk-send-command "set accepttcp always")
  (message "Unsolicited TCP requests will always be honored."))

(defun etalk-tcpaccept-query-command ()
  "Send a set accepttcp query command to the etalk process..."
  (interactive)
  (etalk-send-command "set accepttcp query")
  (message "You will be asked before honoring unsolicited TCP requests."))

(defun etalk-tcpaccept-never-command ()
  "Send a set accepttcp never command to the etalk process..."
  (interactive)
  (etalk-send-command "set accepttcp never")
  (message "Unsolicited tcp requests will never be honored."))

(defun etalk-start-shared-app (appname)
  "Start the shared application APPNAME with the selected user."
  (interactive "sShared application to run: ")
  (cond ((not (etalk-unique-remote-p))
	 (error "Can not share an application with multiple users yet"))
	((not (etalk-other-emacs-p (etalk-unique-remote-p)))
	 (error "Selected user is not running etalk compatible client"))
	(t
	 (let ((uid (cdr (etalk-process-tuple (etalk-unique-remote-p)))))
	   (etalk-send-command (format "start %d %s" uid appname))))))

(defun etalk-enable-user-filter (filtertype)
  "Filter output to the selected user of FILTERTYPE."
  (interactive "sFilter Type: ")
  (cond ((not (etalk-unique-remote-p))
	 (error "Can not set a filter on multiple users yet"))
	((not (etalk-other-emacs-p (etalk-unique-remote-p)))
	 (error "Selected user is not running etalk compatible client"))
	(t
	 (let ((uid (cdr (etalk-process-tuple (etalk-unique-remote-p)))))
	   (etalk-send-command (format "filter %d %s" uid filtertype))))))

(defun etalk-send-command (command)
  "Send one text COMMAND to the etalk process.
Commands are defined within the c code in etalk_cmd.c"
  (interactive "sCommand: ")
  (let ((md (match-data)))
    ;; commands must end in \n, so see if it is in there...
    (if (string-match "\n" command)
	(progn
	  (process-send-string etalk-process command)
	  (etalk-log (format "Sending: %s" command)))
      (process-send-string etalk-process (concat command "\n"))
      (etalk-log (format "Sending: %s\n" command)))
    (store-match-data md))
  )

(defun etalk-log-sentinel (process event)
  "The procedure is called when PROCESS is killed with EVENT.
As a result, all TCP connections need to be closed."

  (ding t)
  (etalk-log (format "Signal: %s\n" event))
    (if (or (eq (process-status process) 'exit)
	    (eq (process-status process) 'signal))
	(progn
	  (setq etalk-process nil)
	  (etalk-zorch-all-processes)
	  (message "The main talk process has been removed."))))

  
(defun etalk-sentinel (process event)
  "Called when PROCESS' status is changed with EVENT.
Does remote buffer/process bookkeeping when a connection is lost."

  (ding t)
  (etalk-log (format "TCP Signal: %s\n" event))
  (if (eq (process-exit-status process) 256)
      (progn
	(etalk-remove-process-from-list process)
	(if etalk-hangup-redo-windows
	    (etalk-setup-windows))))
  (save-excursion
    (set-buffer (etalk-format etalk-local-buffer-name))
    (setq etalk-remote-is-emacs (etalk-all-emacs-p))))

(defun etalk-send-where ()
  "This function is used to determine where output is sent.
If you are in a remote buffer, return that buffer, else return the whole
process list."

  (if (equal (current-buffer)
	     (get-buffer (etalk-format etalk-local-buffer-name)))
      etalk-tcp-list
    (current-buffer)))

(defun etalk-send-output (buffer-or-list output-string)
  "Send BUFFER-OR-LIST processes OUTPUT-STRING.
If buffer is nil, then send to list of all open talk processes."

  (let ((pl etalk-tcp-list))
    (if buffer-or-list
	(if (bufferp buffer-or-list)
	    (setq pl (cons (get-buffer-process buffer-or-list) '()))
	  (if (listp buffer-or-list)
	      (setq pl buffer-or-list))))
    (while (car pl)
      ;; This check allows the TCP list to be passed in as well.
      (if (listp (car pl))
	  (process-send-string (car (car pl)) output-string)
	(process-send-string (car pl) output-string))
      (setq pl (cdr pl)))))

(defun etalk-modify-socket (socket &optional process)
  "Find matching SOCKET for PROCESS.
modify `etalk-tcp-list' so that the process' tuple has this new socket id
associated with it."

  (if (not process)
      (setq process (get-buffer-process (current-buffer))))
  (save-excursion
    (set-buffer (process-buffer process))
    ;; This is where we store new sockets for (really meant for uid
    ;; which shows a bad naming scheme for some reason.)
    (if (and (boundp 'etalk-sinit-flag) etalk-sinit-flag)
	(setq etalk-sinit-uid socket)
      (let ((n '())
	    (l etalk-tcp-list))
	(while l
	  (if (not (equal (car (car l)) process))
	      (setq n (cons (car l) n))
	    (setq n (cons (cons (car (car l)) socket) n)))
	  (setq l (cdr l)))
	(setq etalk-tcp-list n)))))

;;;
;; Remember, a tuple is: (PROCESS . UID)
;;
(defun etalk-process-tuple (&optional process-or-id)
  "Find PROCESS-OR-ID in the tcp-list, and return the  associated tuple.
The tuple is important for getting the user id used in
the binary, or based upon the process-or-id id number if it is an integer.."

  (if (not process-or-id)
      (setq process-or-id (get-buffer-process (current-buffer))))
  (if (processp process-or-id)
      (let ((l etalk-tcp-list))
	(while (and l (not (equal (car (car l)) process-or-id)))
	  (setq l (cdr l)))
	(car l))
    (if (integerp process-or-id)
	(let ((l etalk-tcp-list))
	  (while (and l (not (equal (cdr (car l)) process-or-id)))
	    (setq l (cdr l)))
	  (car l))
      nil)))

(defun etalk-read-talkbuffer ()
  "Read in on the minibuffer a talk buffer to close with completion.
not actually used anywhere yet, and not complete."
  (let* ((l etalk-tcp-list)
	 (s nil)
	 (completelist (while l
			 (setq s (cons
				  (cons
				   (save-excursion
				     (set-buffer (process-buffer
						  (car (car l))))
				     (etalk-format "%u@%m"))
				   1)
				  s))
			 (setq l (cdr l)))))
    (completing-read "Kill connection: "
		     completelist nil t ""))
  )

(defun etalk-nuke-connection-buffer (buffer)
  "Zap the talk process, and the associated BUFFER.
Uses delete.  Should change to signals sometime in the near future."

  (interactive "bEtalk kill buffer: ")

  (save-excursion
    (switch-to-buffer buffer)
    (if (get-buffer-process (current-buffer))
	(etalk-nuke-connection (get-buffer-process (current-buffer))))
    (kill-buffer buffer))
  (etalk-setup-windows))

(defun etalk-nuke-connection (&optional process)
  "Zap the talk PROCESS.
Uses delete.  Should change to signals sometime in the near future."

  (interactive)

  (if (and (not process)
	   (equal (current-buffer)
		  (get-buffer (etalk-format etalk-local-buffer-name))))
      (cond
       ((equal (length etalk-tcp-list) 0)
	(if (y-or-n-p "Delete all buffers related to etalk? ")
	    (let ((buflst (buffer-list)))
	      (save-excursion
		(while buflst
		  (set-buffer (car buflst))
		  (if (and (boundp 'etalk-tag) etalk-tag)
		      (kill-buffer (car buflst)))
		  (setq buflst (cdr buflst))))))
	)
       ((equal (length etalk-tcp-list) 1)
	(etalk-nuke-connection (car (car etalk-tcp-list)))
	(setq etalk-tcp-list '()))
       (t
	(if (y-or-n-p "Zorch all talk processes? ")
	    (etalk-zorch-all-processes))))
    (let ((p (if process process (get-buffer-process (current-buffer)))))
      ;; if we wish to hangup, use the hangup command.
      (etalk-send-command (format "hangup %d\n"
				  (cdr (etalk-process-tuple p))))
      (etalk-remove-process-from-list p))))

(defun etalk-remove-process-from-list (process)
  "Remove one PROCESS object from the process list."

  (let ((n '())
	(l etalk-tcp-list))
    (while l
      (if (not (equal (car (car l)) process))
	  (setq n (cons (car l) n)))
      (setq l (cdr l)))
    (setq etalk-tcp-list n)))

(defun etalk-zorch-dead-processes ()
  "Kill all the talk processes in list which are no longer running."

  (interactive)
  (let ((mlist etalk-tcp-list))
    (while mlist
      (if (equal (process-exit-status (car (car mlist))) 0)
	  ()				;Hey.. keep these
	(etalk-remove-process-from-list (car (car mlist))))
      (setq mlist (cdr mlist)))))

(defun etalk-zorch-all-processes ()
  "Kill every single talk process in the talk process list."

  (interactive)
  (let ((mlist etalk-tcp-list))
    (while mlist
      (if (equal (process-exit-status (car (car mlist))) 0)
	  (etalk-nuke-connection (car (car mlist)))
	(etalk-remove-process-from-list (car (car mlist))))
      (setq mlist (cdr mlist))))
  ;; just to make sure, set this to nil
  (setq etalk-tcp-list nil))

(defun etalk-all-emacs-p ()
  "Return t if all connections are running Emacs talk, nil otherwise."

  (interactive)
  (let ((mlist etalk-tcp-list)
	(all t))
    (while (and mlist all)
      (if (etalk-other-emacs-p (car (car mlist)))
	  ()				;Hey.. keep these
	(setq all nil))
      (setq mlist (cdr mlist)))
    all))

(provide 'etalk-proc)
;;; etalk-proc ends here
