;;; etalk-sinit --- special function initializer
;;
;; Copyright (C) 1996, 1999 Eric M. Ludlam
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
;;  As of the mid 0.11 series, shared functions will now use AUX
;; links to other user's buffers.  These links will permit users to
;; continue to use the talk interface while also playing an emacs
;; talk game.

;;; $Id: etalk-sinit.el,v 1.5 1999/08/26 11:32:34 zappo Exp $

(require 'etalk-lgame)

;;; Code:

(defcustom etalk-sinit-new-frame (and (boundp 'window-system) window-system)
  "*Non nil means games started be etalk are placed into their own frame.
nil means that the current etalk frame is used to display the
game buffer, and when a game is over, `etalk-setup-windows' is called."
  :group 'etalk
  :type 'boolean)

(defvar etalk-sinit-uid nil
  "User id of the process in the current buffer.")

(defvar etalk-sinit-ruid nil
  "User id tuple who we are going to share a emacs mode with.")

(defvar etalk-sinit-function nil
  "Function string of an emacs mode to use after an AUX connection is made.")

(defvar etalk-sinit-flag nil
  "Non-nil means this buffer is being used for a special init.
Used by etalk-proc.el to distinguish between new talk
UIDs and new Special functions")

(defvar etalk-unique-buffer-ix 0
  "Used to maintain unique temp buffers for creating special processes.")

(defun etalk-sinit-special-function (func user)
  "Attempt to initilize a special function FUNC with USER.
(Where USER is a talk process)  This requires the creation of a new link to
USER, and having USER accept that connection.  One verified, the
special funtion will be initiated, and tyrant mode will be started on
it."
  (let* (;; find the buffer belonging to uid
	 (uidb (process-buffer (car (etalk-process-tuple user))))
	 ;; First, create a buffer to maintain the current state of the
	 ;; new process we are going to create
	 (sfb (get-buffer-create (format " *etalk-tmp-sf-%d*"
					 etalk-unique-buffer-ix)))
	 ;; Now get a process for it
	 (sfp (open-network-stream (format "*etalk-%s-sf-%d*"
					   func etalk-unique-buffer-ix)
				   sfb ;; attach to new buffer
				   etalk-local-host
				   etalk-local-socket))
	 (ruid (save-excursion (set-buffer uidb) etalk-remote-who))
	 )
    ;; boost our uniqueafier
    (setq etalk-unique-buffer-ix (1+ etalk-unique-buffer-ix))
    ;; set our buffer
    (save-excursion
      (set-buffer sfb)
      ;; set the etalk-tag
      (make-local-variable 'etalk-tag)
      (setq etalk-tag t)
      ;; set process management
      (set-process-filter sfp 'etalk-new-special-function-filter)
      (set-process-sentinel sfp 'etalk-new-special-function-sentinel)
      (setq mode-line-process '(" %s?"))
      ;; lets get the UID off this process
      (unwind-protect
	  (progn
	    (make-local-variable 'etalk-sinit-function)
	    (make-local-variable 'etalk-sinit-ruid)
	    (make-local-variable 'etalk-sinit-flag)
	    (make-local-variable 'etalk-sinit-uid)
	    (setq etalk-sinit-flag t
		  etalk-sinit-ruid (etalk-process-tuple user)
		  etalk-sinit-function func
		  etalk-sinit-uid nil
		  etalk-waiting-process sfp
		  etalk-filter-message nil)
	    (etalk-log "Starting wait for user id value for game buffer\n")
	    (message "Waiting for new game id ...")
	    (while (not etalk-sinit-uid)
	      (accept-process-output etalk-process 1))
	    (message "Waiting for new game id ... done")
	    ;; Now send the command which turns this user id into an AUX port.
	    (etalk-send-command (format "auxiliary %d %s %s" etalk-sinit-uid
					ruid func)))
	;; reset the waiting process and make sure it's protected!
	(setq etalk-waiting-process nil))))
  ;; the rest is started up by our newly created filter.
  (message "Connection initialized... waiting for acceptance.")
  )

(defun etalk-sinit-unsolicited (uid func)
  "The user specified by UID has requested to play function FUNC.
We need to immediatly connect up to our subprocess and play the game.  We
don't ask questions because that has already happened."
  (let* (;; find the buffer belonging to uid
	 (uidb (process-buffer (car (etalk-process-tuple uid))))
	 ;; First, create a buffer to maintain the current state of the
	 ;; new process we are going to create
	 (sfb (get-buffer-create (format " *etalk-tmp-sf-%d*"
					 etalk-unique-buffer-ix)))
	 ;; Now get a process for it
	 (sfp (open-network-stream (format "*etalk-%s-sf-%d*"
					   func etalk-unique-buffer-ix)
				   sfb ;; attach to new buffer
				   etalk-local-host
				   etalk-local-socket))
	 )
    ;; boost our uniqueafier
    (setq etalk-unique-buffer-ix (1+ etalk-unique-buffer-ix))
    ;; setup a bunch of variables
    (set-buffer sfb)
    (make-local-variable 'etalk-sinit-ruid)
    (make-local-variable 'etalk-sinit-function)
    (setq etalk-sinit-ruid  (etalk-process-tuple uid)
	  etalk-sinit-function func
	  )
    ;; initiate whatever game or program we were working on.
    (etalk-sinit-startup-tyrant)
    ;; and run player-2 hooks (unsolicited means we are player-2)
    (if (and (boundp 'etalk-tyrannical-mode) etalk-tyrannical-mode)
	(progn
	  (tyrant-player2)
	  (run-hooks 'tyrant-player2-hook)
	  (setq tyrant-player1-hook nil)
	  (setq tyrant-player2-hook nil)))))

(defun etalk-sinit-startup-tyrant ()
  "Initialize a buffer for use with tyrant-mode.
Depends on special variables being set, and a processes being
associated with the current buffer."
  (let ((oldbuff (current-buffer))
	(funct (etalk-verify-multiuser-function etalk-sinit-function))
	(proc (get-buffer-process (current-buffer)))
	(uinfobuf (process-buffer (car etalk-sinit-ruid)))
	who name
	)
    (unwind-protect
	(progn
	  (if (not funct)
	      (error "No such function %s" etalk-sinit-function))
	  ;; We need to do this to clean up the environment of
	  ;; icky functions which seem to follow you around now
	  ;; that we are running in emacs 19
	  (if (boundp 'etalk-tyrant-quit-string)
	      (makunbound 'etalk-tyrant-quit-string))
	  (if (fboundp 'etalk-tyrant-quit-string)
	      (fmakunbound 'etalk-tyrant-quit-string))

	  ;; run the special mode here.
	  (require funct (format "games/%s" funct))
	  (funcall funct)

	  ;; did special mode do something?  We know if buffer is different
	  (if (eq (current-buffer) oldbuff)
	      (error "Misdirected special function."))

	  (make-local-variable 'etalk-tyrant-imprisoned-process)
	  (setq etalk-tyrant-imprisoned-process proc)
	  (set-process-buffer proc (current-buffer))

	  ;; Grab statistics on our remote user id
	  (save-excursion
	    (set-buffer uinfobuf)
	    (setq who etalk-remote-who
		  name etalk-remote-preferred-name))

	  (make-local-variable 'etalk-tyrant-imprisoned-user)
	  (setq etalk-tyrant-imprisoned-user who)

	  (make-local-variable 'etalk-tyrant-imprisoned-preferred-name)
	  (setq etalk-tyrant-imprisoned-preferred-name name)

	  (make-local-variable 'tyrant-opponent-type)
	  (setq tyrant-opponent-type 'etalk)

	  (make-local-variable 'etalk-sinit-flag)
	  (setq etalk-sinit-flag t)

	  (make-local-variable 'etalk-tag)
	  (setq etalk-tag t))

      ;; delete our tmp buffer we were using earlier
      ;; If process not transferred correctly then it is killed.
      ;; It must be protected so remote isn't left hanging.
      (kill-buffer oldbuff)
      )
    ;; Tyrantafy the game
    (etalk-tyrannical-mode)
    ))

(defun etalk-new-special-function-filter (process output)
  "Filter for PROCESS to read OUTPUT from a newly created TCP socket.
This socket and buffer will be turned into a tyranted buffer."
  ;; A new tcp link will should have only the output of one sentence
  ;; which is "yes" or some reason why the link failed followed by a "\n"
  (let ((jumpfrom (current-buffer))
	(dojump t))
    (set-buffer (process-buffer process))
    (let ((md (match-data))
	  (msg nil))
      (setq etalk-filter-message (concat etalk-filter-message output))
      (if (string-match "\n" etalk-filter-message)
	  (progn
	    (setq msg (substring output 0 (match-beginning 0)))
	    (if (string= msg "yes")
		(progn
		  (etalk-sinit-startup-tyrant)
		  (tyrant-player1)
		  (run-hooks 'tyrant-player1-hook)
		  (setq tyrant-player1-hook nil
			tyrant-player2-hook nil
			dojump nil))
	      (message msg)
	      (kill-buffer (process-buffer process))
	      ))
	(set-match-data md))
      (if dojump
	  (progn
	    (message "Moving cursor from %s and returning to %s"
		     (current-buffer) jumpfrom)
	    (set-buffer jumpfrom))
	(message "Leaving cursor in %s and not returning to %s"
		 (current-buffer) jumpfrom)
	))))

(defun etalk-new-special-function-sentinel (process event)
  "Handle PROCESS' signal EVENTs from a new special function."
  (ding t)
  (etalk-log (format "special-init tcp signal: %s\n" event))
  (if (equal (process-status process) "closed")
      (kill-buffer (process-buffer process))
    (message "Funny signal %s from sinit" event)))

(provide 'etalk-sinit)
;;; etalk-sinit ends here
