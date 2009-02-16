;;; simap4.el --- A simple IMAP4 (RFC 2060) protocol driver module

;; Copyright (C) 2000, 2001 Takeshi Morishima

;; Author: Takeshi Morishima <tm@interaccess.com>
;; Keywords: mail, imap4
;; Version: See simap4-version definition in the code section.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This is an imap4 driver library module intended to use for MUAs
;; that require only simple message fetch and status update.
;;
;; Set simap4-default-debug-mode to 'display-process-buffer to monitor
;; imap4 process interaction in another frame.

;;; Code:
(require 'timer)

(defconst simap4-version "1.0")
(defconst simap4-compat-level "0")
(defconst simap4-file-version "$Revision: 1.4 $")

;; configureable timer values
(defvar simap4-progress-display-time 5)
(defvar simap4-background-check-time 1)

;; flags
(defvar simap4-default-progress-display-flag nil)
(defvar simap4-default-calc-throughput-flag nil)
(defvar simap4-default-debug-mode nil)
(defvar simap4-debug t)

;; default constants
(defconst *simap4-default-port 143)
(defconst *simap4-default-connection-timeout 60)
(defconst *simap4-default-command-timeout 20)
(defconst *simap4-default-command-retries 3)
(defconst *simap4-default-noop-response-timeout 10)
(defconst *simap4-default-keepalive-interval 600)


;; definition of object attributes and access functions
(defmacro simap4-define-access-functions (object-name &rest attrs)
  (cons
   'progn
   (append
    (mapcar
     '(lambda (attr)
	(` (defsubst
	     (, (intern (concat object-name "-get-" (symbol-name attr))))
	     (obj)
	     (, (concat "Get attribute for '" (symbol-name attr) "'"))
	     (get obj '(, attr))))) attrs)
    (mapcar
     '(lambda (attr)
	(` (defsubst
	     (, (intern (concat object-name "-set-" (symbol-name attr))))
	     (obj val)
	     (, (concat "Set attribute for '" (symbol-name attr) "'"))
	     (put obj '(, attr) val))))
     attrs))))

(simap4-define-access-functions
 "simap4"	    		; OBJECT for "simap4"
 simap4-object			; Indicates an simap4 object
 ;; Server connection attributes
 proc				; TCP stream process for the IMAP4 session
 buffer				; Buffer associated with the IMAP4 session
 port				; TCP destination port number to be connected
 server				; Server to be connected
 login				; Login to be used when authenticate
 password			; Password to be used when authenticate
 client-id			; Client id string to identify application
 ;; State attributes
 selected-mailbox		; Currently selected mailbox name
 selected-mailbox-read-only	; t if currently selected mailbox is ro
 state				; Current IMAP4 state
 failure-cause			; Failure cause
 ;; Connection maintenance
 connection-timeout		; Timeout for initial connection
 keepalive-interval		; Interval for keepalive
 timer				; Keep alive timer
 ;; Command queues
 current-job			; Current job object
 job-queue			; List of job objects to be executed
 last-job			; Last job object executed
 checker-timer			; A timer to check job in background
 command-timeout		; Timeout for this command
 command-retries		; Number of retries for commands
 ;; Throughput measurement and progress display
 use-prog-disp			; An option flag if progress is displayed
 calc-throughput		; An option flag if throughput is calculated
 throughput			; Current throughput
 valid-period     		; For throughput calculation
 invalid-period			; For throughput calculation
 max-throughput			; For throughput calculation
 total-bytes			; For throughput calculation
 total-period			; For throughput calculation
 expected-size			; For progress calculation
 current-size			; For progress calculation
 progress			; For progress calculation
 ;; debug
 debug-mode			; Debug flag
 debug-frame			; Proc buffer disp frame if supported
 )

(simap4-define-access-functions
 "simap4-job"	    		; OBJECT for "simap4-job"
 parent				; Parent object for this job
 tag				; Tag
 command-string			; Command string
 callback			; A function to be called when job is done
 start-point			; Start point of the region of cmd output
 end-point			; End point of the region of cmd output
 result-code			; Result code. "OK", "NO" or "BAD"
 result-resp-code		; Optional response code of the job result
 result-explanation		; Explanation of the job result
 start-time			; When this job started
 end-time			; When this job ended
 checkpoint			; Point when this job was last checked
 checktime			; Time when this job was last checked
 ;; Last command data (parsed IMAP4 Response)
 resp-ok			; A list of parsed OK Responses
 resp-no			; A list of parsed NO Responses
 resp-bad			; A list of parsed BAD Responses
 resp-preauth			; A list of parsed PREAUTH Responses
 resp-bye			; A list of parsed BYE Responses
 resp-capability		; A list of parsed CAPABILITY Responses
 resp-list			; A list of parsed LIST Responses
 resp-lsub			; A list of parsed LSUB Responses
 resp-status			; A list of parsed STATUS Responses
 resp-search			; Parsed SEARCH Response
 resp-flags			; Parsed FLAGS Response
 resp-exists			; An integer indicates # of msg existent
 resp-recent			; An integer indicates # of recent msg
 resp-expunge			; A list of expunged msg seq num
 resp-fetch			; A list of parsed FETCH Responses
 )

;; define other small building blocks
(defsubst simap4-result-ok-p (job)
  (string= (simap4-job-get-result-code job) "ok"))

(defsubst simap4-result-no-p (job)
  (string= (simap4-job-get-result-code job) "no"))

(defsubst simap4-result-bad-p (job)
  (string= (simap4-job-get-result-code job) "bad"))

(defsubst simap4-connected-p (obj)
  (if (not (processp (simap4-get-proc obj)))
      nil
    (not (eq (process-status (simap4-get-proc obj)) 'closed))))

(defsubst simap4-authenticated-p (obj)
  (memq (simap4-get-state obj) '(authenticated selected)))

(defsubst simap4-selected-p (obj)
  (eq (simap4-get-state obj) 'selected))

(defsubst simap4-object-p (obj)
  (and (symbolp obj) (simap4-get-simap4-object obj)))

(defsubst simap4-obj-check (obj)
  (if (not (simap4-object-p obj)) (error "Error: wrong type argument")))

(defsubst simap4-start-keepalive-timer (obj)
  (cancel-timer (simap4-get-timer obj))
  (timer-set-time (simap4-get-timer obj)
		  (timer-relative-time (current-time)
				       (simap4-get-keepalive-interval obj)))
  (timer-activate (simap4-get-timer obj)))

(defsubst simap4-stop-keepalive-timer (obj)
  (cancel-timer (simap4-get-timer obj)))

;; object creation and deletion
(defvar *simap4-object-list nil)
(defun simap4-object-create (client-id server login password
				       &optional timeout port)
  (let ((obj (intern (concat "simap4:" client-id ":" login "@" server))))
    (setq *simap4-object-list (cons obj (delq obj *simap4-object-list)))
    (if (and (simap4-get-timer obj) (timerp (simap4-get-timer obj)))
	(cancel-timer (simap4-get-timer obj)))
    (if (and (simap4-get-proc obj) (processp (simap4-get-proc obj)))
	(delete-process (simap4-get-proc obj)))
    (if (and (simap4-get-buffer obj) (bufferp (simap4-get-buffer obj)))
	(kill-buffer (simap4-get-buffer obj)))
    (simap4-set-simap4-object obj t)
    (simap4-set-port obj (or port *simap4-default-port))
    (simap4-set-client-id obj client-id)
    (simap4-set-server obj server)
    (simap4-set-login obj login)
    (simap4-set-password obj password)
    (simap4-proc-buffer obj)
    (simap4-set-state obj 'disconnected)
    (simap4-set-connection-timeout
     obj (or timeout *simap4-default-connection-timeout))
    (simap4-set-keepalive-interval obj *simap4-default-keepalive-interval)
    (simap4-set-timer obj (timer-create))
    (simap4-set-command-timeout obj *simap4-default-command-timeout)
    (simap4-set-command-retries obj *simap4-default-command-retries)
    (timer-set-function (simap4-get-timer obj) 'simap4-do-keepalive obj)
    ;; default options
    (simap4-set-use-prog-disp obj simap4-default-progress-display-flag)
    (simap4-set-calc-throughput obj simap4-default-calc-throughput-flag)
    (simap4-set-debug-mode obj simap4-default-debug-mode)
    obj))

(defun simap4-setup-proc-buffer-attributes (&optional buf)
  (setq buf (or buf (current-buffer)))
  (if (featurep 'mule)
      (save-excursion
	(set-buffer buf)
	(cond ((condition-case nil
		   (progn (set-buffer-multibyte nil) t)
		 (error nil)))
	      ((local-variable-p 'mc-flag)
	       (set 'mc-flag nil))
	      ((boundp 'enable-multibyte-characters)
	       (set 'enable-multibyte-characters nil))))))

(defun simap4-proc-buffer (obj)
  (if (not (buffer-live-p (simap4-get-buffer obj)))
      (progn
	(simap4-set-buffer obj (get-buffer-create
				(format
				 " *simap4:%s:%s@%s*"
				 (simap4-get-client-id obj)
				 (simap4-get-login obj)
				 (simap4-get-server obj))))
	(simap4-setup-proc-buffer-attributes (simap4-get-buffer obj))))
  (simap4-get-buffer obj))

(defun simap4-object-delete (obj)
  (simap4-obj-check obj)
  (simap4-disconnect obj)
  (if (simap4-get-current-job obj)
      (unintern (simap4-get-current-job obj)))
  (mapcar 'unintern (simap4-get-job-queue obj))
  (simap4-set-current-job obj nil)
  (simap4-set-job-queue obj nil)
  (kill-buffer (simap4-get-buffer obj))
  (delq obj *simap4-object-list)
  (unintern obj))

(defun simap4-find-object-by-account (client-id login server)
  (let ((obj (intern-soft
	      (concat "simap4:" client-id ":" login "@" server))))
    (and obj (simap4-object-p obj) obj)))

;; State machine (based on definition in RFC-2060)
;;
;;            +--------------------------------------+
;;            |             disconnected             |
;;            +--------------------------------------+
;;                      || (1)       || (2)        || (3)
;;                      VV           ||            ||
;;            +-----------------+    ||            ||
;;            |non-authenticated|    ||            ||
;;            +-----------------+    ||            ||
;;             || (7)   || (4)       ||            ||
;;             ||       VV           VV            ||
;;             ||     +----------------+           ||
;;             ||     | authenticated  |<=++       ||
;;             ||     +----------------+  ||       ||
;;             ||       || (7)   || (5)   || (6)   ||
;;             ||       ||       VV       ||       ||
;;             ||       ||    +--------+  ||       ||
;;             ||       ||    |selected|==++       ||
;;             ||       ||    +--------+           ||
;;             ||       ||       || (7)            ||
;;             VV       VV       VV                VV
;;            +--------------------------------------+
;;            |        failed or disconnected        |
;;            +--------------------------------------+
;;
;;       (1) connection without pre-authentication (OK greeting)
;;       (2) pre-authenticated connection (PREAUTH greeting)
;;       (3) rejected connection (BYE greeting) or timeout
;;       (4) successful LOGIN (or AUTHENTICATE TBD) command
;;       (5) successful SELECT or EXAMINE command
;;       (6) CLOSE command, or failed SELECT or EXAMINE command
;;       (7) LOGOUT command, server shutdown, or connection closed

(defun simap4-connect (obj)
  "OBJ is an simap4 object. Make an IMAP4 connection for the IMAP4
server specified by OBJ. Returns non-nil if connected successfully."
  (simap4-obj-check obj)
  (if (not (simap4-connected-p obj))
      (let ((case-fold-search t)
	    (server (simap4-get-server obj))
	    (port (simap4-get-port obj))
	    (buff (simap4-proc-buffer obj))
	    (greeting-regexp
	     (format "^\* \\(OK\\|PREAUTH\\|BYE\\) ?\\(.*\\)\r?\n"))
	    (job (simap4-job-create obj))
	    proc timeout-p start)
	;; clear failure flag
	(simap4-set-state obj 'disconnected)
	(simap4-set-failure-cause obj nil)
	(catch 'simap4-error-exit
	  (save-excursion
	    (set-buffer buff)
	    (erase-buffer)
	    (goto-char (point-max))
	    (setq start (point))
	    (with-timeout ((simap4-get-connection-timeout obj)
			   (simap4-failure obj 'connection-timeout)
			   (throw 'simap4-error-exit nil))
	      (simap4-debug obj "->[Connecting %s:%d]" server port)
	      (condition-case nil
		  (setq proc (open-network-stream
			      "imap4" buff server port))
		(error (simap4-failure obj 'unable-to-open)
		       (throw 'simap4-error-exit nil)))
	      (condition-case nil
		  (set-process-coding-system proc 'binary 'binary)
		(error nil))
	      (simap4-debug obj "<-[Connected]")
	      (simap4-set-proc obj proc)
	      (goto-char start)
	      (while (not (re-search-forward greeting-regexp nil t))
		(accept-process-output proc 3)
		(goto-char start)))
	    (forward-line 1)
	    (simap4-job-set-result-code job (match-string 1))
	    (simap4-job-set-result-explanation job (match-string 2))
	    (simap4-set-last-job obj job)
	    (cond ((string= (simap4-job-get-result-code job) "BYE")
		   (simap4-failure obj 'rejected-by-server))
		  ((string= (simap4-job-get-result-code job) "OK")
		   (simap4-set-state obj 'non-authenticated)
		   (simap4-clear-throutput obj)
		   (simap4-start-keepalive-timer obj))
		  ((string= (simap4-job-get-result-code job) "PREAUTH")
		   (simap4-set-state obj 'authenticated)
		   (simap4-clear-throutput obj)
		   (simap4-start-keepalive-timer obj)))
	    (simap4-debug obj "<-[Greetings confirmed]")))
	(not (eq (simap4-get-state obj) 'failed)))))

(defun simap4-disconnect (obj)
  "OBJ is an simap4 object. Disconnect the IMAP4 connection for the
IMAP4 server specified by OBJ. Returns non-nil if disconnected
successfully."
  (simap4-obj-check obj)
  (if (simap4-connected-p obj)
      (progn
	(delete-process (simap4-get-proc obj))
	(simap4-set-proc obj nil)
	(simap4-set-state obj 'disconnected)
	t)))

(defun simap4-failure (obj cause)
  (simap4-obj-check obj)
  (if (simap4-connected-p obj)
      (let ((proc (simap4-get-proc obj)))
	(delete-process proc)))
  (simap4-stop-keepalive-timer obj)
  (simap4-set-state obj 'failed)
  (simap4-set-failure-cause obj cause))

(defun simap4-recover-state (obj state mbox)
  (let (res)
    (if (simap4-connected-p obj)
	(simap4-disconnect obj))
    (simap4-set-state obj 'disconnected)
    (setq res (simap4-connect obj))
    (if (and res (member state '(authenticated selected)))
	(progn
	  (setq res (simap4-login obj))
	  (if (and res (eq state 'selected))
	      (setq res
		    (simap4-select obj mbox)))))
    res))

(put 'simap4-with-retries 'lisp-indent-function 1)
(defmacro simap4-with-retries (obj &rest body)
  `(let ((num-retries (simap4-get-command-retries ,obj))
	 (current-state (simap4-get-state ,obj))
	 (current-mbox (simap4-get-selected-mailbox ,obj))
	 res)
     (simap4-obj-check ,obj)
     (progn
       (while (not (zerop num-retries))
	 (setq res (progn ,@body))
	 (cond ((or (member res '(disconnected command-timeout)))
		(if (simap4-recover-state ,obj
					  current-state current-mbox)
		    (setq num-retries (1- num-retries))
		  (error "simap4: could not recover the connection.")))
	       (t (setq num-retries 0))))
       (if (and (zerop num-retries)
		(or (member res '(disconnected command-timeout))))
	   (error "simap4: could not complete a command.")))
     res))

(defun simap4-do-keepalive (obj)
  (let (timeout-p)
    (if (not (simap4-connected-p obj))
	(simap4-failure obj 'lost-connection)
      (with-timeout
	  (*simap4-default-noop-response-timeout (setq timeout-p t))
	(simap4-exec-command1 obj "noop"))
      (if timeout-p
	  (simap4-failure obj 'no-keep-alive-response)
	(simap4-start-keepalive-timer obj)))))

;;------------------------------------------------------------------------
;; 6.1.    Client Commands - Any State
;; 6.1.1.  CAPABILITY Command
(defun simap4-capability (obj)
  "OBJ is an simap4 object. Send CAPABILITY IMAP4 Client command.
Returns non-nil if tagged response is OK. Otherwise nil.

The following function(s) should be used to retrieve parsed output.
simap4-job-get-resp-capability (see `simap4-parse-resp-capability'
for format.)
"
  (simap4-with-retries obj
    (let ((job (simap4-exec-command obj "capability")))
      (if (and job (simap4-result-ok-p job))
	  (simap4-parse-untagged-response obj job)))))

;; 6.1.2.  NOOP Command
(defun simap4-noop (obj)
  "OBJ is an simap4 object. Send NOOP IMAP4 Client command.
Returns non-nil if tagged response is OK. Otherwise nil.

No specific responses are expected for this command.
"
  (simap4-obj-check obj)
  (let ((job (simap4-exec-command obj "noop")))
    (if (and job (simap4-result-ok-p job))
	(simap4-parse-untagged-response obj job))))

;; 6.1.3.  LOGOUT Command
(defun simap4-logout (obj)
  "OBJ is an simap4 object. Send LOGOUT IMAP4 Client command.
Returns non-nil if tagged response is OK. Otherwise nil.

The following function(s) should be used to retrieve parsed output.
simap4-job-get-resp-bye (see `simap4-parse-resp-bye' for format.)
"
  (simap4-obj-check obj)
  (simap4-stop-keepalive-timer obj)
  (if (simap4-connected-p obj)
      (let ((proc (simap4-get-proc obj)))
	(simap4-exec-command obj "logout")
	(delete-process proc)))
  (simap4-set-state obj 'disconnected))

;;------------------------------------------------------------------------
;; 6.2.    Client Commands - Non-Authenticated State
;; 6.2.1.  AUTHENTICATE Command
;;; NOT SUPPORTED.

;; 6.2.2.  LOGIN Command
(defun simap4-login (obj)
  "OBJ is an simap4 object. Send LOGIN IMAP4 Client command.
Returns non-nil if tagged response is OK. If the state is already
in an authenticated state (authenticated or selected) returns t.
Otherwise nil.

No specific responses are expected for this command.
"
  (simap4-obj-check obj)
  (let ((state (simap4-get-state obj)))
    (cond ((eq state 'non-authenticated)
	   (simap4-with-retries obj
	     (if (eq (simap4-get-state obj) 'non-authenticated)
		 (let* ((login (simap4-get-login obj))
			(password (simap4-get-password obj))
			(job (simap4-exec-command
			      obj (format "login %s %s" login password))))
		   (if (and job (simap4-result-ok-p job))
		       (simap4-set-state obj 'authenticated)
		     (simap4-failure obj 'login-rejected)
		     nil)))))
	  ((member state '(authenticated selected))
	   t))))

;;------------------------------------------------------------------------
;; 6.3.    Client Commands - Authenticated State
;; 6.3.1.  SELECT Command
(defun simap4-select (obj mailbox)
  "OBJ is an simap4 object. Send SELECT IMAP4 Client command.
Returns non-nil if tagged response is OK. Otherwise nil.

MAILBOX is a string of mailbox to select for read/write operation.

The following function(s) should be used to retrieve parsed output.
simap4-job-get-resp-flags (see `simap4-parse-resp-flags' for format.)
simap4-job-get-resp-exists (see `simap4-parse-resp-exists' for format.)
simap4-job-get-resp-recent (see `simap4-parse-resp-recent' for format.)
simap4-job-get-resp-ok [UNSEEN] (see `simap4-parse-resp-ok' for format.)
simap4-job-get-resp-ok [PERMANENTFLAGS]
"
  (simap4-obj-check obj)
  (if (simap4-authenticated-p obj)
      (if (and (simap4-selected-p obj)
	       (string= (simap4-get-selected-mailbox obj) mailbox)
	       (not (simap4-get-selected-mailbox-read-only obj)))
	  'already-selected
	;; for the retry mechanism to work properly, set the state to
	;; authenticated and the mailbox to nil.
	(simap4-set-state obj 'authenticated)
	(simap4-set-selected-mailbox obj nil)
	(simap4-set-selected-mailbox-read-only obj nil)
	;; process select command.
	(simap4-with-retries obj
	  (let ((job (simap4-exec-command
		      obj (format "select %s" mailbox))))
	    (if (and job (simap4-result-ok-p job))
		(save-excursion
		  (simap4-parse-untagged-response obj job)
		  (simap4-set-state obj 'selected)
		  (simap4-set-selected-mailbox obj mailbox)
		  (simap4-set-selected-mailbox-read-only obj nil)
		  'newly-selected)
	      ;; if select fails, go back to authenticated
	      (simap4-set-state obj 'authenticated)
	      (simap4-set-selected-mailbox obj nil)
	      nil))))))

;; 6.3.2.  EXAMINE Command
(defun simap4-examine (obj mailbox)
  "OBJ is an simap4 object. Send EXAMINE IMAP4 Client command.
Returns non-nil if tagged response is OK. Otherwise nil.

MAILBOX is a string of mailbox to select for read only operation.

The following function(s) should be used to retrieve parsed output.
simap4-job-get-resp-flags (see `simap4-parse-resp-flags' for format.)
simap4-job-get-resp-exists (see `simap4-parse-resp-exists' for format.)
simap4-job-get-resp-recent (see `simap4-parse-resp-recent' for format.)
simap4-job-get-resp-ok [UNSEEN] (see `simap4-parse-resp-ok' for format.)
simap4-job-get-resp-ok [PERMANENTFLAGS]
"
  (simap4-obj-check obj)
  (if (simap4-authenticated-p obj)
      (if (and (simap4-selected-p obj)
	       (string= (simap4-get-selected-mailbox obj) mailbox)
	       (simap4-get-selected-mailbox-read-only obj))
	  'already-selected
	;; for the retry mechanism to work properly, set the state to
	;; authenticated and the mailbox to nil.
	(simap4-set-state obj 'authenticated)
	(simap4-set-selected-mailbox obj nil)
	(simap4-set-selected-mailbox-read-only obj nil)
	;; process examine command.
	(simap4-with-retries obj
	  (let ((job (simap4-exec-command
		      obj (format "examine %s" mailbox))))
	    (if (and job (simap4-result-ok-p job))
		(progn
		  (simap4-parse-untagged-response obj job)
		  (simap4-set-state obj 'selected)
		  (simap4-set-selected-mailbox obj mailbox)
		  (simap4-set-selected-mailbox-read-only obj t)
		  'newly-selected)
	      ;; if select fails, go back to authenticated
	      (simap4-set-state obj 'authenticated)
	      (simap4-set-selected-mailbox obj nil)
	      nil))))))

;; 6.3.3.  CREATE Command
(defun simap4-create (obj mailbox)
  "OBJ is an simap4 object. Send CREATE IMAP4 Client command.
Returns non-nil if tagged response is OK. Otherwise nil.

MAILBOX is a string of mailbox to create.

No specific responses are expected for this command.
"
  (simap4-obj-check obj)
  (let ((job (simap4-exec-command obj (format "create %s" mailbox))))
    (if (and job (simap4-result-ok-p job))
	(simap4-parse-untagged-response obj job))))

;; 6.3.4.  DELETE Command
(defun simap4-delete (obj mailbox)
  "OBJ is an simap4 object. Send DELETE IMAP4 Client command.
Returns non-nil if tagged response is OK. Otherwise nil.

MAILBOX is a string of mailbox to delete.

No specific responses are expected for this command.
"
  (simap4-obj-check obj)
  (let ((job (simap4-exec-command obj (format "delete %s" mailbox))))
    (if (and job (simap4-result-ok-p job))
	(simap4-parse-untagged-response obj job))))

;; 6.3.5.  RENAME Command
(defun simap4-rename (obj old new)
  "OBJ is an simap4 object. Send RENAME IMAP4 Client command.
Returns non-nil if tagged response is OK. Otherwise nil.

OLD is a string of mailbox to be renamed. NEW is a string of new
mailbox name.

No specific responses are expected for this command.
"
  (simap4-obj-check obj)
  (let ((job (simap4-exec-command obj (format "rename %s %s" old new))))
    (if (and job (simap4-result-ok-p job))
	(simap4-parse-untagged-response obj job))))

;; 6.3.6.  SUBSCRIBE Command
(defun simap4-subscribe (obj mailbox)
  "OBJ is an simap4 object. Send SUBSCRIBE IMAP4 Client command.
Returns non-nil if tagged response is OK. Otherwise nil.

MAILBOX is a string of mailbox to subscribe.

No specific responses are expected for this command.
"
  (simap4-with-retries obj
    (let ((job (simap4-exec-command obj (format "subscribe %s" mailbox))))
      (if (and job (simap4-result-ok-p job))
	  (simap4-parse-untagged-response obj job)))))

;; 6.3.7.  UNSUBSCRIBE Command
(defun simap4-unsubscribe (obj mailbox)
  "OBJ is an simap4 object. Send UNSUBSCRIBE IMAP4 Client command.
Returns non-nil if tagged response is OK. Otherwise nil.

MAILBOX is a string of mailbox to unsubscribe.
"
  (simap4-with-retries obj
    (let ((job (simap4-exec-command obj (format "unsubscribe %s" mailbox))))
      (if (and job (simap4-result-ok-p job))
	  (simap4-parse-untagged-response obj job)))))

;; 6.3.8.  LIST Command
(defun simap4-list (obj ref-name mailbox)
  "OBJ is an simap4 object. Send LIST IMAP4 Client command.
Returns non-nil if tagged response is OK. Otherwise nil.

REF-NAME and MAILBOX are strings that specify reference name and
mailbox name with possible wildcards. Please refer to RFC2060 for
details about how reference name and mailbox are interpreted by
the server and what is the possible output.

The following function(s) should be used to retrieve parsed output.
simap4-job-get-resp-list (see `simap4-parse-resp-list' for format.)
"
  (simap4-with-retries obj
    (let ((job (simap4-exec-command
		obj (format "list %s %s" ref-name mailbox))))
      (if (and job (simap4-result-ok-p job))
	  (simap4-parse-untagged-response obj job)))))

;; 6.3.9.  LSUB Command
(defun simap4-lsub (obj ref-name mailbox)
  "OBJ is an simap4 object. Send LSUB IMAP4 Client command.
Returns non-nil if tagged response is OK. Otherwise nil.

REF-NAME and MAILBOX are strings that specify reference name and
mailbox name with possible wildcards. Please refer to RFC2060 for
details about how reference name and mailbox are interpreted by
the server and what is the possible output.

The following function(s) should be used to retrieve parsed output.
simap4-job-get-resp-lsub (see `simap4-parse-resp-lsub' for format.)
"
  (simap4-with-retries obj
    (let ((job (simap4-exec-command
		obj (format "lsub %s %s" ref-name mailbox))))
      (if (and job (simap4-result-ok-p job))
	  (simap4-parse-untagged-response obj job)))))

;; 6.3.10. STATUS Command
(defun simap4-status (obj mailbox data-items)
  "OBJ is an simap4 object. Send STATUS IMAP4 Client command.
Returns non-nil if tagged response is OK. Otherwise nil.

MAILBOX is a string of mailbox name to get status. DATA-ITEMS is a
string that lists IMAP4 specified data items for status.

The currently defined status data items that can be requested are:

      MESSAGES
      RECENT
      UIDNEXT
      UIDVALIDITY
      UNSEEN

Please refer to RFC2060, for more details about these data items.

The following function(s) should be used to retrieve parsed output.
simap4-job-get-resp-status (see `simap4-parse-resp-status' for format.)
"
  (simap4-with-retries obj
    (let ((job (simap4-exec-command
		obj (format "status %s %s" mailbox data-items))))
      (if (and job (simap4-result-ok-p job))
	  (simap4-parse-untagged-response obj job)))))

;; 6.3.11. APPEND Command
(defun simap4-append (obj mailbox message &optional flags date-time)
  "OBJ is an simap4 object. Send APPEND IMAP4 Client command.
Returns non-nil if tagged response is OK. Otherwise nil.

MAILBOX is the mailbox name to append. MESSAGE is a bstr
string or an array of [<buffer> <start-point> <end-point>]
"
  (simap4-obj-check obj)
  (let (job total command work-buf)
    (unwind-protect
	(save-excursion
	  (setq work-buf (get-buffer-create " *cont-temp*"))
	  (set-buffer work-buf)
	  (erase-buffer)
	  (simap4-setup-proc-buffer-attributes)
	  (simap4-insert-bstr message)
	  (save-match-data
	    (goto-char (point-min))
	    (while (re-search-forward "[^\r]$" nil t)
	      (or (eobp) (insert "\r")))
	    (setq total (1- (point-max))))
	  (insert "\r\n")
	  (goto-char (point-min))
	  (insert
	   (concat "append " mailbox
		   (cond ((consp flags) (format " %s" flags))
			 ((and (stringp flags) (string-match "^(.*)$" flags))
			  (concat " " flags))
			 (t ""))
		   (if (stringp date-time)
		       (concat " " date-time)
		     "")
		   " {" (int-to-string total) "}\r\n"))
	  (setq command
		(simap4-make-bstr (current-buffer) (point-min) (point-max)))
	  (setq job (simap4-exec-command obj command))
	  (if (and job (simap4-result-ok-p job))
	      (simap4-parse-untagged-response obj job)))
      (kill-buffer work-buf))))
  
;;------------------------------------------------------------------------
;; 6.4.    Client Commands - Selected State
;; 6.4.1.  CHECK Command
(defun simap4-check (obj)
  "OBJ is an simap4 object. Send CHECK IMAP4 Client command.
Returns non-nil if tagged response is OK. Otherwise nil.

No specific responses are expected for this command.
"
  (simap4-with-retries obj
    (let ((job (simap4-exec-command obj "check")))
      (if (and job (simap4-result-ok-p job))
	  (simap4-parse-untagged-response obj job)))))

;; 6.4.2.  CLOSE Command
(defun simap4-close (obj)
  "OBJ is an simap4 object. Send CLOSE IMAP4 Client command.
Returns non-nil if tagged response is OK. Otherwise nil.

No specific responses are expected for this command.
"
  (simap4-obj-check obj)
  (let ((job (simap4-exec-command obj "close")))
    (if (and job (simap4-result-ok-p job))
	(progn
	  (simap4-parse-untagged-response obj job)
	  (simap4-set-state obj 'authenticated)
	  (simap4-set-selected-mailbox obj nil)))))

;; 6.4.3.  EXPUNGE Command
(defun simap4-expunge (obj)
  "OBJ is an simap4 object. Send EXPUNGE IMAP4 Client command.
Returns non-nil if tagged response is OK. Otherwise nil.

The following function(s) should be used to retrieve parsed output.
simap4-job-get-resp-expunge (see `simap4-parse-resp-expunge' for format.)
"
  (simap4-obj-check obj)
  (let ((job (simap4-exec-command obj "expunge")))
    (if (and job (simap4-result-ok-p job))
	(simap4-parse-untagged-response obj job))))

;; 6.4.4.  SEARCH Command
(defun simap4-search (obj criteria &optional uids)
  "OBJ is an simap4 object. Send SEARCH IMAP4 Client command.
Returns non-nil if tagged response is OK. Otherwise nil.

CRITERIA is a string that lists the following search keys separated by
space character.

      <message set>
      ALL
      ANSWERED
      BCC <string>
      BEFORE <date>
      BODY <string>
      CC <string>
      DELETED
      DRAFT
      FLAGGED
      FROM <string>
      HEADER <field-name> <string>
      KEYWORD <flag>
      LARGER <n>
      NEW
      NOT <search-key>
      OLD
      ON <date>
      OR <search-key1> <search-key2>
      RECENT
      SEEN
      SENTBEFORE <date>
      SENTON <date>
      SENTSINCE <date>
      SINCE <date>
      SMALLER <n>
      SUBJECT <string>
      TEXT <string>
      TO <string>
      UID <message set>
      UNANSWERED
      UNDELETED
      UNDRAFT
      UNFLAGGED
      UNKEYWORD <flag>
      UNSEEN

Please refer to RFC2060, for details about these search keys.

If optional UIDS is non-nil, command will be sent using UID command
and unique identifiers will be returned in SEARCH response.

The following function(s) should be used to retrieve parsed output.
simap4-job-get-resp-search (see `simap4-parse-resp-search' for format.)
"
  (simap4-with-retries obj
    (if uids (setq uids "uid ") (setq uids ""))
    (let ((job (simap4-exec-command
		obj (format "%ssearch %s" uids criteria))))
      (if (and job (simap4-result-ok-p job))
	  (simap4-parse-untagged-response obj job)))))

;; 6.4.5.  FETCH Command
(defun simap4-fetch (obj seqnums data-items &optional uids)
  "OBJ is an simap4 object. Send FETCH IMAP4 Client command.
Returns non-nil if tagged response is OK. Otherwise nil.

SEQNUMS is an integer, or a string in form of IMAP4 message set
expression.  DATA-ITEMS is a string that lists IMAP4 specified data
items to fetch.

The currently defined data items that can be fetched are:

      ALL
      BODY
      BODY[<section>]<<partial>>
      BODY.PEEK[<section>]<<partial>>
      BODYSTRUCTURE
      ENVELOPE
      FAST
      FLAGS
      FULL
      INTERNALDATE
      RFC822
      RFC822.HEADER
      RFC822.SIZE
      RFC822.TEXT
      UID

Please refer to RFC2060, for more details about these data items.

If optional UIDS is non-nil, command will be sent using UID command
and SEQNUMS is interpreted as unique identifier by the server.

The following function(s) should be used to retrieve parsed output.
simap4-job-get-resp-fetch (see `simap4-parse-resp-fetch' for format.)
"
  (simap4-with-retries obj
    (if uids (setq uids "uid ") (setq uids ""))
    (let ((job (simap4-exec-command
		obj (format "%sfetch %s (%s)" uids seqnums data-items))))
      (if (and job (simap4-result-ok-p job))
	  (simap4-parse-untagged-response obj job)))))

;; 6.4.6.  STORE Command
(defun simap4-store (obj seqnums data-items &optional uids bg-callback)
  "OBJ is an simap4 object. Send STORE IMAP4 Client command.
Returns non-nil if tagged response is OK. Otherwise nil.

SEQNUMS is an integer, or a string in form of IMAP4 message set
expression.  DATA-ITEMS is a string that lists IMAP4 specified data
items to store.

The currently defined data items that can be stored are:

      FLAGS <flag list>
      FLAGS.SILENT <flag list>
      +FLAGS <flag list>
      +FLAGS.SILENT <flag list>
      -FLAGS <flag list>
      -FLAGS.SILENT <flag list>

The system flags may be listed in <flag list> are:

      \\Seen \\Answered \\Flagged \\Deleted \\Draft \\Recent

Please refer to RFC2060 for more detail.

If optional UIDS is non-nil, command will be sent using UID command
and SEQNUMS is interpreted as unique identifier by the server.

If optional BG-CALLBACK is non-nil, command is pushed into background
job queue and will be executed in background mode. In this case, this
command will return immediately, and returns a job object that has
been created for the command. If BG-CALLBACK is a function, it will be
called automatically with argments OBJ and the job passed in whenever
the job is finished, terminated or canceled.

The following function(s) should be used to retrieve parsed output.
simap4-job-get-resp-fetch (see `simap4-parse-resp-fetch' for format.)
"
  (simap4-obj-check obj)
  (if uids (setq uids "uid ") (setq uids ""))
  (if bg-callback
      (simap4-exec-background-command
       obj bg-callback (format "%sstore %s %s" uids seqnums data-items))
    (let ((job (simap4-exec-command
		obj (format "%sstore %s %s" uids seqnums data-items))))
      (if (and job (simap4-result-ok-p job))
	  (simap4-parse-untagged-response obj job)))))

;; 6.4.7.  COPY Command
(defun simap4-copy (obj seqnums mailbox &optional uids)
  "OBJ is an simap4 object. Send COPY IMAP4 Client command.
Returns non-nil if tagged response is OK. Otherwise nil.

SEQNUMS is an integer, or a string in form of IMAP4 message set
expression.  MAILBOX is destination mailbox name. If optional UIDS is
non-nil, command will be sent using UID command and SEQNUMS is
interpreted as unique identifier by the server.

No specific responses are expected for this command.
"
  (simap4-obj-check obj)
  (if uids (setq uids "uid ") (setq uids ""))
  (let ((job (simap4-exec-command
	      obj (format "%scopy %s %s" uids seqnums mailbox))))
    (if (and job (simap4-result-ok-p job))
	(simap4-parse-untagged-response obj job))))

;; 6.4.8.  UID Command
;; Command supported as "option" in context of COPY, FETCH, and STORE
;; commands.


;;------------------------------------------------------------------------
;; Untagged server responses parser
(defun simap4-parse-untagged-response (obj job)
  "Always returns t. OBJ is an simap4 object and JOB is an simap4 job object."
  (save-excursion
    (simap4-job-set-resp-ok job nil)
    (simap4-job-set-resp-no job nil)
    (simap4-job-set-resp-bad job nil)
    (simap4-job-set-resp-preauth job nil)
    (simap4-job-set-resp-bye job nil)
    (simap4-job-set-resp-capability job nil)
    (simap4-job-set-resp-list job nil)
    (simap4-job-set-resp-lsub job nil)
    (simap4-job-set-resp-status job nil)
    (simap4-job-set-resp-search job nil)
    (simap4-job-set-resp-flags job nil)
    (simap4-job-set-resp-exists job nil)
    (simap4-job-set-resp-recent job nil)
    (simap4-job-set-resp-expunge job nil)
    (simap4-job-set-resp-fetch job nil)
    (set-buffer (simap4-get-buffer obj))
    (let ((case-fold-search t))
      (goto-char (simap4-job-get-start-point job))
      (while (< (point) (simap4-job-get-end-point job))
	;; Look for all possible responses even though it is not
	;; expected by the given command. This is to avoid a parser
	;; out of sync situation. This is also required by RFC-2060.
	;; (It section 7, it says: "The client MUST be prepared to
	;; accept any response at all times.")
	;; simap4-parse-resp-unexpected will be called last to try fix
	;; the out of sync situation, but this really should not
	;; occur. Parser functions are ordered so that a frequent
	;; response is parsed first.
	(or (simap4-parse-resp-exists job)
	    (simap4-parse-resp-recent job)
	    (simap4-parse-resp-ok job)
	    (simap4-parse-resp-no job)
	    (simap4-parse-resp-expunge job)
	    (simap4-parse-resp-status job)
	    (simap4-parse-resp-search job)
	    (simap4-parse-resp-fetch job)
	    (simap4-parse-resp-flags job)
	    (simap4-parse-resp-capability job)
	    (simap4-parse-resp-list job)
	    (simap4-parse-resp-lsub job)
	    (simap4-parse-resp-bad job)
	    (simap4-parse-resp-preauth job)
	    (simap4-parse-resp-unexpected job)))))
  ;; always return t
  t)

;; All response parser returns non-nil value if parsed successfully.
;; It assumes that current buffer is set to an simap4 process buffer
;; which corresponds to respective simap4 object, and cursor point in
;; the buffer is at the beginning of next response to be parsed. If
;; parsed successfully, point is advanced to the beginning of the next
;; response. case-fold-search must be set to t for the process buffer
;; prior to calling these parser functions.

;; 7.1.    Server Responses - Status Responses
;; 7.1.1.  OK Response
(defun simap4-parse-resp-ok (job)
  "Returns non-nil if parsed successfully. JOB is an simap4 job object."
  (if (looking-at "^* ok \\(\\[\\(.+\\)\\]\\)?[ \t]*\\([^\r\n]*\\)\r?$")
      (let ((res (simap4-job-get-resp-ok job))
	    (parsed-rcode (simap4-parse-resp-code job (match-beginning 2))))
	(if parsed-rcode
	    (setq res
		  (nconc res
			 (list
			  (nconc parsed-rcode
				 (list (match-string 3)))))))
	(if res (simap4-job-set-resp-ok job res))
	(forward-line 1) t)))

;; 7.1.2.  NO Response
(defun simap4-parse-resp-no (job)
  "Returns non-nil if parsed successfully. JOB is an simap4 job object."
  (if (looking-at "^* no \\(\\[\\(.+\\)\\]\\)?[ \t]*\\([^\r\n]*\\)\r?$")
      (let ((res (simap4-job-get-resp-ok job))
	    (parsed-rcode (simap4-parse-resp-code job (match-beginning 2))))
	(if parsed-rcode
	    (setq res
		  (nconc res
			 (list
			  (nconc parsed-rcode
				 (list (match-string 3)))))))
	(if res (simap4-job-set-resp-no job res))
	(forward-line 1) t)))

;; 7.1.3.  BAD Response
(defun simap4-parse-resp-bad (job)
  "Returns non-nil if parsed successfully. JOB is an simap4 job object."
  (if (looking-at "^* bad \\(\\[\\(.+\\)\\]\\)?[ \t]*\\([^\r\n]*\\)\r?$")
      (let ((res (simap4-job-get-resp-ok job))
	    (parsed-rcode (simap4-parse-resp-code job (match-beginning 2))))
	(if parsed-rcode
	    (setq res
		  (nconc res
			 (list
			  (nconc parsed-rcode
				 (list (match-string 3)))))))
	(if res (simap4-job-set-resp-bad job res))
	(forward-line 1) t)))

;; 7.1.4.  PREAUTH Response
(defun simap4-parse-resp-preauth (job)
  "Returns non-nil if parsed successfully. JOB is an simap4 job object."
  (if (looking-at "^* preauth \\(\\[\\(.+\\)\\]\\)?[ \t]*\\([^\r\n]*\\)\r?$")
      (let ((res (simap4-job-get-resp-ok job))
	    (parsed-rcode (simap4-parse-resp-code job (match-beginning 2))))
	(if parsed-rcode
	    (setq res
		  (nconc res
			 (list
			  (nconc parsed-rcode
				 (list (match-string 3)))))))
	(if res (simap4-job-set-resp-preauth job res))
	(forward-line 1) t)))

;; 7.1.5.  BYE Response
(defun simap4-parse-resp-bye (job)
  "Returns non-nil if parsed successfully. JOB is an simap4 job object."
  (if (looking-at "^* bye \\(\\[\\(.+\\)\\]\\)?[ \t]*\\([^\r\n]*\\)\r?$")
      (let ((res (simap4-job-get-resp-ok job))
	    (parsed-rcode (simap4-parse-resp-code job (match-beginning 2))))
	(if parsed-rcode
	    (setq res
		  (nconc res
			 (list
			  (nconc parsed-rcode
				 (list (match-string 3)))))))
	(if res (simap4-job-set-resp-bye job res))
	(forward-line 1) t)))


;; 7.2.    Server Responses - Server and Mailbox Status
;; 7.2.1.  CAPABILITY Response
(defun simap4-parse-resp-capability (job)
  "Returns non-nil if parsed successfully. JOB is an simap4 job object."
  (if (looking-at "^* capability \\([^\r\n]*\\)\r?$")
      (let ((res (simap4-job-get-resp-capability job)))
	(setq res (nconc res (list (simap4-read
				    (concat "(" (match-string 1) ")")))))
	(if res (simap4-job-set-resp-capability job res))
	(forward-line 1) t)))

;; 7.2.2.  LIST Response
(defun simap4-parse-resp-list (job)
  "Returns non-nil if parsed successfully. JOB is an simap4 job object.
resp-list attribute is set to the parsed output data as follows:

 (((<flag1> <flag2> ...) <delimiter-code> <mailbox-name>) ...)
"
    (if (looking-at
	 "^\* list \(\\(.*\\)\) \\(\".\"\\|nil\\) \\([^\r\n]*\\)\r?\n")
	(let ((flags (match-string 1))
	      (delimiter (match-string 2))
	      (name (progn (goto-char (match-beginning 3))
			   (simap4-extract-forward-mailbox)))
	      (res (simap4-job-get-resp-list job)))
	  (setq flags (simap4-extract-flags flags))
	  (if (string= delimiter "NIL") (setq delimiter nil)
	    (if (= (length delimiter) 3) (setq delimiter (elt delimiter 1))
	      delimiter ?/))
	  (setq res (nconc res (list (list flags delimiter name))))
	  (if res (simap4-job-set-resp-list job res))
	  (forward-line 1) t)))

;; 7.2.3.  LSUB Response
(defun simap4-parse-resp-lsub (job)
  "Returns non-nil if parsed successfully. JOB is an simap4 job object.
resp-lsub attribute is set to the parsed output data as follows:

 (((<flag1> <flag2> ...) <delimiter-code> <mailbox-name>) ...)
"
  (if (looking-at
       "^\* lsub \(\\(.*\\)\) \\(\".\"\\|nil\\) \\([^\r\n]*\\)\r?\n")
      (let ((flags (simap4-extract-flags (match-string 1)))
	    (delimiter (match-string 2))
	    (name (progn (goto-char (match-beginning 3))
			 (simap4-extract-forward-mailbox)))
	    (res (simap4-job-get-resp-lsub job)))
	(if (string= delimiter "NIL") (setq delimiter nil)
	  (if (= (length delimiter) 3) (setq delimiter (elt delimiter 1))
	    delimiter ?/))
	(setq res (nconc res (list (list flags delimiter name))))
	(if res (simap4-job-set-resp-lsub job res))
	(forward-line 1) t)))

;; 7.2.4   STATUS Response
(defun simap4-parse-resp-status (job)
  "Returns non-nil if parsed successfully. JOB is an simap4 job object."
  (if (looking-at "^* status \\([^\r\n]*\\)\r?$")
      (let ((res (simap4-job-get-resp-status job)) st)
	(skip-chars-forward "[ \t]")
	(while
	    (setq
	     st
	     (cond
	      ((looking-at ")")
	       nil)
	      ((looking-at "MESSAGES \\([0-9]+\\)")
	       (goto-char (match-end 0))
	       (cons 'messages (string-to-int (match-string 1))))
	      ((looking-at "RECENT \\([0-9]+\\)")
	       (goto-char (match-end 0))
	       (nconc (cons 'recent (string-to-int (match-string 1)))))
	      ((looking-at "UIDNEXT \\([0-9]+\\)")
	       (goto-char (match-end 0))
	       (cons 'uidnext (string-to-int (match-string 1))))
	      ((looking-at "UIDVALIDITY \\([0-9]+\\)")
	       (goto-char (match-end 0))
	       (cons 'uidvalidity (match-string 1)))
	      ((looking-at "UNSEEN \\([0-9]+\\)")
	       (goto-char (match-end 0))
	       (cons 'unseen (string-to-int (match-string 1))))
	      (t nil)))
	  (setq res (nconc st res))
	  (skip-chars-forward "[ \t]"))
	(if res (simap4-job-set-resp-status job res))
	(forward-line 1) t)))

;; 7.2.5.  SEARCH Response
(defun simap4-parse-resp-search (job)
  "Returns non-nil if parsed successfully. JOB is an simap4 job object."
  (if (looking-at "^\* search\\( [^\r\n]*\\|\\)\r$")
      (let ((res (simap4-job-get-resp-search job)))
	(setq res (nconc res (simap4-read
			      (concat "(" (match-string 1) ")"))))
	(if res (simap4-job-set-resp-search job res))
	(forward-line 1) t)))

;; 7.2.6.  FLAGS Response
(defun simap4-parse-resp-flags (job)
  "Returns non-nil if parsed successfully. JOB is an simap4 job object."
  (if (looking-at "^\* flags \(\\([^\r\n]*\\)\)[ \t]*\r$")
      (let ((res (simap4-job-get-resp-flags job)))
	(setq res (nconc res (simap4-extract-flags (match-string 1))))
	(if res (simap4-job-set-resp-flags job res))
	(forward-line 1) t)))

;; 7.3.    Server Responses - Mailbox Size
;; 7.3.1.  EXISTS Response
(defun simap4-parse-resp-exists (job)
  "Returns non-nil if parsed successfully. JOB is an simap4 job object."
  (if (looking-at "^* \\([0-9]+\\) +exists")
      (progn
	(simap4-job-set-resp-exists job (string-to-int (match-string 1)))
	(forward-line 1) t)))

;; 7.3.2.  RECENT Response
(defun simap4-parse-resp-recent (job)
  "Returns non-nil if parsed successfully. JOB is an simap4 job object."
  (if (looking-at "^* \\([0-9]+\\) +recent")
      (progn
	(simap4-job-set-resp-recent job (string-to-int (match-string 1)))
	(forward-line 1) t)))

;; 7.4.    Server Responses - Message Status
;; 7.4.1.  EXPUNGE Response
(defun simap4-parse-resp-expunge (job)
  "Returns non-nil if parsed successfully. JOB is an simap4 job object."
  (if (looking-at "^* \\([0-9]+\\) +expunge")
      (let ((res (nconc (simap4-job-get-resp-expunge job)
			(list (string-to-int (match-string 1))))))
	(if res (simap4-job-set-resp-expunge job res))
	(forward-line 1) t)))

;; 7.4.2.  FETCH Response
(defun simap4-parse-resp-fetch (job)
  "Returns non-nil if parsed successfully. JOB is an simap4 job object."
  (if (looking-at "^\* [0-9]+ fetch (")
      (let ((prev (simap4-job-get-resp-fetch job)) res ret)
	(goto-char (match-end 0))
	(while (setq ret (simap4-parse-resp-fetch1))
	  (setq res (cons ret res)))
	(if res (simap4-job-set-resp-fetch job (nconc prev (list res)))))))

(defun simap4-parse-resp-fetch1 ()
  (skip-chars-forward "[ \t\n\r]")
  (let (bstr res)
    (cond
     ((looking-at ")")
      (forward-line 1)
      nil)
     ((looking-at "BODYSTRUCTURE ")
      (goto-char (match-end 0))
      (setq res (simap4-extract-forward-paren-list))
      (if res
	  (cons 'bodystructure res)
	(forward-line 1)
	nil))
     ((looking-at "\\(BODY\\[.*\\]\\(<\\([0-9.]+\\)>\\)?\\) ")
      (goto-char (match-end 0))
      (setq bstr (simap4-extract-forward-string))
      (if (null bstr)
	  (progn (forward-line 1) nil)
	(cons 'body+ (cons (match-string 1) bstr))))
     ((looking-at "BODY")
      (goto-char (match-end 0))
      (setq res (simap4-extract-forward-paren-list))
      (if res
	  (cons 'body res)
	(forward-line 1)
	nil))
     ((looking-at "ENVELOPE ")
      (goto-char (match-end 0))
      (setq res (simap4-extract-forward-paren-list))
      (if res
	  (cons 'envelope res)
	(forward-line 1)
	nil))
     ((looking-at "FLAGS ")
      (goto-char (match-end 0))
      (let ((beg (point)))
	(if (condition-case nil (not (forward-sexp 1)) (error nil))
	    (cons 'flags
		  (simap4-extract-flags (buffer-substring beg (point))))
	  (forward-line 1)
	  nil)))
     ((looking-at "INTERNALDATE ")
      (goto-char (match-end 0))
      (setq bstr (simap4-extract-forward-string))
      (if (null bstr)
	  (progn (forward-line 1) nil)
	(cons 'internaldate bstr)))
     ((looking-at "RFC822\.HEADER ")
      (goto-char (match-end 0))
      (setq bstr (simap4-extract-forward-string))
      (if (null bstr)
	  (progn (forward-line 1) nil)
	(cons 'rfc822-header bstr)))
     ((looking-at "RFC822\.SIZE \\([0-9]+\\)")
      (goto-char (match-end 0))
      (cons 'rfc822-size (string-to-int (match-string 1))))
     ((looking-at "RFC822\.TEXT ")
      (goto-char (match-end 0))
      (setq bstr (simap4-extract-forward-string))
      (if (null bstr)
	  (progn (forward-line 1) nil)
	(cons 'rfc822-text bstr)))
     ((looking-at "RFC822")
      (goto-char (match-end 0))
      (setq bstr (simap4-extract-forward-string))
      (if (null bstr)
	  (progn (forward-line 1) nil)
	(cons 'rfc822 bstr)))
     ((looking-at "UID \\([0-9]+\\)")
      (goto-char (match-end 0))
      (cons 'uid (string-to-int (match-string 1))))
     (t
      (forward-line 1)
      nil))))

;; Unexpected response/unknown format line. For best effort parsing,
;; find the next response line by looking for "^*"
(defun simap4-parse-resp-unexpected (job)
  "Returns always t. JOB is an simap4 job object. For best effort
parsing, find the next response line by looking for \"^*\""
  (let ((start (point)))
    (forward-line 1)
    (while (and (< (point) (simap4-job-get-end-point job))
		(not (eq (char-after (point)) ?*)))
      (forward-line 1))
    (simap4-debug (simap4-job-get-parent job)
		  "**[Unexpected response (or parser out of sync)]\n%s\n%s"
		  (buffer-substring start (point))
		  "**[Unexpected response]"))
  t)

;; Response code parser for OK, NO, BAD, PREAUTH, BYE Responses.
(defun simap4-parse-resp-code (job pos)
  (save-match-data
    (save-excursion
      (let (resp-code-rec)
	(goto-char pos)
	(if (eq (char-after (point)) 91) ; '['
	    (forward-char 1))
	(cond
	 ((looking-at "ALERT \\(.*\\)\\]$")
	  (cons 'alert (list (match-string 1))))
	 ((looking-at "NEWNAME ")
	  (let (beg old-name new-name)
	    (goto-char (match-end 0))
	    (setq old-name (simap4-extract-forward-mailbox))
	    (setq new-name (simap4-extract-forward-mailbox))
	    (if (and (null old-name) (null new-name))
		(cons 'newname (list (list old-name new-name)))
	      (looking-at ".*$")
	      (simap4-debug (simap4-job-get-parent job)
			    "**[parse-resp-code newname error [%s]]"
			    (match-string 0))
	      (forward-line 1)
	      nil)))
	 ((looking-at "PARSE \\(.*\\)\\]")
          (cons 'parse (list (match-string 1))))
	 ((looking-at "PERMANENTFLAGS \\((.*)\\)")
	  (cons 'permanentflags
		(list (simap4-extract-flags (match-string 1)))))
	 ((looking-at "READ-ONLY")
	  '(read-only))
	 ((looking-at "READ-WRITE")
	  '(read-write))
	 ((looking-at "TRYCREATE")
	  '(trycreate))
	 ((looking-at "UIDVALIDITY \\([0-9]+\\)")
	  (cons 'uidvalidity (list (match-string 1))))
	 ((looking-at "UNSEEN \\([0-9]+\\)")
	  (cons 'unseen (list (string-to-int (match-string 1)))))
	 ((looking-at "UIDNEXT \\([0-9]+\\)")
	  (cons 'uidnext (list (string-to-int (match-string 1)))))
	 ((looking-at ".*$")
	  (simap4-debug (simap4-job-get-parent job)
			"**[parse-resp-code match error [%s]]"
			(match-string 0))
	  (forward-line 1)
	  nil))))))

;;------------------------------------------------------------------------
;; extract-forward functions
(defun simap4-extract-forward-string ()
  "Returns simap4 bstr for rfc2060 string from the current point, and
move the point to the end of the string. Preceeding white spaces are
skipped. Returns nil if nothing happen. In this case, the point will
not be moved."
  (save-match-data
    (let ((current (point)) beg)
      (skip-chars-forward "[ \t\r\n]")
      (setq beg (point))
      (cond
       ;; string ::= quoted / literal
       ((looking-at "\"") ; quoted
	(if (condition-case nil (not (forward-sexp 1)) (error nil))
	    (simap4-read (buffer-substring beg (point)))
	  (goto-char current)
	  nil))
       ((looking-at "{\\([0-9]+\\)}\r\n") ; literal
	(goto-char (match-end 0))
	(let ((bstr (make-vector 3 nil)))
	  (aset bstr 0 (current-buffer))
	  (aset bstr 1 (point))
	  (goto-char (+ (point) (string-to-int (match-string 1))))
	  (if (eobp)
	      (progn (goto-char current) nil)
	    (aset bstr 2 (point))
	    bstr)))
       (t
	(goto-char current)
	nil)))))

(defconst simap4-list_mailbox-atom-regexp "[^(){ \x0-\x1f\x7f\"\\]+")

(defun simap4-extract-forward-mailbox ()
  (save-match-data
    (let ((case-fold-search t) (current (point)) beg res)
      (skip-chars-forward "[ \t\r\n]")
      (setq beg (point))
      (setq res (simap4-extract-forward-string))
      (if res
	  (progn
	    (setq res (simap4-bstr-to-string res))
	    (if (string-match "^inbox$" res)
		"INBOX"
	      res))
	(if (looking-at simap4-list_mailbox-atom-regexp)
	    (progn
	      (goto-char (match-end 0))
	      (match-string 0))
	  (goto-char current)
	  nil)))))

(defun simap4-extract-forward-paren-list ()
  "Recursively parse a parenthesized list of nstring from the current
point. return a parsed lisp expression with nstring represented as
simap4 bstr or nil. Any error during parsing will result in nil."
  (catch 'parse-error
    (let ((case-fold-search t))
      (skip-chars-forward "[ \t\r\n]")
      (if (looking-at "(")
	  (progn
	    (forward-char 1)
	    (skip-chars-forward "[ \t\r\n]")
	    (simap4-extract-forward-paren-list1))))))

(defun simap4-extract-forward-paren-list1 ()
  (let (res elem)
    (while (not (looking-at ")"))
      (cond ((looking-at "(")
	     (forward-char 1)
	     (setq res (cons (simap4-extract-forward-paren-list1) res)))
	    ((looking-at "[0-9]+")
	     (goto-char (match-end 0))
	     (setq res (cons (string-to-int (match-string 0)) res)))
	    ((looking-at "nil\\>")
	     (forward-char 3)
	     (setq res (cons nil res)))
	    (t
	     (setq elem (simap4-extract-forward-string))
	     (if elem
		 (setq res (cons elem res))
	       (throw 'parse-error nil))))
      (skip-chars-forward "[ \t\r\n]"))
    (forward-char 1)
    (nreverse res)))

(defun simap4-extract-flags (flag-list)
  "Extract flags in FLAG-LIST and returns a list of flag string.  for
example, FLAG-LIST of \"\\\\Recent \\\\Seen\" will be converted to
(\"\\\\Recent\" \"\\\\Seen\")"
  (let ((case-fold-search t) (pos 0) (res nil))
    (while (string-match "[^ \t\r\n()]+" flag-list pos)
      (setq res (cons (match-string 0 flag-list) res))
      (setq pos (match-end 0)))
    (nreverse res)))

(defun simap4-read (str)
  "Converts string in Emacs Lisp list expression using lisp read
function. Returns nil if string is malfunctioned lisp form without
flagging an error."
  (condition-case nil (read str)
    (error nil)))

;;------------------------------------------------------------------------
;; interface support functions

(defun simap4-bstr-to-string (bstr)
  (cond ((and (vectorp bstr) (buffer-live-p (get-buffer (elt bstr 0))))
	 (save-excursion
	   (set-buffer (elt bstr 0))
	   (buffer-substring (elt bstr 1) (elt bstr 2))))
 	((stringp bstr) bstr)))

;; simap4 handles string or an array in form of [buffer start end] as
;; buffer string.
(defun simap4-insert-bstr (bstr)
  (cond ((and (vectorp bstr) (buffer-live-p (get-buffer (elt bstr 0))))
	 (insert-buffer-substring (elt bstr 0) (elt bstr 1) (elt bstr 2)))
 	((stringp bstr)
 	 (insert bstr))))

(defun simap4-make-bstr (buffer start end)
  (if (not (buffer-live-p buffer))
      (error
       (format "simap4-make-bstr: invalid BUFFER argument. [%s]" buffer)))
  (if (not (integer-or-marker-p start))
      (error (format "simap4-make-bstr: invalid START argument. [%s]" start)))
  (if (not (integer-or-marker-p start))
      (error (format "simap4-make-bstr: invalid END argument. [%s]" end)))
  (let ((bstr (make-vector 3 buffer)))
    (aset bstr 1 start)
    (aset bstr 2 end)
    bstr))

;; it is the application's responsibility to clean up tcp buffer as
;; necessary. applications should discard last command result and all
;; imap string data when cleans up the buffer (because the point
;; information for buffer strings is no longer valid.) skip if job is
;; in progress.
(defvar *simap4-trim-buffer-threshold 50000)
(defvar *simap4-trim-buffer-amount 40000)
(defun simap4-trim-buffer (obj)
  (if (not (simap4-get-current-job obj))
      (save-excursion
	(set-buffer (simap4-get-buffer obj))
	(if (> (process-mark (simap4-get-proc obj))
	       *simap4-trim-buffer-threshold)
	    (progn
	      (goto-char *simap4-trim-buffer-amount)
	      (forward-line 1)
	      (if (> (point) *simap4-trim-buffer-threshold)
		  (forward-line -1))
	      (delete-region (point-min) (point))))))
  t)

;;------------------------------------------------------------------------
;; throughput calculation and progress display
(defun simap4-clear-throutput (obj)
  (simap4-set-total-bytes obj 0)
  (simap4-set-total-period obj 0)
  (simap4-set-valid-period obj 0)
  (simap4-set-invalid-period obj 0)
  (simap4-set-throughput obj 0)
  (simap4-set-max-throughput obj 0))

(defun simap4-reset-progress (obj expected-size)
  (simap4-set-expected-size obj expected-size)
  (simap4-set-current-size obj 0)
  (simap4-set-progress obj 0))

(defun simap4-update-progress (obj bytes-diff)
  (if (and (numberp (simap4-get-expected-size obj))
	   (> (simap4-get-expected-size obj) 0))
      (progn
	(if (not (numberp (simap4-get-current-size obj)))
	    (simap4-set-current-size obj bytes-diff)
	  (if (< (simap4-get-progress obj) 100)
	      (simap4-set-current-size
	       obj (+ (simap4-get-current-size obj) bytes-diff))))
	(simap4-set-progress obj
			     (/ (* (simap4-get-current-size obj) 100)
				(simap4-get-expected-size obj)))
	(if (> (simap4-get-progress obj) 100)
	    (simap4-set-progress obj 100)))))

(defun simap4-finish-progress (obj)
  (simap4-set-current-size obj (simap4-get-expected-size obj))
  (simap4-set-progress obj 100))

(defun simap4-update-throughput (obj bytes period-ms)
  (if (or (< bytes 0) (< period-ms 0))
      (error (format "bytes:%d ms:%d" bytes period-ms)))
  (let ((throughput (/ (* (float bytes) 1000)
		       (if (> period-ms 0) period-ms 1))))
    (simap4-set-total-bytes obj (+ (simap4-get-total-bytes obj) bytes))
    (simap4-set-total-period obj (+ (simap4-get-total-period obj) period-ms))
    (if (> throughput 200)
	(progn
	  (simap4-set-valid-period
	   obj (+ (simap4-get-valid-period obj) period-ms))
	  (simap4-set-invalid-period obj 0)
	  (if (> (simap4-get-valid-period obj) 500)
	      (progn
		(if (> throughput (simap4-get-max-throughput obj))
		    (simap4-set-max-throughput obj throughput))
		(simap4-set-throughput
		 obj (/ (+ (simap4-get-throughput obj) throughput) 2)))))
      (simap4-set-invalid-period
       obj (+ (simap4-get-invalid-period obj) period-ms))
      (simap4-set-valid-period obj 0)
      (if (> (simap4-get-invalid-period obj) 2000)
	  (simap4-set-throughput obj 0)))))

(defun simap4-msec-elapsed (start-time end-time)
  (let ((sec (- (+ (elt end-time 1)
		   (* 65536 (- (elt end-time 0) (elt start-time 0))))
		(elt start-time 1)))
	(msec (/ (- (+ (elt end-time 2) 1000000) (elt start-time 2)) 1000)))
    (- (+ (* sec 1000) msec) 1000)))

(defconst *simap4-display-list ["|" "/" "-" "\\"])
(defvar *simap4-display-count 0)
(defvar simap4-display-data-transfer "")
(or global-mode-string (setq global-mode-string '("")))
(if (not (member 'simap4-display-data-transfer global-mode-string))
    (setq global-mode-string
	  (append global-mode-string '(simap4-display-data-transfer))))

(defun simap4-display-progress (obj &optional status)
  (setq *simap4-display-count
	(% (1+ *simap4-display-count) (length *simap4-display-list)))
  (let ((throughput (simap4-get-throughput obj)) tpstr)
    (let ((base (simap4-get-expected-size obj))
	  (progress (simap4-get-progress obj)))
      (if (and (numberp base) (> base 0))
	  (setq tpstr (format "[%dKB/%d%%:%.1fKB/s]"
			      (/ base 1024) progress (/ throughput 1000)))
	(setq tpstr (format "[%.1fKB/s]" (/ throughput 1000)))))
    (setq simap4-display-data-transfer
	  (concat tpstr
		  (or status
		      (elt *simap4-display-list *simap4-display-count))))
    (force-mode-line-update)
    (if (null (sit-for 0)) (discard-input)))
  (simap4-start-progress-undisplay-timer))

(defun simap4-display-no-progress (obj)
  (simap4-display-progress obj " "))

(defun simap4-display-progress-stalled (obj)
  (simap4-display-progress obj "*"))

(defun simap4-display-progress-quit-by-user ()
  (setq simap4-display-data-transfer "[*QUIT*]")
  (simap4-start-progress-undisplay-timer))

(defvar *simap4-progress-display-timer nil)

(defun simap4-undisplay-progress ()
  (setq *simap4-progress-display-timer nil)
  (setq simap4-display-data-transfer nil)
  (force-mode-line-update))

(defun simap4-start-progress-undisplay-timer ()
  (if *simap4-progress-display-timer
      (cancel-timer *simap4-progress-display-timer))
  (setq *simap4-progress-display-timer
	(run-with-timer simap4-progress-display-time nil
			'simap4-undisplay-progress)))

;;------------------------------------------------------------------------
;; simap4 jobs for imap4 protocol input and output.
(defvar simap4-command-sequence-num 0)
(defun simap4-job-create (obj)
  (let* ((seq simap4-command-sequence-num)
	 (tag (format "N%08d" seq))
	 (job (intern (concat "simap4-job:" tag))))
    (setq simap4-command-sequence-num (1+ simap4-command-sequence-num))
    (simap4-job-set-tag job tag)
    (simap4-job-set-parent job obj)
    job))

(defun simap4-job-delete (job)
  (unintern job))

(defun simap4-job-start (obj job command)
  (simap4-job-set-command-string job command)
  (simap4-job-start1 obj job))

(defun simap4-send-string (obj job string)
  (save-excursion
    (set-buffer (simap4-get-buffer obj))
    (cond ((string-match
	    "^\\(N[0-9:]+ login\\) \\(.*\\) \\(.*\\)\r\n" string)
	   (simap4-debug
	    obj "->[%s %s %s]" (match-string 1 string) (match-string 2 string)
	    (make-string (length (match-string 3 string)) ?*)))
	  ((string-match "^\\(.*\\)\r\n" string)
	   (simap4-debug obj "->[%s]" (match-string 1 string)))
	  (t
	   (simap4-debug obj "->[%s]" string)))
    (goto-char (process-mark (simap4-get-proc obj)))
    (simap4-job-set-start-point job (point))
    (simap4-job-set-start-time job (current-time))
    (simap4-job-set-checkpoint job (simap4-job-get-start-point job))
    (simap4-job-set-checktime job (simap4-job-get-start-time job)))
  (condition-case nil
      (progn
	(process-send-string (simap4-get-proc obj) string)
	t)
    (error
     (if (not (eq (process-status (simap4-get-proc obj)) 'open))
	 (simap4-set-state obj 'disconnected))
     nil)))

(defun simap4-send-region (obj job start end)
  (save-excursion
    (set-buffer (simap4-get-buffer obj))
    (simap4-debug obj "->[REGION<%s,%s>]" start end)
    (goto-char (process-mark (simap4-get-proc obj)))
    (simap4-job-set-start-point job (point))
    (simap4-job-set-start-time job (current-time))
    (simap4-job-set-checkpoint job (simap4-job-get-start-point job))
    (simap4-job-set-checktime job (simap4-job-get-start-time job)))
  (condition-case nil
      (progn
	(process-send-region (simap4-get-proc obj) start end)
	t)
    (error
     (if (not (eq (process-status (simap4-get-proc obj)) 'open))
	 (simap4-set-state obj 'disconnected))
     nil)))

(defun simap4-job-start1 (obj job &optional continue-p)
  (let ((command (simap4-job-get-command-string job)))
    (save-excursion
      (simap4-job-set-result-code job "NOTCOMPLETED")
      (if (stringp command)
	  (simap4-send-string
	   obj job (format "%s %s\r\n" (simap4-job-get-tag job) command))
	;; bstr case
	(set-buffer (aref command 0))
	(goto-char (aref command 1))
	(forward-line 1)
	(if continue-p
	    ;; this comes 2nd. Send the rest data
	    (simap4-send-region obj job (point) (aref command 2))
	  ;; this comes 1st. Send the first line only with tag
	  (simap4-send-string obj job
			      (format "%s " (simap4-job-get-tag job)))
	  (simap4-send-region obj job (aref command 1) (point)))))
    (if (eq (simap4-get-state obj) 'disconnected)
	'disconnected
      t)))

(defun simap4-job-wait (obj job &optional nonblock-p)
  (if (eq (simap4-get-state obj) 'disconnected)
      'disconnected
    (save-excursion
      (let ((cont-p (vectorp (simap4-job-get-command-string job))) res)
	(setq res (simap4-job-wait1 obj job nonblock-p cont-p))
	(if (eq res 'continue)
	    (progn
	      (simap4-job-start1 obj job 'continue)
	      (setq res (simap4-job-wait1 obj job nonblock-p cont-p))))
	res))))

(defun simap4-job-wait1 (obj job &optional nonblock-p cont-p)
  (catch 'wait-exception
    (if (eq (simap4-get-state obj) 'disconnected)
	(throw 'wait-exception 'disconnected))
    (let ((proc (simap4-get-proc obj))
	  (usec-wait 1000)
	  (count 0)
	  (start (simap4-job-get-start-point job))
	  (start-time (simap4-job-get-start-time job))
	  (checkpoint (simap4-job-get-checkpoint job))
	  (checkpoint-time (simap4-job-get-checktime job))
	  ;; command-timeout-tick is 250ms resolution, hardcoded.
	  (command-timeout-tick (* (simap4-get-command-timeout obj) 4))
	  (stall-count 0)
	  (resp-regexp
	   (if cont-p
	       (concat "^[^ \t]*\\(+\\|" (simap4-job-get-tag job) "\\)")
	     ;; junk tag workaround. a bug somewhere or server?
	     (concat "^[^ \t]*" (simap4-job-get-tag job))))
	  (resp-regexp2
	   (if cont-p
	       (concat "^\\(+\\|" (simap4-job-get-tag job) "\\)")
	     (concat "^" (simap4-job-get-tag job))))
	  (resp-regexp3
	   (concat "^[^ \t]*" (simap4-job-get-tag job)
		   " \\(ok\\|no\\|bad\\) "
		   "\\(\\[\\(.+\\)\\]\\)? ?\\([^\r\n]*\\)\r?$"))
	  end msec bytes res)
      (set-buffer (simap4-get-buffer obj))
      (goto-char (process-mark proc))
      (forward-line -1)
      (if (> start (point)) (goto-char start))
      (if (null nonblock-p)
	  (condition-case nil
	      (progn
		(while (not (looking-at resp-regexp))
		  (setq count (1+ count))
		  (sleep-for (/ usec-wait 1000000)
			     (/ (% usec-wait 1000000) 1000))
		  (cond ((<= count 5) (setq usec-wait (+ usec-wait 1000)))
			((<= count 10) (setq usec-wait (+ usec-wait 10000)))
			((= count 25) (setq usec-wait 250000)))
		  (if (not (eq (process-status proc) 'open))
		      (progn
			(simap4-set-state obj 'disconnected)
			(throw 'wait-exception 'disconnected)))
		  (goto-char (process-mark proc))
		  (setq msec
			(simap4-msec-elapsed checkpoint-time (current-time)))
		  ;; check if time elapsed for 250ms. protect time change in
		  ;; the middle of this loop.
		  (if (or (> msec 250) (< msec 0))
		      (progn
			(setq bytes (- (point) checkpoint))
			(cond
			 ((< bytes 0) ;; buffer cleared in the middle?
			  (error
			   "simap4: error due to buffer pointer problem"))
			 ((= bytes 0) ;; transfer stalled
			  (if (simap4-get-use-prog-disp obj)
			      (simap4-display-progress-stalled obj))
			  (if (> stall-count command-timeout-tick)
			      (progn
				(simap4-debug obj "**[Timeout]")
				(throw 'wait-exception 'command-timeout))
			    (setq stall-count (1+ stall-count))))
			 (t
			  (setq stall-count 0)
			  (simap4-update-throughput obj bytes msec)
			  (simap4-update-progress obj bytes)
			  ;; display progress
			  (if (simap4-get-use-prog-disp obj)
			      (simap4-display-progress obj))))
			(setq checkpoint (point))
			(setq checkpoint-time (current-time))))
		  (forward-line -1)
		  (if (> start (point)) (goto-char start)))
		(if (not (looking-at resp-regexp2))
		    (simap4-debug obj "**[Junk lable returned]"))
		;; remove display
		(if (simap4-get-use-prog-disp obj)
		    (simap4-display-no-progress obj)))
	    (quit
	     ;; quit by user. clean up the loop context.
	     (beep)
	     (simap4-display-progress-quit-by-user)
	     (delete-process (simap4-get-proc obj))
	     (simap4-set-state obj 'disconnected)
	     (throw 'wait-exception 'quit))))
      (if (let ((case-fold-search t)) (looking-at resp-regexp3))
	  (progn
	    (simap4-job-set-result-code job (downcase (match-string 1)))
	    (if (match-string 3)
		(simap4-job-set-result-resp-code
		 job (simap4-parse-resp-code job (match-beginning 3))))
	    (simap4-job-set-result-explanation job (match-string 4))
	    (setq end (point))
	    (simap4-job-set-end-point job end)

	    ;; update display
	    (setq msec (simap4-msec-elapsed checkpoint-time (current-time)))
	    (setq bytes (- (point) checkpoint))
	    (if (<= bytes 0)
		(setq bytes 0))
	    (simap4-update-throughput obj bytes msec)
	    (simap4-finish-progress obj)
	    (if (simap4-get-use-prog-disp obj)
		(simap4-display-no-progress obj))
	    
	    (goto-char (process-mark proc))
	    (simap4-debug obj "<-[%s done; output count = %d, timeout %d]"
			  (simap4-job-get-tag job) count usec-wait)
	    t)
	(if cont-p
	    'continue
	  ;; In non-blocking case, it expects a nil return
	  )))))

(defun simap4-exec-command1 (obj command)
  (let ((job (simap4-job-create obj)))
    (simap4-queue-job obj job nil command)
    (simap4-run-queue obj)
    (setq job (simap4-get-current-job obj))
    (while job
      (if (simap4-get-last-job obj)
	  (simap4-job-delete (simap4-get-last-job obj)))
      (simap4-job-wait obj job)
      (simap4-set-last-job obj job)
      (simap4-set-current-job obj nil)
      (simap4-run-queue obj)
      (setq job (simap4-get-current-job obj)))
    (simap4-get-last-job obj)))

(defun simap4-queue-job (obj job bg-callback command)
  (simap4-job-set-command-string job command)
  (simap4-job-set-callback job bg-callback)
  (simap4-set-job-queue obj (nconc (simap4-get-job-queue obj) (list job))))

(defun simap4-run-queue (obj)
  (if (null (simap4-get-current-job obj))
      (let* ((job-queue (simap4-get-job-queue obj))
	     (job (car job-queue)))
	(if job-queue
	    (save-excursion
	      (simap4-set-current-job obj job)
	      (simap4-set-job-queue obj (cdr job-queue))
	      (simap4-job-start1 obj job))))))

(defun simap4-run-checker (obj)
  (let ((checker-timer (simap4-get-checker-timer obj)))
    (if checker-timer (cancel-timer checker-timer))
    (simap4-background-checker obj)))

(defun simap4-background-checker (obj)
  (simap4-set-checker-timer obj nil)
  (let ((job (simap4-get-current-job obj)))
    (if job
	(if (simap4-job-wait obj job 'nonblock)
	    (progn
	      (simap4-set-last-job obj job)
	      (simap4-set-current-job obj nil)
	      (simap4-run-queue obj)))))
  (if (simap4-get-current-job obj)
      (simap4-set-checker-timer
       obj (run-with-timer simap4-background-check-time
			   nil 'simap4-background-checker obj))))

(defun simap4-exec-background-command (obj bg-callback command)
  (if (simap4-connected-p obj)
      (let ((job (simap4-job-create obj)))
	(simap4-queue-job obj job bg-callback command)
	(simap4-run-queue obj)
	(simap4-run-checker obj))
    (simap4-display-progress obj "[DISC]")
    nil))

(defun simap4-exec-command (obj command)
  (simap4-stop-keepalive-timer obj)
  (let (job)
    (if (simap4-connected-p obj)
	(unwind-protect
	    (setq job (simap4-exec-command1 obj command))
	  (simap4-start-keepalive-timer obj))
      (simap4-failure obj 'lost-connection)
      (simap4-display-progress obj "[DISC]"))
    job))

;;------------------------------------------------------------------------
;; debug
(defun simap4-debug (obj fmt &rest args)
  (if simap4-debug
      (let* ((curtime (current-time))
	     (timestamp (format "[%s.%03d]"
				(format-time-string "%M:%S" curtime)
				(/ (elt curtime 2) 1000)))
	     mark)
	(if (eq (simap4-get-debug-mode obj) 'display-process-buffer)
	    (progn
	      (if (not (frame-live-p (simap4-get-debug-frame obj)))
		  (let ((name (format "SIMAP4-DEBUG [%s]"
				      (buffer-name
				       (simap4-get-buffer obj)))))
		    (if (framep (simap4-get-debug-frame obj))
			(delete-frame (simap4-get-debug-frame obj)))
		    (simap4-set-debug-frame
		     obj (make-frame (list (cons 'name name))))))
	      (raise-frame (simap4-get-debug-frame obj))
	      (set-window-buffer
	       (frame-selected-window (simap4-get-debug-frame obj))
	       (simap4-get-buffer obj))))
	(let ((prev-buff (current-buffer)))
	  (unwind-protect
	      (progn
		(set-buffer (simap4-get-buffer obj))
		(if (processp (simap4-get-proc obj))
		    (progn
		      (setq mark (process-mark (simap4-get-proc obj)))
		      (if (marker-position mark)
			  (goto-char mark)
			(goto-char (point-max))))
		  (goto-char (point-max)))
		(insert-before-markers timestamp)
		(insert-before-markers (apply 'format (cons fmt args)))
		(insert-before-markers "\n")
		(if (processp (simap4-get-proc obj))
		    (progn
		      (setq mark (process-mark (simap4-get-proc obj)))
		      (if (marker-position mark)
			  (goto-char mark)
			(goto-char (point-max))))
		  (goto-char (point-max))))
	    (set-buffer prev-buff))))))

(defun simap4-kill-objects ()
  (interactive)
  (mapcar 'simap4-object-delete *simap4-object-list))

(provide 'simap4)
