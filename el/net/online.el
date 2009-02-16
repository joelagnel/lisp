;;; online.el --- check reachability of servers before connecting TCP.

;; Author: Takeshi Morishima <tm@interaccess.com>
;; Version: 1.0
;; Date: Oct 6, 2001
;; Keywords: network communications

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
;;
;; The purpose of this program is to gurantee relatively short
;; response time for a TCP connection establishment attempt even when
;; the server cannot be reached and expected to fail, plus provide a
;; way to query reasonably recent status of server reachability. It
;; also provides hooks to automatically execute procedures when
;; reachability status changes between off-line and on-line. This may
;; be useful when changing status display, and any background
;; synchronization of off-line mode work.
;;
;; Typical users start wondering what is going on after abut 5 sec
;; wait time. Nowadays most networks (except probably for some
;; widearea wireless mobile data access) are high bandwidth
;; environment which likely gives a response time less than 5 sec if
;; server is reachable.  When a host is unreachable, the question is
;; how quick that the connection request could fail.  Unfortunately
;; timer and retry values in typical implementation of TCP stack are
;; too large, requires long time to return a failure response. In
;; addition, name resolution could sometimes take long time when DNS
;; server is not reachable. If a negative response can be returned
;; within 5 sec, user's stress due to such wait time may be greatly
;; reduced.
;; 
;; This program makes use of ping program as an external program to
;; test reachability of a server. This is a kind of hack, but
;; probably is a reasonable compromise since it is not practical to
;; change TCP behavior in the OS TCP/IP stack and a ping program is
;; available in most of OS environment that deals with TCP/IP. This
;; also works well when delay is introduced by name resolution in
;; typical multi-tasking OS environment.  Because ping is an external
;; process, Emacs has control to kill the process at any time, even if
;; the ping process is completely blocked for name resolution. In
;; addition, it does not block other Emacs user interaction while
;; backgound check is done. It simply starts external process, and
;; check the result after some time period.
;;
;; It is assumed that the workstation or PC running this program is
;; reasonably fast so that the overhead of launching the ping program
;; is very small. (In 366MHz Pentium II with NT4.0 environement it is
;; measured 10-20ms in average). If you are on a slow host like 3MIPS
;; machine, you may not want to load this program. (And you may not
;; want to use recent Emacs either.) If you are interested in seeing
;; how yours is performing, after running this program some time, you
;; can do M-x online-report-stats. It will report some stats collected
;; in this module. By the way, this module also collects statistics
;; for the server connection establishment time and reports through
;; the above command.
;;
;; For those applications that use TCP frequently in bursty manner
;; (such as http fetch), an online/offline status cache timers are
;; used so that reachability test will not be performed every time if
;; last check is relatively recent (the cache timer has not been
;; expired). The online cache timer will be restarted whenever
;; reachability is confirmed through ping response, or a successful
;; TCP establishment. The default online cache timer value is 5
;; second. The offline cache timer value is 1min, which may be
;; canceled by an explicit reachability test request from application
;; or the background status checker.
;;
;; To maintain reasonably recent reachability status and to detect
;; automatic status change, a periodic ping will be performed in
;; background once a network connection request is attempted. By
;; default, periodic ping will be performed before offline status
;; cache timer expires to keep the status recent. This helps the user
;; not to be blocked unnecessarily when a occasional TCP open is
;; attempted while the status is off-line.
;;
;; Note: this may be considered as an 'evil' program, since it
;; generates unnecessary IP traffic (ICMP echo request and reply) for
;; the sake of the user's convenience.  Therefore, it is discouraged
;; to use this program to monitor servers on the Internet where
;; network bandwidth resources are shared in public, especially if the
;; server is located outside of your country or continent over shared,
;; low bandwidth links. Use this in your private network environment
;; such as your company intranet which reserves enough bandwidth for
;; you. (Of course, one can argue that this would be ignoreable
;; comparing to most of commercial web http transaction or spam email,
;; etc.)
;;
;; Usage
;;
;; This program requires the following OS features.
;;    o ping program
;;    o sub-second clock resolution for current-time and sleep-for
;;    o general multitasking for possible multiple ping tasks
;; Please make sure that your OS enviroenment supports these.
;;
;; To use the basic open-network-stream extention, simply load this
;; module from your .emacs file, by (load "online"). You can use
;; customize to change option values once the module is loaded. M-x
;; customize-group and type online to invoke the customize for this.
;;
;; The following facilities may be usuful from other networking and
;; communications programs.
;;
;; online-check-reachability - Returns t if a server is online,
;; otherwise nil. If cache is not recent, it will perform reachability
;; test. If a test is performed, the caller will be blocked during the
;; test. An example is to determine if an email message should be
;; queued or sent to the server. An option to force reachability test
;; is also provided.
;;
;; online-expire-cache - Forces reachability test for next TCP open
;; attempt regardless of the cached status. Users typically do not
;; want to wait long time for the cache to expire if they know the
;; connection is already up. An example is a command to flush queued
;; email messages.  When executing such command, the user probably
;; know the server is online e.g. manually connected through a dial-up
;; modem. (online-check-reachability takes an optional argument to
;; ignore cache as well, if you do not need to perform TCP open.)
;;
;; online-status-change-hooks - Hooks set to this variable will be
;; called anytime the reachability status has changed. Please refer to
;; documentation of this variable for argument passed to the hooks.
;; In this module, this is used to display reachability status on the
;; mode line.
;;

;;; Code:
(require 'timer)
(require 'advice)

;; ------------------------------------------------------------------------
;;  feature options for customization
;; ------------------------------------------------------------------------

;; customize backward compatibility code
;; Please refer to: <http://www.dina.kvl.dk/~abraham/custom/>
(eval-and-compile
  (condition-case ()
      (require 'custom)
    (error nil))
  (if (and (featurep 'custom) (fboundp 'custom-declare-variable))
      nil ;; We've got what we needed
    ;; We have the old custom-library, hack around it!
    (defmacro defgroup (&rest args)
      nil)
    (defmacro defcustom (var value doc &rest args)
      (` (defvar (, var) (, value) (, doc))))))

(defgroup online nil
  "Check reachability of servers before connecting TCP."
  :group 'comm)

(defcustom online-display-status t
  "*Non-nil means display online status on the mode line"
  :type 'boolean
  :group 'online)

(defcustom online-display-status-format "[%s]"
  "*Format of the mode line display when online-display-status is
non-nil.  The first %s will be a string of online status in the order
of server appeared in online-display-status-servers. In this string,
'*' means online, '-' means offline. ? means unknown."
  :type 'string
  :group 'online)

(defcustom online-display-status-servers nil
  "*A list of server names to display status on the mode line."
  :type '(repeat string)
  :group 'online)

(defcustom online-inactivity-timeout 5
  "*Timeout value in seconds for positive ping response."
  :type 'integer
  :group 'online)

(defcustom online-cache-timer 5
  "*Time interval in seconds for a online status to be valid after
reachability was last confirmed."
  :type 'integer
  :group 'online)

(defcustom online-offline-cache-timer 60
  "*Time interval in seconds for a offline status to be valid after
unreachability was last confirmed."
  :type 'integer
  :group 'online)

(defcustom online-bg-check-interval
  (- online-offline-cache-timer online-inactivity-timeout 1)
  "*Time interval in seconds for periodic background check."
  :type 'integer
  :group 'online)

;; ping program specification
(defcustom online-ping-program "ping"
  "*Program to send network test packets to a host."
  :type 'string
  :group 'online)

(defcustom online-ping-program-options 
  (cond ((eq system-type 'berkeley-unix) (list "-i2" "-v"))
	((eq system-type 'windows-nt) (list "-t")))
  "*List of option strings for the ping program."
  :type '(repeat string)
  :group 'online)

(defcustom online-ping-alive-regexp
  (cond ((eq system-type 'windows-nt) "Reply from")
	(t "icmp_seq="))
  "*A regular expression that match a successful ping response."
  :type 'regexp
  :group 'online)

;; A reachability status change hook variable, intended for use from
;; other programs; therefore, it is not a customizable.
(defvar online-status-change-hooks '(online-display-status)
  "A hook variable for reachability status change. It will be called
in the order appeared in the list, with arguments as SERVER and
STATUS. SERVER is a server name string and STATUS is a new
reachability status, either 'online or 'offline. Any errors detected
during execution of a hook will result in a silent removal from this
variable.")

;; ------------------------------------------------------------------------
;;  globals
;; ------------------------------------------------------------------------
;; internal control variables
(defvar online-stats-average-ping-launch-time 0)
(defvar online-stats-average-ping-count 0)
(defvar online-servers-list nil)
(defvar online-debug nil)

;; ------------------------------------------------------------------------
;;  misc functions
;; ------------------------------------------------------------------------
(defun online-server-symbol (server)
  "Returns a symbol for SERVER. The simbol is used to remember various
attributes using the symbol's property through get/set functions."
  (let ((ssym (intern (concat "online-attribute-" server))))
    (if (null (memq ssym online-servers-list))
	(progn
	  (setq online-servers-list (cons ssym online-servers-list))
	  (put ssym 'server server)))
    ssym))

(defun online-get-status (server)
  "Returns a cons of reachability status and time stamp at the time
last the status was checked in for SERVER, with the format returned by
current-time function. If nothing has been checked in, nil will be
returned."
  (let* ((ssym (online-server-symbol server))
	 (status (get ssym 'current-status)))
    (and status (cons status (get ssym 'last-update-time)))))

(defun online-set-status (server new-status)
  "Set new reachability status and current time stamp for SERVER.  It
also runs online-status-change-hooks if old status and new status are
different."
  (let* ((ssym (online-server-symbol server))
	 (old-status (get ssym 'current-status)))
    (if online-debug
	(message (format "online-debug: status update for %s -> %s."
			 server new-status)))
    (put ssym 'last-update-time (current-time))
    (put ssym 'current-status new-status)
    (if (not (eq old-status new-status))
	(online-run-change-hooks server new-status))))

(defun online-run-change-hooks (server new-status)
  "Runs hooks listed in online-status-change-hooks."
  (let ((hooks online-status-change-hooks) hook)
    (while hooks
      (setq hook (car hooks))
      (setq hooks (cdr hooks))
      (condition-case error-data
	  (funcall hook server new-status)
	(error
	 (if online-debug
	     (message
	      "Error returned in online-status-change-hooks (%s)\n[%s]"
	      hook (cdr error-data)))
	 (setq online-status-change-hooks
	       (delq hook online-status-change-hooks)))))))

(defun online-expire-cache (server)
  "Forces next reachability test for SERVER regardless of cached
status by setting last update time stamp to nil."
  (let ((ssym (online-server-symbol server)))
    (put ssym 'last-update-time nil)))

(defun online-recent-p (server valid-time)
  "Check if last status update time of the SERVER is recent, within
the time specified in seconds by VALID-TIME. Return t if change is
recent, otherwise nil."
  (let ((last-update-time (cdr (online-get-status server))))
    (if last-update-time
	(let ((elapsed (online-msec-elapsed last-update-time (current-time))))
	  (and (>= elapsed 0) (< elapsed (* valid-time 1000)))))))

(defun online-msec-elapsed (start-time end-time)
  "Calculate elapsed time from START-TIME to END-TIME, return the time
difference in milliseconds. START-TIME and END-TIME is the form
returned from current-time function"
  (let ((sec (- (+ (elt end-time 1)
		   (* 65536 (- (elt end-time 0) (elt start-time 0))))
		(elt start-time 1)))
	(msec (/ (- (+ (elt end-time 2) 1000000) (elt start-time 2)) 1000)))
    (- (+ (* sec 1000) msec) 1000)))

(defun online-check-reachability (server &optional ignore-cache)
  "Check reachability for SERVER considering status cache. It returns
t if server is considered to be online, or otherwise nil. If status
cache is invalid, then it refresh the cache by performing ping
reachability test. This also schedules background periodic
reachability check if cache is updated."
  (let ((status (car (online-get-status server))))
    (cond ((and (null ignore-cache)
		(eq status 'online)
		(online-recent-p server online-cache-timer))
	   (online-peg-server-stats server '+hit)
	   t)
	  ((and (null ignore-cache)
		(eq status 'offline)
		(online-recent-p server online-offline-cache-timer))
	   (online-peg-server-stats server '-hit)
	   nil)
	  (t
	   (online-start-background server)
	   (online-ping-it server)))))

(defun online-ping-it (server)
  "Perform ping for SERVER, and returns t if reachability is
confirmed. Otherwise returns nil. The caller will be blocked until the
ping returns a positive response, abnormally exited, or time specified
by online-inactivity-timeout has elapsed."
  (save-excursion
    (save-match-data
      (let (start proc buffer result)
	(setq start (current-time))
	(unwind-protect
	    (progn
	      (setq buffer (get-buffer-create
			    (concat " *online-ping-it:" server)))
	      (set-buffer buffer)
	      (erase-buffer)
	      (setq proc (apply 'start-process "online-ping" buffer
				online-ping-program
				(append online-ping-program-options
					(list server))))
	      (process-kill-without-query proc)
	      (setq online-stats-average-ping-count
		    (1+ online-stats-average-ping-count))
	      (setq online-stats-average-ping-launch-time
		    (/ (+ online-stats-average-ping-launch-time
			  (online-msec-elapsed start (current-time))) 2))
	      (with-timeout (online-inactivity-timeout nil)
		(goto-char (process-mark proc))
		(sleep-for 0 100)
		(while (and (eq (process-status proc) 'run)
			    (not (re-search-backward
				  online-ping-alive-regexp nil t)))
		  (sleep-for 0 100)
		  (goto-char (process-mark proc)))
		(goto-char (process-mark proc)))
	      (if (re-search-backward online-ping-alive-regexp nil t)
		  (setq result t))
	      (if result
		  (online-set-status server 'online)
		(online-set-status server 'offline)))
	  (condition-case nil (if (processp proc) (delete-process proc)))
	  (if (buffer-live-p buffer) (kill-buffer buffer)))
	result))))

;; ------------------------------------------------------------------------
;;  background checking
;; ------------------------------------------------------------------------
(defun online-start-background (server)
  "This function start background reachability check for SERVER. It
launches a timer task to perform a background reachability test."
  (let ((ssym (online-server-symbol server)))
    (if (and (get ssym 'periodic-check-timer)
	     (timerp (get ssym 'periodic-check-timer)))
	(cancel-timer (get ssym 'periodic-check-timer)))
    (if (and (get ssym 'ping-result-timer)
	     (timerp (get ssym 'ping-result-timer)))
	(cancel-timer (get ssym 'ping-result-timer)))
    (put ssym 'periodic-check-timer
	 (run-with-timer online-bg-check-interval
			 nil 'online-kickoff-ping server))))

(defun online-kickoff-ping (server)
  "This function is a timer action and must called only by the timer.
It invokes a ping program for SERVER, and launches a timer task for
result test after online-inactivity-timeout seconds. It also restarts
periodic background check timer to repeat this function for another
online-bg-check-interval seconds."
  (let ((ssym (online-server-symbol server)))
    (put ssym 'periodic-check-timer
	 (run-with-timer online-bg-check-interval
			 nil 'online-kickoff-ping server))
    (put ssym 'ping-result-timer
	 (run-with-timer online-inactivity-timeout
			 nil 'online-check-ping-result server)))
  (if online-debug
      (message (format "online-debug: kick ping for %s." server)))
  (let ((buffer (get-buffer-create (concat " *online-bg-ping:" server)))
	(start (current-time)) proc)
    (save-excursion
      (set-buffer buffer)
      (erase-buffer))
    (setq proc (apply 'start-process "online-ping" buffer
		      online-ping-program
		      (append online-ping-program-options (list server))))
    (process-kill-without-query proc)
    (setq online-stats-average-ping-count
	  (1+ online-stats-average-ping-count))
    (setq online-stats-average-ping-launch-time
	  (/ (+ online-stats-average-ping-launch-time
		(online-msec-elapsed start (current-time))) 2))))

(defun online-check-ping-result (server)
  "This function is a timer action and must called only by the timer.
It checks the result of ping, and determine the reachability status of
SERVER. Any confirmed status will be checked in using
online-set-status which in turn updates status cache and executes any
status change hooks specified online-status-change-hooks."
  (put (online-server-symbol server) 'ping-result-timer nil)
  (if online-debug
      (message (format "online-debug: check ping result for %s." server)))
  (save-excursion
    (save-match-data
      (let ((buffer (get-buffer (concat " *online-bg-ping:" server)))
	    (status (car (online-get-status server))) proc)
	(if (or (null buffer) (not (buffer-live-p buffer)))
	    (online-set-status server 'offline)
	  (set-buffer buffer)
	  (setq proc (get-buffer-process buffer))
	  (if (not (processp proc))
	      (online-set-status server 'offline)
	    (goto-char (process-mark proc))
	    (if (re-search-backward online-ping-alive-regexp nil t)
		(online-set-status server 'online)
	      (online-set-status server 'offline))
	    (condition-case nil
		(progn
		  (if (processp proc) (delete-process proc))
		  (if (buffer-live-p buffer) (kill-buffer buffer))))))))))

;; ------------------------------------------------------------------------
;;  statistics report
;; ------------------------------------------------------------------------
(defun online-report-stats ()
  "Lists statstics collected by the online library module for all
known servers. The first section shows per-server stats. The following
explains each column in this per-server section.

Server column - the server name for the row.
status column - the server reachability status.
max column - Maximum value of successful TCP open time in msec.
min column - Minimum value of successful TCP open time in msec.
ave column - Average value of successful TCP open time in msec.
atmpt column - Number of actual TCP open attempt (cache hit excluded).
succ column - Number of successful TCP open (cache hit excluded).
+hit column - Number of cache hit during online. (no ping check done).
-hit column - Number of cache hit during offline. (no ping check done).

The second section shows other general statistics not specific to a
server. Each stats are explained below.

Ping process invoke time - average time taken to start the ping
process in msec.

Ping process attempt count - Total number of times the ping process
has been used for server reachability check.
"
  (interactive)
  (with-output-to-temp-buffer "*Online Statistics Report*"
    (let ((ssym-list online-servers-list) servers)
      (while ssym-list
	(setq servers (cons (get (car ssym-list) 'server) servers))
	(setq ssym-list (cdr ssym-list)))
      (setq servers (sort servers 'string<))
      (princ "Server                     ")
      (princ "status ")
      (princ "  max  min  ave atmpt  succ  +hit  -hit\n")
      (princ "--------------------------")
      (princ " -------")
      (princ " ---- ---- ---- ----- ----- ----- -----\n")
      (if (null servers)
	  (princ "No server stats available.\n")
	(while servers
	  (online-report-stats-for-server (car servers))
	  (setq servers (cdr servers))))
      (princ "\n\n")
      (princ (format "Ping process invoke time (msec):  %5d\n"
		     online-stats-average-ping-launch-time))
      (princ (format "Ping process attempt count:       %5d\n"
		     online-stats-average-ping-count)))))

(defun online-report-stats-for-server (server)
  (let ((ssym (online-server-symbol server))
	(sstr (if (> (length server) 25)
		  (concat (substring server 0 24) "$")
		server)))
    (princ (format "%-25s: " sstr))
    (princ (format "%-7s " (or (get ssym 'current-status) "unknown")))
    (princ (format "%4d " (or (get ssym 'max-open-time) 0)))
    (princ (format "%4d " (or (get ssym 'min-open-time) 0)))
    (princ (format "%4d " (or (get ssym 'ave-open-time) 0)))
    (princ (format "%5d " (or (get ssym 'open-attempt) 0)))
    (princ (format "%5d " (or (get ssym 'open-success) 0)))
    (princ (format "%5d " (or (get ssym '+hit) 0)))
    (princ (format "%5d\n" (or (get ssym '-hit) 0)))))

(defun online-peg-server-stats (server tag &optional data)
  "Pegs statistics specified by TAG for SERVER. When open-time is
specified as TAG, DATA is interpreted as a successful TCP open time in
milliseconds, and max, min and running average values are calculated
and stored. All other types of TAG is considered a simple counter, and
current value is increased by one and stored."
  (let ((ssym (online-server-symbol server)) count)
    (cond ((and (eq tag 'open-time) (integerp data))
	   (if (or (null (get ssym 'max-open-time))
		   (> data (get ssym 'max-open-time)))
	       (put ssym 'max-open-time data))
	   (if (or (null (get ssym 'min-open-time))
		   (< data (get ssym 'min-open-time)))
	       (put ssym 'min-open-time data))
	   (if (null (get ssym 'ave-open-time))
	       (put ssym 'ave-open-time data)
	     (put ssym 'ave-open-time
		  (/ (+ data (get ssym 'ave-open-time)) 2))))
	  (t (setq count (or (get ssym tag) 0))
	     (put ssym tag (1+ count))))))

;; ------------------------------------------------------------------------
;;  mode line display
;; ------------------------------------------------------------------------
(defvar online-display-data-transfer "")
(or global-mode-string (setq global-mode-string '("")))
(if (not (member 'online-display-data-transfer global-mode-string))
    (setq global-mode-string
	  (append global-mode-string '(online-display-data-transfer))))
(defun online-display-status (&optional server status)
  "When online-display-status is t, displays server reachability
status in mode line using format as specified by
online-display-status-format. The servers to display are specified by
online-display-status-servers. See document of
online-display-status-format for details of display symbol
explanation."
  (if (and online-display-status online-display-status-servers)
      (let ((servers online-display-status-servers) (s-string "") status)
	(while servers
	  (setq status (car (online-get-status (car servers))))
	  (setq s-string (concat s-string
				 (cond ((eq status 'online) "*")
				       ((eq status 'offline) "-")
				       (t "?"))))
	  (setq servers (cdr servers)))
	(setq online-display-data-transfer
	      (format online-display-status-format s-string))
	(force-mode-line-update)
	(sit-for 0))))
(if (and online-display-status-servers online-display-status)
    (online-display-status))

;; ------------------------------------------------------------------------
;; open-network-stream overloading
;; ------------------------------------------------------------------------
;; At the last of loading if evrything is loaded OK, overload
;; open-network-stream function to add the feature.
(defadvice open-network-stream (around online-open-network-stream activate)
  "This is adviced to add extention to perform ping based reachability
test and cache to avoid unnecessary user wait time. It also collects some
statistics on open operation. See comments in online.el for detail."
  (let ((server (ad-get-arg 2)) start)
    (if (online-check-reachability server)
	(progn
	  (setq start (current-time))
	  (online-peg-server-stats server 'open-attempt)
	  ad-do-it
	  (if ad-return-value
	      (progn
		(online-set-status server 'online)
		(online-peg-server-stats server 'open-success)
		(online-peg-server-stats
		 server 'open-time
		 (online-msec-elapsed start (current-time)))))
	  ad-return-value)
      (error "Server not online."))))

(provide 'online)
