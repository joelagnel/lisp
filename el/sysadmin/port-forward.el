;;; Saved through ges-version 0.3.3dev at 2003-01-14 21:08
;;; ;;; From: no-spam@cua.dk (Kim F. Storm)
;;; ;;; Subject: Re: port-forward.el --- Open a port and forward to remote-host/port
;;; ;;; Newsgroups: gnu.emacs.sources
;;; ;;; Date: 14 Jan 2003 12:34:02 +0100
;;; ;;; Organization: StormWare


;;; Helmut Eller <e9626484@stud3.tuwien.ac.at> writes:

;;; > Wouldn't it be nicer if there where an extra plist slot, reserved for
;;; > user data, in all processes?  This slot could be set with a :plist
;;; > argument or accessed with process-plist/set-process-plist.

;;; I wrote:
 
;;; > Yes.  I have implemented new process-variable and set-process-variable
;;; > functions in CVS emacs, and added a :vars arg to make-network-process
;;; > to initialize the process variables for a new network process.

;;; On second thought, Helmut's API proposal is much better than mine, so
;;; I have now reworked the "process-variable" interface in CVS emacs to
;;; simply being a property list.

;;; We now have low-level process-plist and set-process-plist functions,
;;; and higher level process-get and process-put functions.

;;; Here is yet another version of Mario's package, adapted to the new API:

;;; port-forward.el --- Forward TCP ports from your host to a remote host/port

;; Copyright (C) 2003  Free Software Foundation, Inc.

;; Author: Mario Lang <mlang@delysid.org>
;; Modified by: Kim F. Storm <storm@cua.dk>
;; Keywords: comm, processes

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This file requires Emacs 21.3.50 from 2003-01-14 or later
;;
;; It can probably use some safety-checks to run really stable,
;; but I tested it with a SSH session, and that worked just fine,
;; so I thought I'd release this for people to play with.

;;; Code:

(defun forward-port-filter (proc string)
  (let ((partner (process-get proc 'partner)))
    (if (eq (process-status partner) 'open)
	(process-send-string partner string)
      (delete-process partner)
      (delete-process proc))))

(defun forward-port-slave-sentinel (slave event)
  (if (or (string-match "^finished" event)
	  (string-match "^exited" event))
      (let ((master (process-get slave 'partner)))
	(if master
	    (delete-process master))
	(delete-process slave))))

(defun forward-port-master-sentinel (master event)
  (cond
   ((string-match "^open" event)
    (let ((slave (make-network-process
		  :name (format "fw%s <%s>" 
				(process-get master 'lport)
				(format-network-address (process-contact master :remote)))
		  :host (process-get master 'rhost)
		  :service (process-get master 'rport)
		  :sentinel 'forward-port-slave-sentinel
		  :filter 'forward-port-filter
		  :plist `(partner ,master))))
      (if slave
	  (process-put master 'partner slave)
	(delete-process master))))

   ((or (string-match "^finished" event)
	(string-match "^exited" event))
    (let ((slave (process-get master 'partner)))
      (if slave
	  (delete-process slave))
      (delete-process master)))))

(defun forward-port (lport rhost rport)
  "Initiate a server-process listening on LPORT.
Open a new connection to RHOST:RPORT for each incoming connection."
  (interactive
   (list (let* ((p (read-string "Local port: "))
		(n (string-to-number p 10)))
	   (if (or (= n 0) (not (integerp n))) p n))
	 (read-string "Remote host: ") (read-string "Remote port: ")))
  (make-network-process
   :server t :noquery t
   :name (format "pf%s" lport)
   :buffer nil ; (format "server-on-%s" lport)
   :host nil :service lport
   :sentinel 'forward-port-master-sentinel
   :filter 'forward-port-filter
   :plist `(rhost ,rhost rport ,rport lport ,lport)))

(provide 'port-forward)
;;; port-forward.el ends here

;;; -- 
;;; Kim F. Storm  http://www.cua.dk

