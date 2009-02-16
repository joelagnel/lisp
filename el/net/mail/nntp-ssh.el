;;; nntp-ssh.el --- nntp access using SSH
;; Copyright (C) 2000 Free Software Foundation, Inc.

;; Author: Katsumi Yamaoka <yamaoka@jpl.org>
;; Created: 2000/08/18
;; Revised: 2000/12/28
;; Keywords: news, ssh, socks

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; NOTE: This module is now obsolete.  You should never use this module
;; if you are using Oort Gnus v0.04 or later, since Gnus provides the
;; similar or better feature by itself.  For example, the following
;; select method can be a substitute for the former technique.
;;
;;(nntp "Faraway"
;;      (nntp-address "NntpServer")
;;      (nntp-via-address "RemoteHost")
;;      (nntp-open-connection-function nntp-open-via-rlogin-and-telnet)
;;      (nntp-pre-command "runsocks")
;;      (nntp-via-rlogin-command "ssh")
;;      (nntp-end-of-line "\n")
;;      (nntp-authinfo-user "login name for NntpServer")
;;      (nntp-authinfo-password "password for NntpServer"))

;; This program provides to connect to the remote host using SSH and
;; then telnet from there to get to the NNTP server.  You can use the
;; select-method as such as follows:
;;
;;(nntp "Faraway"
;;      (nntp-address "RemoteHost")
;;      (nntp-open-connection-function nntp-open-ssh)
;;      (nntp-ssh-environments ("LD_LIBRARY_PATH=/usr/local/lib"
;;			      "LD_PRELOAD=libsocks5_sh.so"))
;;      (nntp-ssh-password-required t)
;;      (nntp-ssh-user-name "login name for RemoteHost")
;;      (nntp-ssh-passwd "password for RemoteHost")
;;      (nntp-telnet-shell-prompt "%[\t ]*")
;;      (nntp-end-of-line "\n")
;;      (nntp-telnet-parameters ("exec" "telnet" "-8" "NntpServer" "nntp"))
;;      (nntp-authinfo-user "login name for NntpServer")
;;      (nntp-authinfo-password "password for NntpServer"))

;;; Code:

(require 'nntp)

(defvoo nntp-ssh-command "ssh"
  "*Name of program to invoke SSH.")

(defvoo nntp-ssh-explicit-args '("-C")
  "*List of arguments to pass to SSH on the command line.")

(defvoo nntp-ssh-environments nil
  "*Environment variables that are used while SSH is running.  If you are
behind a firewall machine, like the following value can be used for a
substitute for `runsocks'.

\(\"LD_LIBRARY_PATH=/usr/local/lib\" \"LD_PRELOAD=libsocks5_sh.so\")")

(defvoo nntp-ssh-password-required nil
  "*Non-nil if a password is required when connecting to remote host.")

(defvoo nntp-ssh-user-name nil)
(defvoo nntp-ssh-passwd nil)

(defun nntp-open-ssh (buffer)
  "Open a connection to remote host using SSH."
  (save-excursion
    (set-buffer buffer)
    (erase-buffer)
    (let ((process-environment (append nntp-ssh-environments
				       process-environment))
	  (userhost (if nntp-ssh-user-name
			(concat nntp-ssh-user-name "@" nntp-address)
		      nntp-address))
	  (case-fold-search t)
	  proc)
      (message "Opening SSH connection to %s..." userhost)
      (setq proc (as-binary-process
		  (apply 'start-process "nntpd" buffer nntp-ssh-command
			 (append nntp-ssh-explicit-args (list userhost)))))
      (when (memq (process-status proc) '(open run))
	(when nntp-ssh-password-required
	  (nntp-wait-for-string "password:")
	  (process-send-string
	   proc (concat
		 (or nntp-ssh-passwd
		     (setq nntp-ssh-passwd
			   (mail-source-read-passwd
			    (concat "Password for " userhost ": "))))
		 "\n"))
	  (message "Logging in to %s..." userhost))
	(nntp-wait-for-string nntp-telnet-shell-prompt)
	(if (let (case-fold-search)
	      (string-match "^exec r?telnet -8 [^ $:={}]+ nntp$"
			    (mapconcat 'identity nntp-telnet-parameters " ")))
	    (progn
	      (message "Opening NNTP connetction to %s via %s..."
		       (nth 3 nntp-telnet-parameters) userhost)
	      (process-send-string proc (concat "exec "
						(nth 1 nntp-telnet-parameters)
						"\n"))
	      (nntp-wait-for-string "^r?telnet")
	      (process-send-string proc "set binary\n")
	      (nntp-wait-for-string "^r?telnet")
	      (process-send-string proc (concat "open "
						(nth 3 nntp-telnet-parameters)
						" nntp\n")))
	  (message "Opening NNTP connetction via %s..." userhost)
	  (process-send-string
	   proc (concat (mapconcat 'identity nntp-telnet-parameters " ")
			"\n")))
	(nntp-wait-for-string "^\r*20[01]")
	(beginning-of-line)
	(delete-region (point-min) (point))
	(process-send-string proc "\^]")
	(nntp-wait-for-string "^r?telnet")
	(process-send-string proc "mode character\n")
	(accept-process-output proc 5)
	(save-excursion
	  (sit-for 1))
	(goto-char (point-min))
	(forward-line 1)
	(delete-region (point) (point-max)))
      (message "")
      proc)))

(provide 'nntp-ssh)

;; nntp-ssh.el ends here
