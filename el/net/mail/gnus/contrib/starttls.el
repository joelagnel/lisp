;;; starttls.el --- STARTTLS support via wrapper around GNU TLS

;; Copyright (C) 2003 Free Software Foundation, Inc.

;; Author: Simon Josefsson <simon@josefsson.org>
;; Keywords: comm, tls, gnutls, ssl

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This package implements a simple wrapper around the GNU TLS command
;; line application "gnutls-cli" to make Emacs support STARTTLS.  It
;; is backwards compatible (same API functions) with the "starttls.el"
;; that is part of Emacs 21 written by Daiki Ueno <ueno@unixuser.org>.
;; (That version used an external program "starttls" that isn't widely
;; installed, and was based on OpenSSL.)

;; This package require GNUTLS 0.9.90 (released 2003-10-08) or later.

;; Usage is similar to `open-network-stream'.  Evaluating the following:
;;
;; (progn
;;   (setq tmp (open-starttls-stream "test" (current-buffer) "mail.example.com" 143))
;;   (process-send-string tmp ". starttls\n")
;;   (sit-for 4)
;;   (message "STARTTLS output:\n%s" (negotiate-starttls tmp))
;;   (process-send-string tmp ". capability\n"))
;;
;; in, e.g., the *scratch* buffer, yields the following output:
;;
;; * OK imap.example.com Cyrus IMAP4 v2.1.15 server ready
;; . OK Begin TLS negotiation now
;; * CAPABILITY IMAP4 IMAP4rev1 ACL QUOTA ...
;; . OK Completed
;; nil
;;
;; And the message buffer contains:
;;
;; STARTTLS output:
;; *** Starting TLS handshake
;; - Server's trusted authorities:
;;    [0]: O=Sendmail,OU=Sendmail Server,CN=imap.example.com,EMAIL=admin@imap.example.com
;; - Certificate type: X.509
;;  - Got a certificate list of 1 certificates.
;;
;;  - Certificate[0] info:
;;  # The hostname in the certificate matches 'imap.example.com'.
;;  # valid since: Wed Aug 28 12:47:00 CEST 2002
;;  # expires at: Thu Aug 28 12:47:00 CEST 2003
;;  # serial number: 00
;;  # fingerprint: 06 3f 25 cb 44 aa 5c 1e 79 d7 63 86 f8 b1 9a cf
;;  # version: #3
;;  # public key algorithm: RSA
;;  #   Modulus: 1024 bits
;;  # Subject's DN: O=Sendmail,OU=Sendmail Server,CN=imap.example.com,EMAIL=admin@imap.example.com
;;  # Issuer's DN: O=Sendmail,OU=Sendmail Server,CN=imap.example.com,EMAIL=admin@imap.example.com
;;
;;
;; - Peer's certificate issuer is unknown
;; - Peer's certificate is NOT trusted
;; - Version: TLS 1.0
;; - Key Exchange: RSA
;; - Cipher: ARCFOUR 128
;; - MAC: SHA
;; - Compression: NULL

;; Revision history:
;;
;; 2003-09-20: Added to Gnus CVS.
;; 2003-10-02: Minor fixes.
;; 2003-11-15: Cleanup, and posted to gnu.emacs.sources.
;; 2003-11-28: Fixes variable name conflicts, various other fixes, posted g.e.s.

;;; Code:

(defgroup starttls nil
  "Negotiated Transport Layer Security (STARTTLS) parameters."
  :group 'comm)

(defcustom starttls-file-name "gnutls-cli"
  "Name of the program to run in a subprocess to open an STARTTLS connection.
The program should read input on stdin, write output to stdout,
and initiate TLS negotiation when receiving the SIGALRM signal.
Also see `starttls-connect', `starttls-failure', and
`starttls-success' for what the program should output after
initial connection and successful negotiation respectively."
  :type 'string
  :group 'starttls)

(defcustom starttls-extra-arguments nil
  "List of extra arguments to `starttls-file-name'.
E.g., (\"--protocols\" \"ssl3\")."
  :type '(repeat string)
  :group 'starttls)

(defcustom starttls-process-connection-type nil
  "*Value for `process-connection-type' to use when starting STARTTLS process."
  :type 'boolean
  :group 'starttls)

(defcustom starttls-connect "- Simple Client Mode:\n\n"
  "*Regular expression indicating successful connection.
The default is what GNUTLS's \"gnutls-cli\" outputs."
  ;; GNUTLS cli.c:main() print this string when it is starting to run
  ;; in the application read/write phase.  If the logic, or the string
  ;; itself, is modified, this must be updated.
  :type 'regexp
  :group 'starttls)

(defcustom starttls-failure "*** Handshake has failed"
  "*Regular expression indicating failed TLS handshake.
The default is what GNUTLS's \"gnutls-cli\" outputs."
  ;; GNUTLS cli.c:do_handshake() print this string on failure.  If the
  ;; logic, or the string itself, is modified, this must be updated.
  :type 'regexp
  :group 'starttls)

(defcustom starttls-success "- Compression: "
  "*Regular expression indicating completed TLS handshakes.
The default is what GNUTLS's \"gnutls-cli\" outputs."
  ;; GNUTLS cli.c:do_handshake() calls, on success,
  ;; common.c:print_info(), that unconditionally print this string
  ;; last.  If that logic, or the string itself, is modified, this
  ;; must be updated.
  :type 'regexp
  :group 'starttls)

(defun negotiate-starttls (process)
  "Negotiate TLS on process opened by `open-starttls-stream'.
This should typically only be done once.  It typically return a
multi-line informational message with information about the
handshake, or NIL on failure."
  (let (buffer info old-max done-ok done-bad)
    (if (null (setq buffer (process-buffer process)))
	;; XXX How to remove/extract the TLS negotiation junk?
	(signal-process (process-id process) 'SIGALRM)
      (with-current-buffer buffer
	(save-excursion
	  (setq old-max (goto-char (point-max)))
	  (signal-process (process-id process) 'SIGALRM)
	  (while (and (processp process)
		      (eq (process-status process) 'run)
		      (save-excursion
			(goto-char old-max)
			(not (or (setq done-ok (re-search-forward
						starttls-success nil t))
				 (setq done-bad (re-search-forward
						 starttls-failure nil t))))))
	    (accept-process-output process 1 100)
	    (sit-for 0.1))
	  (setq info (buffer-substring-no-properties old-max (point-max)))
	  (delete-region old-max (point-max))
	  (if (or (and done-ok (not done-bad))
		  ;; Prevent mitm that fake success msg after failure msg.
		  (and done-ok done-bad (< done-ok done-bad)))
	      info
	    (message "STARTTLS negotiation failed: %s" info)
	    nil))))))

(defun open-starttls-stream (name buffer host service)
  "Open a TLS connection for a service to a host.
Returns a subprocess-object to represent the connection.
Input and output work as for subprocesses; `delete-process' closes it.
Args are NAME BUFFER HOST SERVICE.
NAME is name for process.  It is modified if necessary to make it unique.
BUFFER is the buffer (or buffer-name) to associate with the process.
 Process output goes at end of that buffer, unless you specify
 an output stream or filter function to handle the output.
Third arg is name of the host to connect to, or its IP address.
Fourth arg SERVICE is name of the service desired, or an integer
specifying a port number to connect to."
  (message "Opening STARTTLS connection to `%s'..." host)
  (let* (done
	 (old-max (with-current-buffer buffer (point-max)))
	 (process-connection-type starttls-process-connection-type)
	 (process (apply #'start-process name buffer
			 starttls-file-name "-s" host
			 "-p" (if (integerp service)
				  (int-to-string service)
				service)
			 starttls-extra-arguments)))
    (process-kill-without-query process)
    (while (and (processp process)
		(eq (process-status process) 'run)
		(save-excursion
		  (set-buffer buffer)
		  (goto-char old-max)
		  (not (setq done (re-search-forward
				   starttls-connect nil t)))))
      (accept-process-output process 0 100)
      (sit-for 0.1))
    (if done
	(with-current-buffer buffer
	  (delete-region old-max done))
      (delete-process process)
      (setq process nil))
    (message "Opening STARTTLS connection to `%s'...%s"
	     host (if done "done" "failed"))
    process))

;; Compatibility with starttls.el by Daiki Ueno <ueno@unixuser.org>:
(defvaralias 'starttls-program 'starttls-file-name)
(make-obsolete-variable 'starttls-program 'starttls-file-name)
(defvaralias 'starttls-extra-args 'starttls-extra-arguments)
(make-obsolete-variable 'starttls-extra-args 'starttls-extra-arguments)
(defalias 'starttls-open-stream 'open-starttls-stream)
(defalias 'starttls-negotiate 'negotiate-starttls)

(provide 'starttls)

;;; arch-tag: 1955b2ca-0cb2-47ad-bb95-47b43e5a15f5
;;; starttls.el ends here
