;;; ftcp.el --- wrapper for open-network-stream to use external process

;; Copyright (C) 1996, 1999 Noah S. Friedman

;; Author: Noah Friedman <friedman@splode.com>
;; Maintainer: friedman@splode.com
;; Keywords: extensions
;; Created: 1996-03-03

;; $Id: ftcp.el,v 1.5 2000/01/10 20:26:20 friedman Exp $

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
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.

;;; Commentary:

;; In Emacs 19.30 and prior, emacs can hang indefinitely during a call to
;; open-network-stream if the remote host is down, because it must wait for
;; the `connect' system call to time out; there is no way to interrupt it.
;; In Emacs 19.31 this call was made interruptable, but this package is
;; still useful for making proxy connections through firewalls using
;; kerberos or ssh, for example.

;; This package redefines `open-network-stream' so that you can toggle
;; between using an external program to handle TCP connection and I/O, or
;; use emacs' internal support (if available).

;; The Emacs distribution comes with a program called `tcp' in the lib-src
;; directory.  Normally it is not compiled or installed, so it is not used
;; here by default.  Another similar program is `nc' ("netcat") which is
;; available as an RPM for Red Hat systems.
;;
;; Another alternative is my `tcpconnect' program, available via
;; ftp://ftp.splode.com/pub/users/friedman/scripts/tcpconnect or you can
;; write to me by email and ask for a copy.  `tcpconnect' is available
;; under the same legal conditions as this program.

;;; Code:

(require 'advice)

(defvar ftcp-open-network-stream-mode nil
  "*If non-`nil', use an external program to handle TCP connections and I/O.
Otherwise, use the built-in TCP support in Emacs (if available).
This value of this variable is examined only when `open-network-stream' is
called; once the process/stream object is created, its nature is fixed.")

(defvar ftcp-open-network-stream-program "nc" ;; "tcp" "tcpconnect"
  "*Name of external program used handle TCP connections and I/O.
This program should accept at least two arguments: a hostname and a port
number.  It should also recognize any additional arguments set in
`ftcp-open-network-stream-program-args', which are passed first.")

(defvar ftcp-open-network-stream-program-args nil
  ;; '("--no-intr" "--") ; for use with tcpconnect on ttys
  "*A list of standard arguments to an external tcp handler program.
These are prepended to arguments consisting of the host and port number.")

(defvar ftcp-process-connection-type 'default
  "*Control type of device used to communicate with subprocesses.
Values are nil to use a pipe, or t or `pty' to use a pty;
when value is `default', always defer to the current value of
`process-connection-type' instead.
The value has no effect if the system has no ptys or if all ptys are busy:
then a pipe is used in any case.
The value takes effect when `open-network-stream' is called and
`ftcp-open-network-stream-mode' is non-nil.")


;;;###autoload
(defun ftcp-open-network-stream-mode (&optional prefix)
  "Toggle using internal network support or an external program for TCP.
A positive prefix argument always enables this mode.
A negative prefix argument always disables this mode.
Invoking this command with no prefix toggles the current state of the mode.

See documentation for the variable `ftcp-open-network-stream-mode' for more
information."
  (interactive "P")
  (cond ((null prefix)
         (setq ftcp-open-network-stream-mode
               (not ftcp-open-network-stream-mode)))
        ((>= prefix 0)
         (setq ftcp-open-network-stream-mode t))
        (t
         (setq ftcp-open-network-stream-mode nil)))
  (and (interactive-p)
       (message "open-network-stream will use %s"
                (if ftcp-open-network-stream-mode
                    "an external process"
                  "builtin network support"))))

;;;###autoload
(defun ftcp-open-network-stream (name buffer host service)
  "Open a TCP connection for a service to a host.
Returns a subprocess-object to represent the connection.
Input and output work as for subprocesses; `delete-process' closes it.
Args are NAME BUFFER HOST SERVICE.
NAME is name for process.  It is modified if necessary to make it unique.
BUFFER is the buffer (or buffer-name) to associate with the process.
 Process output goes at end of that buffer, unless you specify
 an output stream or filter function to handle the output.
 BUFFER may be also nil, meaning that this process is not associated
 with any buffer
Third arg is name of the host to connect to, or its IP address.
Fourth arg SERVICE is name of the service desired, or an integer
 specifying a port number to connect to."
  (and (numberp service)
       (setq service (number-to-string service)))
  (let ((process-connection-type (if (eq ftcp-process-connection-type 'default)
                                     process-connection-type
                                   ftcp-process-connection-type)))
    (apply 'start-process
           name buffer
           ftcp-open-network-stream-program
           (append ftcp-open-network-stream-program-args
                   (list host service)))))

(defadvice open-network-stream (around ftcp activate)
  "Use external process for network connections when
ftcp-open-network-stream-mode is non-nil."
  (if ftcp-open-network-stream-mode
      (setq ad-return-value (apply 'ftcp-open-network-stream (ad-get-args 0)))
    ad-do-it))

(provide 'ftcp)

;;; ftcp.el ends here
