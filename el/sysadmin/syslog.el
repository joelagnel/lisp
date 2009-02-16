;; A simple BSD syslog, http://www.ietf.org/rfc/rfc3164.txt, client and server.
;;
;; Copyright (C) 2003 Will Glozer
;;
;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
;; more details.
;;
;; You should have received a copy of the GNU General Public License along with
;; this program; if not, write to the Free Software Foundation, Inc., 59
;; Temple Place, Suite 330, Boston, MA 02111-1307 USA
;;
;; ----------------------------------------------------------------------
;;
;; Messages to the server are displayed in a buffer with color hints.  Requires
;; a fairly recent version of GNU emacs with support for UDP server sockets.
;;
;; syslog message format: <priority>timestamp message

(require 'time-stamp)

(defcustom syslog-host "127.0.0.1"
  "host to send syslog messages to"
  :type 'string
  :group 'syslog)

(defcustom syslog-port 514
  "port to send syslog messages to"
  :type 'integer
  :group 'syslog)

(defcustom syslogd-buffer "*syslogd*"
  "buffer to write syslog messages to"
  :type 'string
  :group 'syslog)

(defcustom syslogd-critical-facility 'ftp
  "facilities at or below this level are considered critical"
  :type 'symbol
  :group 'syslog)

(defcustom syslogd-critical-severity 'info
  "severities at or below this level are considered critical"
  :type 'symbol
  :group 'syslog)

(defcustom syslogd-port 514
  "port that syslog server listens on"
  :type 'integer
  :group 'syslog)

(defface syslogd-normal-face
  '((t (:forground "black")))
  "face for normal syslog messages"
  :group 'syslog)

(defface syslogd-critical-face
  '((t (:foreground "red")))
  "face for critical syslog messages"
  :group 'syslog)

(defconst syslog-facilities
  '((kern     . 0)
    (user     . 1)
    (mail     . 2)
    (daemon   . 3)
    (auth     . 4)
    (syslog   . 5)
    (lpr      . 6)
    (news     . 7)
    (uucp     . 8)
    (cron     . 9)
    (authpriv . 10)
    (ftp      . 11)
    (ntp      . 12)
    (logaudit . 13)
    (logalert . 14)
    (cron2    . 15)
    (local0   . 16)
    (local1   . 17)
    (local2   . 18)
    (local3   . 19)
    (local4   . 20)
    (local5   . 21)
    (local6   . 22)
    (local7   . 23))
  "syslog message facility, i.e. sender")

(defconst syslog-severities
  '((emerg   . 0)
    (alert   . 1)
    (crit    . 2)
    (err     . 3)
    (warning . 4)
    (notice  . 5)
    (info    . 6)
    (debug   . 7))
  "syslog message severity level")

;; shared

(defun syslog-facility-to-num (facility)
  "convert a syslog facility symbol into the approriate numeric value"
  (or (cdr (assoc facility syslog-facilities))
      (cdr (assoc 'local7 syslog-facilities))))

(defun syslog-severity-to-num (severity)
  "convert a syslog severity symbol into the approriate numeric value"
  (or (cdr (assoc severity syslog-severities))
      (cdr (assoc 'debug syslog-severities))))

(defun syslog-num-to-facility (num)
  "convert a numeric syslog facility to the appropriate symbol"
  (or (car (rassoc num syslog-facilities))
      (car (assoc 'local7 syslog-facilities))))

(defun syslog-num-to-severity (num)
  "convert a numeric syslog severity to the appropriate symbol"
  (or (car (rassoc num syslog-severities))
      (car (assoc 'debug syslog-severities))))

;; server

(defun syslogd-filter (process message)
  (with-current-buffer (process-buffer process)
    (goto-char (point-max))
    (let* ((contact-info (process-contact process t))
           (sender (cadr (memq :remote contact-info)))
           (host (syslog-parse-ip-addr sender))
           (port (syslog-parse-ip-port sender)))
      (syslogd-display host port message)
      (setq other-window-scroll-buffer (process-buffer process))
      (scroll-other-window 1))))

(defun syslogd-display (host port data)
  "display a syslog message in the current buffer"
  (if (string-match "<\\([0-9]+\\)>\\(.+\\)" data)
      (let* ((priority (string-to-int (match-string 1 data)))
             (facility (syslog-num-to-facility (/ priority 8)))
             (severity (syslog-num-to-severity (mod priority 8)))
             (message (match-string 2 data)))
        (syslogd-insert host port facility severity message))
    (syslogd-insert host port 0 0 (format "garbled message '%s'" message))))

(defun syslogd-insert (host port facility severity message)
  (let* ((date (time-stamp-yyyy-mm-dd))
         (time (time-stamp-hh:mm:ss))
         (from (format "%s.%s %s:%s %s %s" 
                       facility
                       severity
                       host
                       port
                       date
                       time))
         (face (if (syslogd-critical-p facility severity)
                   'syslogd-critical-face
                 'syslogd-normal-face))
         (mouse-face '((:background "lightgrey")))
         (start (point)))
    (insert message)
    (set-text-properties start (point) `(help-echo ,from
                                         face ,face
                                         mouse-face ,mouse-face))
    (insert-char ?\n 1)
    (set-text-properties (point) (point) nil)))

(defun syslogd-critical-p (facility severity)
  "determine if a syslog message is critical or not"
  (or (<= (syslog-facility-to-num facility)
          (syslog-facility-to-num syslogd-critical-facility))
      (<= (syslog-severity-to-num severity)
          (syslog-severity-to-num syslogd-critical-severity))))

(defun syslog-parse-ip-addr (sender)
  "parse the IP address from the vector returned by a call to process-contact"
  (format "%d.%d.%d.%d"
          (aref sender 0)          
          (aref sender 1)
          (aref sender 2)
          (aref sender 3)))

(defun syslog-parse-ip-port (sender)
  "parse the UDP port from the vector returned by a call to process-contact"
  (aref sender 4))

(defun syslogd-start ()
  "start the syslogd server"
  (interactive)
  (make-network-process :buffer syslogd-buffer
                        :filter 'syslogd-filter
                        :name "syslogd"
                        :noquery t
                        :server t
                        :service syslogd-port
                        :type 'datagram))

;; client

(defun syslog (facility severity message)
  "send a syslog message"
  (if (not (eq (process-status "syslog") 'open))
      (syslog-open syslog-host syslog-port))
  (process-send-string "syslog" (syslog-format facility severity message)))

(defun syslog-format (facility severity message)
  "format a message suitable for syslog consumption"
  (let* ((facility-num (syslog-facility-to-num facility))
         (severity-num (syslog-severity-to-num severity))
         (priority (+ (* facility-num 8) severity-num)))
    (format "<%d>%s\n" priority message)))

(defun syslog-open (host port)
  "open a connection to host:port"
  (make-network-process :name "syslog"
                        :host host
                        :noquery t
                        :service port
                        :type 'datagram))

(defun syslog-close ()
  "close the syslog connection if it is open"
  (interactive)
  (if (eq (process-status "syslog") 'open)
      (delete-process "syslog")
    (message "syslog socket is not open")))
