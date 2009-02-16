;;; Saved through ges-version 0.3.3dev at 2003-01-20 16:55
;;; From: TSUCHIYA Masatoshi <tsuchiya@namazu.org>
;;; Subject: battery-acpi.el
;;; Newsgroups: gnu.emacs.sources
;;; Date: Tue, 21 Jan 2003 01:15:34 +0900
;;; Organization: Kyoto University, Japan

;;; battery-acpi.el --- Check battery status with Linux ACPI interface

;; Copyright (C) 2002 TSUCHIYA Masatoshi <tsuchiya@namazu.org>

;; Author: TSUCHIYA Masatoshi <tsuchiya@namazu.org>
;; Version: $Revision: 1.5 $

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.


;;; Commentary:

;; This file provides the function to check battery status with Linux
;; ACPI interface.


;;; Install:

;; Put this file to load-path'ed directory, and byte compile it if
;; desired.  And put these expressions into your ~/.emacs.
;;
;;     (require 'battery-acpi)
;;     (battery-acpi-insinuate)
;;
;; And just type M-x battery RET or M-x display-battery RET.


;;; Code:

(eval-when-compile
  (require 'battery))

(eval-and-compile
  (autoload 'battery "battery"
    "Display battery status information in the echo area.
The text being displayed in the echo area is controlled by the variables
`battery-echo-area-format' and `battery-status-function'." t)
  (autoload 'display-battery "battery"
    "Display battery status information in the mode line.
The text being displayed in the mode line is controlled by the variables
`battery-mode-line-format' and `battery-status-function'.
The mode line will be updated automatically every `battery-update-interval'
seconds." t)
  (autoload 'battery-insert-file-contents "battery"))

(defconst battery-linux-proc-acpi-low-rate 20)

(defconst battery-linux-proc-acpi-critical-rate 10)

(defun battery-linux-proc-acpi ()
  "Get ACPI status information from Linux kernel.
This function requires ACPI subsystem version 20020308.

The following %-sequences are provided:
%v Linux driver version
%L AC line status (verbose)
%B Battery status (verbose)
%b Battery status, empty means high, `-' means low,
   `!' means critical, and `+' means charging
%p battery load percentage
%s Remaining time in seconds
%m Remaining time in minutes
%h Remaining time in hours
%t Remaining time in the form `h:min'"
  (with-temp-buffer
    (let (version dcap remain percent sec min hour charging)
      (setq version
	    (and (battery-insert-file-contents "/proc/acpi/info")
		 (re-search-forward "^version:[ \t]+\\(.*\\)$" nil t)
		 (match-string 1)))
      (when (>= (string-to-number version) 20020308)
	(erase-buffer)
	(and
	 (battery-insert-file-contents "/proc/acpi/battery/BAT1/info")
	 (re-search-forward "design capacity:[ \t]+\\([0-9]+\\) mWh$" nil t)
	 (setq dcap (string-to-number (match-string 1)))
	 (progn
	   (erase-buffer)
	   (battery-insert-file-contents "/proc/acpi/battery/BAT1/state"))
	 (progn
	   ;; This section of the code will calculate "percentage
	   ;; remaining" using battery capacity, and the following
	   ;; formula (acpi spec 3.9.2):
	   ;; percentage = (current_capacity / last_full_capacity) * 100
	   (when (re-search-forward
		  "^remaining capacity:[ \t]+\\([0-9]+\\) mWh$" nil t)
	     (setq percent
		   (* (/ (setq remain
			       (float (string-to-number (match-string 1))))
			 dcap)
		      100)))
	   ;; this section of code will calculate "time remaining"
	   ;; using battery remaining capacity, and battery "rate"
	   ;; (3.9.3)
	   (goto-char (point-min))
	   (when (re-search-forward
		  "^present rate:[ \t]+\\([0-9]+\\) mW$" nil t)
	     (setq sec
		   (truncate
		    (* (/ remain (string-to-number (match-string 1))) 3600))
		   min (/ sec 60)
		   hour (/ min 60)))
	   ;; check charging or not.
	   (goto-char (point-min))
	   (when (re-search-forward "^charging state:[ \t]+charging$" nil t)
	     (setq charging t))))
	(list
	 (cons ?v version)
	 (cons ?L
	       (if (or charging
		       (with-temp-buffer
			 (and (battery-insert-file-contents
			       "/proc/acpi/ac_adapter/ACAD/state")
			      (search-forward "on-line" nil t))))
		   "on-line" "off-line"))
	 (cons ?B
	       (cond
		(charging "charging")
		((<= percent battery-linux-proc-acpi-critical-rate) "critical")
		((<= percent battery-linux-proc-acpi-low-rate) "low")
		(t "high")))
	 (cons ?b
	       (cond
		(charging "+")
		((<= percent battery-linux-proc-acpi-critical-rate) "!")
		((<= percent battery-linux-proc-acpi-low-rate) "-")
		(t "")))
	 (cons ?p (format "%2.1f" percent))
	 (cons ?s sec)
	 (cons ?m min)
	 (cons ?h hour)
	 (cons ?t (format "%d:%02d" hour (% min 60)))
	 ;; Entries to keep compatibility with `battery-linux-proc-apm'.
	 (cons ?V "N/A") ; %V means APM BIOS version.
	 (cons ?I "N/A") ; %I means APM BIOS status (verbose).
	 )))))

(defun battery-acpi-insinuate ()
  "Overwrite options of battery.el with default values for ACPI."
  (interactive)
  (if (and (eq system-type 'gnu/linux)
	   (battery-linux-proc-acpi))
      (setq battery-status-function 'battery-linux-proc-acpi
	    battery-mode-line-format " [%b%p%%]"
	    battery-echo-area-format
	    "Power %L, battery %B (%p%% load, remaining time %t)")
    (message "This system does not have ACPI interface.")))

(provide 'battery-acpi)

;;; battery-acpi.el ends here

