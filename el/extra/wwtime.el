;;; wwtime.el --- Insert a time of day with appropriate world-wide localization

;; Copyright (C) 2001 Norman Walsh

;; Author: Norman Walsh <ndw@nwalsh.com>
;; Maintainer: Norman Walsh <ndw@nwalsh.com>
;; Created: 2001-01-23
;; Version: 1.4
;; CVS ID: $Id: wwtime.el,v 1.11 2002/03/07 13:29:52 ndw Exp $
;; Keywords: time

;; This file is NOT part of GNU emacs.

;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; I frequently arrange teleconferences with participants from several
;; time zones. In an effort to reduce confusion about the time of the
;; meeting, I try to publish the time in several relevant time zones.

;; But correctly calculating EST, PST, GMT, CET, and JST, for example,
;; requires too much thought. That's what computers are for. And since
;; I'm usually sending email reminders of the meeting time, that's
;; what Emacs is for.  And that's what this code is for.

;;; Usage:
;;
;; (wwtime)
;;
;; wwtime will prompt for the time of day in the minibuffer. The time
;; entered will be inserted into the current buffer in each of the
;; time zones listed in wwtime-display. Note that the time can be
;; entered in any time zone, but will always be displayed in the time
;; zones listed in wwtime-display (and the time zone entered, even
;; if it is not a member of wwtime-display).
;;
;; The time displayed will be followed by a + or - to indicate tomorrow
;; or yesterday (with respect to the time zone entered).

;; (wwtime-convert)
;;
;; wwtime-convert prompts for a time of day and two time zones. It
;; converts the time from the first time zone to the second and displays
;; the result in the minibuffer.

;;; Example:
;;
;; Given the default settings, (wwtime) for "15:00 AKST" will insert:
;; "15:00AKST (07:00p EST, 04:00p PST, 00:00GMT+, 01:00CET+, 09:00JST+) "

;;; Customization:

;; There are five customization variables. As of version 1.1, you can
;; set these with the Emacs customization feature. Wwtime is in the
;; "Convenience" group under M-x customize.

;; wwtime-time-zones is the list of recognized time zones
;; wwtime-display is the list of time zones to display
;; wwtime-ampm is a list of time zones that use 12-hour am/pm format
;; wwtime-default-function is the function that calculates the default time
;; wwtime-time-zone-aliases maps long time zone names to one-word names

;;; Changes

;; v1.4
;;   No changes; just needed to sync with a bug on the wwtime web page.

;; v1.3
;;   Initialize wwtime-convert-history (really, this time!)
;;   Add time zone aliases (to convert "Eastern Standard Time" to "EST"
;;     automatically)
;;   Fold time zone names to uppercase (so that lowercase and mixed case
;;     names can be entered by the user)

;; v1.2
;;   Support swatch time (timezone '@')
;;   Initialize wwtime-convert-history
;;
;; v1.1
;;   Always display timezone entered
;;   Use emacs customize
;;   Added wwtime-convert
;;
;; v1.0
;;   First release

;;; Code:

(eval-when-compile
  (require 'cl))

(eval-and-compile
  ;; If customize isn't available just use defvar instead.
  (unless (fboundp 'defgroup)
    (defmacro defgroup  (&rest rest) nil)
    (defmacro defcustom (symbol init docstring &rest rest)
      `(defvar ,symbol ,init ,docstring))))

(defgroup wwtime nil
  "Display time of day in different time zones."
  :group  'convenience
  :prefix "wwtime-")

(defcustom wwtime-display '("EST" "PST" "GMT" "CET" "JST")
  "*The time zones to insert into the text.

The wwtime function will insert the specified time into the current
buffer in all of the time zones listed."
  :type '(repeat string)
  :group 'wwtime)

(defcustom wwtime-time-zones '(
			   ("ACST"  +9.5 "AU Central Standard Time")
			   ("ADT"   -3 "Atlantic Daylight Time")
			   ("AEST" +10 "AU Eastern Standard/Summer Time")
			   ("AKDT"  -8 "Alaska Standard Daylight Time")
			   ("AKST"  -9 "Alaska Standard Time")
			   ("AST"   -4 "Atlantic Standard Time")
			   ("AWST"  +8 "AU Western Standard Time")
			   ("BST"   +1 "British Summer Time")
			   ("CDT"   -5 "Central Daylight Saving Time")
			   ("CEST"  +2 "Central Europe Summer Time")
			   ("CET"   +1 "Central Europe Time")
			   ("CST"   -6 "Central Standard Time")
			   ("EDT"   -4 "Eastern Daylight Saving Time")
			   ("EEST"  +3 "Eastern Europe Summer Time")
			   ("EET"   +2 "Eastern Europe Time")
			   ("EST"   -5 "Eastern Standard Time")
			   ("GMT"    0 "Greenwich Mean Time")
			   ("HST"  -10 "Hawaiian Standard Time")
			   ("IST"   +1 "Irish Summer Time")
			   ("JST"   +9 "Japan Standard Time")
			   ("MDT"   -6 "Mountain Daylight Saving Time")
			   ("MSD"   +4 "Moscow Summer Time")
			   ("MSK"   +3 "Moscow Time")
			   ("MST"   -7 "Mountain Standard Time")
			   ("NZST" +12 "New Zealand Standard Time")
			   ("PDT"   -7 "Pacific Daylight Saving Time")
			   ("PST"   -8 "Pacific Standard Time")
			   ("WEST"  +1 "Western Europe Summer Time")
			   ("WET"    0 "Western Europe Time")
			   ("@"     +1 "Swatch time")
			   ("Y"    -12 "Time zone Y")
			   ("X"    -11 "Time zone X")
			   ("W"    -10 "Time zone W")
			   ("V"     -9 "Time zone V")
			   ("U"     -8 "Time zone U")
			   ("T"     -7 "Time zone T")
			   ("S"     -6 "Time zone S")
			   ("R"     -5 "Time zone R")
			   ("Q"     -4 "Time zone Q")
			   ("P"     -3 "Time zone P")
			   ("O"     -2 "Time zone O")
			   ("N"     -1 "Time zone N")
			   ("Z"      0 "Time zone Z")
			   ("A"     +1 "Time zone A")
			   ("B"     +2 "Time zone B")
			   ("C"     +3 "Time zone C")
			   ("D"     +4 "Time zone D")
			   ("E"     +5 "Time zone E")
			   ("F"     +6 "Time zone F")
			   ("G"     +7 "Time zone G")
			   ("H"     +8 "Time zone H")
			   ("I"     +9 "Time zone I")
			   ("K"    +10 "Time zone K")
			   ("L"    +11 "Time zone L")
			   ("M"    +12 "Time zone M")
			   )
  "*The set of recognized time zone abbreviations.

Each time zone is identified by a single-word abbreviation, an
offset from GMT (in hours) and a human-readable description.

For example, EST is GMT-5 and its entry is

   (\"EST\"   -5 \"Eastern Standard Time\")

The hour delta does not have to be an integer: Australian Central
Standard Time is GMT+9:30:

   (\"ACST\"  +9.5 \"AU Central Standard Time\")

You can assign any abbreviations your audience will
understand. Time zones are clearly locale-specific and I've simply
selected a bunch off the web."
  :type '(repeat (list (string :tag "Time zone     ")
		       (number :tag "Hours from GMT")
		       (string :tag "Description   ")))
  :group 'wwtime)

(defcustom wwtime-ampm '("EST" "CST" "MST" "PST" "AKST"
			 "EDT" "CDT" "MDT" "PDT" "AKDT")
  "*The set of time zones that use 12-hour (am/pm) notation.

All other time zones are presented in 24-hour notation."
  :type '(repeat string)
  :group 'wwtime)

(defcustom wwtime-time-zone-aliases '(
			   ("Eastern Standard Time" "EST")
			   ("Eastern Daylight Time" "EDT")
			   ("Central Standard Time" "CST")
			   ("Central Daylight Time" "CDT")
			   ("Mountain Standard Time" "MST")
			   ("Mountain Daylight Time" "MDT")
			   ("Pacific Standard Time" "PST")
			   ("Pacific Daylight Time" "PDT")
			   ("Alaska Standard Time" "AKST")
			   ("Alaska Daylight Time" "AKDT")
			   )
  "*A set of time zone aliases.

Some systems return a long form of the time zone name. In order for
wwtime to parse the time zone, a single-word abbreviation must be used
instead.

If your system returns a long name, add that name and the appropriate
abbreviated name to this list. It will automatically be translated into
the short name."
  :type '(repeat (list (string :tag "Time zone long name ")
		       (string :tag "Time zone short name")))
  :group 'wwtime)

(defcustom wwtime-default-function 'wwtime-default-next-hour
  "*A function to call to calculate the default time to convert.

Three functions are provided for you:

  wwtime-default-blank      defaults to empty
  wwtime-default-current    defaults to the current time of day
  wwtime-default-next-hour  defaults to the top of the next hour"
  :type 'function
  :group 'wwtime)

;; ====================================================================

(defun wwtime-default-blank ()
  "")

(defun wwtime-default-current ()
  (format "%s %s"
	  (substring (current-time-string) 11 16)
	  (wwtime-time-zone-name)))

(defun wwtime-default-next-hour ()
  (let ((hour (string-to-number (substring (current-time-string) 11 13))))
    (if (< hour 23)
	(format "%d:00 %s" (+ hour 1) (wwtime-time-zone-name))
      (format "00:00 %s" (wwtime-time-zone-name)))))

;; ====================================================================

(defvar wwtime-history nil
  "The history list for wwtime")

(defvar wwtime-convert-history nil
  "The history list for wwtime-convert")

(defun wwtime-time-zone-name ()
  "Return the time zone name.

The current-time-zone name is returned, possibly abbreviated from
the wwtime-time-zone-aliases."
  (let ((name (cadr (current-time-zone))))
    (if (assoc name wwtime-time-zone-aliases)
	(cadr (assoc name wwtime-time-zone-aliases))
      name)))

(defun wwtime-string-to-list (time-string)
  "Convert a time string into the internal representation used by
wwtime. The internal representation is a two element list where the
first element is the number of minutes past midnight and the second
element is the time zone. The number of minutes is interpreted as a
positive integer mod 24. A time zone must be a single word
abbreviation such as PST or EDT.."
  (let ((hour 0)
	(min 0)
	(parsed-ok t)
	(tz (wwtime-time-zone-name)))
    (cond
     ((string-match "^@\\([0-9]+\\)$" time-string)
      (setq min  (string-to-number (match-string 1 time-string)))
      (setq hour (floor (/ (* min 1.44) 60)))
      (setq min  (floor (- (* min 1.44) (* hour 60))))
      (setq tz   "@"))
     ((string-match "^\\([0-9]+\\):\\([0-9]+\\)\\(a\\|p\\|am\\|pm\\) \\([a-zA-Z@]+\\)$" time-string)
      (setq hour (string-to-number (match-string 1 time-string)))
      (setq min (string-to-number (match-string 2 time-string)))
      (if (and (string= (substring (match-string 3 time-string) 0 1) "p")
	       (not (= hour 12)))
	  (setq hour (+ hour 12)))
      (if (and (string= (substring (match-string 3 time-string) 0 1) "a")
	       (= hour 12))
	  (setq hour 0))
      (setq tz (upcase (match-string 4 time-string))))

     ((string-match "^\\([0-9]+\\):\\([0-9]+\\) \\([a-zA-Z@]+\\)$" time-string)
      (setq hour (string-to-number (match-string 1 time-string)))
      (setq min (string-to-number (match-string 2 time-string)))
      (setq tz (upcase (match-string 3 time-string))))

     ((string-match "^\\([0-9]+\\):\\([0-9]+\\)\\(a\\|p\\|am\\|pm\\)$" time-string)
      (setq hour (string-to-number (match-string 1 time-string)))
      (setq min (string-to-number (match-string 2 time-string)))
      (if (and (string= (substring (match-string 3 time-string) 0 1) "p")
	       (not (= hour 12)))
	  (setq hour (+ hour 12)))
      (if (and (string= (substring (match-string 3 time-string) 0 1) "a")
	       (= hour 12))
	  (setq hour 0)))

     ((string-match "^\\([0-9]+\\):\\([0-9]+\\)$" time-string)
      (setq hour (string-to-number (match-string 1 time-string)))
      (setq min (string-to-number (match-string 2 time-string))))
     (t
      (setq parsed-ok nil)))

    (if parsed-ok
	(list (+ (* hour 60) min) tz)
      nil)))

(defun wwtime-list-to-string (time)
  "Convert the time represented by 'time' into a string."
  (let* ((hour (floor (car time) 60))
	 (min  (- (car time) (* hour 60)))
	 (tz   (cadr time))
	 (delta "")
	 (ampm ""))

    (while (> hour 23)
      (setq hour (- hour 24))
      (setq delta "+"))

    (while (< hour 0)
      (setq hour (+ hour 24))
      (setq delta "-"))

    (if (member tz wwtime-ampm)
	(progn
	  (setq ampm "a ")
	  (if (>= hour 12)
	      (setq ampm "p "))
	  (if (> hour 12)
	      (setq hour (- hour 12)))))

    (if (string= tz "@")
	;; swatch time
	(format "@%03d" (round (/ (car time) 1.44)))
      (format "%02d:%02d%s%s%s" hour min ampm tz delta))))

(defun wwtime-list-to-gmt (time)
  "Convert the time represented by the time list to GMT"
  (let ((local-time (car time))
	(tz (cadr time)))
    (if (assoc tz wwtime-time-zones)
	(list (- local-time (* 60 (cadr (assoc tz wwtime-time-zones)))) "GMT")
      nil)))

(defun wwtime-change-time-zone (time tz)
  "Convert the time represented by the time list to a new time zone"
  (let ((gmt-time (car (wwtime-list-to-gmt time)))
	(min-offset 0))
    (if (assoc tz wwtime-time-zones)
	(list (+ gmt-time (* 60 (cadr (assoc tz wwtime-time-zones)))) tz)
      nil)))

;; ====================================================================

(defun wwtime ()
  "Read a time of day from the minibuffer and insert it into the
current buffer in the selected time zones."
  (interactive)
  (let* ((time-string (read-from-minibuffer
		       "Enter time (e.g. 13:00 EST): "
		       (funcall wwtime-default-function)
		       nil
		       nil
		       'wwtime-history))
	 (local-time   (wwtime-string-to-list time-string))
	 (local-tz     (cadr local-time))
	 (tz-list      wwtime-display)
	 (count        0))
    (if local-time
	(progn
	  (if (not (member local-tz tz-list))
	      (setq tz-list (nconc (list local-tz) tz-list)))
	  (setq count 0)
	  (while (nth count tz-list)
	    (progn
	      (cond
	       ((= count 1) (insert " ("))
	       ((> count 1) (insert ", ")))
	      (insert (wwtime-list-to-string
		       (wwtime-change-time-zone
			local-time (nth count tz-list))))
	      (setq count (+ count 1))
	      (if (= count (length tz-list))
		  (insert ") ")))))
      (message (format "%s isn't a time...on Earth, anyway." time-string)))))

(defun wwtime-convert ()
  "Read a time of day from the minibuffer and convert it into an
alternate time zone"
  (interactive)
  (let ((convert-string (read-from-minibuffer
			 "Enter time and new time zone (e.g. 13:00 EST PST): "
			 (funcall wwtime-default-function)
			 nil
			 nil
			 'wwtime-convert-history))
	(time-string "")
	(new-time-zone "")
	(local-time nil)
	(new-time nil))
    (if (string-match "^\\(.*\\) \\([a-zA-Z@]+\\)$" convert-string)
	(progn
	  (setq time-string (match-string 1 convert-string))
	  (setq new-time-zone (upcase (match-string 2 convert-string)))

	  ;; The time zone 'J' is a special case, it always means local time
	  (if (string= new-time-zone "J")
	      (setq new-time-zone (wwtime-time-zone-name)))

	  (setq local-time (wwtime-string-to-list time-string))
	  (if local-time
	      (progn
		(setq new-time
		      (wwtime-change-time-zone local-time new-time-zone))
		(if new-time
		    (if (string= time-string
				 (wwtime-list-to-string local-time))
			(message (format "%s is %s"
					 time-string
					 (wwtime-list-to-string new-time)))
		      (message (format "%s (%s) is %s"
				       time-string
				       (wwtime-list-to-string local-time)
				       (wwtime-list-to-string new-time))))
		  (message "%s isn't a recognized time zone." new-time-zone)))
	    (message (format "%s isn't a recognized time." time-string))))
      (message (format "I don't understand %s." convert-string)))))

;; wwtime.el ends here
