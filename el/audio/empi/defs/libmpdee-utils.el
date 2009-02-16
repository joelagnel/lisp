;;; LIBMPDEE-UTILS.EL --- Utilities for the libmpdee package

;; Copyright (C) 2004 R.Ramkumar

;; Author: 	R.Ramkumar <andyetitmoves@gmail.com>
;; Created: 	16 May 2004
;; Version: 	1.0
;; Keywords:	mpd, music

;; This file is (strangely) *NOT* part of GNU Emacs

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; A copy of the GNU General Public License can be obtained from this program's
;; author (send electronic mail to <andyetitmoves@gmail.com>) or from the Free
;; Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;; LCD Archive Entry:
;; libmpdee-utils|R.Ramkumar|andyetitmoves@gmail.com
;; |Utilities for the libmpdee package
;; |$Date$|$Revision$|~/packages/libmpdee-utils.el

;;; Code:

(require 'libmpdee)
(require 'flexi-print)

(defun mpd-jump-to-time (conn time)
  (let ((status (mpd-get-status conn)))
    (and status (and (mpd-seek conn (aref status 6) time) status))))

(defun mpd-skip-time (conn time)
  (assert-numberp time)
  (let ((status (mpd-get-status conn)))
    (and status
	 (mpd-seek conn (aref status 6)
		   (if (< (setq time (+ (aref status 9) time)) 0) 0 time))
	 status)))

(defsubst mpd-jump-to-time-msec (conn time)
  (mpd-jump-to-time conn (floor (/ time 1000))))

(defsubst mpd-decrease-volume (conn by)
  (mpd-adjust-volume conn (- by)))

(defsubst mpd-get-volume (conn)
  (aref (mpd-get-status conn) 0))

(defun mpd-compat-play (conn)
   (interactive (list mpd-inter-conn))
   (let ((status (and (eq (mpd-connection-status conn) 'ready)
		      (mpd-get-status conn))))
     (and (mpd-play conn (and status (aref status 6))) status)))

(defun mpd-compat-stop (conn)
  (interactive (list mpd-inter-conn))
  (let ((status (and (eq (mpd-connection-status conn) 'ready)
		     (mpd-get-status conn))) state (ret t))
    (setq state (if status (aref status 8) 'stop))
    (if (eq state 'play) (setq ret (mpd-pause conn)))
    (or (eq state 'stop) (setq ret (mpd-seek conn (aref status 6))))
    (and ret status)))

(defsubst mpd-convert-compat-pausedp (status)
  (and (eq (aref status 8) 'pause)
       (not (= (aref status 9) 0))))

(defun mpd-compat-pausedp (conn)
  (interactive (list mpd-inter-conn))
  (mpd-convert-compat-pausedp (mpd-get-status conn)))

(defun mpd-fs-enqueue (conn file)
  (and (stringp file)
       (setq file (file-truename file))
       (mpd-enqueue conn
		    (and (not (string= (file-name-as-directory mpd-db-root)
				       (file-name-as-directory file)))
			 (file-relative-name file mpd-db-root)))))

(defun mpd-time-ms-format (time)
  (if (not (numberp time))
      nil
    (format "%d:%02d" (/ time 60) (mod time 60))))

(defun mpd-song-data-query (data query)
  (let ((offset 0))
    (while (and (< offset (mpd-song-data-length))
		(not (eq (compare-strings (aref mpd-song-data offset) nil nil
					  query nil nil t) t)))
      (setq offset (1+ offset)))
    (and (not (= offset (mpd-song-data-length)))
	 (if (= offset (eval-when-compile
			 (find-key-field "Time" mpd-song-data)))
	     (mpd-time-ms-format (aref data offset))
	   (aref data offset)))))

(defvar mpd-song-data-flexi-scheme
  (eval-when-compile
    (flexi-print-compile
     :default '(lambda ()
		 (mpd-song-data-query flexi-print-cookie flexi-print-query)))))

(defun mpd-format-title (conn fmt &optional pos)
  (or pos (setq pos (aref (mpd-get-status conn) 6)))
  (let ((flexi-print-cookie (car (mpd-get-playlist-entry conn pos))))
    (flexi-print mpd-song-data-flexi-scheme fmt)))

(defsubst mpd-pausedp (conn)
  (eq (aref (mpd-get-status conn) 8) 'pause))

(defsubst mpd-playingp (conn)
  (eq (aref (mpd-get-status conn) 8) 'play))

(defsubst mpd-get-elapsed-time (conn)
  (aref (mpd-get-status conn) 9))

(defsubst mpd-repeat-flag (conn)
  (aref (mpd-get-status conn) 1))

(defsubst mpd-random-flag (conn)
  (aref (mpd-get-status conn) 2))

;;;###autoload
(defun mpd-enqueue-from-log (conn)
  (interactive (list mpd-inter-conn))
  (with-free-buffer
    (forward-line 0)
    (if (looking-at "\\w+ +[0-9]+ +[0-9]+:[0-9]+ : \
\\(?:added\\|updating\\) \\(.*\\)")
	(let ((file (match-string 1)))
	  (if (mpd-enqueue conn file)
	      (message "Enqueued file %s" file)
	    (error "Unable to enqueue file %s: %s" file
		   (mpd-get-last-error conn))))
      (error "No file on current line"))))

(provide 'libmpdee-utils)

;;; LIBMPDEE-UTILS.EL ends here
