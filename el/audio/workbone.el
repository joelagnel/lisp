;;; Saved through ges-version 0.3.2dev at 2002-12-18 14:15
;;; ;;; From: Benjamin Drieu <bdrieu@april.org>
;;; ;;; Subject: workbone.el 1.12
;;; ;;; Newsgroups: gnu.emacs.sources
;;; ;;; Date: Wed, 18 Dec 2002 16:03:38 +0100
;;; ;;; Organization: APRIL - http://www.april.org/

;;; --=-=-=


;;; workbone.el is an emacs-based CD player wrapper.  It uses workbone, a
;;; text-based CD player and allows you to control CD playing within
;;; Emacs.  workbone.el may have less features than xmcd, but it runs with
;;; Emacs.  :-)

;;; Since last version, it now works with emacs21 and some typos have been
;;; fixed.

;;; ;;  You need workbone to use this program.  You also need the volume
;;; ;;  program to be able to change volume with keystrokes.  If you have
;;; ;;  an Internet connection and cddb.pl, you will have tracks titles on
;;; ;;  the mode line (which is *cool*).
;;; ;;
;;; ;;  Workbone : /ftp@sunsite.unc.edu:/pub/Linux/apps/sound/cdrom/curses
;;; ;;  Volume :   /ftp@sunsite.unc.edu:/pub/Linux/apps/sound/soundcard
;;; ;;  cddb.pl :  http://armin.emx.at/cddb/

;;; ;;  Usage is basically the same as the traditional workbone, which is
;;; ;;  controlled by the numeric pad.  The only difference is that all
;;; ;;  commands are prefixed by C-x w (w stands for workbone).
;;; ;;
;;; ;;  +----| number pad |----+
;;; ;;  |                      |
;;; ;;  |    []    ||    =>    |    7	 	8		9
;;; ;;  |                      |	^ Stop		^ Pause /Resume	^ Play
;;; ;;  |    <     ^^     >    |	4		5		6
;;; ;;  |                      |	^ Play previous	^ Restart	^ Play next
;;; ;;  |    <<    ..    >>    |	1 		2 		3
;;; ;;  |                      |	^ Back 15 '	^ Eject		^ Forward 15 '
;;; ;;  |    quit         ?    |	0   	  	 		.
;;; ;;  |                      |
;;; ;;  +----------------------+
;;; ;;                              C-x w + :   Set the sound up
;;; ;;                              C-x w - :   Set the sound down
;;; ;;				C-x w p n : Play track number n


;;; -- 
;;; o Benjamin Drieu:       bdrieu@april.org
;;;                         benj@grassouille.org
;;; o APRIL:                http://www.april.org/

;;; --=-=-=
;;; Content-Type: application/emacs-lisp
;;; Content-Disposition: attachment; filename=workbone.el
;;; Content-Transfer-Encoding: 8bit

;;; workbone.el --- Workbone interface for the Emacs editor

;; Copyright (C) 2000 by Benjamin Drieu

;; Author: Benjamin Drieu <bdrieu@april.org>
;; Keywords: tools

;; This file is NOT part of GNU Emacs.

;; This program as GNU Emacs are free software; you can redistribute
;; them and/or modify them under the terms of the GNU General Public
;; License as published by the Free Software Foundation; either
;; version 2, or (at your option) any later version.

;; They are distributed in the hope that they will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with them; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Commentary:

;;  $Id: workbone.el,v 1.12 2002/12/18 15:02:46 benj Exp $

;;  You need workbone to use this program.  You also need the volume
;;  program to be able to change volume with keystrokes.  If you have
;;  an Internet connection and cddb.pl, you will have tracks titles on
;;  the mode line (which is *cool*).
;;
;;  Workbone : /ftp@sunsite.unc.edu:/pub/Linux/apps/sound/cdrom/curses
;;  Volume :   /ftp@sunsite.unc.edu:/pub/Linux/apps/sound/soundcard
;;  cddb.pl :  http://armin.emx.at/cddb/

;;  Usage is basically the same as the traditional workbone, which is
;;  controlled by the numeric pad.  The only difference is that all
;;  commands are prefixed by C-x w (w stands for workbone).
;;
;;  +----| number pad |----+
;;  |                      |
;;  |    []    ||    =>    |    7	 	8		9
;;  |                      |	^ Stop		^ Pause /Resume	^ Play
;;  |    <     ^^     >    |	4		5		6
;;  |                      |	^ Play previous	^ Restart	^ Play next
;;  |    <<    ..    >>    |	1 		2 		3
;;  |                      |	^ Back 15 '	^ Eject		^ Forward 15 '
;;  |    quit         ?    |	0   	  	 		.
;;  |                      |
;;  +----------------------+
;;                              C-x w + :   Set the sound up
;;                              C-x w - :   Set the sound down
;;				C-x w p n : Play track number n

;; $Log: workbone.el,v $
;; Revision 1.12  2002/12/18 15:02:46  benj
;; - Fix typos
;;
;; Revision 1.11  2002/05/18 21:34:43  benj
;; - now (provide 'workbone)
;;
;; Revision 1.10  2002/05/05 13:26:40  benj
;; - fix stuff tu make it work with emacs21
;;
;; Revision 1.9  2000/11/20 10:46:24  benj
;; - did some cleanup
;;
;; Revision 1.8  2000/06/29 12:38:45  drieu
;;  - fiabilisation de workbone-init-cddb (si on ne trouve pas le titre,
;;    on ne va pas scanner la base de donnees a nouveau).
;;
;; Revision 1.7  1999/11/06 14:10:10  benj
;; - Fix a bug with workbone-eject
;;
;; Revision 1.6  1999/11/06 13:58:55  benj
;; - now use global-mode-string instead of an ugly dance
;; - fix soem bugs
;; - workbone-mode now knows how to behave when its workbone process is
;;   killed
;; - English typos fixed
;;

;;; Code:

;;  Custom variables (may be set by the user)

(defgroup workbone nil 
  "CD player for GNU Emacs."
  :tag "Workbone"
  :group 'applications)

(defcustom workbone-command "workbone"
  "*Program that is lauched to act with the audio cd"
  :group 'workbone
  :type '(string))

(defcustom workbone-options "-aq"
  "*Command added when the workbone command is launched"
  :group 'workbone
  :type '(string))

(defcustom workbone-volume-command "volume"
  "*Program that is lauched when the user wants to change volume"
  :group 'workbone
  :type '(string))

(defcustom workbone-cddb.pl "cddb.pl"
  "Program used to extract track names"
  :group 'workbone
  :type '(string))

(defcustom workbone-volume-string-to-match "Current volume: R = \\([0-9]*\\), L = \\([0-9]*\\)"
  "String to match to extract volume"
  :group 'workbone
  :type '(string))

(defcustom workbone-stop-string-to-match "stopped\|quit\|1A"
  "String to match when workbone says he stop"
  :group 'workbone
  :type '(string))

(defcustom workbone-track-number-to-match "[a-z]* #\\([0-9]*\\)"
  "*String that is matched when guessing track number
You may change it if workbone is improved by its author"
  :group 'workbone
  :type '(string))

(defcustom workbone-volume-number 40 "Volume used"
  :group 'workbone
  :type '(integer))

(defcustom workbone-volume-step 5
  "*Amount of steps that are added or substracted each time the volume is changed"
  :group 'workbone
  :type '(integer))

(defcustom workbone-stop-string "7" 
  "*String used to stop the program"
  :group 'workbone
  :type '(string))

(defcustom workbone-pause-string "8" 
  "*String used to pause the audio playing"
  :group 'workbone
  :type '(string))

(defcustom workbone-resume-string "8"
  "*String used to resume the ausin playing (same as workbone-pause-string)"
  :group 'workbone
  :type '(string))

(defcustom workbone-eject-string "2" 
  "*String used to eject the CD"
  :group 'workbone
  :type '(string))

(defcustom workbone-backward-string "1" 
  "*String used to go back 15'"
  :group 'workbone
  :type '(string))

(defcustom workbone-forward-string "3" 
  "*String used to go forward 15'"
  :group 'workbone
  :type '(string))

(defcustom workbone-play-string "9" 
  "*String used to start playing"
  :group 'workbone
  :type '(string))

(defcustom workbone-play-next-string "6" 
  "*String used to play next track"
  :group 'workbone
  :type '(string))

(defcustom workbone-play-previous-string "4"
  "*String used to play previous track"
  :group 'workbone
  :type '(string))

(defcustom workbone-restart-string "5"
  "*String used to restart playing one track"
  :group 'workbone
  :type '(string))

(defcustom workbone-quit-string "0"
  "*String used to quit the interface without stopping the music"
  :group 'workbone
  :type '(string))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;  Variables used by workbone (believe me, you don't want to know what they do !)

(defvar workbone-track-number 0 "Track number")
(defvar workbone-process nil "Workbone process")
(defvar workbone-track-list nil)
(defvar workbone-mode nil)
(defvar workbone-string "")
(defvar workbone-paused nil)
(defvar workbone-volume-buffer "*Temporary volume buffer*")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;  Keystrokes used by the workbone interface (hmmm... seems ugly to
;;  me, need more work)

(define-key (current-global-map) "\C-xw+" 'workbone-volume-up)
(define-key (current-global-map) "\C-xw-" 'workbone-volume-down)
(define-key (current-global-map) "\C-xw7" 'workbone-stop)
(define-key (current-global-map) "\C-xw8" 'workbone-pause-or-resume)
(define-key (current-global-map) "\C-xw9" 'workbone-play)
(define-key (current-global-map) "\C-xw5" 'workbone-restart)
(define-key (current-global-map) "\C-xw4" 'workbone-play-previous)
(define-key (current-global-map) "\C-xw6" 'workbone-play-next)
(define-key (current-global-map) "\C-xw1" 'workbone-backward)
(define-key (current-global-map) "\C-xw2" 'workbone-eject)
(define-key (current-global-map) "\C-xw3" 'workbone-forward)
(define-key (current-global-map) "\C-xw0" 'workbone-quit)
(define-key (current-global-map) "\C-xwp" 'workbone-play-nth)
(define-key (current-global-map) "\C-xwv" 'workbone-set-volume)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;  Fun stuff : all the code

;;;###autoload
(defun workbone ()
  "Play an audio cd using workbone."
  (interactive)
  (workbone-mode 1))


(defun workbone-mode (&optional arg)
  "Toggle reading of an audio cd using workbone-mode.

With a numeric argument, enable it if arg is positive."
  (interactive)
  (if
      (if (null arg) (not workbone-mode)
	(> (prefix-numeric-value arg) 0))
      (workbone-enable)
    (workbone-disable)))


(defun workbone-enable ()
  "Enable reading of an audio cd using workbone-mode."
  (interactive)
  (setq workbone-mode t)
  (or (assq 'workbone-mode minor-mode-alist)
      (setq minor-mode-alist
	    (cons minor-mode-alist '((workbone-mode workbone-string)))))
  (if (workbone-not-running)
      (if (not (workbone-start-process))
	  (progn
	    (princ "Workbone is not installed")
	    nil)
	(setq workbone-track-number 0)
	(workbone-set-volume workbone-volume-number)))
  (workbone-play))


(defun workbone-disable ()
  "Disable reading of an audio cd using workbone-mode."
  (interactive)
  (setq workbone-mode nil)
  (workbone-quit)
  (setq global-mode-string (delete 'workbone-string global-mode-string)))


(defun workbone-start-process ()
  "Launch the workbone process"
  (interactive)
  (setq workbone-process (start-process workbone-command nil 
					workbone-command workbone-options))
  (set-process-filter workbone-process 'workbone-filter))


(defun workbone-not-running ()
  "State if workbone is already running"
  (equal nil (get-process workbone-command)))


(defun workbone-send-request (request &optional n)
  "Send a request to the workbone process"
  (interactive "sRequest to send: ")
  (process-send-string (process-name workbone-process) request)
  (if (and n (> n 1))
      (workbone-send-request request (1- n))))


(defun workbone-change-status (string)
  "Change process status variable"
  (setq workbone-status string))


(defun workbone-play ()
  "Start playing"
  (interactive)
  (when workbone-mode
    (if (workbone-not-running)
	(workbone-start-process))
    (workbone-change-mode-line "Playing")
    (workbone-send-request workbone-play-string))
  (workbone-init-cddb))


(defun workbone-restart ()
  "Restart playing a track"
  (interactive)
  (workbone-init-cddb)
  (when workbone-mode
    (if (workbone-not-running)
	(workbone-start-process))
    (workbone-change-mode-line "Playing")
    (workbone-send-request workbone-restart-string)))


(defun workbone-play-next (&optional n)
  "Play next track"
  (interactive "P")
  (when workbone-mode
    (if (workbone-not-running)
	(workbone-start-process))
    (workbone-change-mode-line)
    (workbone-send-request workbone-play-next-string n)))


(defun workbone-play-previous (&optional n)
  "Play previous track"
  (interactive "P")
  (when workbone-mode
    (if (workbone-not-running)
	(workbone-start-process))
    (workbone-change-mode-line)
    (workbone-send-request workbone-play-previous-string n)))


(defun workbone-quit ()
  "Quit the workbone interface, and kill some stuff"
  (interactive)
  (setq workbone-track-list nil)
  (when workbone-mode
    (if (not (workbone-not-running))
	(and
	 (workbone-send-request workbone-stop-string)
	 (workbone-send-request workbone-quit-string)))))


(defun workbone-stop ()
  "Stop playing"
  (interactive)
  (when workbone-mode
    (when (workbone-not-running)
      (workbone-start-process))
    (setq workbone-track-list nil)
    (workbone-change-mode-line "Stopped")
    (workbone-send-request workbone-stop-string)))


(defun workbone-pause-or-resume ()
  "Pause or resume reading"
  (interactive)
  (when workbone-mode
    (if (workbone-not-running)
	(workbone-start-process))
    (if workbone-paused
	(progn
	  (setq workbone-paused nil)
	  (workbone-change-mode-line "Playing")
	  (workbone-send-request workbone-resume-string))
      (setq workbone-paused t)
      (workbone-change-mode-line "Pause")
      (workbone-send-request workbone-pause-string))))


(defun workbone-backward (&optional n)
  "Go backward 15 ' (or more)"
  (interactive "P")
  (when workbone-mode
    (if (workbone-not-running)
	(workbone-start-process))
    (workbone-send-request workbone-backward-string n)))
   

(defun workbone-eject ()
  "Eject the audio cd"
  (interactive)
  (when workbone-mode
    (if (workbone-not-running)
	(workbone-start-process))
    (workbone-send-request workbone-eject-string)
    (setq workbone-track-number 0)
    (setq workbone-track-list nil)
    (workbone-change-mode-line "Stopped")))


(defun workbone-forward (&optional n)
  "Go forward 15'' (or more)"
  (interactive "P")
  (when workbone-mode
    (if (workbone-not-running)
	(workbone-start-process))
    (workbone-send-request workbone-forward-string n)))


(defun workbone-play-nth (&optional n)
  "Play Nth track"
  (interactive "nPlay track: ")
  (when workbone-mode
    (if (> workbone-track-number n)
	(workbone-play-previous (- workbone-track-number n))
      (workbone-play-next (- n workbone-track-number)))))


(defun workbone-track-format ()
  (format "%d %s" workbone-track-number workbone-status))


(defun workbone-filter (proc string)
  "Filter of the workbone processus"
  (cond
   ((string-match workbone-track-number-to-match string)
    (let ((track-string (match-string 1 string)))
      (if (null track-string)
	  nil
	(setq local-track-number (string-to-number track-string))
	(if (not (equal local-track-number workbone-track-number))
	    (progn
	      (setq workbone-track-number local-track-number)
	      (workbone-change-mode-line))
	  nil))))
   ((string-match workbone-stop-string-to-match string))))


(defun workbone-change-mode-line (&optional arg)
  (when arg
    (setq workbone-status arg))
  (setq workbone-string (format " [ %s | %s ] " (workbone-make-title workbone-track-number) workbone-status))
  (force-mode-line-update))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; CDDB interface - you need cddb.pl to do that

(defun workbone-init-cddb ()
  (interactive)
  (unless (not (null workbone-track-list))
    (let ((result (shell-command-to-string workbone-cddb.pl)))
      (setq workbone-track-list
	    (if (string-match "no cddb entry found" result)
		'(("artist" . "Unknown artist"))
	      (workbone-parse-result result 0))))))


(defun workbone-parse-result (string start)
  (if (string-match "\\(track \\([0-9]\+\\)\\|artist\\|title\\): \\([^\n]+\\)" string start)
      (let ((name (or (match-string 2 string) (match-string 1 string))))
	(cons (cons name (match-string 3 string)) (workbone-parse-result string (match-end 3))))
    nil))


(defun workbone-make-title (&optional n)
  (format "%s: %s"
	  (or (cdr (assoc "artist" workbone-track-list))
	      "Unknown artist")
	  ;; (cdr (assoc "title" workbone-track-list))
	  (or (cdr (assoc (format "%d" n) workbone-track-list))
	      "Unknown title")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Volume control - you need volume to do that

(defun workbone-guess-volume ()
  "Guess the current volume"
  (interactive)
  ;;; Why not (start-process) ?
  (call-process workbone-volume-command nil 
		(get-buffer-create workbone-volume-buffer) nil)
  (set-buffer (get-buffer workbone-volume-buffer))
  (string-match workbone-volume-string-to-match (buffer-string))
  (setq workbone-volume-number (string-to-number 
				(match-string 1 (buffer-string))))
  (kill-buffer (get-buffer workbone-volume-buffer)))


(defun workbone-prompt-for-volume ()
  "Read volume from input"
  (let ((n (string-to-int 
	    (read-input 
	     (format "Set volume to (default %d): " workbone-volume-number)))))
    (if n n workbone-volume-number)))
      

(defun workbone-set-volume (&optional n)
  "Set volume"
  (interactive (list (workbone-prompt-for-volume)))
  (when (not n)
    (setq n 1))
  (if (and 
       (> n 0)
       (< n 100))
      (setq workbone-volume-number n))
  (workbone-update-volume))


(defun workbone-volume-down (&optional n)
  "Set volume higher"
  (interactive "P")
	(workbone-guess-volume)
  (when (not n)
    (setq n 1))
  (if (> workbone-volume-number 0)
      (setq workbone-volume-number (- workbone-volume-number 
				      (* n workbone-volume-step))))
  (workbone-update-volume))


(defun workbone-volume-up (&optional n)
  "Set volume lower"
  (interactive "P")
  (workbone-guess-volume)
  (when (not n)
    (setq n 1))
  (if (< workbone-volume-number 100)
      (setq workbone-volume-number (+ workbone-volume-number 
				      (* n workbone-volume-step))))
  (workbone-update-volume))


(defun workbone-update-volume ()
  (start-process workbone-volume-command nil workbone-volume-command
		 (number-to-string workbone-volume-number)))

(provide 'workbone)

;;; workbone.el ends here

;;; --=-=-=--

