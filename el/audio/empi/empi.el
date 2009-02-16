;;; EMPI.EL --- Emacs Media Player Interface

;; Copyright (C) 2004 R.Ramkumar

;; Author: 	R.Ramkumar <andyetitmoves@gmail.com>
;; Created: 	12 May 2004
;; Version: 	1.0
;; Keywords:	music

;; This file is (strangely) *NOT* part of GNU Emacs

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; A copy of the GNU General Public License can be obtained from this
;; program's author (send electronic mail to
;; <andyetitmoves@gmail.com>) or from the Free Software Foundation,
;; Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;; LCD Archive Entry:
;; empi|R.Ramkumar|<andyetitmoves@gmail.com>
;; |Emacs Media Player Interface
;; |$Date: 2004/05/12 10:51:06 $|$Revision: 1.1 $|~/packages/empi.el

;;; Commentary:

;; See empi-core.el for a detailed commentary.

;;; Code:

(require 'empi-core)

(defvar empi-map
  (let ((map (make-sparse-keymap)))
    (define-keys map
      [?z]		'empi-prev
      [?x]		'empi-play
      [?c]		'empi-pause
      [?v]		'empi-stop
      [?b]		'empi-next
      [?e]		'empi-enqueue
      [f1]		'empi-player-version
      [?s]		'empi-send-command
      [?i]		'empi-send-input
      [?|]		'empi-volume
      [?-]		'empi-balance
      [?j]		'empi-jump-to-item
      [?t]		'empi-jump-to-time
      [?a]		'empi-caption-mode
      [?m]		'empi-mode-line-control-mode
      [?p]		'empi-mode-line-playtime-mode
      [(control ?r)]	'empi-toggle-repeat
      [(control ?s)]	'empi-toggle-shuffle
      ) map)
  "The keymap for all EMPI commands.
The keys in the map are as follows:

\\{empi-map}")

;;;; Basic commands

;;;###autoload
(defun empi-prev ()
  "Play previous item in the EMPI playlist."
  (interactive)
  (empi-simple-action :plback))

;;;###autoload
(defun empi-play ()
  "Start or restart playing the current item in the EMPI playlist."
  (interactive)
  (empi-simple-action :play))

;;;###autoload
(defun empi-pause ()
  "Pause playback for the EMPI player."
  (interactive)
  (empi-simple-action :pause))

;;;###autoload
(defun empi-stop ()
  "Stop playback for the EMPI player."
  (interactive)
  (empi-simple-action :stop))

;;;###autoload
(defun empi-next ()
  "Play next item in the EMPI playlist."
  (interactive)
  (empi-simple-action :plnext))

(defvar empi-enqueue-history nil)

;;;###autoload
(defun empi-enqueue (fil)
  "Enqueue a file to the EMPI playlist."
  (interactive
   (let ((file-name-history empi-enqueue-history))
     (prog1
	 (list (read-file-name "Enqueue what: "))
       (setq empi-enqueue-history file-name-history))))
  (empi-simple-action :enqueue fil))

;;;###autoload
(defun empi-player-version ()
  "Print version information for the EMPI player."
  (interactive)
  (empi-send-command :version))

(empi-static-dep :qrepeat :repeat)

;;;###autoload
(defun empi-toggle-repeat (&optional arg)
  "Change whether the EMPI playlist is played in a circular fashion.
With ARG, make the playlist circular iff ARG is positive."
  (interactive "P")
  (with-empi-cache
    (empi-simple-action :repeat arg)
    (if (setq arg (empi-recov-query :qrepeat))
	(if (equal arg 0) (message "EMPI: repeat is now off")
	  (if (equal arg 1) (message "EMPI: repeat is now on")
	    (emperet "qrepeat"))))))

(empi-static-dep :qshuffle :shuffle)

;;;###autoload
(defun empi-toggle-shuffle (&optional arg)
  "Change whether the EMPI playlist is played randomly.
With ARG, make the playback random iff ARG is positive."
  (interactive "P")
  (with-empi-cache
    (empi-simple-action :shuffle arg)
    (if (setq arg (empi-recov-query :qshuffle))
	(if (equal arg 0) (message "EMPI: shuffle is now off")
	  (if (equal arg 1) (message "EMPI: shuffle is now on")
	    (emperet "qshuffle"))))))

;;; uses :jumptime
;;;###autoload
(defun empi-jump-to-time (secs)
  (interactive "sEnter time to jump to: ")
  (cond
   ((numberp secs))
   ((stringp secs)
    (let (num (parts (split-string secs ":")))
      (or parts (error "Invalid time syntax"))
      (setq num (string-to-number (car parts)))
      (and (setq parts (cdr parts))
	   (setq num (+ (* num 60) (string-to-number (car parts)))))
      (setq num (* num 1000))
      (and (setq parts (cdr parts))
	   (setq num (+ num (string-to-number (car parts)))))
      (and (cdr parts) (error "Invalid time syntax"))
      (setq secs num)))
   (t (error "Argument must be a number or a string")))
  (empi-simple-action :jumptime secs))

;;; uses :jumpitemnum :jumpitem
;;;###autoload
(defun empi-jump-to-item (item)
  (interactive "sEnter the number of name to where you want to jump: ")
  (cond
   ((numberp item) (empi-simple-action :jumpitemnum item))
   ((stringp item)
    (let (itno)
      (setq itno (string-to-number item))
      (if (and itno (> itno 0) (integerp itno))
	  (empi-simple-action :jumpitemnum itno)
	(empi-simple-action :jumpitem item))))
   (t (error "Argument must be a number or a string"))))

;;; uses :misc
;;;###autoload
(defun empi-send-input (str)
  "Send a raw command string to the EMPI player."
  (interactive "sEnter string to be sent: ")
  (with-current-buffer (get-buffer-create "*empi-output*")
    (save-excursion
      (setq str (empi-simple-action :misc str))
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert (format "%s" str))
      (toggle-read-only 1)
      (display-message-or-buffer (current-buffer)))))

;;;; Volume and balance controls

(defgroup empi-volume-balance nil
  "Volume and balance control for EMPI"
  :prefix "empi-vol-" :group 'empi)

(defcustom empi-vol-step 2
  "*Atomic positive integer to adjust EMPI volume by.
The volume to adjust by a singal keypress in the volume control
either up or down."
  :type 'natnum :group 'empi-volume-balance)

(defcustom empi-vol-fill-char ?>
  "*Character to draw the EMPI volume bar with."
  :type 'character :group 'empi-volume-balance)

(defcustom empi-bal-step 5
  "*Atomic positive integer to adjust EMPI balance by.
The balance to adjust by a singal keypress in the volume or balance control
either to the left or to the right."
  :type 'natnum :group 'empi-volume-balance)

(defcustom empi-bal-left-fill-char ?<
  "*Character to draw the left bias in the EMPI balance bar."
  :type 'character :group 'empi-volume-balance)

(defcustom empi-bal-right-fill-char ?>
  "*Character to draw the right bias in the EMPI balance bar."
  :type 'character :group 'empi-volume-balance)

(defcustom empi-bal-center-marker "###"
  "*The center marker string in the EMPI balance bar."
  :type 'string :group 'empi-volume-balance)

(defconst empi-fallback-screen-width 80)
(defconst empi-vol-min-width 100)

(defun empi-volume-string (prefix vol range)
  (let ((width (frame-parameter nil 'width)) valw)
    (or (wholenump width) (setq width empi-fallback-screen-width))
    (setq valw (ceiling (log (1+ range) 10)))
    (or (> (setq width (- width (length prefix) valw 2)) 0)
	(error "No width left to display volume, consider \
increasing screen width"))
    (concat prefix (format (format "%%%dd " valw) vol)
	    (make-string (/ (* vol width) range) empi-vol-fill-char))))

(empi-static-dep :qvolume :voladd :volsub :baladd :balsub)
(empi-static-dep :qbalance :baladd :balsub)

;;; uses :qvolume :voladd :volsub :balance
;;;###autoload
(defun empi-volume ()
  (interactive)
  (with-empi-cache-let ((msg t) rvol lvol range)
    (while msg
      (let (vols)
	(setq vols (empi-simple-query :qvolume))
	(unless (and (listp vols) (>= (length vols) 2)
		     (wholenump (setq lvol (car vols)))
		     (wholenump (setq rvol (cadr vols))))
	  (emperet "volume")))
      (setq range (max rvol lvol (or range empi-vol-min-width)))
      (setq msg (concat (empi-volume-string "L " lvol range) "\n"
			(empi-volume-string "R " rvol range)))
      (let ((evt (read-event msg)))
	(if (eq evt ?q)
	    (setq msg nil)
	  (unlogged-message "%s" msg)
	  (cond
	   ((eq evt 'up) (empi-recov-action :voladd empi-vol-step))
	   ((eq evt 'down) (empi-recov-action :volsub empi-vol-step))
	   ((eq evt 'left) (empi-recov-action :balsub empi-bal-step))
	   ((eq evt 'right) (empi-recov-action :baladd empi-bal-step))
	   ((not (eq evt 'f5)) (ding))))))))

(defconst empi-bal-min-half-width 100)

(defun empi-balance-string (bal rvar)
  (let ((width (frame-parameter nil 'width)) cnum valw)
    (or (wholenump width) (setq width empi-fallback-screen-width))
    (set rvar (max (abs bal) (or (symbol-value rvar) empi-bal-min-half-width)))
    (setq valw (1+ (ceiling (log (1+ (symbol-value rvar)) 10))))
    (setq width (/ (- width (length empi-bal-center-marker) valw 1) 2))
    (or (> width 0) (error "No width left to display balance, consider \
shortening the center marker or increasing screen width"))
    (setq cnum (/ (* (abs bal) width) (symbol-value rvar)))
    (concat (format (format "%%%dd " valw) bal)
	    (make-string (if (< bal 0) (- width cnum) width) ? )
	    (if (< bal 0) (make-string cnum empi-bal-left-fill-char))
	    empi-bal-center-marker
	    (if (> bal 0) (make-string cnum empi-bal-right-fill-char)))))

(empi-static-dep :qbalance :balance :baladd :balsub)

;;; uses :balance
;;;###autoload
(defun empi-balance ()
  (interactive)
  (with-empi-cache-let ((msg t) bal range)
    (while msg
      (setq bal (empi-simple-query :qbalance))
      (unless (numberp bal) (emperet "balance"))
      (setq msg (empi-balance-string bal 'range))
      (let ((evt (read-event msg)))
	(if (eq evt ?q)
	    (setq msg nil)
	  (unlogged-message "%s" msg)
	  (cond
	   ((or (eq evt 'left) (eq evt 'down))
	    (empi-recov-action :balsub empi-bal-step))
	   ((or (eq evt 'right) (eq evt 'up))
	    (empi-recov-action :baladd empi-bal-step))
	   ((eq evt ?0)
	    (empi-recov-action :balance 0))
	   ((not (eq evt 'f5)) (ding))))))))

;;;; Automatic caption rendering

(defun randcharstr () (char-to-string (+ (random 95) 32)))

(defun randstr (len)
  (let ((i len) (str ""))
    (while (> len 0)
      (setq str (concat str (randcharstr)))
      (setq len (1- len))) str))

;;;###autoload
(defun empi-test-title-length (&optional prefix)
  (interactive "sEnter prefix: ")
  (or prefix (setq prefix ""))
  (let ((test-str (concat prefix (randcharstr))) (resok -1) (totlen 0) (tries 0)
	minlen (maxlen 0) (lname (frame-parameter nil 'name)))
    (set-frame-name "")
    (condition-case nil
	(while t
	  (while (< resok 2)
	    (set-frame-name test-str)
	    (if (y-or-n-p (concat "Can you see till \""
				  (if (> (length test-str) 5)
				      (concat "..." (substring test-str -5))
				    test-str) "\" "))
		(if (= resok 1) (setq resok 2) (if (= resok -1) (setq resok 0)))
	      (if (= resok 0) (setq resok 3) (if (= resok -1) (setq resok 1))))
	    (or (string= (frame-parameter nil 'name) test-str)
		(error "Frame title changed unexpectedly, ensure that \
no automatic title updation is on"))
	    (and (not (= resok 2))
		 (setq test-str
		       (if (= resok 0)
			   (concat test-str (randcharstr))
			 (if (string= test-str "")
			     (error "Display area too small!")
			   (substring test-str 0 -1))))))
	  (setq resok (length test-str))
	  (setq totlen (+ totlen resok))
	  (setq tries (1+ tries))
	  (and (or (not minlen) (< resok minlen)) (setq minlen resok))
	  (and (> resok maxlen) (setq maxlen resok))
	  (if (< resok (length prefix))
	      (error "Prefix too long, try with a shorter one")
	    (setq test-str (concat prefix (randstr (- resok (length prefix))))))
	  (setq resok -1))
      (quit
       (if (> tries 0)
	   (message "Number of samples analyzed: %d
The maximum title length was %d.
The minimum was %d.\nMaybe you should rely on %d."
		    tries maxlen minlen (/ totlen tries))
	 (message "You should have carried on, try again!"))))
    (set-frame-name lname)))

(require 'easy-mmode)

(defvar empi-caption-mode nil)
(defvar empi-caption-scroll t)
(defvar empi-caption-scroll-threshold 100)
(defvar empi-caption-scroll-step 1)
(defvar empi-caption-end-indicator " *** ")
(defvar empi-caption-update-interval 1)
(defvar empi-caption-redisplay-interval 3)
(defvar empi-caption-capture-all t)
(defvar empi-caption-capture-dynamic nil)

(defvar empi-caption-scroll-pos 0)

(defun empi-caption-scroll (head title)
  (let ((thres 0))
    (or (and (stringp empi-caption-scroll) (string= empi-caption-scroll "a"))
	(setq thres (length head)))
    (setq head (concat head title))
    (and empi-caption-scroll
	 (if (< (length head) empi-caption-scroll-threshold)
	     (setq empi-caption-scroll-pos 0)
	   (setq head (concat head empi-caption-end-indicator))
	   (setq empi-caption-scroll-pos
		 (mod (+ empi-caption-scroll-pos empi-caption-scroll-step)
		      (- (length head) thres)))
	   (setq head
		 (concat (if (> thres 0) (substring head 0 thres))
			 (substring head (+ empi-caption-scroll-pos thres))
			 (and (> empi-caption-scroll-pos thres)
			      (substring head thres
					 empi-caption-scroll-pos)))))) head))

(defvar empi-caption-defaults nil)
(defvar empi-caption-playtime nil)
(defvar empi-caption-title nil)

(defsubst empi-set-frame-name (frame name)
  (modify-frame-parameters frame (list (cons 'name name))))

;;;###autoload
(defun empi-caption-redisplay ()
  (interactive)
  (when (and empi-caption-capture-dynamic
	     (not empi-caption-capture-all)
	     (not (eq (selected-frame) (caar empi-caption-defaults))))
    (empi-set-frame-name (caar empi-caption-defaults)
			 (cdar empi-caption-defaults))
    (let ((this (selected-frame)))
      (setcar empi-caption-defaults (cons this (frame-parameter this 'name)))))
  (let ((rest empi-caption-defaults) item last)
    (while rest
      (setq item (car rest))
      (if (frame-live-p (car item))
	  (empi-set-frame-name
	   (car item)
	   (if (or empi-caption-title empi-caption-playtime)
	       (empi-caption-scroll empi-caption-playtime empi-caption-title)
	     (cdr item)))
	(if last
	    (setcdr last (cdr rest))
	  (setq empi-caption-defaults (cdr empi-caption-defaults))))
      (setq last rest)
      (setq rest (cdr rest)))))

(defun time-ms-format (time)
  (when (numberp time)
    (setq time (/ time 1000))
    (format "%d:%02d" (/ time 60) (mod time 60))))

(defun empi-playtime-string ()
  (with-empi-cache
    (let ((curt (time-ms-format (empi-query :qtime)))
	  (tott (time-ms-format (empi-query :qsonglength)))
	  (state (empi-query :pausedp)))
      (setq state (if (numberp state)
		      (if (not (= state 0)) 2
			(if (numberp (setq state (empi-query :playingp)))
			    (if (= state 0) 0 1)))))
      (and (or state curt tott)
	   (concat (if (and curt (not (and state (= state 0))))
		       (concat "[" curt (and tott (concat "/" tott)) "] "))
		   (when (and state (not (= state 1)))
		     (concat "< " (if (= state 2) "Paused" "Stopped")
			     " > ")))))))

;;;###autoload
(defun empi-caption-update ()
  (interactive)
  (with-empi-cache
    (setq empi-caption-playtime (empi-playtime-string))
    (setq empi-caption-title (empi-query :qtitle))
    (or empi-caption-title empi-caption-playtime
	(error "Unable to retrieve any frame parameter"))
    (when (and empi-caption-title (not (stringp empi-caption-title)))
      (setq empi-caption-title nil)
      (emperet "title"))))

(defvar empi-caption-update-timer nil)
(defvar empi-caption-redisplay-timer nil)

(defun empi-caption-mode-off ()
  (when empi-caption-update-timer
    (mapc '(lambda (frame)
	     (and (frame-live-p (car frame))
		  (empi-set-frame-name (car frame) (cdr frame))))
	  empi-caption-defaults)
    (remove-hook 'after-make-frame-functions 'empi-add-captured-frame)
    (setq empi-caption-defaults nil)
    (empi-update-unregister empi-caption-update-timer)
    (empi-update-unregister empi-caption-redisplay-timer)
    (setq empi-caption-update-timer nil)))

(defun empi-add-captured-frame (frame)
  (setq empi-caption-defaults
	(cons (cons frame (frame-parameter frame 'name))
	      empi-caption-defaults)))

(defun empi-caption-mode-on ()
  (unless empi-caption-update-timer
    (setq empi-caption-defaults
	  (mapcar '(lambda (frame) (cons frame (frame-parameter frame 'name)))
		  (if empi-caption-capture-all
		      (progn
			(if empi-caption-capture-dynamic
			    (add-hook 'after-make-frame-functions
				      'empi-add-captured-frame))
			(frame-list))
		    (list (selected-frame)))))
    (empi-caption-update)
    (combine-empi-update-registers
     (setq empi-caption-update-timer
	   (empi-update-register 'empi-caption-update
				 empi-caption-update-interval nil))
     (setq empi-caption-redisplay-timer
	   (empi-update-register 'empi-caption-redisplay
				 empi-caption-redisplay-interval nil)))))

(defun toggle-variable (var arg)
  (set var (if arg (if (> (prefix-numeric-value arg) 0) t)
	     (if (symbol-value var) nil t))))

(defun empi-caption-set-restart (sym val)
  (set sym val)
  (and empi-caption-mode
       (progn
	 (empi-caption-mode-off)
	 (empi-caption-mode-on))))

(defgroup empi-caption nil
  "Automatic caption rendering for EMPI."
  :prefix "empi-caption-" :group 'empi)

(define-minor-mode empi-caption-mode
  "Toggle EMPI caption mode.
With arg, turn mode on iff arg is positive.
When enabled, displays and updates the title bar in window systems.
The title bar is set to the playtime followed by the current title."
  :global t :group 'empi-caption :require 'empi
  (if empi-caption-mode
      (empi-caption-mode-on)
    (empi-caption-mode-off)))

(defcustom empi-caption-scroll t
  "*Scroll EMPI caption if it exceeds a specified length.
A value of t scrolls the title part of the caption (excluding the time string)
if long enough. nil means that no scrolling is done. Any other value scrolls the
entire caption. See also `empi-caption-scroll-threshold'."
  :type '(choice (const :tag "None" nil)
		 (const :tag "Title" t)
		 (other :tag "All" 'all)) :group 'empi-caption)

(defcustom empi-caption-scroll-threshold 100
  "*Maximum length of EMPI caption rendered without scrolling.
There is no good way (that I know, that is) to find out the number of characters
the title bar can accomodate without clipping in graphical window systems, hence
this variable. The default in terminals is a good one.
Use `empi-test-title-length' for a semi-automatic method to determine a value."
  :type 'natnum :group 'empi-caption
  :initialize '(lambda (var val)
		 (or (boundp var)
		     (set var (if window-system 100 (- (frame-width) 10))))))

(defcustom empi-caption-scroll-step 1
  "*Number of characters to shift the EMPI caption for each scroll.
To change the speed of scrolling, use `empi-caption-redisplay-interval'."
  :type 'natnum :group 'empi-caption)

(defcustom empi-caption-end-indicator " *** "
  "*String to append to EMPI caption while scrolling.
The string specified marks the end of the caption when scrolling is on.
This is not used when scrolling is off or not required."
  :type 'string :group 'empi-caption)

(defcustom empi-caption-update-interval 1
  "*Scale for interval between updates of EMPI caption.
The actual value in seconds is `empi-update-reference-interval'/<this value>.
This is independent of the speed with which the display is refreshed,
see `empi-caption-redisplay-interval'. Don't use nil to disable updates,
instead use `empi-caption-mode'."
  :type 'natnum :group 'empi-caption :set 'empi-caption-set-restart)

(defcustom empi-caption-redisplay-interval 3
  "*Scale for interval between display refresh of the EMPI caption.
The actual value in seconds is `empi-update-reference-interval'/<this value>.
If scrolling is on, this value corresponds to the scrolling speed when it is
required. Otherwise, any value other than `empi-caption-update-interval' is
sub-optimal."
  :type 'natnum :group 'empi-caption :set 'empi-caption-set-restart)

(defcustom empi-caption-capture-all t
  "*If non-nil, EMPI auto caption update occurs for all frames.
Otherwise, just the selected frame is updated.
See `empi-caption-capture-dynamic' for when these frames are listed."
  :type 'boolean :group 'empi-caption :set 'empi-caption-set-restart)

(defcustom empi-caption-capture-dynamic nil
  "*If non-nil, EMPI caption update frames are dynamically determined.
That is to say, the frames updated based on the value of
`empi-caption-capture-all' depend dynamically on the frames visible or the
selected frame. Otherwise, all frames or the selected frame at the time of
activation of the auto caption mode is taken for updation."
  :type 'boolean :group 'empi-caption :set 'empi-caption-set-restart)

;;;; Mode line playback control

(defgroup empi-mode-line nil
  "Mode line interface for EMPI."
  :prefix "empi-mode-line-" :group 'empi)

(define-minor-mode empi-mode-line-control-mode
  "Toggle EMPI mode-line control mode.
With arg, turn mode on iff arg is positive.
When enabled, adds buttons on the mode-line to control EMPI."
  nil nil nil :global t :group 'empi-mode-line :require 'empi
  (force-mode-line-update))

(defface empi-mode-line-control-item-face
  '((((background light)) (:background "lightyellow"))
    (((background dark)) (:background "grey28")))
  "Face for each button of the EMPI control on the mode-line."
  :group 'empi-mode-line)

(defun empi-mode-line-string (str action help)
  (propertize
   str 'local-map (list 'keymap
			(list 'mode-line 'keymap '(down-mouse-1 . ignore)
			      (cons 'mouse-1 action)))
   'face 'empi-mode-line-control-item-face
   'help-echo (concat "mouse-1: " help)))

(defun empi-half-space ()
  (propertize " " 'display '((space . (:width 0.5)))))

(defun empi-mode-line-item (str action help)
  (concat (empi-mode-line-string
	   (concat (empi-half-space) str (empi-half-space)) action help)
	  (propertize " " 'display '((space . (:width 0.5))))))

(defvar empi-mode-line-control-string
  (concat (empi-mode-line-item "<" 'empi-prev "previous")
	  (empi-mode-line-item "P" 'empi-play "play")
	  (empi-mode-line-item "#" 'empi-pause "pause")
	  (empi-mode-line-item "S" 'empi-stop "stop")
	  (empi-mode-line-item ">" 'empi-next "next")))

(add-to-list 'global-mode-string
	     '(empi-mode-line-control-mode empi-mode-line-control-string) t)
(force-mode-line-update)

;; Silence the compiler
(defvar empi-mode-line-playtime-update-interval 1)

(defvar empi-mode-line-playtime-value nil)
(defvar empi-mode-line-playtime-timer nil)

(defun empi-mode-line-playtime-value-update ()
  (interactive)
  (setq empi-mode-line-playtime-value (empi-playtime-string))
  (force-mode-line-update))

(define-minor-mode empi-mode-line-playtime-mode
  "Toggle EMPI mode-line playtime mode.
With arg, turn mode on iff arg is positive.
When enabled, shows current playtime on the mode-line."
  :global t :group 'empi-mode-line :require 'empi
  (if empi-mode-line-playtime-mode
      (unless empi-mode-line-playtime-timer
	(empi-mode-line-playtime-value-update)
	(setq empi-mode-line-playtime-timer
	      (empi-update-register 'empi-mode-line-playtime-value-update
				    empi-mode-line-playtime-update-interval
				    nil)))
    (when empi-mode-line-playtime-timer
      (empi-update-unregister empi-mode-line-playtime-timer)
      (setq empi-mode-line-playtime-timer nil)
      (force-mode-line-update))))

(defun empi-mode-line-playtime-set-restart (sym val)
  (setq sym val)
  (and empi-mode-line-playtime-mode
       (empi-mode-line-playtime-mode -1)
       (empi-mode-line-playtime-mode 1)))

(add-to-list 'global-mode-string
	     '(empi-mode-line-playtime-mode empi-mode-line-playtime-value) t)

(defcustom empi-mode-line-playtime-update-interval 1
  "*Scale for interval between updates of EMPI mode-line playtime display.
The actual value in seconds is `empi-update-reference-interval'/<this value>.
Don't use nil to disable updates, instead use `empi-mode-line-playtime-mode'."
  :type 'natnum :group 'empi-mode-line
  :set 'empi-mode-line-playtime-set-restart)

(provide 'empi)

;;; EMPI.EL ends here
