;;; etalk-tyrn -- tyrant mode support under etalk
;;
;; Copyright (C) 1994, 1995, 1996, 1998 Free Software Foundation
;;
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
;; program's author (see below) or write to:
;;
;;              The Free Software Foundation, Inc.
;;              675 Mass Ave.
;;              Cambridge, MA 02139, USA.
;;
;; Please send bug reports, etc. to zappo@gnu.ai.mit.edu.

;;; Commentary:
;;   This file contains the etalk-tyrant code for initializing tyrant
;; mode within a talk process.  Handles basic data transfers in the
;; form of messages.  Valid messages current consist of minibuffer
;; messages, Quit command, secret game information, mouse activity,
;; and query responses.

;;; $Id: etalk-tyrn.el,v 1.9 1998/10/11 02:17:51 zappo Exp $

;;; Code:

(defvar etalk-tyrant-local-buildsequence nil
  "Used to build a keysequence to send to remote.")

(defvar etalk-tyrant-remote-buildsequence nil
  "Used to build a keysequence to interpret from remote.")

(defvar etalk-tyrant-enabled-console nil
  "Variable for minor-mode alist, defining when the tyrant console is enabled.
This provides default value.")

(defvar tyrant-player1-hook nil
  "Hook used when starting up player 1's player space.
This should only be set by games who believe that they will be used under
tyrant mode.")

(defvar tyrant-player2-hook nil
  "Hook used when starting up player 2's player space.
This should only be set by games who believe that they will be used under
tyrant mode.")

(defvar tyrant-mouse-function nil
  "Function used to evaluate mouse actions locally in a buffer.")

(defvar tyrant-query-response nil
  "Remotes response to a question sent over the net.")

;; add mode etalk-tyrannical-mode to mode alist
(or (assq 'etalk-tyrannical-mode minor-mode-alist)
    (setq minor-mode-alist
	  (cons '(etalk-tyrannical-mode " Tyrant") minor-mode-alist)))

;; add mode etalk-tyrant-enabled-console to mode alist
(or (assq 'etalk-tyrant-enabled-console minor-mode-alist)
    (setq minor-mode-alist
	  (cons '(etalk-tyrant-enabled-console " T-Con") minor-mode-alist)))

;; This is a duplicate needed to provide keymaps if tyrant-mode is not
;; used with etalk (ai version for instance...)
(defun etalk-superbind-alpha (keymap fn)
  "In KEYMAP bind all occurances of alphanumeric keys to FN.
An alphanumeric key is any value between 0 and 128"
  (let ((key "\00"))
    (aset key 0 0)
    (while (< (aref key 0) 128)
      (define-key keymap key fn)
      (aset key 0 (1+ (aref key 0))))))

(defvar etalk-tyrant-map nil
  "Keymap used when taking over keyboard to interact with functions.")

(defvar etalk-tyrant-c-map nil
  "Tyrant key sequences after a c-c.")

(if etalk-tyrant-c-map
    ()
  (setq etalk-tyrant-c-map (make-sparse-keymap))
  (define-key etalk-tyrant-c-map "\C-c" 'etalk-usurp-tyrant-keyed)
  (define-key etalk-tyrant-c-map "m" 'tyrant-send-minibuffer-message))

(if etalk-tyrant-map
    ()
  (setq etalk-tyrant-map (make-keymap))
  (suppress-keymap etalk-tyrant-map)
  (etalk-superbind-alpha etalk-tyrant-map 'etalk-tyrant-fork-keypress)
  (define-key etalk-tyrant-map "\C-c"  etalk-tyrant-c-map)
  (define-key etalk-tyrant-map "\e"    nil)
;; Note to self.  Find some other way to bind this in.
;;  (define-key etalk-tyrant-map "\C-h"  'etalk-tyrant-help)
  (define-key etalk-tyrant-map "\C-l"  nil)
  (define-key etalk-tyrant-map "\C-x"  nil)
  ;; the following binding is to prevent accidental process deletion
  ;; upon deletion of a tyranted buffer.
  (define-key etalk-tyrant-map "\C-xk" 'etalk-usurp-tyrant-keyed)
  (define-key etalk-tyrant-map "\C-z"  nil)

  (if (string-match "XEmacs" emacs-version)
      (progn
	)
    ;; Here, rebind all the arrows and the like to produce things
    ;; which we want to see (keys like "^M" instead of [return]
    ;;
    ;; Hopefully, a key pres to C-m wont go to [return] brining us
    ;; back to c-m
    (define-key etalk-tyrant-map [return] "\C-m")
    (define-key etalk-tyrant-map [right]  "\C-f")
    (define-key etalk-tyrant-map [left]   "\C-b")
    (define-key etalk-tyrant-map [up]     "\C-p")
    (define-key etalk-tyrant-map [down]   "\C-n")

    ;; Now lets try to redifine the keypad (kp0 -> kp9, and some others)
    ;; to be the actual numbers.  this is good for games with
    ;; strategic diagonal movement
    (define-key etalk-tyrant-map [kp-0] "0")
    (define-key etalk-tyrant-map [kp-1] "1")
    (define-key etalk-tyrant-map [kp-2] "2")
    (define-key etalk-tyrant-map [kp-3] "3")
    (define-key etalk-tyrant-map [kp-4] "4")
    (define-key etalk-tyrant-map [kp-5] "5")
    (define-key etalk-tyrant-map [kp-6] "6")
    (define-key etalk-tyrant-map [kp-7] "7")
    (define-key etalk-tyrant-map [kp-8] "8")
    (define-key etalk-tyrant-map [kp-9] "9")
    (define-key etalk-tyrant-map [kp-enter] "\C-m")
    (define-key etalk-tyrant-map [kp-add] "+")
    (define-key etalk-tyrant-map [kp-subtract] "-")
    (define-key etalk-tyrant-map [kp-multiply] "*")
    (define-key etalk-tyrant-map [kp-divide] "/")
    (define-key etalk-tyrant-map [kp-decimal] ".")

    ;; Here, lets make sure we can support the mouse in tyrant
    ;; mode, thus, grab all mouse type events, and let the
    ;; tyrant-mouse manager handle this stuff.
    (define-key etalk-tyrant-map [down-mouse-1] 'etalk-tyrant-handle-mouse)
    (define-key etalk-tyrant-map [down-mouse-2] 'etalk-tyrant-handle-mouse)
    (define-key etalk-tyrant-map [down-mouse-3] 'etalk-tyrant-handle-mouse)
    (define-key etalk-tyrant-map [mouse-1] 'etalk-tyrant-handle-mouse)
    (define-key etalk-tyrant-map [mouse-2] 'etalk-tyrant-handle-mouse)
    (define-key etalk-tyrant-map [mouse-3] 'etalk-tyrant-handle-mouse)
    (define-key etalk-tyrant-map [drag-mouse-1] 'etalk-tyrant-handle-mouse)
    (define-key etalk-tyrant-map [drag-mouse-2] 'etalk-tyrant-handle-mouse)
    (define-key etalk-tyrant-map [drag-mouse-3] 'etalk-tyrant-handle-mouse)
    (define-key etalk-tyrant-map [double-down-mouse-1] 'etalk-tyrant-handle-mouse)
    (define-key etalk-tyrant-map [double-down-mouse-2] 'etalk-tyrant-handle-mouse)
    (define-key etalk-tyrant-map [double-down-mouse-3] 'etalk-tyrant-handle-mouse)
    (define-key etalk-tyrant-map [double-mouse-1] 'etalk-tyrant-handle-mouse)
    (define-key etalk-tyrant-map [double-mouse-2] 'etalk-tyrant-handle-mouse)
    (define-key etalk-tyrant-map [double-mouse-3] 'etalk-tyrant-handle-mouse)
    )
  )

;; The following list of variables describes what should be local variables
(defvar tyrant-point nil
  "Point where a tyrant function has left the cursor.")

(defvar etalk-borrowed-keymap nil
  "Keymap tyrant mode has taken over.")

(defvar tyrant-call-interpreter nil
  "Function which can be set to evaluate message type 1 (game data).
Allows games to play as master/slave where initiator knows some piece
of global/random data like cards of word-thing pieces.")

(defvar etalk-tyrannical-usurped nil
  "Non nil when something has be usurped by tyrant-mode.
Asynchonous function calls use this so they know what is going on when
a tyranted game is ending.")

(defvar etalk-tyrannical-mode nil
  "Minor Mode variable.")

(defvar etalk-tyrant-enabled-console nil
  "For games with Turn, this flag is set by game to control playing.
When this is NIL, key presses are not interpretted, otherwise play can
proceede as normal.")

(defvar etalk-tyrant-brief-help nil
  "Hook to control what kind of help is displayed.
If this is bound, then this function is called, otherwise the default
`tyrant-mode' help is used.")

(defvar etalk-tyrant-quit-string nil
  "Hook to control what happens when a tyranted mode is quit.
If this is bound, or set to a string, this string is displayed.
If not, the standard tyrant mode info is displayed.")

(defvar tyrant-not-turn-message nil
  "Non nil means the specified string is used instead of the default.")

(defvar tyrant-player-index nil
  "Flag indicating what player the current buffer represents.
Player 1 means initiator, player 2 means you agreed to a game request")

(defvar tyrant-turn nil
  "Local version of who's turn it is during a tyrant controlled game.
This allows tyrant-format to easily print strings based on turn so the
game need not figure it out.")

(defvar tyrant-dont-restore-position nil
  "Set in a tyrant key press to allow games to control the cursor position.")

;; functions

(defun etalk-tyrannical-mode ()
  "Minor mode taking over keypresses of innocent modes.
All keypresses are then interpreted based on the current mode if tyranicism.
If the tyranted mode is hooked into etalk, then keypresses are sent to
be interpreted by the remote.  If it is in AI mode, then nothing else
is done with the keypress, but the ai portion is then allowed control."

  ;; not interactive!

  ;; set a mark to keep track of cursor
  (if (not (markerp 'tyrant-point))
      (progn
	(make-local-variable 'tyrant-point)
	(setq tyrant-point (make-marker))))
  (set-marker tyrant-point (point-max))

  ;; mark this buffer as used by etalk
  (make-local-variable 'etalk-tag)
  (setq etalk-tag t)

  ;; get map generated by local mode
  (make-local-variable 'etalk-borrowed-keymap)
  (setq etalk-borrowed-keymap (current-local-map))

  ;; take over the buffer
  (use-local-map etalk-tyrant-map)

  ;; take over the etalk filter
  (make-local-variable 'etalk-filter-message)
  (setq etalk-filter-message nil)
  (make-local-variable 'etalk-filter-message-type)
  (setq etalk-filter-message-type 0)

  (if etalk-tyrant-imprisoned-process
      (progn
	(set-process-filter etalk-tyrant-imprisoned-process
			    'etalk-tyrant-filter-proc)
	(set-process-buffer etalk-tyrant-imprisoned-process
			    (current-buffer))))

  ;; flag for some procedures when the game is usurped.
  (make-local-variable 'etalk-tyrannical-usurped)
  (setq etalk-tyrannical-usurped nil)

  ;; use this variable to determine if you are a supressed mode.
  (make-local-variable 'etalk-tyrannical-mode)
  (setq etalk-tyrannical-mode t)

  ;; ALWAYS START WITH CONSOLE ENABLED
  ;; The etalk-filter-proc always sets the answeree to nil
  ;; in this case.  If you write something with no distinct "turns"
  ;; then you must set this variable back in your program.
  (make-local-variable 'etalk-tyrant-enabled-console)
  (setq etalk-tyrant-enabled-console t)
  (run-hooks 'etalk-tyrannical-hook)
  )

(defun tyrant-format (formatstring &rest args)
  "Format FORMATSTRING using with % tokens, then ARGS are processed as normal.
Valid %s are:
  %u : player one's username.
  %U : player two's username.
  %n : player one's preferred name.
  %N : player two's preferred name.
  %p : choose player based on local variable tyrant-turn.
  %P : choose player based on local variable tyrant-turn and use pref name"
  (let ((extensions (list (list ?u (if (equal tyrant-player-index 1)
				       etalk-announce-as
				     etalk-tyrant-imprisoned-user))
			  (list ?U (if (equal tyrant-player-index 2)
				       etalk-announce-as
				     etalk-tyrant-imprisoned-user))
			  (list ?n (if (equal tyrant-player-index 1)
				       etalk-preferred-name
				     etalk-tyrant-imprisoned-preferred-name))
			  (list ?N (if (equal tyrant-player-index 2)
				       etalk-preferred-name
				     etalk-tyrant-imprisoned-preferred-name))
			  (list ?p (if tyrant-turn
				       (if (equal tyrant-turn 1)
					   (if (equal tyrant-player-index 1)
					       etalk-announce-as
					     etalk-tyrant-imprisoned-user)
					 (if (equal tyrant-player-index 2)
					     etalk-announce-as
					   etalk-tyrant-imprisoned-user))
				     "name"))
			  (list ?P (if tyrant-turn
				       (if (equal tyrant-turn 1)
					   (if (equal tyrant-player-index 1)
					       etalk-preferred-name
					     etalk-tyrant-imprisoned-preferred-name)
					 (if (equal tyrant-player-index 2)
					     etalk-preferred-name
					   etalk-tyrant-imprisoned-preferred-name))
				     "name")))))
    (eval (cons 'Sformat (cons (quote extensions) (cons formatstring args))))))

(defun tyrant-player1 ()
  "Book keeping proc to set some things up for tyrant mode."
  (make-local-variable 'tyrant-player-index)
  (setq tyrant-player-index 1))

(defun tyrant-player2 ()
  "Book keeping proc to set some things up for tyrant mode."
  (make-local-variable 'tyrant-player-index)
  (setq tyrant-player-index 2))

(defun etalk-tyrant-help ()
  "Universal help function for all tyrant mode games.
Depends on having certain variables defined by the game itself."

  (interactive)
  (if etalk-tyrant-imprisoned-process
      (process-send-string etalk-tyrant-imprisoned-process ""))
  (if (equal last-command 'etalk-tyrant-help)
      (describe-mode)
    (if (and (boundp 'etalk-tyrant-brief-help) etalk-tyrant-brief-help)
	(message etalk-tyrant-brief-help)
      (message
"Tyranted game: `C-c m' send message, `C-c C-c' quit `C-h' for describe-mode."
     ))))

(defun etalk-usurp-tyrant (&optional exit-message)
  "Usurps the tyrant, and bring back the talk buffers and show EXIT-MESSAGE.
Kills the process linking this buffer with etalk."
;; old comment
;  "Restore the buffer the way it was before.  Includes restoring the
;filter and the buffer keymap and some other things."

  (interactive)
  (use-local-map etalk-borrowed-keymap)
  (setq etalk-tyrannical-usurped t)
  ;; remove the process linking us together
  (if etalk-tyrant-imprisoned-process
      (delete-process etalk-tyrant-imprisoned-process))

  ;; ok  The buffer is restored.  Now we hope that the innocent mode
  ;; can remove itself and regenerate the etalk screen
  (setq etalk-tyrannical-mode nil)
  ;; Ok now pause meaningfully on the window.  process actions happen
  ;; as usuall, and will be seen after hitting return.
  (if exit-message
      (read-string (format "%s : Hit return to continue." exit-message))
    (read-string "Hit return to continue.")))

(defun etalk-usurp-tyrant-keyed (&optional nosend)
  "Keybinding which will usurp the current tyrant process.
This will also send the usurp command to the remote machine unless
NOSEND in non-nil."
  
  (interactive)
  (if (and (not nosend) etalk-tyrant-imprisoned-process
	   (= (process-exit-status etalk-tyrant-imprisoned-process) 0))
      (process-send-string etalk-tyrant-imprisoned-process "\C-c\C-a"))
  (let ((qmess (if (boundp 'etalk-tyrant-quit-string)
		   (if (fboundp 'etalk-tyrant-quit-string)
		       (funcall 'etalk-tyrant-quit-string)
		     (if (stringp etalk-tyrant-quit-string)
			 etalk-tyrant-quit-string
		       nil)))))
    (etalk-usurp-tyrant qmess)
    (if (equal tyrant-opponent-type 'etalk)
	(etalk-setup-windows))))

(defun tyrant-send-message (msg)
  "Send message MSG to a remote process for secret game data."

  (if etalk-tyrant-imprisoned-process
      (process-send-string etalk-tyrant-imprisoned-process
			   (concat "\C-[" msg "\n"))))

(defun tyrant-send-query-answer (msg)
  "Send message MSG to a remote process expounding on a questions answer."

  (if etalk-tyrant-imprisoned-process
      (process-send-string etalk-tyrant-imprisoned-process
			   (concat "\C-\\" msg "\n"))))

(defun tyrant-send-minibuffer-message (msg)
  "Send message MSG to remote tyranted process to make message in minibuffer."
  
  (interactive "sMessage: ")
  (if etalk-tyrant-imprisoned-process
      (process-send-string etalk-tyrant-imprisoned-process
			   (concat "" etalk-announce-as ": " msg "\n"))))

(defun etalk-tyrant-handle-mouse (event)
  "Handle mouse EVENTs by encoding and sending them to the remote process."
  (interactive "e")
  (if (not etalk-tyrant-enabled-console)
      (if (and (boundp 'tyrant-not-turn-message) tyrant-not-turn-message)
	  (error tyrant-not-turn-message)
	(error "It isn't your turn!")))
  (etalk-tyrant-do-mouse-thing event)
  (track-mouse
    (while (progn
	     (setq event (read-event))
	     (or (mouse-movement-p event)
		 (eq (car-safe event) 'switch-frame)))
      (etalk-tyrant-do-mouse-thing event))
    )
  (etalk-tyrant-do-mouse-thing event)
  )

(defun etalk-tyrant-do-mouse-thing (event)
  "Do the mouse thing with EVENT.
The handler handles drag type things."
  (let ((pos (event-start event)))
    (if etalk-tyrant-imprisoned-process
	(process-send-string etalk-tyrant-imprisoned-process
			     (concat "\C-]"
				     ;; we can use %S because this
				     ;; function will never be called by
				     ;; 18 because we use using mousey
				     ;; things.  The message, however,
				     ;; will be interpretable
				     (format "%d %S %S"
					     (posn-point pos)
					     (event-basic-type event)
					     (event-modifiers event))
				     "\n")))
    (if tyrant-mouse-function
	(eval (list tyrant-mouse-function
		    (posn-point pos)
		    '(event-basic-type event)
		    '(event-modifiers event)))
      )
    
    ;; check for exit condition
    (if (and etalk-tyrannical-usurped (equal tyrant-opponent-type 'etalk))
	(etalk-setup-windows)
      (if (not (equal tyrant-opponent-type 'etalk)) ;we are playing a program!
	  (if (not etalk-tyrant-enabled-console) ;it is thier turn
	      (progn
		(sit-for 0)
		;; this is fset in tyrn-ai
		(tyrant-opponent-type)))))
    ))
  
(defun etalk-tyrant-interpolate-keypress (the-key send)
  "Interpolates a keypress based on one new character in THE-KEY.
Returns t if it is used, and nil if it is not.  If SEND is t, then
send sequence to remote"

  ;; buildsequence is null when not given as argument.
  (let* ((sequence (concat etalk-tyrant-local-buildsequence
			   (char-to-string the-key)))
	 (ttfk-sym (lookup-key etalk-borrowed-keymap sequence)))
    (if ttfk-sym
	(progn
	  (cond
	   ;; if ttfk-symbol is a keymap, then read the next char untill done.
	   ((keymapp ttfk-sym)
	    (setq etalk-tyrant-local-buildsequence ttfk-sym))
	   ;; if symbol is a function, then get function and send sequence
	   ;; to remote, but only if we are not called from a filter
	   ((fboundp ttfk-sym)
	    (if (and send (equal tyrant-opponent-type 'etalk))
		(process-send-string etalk-tyrant-imprisoned-process sequence))
	    (setq etalk-tyrant-local-buildsequence nil)
	    (if (not send)
		(setq last-input-char the-key))
	    (funcall ttfk-sym))
	   ;; just in case something weird happens
	   (t
	    (setq etalk-tyrant-local-buildsequence nil)
	    (etalk-tyrant-help)))
	  t)
      nil)
    ))
 
(defun etalk-tyrant-fork-keypress ()
  "Find the command needed by a keypress.
When applicable send to the remote tyranted process as well."

  (interactive)
  (if (not etalk-tyrant-enabled-console)
      (if (and (boundp 'tyrant-not-turn-message) tyrant-not-turn-message)
	  (error tyrant-not-turn-message)
	(error "It isn't your turn!")))

  ;; interpret it, and do help if we fail
  (if (not (etalk-tyrant-interpolate-keypress last-input-char t))
      (etalk-tyrant-help))

  ;; check for exit condition
  (if (and etalk-tyrannical-usurped (equal tyrant-opponent-type 'etalk))
      (etalk-setup-windows)
    (if (not (equal tyrant-opponent-type 'etalk)) ;we are playing a program!
	(if (not etalk-tyrant-enabled-console) ;it is thier turn
	    (progn
	      (sit-for 0)
	      ;; this is fset in tyrn-ai
	      (tyrant-opponent-type))))) )

(defun etalk-tyrant-filter-proc (process output)
  "Filters keypresses sent from the remote tyrant PROCESS in OUTPUT.
Runs procedures based on those sequences."

  (let ((tf-oldbuffer (window-buffer (selected-window)))
	(tf-pbuffer (process-buffer process)))

    (save-excursion
      (set-buffer tf-pbuffer)
      (move-marker tyrant-point (point))) ;backup point mover when
					;nothing is happening.
  (let ((usurped nil)
	(tyrant-dont-restore-position nil)
	(tf-strlen (length output))
	(tf-cnt 0)
	(tf-tchar))
    (while (and ( < tf-cnt tf-strlen) (not usurped))
      (setq tf-tchar (string-to-char (substring output tf-cnt (+ 1 tf-cnt))))
      (setq tf-cnt (+ 1 tf-cnt))
      (cond
       ((equal tf-tchar ?\C-c)
	(save-excursion
	  (set-buffer tf-pbuffer)
	  (setq etalk-filter-message "")
	  (setq etalk-filter-message-type 0)))
       ((equal tf-tchar 29)	;C-] mice
	(save-excursion
	  (set-buffer tf-pbuffer)
	  (setq etalk-filter-message "")
	  (setq etalk-filter-message-type 2)))
       ((equal tf-tchar 27)	;C-[ message
	(save-excursion
	  (set-buffer tf-pbuffer)
	  (setq etalk-filter-message "")
	  (setq etalk-filter-message-type 1)))
       ((equal tf-tchar 28)	;C-\ query response
	(save-excursion
	  (set-buffer tf-pbuffer)
	  (setq etalk-filter-message "")
	  (setq etalk-filter-message-type 3)))
       (t
	;; if there is a message, _and_ if that message has a type
	;; then continue on, else, wait for message to have a type.
	(if (save-excursion (and (set-buffer tf-pbuffer)
				 etalk-filter-message
				 etalk-filter-message-type))
	    (save-excursion
	      (set-buffer tf-pbuffer)
	      (if (and (equal etalk-filter-message "")
		       (equal tf-tchar 1))
		  (progn
		    (setq etalk-filter-message nil)
		    (setq usurped t))
		(progn
		  ;; when we hit the CR ending of message text.
		  (if (equal tf-tchar 10)
		      (progn
			(cond
			 ((equal etalk-filter-message-type 0)
			  ;; dont display if in minibuffer!!!!!!!
			  (setq etalk-filter-message-type nil)
			  ;; set type to nil _first_ in case of
			  ;; interuption due to sleeps in tyranted code
			  (if (equal (selected-window)
				     (minibuffer-window))
			      ()
			    (message etalk-filter-message))
			  (setq etalk-filter-message nil))
			 ((equal etalk-filter-message-type 1)
			  (save-excursion
			    ;; set type to nil _first_ in case of
			    ;; interuption due to sleeps in tyranted code
			    (setq etalk-filter-message-type nil)
			    (funcall tyrant-call-interpreter
				     etalk-filter-message)
			    (move-marker tyrant-point (point))
			    (setq etalk-filter-message nil)))
			 ((equal etalk-filter-message-type 2)
			  ;; parse the message for mouse commands
			  (let ((omd (match-data)))
			    (if (string-match
				 "\\([0-9]+\\) \\([-a-z0-9]+\\)"
				 etalk-filter-message)
				(let ((pos (string-to-int
					    (substring
					     etalk-filter-message
					     (match-beginning 1)
					     (match-end 1))))
				      (evnt (read
					     (substring
					      etalk-filter-message
					      (match-beginning 2)
					      (match-end 2))))
				      (mod (read
					    (substring
					     etalk-filter-message
					     (match-end 2)))))
				  ;; Now that it is parsed, lets
				  ;; do some mouse things.
				  (if tyrant-mouse-function
				      (eval (list tyrant-mouse-function
						  pos
						  'evnt
						  'mod)))))
			    (move-marker tyrant-point (point))
			    (save-match-data omd)))
			 ((equal etalk-filter-message-type 3)
			  (setq tyrant-query-response
				etalk-filter-message)
			  (setq etalk-filter-message nil))
			 
			 ))
		    (setq etalk-filter-message
			  (concat etalk-filter-message
				  (char-to-string tf-tchar)))
		    (setq tyrant-dont-restore-position nil)))))
	  (if (equal tf-tchar ?\C-h)
	      (message "Remote is reading help!!!")
	    (save-excursion
	      (set-buffer tf-pbuffer)
	      (etalk-tyrant-interpolate-keypress tf-tchar nil)
	      (move-marker tyrant-point (point))))))))
    (if tyrant-dont-restore-position (set-buffer tf-oldbuffer))
    (if (save-excursion
	  (set-buffer tf-pbuffer)
	  etalk-tyrannical-usurped)
	(etalk-setup-windows)
      ;; else
      (if (not (equal (selected-window) (minibuffer-window)))
	  (etalk-move-realpoint tf-pbuffer
			       (save-excursion
				 (set-buffer tf-pbuffer)
				 tyrant-point))))
    (if (save-excursion
	  (set-buffer tf-pbuffer)
	  usurped)
	(progn
	  (etalk-usurp-tyrant-keyed t)	;keyed with NOSEND set to print quit
	  (etalk-setup-windows))))))

(provide 'etalk-tyrn)
;;; etalk-tyrn ends here
