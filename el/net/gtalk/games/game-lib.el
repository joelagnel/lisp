;;; game-lib.el --- common routines for multiuser games under etalk
;;
;; Author: Eric M. Ludlam <zappo@gnu.ai.mit.edu>
;; Keywords: games, extensions
;;
;; Copyright (C) 1994, 1995, 1996, 1997, 1999 Free Software Foundation
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
;;

;;; $Id: game-lib.el,v 1.12 1999/08/26 11:52:15 zappo Exp $

;;; Commentary:
;;   This source was written to supply a cleaner and more functional
;;   approach to writing games for emacs talk.  These routines supply
;;   a single spot for providing user-supplied parameters that can
;;   effect all games, specifically, colors, and tyrant control.

;;;   This package doesn't require tyrant-mode because all calls are
;;   checked for use of tyrant variables before running.

;; this variable is duplicated in etalk-tyrn.  this doesn't matter,
;; but provides better compiler messages
;;; Code:
(defvar game-lib-xemacs-p (string-match "XEmacs" emacs-version)
  "How any game can know the difference between emacs and XEmacs.")

;; From custom web page for compatibility between versions of custom:
(eval-and-compile
  (condition-case ()
      (require 'custom)
    (error nil))
  (if (and (featurep 'custom) (fboundp 'custom-declare-variable))
      nil ;; We've got what we needed
    ;; We have the old custom-library, hack around it!
    (defmacro defgroup (&rest args)
      nil)
    (defmacro custom-add-option (&rest args)
      nil)
    (defmacro defface (var values doc &rest args)
      (` (make-face ` (, var))))
    (defmacro defcustom (var value doc &rest args)
      (` (defvar (, var) (, value) (, doc))))))

(defgroup etalk-game nil
  "Games that can be played under etalk."
  :prefix "game-lib"
  :group 'etalk)

(defvar tyrant-turn nil
  "Variable which will always be used in each game as the turn variable. 
All games which use game-lib, or etalk-tyrn must use this
to keep track of whos turn it is (1 or 2)")

(defvar tyrant-query-response nil
  "Variable set by `etalk-tyrant-filter-proc' when remote answers a question. 
Should never be set by programs!.")

(defvar game-lib-replace t
  "Mode variable controlling if inserted characters are replaced.
In most cases, we wish to replace characters for games, rarely,
we need a little hook in so we may insert (end of lines, etc).  The
drawing function will insert characters when placing them at the end
of a line despite the value of this variable.")

(defvar game-lib-use-colors (and  (>= (string-to-int emacs-version) 19)
				  window-system)
  "Nil means you never wish to use colors, set to t to force colors.")

(defun game-lib-load-color (sym l-fg l-bg d-fg d-bg &optional bold italic underline)
  "Create a color for SYM with a L-FG and L-BG color, or D-FG and D-BG.
Optionally make BOLD, ITALIC, or UNDERLINE if applicable.  If the
background attribute of the current frame is determined to be light
{white, for example} then L-FG and L-BG is used.  If not, then D-FG
and D-BG is used.  This will allocate the colors in the best possible
mannor.  This will allow me to store multiple defaults and dynamically
determine which colors to use."
  (if window-system
      (let* ((params (frame-parameters))
	     (disp-res (if (fboundp 'x-get-resource)
			   (if game-lib-xemacs-p
			       (x-get-resource ".displayType" "DisplayType" 'string)
			     (x-get-resource ".displayType" "DisplayType"))
			 nil))
	     (display-type
	      (cond (disp-res (intern (downcase disp-res)))
		    ((and (fboundp 'x-display-color-p) (x-display-color-p)) 'color)
		    (t 'mono)))
	     (bg-res (if (fboundp 'x-get-resource)
			 (if (eval-when-compile game-lib-xemacs-p)
			     (x-get-resource ".backgroundMode" "BackgroundMode" 'string)
			   (x-get-resource ".backgroundMode" "BackgroundMode"))
		       nil))
	     (bgmode
	      (cond (bg-res (intern (downcase bg-res)))
		    ((let* ((bgc (or (cdr (assq 'background-color params))
				     (if (eval-when-compile game-lib-xemacs-p)
					 (x-get-resource ".background"
							 "Background" 'string)
				       (x-get-resource ".background"
						       "Background"))
				     ;; if no other options, default is white
				     "white"))
			    (bgcr (if (eval-when-compile game-lib-xemacs-p)
				      (color-instance-rgb-components
				       (make-color-instance bgc))
				    (x-color-values bgc)))
			    (wcr (if (eval-when-compile game-lib-xemacs-p)
				     (color-instance-rgb-components
				      (make-color-instance "white"))
				   (x-color-values "white"))))
		       (< (apply '+ bgcr) (/ (apply '+ wcr) 3)))
		     'dark)
		    (t 'light)))	;our default
	     (set-p (function (lambda (face-name resource)
				(if game-lib-xemacs-p
				    (x-get-resource 
				     (concat face-name ".attribute" resource)
				     (concat "Face.Attribute" resource)
				     'string)
				  (x-get-resource 
				   (concat face-name ".attribute" resource)
				   (concat "Face.Attribute" resource)))
				)))
	     (nbg (cond ((eq bgmode 'dark) d-bg) 
			(t l-bg)))
	     (nfg (cond ((eq bgmode 'dark) d-fg)
			(t l-fg))))

	(if (not (eq display-type 'color))
	    ;; we need a face of some sort, so just make due with default
	    (progn
	      (copy-face 'default sym)
	      (if bold (condition-case nil
			   (make-face-bold sym)
			 (error (message "Cannot make face %s bold!" 
					 (symbol-name sym)))))
	      (if italic (condition-case nil
			     (make-face-italic sym)
			   (error (message "Cannot make face %s italic!"
					   (symbol-name sym)))))
	      (set-face-underline-p sym underline)
	      )
	  ;; make a colorized version of a face.  Be sure to check Xdefaults
	  ;; for possible overrides first!
	  (let ((newface (make-face sym)))
	    ;; For each attribute, check if it might already be set by Xdefaults
	    (if (and nfg (not (funcall set-p (symbol-name sym) "Foreground")))
		(set-face-foreground newface nfg))
	    (if (and nbg (not (funcall set-p (symbol-name sym) "Background")))
		(set-face-background newface nbg))
	
	    (if bold (condition-case nil
			 (make-face-bold newface)
		       (error (message "Cannot make face %s bold!"
				       (symbol-name sym)))))
	    (if italic (condition-case nil
			   (make-face-italic newface)
			 (error (message "Cannot make face %s italic!"
					 (symbol-name newface)))))
	    (set-face-underline-p newface underline)
	    )))))

;;; If we have a window system, load in cool colors to use on the game board
(if (and game-lib-use-colors  
	 (>= (string-to-int emacs-version) 19) 
	 window-system)
    (if (fboundp 'custom-declare-variable)
	(progn
	  (defface game-lib-player1-face '((((class color)) (:foreground "red")))
	    "Face used to display player 1."
	    :group 'etalk-game)
	  (defface game-lib-player2-face '((((class color) (background light))
					    (:foreground "blue"))
					   (((class color) (background dark))
					    (:foreground "yellow")))
	    "Face used to display player 2."
	    :group 'etalk-game)
	  (defface game-lib-player1-face-R '((((class color))
					      (:foreground "black" :background "red")))
	    "Face used to display player 1."
	    :group 'etalk-game)
	  (defface game-lib-player2-face-R '((((class color) (background light))
					      (:foreground "white" :background "blue"))
					     (((class color) (background dark))
					      (:foreground "black" :background "yellow")))
	    "Face used to display player 2."
	    :group 'etalk-game))
      (game-lib-load-color 'game-lib-player1-face "red" nil "red" nil nil)
      (game-lib-load-color 'game-lib-player2-face "blue" nil "yellow" nil nil)
      (game-lib-load-color 'game-lib-player1-face-R "black" "red" "black" "red" nil)
      (game-lib-load-color 'game-lib-player2-face-R "white" "blue" "black" "yellow" nil)
      )
  ;; make sure this is set to the system... just in case
  (setq game-lib-use-colors nil))

(defun game-lib-add-mouse-support (keymap)
  "Add mouse support to a game.
It puts a call to `game-lib-handle-mouse' into KEYMAP entries for
`game-lib-handle-mouse' which will be used to handle the mouse events.
This must be done this way so mouse-related actions can be sent over
the network.  The event [mouse] can't be transferred as a character.

  `game-lib-handle-mouse' will call the buffer localally set variable
`tyrant-mouse-function', which should receive the list (p e m) where P
is the position in the buffer, and e is the event type (`mouse-1',
`mouse-2', `mouse-movement') and m is a list of modifiers (down, click,
drag, etc) Your average game will probably skip many of these types."

  (if (>= (string-to-int emacs-version) 19)
      (progn
	(define-key keymap [down-mouse-1] 'game-lib-handle-mouse)
	(define-key keymap [down-mouse-2] 'game-lib-handle-mouse)
	(define-key keymap [down-mouse-3] 'game-lib-handle-mouse)
	(define-key keymap [mouse-1] 'game-lib-handle-mouse)
	(define-key keymap [mouse-2] 'game-lib-handle-mouse)
	(define-key keymap [mouse-3] 'game-lib-handle-mouse)
	(define-key keymap [drag-mouse-1] 'game-lib-handle-mouse)
	(define-key keymap [drag-mouse-2] 'game-lib-handle-mouse)
	(define-key keymap [drag-mouse-3] 'game-lib-handle-mouse)
	(define-key keymap [double-down-mouse-1] 'game-lib-handle-mouse)
	(define-key keymap [double-down-mouse-2] 'game-lib-handle-mouse)
	(define-key keymap [double-down-mouse-3] 'game-lib-handle-mouse)
	(define-key keymap [double-mouse-1] 'game-lib-handle-mouse)
	(define-key keymap [double-mouse-2] 'game-lib-handle-mouse)
	(define-key keymap [double-mouse-3] 'game-lib-handle-mouse)

	)))

(defun game-lib-handle-mouse (event)
  "Handle a mouse EVENT by gleening the important information from the event."
  (interactive "e")
  (game-lib-do-mouse-thing event)
  (track-mouse
    (while (progn
	     (setq event (read-event))
	     (or (mouse-movement-p event)
		 (eq (car-safe event) 'switch-frame)))
      (game-lib-do-mouse-thing event))
    )
  (game-lib-do-mouse-thing event)
  )

(defun game-lib-do-mouse-thing (event)
  "Do the mouse thing with EVENT.
The handler handles drag type things."
  (let ((pos (event-start event)))
    (if tyrant-mouse-function
	(eval (list tyrant-mouse-function
		    (posn-point pos)
		    '(event-basic-type event)
		    '(event-modifiers event)))
      )))

(defun game-lib-clear-buffer (&optional buffer)
  "Take the current BUFFER and delete everything in it.
This also will delete any overlays existing in the buffer."
  (save-excursion
    (if (bufferp buffer) (set-buffer buffer))
    (delete-region (point-min) (point-max))
    (if (fboundp 'overlay-lists)
	(let ((ol (car (overlay-lists))))
	  ;; delete overlays sitting here already
	  (while ol
	    (delete-overlay (car ol))
	    (setq ol (cdr ol))))
      )))

(defun game-lib-insert-string (pnt string color)
  "At PNT take STRING and place into buffer, replaceing what was there.
Make sure that face color is COLOR (being a symbol containing a face)
Colors will not be used if current emacs session cannot support them."

  (let ((rpnt (point))
	(ol (if (fboundp 'overlays-at)
		(overlays-at pnt)
	      nil))
	(end (+ pnt (length string)))
	(no nil))
    ;; delete overlays sitting here already
    (while ol
      (if (and (= (overlay-start (car ol)) pnt)
	       (= (overlay-end (car ol)) end)
	       (overlay-get (car ol) 'game-lib))
	  (delete-overlay (car ol)))
      (setq ol (cdr ol)))
    (goto-char pnt)
    ;; Insert first, then delete.  This prevents overlapping overlays
    ;; from befuddling themselves all over the place.
    (insert string)
    (if game-lib-replace		;only delete if turned on
	(if (> (length string) (- (point-max) (point)))
	    (delete-region (point) (point-max))
	  (delete-char (length string))))
    (if (and color (fboundp 'make-overlay) (fboundp 'overlay-put) 
	     window-system)
	(progn
	  (setq no (make-overlay pnt end))
	  (overlay-put no 'face color)
	  (overlay-put no 'game-lib t)))
    ;; when not replacing, we are expecting to be at the end of this
    ;; new string
    (if game-lib-replace
	(goto-char rpnt))))

(defun game-lib-add-overlay (start end face label)
  "Add a face over existing text from START to END of type FACE.
Give each overlay the label LABEL so that we can delete it easilly."
  (if game-lib-use-colors
      (let ((no (make-overlay start end)))
	(overlay-put no 'face face)
	(overlay-put no label t))))

(defun game-lib-delete-labeled-overlay (label)
  "Delete all overlays with LABEL as it's attribute."
  (if game-lib-use-colors
      (save-excursion
	(mapcar (function (lambda (ovr)
			    (and (overlay-get ovr label) 
				 (delete-overlay ovr))))
		(car (overlay-lists)))
	(mapcar (function (lambda (ovr)
			    (and (overlay-get ovr label) 
				 (delete-overlay ovr))))
		(cdr (overlay-lists))))))

(defun game-lib-query (prompt initial-input completion-list)
  "As user to enter a string with PROMPT.
Use INITIAL-INPUT as the default answer to use.  Use COMPLETION-LIST
to offer completions on possible answers, but do not restrict answers.
COMPLETION-LIST has the value as defined by the function
`completing-read`.  If COMPLETION-LIST is a symbol, and not a list,
then query for a free string."

  (if (not (and (boundp 'etalk-tyrannical-mode) etalk-tyrannical-mode))
      ;; If not in tyrant mode, do this!
      (game-lib-do-read prompt initial-input completion-list)
    (if etalk-tyrant-enabled-console
	;; If it is our turn, Ask Your Question!
	(let ((respond nil))
	  (unwind-protect
	      (setq respond
		    (game-lib-do-read prompt initial-input completion-list))
	    (if (not respond)
		(setq respond initial-input))
	    (tyrant-send-query-answer respond)))
      ;; If not, print MSG, and loop-wait for response from remote
      (let ((tyrant-query-response nil))
	;; loop until it is set.
	(message "Remote asked: \"%s\"  DO NOT HIT C-g" prompt)
	(while (not tyrant-query-response)
	  (accept-process-output etalk-process 1))
	tyrant-query-response))))

(defun game-lib-do-read (prompt initial-input completion-list)
  ; checkdoc-params: (prompt initial-input completion-list)
  "Helper function used by `game-lib-query'.
Do not use in tyrant games for fear of loosing compatibility.
Parameters conform to `game-lib-query'."
  (if (symbolp completion-list)
      (read-string prompt initial-input)
    (let ((completion-ignore-case t))
      (completing-read prompt completion-list nil t initial-input))))

(defun game-lib-win (who etalk-message solo-message)
  "Declare WHO winner handling tyrant problems.
If tyranted, use the ETALK-MESSAGE, otherwise, use SOLO-MESSAGE, where
the etalk message contains `tyrant-format' controls, and solo-message
may contain one %d which will be filled in by WHO.  Just before
printing the message, `tyrant-turn' is set to WHO, thus allowing use of
%P etc in tyrant messages even when it isn't tyrant-turn's turn"

  (let ((tyrant-turn who))
    (if (and (boundp 'etalk-tyrannical-mode) etalk-tyrannical-mode)
	(etalk-usurp-tyrant (tyrant-format etalk-message))
      (message solo-message tyrant-turn))))

(defun game-lib-swap-turns (etalk-message solo-message)
  "Swap turns handling tyrant problems.
If tyranted, use the ETALK-MESSAGE, otherwise use SOLO-MESSAGE, where
the etalk messasge contains tyrant-format controls, and solo-message
may contain one %d which will be give the current turn number."

 ;; tyrant-turn should always be defined for every game as the
  (cond
   ((= tyrant-turn 1) (setq tyrant-turn 2))
   ((= tyrant-turn 2) (setq tyrant-turn 1)))
  ;; tyrant mode support:
  ;; flip turns accross net each time.
  (if (and (boundp 'etalk-tyrant-enabled-console)
	   (boundp 'etalk-tyrannical-mode)
	   etalk-tyrannical-mode)
      (progn
	(message (tyrant-format etalk-message))
	(setq etalk-tyrant-enabled-console (not etalk-tyrant-enabled-console)))
    (message solo-message tyrant-turn)))


(defun game-lib-quit (kill)
  "Covers over icky tyrant mode stuff needed to quit a game.
If KILL, then kill the buffer when done."

  (if (and (boundp 'etalk-tyrannical-mode) etalk-tyrannical-mode)  
      (etalk-usurp-tyrant "Quit by player's request")
    (kill-buffer (current-buffer))))

(provide 'game-lib)
;;; game-lib.el ends here
