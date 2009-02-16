;;; tick tac toe interface (emacs talk optional)
;;;
;;; Copyright (C) 1992 Eric M. Ludlam
;;; Copyright (C) 1994 Free Software Foundation
;;;
;;; Author: Eric M. Ludlam <zappo@gnu.ai.mit.edu>
;;; Keywords: games, talk
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, you can either send email to this
;;; program's author (see below) or write to:
;;;
;;;              The Free Software Foundation, Inc.
;;;              675 Mass Ave.
;;;              Cambridge, MA 02139, USA. 
;;;
;;; Please send bug reports, etc. to zappo@gnu.ai.mit.edu
;;;
;;; $Id: talk-tac-toe.el,v 1.2 1995/06/06 01:46:30 zappo Exp $
;;; History:
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                      ;;;
;;;                          TYRANT MODE GAME                            ;;;
;;;                                                                      ;;;
;;;  This program contains a program designed for use with               ;;;
;;; tyrant-mode.  This program may be used under the following           ;;;
;;; software conditions.                                                 ;;;
;;;                                                                      ;;;
;;;  By itself with no tyrant support.  All extraneous keys will mess    ;;;
;;;  up the information in a given buffer.                               ;;;
;;;                                                                      ;;;
;;;  Under tyrant-mode for TALK.  To run this way, use "etalk"           ;;;
;;;  and once a connection is established, use the game playing          ;;;
;;;  function "etalk-initiate-special-function" bound to C-c g           ;;;
;;;  to start.  Must be installed on all systems.                        ;;;
;;;                                                                      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'game-lib "games/game-lib")

(defvar ttt-map nil
  "Keymap used in talk-tac-toe.")
(if ttt-map
    ()
  (setq ttt-map (make-sparse-keymap))
  (define-key ttt-map "q" 'ttt-quit)
  (define-key ttt-map "1" 'ttt-move)
  (define-key ttt-map "2" 'ttt-move)
  (define-key ttt-map "3" 'ttt-move)
  (define-key ttt-map "4" 'ttt-move)
  (define-key ttt-map "5" 'ttt-move)
  (define-key ttt-map "6" 'ttt-move)
  (define-key ttt-map "7" 'ttt-move)
  (define-key ttt-map "8" 'ttt-move)
  (define-key ttt-map "9" 'ttt-move)
  (game-lib-add-mouse-support ttt-map)
  )

(defvar ttt-p1-mark "X"
  "Player 1's mark")
(defvar ttt-p2-mark "O"
  "Player 2's mark")

(defvar ttt-numb-moves nil
  "Number of moves so far. Used to find stalemate")

(defun talk-tac-toe ()
  "Talk tac toe:  Mode for playing tic-tac-toe with or without
Tyrant-mode running.  Press number associated with a square to move in
that square. \\<ttt-map>
\\[ttt-move] : move to a square
\\[ttt-quit] : quit
"

  (interactive)
  (let ((buff (get-buffer-create "TALK-TAC-TOE")))
    ;; bring buffer forwards
    (switch-to-buffer buff))
  (setq major-mode 'talk-tac-toe)
  (setq mode-name "T-T-T")
  (use-local-map ttt-map)
  ;; toast buffer contents
  (game-lib-clear-buffer)
  ;; insert board into buffer Note: spaces etc have been counted so
  ;; keeping this clean is importnat.
  (game-lib-insert-string 1 "-------------
| 1 | 2 | 3 |
-------------
| 4 | 5 | 6 |
-------------
| 7 | 8 | 9 |
-------------" nil)
  ;; whos turn is it?
  (make-local-variable 'tyrant-turn)
  (setq tyrant-turn 1)
  ;; get the restore position flag local
  (make-local-variable 'tyrant-dont-restore-position)
  (setq tyrant-dont-restore-position nil)
  ;; tyrant mode mouse support
  (make-local-variable 'tyrant-mouse-function)
  (setq tyrant-mouse-function 'ttt-mouse-support)
  ;; tyrant mode help string
  (make-local-variable 'etalk-tyrant-brief-help)
  (setq etalk-tyrant-brief-help 
	"TalkTacToe: # to move, q to quit, C-c m to send message.")
  (make-local-variable 'etalk-tyrant-quit-string)
  (setq etalk-tyrant-quit-string "")
  (setq ttt-numb-moves 0)
  ;; ok.. setup hooks dependant on wether you are player1 or player2
  (setq tyrant-player1-hook 
	'(lambda ()
	   (message "You are player 1")))
  (setq tyrant-player2-hook
	'(lambda ()
	   (setq etalk-tyrant-enabled-console nil)
	   (message "Your are player 2"))) ;player2 goes 2nd
)

(defun ttt-mouse-support(p e m)
  "Reads in a mouse event from the game-lib driver, and allows a
player to click on a square."

  ;; find which square we are closest to and go there
  (if (and (not (memq 'drag m)) (integerp p))
      (let ((first-line 0)
	    (line-width 14)
	    (block-width 3)
	    (block-height 1)
	    (block-vsep 1)			;vertical separator width
	    (block-hsep 1)			;horizontal sep width
	    x y xt yt
	    )
	(setq yt (/ (- p (* first-line line-width)) line-width))
	(setq xt (- (% p line-width) 1))
	(setq y (/ yt (+ block-height block-vsep)))
	(setq x (/ xt (+ block-width block-hsep)))

	(message "Clicked on %d %d" x y)
	
	(if (not (or (< (% xt (+ block-width block-hsep)) block-hsep)
		     (< (% yt (+ block-height block-vsep))
			block-vsep)))
	    (if (memq 'click m)
		(let ((last-input-char (+ (* y 3) x 1 ?0)))
		  (ttt-move)))))
    )
  )

(defun ttt-move ()
  "Place a mark upon the board."
  (interactive)
  (let ((key last-input-char)
	(didiwin nil))
    (cond
     ((equal key ?\C-m) (error "You fruitcake!"))
     ((equal key ?1) (goto-char 17))
     ((equal key ?2) (goto-char 21))
     ((equal key ?3) (goto-char 25))
     ((equal key ?4) (goto-char 45))
     ((equal key ?5) (goto-char 49))
     ((equal key ?6) (goto-char 53))
     ((equal key ?7) (goto-char 73))
     ((equal key ?8) (goto-char 77))
     ((equal key ?9) (goto-char 81))
     (t (error "Bad key value...[%s]" key)))
    
    (if (or (equal (following-char) ?X)
	    (equal (following-char) ?O))
	(error "Player %d: Move to %c taken already!" tyrant-turn key)
      (setq ttt-numb-moves (+ 1 ttt-numb-moves))
      (cond
       ((= tyrant-turn 1) (game-lib-insert-string (point)
						  ttt-p1-mark
						  'game-lib-player1-face))
       ((= tyrant-turn 2) (game-lib-insert-string (point)
						  ttt-p2-mark
						  'game-lib-player2-face))
       )
      (goto-char 0))
    (if (setq didiwin (ttt-detect-win))
	(progn
	 (game-lib-win tyrant-turn "%P wins!" "Player %d wins!")
	 (setq tyrant-dont-restore-position t))
      (if (equal ttt-numb-moves 9)
	  (progn
	    (game-lib-win tyrant-turn "No moves left! Stalemate."
			  "No moves left! Stalemate.")
	    (setq tyrant-dont-restore-position t))
	(ttt-swap-turns)))))

(defun ttt-check-own (pos)
  "See if that position is taken."
  (save-excursion
    (cond
     ((equal pos ?1) (goto-char 17))
     ((equal pos ?2) (goto-char 21))
     ((equal pos ?3) (goto-char 25))
     ((equal pos ?4) (goto-char 45))
     ((equal pos ?5) (goto-char 49))
     ((equal pos ?6) (goto-char 53))
     ((equal pos ?7) (goto-char 73))
     ((equal pos ?8) (goto-char 77))
     ((equal pos ?9) (goto-char 81))
     (t (error "Bad pos value...")))
    (following-char)))

(defun ttt-detect-win ()
  "Detect wether a move to position n actually creates a winning situation."

  (cond
   ((3equal (ttt-check-own ?1) (ttt-check-own ?2) (ttt-check-own ?3)) 
    (ttt-check-own ?1))
   ((3equal (ttt-check-own ?4) (ttt-check-own ?5) (ttt-check-own ?6)) 
    (ttt-check-own ?4))
   ((3equal (ttt-check-own ?7) (ttt-check-own ?8) (ttt-check-own ?9)) 
    (ttt-check-own ?7))
   ((3equal (ttt-check-own ?1) (ttt-check-own ?4) (ttt-check-own ?7)) 
    (ttt-check-own ?1))
   ((3equal (ttt-check-own ?2) (ttt-check-own ?5) (ttt-check-own ?8)) 
    (ttt-check-own ?2))
   ((3equal (ttt-check-own ?3) (ttt-check-own ?6) (ttt-check-own ?9)) 
    (ttt-check-own ?3))
   ((3equal (ttt-check-own ?1) (ttt-check-own ?5) (ttt-check-own ?9)) 
    (ttt-check-own ?1))
   ((3equal (ttt-check-own ?3) (ttt-check-own ?5) (ttt-check-own ?7)) 
    (ttt-check-own ?3))
   (t nil)
   ))

(defun 3equal (thing1 thing2 thing3)
  "Return t if all 3 things are the same"
  (and (equal thing1 thing2) (equal thing2 thing3)))

(defun ttt-swap-turns ()
  "Go to next person's turn."

  (game-lib-swap-turns "It is now %P's turn." "Player %d's turn"))

(defun ttt-quit ()
  "Quit talk-tac-toe nicely"
  (interactive)
  (game-lib-quit t))

;;; end of lisp

(provide 'talk-tac-toe)
