;;; dots! game interface (emacs talk optional)
;;;
;;; Copyright (C) 1994, 1999 Free Software Foundation
;;; Copyright (C) 1992 Eric M. Ludlam
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
;;; Please send bug reports, etc. to zappo@gnu.ai.mit.edu.
;;;
;;; $Id: dots.el,v 1.3 1999/08/26 11:33:49 zappo Exp $
;;; History:
;;; <joe@opus.ohio-state.edu> 9/10/94
;;; Fixed documentation problems, and a couple goofy variable names.
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

(defvar dots-map nil
  "Keymap used in playing dots")

(if dots-map
    ()
  (setq dots-map (make-sparse-keymap))
  (define-key dots-map "" 'dots-move)  
  (define-key dots-map "f" 'dots-move)
  (define-key dots-map "" 'dots-move)
  (define-key dots-map "b" 'dots-move)
  (define-key dots-map "" 'dots-move)
  (define-key dots-map "p" 'dots-move)
  (define-key dots-map "" 'dots-move)
  (define-key dots-map "n" 'dots-move)
  (define-key dots-map " " 'dots-place-piece)
  (define-key dots-map "q" 'dots-quit)
  (game-lib-add-mouse-support dots-map)
)

(defvar dots-buffer-name "DOTS!"
  "Buffer name format string for dots!")

(defvar dots-x nil
  "X position of cursor")

(defvar dots-y nil
  "Y position of cursor")

(defvar dots-p1-mark nil
  "Mark used for player 1. If name is 'zappo' translates to 'zap'")

(defvar dots-p2-mark nil
  "Mark used for player 2. If name is 'zappo' translates to 'zap'")

(defvar dots-p1-name nil
  "Player 1's name.")

(defvar dots-p2-name nil
  "Player 2's name.")

(defvar dots-moves-left nil
  "Number of moves left on the board.  Simple numeric for the number
of squares.")

(defvar dots-score-1 nil
  "Player 1's score")

(defvar dots-score-2 nil
  "Player 2's score")

(defun dots ()
  "Mode for playing a game call dots, which is played by placing lines
on the board to get enclose grid squares.  The goal is to have
enclosed the most squares.  Legal moves are any location on the
playing field."

  (interactive)
  (switch-to-buffer (get-buffer-create dots-buffer-name))
  (setq mode-name "Dots")
  (setq major-mode 'dots)
  (game-lib-clear-buffer)
  (insert "+-------------------------------------------------------------------+
| +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   + |
|                                                                   |
| +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   + |
|                                                                   |
| +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   + |
|                                                                   |
| +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   + |
|                                                                   |
| +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   + |
|                                                                   |
| +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   + |
|                                                                   |
| +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   + |
|                                                                   |
| +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   + |
|                                                                   |
| +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   + |
|                                                                   |
| +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   + |
+-------------------------------------------------------------------+")
  (delete-other-windows (selected-window))
  (use-local-map dots-map)
  (make-local-variable 'tyrant-turn)
  (setq tyrant-turn 1)
  (make-local-variable 'dots-x)
  (setq dots-x 0)
  (make-local-variable 'dots-y)
  (setq dots-y 0)
  (dots-place-cursor)
  (make-local-variable 'dots-p1-mark)
  (setq dots-p1-mark " 1 ")
  (make-local-variable 'dots-p2-mark)
  (setq dots-p2-mark " 2 ")
  (make-local-variable 'dots-p1-name)
  (setq dots-p1-name "Player1")
  (make-local-variable 'dots-p2-name)
  (setq dots-p2-name "Player2")
  (make-local-variable 'dots-moves-left)
  (setq dots-moves-left 313)
  (make-local-variable 'dots-score-1)
  (setq dots-score-1 0)
  (make-local-variable 'dots-score-2)
  (setq dots-score-2 0)
  (setq mode-line-buffer-identification 
	(list "Emacs" ": %5b"
	      (format " %s [%2d] %s [%2d]" 
		      dots-p1-name dots-score-1
		      dots-p2-name dots-score-2)))
  (set-buffer-modified-p (buffer-modified-p))
  
  ;; tyrant stuff here!
  (make-local-variable 'tyrant-mouse-function)
  (setq tyrant-mouse-function 'dots-mouse-support)
  (make-local-variable 'etalk-tyrant-brief-help)
  (setq etalk-tyrant-brief-help "DOTS: C-[fbnp] movement [SPC] go")
  (make-local-variable 'etalk-tyrant-quit-string)
  (fset 'etalk-tyrant-quit-string 'dots-win-string)

  (setq tyrant-player1-hook 
	'(lambda ()
	   (dots-tyrant-onechar)
	   (message "You are player 1")))
  (setq tyrant-player2-hook
	'(lambda ()
	   (setq etalk-tyrant-enabled-console nil)
	   (dots-tyrant-onechar)
	   (message "Your are player 2"))) ;player2 goes 2nd
  )

(defun dots-mouse-support(p e m)
  "Reads in a mouse event from the game-lib driver, and allows a
player to click on a square."

  ;; find which square we are closest to and go there
  (if (and (not (memq 'drag m)) (integerp p))
      (let ((first-line 0)
	    (line-width 70)
	    (block-width 3)
	    (block-height 1)
	    (block-vsep 0)			;vertical separator width
	    (block-hsep 1)			;horizontal sep width
	    x y xt yt
	    )
	(setq yt (/ (- p (* first-line line-width)) line-width))
	(setq xt (- (% p line-width) 1))
	(setq y (/ (- yt 1) (+ block-height block-vsep)))
	(if (= (% y 2) 0)
	    (setq xt (- xt 2)))
	(setq x (/ xt (+ block-width block-hsep)))
	(if (and (= (% y 2) 0) (> x 15))
	    (message "Off board...")

	  (if (not (or (< (% xt (+ block-width block-hsep)) block-hsep)
		       (< (% yt (+ block-height block-vsep))
			  block-vsep)))
	      (progn
		(setq dots-x x)
		(setq dots-y y)
		(goto-char (+ (dots-xy2index x y) 1))
		(if (memq 'click m)
		    (dots-place-piece))))))
    )
  )

(defun dots-tyrant-onechar ()
  "Look at the first char of the names in tyrant mode and select them.
If both people have same char 1, then use numbers."
  (let ((c1 (tyrant-format "%3u"))
	(c2 (tyrant-format "%3U")))
    (if (not (string= c1 c2))
	(progn
	  (setq dots-p1-mark c1)
	  (setq dots-p2-mark c2)
	  (setq dots-p1-name (tyrant-format "%u"))
	  (setq dots-p2-name (tyrant-format "%U"))))
    (setq mode-line-buffer-identification 
	  (list "Emacs" ": %5b"
		(format " %s [%2d] %s [%2d]" 
			dots-p1-name dots-score-1
			dots-p2-name dots-score-2)))))
	
(defun dots-place-piece ()
  "Put a piece onto the board."

  (interactive)
  (if (dots-owned dots-x dots-y)
      (error "Piece already there!" (dots-owned dots-x dots-y))
    (dots-put dots-x dots-y)
    (setq dots-moves-left (- dots-moves-left 1))
    (if (dots-gain-box)
	(progn
	  (if (and (boundp 'etalk-tyrannical-mode) etalk-tyrannical-mode)
	      (message (tyrant-format "It's still %p's turn."))
	    (message "It is STILL player %d's turn" tyrant-turn))
	  (if (equal dots-moves-left 0)
	      (if (and (boundp 'etalk-tyrannical-mode) etalk-tyrannical-mode)  
		  (etalk-usurp-tyrant (dots-win-string))
		(message (dots-win-string)))))
      (dots-swap-turns)))

  (setq mode-line-buffer-identification 
	(list "Emacs" ": %5b"
	      (format " %s [%2d] %s [%2d]" 
		      dots-p1-name dots-score-1
		      dots-p2-name dots-score-2)))
  (set-buffer-modified-p (buffer-modified-p)))

(defun dots-win-string ()
  "create a string declairing who wins based onthe score."
  
  (game-lib-win (if (< dots-score-1 dots-score-2) 1 2)
		(if (= dots-score-1 dots-score-2) 
		    "Nobody wins"
		  "%P wins!!!")
		(if (= dots-score-1 dots-score-2) 
		    "Nobody wins"
		  "player %d wins!!!")))

(defun dots-turn-char ()
  "return character to place based on turn"
  (if (= tyrant-turn 1)
      dots-p1-mark dots-p2-mark))

(defun dots-gain-box (&optional nope)
  "Check the move to x y to see if you win a box or not.
Optional arg NOPE indicates weather to actually mark the square."
  (let ((flag 0))
    (save-excursion
      (if (equal (% dots-y 2) 0)
	  (progn
	    (if (and (dots-owned dots-x (- dots-y 1))
		     (dots-owned dots-x (- dots-y 2))
		     (dots-owned (+ dots-x 1) (- dots-y 1)))
		(progn
		  (setq flag (+ flag 1))
		  (if nope
		      nil
		    (game-lib-insert-string
		     (+ (dots-xy2index dots-x (- dots-y 1)) 2)
		     (dots-turn-char)
		     (if (= tyrant-turn 1)
			 'game-lib-player1-face
		       'game-lib-player2-face)))))
	    (if (and (dots-owned dots-x (+ dots-y 1))
		     (dots-owned dots-x (+ dots-y 2))
		     (dots-owned (+ dots-x 1) (+ dots-y 1)))
		(progn
		  (setq flag (+ flag 1))
		  (if nope
		      nil
		    (game-lib-insert-string
		     (+ (dots-xy2index dots-x (+ dots-y 1)) 2)
		     (dots-turn-char)
		     (if (= tyrant-turn 1)
			 'game-lib-player1-face
		       'game-lib-player2-face))))))
	(if (and (dots-owned (- dots-x 1) (- dots-y 1))
		 (dots-owned (- dots-x 1) (+ dots-y 1))
		 (dots-owned (- dots-x 1) dots-y))
	    (progn
	      (setq flag (+ flag 1))
	      (if nope
		  nil
		(game-lib-insert-string
		 (- (dots-xy2index dots-x dots-y) 2)
		 (dots-turn-char)
		 (if (= tyrant-turn 1)
		     'game-lib-player1-face
		   'game-lib-player2-face)))))
	(if (and (dots-owned dots-x (+ dots-y 1))
		 (dots-owned dots-x (- dots-y 1))
		 (dots-owned (+ dots-x 1) dots-y))
	    (progn
	      (setq flag (+ flag 1))
	      (if nope
		  nil
		(game-lib-insert-string
		 (+ (dots-xy2index dots-x dots-y) 2)
		 (dots-turn-char)
		 (if (= tyrant-turn 1)
		     'game-lib-player1-face
		   'game-lib-player2-face))))))
      (if (not nope)			; no scoring during tests.
	  (if (equal tyrant-turn 1)
	      (setq dots-score-1 (+ dots-score-1 flag))
	    (setq dots-score-2 (+ dots-score-2 flag))))
      (if (equal flag 0)
	  nil
	flag))))

(defun dots-owned (x y)
  "returns nil if empty, 1 if player 1 piece, and 2 if player 2 piece"
  (if (or (< x 0) (or (and (equal (% y 2) 0) (> x 15))
		      (> x 16))
	  (< y 0) (> y 18))
      nil
    (save-excursion
      (goto-char (+ (dots-xy2index x y) 1))
      (cond
       ((= (following-char) ?\ )
	nil)
       (t t)))))

(defun dots-move ()
  "Move the cursor to a new position"
  (interactive)
  (cond
   ((or (= last-input-char ?\C-f)
	(= last-input-char ?f))
    (if (< dots-x 15)
	(setq dots-x (+ dots-x 1))
      (if (equal (% dots-y 2) 1)
	  (if (< dots-x 16)
	      (setq dots-x (+ dots-x 1))
	    (error "Can't go farther right!"))
	(error "Can't go farther right!"))))
   ((or (= last-input-char ?\C-b)
	(= last-input-char ?b))
    (if (> dots-x 0)
	  (setq dots-x (- dots-x 1))
      (error "Can't go farther left!")))
   ((or (= last-input-char ?\C-p)
	(= last-input-char ?p))
    (if (> dots-y 0)
	(progn
	  (setq dots-y (- dots-y 1))
	  (if (equal dots-x 16) (setq dots-x 15)))
      (error "Can't go farther up!")))
   ((or (= last-input-char ?\C-n)
	(= last-input-char ?n))
    (if (< dots-y 18)
	(progn
	  (setq dots-y (+ dots-y 1))
	  (if (equal dots-x 16) (setq dots-x 15)))
      (error "Can't go farther down!"))))
  (dots-place-cursor))

(defun dots-place-cursor ()
  "Place the cursor on the correct spot on the board..."

  (goto-char (+ 1 (dots-xy2index dots-x dots-y))))

(defun dots-put (x y &optional off)
  "Based on variable \"tyrant-turn\" place piece there."

  (save-excursion
    (goto-char (+ 1 (dots-xy2index x y)))
    (if (equal (% dots-y 2) 0)
	(progn
	  (forward-char -1)
	  (delete-char 3)
	  (insert "---"))
      (delete-char 1)
      (insert "|"))))
  
(defun dots-xy2index (x y)
  "change x y position to absolute buffer address"

  (+ 70 (* 70 y) (* x 4) (if (equal (% y 2) 0) 4 2)))

(defun dots-swap-turns ()
  "Swap turns in dots"

  (interactive)
  (game-lib-swap-turns "It is now %P's turn"
		       "It is now Player %d's turn"))

(defun dots-quit ()
  "Quit dots"
  
  (interactive)
  (game-lib-quit t))

;; end of lisp

(provide 'dots)

