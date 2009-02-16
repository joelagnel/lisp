;;; gess game with alternate footprint rule.
;;;
;;; Copyright (C) 1994, 1995 Free Software Foundation
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
;;; $Id: gessft2.el,v 1.2 1995/03/25 04:30:13 zappo Exp $
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

;; This uses original gess package function basics
(require 'gess "games/gess")

(defvar gessft2-map nil
  "Keymap used in playing gess with alternate footprint rule")

(if gessft2-map
    ()
  (setq gessft2-map (make-sparse-keymap))
  (define-key gessft2-map "\C-f" 'gess-move)  
  (define-key gessft2-map "\C-b" 'gess-move)
  (define-key gessft2-map "\C-p" 'gess-move)
  (define-key gessft2-map "\C-n" 'gess-move)
  (define-key gessft2-map "f" 'gess-move)  
  (define-key gessft2-map "b" 'gess-move)
  (define-key gessft2-map "p" 'gess-move)
  (define-key gessft2-map "n" 'gess-move)
  (define-key gessft2-map "1" 'gess-move)
  (define-key gessft2-map "2" 'gess-move)
  (define-key gessft2-map "3" 'gess-move)
  (define-key gessft2-map "4" 'gess-move)
  (define-key gessft2-map "6" 'gess-move)
  (define-key gessft2-map "7" 'gess-move)
  (define-key gessft2-map "8" 'gess-move)
  (define-key gessft2-map "9" 'gess-move)
  (define-key gessft2-map " " 'gessft2-grab-or-drop)
  (define-key gessft2-map "5" 'gessft2-grab-or-drop)
  (define-key gessft2-map "q" 'gess-quit)
  (game-lib-add-mouse-support gessft2-map)
)

(defconst gessft2-piece1 "X"
  "String as piece 1")

(defconst gessft2-piece2 "O"
  "String as piece 2")

(defun gessft2 ()
  "Mode for playing gessft2.  Gessft2 uses a go board, but is played with a
chess like strategy.  A `piece` is any 3x3 block of spots (represented
by a period (.).  The direction of travel for a piece is anywhere
there is a piece on an outer edge.  If there is a stone in the center,
you may travel as far as you like in any direction.  If there is not,
you may travel a maximum of one block-width in a given direction.
The goal is to take a piece of the opponants king, or `ring'
Thus:
 X X X
 X . X  A king.  travels in any direction one piece width
 X X X

 X . X
 . X . A bishop, travel as far as you like in any diagonal.
 X . X

 . X .
 X X X A rook.  May travel as far as you like in vertical and horizontal.
 . X .

 X X X
 . X . A goofy piece made by two other pieces being next to eachother.
 . X .         it can travel as far as it likes down, or any vector up.

 . . .
 . . . A pawn. Travels one block width down.
 . X .

Admittedly, you need not stick with default pieces. Any peice is
defined by it's center, and a piece is devoid if there is an opponant
piece in it's `footprint`, which is the 3x3 block.  Thus, a pawn could
be adjusted by doing this:
 . . .
 . . . It can now travel diagonally.  The same can let it go backwards.
 X . .

A piece may only travel until some other piece is in it's footprint.
this includes your own pieces.  If your own pieces are under your
footprint, they get captured.  Yikes!

Movement is standard arrow keys, or the keyboard, which let's you move
diagonally.  SPC, and 5 are the center grabbing keys.

     7  8  9        C-p
      \ | /          |
     4--5--6   C-b -SPC- C-f
      / | \          |
     1  2  3        C-n
"
  (interactive)
  (switch-to-buffer (get-buffer-create "GESS alternate footprint rule"))
  (setq mode-name "GESSFT2")
  (setq major-mode 'gessft2)
  (game-lib-clear-buffer)
  (insert "
 . 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 .
 a . . . . . . . . . . . . . . . . . . . . a
 b . . . . . . . . . . . . . . . . . . . . b
 c . . . . . . . . . . . . . . . . . . . . c
 d . . . . . . . . . . . . . . . . . . . . d
 e . . . . . . . . . . . . . . . . . . . . e
 f . . . . . . . . . . . . . . . . . . . . f
 g . . . . . . . . . . . . . . . . . . . . g
 h . . . . . . . . . . . . . . . . . . . . h
 i . . . . . . . . . . . . . . . . . . . . i
 j . . . . . . . . . . . . . . . . . . . . j
 k . . . . . . . . . . . . . . . . . . . . k
 l . . . . . . . . . . . . . . . . . . . . l
 m . . . . . . . . . . . . . . . . . . . . m
 n . . . . . . . . . . . . . . . . . . . . n
 o . . . . . . . . . . . . . . . . . . . . o
 p . . . . . . . . . . . . . . . . . . . . p
 q . . . . . . . . . . . . . . . . . . . . q
 r . . . . . . . . . . . . . . . . . . . . r
 s . . . . . . . . . . . . . . . . . . . . s
 t . . . . . . . . . . . . . . . . . . . . t
 . 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 .")
  (delete-other-windows (selected-window))
  (use-local-map gessft2-map)
  (make-local-variable 'tyrant-turn)
  (setq tyrant-turn 1)
  (make-local-variable 'gess-x)
  (setq gess-x 1)
  (make-local-variable 'gess-y)
  (setq gess-y 1)
  (make-local-variable 'gess-grab-x)
  (setq gess-grab-x nil)
  (make-local-variable 'gess-grab-y)
  (setq gess-grab-y nil)
  (make-local-variable 'gess-valid-moves)
  (setq gess-valid-moves nil)
  (make-local-variable 'gess-p1-ring-x)
  (setq gess-p1-ring-x 11)
  (make-local-variable 'gess-p1-ring-y)
  (setq gess-p1-ring-y 2)
  (make-local-variable 'gess-p2-ring-x)
  (setq gess-p2-ring-x 11)
  (make-local-variable 'gess-p2-ring-y)
  (setq gess-p2-ring-y 17)
  ;; ROOK 1 P1
  (gess-put 2 1) (gess-put 1 2) (gess-put 2 2) (gess-put 3 2) (gess-put 2 3)
  ;; BISHP 1 P1
  (gess-put 4 1) (gess-put 4 3) (gess-put 5 2) (gess-put 6 1) (gess-put 6 3)
  ;; Queen P1
  (gess-put 7 1) (gess-put 8 1) (gess-put 9 1) 
  (gess-put 7 2) (gess-put 8 2) (gess-put 9 2)
  (gess-put 7 3) (gess-put 8 3) (gess-put 9 3) 
  ;; King P1
  (gess-put 10 1) (gess-put 11 1) (gess-put 12 1) 
  (gess-put 10 2) (gess-put 12 2)
  (gess-put 10 3) (gess-put 11 3) (gess-put 12 3) 
  ;; BISHP 2 P1
  (gess-put 13 1)(gess-put 13 3)(gess-put 14 2)(gess-put 15 1)(gess-put 15 3)
  ;; ROOK 2 P1
  (gess-put 17 1)(gess-put 16 2)(gess-put 17 2)(gess-put 18 2)(gess-put 17 3)
  ;; PAWNS 1-6 P1
  (gess-put 2 6) (gess-put 5 6) (gess-put 8 6) (gess-put 11 6) (gess-put 14 6)
  (gess-put 17 6)
  (let ((tyrant-turn 2))
    ;; ROOK 1 P2
    (gess-put 2 18) (gess-put 1 17) (gess-put 2 17) (gess-put 3 17) (gess-put 2 16)
    ;; BISHP 1 P2
    (gess-put 4 18) (gess-put 4 16) (gess-put 5 17) (gess-put 6 18) (gess-put 6 16)
    ;; Queem P2
    (gess-put 7 18) (gess-put 8 18) (gess-put 9 18) 
    (gess-put 7 17) (gess-put 8 17) (gess-put 9 17)
    (gess-put 7 16) (gess-put 8 16) (gess-put 9 16) 
    ;; King P2
    (gess-put 10 18) (gess-put 11 18) (gess-put 12 18) 
    (gess-put 10 17) (gess-put 12 17)
    (gess-put 10 16) (gess-put 11 16) (gess-put 12 16) 
    ;; BISHP 2 P2
    (gess-put 13 18)(gess-put 13 16)(gess-put 14 17)(gess-put 15 18)(gess-put 15 16)
    ;; ROOK 2 P2
    (gess-put 17 18)(gess-put 16 17)(gess-put 17 17)(gess-put 18 17)(gess-put 17 16)
    ;; PAWNS 1-6 P2
    (gess-put 2 13) (gess-put 5 13) (gess-put 8 13) (gess-put 11 13) (gess-put 14 13)
    (gess-put 17 13)
    )
  (make-local-variable 'tyrant-mouse-function)
  (setq tyrant-mouse-function 'gessft2-mouse-support)
  (make-local-variable 'etalk-tyrant-brief-help)
  (setq etalk-tyrant-brief-help
"GESSFT2: [C-f,f] Forward [C-b,b] Back [C-p,p] Previous [C-n,n] Next [SPC] DO IT")
  (setq tyrant-player1-hook 
	'(lambda ()
	   (message "You are player 1")))
  (setq tyrant-player2-hook
	'(lambda ()
	   (setq etalk-tyrant-enabled-console nil)
	   (message "Your are player 2"))) ;player2 goes 2nd
  (gess-place-cursor)
  )

(defun gessft2-mouse-support(p e m)
  "Reads in a mouse event from the game-lib driver, and allows a
player to click on a square."

  ;; find which square we are closest to and go there
  (if (and (not (memq 'drag m)) (integerp p))
      (let ((first-line 1)
	    (line-width 45)
	    (block-width 1)
	    (block-height 1)
	    (block-vsep 0)			;vertical separator width
	    (block-hsep 1)			;horizontal sep width
	    x y xt yt
	    )
	(setq yt (/ (- p (* first-line line-width)) line-width))
	(setq xt (- (% p line-width) 4))
	(setq y (/ yt (+ block-height block-vsep)))
	(setq x (/ xt (+ block-width block-hsep)))
	
	(if (not (or (< (% xt (+ block-width block-hsep)) block-hsep)
		     (< (% yt (+ block-height block-vsep))
			block-vsep)
		     (< x 1)
		     (> x 18)
		     (< y 1)
		     (> y 18)))
	    (progn
	      (setq gess-x x)
	      (setq gess-y y)
	      (goto-char (+ (gess-xy2index x y) 1))
	      (if (memq 'click m)
		  (gessft2-grab-or-drop)))))
    )
  )

(defun gessft2-grab-or-drop ()
  "Put a piece (stone) onto the board."
  (interactive)
  (let ((mdir (gessft2-user-in gess-x gess-y tyrant-turn t)))
    (if (and (not mdir) (not gess-valid-moves))
	(message "You may not select a piece with none of your stones in it.")
      (gessft2-select gess-x gess-y)
      (if (equal gess-valid-moves t)
	  (setq gess-valid-moves nil)
	(if (not gess-valid-moves)
	    (progn
	      (setq gess-valid-moves mdir)
	      (gess-hilight-valid-spots)))))))
    
(defun gessft2-select (x y)
  "Hilight the given area under X and Y.  For Terminals, put | around
the pieces, and for e19, but nifty inverse overlay over it.  If it has
already been grabbed, degrab it."
  (if (and gess-grab-x gess-grab-y)
      (if (and (= x gess-grab-x)
	       (= y gess-grab-y))
	  (gess-grab-marker x y " " " " nil)
	(if (gess-pt-in-list x y)
	    (let ((pv (gess-get-piece-vector gess-grab-x gess-grab-y
					     tyrant-turn)))
	      (cond ((and (= tyrant-turn 1)
			  (= gess-p1-ring-x gess-grab-x)
			  (= gess-p1-ring-y gess-grab-y))
		     (setq gess-p1-ring-x x)
		     (setq gess-p1-ring-y y)
		     (message "Player 1 moved his ring!"))
		    ((and (= tyrant-turn 2)
			  (= gess-p2-ring-x gess-grab-x)
			  (= gess-p2-ring-y gess-grab-y))
		     (setq gess-p2-ring-x x)
		     (setq gess-p2-ring-y y)
		     (message "Player 2 moved his ring!")))
	      (gessft2-reset-square gess-grab-x gess-grab-y pv)
	      (gessft2-put-piece x y pv)
	      (gess-grab-marker gess-grab-x gess-grab-y " " " " nil)
	      (sit-for 0)
	      (let ((ring1 (or (gess-ring-p gess-p1-ring-x gess-p1-ring-y 1)
			       (gess-ring-somewhere 1)))
		    (ring2 (or  (gess-ring-p gess-p2-ring-x gess-p2-ring-y 2)
				(gess-ring-somewhere 2))))
		(cond 
		 ((and (not ring1) (not ring2))
		  (game-lib-win 1 "Players simultaneously killed!"
				"Players simultaneously killed!"))
		 ((not ring1)
		  (game-lib-win 2 "Player %P wins!" "Player 2 wins!"))
		 ((not ring2)
		  (game-lib-win 1 "Player %P wins!" "Player 1 wins!"))
		 (t
		  (gessft2-swap-turns)))))
	  (message "Invalid place to move to.")
	  (ding)))
    (gess-grab-marker x y ">" "<" t)
    (message "We need thier computer things!")))

(defun gessft2-reset-square (x y pv)
  "Blank out all pieces in a given square where the pieces from the
vector land."
  (let ((dv [(-1 -1) (-1 0) (-1 1)
	     ( 0 -1) ( 0 0) ( 0 1)
	     ( 1 -1) ( 1 0) ( 1 1)])
	(ix 0))
    (while (< ix 9)
      (if (aref pv ix)
	  (gess-put (+ x (car (aref dv ix))) (+ y (car (cdr (aref dv ix)))) t))
      (setq ix (1+ ix)))))

(defun gessft2-put-piece (x y pv)
  "Take vector PV and put it at position X Y"
  (let ((dv [(-1 -1) (-1 0) (-1 1)
	     ( 0 -1) ( 0 0) ( 0 1)
	     ( 1 -1) ( 1 0) ( 1 1)])
	(ix 0))
    (while (< ix 9)
      (if (aref pv ix)
	  (gess-put (+ x (car (aref dv ix))) (+ y (car (cdr (aref dv ix)))) 
		    nil))
      (setq ix (1+ ix)))))

(defun gessft2-user-in (x y player &optional genlist)
  "Check to see if 3x3 block at X and Y has a piece in it owned by
player, and return a list of direction he is allowed to go."
  (let ((dv [(-1 -1) (-1 0) (-1 1)
	     ( 0 -1)        ( 0 1)
	     ( 1 -1) ( 1 0) ( 1 1)])
	(ix 0)
	(any nil)
	(nl nil)
	(tv nil)
	(middle (gess-owned-one x y player)))
    (while (< ix 8)
      (setq any (or (setq tv (gess-owned-one 
			      (+ x (car (aref dv ix)))
			      (+ y (car (cdr (aref dv ix))))
			      player))
		    any))
      (if tv (setq nl (append nl (gessft2-direction-list 
				  x y player (aref dv ix) middle))))
      (setq ix (1+ ix)))
    (if genlist nl any)))

(defun gessft2-direction-list (x y player delta distance)
  "Starting at X Y, for PLAYER travel in DELTA direction and return a
list of valid squares.  If DISTANCE, travel forever (until border)
otherwise, only travel up to 3 away."
  (let* ((pv nil)
	 (mf (gess-get-piece-vector x y player))
	 (xd (car delta))
	 (yd (car (cdr delta)))
	 (tx (+ x xd))
	 (ty (+ y yd))
	 (tl nil)
	 (end nil)
	 (op (if (= player 1) 2 1)))
    (gessft2-reset-square x y mf)
    (while (not end)
      (if (not (or (> tx 18) (< tx 1) (> ty 18) (< ty 1)))
	  (setq tl (cons (list tx ty) tl)))
      (setq pv (gess-get-piece-vector tx ty 3))
      (setq end (or (and (aref pv 0) (aref mf 0))
		    (and (aref pv 1) (aref mf 1))
		    (and (aref pv 2) (aref mf 2))
		    (and (aref pv 3) (aref mf 3))
		    (and (aref pv 4) (aref mf 4))
		    (and (aref pv 5) (aref mf 5))
		    (and (aref pv 6) (aref mf 6))
		    (and (aref pv 7) (aref mf 7))
		    (and (aref pv 8) (aref mf 8))
		    ;; edges of board
		    (> tx 18)
		    (< tx 1)
		    (> ty 18)
		    (< ty 1)
		    ;; distance traveled
		    (and (not middle) (= 3 (length tl)))))  
      (setq tx (+ tx xd))
      (setq ty (+ ty yd)))
    (gessft2-put-piece x y mf)
    tl))

(defun gessft2-swap-turns ()
  "Swap turns in connect 4"
  (game-lib-swap-turns "It is now %P's turn."
		       "Player %d's turn, Can you make us go?"))

(defun gessft2-quit ()
  "quit 4 by 4"
  
  (interactive)
  (game-lib-quit t))

;;; end of lisp

(provide 'gessft2)
