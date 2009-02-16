;;; gess game, chesslike actions on go board
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
;;; $Id: gess.el,v 1.5 1995/09/21 02:08:42 zappo Exp $
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

(defvar gess-map nil
  "Keymap used in playing gess")

(if gess-map
    ()
  (setq gess-map (make-sparse-keymap))
  (define-key gess-map "\C-f" 'gess-move)  
  (define-key gess-map "\C-b" 'gess-move)
  (define-key gess-map "\C-p" 'gess-move)
  (define-key gess-map "\C-n" 'gess-move)
  (define-key gess-map "f" 'gess-move)  
  (define-key gess-map "b" 'gess-move)
  (define-key gess-map "p" 'gess-move)
  (define-key gess-map "n" 'gess-move)
  (define-key gess-map "1" 'gess-move)
  (define-key gess-map "2" 'gess-move)
  (define-key gess-map "3" 'gess-move)
  (define-key gess-map "4" 'gess-move)
  (define-key gess-map "6" 'gess-move)
  (define-key gess-map "7" 'gess-move)
  (define-key gess-map "8" 'gess-move)
  (define-key gess-map "9" 'gess-move)
  (define-key gess-map " " 'gess-grab-or-drop)
  (define-key gess-map "5" 'gess-grab-or-drop)
  (define-key gess-map "q" 'gess-quit)
  (game-lib-add-mouse-support gess-map)
)

(defconst gess-piece1 "X"
  "String as piece 1")

(defconst gess-piece2 "O"
  "String as piece 2")

(defvar gess-x 1
  "Current X position in gess board")

(defvar gess-y 1
  "Current Y position in gess board")

(defvar gess-grab-x nil
  "Current X position of grabbed group")
(defvar gess-grab-y nil
  "Current Y position of grabbed group")
(defvar gess-valid-moves nil
  "List of possible moves iff a piece has been selected.")

(defvar gess-p1-ring-x 11
  "Gess player 1's x ring position")

(defvar gess-p1-ring-y 2
  "Gess player 1's y ring position")

(defvar gess-p2-ring-x 11
  "Gess player 2's x ring position")

(defvar gess-p2-ring-y 17
  "Gess player 2's y ring position")

(defvar gess-selected-background "gray"
  "Color behind peices")
(defvar gess-can-go-background "blue"
  "Color behind valid move squares")

;;; If we have a window system, load in cool colors to use on the game board
(if (and game-lib-use-colors window-system)
    (progn
      (game-lib-load-color 'gess-select-face
			   nil "gray" nil "gray" t)
      (game-lib-load-color 'gess-go-face
			   nil "blue" nil "blue" t)))

(defun gess ()
  "Mode for playing gess.  Gess uses a go board, but is played with a
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
  (switch-to-buffer (get-buffer-create "GESS What!"))
  (setq mode-name "GESS")
  (setq major-mode 'gess)
  (game-lib-clear-buffer)
  (insert "
 + a b c d e f g h i j k l m n o p q r s t +
20 . . . . . . . . . . . . . . . . . . . .20
19 . . . . . . . . . . . . . . . . . . . .19
18 . . . . . . . . . . . . . . . . . . . .18
17 . . . . . . . . . . . . . . . . . . . .17
16 . . . . . . . . . . . . . . . . . . . .16
15 . . . . . . . . . . . . . . . . . . . .15
14 . . . . . . . . . . . . . . . . . . . .14
13 . . . . . . . . . . . . . . . . . . . .13
12 . . . . . . . . . . . . . . . . . . . .12
11 . . . . . . . . . . . . . . . . . . . .11
10 . . . . . . . . . . . . . . . . . . . .10
 9 . . . . . . . . . . . . . . . . . . . . 9
 8 . . . . . . . . . . . . . . . . . . . . 8
 7 . . . . . . . . . . . . . . . . . . . . 7
 6 . . . . . . . . . . . . . . . . . . . . 6
 5 . . . . . . . . . . . . . . . . . . . . 5
 4 . . . . . . . . . . . . . . . . . . . . 4
 3 . . . . . . . . . . . . . . . . . . . . 3
 2 . . . . . . . . . . . . . . . . . . . . 2
 1 . . . . . . . . . . . . . . . . . . . . 1
 + a b c d e f g h i j k l m n o p q r s t +")
  (delete-other-windows (selected-window))
  (use-local-map gess-map)
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
  (setq tyrant-mouse-function 'gess-mouse-support)
  (make-local-variable 'etalk-tyrant-brief-help)
  (setq etalk-tyrant-brief-help
"GESS: [C-f,f] Forward [C-b,b] Back [C-p,p] Previous [C-n,n] Next [SPC] DO IT")
  (setq tyrant-player1-hook 
	'(lambda ()
	   (message "You are player 1")))
  (setq tyrant-player2-hook
	'(lambda ()
	   (setq etalk-tyrant-enabled-console nil)
	   (message "Your are player 2"))) ;player2 goes 2nd
  (gess-place-cursor)
  )

(defun gess-mouse-support(p e m)
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
		  (gess-grab-or-drop)))))
    )
  )

(defun gess-grab-or-drop ()
  "Put a piece (stone) onto the board."
  (interactive)
  (let ((mdir (gess-user-in gess-x gess-y tyrant-turn t))
	(hdir (gess-user-in gess-x gess-y (if (= tyrant-turn 1) 2 1))))
    (if (and hdir (not gess-valid-moves))
	(message "You may not select a piece with the enemy in it.")
      (if (and (not mdir) (not gess-valid-moves))
	  (message "You may not select a piece with none of your stones in it.")
	(gess-select gess-x gess-y)
	(if (equal gess-valid-moves t)
	    (setq gess-valid-moves nil)
	  (if (not gess-valid-moves)
	      (progn
		(setq gess-valid-moves mdir)
		(gess-hilight-valid-spots))))))))

(defun gess-grab-marker (x y c1 c2 over)
  "Turn on/off a grab marker to indicate that a player has grabbed a
piece."
  (save-excursion
    (game-lib-insert-string (gess-xy2index (1-  x) (1- y)) c1 nil)
    (game-lib-insert-string (gess-xy2index (1-  x)     y)  c1 nil)
    (game-lib-insert-string (gess-xy2index (1-  x) (1+ y)) c1 nil)
    (game-lib-insert-string (gess-xy2index (+ 2 x) (1- y)) c2 nil)
    (game-lib-insert-string (gess-xy2index (+ 2 x)     y)  c2 nil)
    (game-lib-insert-string (gess-xy2index (+ 2 x) (1+ y)) c2 nil))
  (if over
      (progn
	(game-lib-add-overlay (1+ (gess-xy2index (1- x) (1- y)))
			      (gess-xy2index (+ 2 x) (1- y))
			      'gess-select-face
			      'gess-select)
	(game-lib-add-overlay (1+ (gess-xy2index (1- x) y))
			      (gess-xy2index (+ 2 x) y)
			      'gess-select-face
			      'gess-select)
	(game-lib-add-overlay (1+ (gess-xy2index (1- x) (1+ y)))
			      (gess-xy2index (+ 2 x) (1+ y))
			      'gess-select-face
			      'gess-select)
	(setq gess-grab-x x)
	(setq gess-grab-y y))
    (game-lib-delete-labeled-overlay 'gess-select)
    (game-lib-delete-labeled-overlay 'gess-valid-spots)
    (setq gess-valid-moves t)
    (setq gess-grab-x nil)
    (setq gess-grab-y nil)))
    

(defun gess-select (x y)
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
	      (gess-reset-square gess-grab-x gess-grab-y)
	      (gess-put-piece x y pv)
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
		  (gess-swap-turns)))))
	  (message "Invalid place to move to.")
	  (ding)))
    (gess-grab-marker x y ">" "<" t)
    (message "We need thier computer things!")))

(defun gess-user-in (x y player &optional genlist)
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
      (if tv (setq nl (append nl (gess-direction-list x y player (aref dv ix) middle))))
      (setq ix (1+ ix)))
    (if genlist nl any)))

(defun gess-get-piece-vector (x y player)
  "Return a vector 9 long with t marking the position of player's
pieces in that 3x3 square"
  (let ((dv [(-1 -1) (-1 0) (-1 1)
	     ( 0 -1) ( 0 0) ( 0 1)
	     ( 1 -1) ( 1 0) ( 1 1)])
	(ix 0)
	(nv (make-vector 9 nil)))
    (while (< ix 9)
      (aset nv ix (gess-owned-one 
		   (+ x (car (aref dv ix)))
		   (+ y (car (cdr (aref dv ix))))
		   player))
      (setq ix (1+ ix)))
    nv))

(defun gess-reset-square (x y)
  "Return a vector 9 long with t marking the position of player's
pieces in that 3x3 square"
  (let ((dv [(-1 -1) (-1 0) (-1 1)
	     ( 0 -1) ( 0 0) ( 0 1)
	     ( 1 -1) ( 1 0) ( 1 1)])
	(ix 0))
    (while (< ix 9)
      (gess-put (+ x (car (aref dv ix))) (+ y (car (cdr (aref dv ix)))) t)
      (setq ix (1+ ix)))))

(defun gess-put-piece (x y pv)
  "Take vector PV and put it at position X Y"
  (let ((dv [(-1 -1) (-1 0) (-1 1)
	     ( 0 -1) ( 0 0) ( 0 1)
	     ( 1 -1) ( 1 0) ( 1 1)])
	(ix 0))
    (while (< ix 9)
      (gess-put (+ x (car (aref dv ix))) (+ y (car (cdr (aref dv ix)))) 
		(if (aref pv ix) nil t))
      (setq ix (1+ ix)))))

(defun gess-pt-in-list (x y)
  "Check to see if position X, Y is in the valid moves list."
  (let ((tl gess-valid-moves))
    (while (and tl (or (/= x (car (car tl))) (/= y (car (cdr (car tl))))))
      (setq tl (cdr tl)))
    tl))

(defun gess-hilight-valid-spots ()
  "Check to see if position X, Y is in the valid moves list."
  (let ((tl gess-valid-moves))
    (while tl
      (let ((pos (gess-xy2index (car (car tl)) (car (cdr (car tl))))))
	(game-lib-add-overlay (+ pos 1)
			      (+ pos 2)
			      'gess-go-face
			      'gess-valid-spots))
      (setq tl (cdr tl)))))

(defun gess-direction-list (x y player delta distance)
  "Starting at X Y, for PLAYER travel in DELTA direction and return a
list of valid squares.  If DISTANCE, travel forever (until border)
otherwise, only travel up to 3 away."
  (let* ((xd (car delta))
	 (yd (car (cdr delta)))
	 (tx (+ x xd))
	 (ty (+ y yd))
	 (tl nil)
	 (end nil)
	 (op (if (= player 1) 2 1)))
    (while (not end)
      (if (not (or (> tx 18) (< tx 1) (> ty 18) (< ty 1)))
	  (setq tl (cons (list tx ty) tl)))
      (setq end
	    (or
	     ;; This checks one square twice for diagnals, but it
	     ;; makes for cheap code.
	     (if (/= 0 xd)
		 (or
		  (gess-owned-one (+ tx xd) (+ ty -1) 3)
		  (gess-owned-one (+ tx xd) ty 3)
		  (gess-owned-one (+ tx xd) (+ ty 1) 3))
	       nil)
	     (if (/= 0 yd)
		 (or
		  (gess-owned-one (+ tx -1) (+ ty yd) 3)
		  (gess-owned-one tx (+ ty yd) 3)
		  (gess-owned-one (+ tx 1) (+ ty yd) 3))
	       nil)
	     ;; edges of board
	     (> tx 18)
	     (< tx 1)
	     (> ty 18)
	     (< ty 1)
	     ;; distance traveled:
	     ;; middle is declared in a LET in the caller
	     (and (not middle) (= 3 (length tl)))))
      (setq tx (+ tx xd))
      (setq ty (+ ty yd)))
    tl))

(defun gess-ring-p (x y player)
  "Return t if the position x y is a ring."
  (let ((rv (gess-get-piece-vector x y player)))
    (and (aref rv 0)      (aref rv 1)  (aref rv 2)
	 (aref rv 3) (not (aref rv 4)) (aref rv 5)
	 (aref rv 6)      (aref rv 7)  (aref rv 8))))

(defun gess-ring-somewhere (player)
  "return t if that is a winning move"
  (message "We look for rings ... ")
  (let ((x 1) (y 1) (found nil))
    (while (and (< y 18) (not found))
      (while (and (< x 18) (not found))
	(setq found (gess-ring-p x y player))
	(if (not found)	(setq x (1+ x))))
      (if (not found)
	  (progn
	    (setq x 1)
	    (setq y (1+ y)))))
    (message "We look for rings ... %s" 
	     (if found "Here it is!" "It is broken!"))
    (sleep-for 1)
    (if found
	(cond ((= player 1)
	       (setq gess-p1-ring-x x)
	       (setq gess-p2-ring-y y))
	      ((= player 2)
	       (setq gess-p2-ring-x x)
	       (setq gess-p2-ring-y y))))
    found))
  
(defun gess-owned-one (x y who)
  "Returns t if WHO owns the block.  If WHO is t, then return t if
there is either player is there."
  (save-excursion
    (goto-char (+ (gess-xy2index x y) 1))
    (cond
     ((= (following-char) ?. )
      nil)
     ((and (= (following-char) (string-to-char gess-piece1))
	   (= who 1))
      t)
     ((and (= (following-char) (string-to-char gess-piece2))
	   (= who 2))
      t)
     ((and (/= (following-char) ?. )
	   (= who 3))
      t)
     (t nil))))

(defun gess-move ()
  "Move the cursor to a new position"
  (interactive)
  (if (or (= last-input-char ?9)
	  (= last-input-char ?6)
	  (= last-input-char ?3)
	  (= last-input-char ?\C-f)
	  (= last-input-char ?f))
      (if (< gess-x 18)
	  (setq gess-x (+ gess-x 1))
	(error "Can't go farther right!")))
   (if (or (= last-input-char ?7)	
	   (= last-input-char ?4)
	   (= last-input-char ?1)	
	   (= last-input-char ?\C-b)
	   (= last-input-char ?b))
       (if (> gess-x 1)
	   (setq gess-x (- gess-x 1))
	 (error "Can't go farther left!")))
   (if (or (= last-input-char ?9)
	   (= last-input-char ?8)
	   (= last-input-char ?7)
	   (= last-input-char ?\C-p)
	   (= last-input-char ?p))
       (if (> gess-y 1)
	   (setq gess-y (- gess-y 1))
	 (error "Can't go farther up!")))
   (if (or (= last-input-char ?1)
	   (= last-input-char ?2)
	   (= last-input-char ?3)
	   (= last-input-char ?\C-n)
	   (= last-input-char ?n))
       (if (< gess-y 18)
	   (setq gess-y (+ gess-y 1))
	 (error "Can't go farther down!")))
   (gess-place-cursor))

(defun gess-put (x y &optional off)
  "Based on variable \"turn\" place peice there."

  (game-lib-insert-string (+ 1 (gess-xy2index x y))
			  (if (equal off t) "."
			    (if (= tyrant-turn 1)
				gess-piece1
			      gess-piece2))
			  (if off
			      nil
			    (if (= tyrant-turn 1)
				'game-lib-player1-face
			      'game-lib-player2-face))))

(defun gess-place-cursor ()
  "Place the cursor on the correct spot on the board..."

  (goto-char (+ 1 (gess-xy2index gess-x gess-y))))

(defun gess-xy2index (x y)
  "change x y position to absolute buffer address"

  (+ (* x 2) 4 (* y 45) 45))

(defun gess-swap-turns ()
  "Swap turns in connect 4"
  (game-lib-swap-turns "It is now %P's turn."
		       "Player %d's turn, Can you make us go?"))

(defun gess-quit ()
  "quit 4 by 4"
  
  (interactive)
  (game-lib-quit t))

;;; end of lisp

(provide 'gess)
