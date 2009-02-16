;;; chess.el - chess game interface
;;;
;;; Copyright (C) 1995 Eric M. Ludlam
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
;;; $Id: chess.el,v 1.2 1995/12/10 16:50:08 zappo Exp $
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

(require 'game-lib "games/game-lib.el")

(defvar chess-map nil
  "Keymap used in playing chess")

(if chess-map
    ()
  (setq chess-map (make-sparse-keymap))
  (define-key chess-map "\C-f" 'chess-move)  
  (define-key chess-map "\C-b" 'chess-move)
  (define-key chess-map "\C-p" 'chess-move)
  (define-key chess-map "\C-n" 'chess-move)
  (define-key chess-map "f" 'chess-move)  
  (define-key chess-map "b" 'chess-move)
  (define-key chess-map "p" 'chess-move)
  (define-key chess-map "n" 'chess-move)
  (define-key chess-map "1" 'chess-move)
  (define-key chess-map "2" 'chess-move)
  (define-key chess-map "3" 'chess-move)
  (define-key chess-map "4" 'chess-move)
  (define-key chess-map "6" 'chess-move)
  (define-key chess-map "7" 'chess-move)
  (define-key chess-map "8" 'chess-move)
  (define-key chess-map "9" 'chess-move)
  (define-key chess-map " " 'chess-grab-or-drop)
  (define-key chess-map "5" 'chess-grab-or-drop)
  (define-key chess-map "q" 'chess-quit)
  (game-lib-add-mouse-support chess-map)
)

(defconst chess-piece1-vector [ " " "p" "r" "n" "b" "q" "k" ]
  "Vector of piece symbols for player 1")

(defconst chess-piece2-vector [ " " "P" "R" "N" "B" "Q" "K" ]
  "Vector of piece symbols for player 2")

(defconst chess-piece-names [ "No Piece" "Pawn" "Rook" "Knight" "Bishop"
			      "Queen" "King" ]
  "Vector of piece full names for use in describing a move")

(defconst chess-piece-completion-list 
  '( ("Rook" . 2)
     ("Knight" . 3)
     ("Bishop" . 4)
     ("Queen" . 5) )
  "Alist (table for completion) of pieces you can get when you promote
a pawn.  The associated number is the piece index.")

(defconst chess-piece-move-hooks
  [ ( )
    ( chess-pawn-move )
    ( chess-rook-move )
    ( chess-knight-move )
    ( chess-bishop-move )
    ( chess-rook-move chess-bishop-move )
    ( chess-king-move chess-king-castle ) ]
  "Lists of move list generators for each individual piece")

(defconst chess-stat "*Chess Statistics*"
  "Holds the buffer name of the chess statistics buffer")

(defvar chess-x 1
  "Current X position in chess board")

(defvar chess-y 1
  "Current Y position in chess board")

(defvar chess-grab-x nil
  "Current X position of grabbed piece")
(defvar chess-grab-y nil
  "Current Y position of grabbed piece")
(defvar chess-valid-moves nil
  "List of possible moves iff a piece has been selected.")
(defvar chess-player1-captured nil
  "List of player 1's pieces which have been captured")
(defvar chess-player2-captured nil
  "List of player 2's pieces which have been captured")
(defvar chess-captured nil
  "Stores the type index of the piece last captured")
(defvar chess-special-string nil
  "Special status string printed in the status buffer after a move if
it's value is set")
(defvar chess-grabbed-piece nil
  "The piece index which is currently grabbed")

(defun chess ()
  "Start up chess mode for use with tyrants"
  (interactive)
  (switch-to-buffer (get-buffer-create "*Chess*"))
  (setq mode-name "Chess")
  (setq major-mode 'chess)
  (delete-other-windows (selected-window))
  (use-local-map chess-map)
  (game-lib-clear-buffer)
  (game-lib-insert-string 1 "    a   b   c   d   e   f   g   h      |
  +---+---+---+---+---+---+---+---+    |
1 | r | n | b | q | k | b | n | r | 1  |
  +---+---+---+---+---+---+---+---+    |
2 | p | p | p | p | p | p | p | p | 2  |
  +---+---+---+---+---+---+---+---+    |
3 |   |   |   |   |   |   |   |   | 3  |
  +---+---+---+---+---+---+---+---+    |
4 |   |   |   |   |   |   |   |   | 4  |
  +---+---+---+---+---+---+---+---+    |
5 |   |   |   |   |   |   |   |   | 5  |
  +---+---+---+---+---+---+---+---+    |
6 |   |   |   |   |   |   |   |   | 6  |
  +---+---+---+---+---+---+---+---+    |
7 | P | P | P | P | P | P | P | P | 7  |
  +---+---+---+---+---+---+---+---+    |
8 | R | N | B | Q | K | B | N | R | 8  |
  +---+---+---+---+---+---+---+---+    |
    a   b   c   d   e   f   g   h      |
" nil)
  (make-local-variable 'tyrant-turn)
  (setq tyrant-turn 1
	chess-x 1
	chess-y 1
	chess-valid-moves nil
	chess-grab-x nil
	chess-grab-y nil
	chess-player1-captured nil
	chess-player2-captured nil
	)
  (let ((x 1) (y 1))
    (while (<= y 8)
      (setq x 1)
      (while (<= x 8)
	(chess-put x y nil)		;update colors in that cell
	(setq x (1+ x)))
      (setq y (1+ y))))
  (make-local-variable 'tyrant-mouse-function)
  (setq tyrant-mouse-function 'chess-mouse-support)
  ;; Modify the display
  (split-window (selected-window) 50 t)
  (other-window 1)
  (switch-to-buffer (get-buffer-create chess-stat))
  (game-lib-clear-buffer)
  (insert "Game has started

Player 1 is lower case.

Player 2 is upper case.")
  (other-window 1)
  (chess-place-cursor)

  ;; ok.. setup hooks dependant on wether you are player1 or player2
  (setq tyrant-player1-hook 
	'(lambda ()
	   (save-excursion
	     (set-buffer chess-stat)
	     (make-local-variable 'etalk-tag)
	     (setq etalk-tag t))
	   (message "You are player 1")))
  (setq tyrant-player2-hook
	'(lambda ()
	   (save-excursion
	     (set-buffer chess-stat)
	     (make-local-variable 'etalk-tag)
	     (setq etalk-tag t))
	   (setq etalk-tyrant-enabled-console nil)
	   (message "Your are player 2"))) ;player2 goes 2nd
  )

(defun chess-mouse-support(p e m)
  "Reads in a mouse event from the game-lib driver, and allows a
player to click on a square."

  ;; find which square we are closest to and go there
  (if (and (not (memq 'drag m)) (integerp p))
      (let ((first-line 1)
	    (line-width 41)
	    (block-width 3)
	    (block-height 1)
	    (block-vsep 1)			;vertical separator width
	    (block-hsep 1)			;horizontal sep width
	    x y xt yt
	    )
	(setq yt (/ (- p (* first-line line-width)) line-width))
	(setq xt (- (% p line-width) 3))
	(setq y (+ (/ yt (+ block-height block-vsep)) 1))
	(setq x (+ (/ xt (+ block-width block-hsep)) 1))
	
	(if (not (or (< (% xt (+ block-width block-hsep)) block-hsep)
		     (< (% yt (+ block-height block-vsep)) block-vsep)
		     (< x 0) (> x 8) (< y 0) (> y 8)))
	    (progn
	      (if (= ?% (save-excursion
			  (goto-char (+ (chess-xy2index x y) 1))
			  (following-char)))
		  (message "Illegal move.")
		(setq chess-x x)
		(setq chess-y y)
		(goto-char (+ (chess-xy2index x y) 1))
		(if (memq 'click m)
		    (chess-grab-or-drop))))))
    )
  )

(defun chess-move ()
  "Move the cursor based on last input character"
  (interactive)
  (if (or (= last-input-char ?9)
	  (= last-input-char ?6)
	  (= last-input-char ?3)
	  (= last-input-char ?\C-f)
	  (= last-input-char ?f))
      (if (< chess-x 8)
	  (setq chess-x (+ chess-x 1))
	(error "Can't go farther right!")))
   (if (or (= last-input-char ?7)	
	   (= last-input-char ?4)
	   (= last-input-char ?1)	
	   (= last-input-char ?\C-b)
	   (= last-input-char ?b))
       (if (> chess-x 1)
	   (setq chess-x (- chess-x 1))
	 (error "Can't go farther left!")))
   (if (or (= last-input-char ?9)
	   (= last-input-char ?8)
	   (= last-input-char ?7)
	   (= last-input-char ?\C-p)
	   (= last-input-char ?p))
       (if (> chess-y 1)
	   (setq chess-y (- chess-y 1))
	 (error "Can't go farther up!")))
   (if (or (= last-input-char ?1)
	   (= last-input-char ?2)
	   (= last-input-char ?3)
	   (= last-input-char ?\C-n)
	   (= last-input-char ?n))
       (if (< chess-y 8)
	   (setq chess-y (+ chess-y 1))
	 (error "Can't go farther down!")))
   (chess-place-cursor))

(defun chess-grab-or-drop ()
  "From the current square, either grab the current chess piece, or
drop a piece currently grabbed at a new (or same) location."
  (interactive)
  (setq chess-captured nil		;fix this ahead of time!
	chess-special-string nil)
  (if (and (not chess-grab-x) (not chess-grab-y))
      ;; in this case, select the piece under the cursor (if it is
      ;; ours) and then generate the list of valid squares we can move
      ;; to.
      (progn
	(if (chess-owner chess-x chess-y tyrant-turn)
	    (let* ((pi  (chess-get-piece-index chess-x chess-y tyrant-turn))
		   (gml (aref chess-piece-move-hooks pi))
		   (tl nil))
	      ;; Mark the square, and then collect all the possible
	      ;; moves, and mark them
	      (setq chess-grab-x chess-x
		    chess-grab-y chess-y
		    chess-grabbed-piece (chess-get-piece-index
					 chess-x chess-y tyrant-turn))
	      (chess-put chess-x chess-y nil nil t)
	      (while gml
		(setq tl (append chess-valid-moves
				 (funcall (car gml) chess-x chess-y
					  tyrant-turn) tl)
		      gml (cdr gml)))
	      (setq chess-valid-moves tl)
	      (while tl (chess-put 
			 (car (car tl)) (car (cdr (car tl))) nil nil nil
			 (not (car (cdr (cdr (car tl)))))
			 (car (cdr (cdr (car tl)))))
		     (setq tl (cdr tl)))
	      (chess-update-stats)
	      )
	  ;; Signal an error
	  (error "You cannot grab that piece!"))
	)
    ;; In this case, make sure the cursor is in a valid square, and if
    ;; so, move there, preform any captures, and then clear all
    ;; squares marked as movable
    (if (and (= chess-grab-x chess-x) (= chess-grab-y chess-y))
	(progn
	  ;; Ungrab the piece that is there
	  (setq chess-grab-x nil
		chess-grab-y nil)
	  (chess-update-stats)
	  (setq chess-grabbed-piece nil)
	  (chess-put chess-x chess-y nil)
	  (chess-clear-valid-moves))
      ;; check if piece is in our list of valid squares
      (let ((loop chess-valid-moves))
	(while loop
	  (if (and (= chess-x (car (car loop)))
		   (= chess-y (car (cdr (car loop)))))
	      (if (car (cdr (cdr (car loop))))
		  ;; In this case, call the function with X Y PLAYER
		  ;; to do the move because it has a relation to
		  ;; other peices we cannot predict
		  (funcall (car (cdr (cdr (car loop))))
			   chess-x chess-y tyrant-turn)
		;; Move piece to this new location, and clear list
		;; This is a normal move, capturing whoever you land on.
		(let ((mypiece (chess-get-piece-index 
				chess-grab-x chess-grab-y tyrant-turn)))
		  (chess-put chess-grab-x chess-grab-y 0)
		  (chess-clear-valid-moves)
		  ;; store the piece just captured
		  (setq chess-captured (chess-get-piece-index 
					chess-x chess-y
					(if (= tyrant-turn 1) 2 1)))
		  (if chess-captured
		      (if (= tyrant-turn 2)
			  (setq chess-player1-captured
				(cons chess-captured chess-player1-captured))
			(setq chess-player2-captured
			      (cons chess-captured chess-player2-captured))))
		  ;; move our piece AFTER clearing valid moves
		  (chess-put chess-x chess-y mypiece tyrant-turn)
		  ;; Lastly check for after-the-fact happings
		  (chess-pawn-promote-p chess-x chess-y tyrant-turn))
		  ;; Must update before nulling grab ptrs
		(chess-update-stats)
		(setq chess-grab-x nil
		      chess-grab-y nil
		      chess-grabbed-piece nil
		      loop nil)
		;; Lastly, lets take a peek to see if we just won!
		(if (equal chess-captured 6)
		    (game-lib-win tyrant-turn
				  "%P has captured his opponants king!"
				  "Player %d has captured his opponants king!")
		  (chess-swap-turns))))
	  (setq loop (cdr loop)))
	(if (or chess-grab-x chess-grab-y)
	    (error "Cannot move to position %d %d" chess-x chess-y))))))

(defun chess-clear-valid-moves ()
  "Run through the valid moves list, and clear them all"
  (while chess-valid-moves
    (chess-put (car (car chess-valid-moves)) 
	       (car (cdr (car chess-valid-moves))) nil)
    (setq chess-valid-moves (cdr chess-valid-moves))))
  
(defun chess-owner (x y &optional player)
  "At position X Y returns a list.  If it returns nil, then it is
empty, otherwise the car is the player (1 or 2), and the cdar is the
piece index in the array of pieces (1 .. n)"
  ;; Return t if it belongs to player
  (if player (numberp (chess-get-piece-index x y player))
    ;; otherwise, return index of player who it belongs to
    (let ((pi (chess-get-piece-index x y 1)))
      ;; Use get-piece-index to do hard work, the extropolate the data
      (if (equal pi nil) nil (if (equal pi t) 2 1)))))

(defun chess-get-piece-index (x y player)
  "Returns the index of piece at X Y if it belongs to PLAYER.  If it
is blank, return nil, otherwise return t for an opponant's piece"
  (let ((ps (chess-piece-at-pos x y))
	(pv (if (= player 1) chess-piece1-vector chess-piece2-vector))
	(i 1))
    (if ps
	(if (string= ps " ") nil
	  (while (and (< i (length pv)) (not (string= ps (aref pv i))))
	    (setq i (1+ i)))
	  (if (= i (length pv)) t i))
      'off-board)))

(defun chess-piece-at-pos (x y)
  "returns the string to the pieces at X Y"
  (if (or (< x 1) (> x 8) (< y 1) (> y 8)) nil
    (save-excursion (goto-char (1+ (chess-xy2index x y)))
		    (buffer-substring (point) (1+ (point))))))

(defun chess-put (x y piece &optional player mark selected special)
  "Place at X Y the token PIECE, where nil means to leave the existing
piece in the square.  If optional arg PLAYER is set, use that players
vector, otherwise use vector specified by tyrant-turn.  If optional
arg MARK is set, then place '> <' characters around the marked piece.
If optional arg SELECTED is set, will place '( )' around pieces which
can be moved to."
  (if (or (< x 1) (> x 8) (< y 1) (> y 8)) nil
    (let* ((pos (chess-xy2index x y))
	   (pv (if player
		   (if (= player 1) chess-piece1-vector chess-piece2-vector)
		 (if (= tyrant-turn 1) chess-piece1-vector chess-piece2-vector)))
	   (pc  (if (equal piece nil)
		    (chess-piece-at-pos x y)
		  (aref pv piece)))
	   (ns  (concat (cond (mark ">") (selected "(") (special "!") (t " "))
			pc
			(cond (mark "<") (selected ")") (special "!") (t " "))))
	   ;; recalc which player we are
	   (pl  (if (and (not player) (not piece))
		    (if (string= pc " ") tyrant-turn
		      (if (and (>= (aref pc 0) (aref "a" 0)) 
			       (<= (aref pc 0) (aref "z" 0))) 
			  1 2))
		  (if (not player)
		      tyrant-turn
		    player)))
	   (nf   (if (= pl 1) 'game-lib-player1-face 'game-lib-player2-face)))
      (game-lib-insert-string pos ns nf))))

(defun chess-detect-check (player)
  "Quickly check to see if PLAYER is in check."

  )

(defun chess-get-delta-list (x y dx dy &optional player)
  "Starting at X Y, travel in direction specified by DX and DY, and
collect a list of empty squares.  If PLAYER is set, the also include
the last square if an opponant's piece is there."
  (setq x (+ x dx)
	y (+ y dy))
  (let ((nl nil) tp)
    (while (not (setq tp (chess-get-piece-index x y player)))
      (setq nl (cons (list x y) nl)
	    x (+ x dx)
	    y (+ y dy)))
    (if (and player (equal tp t)) 
	(setq nl (cons (list x y) nl)))
    nl))

(defun chess-get-list-deltas (x y player d)
  "From position X Y for PLAYER, check list D of delta's, and return a
list of places which are valid to move to (captures or empties)"
  (let ((nl nil))
    (while d
      (if (member (chess-get-piece-index (+ x (car (car d))) 
					 (+ y (car (cdr (car d)))) player)
		  '(t nil))
	  (setq nl (cons (list  (+ x (car (car d))) (+ y (car (cdr (car d)))))
			 nl)))
      (setq d (cdr d)))
    nl))

(defun chess-place-cursor ()
  "Place the cursor on the correct spot on the board..."
  (goto-char (+ 1 (chess-xy2index chess-x chess-y))))

(defun chess-xy2index (x y)
  "change x y position to absolute buffer address"
  (+ (+ (+ 0 (* x 4)) (+ (* y 82) 0)) 0))

(defun chess-pawn-move (x y player)
  "Return a list of possible moves for a pawn"
  (let ((nl nil)
	(pd (if (= player 1) 1 -1)))
    ;; check first move two ahead
    (if (and (or (and (= player 1) (= y 2))
		 (and (= player 2) (= y 7)))
	     (not (chess-get-piece-index x (+ y pd pd) player)))
	(setq nl (cons (list x (+ y pd pd)) nl)))
    ;; check directly ahead
    (if (not (chess-get-piece-index x (+ y pd) player))
	(setq nl (cons (list x (+ y pd)) nl)))
    ;; check the two captures
    (if (equal t (chess-get-piece-index (1+ x) (+ y pd) player))
	(setq nl (cons (list (1+ x) (+ y pd)) nl)))
    (if (equal t (chess-get-piece-index (1- x) (+ y pd) player))
	(setq nl (cons (list (1- x) (+ y pd)) nl)))
    nl))

(defun chess-pawn-promote-p (x y player)
  "If piece at X Y is a pawn, and it's in the last row of the board,
then promote it, and change it.  This function will be called AFTER
each move."
  ;; check 1 or 8, since a pawn never goes backwards
  (if (and (= (chess-get-piece-index x y player) 1) (or (= y 1) (= y 8)))
      (let* ((pl (assoc (game-lib-query "Promote pawn to: " "Queen" 
					chess-piece-completion-list)
			chess-piece-completion-list))
	     (pi (cdr pl)))
	(chess-put x y pi player)
	(setq chess-special-string (concat chess-special-string
					   "Pawn promoted to "
					   (car pl))))))

(defun chess-rook-move (x y player)
  "Return a list of available places a rook could move to from X Y for PLAYER"
  (append (chess-get-delta-list x y  0  1 player)
	  (chess-get-delta-list x y  0 -1 player)
	  (chess-get-delta-list x y  1  0 player)
	  (chess-get-delta-list x y -1  0 player)))

(defun chess-knight-move (x y player)
  "Return a list of available places a knight could move to from X Y for PLAYER"
  (chess-get-list-deltas 
   x y player '((1 2) (2 1) (-1 2) (2 -1) (1 -2) (-2 1) (-1 -2) (-2 -1))))

(defun chess-bishop-move (x y player)
  "Return a list of available places a bishop could move to from X Y for PLAYER"
  (append
   (chess-get-delta-list x y  1  1 player)
   (chess-get-delta-list x y  1 -1 player)
   (chess-get-delta-list x y -1  1 player)
   (chess-get-delta-list x y -1 -1 player)) 
 )
(defun chess-king-move (x y player)
  "Return a list of available places a king could move to from X Y for PLAYER"
  (chess-get-list-deltas 
   x y player '((1 0) (-1 0) (0 1) (1 1) (-1 1) (0 -1) (1 -1) (-1 -1))))

(defun chess-castle-move (x y player)
  "Castle PLAYER's king with rook at position X Y"
  ;; At this point, assume error checking has been done so just do the move
  (cond ((= x 8)
	 (setq chess-special-string "Castles right")
	 (chess-put 6 y 2 player)
	 (chess-put 7 y 6 player)
	 (chess-put 5 y 0 player)
	 (chess-put 8 y 0 player))
	((= x 1)
	 (setq chess-special-string "Castles Left")
	 (chess-put 3 y 2 player)
	 (chess-put 2 y 6 player)
	 (chess-put 5 y 0 player)
	 (chess-put 1 y 0 player)))
  ;;cleanup work
  (chess-clear-valid-moves)
  (chess-update-stats)
  (setq chess-grab-x nil
	chess-grab-y nil
	chess-grab-y nil)
  (chess-swap-turns)
  )

(defun chess-king-castle (x y player)
  "Return a list of available places a king could castle to from X Y for PLAYER"
  (let ((nl nil))
    (if (and (= player 1) (= x 5) (= y 1)
	     (equal 2 (chess-get-piece-index 8 1 1))
	     (equal nil (chess-get-piece-index 6 1 1))
	     (equal nil (chess-get-piece-index 7 1 1)))
	(setq nl (cons (list 8 1 'chess-castle-move) nl)))
    (if (and (= player 1) (= x 5) (= y 1)
	     (equal 2 (chess-get-piece-index 1 1 1))
	     (equal nil (chess-get-piece-index 2 1 1))
	     (equal nil (chess-get-piece-index 3 1 1)))
	(setq nl (cons (list 1 1 'chess-castle-move) nl)))
    (if (and (= player 2) (= x 5) (= y 8)
	     (equal 2 (chess-get-piece-index 8 8 2))
	     (equal nil (chess-get-piece-index 6 8 2))
	     (equal nil (chess-get-piece-index 7 8 2)))
	(setq nl (cons (list 8 8 'chess-castle-move) nl)))
    (if (and (= player 2) (= x 5) (= y 8)
	     (equal 2 (chess-get-piece-index 1 8 2))
	     (equal nil (chess-get-piece-index 2 8 2))
	     (equal nil (chess-get-piece-index 3 8 2)))
	(setq nl (cons (list 1 8 'chess-castle-move) nl)))
    nl))

(defun chess-adjust-capture-string (p-lost win-width)
  "Put CRs into a string to fill it smartly for the status buffer"
  (let ((ts 1)
	(vcol 1)
	(lcol 1))
    (while (< ts (length p-lost))
      (if (= (aref p-lost ts) ? )
	  (setq vcol ts))
      (if (> lcol win-width)
	  (progn
	    (aset p-lost vcol ?\n)
	    (setq lcol 1)))
      (setq ts (1+ ts)
	    lcol (1+ lcol)))))

(defun chess-update-stats ()
  "Looks at all the data currently available and updates the stats display"
  (let* ((tyrn (and (boundp 'etalk-tyrannical-mode) etalk-tyrannical-mode))
	 (turn tyrant-turn)
	 (loop nil)
	 (ts nil)
	 (win-width (window-width (get-buffer-window chess-stat)))
	 (p1-lost (progn (setq loop chess-player1-captured
			       ts "")
			 (while loop
			   (setq ts (concat (aref chess-piece-names (car loop))
					    (if (> (length ts) 1) ", ")
					    ts)
				 loop (cdr loop)))
			 ts))
	 (p2-lost (progn (setq loop chess-player2-captured
			       ts "")
			 (while loop
			   (setq ts (concat (aref chess-piece-names (car loop))
					    (if (> (length ts) 1) ", ")
					    ts)
				 loop (cdr loop)))
			 ts))
	 (state (concat
		 (if (and chess-grab-x chess-grab-y)
		     (if (or (/= chess-x chess-grab-x)
			     (/= chess-y chess-grab-y))
			 ;; This signifies a move to a new position
			 (format "%s %d %d -> %d %d"
				 (aref chess-piece-names chess-grabbed-piece)
				 chess-grab-x chess-grab-y
				 chess-x chess-y)
		       (format "%s %d %d ->" 
			       (aref chess-piece-names chess-grabbed-piece)
			       chess-grab-x chess-grab-y))
		   (format "Released %s" (aref chess-piece-names
					       chess-grabbed-piece)))
		 (if chess-special-string
		     (concat "\n  " chess-special-string)
		   "")
		 (if chess-captured 
		     (concat "\n  Captured " 
			     (aref chess-piece-names chess-captured))
		   "")))
	 )
    (if (not win-width) 
	(setq win-width 25) 
      (setq win-width (- win-width 2)))
    
    ;; Quickly scan the printed lists for length, then insert /n's where
    ;; applicable
    (chess-adjust-capture-string p1-lost win-width)
    (chess-adjust-capture-string p2-lost win-width)

    (save-window-excursion
      (set-buffer chess-stat)
      (game-lib-clear-buffer)
      (game-lib-insert-string (point-max) "Player 1 Lost Pieces:\n" nil)
      (game-lib-insert-string (point-max) p1-lost 'game-lib-player1-face)
      (game-lib-insert-string (point-max) "\n\nPlayer 2 Lost Pieces:\n" nil)
      (game-lib-insert-string (point-max) p2-lost 'game-lib-player2-face)
      (if tyrn
	  (game-lib-insert-string (point-max) (tyrant-format "\n\n%:1p Move:\n") nil)
	(game-lib-insert-string (point-max) (format "\n\nPlayer %d's Move:\n" turn) nil))
      (game-lib-insert-string (point-max) state nil)
      )))

(defun chess-swap-turns ()
  "Swap turns in chess"
  (game-lib-swap-turns "It is now %P's turn" "Player %d's move"))

(defun chess-quit ()
  "Quit chess"
  (interactive)
  (delete-window (get-buffer-window chess-stat))
  (kill-buffer chess-stat)
  (game-lib-quit t))

;; end of list
(provide 'chess)
