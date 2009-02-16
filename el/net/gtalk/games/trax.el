;;; Trax - Interface to play that weird trax game
;;;
;;; Copyright (C) 1995 Eric M Ludlam
;;;
;;; Note on copyright:  The copyright on this lisp code is owned by
;;; me.  The algorithms/techniques to determine tilefilling was
;;; created by me for elisp, but pre-existed in "traxfig" which is (as
;;; far as I can tell) public domain.  The game of trax is owned by
;;; David Smith (EXTL451@csc.canterbury.ac.nz)
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
;;; $Id: trax.el,v 1.4 1995/05/29 13:08:53 zappo Exp $
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
(require 'tile-lib "games/tile-lib")

(defvar trax-map nil
  "Keymap used in playing Trax")

(if trax-map
    ()
  (setq trax-map (make-sparse-keymap))
  (define-key trax-map "f" 'trax-move)
  (define-key trax-map "b" 'trax-move)
  (define-key trax-map "n" 'trax-move)
  (define-key trax-map "p" 'trax-move)
  (define-key trax-map "\C-f" 'trax-move)
  (define-key trax-map "\C-b" 'trax-move)
  (define-key trax-map "\C-n" 'trax-move)
  (define-key trax-map "\C-p" 'trax-move)
  (define-key trax-map " " 'trax-select-piece)
  (define-key trax-map "\C-m" 'trax-drop-piece)
  (define-key trax-map "q" 'trax-quit)
)

(defconst trax-tile-list 
  (list 
   ;; empty
   '("+.....+"
     ":     :"
     ":  .  :"
     ":     :"
     "+.....+")
   ;; two cross pieces
   '("+..#..+"
     ":  #  :" 
     "-------" 
     ":  #  :" 
     "+..#..+")
   '("+..|..+" 
     ":  |  :" 
     "###|###" 
     ":  |  :" 
     "+..|..+")
   ;; curly pieces
   '("+..#..+" 
     ": #   :" 
     "##  /--" 
     ":  /  :" 
     "+..|..+")
   '("+..#..+" 
     ":   # :" 
     "--\\  ##" 
     ":  \\  :" 
     "+..|..+")
   '("+..|..+" 
     ":  /  :" 
     "--/  ##" 
     ":   # :" 
     "+..#..+")
   '("+..|..+" 
     ":  \\  :" 
     "##  \\--" 
     ": #   :" 
     "+..#..+"))
  "This defines a list of tiles which represent the different tiles to
be placed on the trax tile board.")

(defconst trax-side-list
  '( nil
     [ 2 2 1 1 ]
     [ 1 1 2 2 ]
     [ 2 1 2 1 ]
     [ 2 1 1 2 ]
     [ 1 2 1 2 ]
     [ 1 2 2 1 ]
     )
  "Define a vector of 4 elts, where the elts are top bottm left right,
and represent the play index coming out of that side.  A match must be
made when placing a tile.")

(defconst traxt-empty 0 "Trax tile index.")
(defconst traxt-cross-v 1 "Trax tile index.")
(defconst traxt-cross-h 2 "Trax tile index.")
(defconst traxt-curve1 3 "Trax tile index.")
(defconst traxt-curve2 4 "Trax tile index.")
(defconst traxt-curve3 5 "Trax tile index.")
(defconst traxt-curve4 6 "Trax tile index.")

(defvar trax-cursor-x nil
  "The X position of the cursor, relative the the origin specified by
the printing routine.")

(defvar trax-cursor-y nil
  "The Y position of the cursor, relative the the origin specified by
the printing routine.")

(defvar trax-selected-piece nil
  "The piece currently selected under the cursor.  This is used to
select a tile up to the point where the user hits RET and places this
tile on the board.")

(defvar trax-inhibit-startup-message nil
  "Inhibit the copyright information displayed when a game first starts up.")

(defun trax ()
  "Initialize a trax board and screen for the playing of the trax
game.  To play, line up tiles in the board and attempt to connect your
color of lines into a circle, or attempt to span the entire board of
of at least 8 tiles from edge to edge.  By convention, player1 is
white, and his pipes are the | and - characters, and player 2 is read,
represented by the # characters.  Movement is via the normal emacs
movement keys:

  f, C-f  Foreward
  b, C-b  Backward
  n, C-n  Next line
  p, C-p  Back line

To select a piece, press SPCbar, and all tiles you are allowed to
place are cycled through on the board.  A . character will be in the
tile until it is selected.  To place that piece, press RET to verify
that move.  Emacs will then play any forced moves that need to be
done.  "
  (interactive)
  (switch-to-buffer (get-buffer-create "*TRAX*"))
  (delete-other-windows (selected-window))  
  (use-local-map trax-map)
  (setq major-mode 'trax)
  (setq mode-name "Trax")
  (game-lib-clear-buffer)
  (make-variable-buffer-local 'tile-lib-width)
  (setq tile-lib-width 6)
  (make-variable-buffer-local 'tile-lib-height)
  (setq tile-lib-height 4)
  (make-variable-buffer-local 'tile-lib-board)
  (setq tile-lib-board (tile-lib-make-board 50 50))
  (make-variable-buffer-local 'tile-lib-tile-list)
  (setq tile-lib-tile-list trax-tile-list)
  (make-variable-buffer-local 'tile-lib-origin-x)
  (setq tile-lib-origin-x 25)
  (make-variable-buffer-local 'tile-lib-origin-y)
  (setq tile-lib-origin-y 25)
  (make-variable-buffer-local 'tile-lib-max-x)
  (setq tile-lib-max-x 25)
  (make-variable-buffer-local 'tile-lib-max-y)
  (setq tile-lib-max-y 25)
  (make-variable-buffer-local 'trax-cursor-x)
  (setq trax-cursor-x 0)
  (make-variable-buffer-local 'trax-cursor-y)
  (setq trax-cursor-y 0)
  (make-variable-buffer-local 'trax-selected-piece)
  (setq trax-selected-piece traxt-empty)
  (trax-put-tile 0 0 traxt-empty)

  ;; Now do all the tyrant stuff
  (make-local-variable 'tyrant-turn)
  (setq tyrant-turn 1)
  (make-local-variable 'etalk-tyrant-brief-help)
  (setq etalk-tyrant-brief-help
	"TRAX: [C-] f b p n Move Selection. SPC select a tile. RET Drop tile.")

  ;; ok.. setup hooks dependant on wether you are player1 or player2
  (setq tyrant-player1-hook 
	'(lambda ()
	   (message "You are player 1")))
  (setq tyrant-player2-hook
	'(lambda ()
	   (setq etalk-tyrant-enabled-console nil)
	   (message "Your are player 2"))) ;player2 goes 2nd

  ;; We do the tiled board with the origin.
  (tile-lib-draw-board t)

  ;; Insert copyright blurb.
  (if trax-inhibit-startup-message
      nil
    (goto-char (point-max))
    (insert "-------------------------------------------------------
Lines indicated by ---\\  characters represent player 1
                      |
Lines indicated by ###   characters represent player 2
                      #
Officially:
  \"This program authorised for free use by David Smith of Christchurch, NZ
  who owns the copyright to the rules and the trade mark to the name Trax
  and reserves the right to withdraw this consent at some future date.\"
Personally: 
  I suggest that everyone who enjoys this write to him at:
  EXTL451@csc.canterbury.ac.nz and negotiate a purchase of nifty
  plastic tiles in a handy black bag so he doesn't decided to withdraw
  his consent at some future date.
Note:
  Put (setq trax-inhibit-startup-message t) in your .emacs to prevent
  this message from reappearing.")
    (setq trax-inhibit-startup-message t))
  ;; and finally, lets put the cursor initial position somewhere
  (trax-position-cursor)
)

(defun trax-position-cursor ()
  "Position the trax cursor on the selected tile."
  (tile-lib-goto trax-cursor-x trax-cursor-y t)
  ;; Make sure the point is always visible on the screen.
  (hscroll-point-visible))

(defun trax-put-tile (x y tile)
  "Puts a tile on the board relative to the origin of the game."
  (tile-lib-set (+ tile-lib-origin-x x) (+ tile-lib-origin-y y) tile))

(defun trax-available-p (x y &optional empty) 
  "Return t if the X Y position holds a tile.  If optional
empty, only return t if exists and is an empty tile space."
  (and
   (>= x 0) (>= y 0)
   (tile-lib-get (+ tile-lib-origin-x x) (+ tile-lib-origin-y y))
   (or (not empty)
       (save-excursion (tile-lib-goto x y t) (= (following-char) ?.)))))

(defun trax-get-match-vector (x y)
  "Return the match vector of the tile on square x, y"
  (let ((libindex (tile-lib-get (+ tile-lib-origin-x x) (+ tile-lib-origin-y y))))
    (and libindex (nth libindex trax-side-list))))

(defun trax-fit-p (x y index)
  "Return non-nil if at position X Y the INDEXed piece type will fit.
Rule is: for every neighbor that exists, match the owners on the edge
side from the trax-side-list.  If there are no neighbors (first play)
then only allow index 2 or 7 (white exiting on top edge pieces."
  (and (trax-available-p x y t)
       (let ((top (trax-get-match-vector x (1- y)))
	     (bot (trax-get-match-vector x (1+ y)))
	     (lft (trax-get-match-vector (1- x) y))
	     (rit (trax-get-match-vector (1+ x) y))
	     (np (nth index trax-side-list)))
	 (if (or top bot lft rit)
	     (if np
		 (and (or (not top) (= (aref top 1) (aref np 0)))
		      (or (not bot) (= (aref bot 0) (aref np 1)))
		      (or (not lft) (= (aref lft 3) (aref np 2)))
		      (or (not rit) (= (aref rit 2) (aref np 3))))
	       t)
	   (or (= index 0) (= index 2) (= index 5))))))

(defun trax-select-piece ()
  "Select the the square under the cursor, and keep rotating pieces
until the one the user wants appears.  this mean dropping a . in the
tile until the user hits RET"
  (interactive)
  (if (trax-available-p trax-cursor-x trax-cursor-y t) nil
    (error "That tile is not available."))
  (setq trax-selected-piece (1+ trax-selected-piece))
  (if (= trax-selected-piece 7) (setq trax-selected-piece 0))
  (while (not (trax-fit-p trax-cursor-x trax-cursor-y trax-selected-piece))
    (setq trax-selected-piece (1+ trax-selected-piece))
    (if (= trax-selected-piece 7) (setq trax-selected-piece 0)))
  (trax-put-tile trax-cursor-x trax-cursor-y trax-selected-piece)
  (tile-lib-draw trax-cursor-x trax-cursor-y 
		 (nth trax-selected-piece trax-tile-list) t)
  ;; (tile-lib-draw-board t)
  (trax-position-cursor)
  (delete-char 1)
  (insert ".")
  (forward-char -1))

(defun trax-move ()
  "Parse the last input character and try to go in the selected
direction."
  (interactive)
  (if (/= trax-selected-piece traxt-empty)
      (progn
	(setq trax-selected-piece traxt-empty)
	(trax-put-tile trax-cursor-x trax-cursor-y trax-selected-piece)
	(tile-lib-draw trax-cursor-x trax-cursor-y 
		       (nth trax-selected-piece trax-tile-list))))
  ;; now try to move the cursor somewhere
  (let ((newx (+ trax-cursor-x 
		 (if (or (= last-input-char ?f)
			 (= last-input-char ?\C-f)) 1 0)
		 (if (or (= last-input-char ?b)
			 (= last-input-char ?\C-b)) -1 0)))
	(newy (+ trax-cursor-y
		 (if (or (= last-input-char ?n)
			 (= last-input-char ?\C-n)) 1 0)
		 (if (or (= last-input-char ?p)
			 (= last-input-char ?\C-p)) -1 0))))
    (if (trax-available-p newx newy)
	(progn
	  (setq trax-cursor-x newx)
	  (setq trax-cursor-y newy))
      (message "Can't go that way!")
      (ding)))
  ;; and move the cursor
  (trax-position-cursor)
)

(defun trax-check-win-0 (tilex tiley)
  "Check for any winners for position at tilex and tiley.  This counts
for both players in both directions.  Go in all possible directions
{all 4} and return result of any of those!"
  (or 
   (trax-check-win-1 (+ trax-cursor-x 1) trax-cursor-y
		     trax-cursor-x trax-cursor-y
		     trax-cursor-x trax-cursor-y
		     (aref (trax-get-match-vector 
			    trax-cursor-x trax-cursor-y) 3))
   (trax-check-win-1 (- trax-cursor-x 1) trax-cursor-y
		     trax-cursor-x trax-cursor-y
		     trax-cursor-x trax-cursor-y
		     (aref (trax-get-match-vector 
			    trax-cursor-x trax-cursor-y) 2))
   (trax-check-win-1 trax-cursor-x (+ trax-cursor-y 1)
		     trax-cursor-x trax-cursor-y
		     trax-cursor-x trax-cursor-y
		     (aref (trax-get-match-vector 
			    trax-cursor-x trax-cursor-y) 1))
   (trax-check-win-1 trax-cursor-x (- trax-cursor-y 1)
		     trax-cursor-x trax-cursor-y
		     trax-cursor-x trax-cursor-y
		     (aref (trax-get-match-vector 
			    trax-cursor-x trax-cursor-y) 0))))

(defun trax-check-win-1 (x y fromx fromy startx starty player)
  "On tile, check X Y.  Detect win with a loop, or end on opposite
sides of a board of width 8.  If no win, continue in direction away
from fromx and fromy."
;  (let ((trax-cursor-x x) (trax-cursor-y y)) (trax-position-cursor)
;       (message "Checking (%d %d) from (%d %d) start at [%d %d] player %d"
;		x y fromx fromy startx starty player))
;       (read-char))
  ;; First, termination conditions...
  (if (let ((b (tile-lib-get (+ tile-lib-origin-x x) (+ tile-lib-origin-y y))))
	(or (= b 0) (not b)))
      nil
    ;; Second, the winning conditions...
    (if (let ((maxx (- tile-lib-max-x tile-lib-origin-x))
	      (maxy (- tile-lib-max-y tile-lib-origin-y))
	      (mvo (trax-get-match-vector startx starty))
	      (mvn (trax-get-match-vector x y)))
	  (or ;; first, a circle!
	   (and (= x startx) (= y starty))
	   ;; we use 9 because maxx is 2 greater than the # of tiles
	   (and (> maxx 9)
		(or (and (= x 1) (= startx (- maxx 2))
			 (= player (aref mvo 3)) (= player (aref mvn 2)))
		    (and (= x (- maxx 2)) (= startx 1)
			 (= player (aref mvo 2)) (= player (aref mvn 3)))))
	   (and (> maxy 9)
		(or (and (= y 1) (= starty (- maxy 2))
			 (= player (aref mvo 1)) (= player (aref mvn 0)))
		    (and (= y (- maxy 2)) (= starty 1)
			 (= player (aref mvo 0)) (= player (aref mvn 1)))))
	   ))
	t
      ;; And recurse down looking for a possible win!
      (let* ((mv (trax-get-match-vector x y))
	     ;; index of side we came from.  use lisp trick of and/or
	     ;; short circut to accomplish comples if/else tree.
	     (di (or (and (= x fromx) (> y fromy) 0)
		     (and (= x fromx) (< y fromy) 1)
		     (and (= y fromy) (> x fromx) 2)
		     (and (= y fromy) (< x fromx) 3)))
	     ;; Now do the same to determine the direction we are going...
	     (gi (or (and (/= di 0) (= (aref mv 0) player) 0)
		     (and (/= di 1) (= (aref mv 1) player) 1)
		     (and (/= di 2) (= (aref mv 2) player) 2)
		     (and (/= di 3) (= (aref mv 3) player) 3)))
	     ;; Calc the dx/dy to the next square...
	     (dx (or (and (or (= gi 0) (= gi 1)) 0)
		     (and (= gi 2) -1)
		     (and (= gi 3) 1)))
	     (dy (or (and (or (= gi 2) (= gi 3)) 0)
		     (and (= gi 0) -1)
		     (and (= gi 1) 1))))
	;; and return the recursive determination...
	(trax-check-win-1 (+ x dx) (+ y dy) x y startx starty player)))))
  
(defun trax-drop-piece (&optional no-redraw)
  "Take the currently selected piece, and drop it under the cursor,
and beep if illegal, of course."
  (interactive)
  (if (trax-drop-piece-0 no-redraw)
      (game-lib-win tyrant-turn "%P wins!" "Player %d wins!")
    (game-lib-swap-turns "It is now %P's turn." 
			 "Player %d's move.")
    (trax-position-cursor)))
  
(defun trax-drop-piece-0 (&optional no-redraw)
  "Take the currently selected piece, and drop it under the cursor,
and beep if illegal, of course."
  (if (= trax-selected-piece traxt-empty)
      (error "You must select a tile first."))
  (setq trax-selected-piece traxt-empty)
  ;; Now find all new tile we can set to 0
  (let ((top (tile-lib-get (+ trax-cursor-x tile-lib-origin-x   )
			   (+ trax-cursor-y tile-lib-origin-y -1)))
	(bot (tile-lib-get (+ trax-cursor-x tile-lib-origin-x   )
			   (+ trax-cursor-y tile-lib-origin-y  1)))
	(lft (tile-lib-get (+ trax-cursor-x tile-lib-origin-x -1)
			   (+ trax-cursor-y tile-lib-origin-y   )))
	(rit (tile-lib-get (+ trax-cursor-x tile-lib-origin-x  1)
			   (+ trax-cursor-y tile-lib-origin-y   )))
	(redraw nil))
    (or top (trax-put-tile trax-cursor-x (1- trax-cursor-y) traxt-empty))
    (or bot (trax-put-tile trax-cursor-x (1+ trax-cursor-y) traxt-empty))
    (or lft (trax-put-tile (1- trax-cursor-x) trax-cursor-y traxt-empty))
    (or rit (trax-put-tile (1+ trax-cursor-x) trax-cursor-y traxt-empty))
    ;; Handle board growth
    (if (and (not top) (= trax-cursor-y 0))
	(progn
	  (setq redraw t)
	  (setq trax-cursor-y 1)))
    ;; handle board growth to the right.
    (if (and (not lft) (= trax-cursor-x 0))
	(progn
	  (setq redraw t)
	  (setq trax-cursor-x 1)))
    ;; At this time, we should only redraw the whole board if the origin
    ;; changed, as known in the redraw flag, otherwise, just redraw the
    ;; tiles nearby.
    (if redraw
	(tile-lib-draw-board t)
      (or top (tile-lib-draw trax-cursor-x (1- trax-cursor-y)
			     (nth traxt-empty trax-tile-list) t))
      (or bot (tile-lib-draw trax-cursor-x (1+ trax-cursor-y)
			     (nth traxt-empty trax-tile-list) t))
      (or lft (tile-lib-draw (1- trax-cursor-x) trax-cursor-y
			     (nth traxt-empty trax-tile-list) t))
      (or rit (tile-lib-draw (1+ trax-cursor-x) trax-cursor-y
			     (nth traxt-empty trax-tile-list) t))
      (tile-lib-draw trax-cursor-x trax-cursor-y
		     (nth (tile-lib-get (+ trax-cursor-x tile-lib-origin-x)
					(+ trax-cursor-y tile-lib-origin-y))
			  trax-tile-list)))
    (sit-for 0)
    ;; Use magic or to stop forcing pieces when a win is detected.
    (or
    ;; Handle recursive dropping of pieces
     (let* ((trax-cursor-x trax-cursor-x)
	    (trax-cursor-y (1- trax-cursor-y))
	    (trax-selected-piece (trax-fit-list trax-cursor-x trax-cursor-y)))
       (if (= 1 (length trax-selected-piece))
	   (progn
	     (setq trax-selected-piece (car trax-selected-piece))
	     (trax-put-tile trax-cursor-x trax-cursor-y trax-selected-piece)
	     (trax-drop-piece-0 t))))
     (let* ((trax-cursor-x trax-cursor-x)
	    (trax-cursor-y (1+ trax-cursor-y))
	    (trax-selected-piece (trax-fit-list trax-cursor-x trax-cursor-y)))
       (if (= 1 (length trax-selected-piece))
	   (progn
	     (setq trax-selected-piece (car trax-selected-piece))
	     (trax-put-tile trax-cursor-x trax-cursor-y trax-selected-piece)
	     (trax-drop-piece-0 t))))
     (let* ((trax-cursor-x (1- trax-cursor-x))
	    (trax-cursor-y trax-cursor-y)
	    (trax-selected-piece (trax-fit-list trax-cursor-x trax-cursor-y)))
       (if (= 1 (length trax-selected-piece))
	   (progn
	     (setq trax-selected-piece (car trax-selected-piece))
	     (trax-put-tile trax-cursor-x trax-cursor-y trax-selected-piece)
	     (trax-drop-piece-0 t))))
     (let* ((trax-cursor-x (1+ trax-cursor-x))
	    (trax-cursor-y trax-cursor-y)
	    (trax-selected-piece (trax-fit-list trax-cursor-x trax-cursor-y)))
       (if (= 1 (length trax-selected-piece))
	   (progn
	     (setq trax-selected-piece (car trax-selected-piece))
	     (trax-put-tile trax-cursor-x trax-cursor-y trax-selected-piece)
	     (trax-drop-piece-0 t))))
     (progn
       ;; Now go and check for a win from this tile!!!!!!!!!!!
       (if (trax-check-win-0 trax-cursor-x trax-cursor-y)
	   t
	 nil)))))

(defun trax-fit-list (x y)
  "Return a list of all pieces indicies which will fit at the given X Y
relative coordinates.  Do not include empty tile since that one is always
obvious."
  (let ((lst nil)
	(cnt 1))
    (while (< cnt (length trax-side-list))
      (if (trax-fit-p x y cnt)
	  (setq lst (cons cnt lst)))
      (setq cnt (1+ cnt)))
    lst))
 
(defun trax-quit () "Quit trax" (interactive) (game-lib-quit t))

;;; end of lisp
(provide 'trax)