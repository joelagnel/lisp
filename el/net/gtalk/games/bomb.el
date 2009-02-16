;;; bombs.el - bombs game 
;;;
;;; Copyright (C) 1995, 1999 Eric M. Ludlam
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
;;; $Id: bomb.el,v 1.4 1999/08/26 11:33:08 zappo Exp $
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

(defvar bomb-map nil
  "Keymap used in playing bombs")

(if bomb-map
    ()
  (setq bomb-map (make-sparse-keymap))
  (define-key bomb-map "\C-f" 'bomb-move)  
  (define-key bomb-map "f" 'bomb-move)
  (define-key bomb-map "\C-b" 'bomb-move)
  (define-key bomb-map "b" 'bomb-move)
  (define-key bomb-map "\C-n" 'bomb-move)
  (define-key bomb-map "n" 'bomb-move)
  (define-key bomb-map "\C-p" 'bomb-move)
  (define-key bomb-map "p" 'bomb-move)
  (define-key bomb-map "" 'bomb-place-piece)
  (define-key bomb-map " " 'bomb-place-piece)
  (define-key bomb-map "q" 'bomb-quit)
  (game-lib-add-mouse-support bomb-map)
)

(defvar bomb-player-1 "@" "Player 1's bomb piece representation")
(defvar bomb-player-2 "*" "Player 2's bomb piece representation")

(defvar bomb-x 0 "Current X position of bomb placement cursor.")
(defvar bomb-y 0 "Current X position of bomb placement cursor.")

(defvar bombvector nil "Current state of the board in a 2D vector.")
(defvar bombmax [ [ 2 3 3 3 3 2 ]
		  [ 3 4 4 4 4 3 ]
		  [ 3 4 4 4 4 3 ]
		  [ 3 4 4 4 4 3 ]
		  [ 2 3 3 3 3 2 ] ]
  "Represents the max # of bombs which fit on any given square.")

(defvar bomb-explode-sound nil
  "*Shell command which goes BOOM! during bomb game.")

(defvar bomb-check nil 
  "Variable set to t after first move.  Signifies check of opponant
annihilation")

(defvar bomb-vector nil
  "Variable used to hold bomb data for the board.")

(defvar bomb-recurse-forever nil
  "Set to t to avoid win check during explosions.")

(defun bomb ()
  "Bomb mode for etalk.  To play, place a bomb of yours into an empty
square, or into a square you already own.  When a square is filled up,
the bombs explode.  The square is full when the number of bombs in the
square is equal to the number of neighbor squares.  Corner squares can
hold 2 bombs, edge squares 3, and center squares can hold 4 bombs.

  When the bombs explode, one bomb is moved into each of the adjacent
squares.  If your opponand owns the adjacent square, you become owner
if one of your bombs explodes into it.  To win, destroy all of your
neighbors bombs.

For instance, the corner situation for @s move int the upper left
(where the ! is) would work like this:

             ------->
+-----+-----+--    +-----+-----+--
|  !  |     |  	   |     |     |  
| @   | *   |  	   |     | @ @ |  
+-----+-----+--	   +-----+-----+--
|     | *   |  	   |     | *   |  
| @   | * * |  	   | @ @ | * * |  
+-----+-----+--	   +-----+-----+--
|     |     |  	   |     |     |  
| @   |     |      | @   |     |

Now, it's * turn.  When this happens, we will see a chain reaction,
during which, * will win.

              ------->           ------>           ----->
 +-----+-----+--    +-----+-----+--   +-----+-----+--  +-----+-----+--
 |     |     |      |     | *   |     |     |     |    |     |     |  
 |     | @ @ |      |     | * * |     | * * |     | *  |     | *   | *
 +-----+-----+--    +-----+-----+--   +-----+-----+--  +-----+-----+--
 |     | *!  |      | *   |     |     |     |     |    |     |     |  
 | @ @ | * * |      | * * |     | *   |     | * * | *  | *   | * * | *
 +-----+-----+--    +-----+-----+--   +-----+-----+--  +-----+-----+--
 |     |     |      |     |     |     |     |     |    |     |     |  
 | @   |     |      | @   | *   |     | * * | *   |    | * * | *   |  
"
  (interactive)
  (switch-to-buffer (set-buffer (get-buffer-create "*Bombs*")))
  (setq mode-name "bomb")
  (setq major-mode 'bomb)
  (game-lib-clear-buffer)
  (game-lib-insert-string 1 "+-----+-----+-----+-----+-----+-----+
|     |     |     |     |     |     |
|     |     |     |     |     |     |
+-----+-----+-----+-----+-----+-----+
|     |     |     |     |     |     |
|     |     |     |     |     |     |
+-----+-----+-----+-----+-----+-----+
|     |     |     |     |     |     |
|     |     |     |     |     |     |
+-----+-----+-----+-----+-----+-----+
|     |     |     |     |     |     |
|     |     |     |     |     |     |
+-----+-----+-----+-----+-----+-----+
|     |     |     |     |     |     |
|     |     |     |     |     |     |
+-----+-----+-----+-----+-----+-----+" nil)
  (delete-other-windows (selected-window))
  (use-local-map bomb-map) 
  (setq tyrant-turn 1
	bomb-x 0
	bomb-y 0
	bomb-check nil)

  (make-local-variable 'tyrant-mouse-function)
  (setq tyrant-mouse-function 'bomb-mouse-support)

  (setq bomb-vector (make-vector 5 nil))
  (aset bomb-vector 0 (make-vector 6 nil))
  (aset bomb-vector 1 (make-vector 6 nil))
  (aset bomb-vector 2 (make-vector 6 nil))
  (aset bomb-vector 3 (make-vector 6 nil))
  (aset bomb-vector 4 (make-vector 6 nil))

  ;; ok.. setup hooks dependant on wether you are player1 or player2
  (setq tyrant-player1-hook 
	'(lambda ()
	   (message "You are player 1")))
  (setq tyrant-player2-hook
	'(lambda ()
	   (setq etalk-tyrant-enabled-console nil)
	   (message "Your are player 2"))) ;player2 goes 2nd

  (bomb-place-cursor)
)

(defun bomb-mouse-support(p e m)
  "Reads in a mouse event from the game-lib driver, and allows a
player to click on a square."

  ;; find which square we are closest to and go there
  (if (and (not (memq 'drag m)) (integerp p))
      (let ((first-line 0)
	    (line-width 38)
	    (block-width 5)
	    (block-height 2)
	    (block-vsep 1)			;vertical separator width
	    (block-hsep 1)			;horizontal sep width
	    x y xt yt
	    )
	(setq yt (/ (- p (* first-line line-width)) line-width))
	(setq xt (- (% p line-width) 1))
	(setq y (/ yt (+ block-height block-vsep)))
	(setq x (/ xt (+ block-width block-hsep)))
	
	(if (not (or (< (% xt (+ block-width block-hsep)) block-hsep)
		     (< (% yt (+ block-height block-vsep))
			block-vsep)))
	    (progn
	      (setq bomb-x x)
	      (setq bomb-y y)
	      (goto-char (+ (bomb-xy2index x y) 1))
	      (if (memq 'click m)
		  (bomb-place-piece)))))
    )
  )

(defun bomb-xy2index (x y)
  "change x y position to absolute buffer address"

  (+ (+ (+ 1 (* x 6)) (+ (* y 114) 1)) 39))

(defun bomb-place-cursor ()
  "Place the cursor on the correct spot on the board..."
  (goto-char (+ 1 (bomb-xy2index bomb-x bomb-y))))

(defun bomb-move ()
  "Move the cursor to a new position"
  (interactive)
  (cond
   ((or (= last-input-char ?\C-f)
	(= last-input-char ?f))
    (if (< bomb-x 5)
	(setq bomb-x (+ bomb-x 1))
      (error "Can't go farther right!")))
   ((or (= last-input-char ?\C-b)
	(= last-input-char ?b))
    (if (> bomb-x 0)
	(setq bomb-x (- bomb-x 1))
      (error "Can't go farther left!")))
   ((or (= last-input-char ?\C-p)
	(= last-input-char ?p))
    (if (> bomb-y 0)
	(setq bomb-y (- bomb-y 1))
      (error "Can't go farther up!")))
   ((or (= last-input-char ?\C-n)
	(= last-input-char ?n))
    (if (< bomb-y 4)
	(setq bomb-y (+ bomb-y 1))
      (error "Can't go farther down!"))))
  (bomb-place-cursor))

(defun bomb-owner (x y)
  "Return number of owner of square at x y"
  (let* ((curset (aref (aref bomb-vector y) x)))
    (if curset
	(car curset)
      nil)))

(defun bomb-value (x y)
  "Return number of owner of square at x y"
  (let* ((curset (aref (aref bomb-vector y) x)))
    (if curset
	(cdr curset)
      0)))

(defun bomb-p (x y)
  "Return t if the position x y should go boom!"
  (let* ((curset (aref (aref bomb-vector y) x))
	 (curmax (aref (aref bombmax y) x)))
    (and curset (>= (cdr curset) curmax))))

(defun bomb-addbomb (x y owner)
  "Add 1 bomb the the number of bombs at position X Y, and set to
OWNER.  Return a value if it is legal to add something there"
  (if (and (< x 6) (< y 5) (>= x 0) (>= y 0))
      (let* ((curset (aref (aref bomb-vector y) x))
	     (curnum (if curset (cdr curset) 0)))
	(aset (aref bomb-vector y) x (cons owner (1+ curnum)))
	(bomb-redraw x y))
    nil))

(defun bomb-explode (maybe)
  "Recursivly explode all bombs in this LIST, where each elt in LIST
is a dot pair ( x . y )"
  (let ((newlist nil)
	(tl nil))
    (while maybe
      (let ((tx  (car (car maybe)))
	    (ty  (cdr (car maybe))))
	(if (bomb-p tx ty)
	    (progn
	      (aset (aref bomb-vector ty) tx nil) ;reset this sqare!
	      (bomb-redraw tx ty)
	      (setq tl (cons (car maybe) tl)) ;find all exploded squares
	      (if (bomb-addbomb (1- tx) ty tyrant-turn)
		  (setq newlist (cons (cons (1- tx) ty) newlist)))
	      (if (bomb-addbomb (1+ tx) ty tyrant-turn)
		  (setq newlist (cons (cons (1+ tx) ty) newlist)))
	      (if (bomb-addbomb tx (1- ty) tyrant-turn)
		  (setq newlist (cons (cons tx (1- ty)) newlist)))
	      (if (bomb-addbomb tx (1+ ty) tyrant-turn)
		  (setq newlist (cons (cons tx (1+ ty)) newlist)))))
	(setq maybe (cdr maybe)))
      )
    (while tl
      (let ((tx  (car (car tl)))
	    (ty  (cdr (car tl))))
	(aset (aref bomb-vector ty) tx nil) ;reset this sqare again
					;just in case
	(bomb-redraw tx ty))
      (setq tl (cdr tl)))
	
    (if (or bomb-recurse-forever (not (bomb-check-boom)))
	(if newlist (bomb-explode newlist)))))

(defun bomb-place-piece ()
  "Place one bomb into the selected square.  If there are too many
bombs, then BOOM!"
  (interactive)

  (if (and (bomb-owner bomb-x bomb-y)
	   (/= tyrant-turn (bomb-owner bomb-x bomb-y)))
      (error "You may not add a bomb there!"))

  (bomb-addbomb bomb-x bomb-y tyrant-turn)
  ;; run explode routine to clean up any problems..
  (bomb-explode (list (cons bomb-x bomb-y)))
  (bomb-swap-turns))

(defun bomb-redraw (x y)
  "Redraw the bomb square at X and Y"
  (let* ((curset (aref (aref bomb-vector y) x))
	 (curnum (if curset (cdr curset) 0))
	 (curchar (if (= tyrant-turn 1) bomb-player-1 bomb-player-2))
	 (curface (if (= tyrant-turn 1)
		      (if (bomb-p x y) 
			  'game-lib-player1-face-R
			'game-lib-player1-face)
		    (if (bomb-p x y)
			'game-lib-player2-face-R
		      'game-lib-player2-face)))
	 (line1 (cond ((< curnum 3) "   ")
		      ((= curnum 3) (concat curchar "  "))
		      (t (concat curchar " " curchar))))
	 (line2 (cond ((= curnum 0) "   ")
		      ((= curnum 1) (concat curchar "  "))
		      (t (concat curchar " " curchar)))))
    ;; Plop these onto the grid...
    (game-lib-insert-string (bomb-xy2index x y) line1 curface)
    (game-lib-insert-string (+ (bomb-xy2index x y) 38) line2 curface))
  (sit-for 0))

(defun bomb-check-boom (&optional win)
  "Run through the entire vector, and see how many squares everyone
has."
  (let ((x 5) (p1 0) (p2 0))
    (while (>= x 0)
      (let ((y 4))
	(while (>= y 0)
	  (let ((bo (bomb-owner x y)))
	    (if bo
		(if (= bo 1)
		    (setq p1 (1+ p1))
		  (setq p2 (1+ p2)))))
	  (setq y (1- y))))
      (setq x (1- x)))
    (if (= p2 0) 
	(progn
	  (if win (game-lib-win p1 "%u has destroyed %U!" 
				"Player 1 has destroyed Player 2!"))
	  1)
      (if (= p1 0)
	  (progn
	    (if win (game-lib-win p2 "%U has destroyed %u!"
				  "Player 2 has destroyed Player 1!"))
	    2)
	nil))))

(defun bomb-swap-turns ()
  "Swap turns in the bomb game"
  (if bomb-check
      (if (not (bomb-check-boom t))
	  (game-lib-swap-turns "It is now %P's turn." 
			       "Player %d's move."))
    (setq bomb-check t)			;set at beginning of player
					;2's turn.
    (game-lib-swap-turns "It is now %P's turn." 
			 "Player %d's move.")))

(defun bomb-quit ()
  "quit bomb game"
  (interactive)
  (game-lib-quit t))

;;; end lisp

(provide 'bomb)


