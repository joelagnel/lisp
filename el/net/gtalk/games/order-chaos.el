;;; order and chaos 4 in a row game (emacs talk optional)
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
;;; Please send bug reports, etc. to zappo@gnu.ai.mit.edu.
;;;
;;; $Id: order-chaos.el,v 1.1 1994/08/29 23:51:50 zappo Exp $
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

(defvar order-chaos-map nil
  "Keymap used in playing 4 by 4")

(if order-chaos-map
    ()
  (setq order-chaos-map (make-sparse-keymap))
  (define-key order-chaos-map "" 'order-chaos-move)  
  (define-key order-chaos-map "f" 'order-chaos-move)
  (define-key order-chaos-map "" 'order-chaos-move)
  (define-key order-chaos-map "b" 'order-chaos-move)
  (define-key order-chaos-map "" 'order-chaos-move)
  (define-key order-chaos-map "p" 'order-chaos-move)
  (define-key order-chaos-map "" 'order-chaos-move)
  (define-key order-chaos-map "n" 'order-chaos-move)
  (define-key order-chaos-map "x" 'order-chaos-place-piece)
  (define-key order-chaos-map "o" 'order-chaos-place-piece)
  (define-key order-chaos-map "q" 'order-chaos-quit)
  (game-lib-add-mouse-support order-chaos-map)
)

(defconst order-chaos-piece1 [ " /XX\\ " " \\XX/ "]
  "String as piece 1")

(defconst order-chaos-piece2 [ " /--\\ " " \\__/ " ]
  "String as piece 2")

(defconst order-chaos-piece-win [ " /\\/\\ " " \\/\\/ " ]
  "String as piece 2")

(defvar order-chaos-x nil
  "X position of cursor")

(defvar order-chaos-y nil
  "Y position of cursor")

(defvar order-chaos-moves nil
  "Number of moves left on board.")

(defun order-chaos ()
  "Mode for playing order and chaos.
Rules:  ORDER must get 5 in a row of either X or O pieces.
        CHAOS must prevent order from doing that.
      Either player may place an X or O peice as they wish.
   C-f, C-b, C-p, C-n : movement
   x                  : X piece
   o                  : O piece"

  (interactive)
  (switch-to-buffer (get-buffer-create "Order and ChAoS"))
  (setq mode-name "O&C")
  (setq major-mode 'order-chaos)
  (game-lib-clear-buffer)
  (game-lib-insert-string 1
"+---------------------------------------------------------------------+
|  ----       +------+------+------+------+------+------+     ----\\   |
| /    \\      |      |      |      |      |      |      |    /        |
| \\____/      |      |      |      |      |      |      |    |        |
| ----        +------+------+------+------+------+------+    \\----/   |
| |   \\       |      |      |      |      |      |      |             |
| |---/       |      |      |      |      |      |      |    |    |   |
| |  \\        +------+------+------+------+------+------+    |----|   |
| ----        |      |      |      |      |      |      |    |    |   |
| |   \\       |      |      |      |      |      |      |             |
| |   |       +------+------+------+------+------+------+      /-\\    |
| |___/       |      |      |      |      |      |      |     /   \\   |
|  ____       |      |      |      |      |      |      |     |---|   |
| |           +------+------+------+------+------+------+     |   |   |
| |---        |      |      |      |      |      |      |      ---    |
| |____       |      |      |      |      |      |      |     /   \\   |
| ----        +------+------+------+------+------+------+     \\___/   |
| |   \\       |      |      |      |      |      |      |     .--     |
| |---/       |      |      |      |      |      |      |     |__.    |
| |  \\        +------+------+------+------+------+------+      __|    |
+---------------------------------------------------------------------+" nil)
  (delete-other-windows (selected-window))
  (use-local-map order-chaos-map)
  (make-local-variable 'tyrant-turn)
  (setq tyrant-turn 1)
  (make-local-variable 'order-chaos-x)
  (setq order-chaos-x 0)
  (make-local-variable 'order-chaos-y)
  (setq order-chaos-y 0)
  (make-local-variable 'order-chaos-moves)
  (setq order-chaos-moves 36)
  (make-local-variable 'tyrant-mouse-function)
  (setq tyrant-mouse-function 'order-chaos-mouse-support)
  (make-local-variable 'etalk-tyrant-brief-help)
  (setq etalk-tyrant-brief-help
	"O&C: [C-fbnp] Move [x] Place an X piece [o] place an O piece.")
  (setq tyrant-player1-hook 
	'(lambda ()
	   (message "You are ORDER.")))
  (setq tyrant-player2-hook
	'(lambda ()
	   (setq etalk-tyrant-enabled-console nil)
	   (message "You are CHAOS."))) ;player2 goes 2nd
  )


(defun order-chaos-mouse-support(p e m)
  "Reads in a mouse event from the game-lib driver, and allows a
player to click on a square."

  ;; find which square we are closest to and go there
  (if (and (not (memq 'drag m)) (integerp p))
      (let ((first-line 1)
	    (line-width 72)
	    (block-width 7)
	    (block-height 2)
	    (block-vsep 1)			;vertical separator width
	    (block-hsep 1)			;horizontal sep width
	    x y xt yt
	    )
	(setq yt (/ (- p (* first-line line-width)) line-width))
	(setq xt (% p line-width))
	(if (or (< xt 14) (> xt 57))
	    (message "Mouse %d %d not in board." xt yt)
	  (setq xt (- xt 12))
	  (setq y (/ yt (+ block-height block-vsep)))
	  (setq x (/ xt (+ block-width block-hsep)))
	  
	  (if (not (or (< (% xt (+ block-width block-hsep)) block-hsep)
		       (< (% yt (+ block-height block-vsep)) block-vsep)
		       (> x 5)))
	      (progn
		(setq order-chaos-x x)
		(setq order-chaos-y y)
		(order-chaos-place-cursor)
		(if (memq 'click m)
		    (let ((last-input-char 
			   (if (= (aref (symbol-name e) 
					(- (length (symbol-name e)) 1))
				  ?1) ?x ?o)))
		      (order-chaos-place-piece)))))))
    )
  )

(defun order-chaos-win (move-piece)
  "return t if that is a winning move"
  (cond
   ((>= (+ 1 (order-chaos-check-dir 0 1 move-piece)
	   (order-chaos-check-dir 0 -1 move-piece)) 5)
    (order-chaos-check-dir 0 1 move-piece t) 
    (order-chaos-check-dir 0 -1 move-piece t)
    (order-chaos-put order-chaos-x order-chaos-y 3) t)
   ((>= (+ 1 (order-chaos-check-dir 1 0 move-piece) 
	   (order-chaos-check-dir -1 0 move-piece)) 5)
    (order-chaos-check-dir 1 0 move-piece t) 
    (order-chaos-check-dir -1 0 move-piece t)
    (order-chaos-put order-chaos-x order-chaos-y 3) t)
   ((>= (+ 1 (order-chaos-check-dir 1 1 move-piece) 
	   (order-chaos-check-dir -1 -1 move-piece)) 5)
    (order-chaos-check-dir 1 1 move-piece t) 
    (order-chaos-check-dir -1 -1 move-piece t)
    (order-chaos-put order-chaos-x order-chaos-y 3) t)
   ((>= (+ 1 (order-chaos-check-dir -1 1 move-piece) 
	   (order-chaos-check-dir 1 -1 move-piece)) 5)
    (order-chaos-check-dir -1 1 move-piece t) 
    (order-chaos-check-dir 1 -1 move-piece t)
    (order-chaos-put order-chaos-x order-chaos-y 3) t)
   (t nil)))

(defun order-chaos-check-dir (dx dy &optional count place)
  "if count: returns t if supported by a side
else returns # of peices of player counts in that direction"
  (let ((x (+ order-chaos-x dx)) (y (+ order-chaos-y dy)) (number 0))
    (while (and (and (and (>= x 0) (<= x 5))  ;; positionally ok
		     (and (>= y 0) (<= y 5)))
		(or  (and (not count) (order-chaos-owned x y))
		     (and count (eq (order-chaos-owned x y) count))))
      (setq number (+ number 1))
      (if place (order-chaos-put x y 3))
      (setq x (+ x dx))
      (setq y (+ y dy)))
    (if count
	number
      (or (= x -1) (= x 8) (= y -1) (= y 8)))))

(defun order-chaos-owned (x y)
  "returns nil if empty, 1 if player 1 peice, and 2 if player 2 peice"
  (save-excursion
    (goto-char (+ (order-chaos-xy2index x y) 2))
    (cond
     ((= (following-char) ?\ )
      nil)
     ((= (following-char) ?X)
      1)
     ((= (following-char) ?-)
      2)
     (t nil))))

(defun order-chaos-move ()
  "Move the cursor to a new position"
  (interactive)
  (cond
   ((or (= last-input-char ?\C-f)
	(= last-input-char ?f))
    (if (< order-chaos-x 5)
	(setq order-chaos-x (+ order-chaos-x 1))
      (error "Can't go farther right!")))
   ((or (= last-input-char ?\C-b)
	(= last-input-char ?b))
    (if (> order-chaos-x 0)
	(setq order-chaos-x (- order-chaos-x 1))
      (error "Can't go farther left!")))
   ((or (= last-input-char ?\C-p)
	(= last-input-char ?p))
    (if (> order-chaos-y 0)
	(setq order-chaos-y (- order-chaos-y 1))
      (error "Can't go farther up!")))
   ((or (= last-input-char ?\C-n)
	(= last-input-char ?n))
    (if (< order-chaos-y 5)
	(setq order-chaos-y (+ order-chaos-y 1))
      (error "Can't go farther down!"))))
  (order-chaos-place-cursor))

(defun order-chaos-place-cursor ()
  "Place the cursor on the correct spot on the board..."

  (goto-char (+ 1 (order-chaos-xy2index order-chaos-x order-chaos-y))))

(defun order-chaos-place-piece ()
  "Actuall place the piece on the board"

  (interactive)
  (if (= order-chaos-moves 0)
      (error "Game is over man!"))
  (if (= last-input-char ?x)
      (order-chaos-put order-chaos-x order-chaos-y 1)
    (order-chaos-put order-chaos-x order-chaos-y 2))
  (sit-for 0)
  (if (order-chaos-win (if (= last-input-char ?x) 1 2))
      (progn
	(if (and (boundp 'etalk-tyrannical-mode) etalk-tyrannical-mode)
	    (etalk-usurp-tyrant "ORDER WINS!")
	  (message "ORDER WINS!")))
    (setq order-chaos-moves (1- order-chaos-moves))
    (if (= order-chaos-moves 0)
	(progn
	  (if (and (boundp 'etalk-tyrannical-mode) etalk-tyrannical-mode)
	      (etalk-usurp-tyrant "CHAOS WINS!")
	    (message "CHAOS WINS!")))
      (order-chaos-swap-turns))))

(defun order-chaos-put (x y player)
  "Place a piece made of char at position x and y"
  
  (let ((str1 nil) (str2 nil) (co nil))
    (if (and (order-chaos-owned x y) 
	     (not (= player 3)))
	(error "Piece already there!"))
    (cond
     ((equal player 0) (setq str1 "      ") (setq str2 "      "))
     ((equal player 1) 
      (setq co 'game-lib-player1-face)
      (setq str1 (aref order-chaos-piece1 0))
      (setq str2 (aref order-chaos-piece1 1)))
     ((equal player 2) 
      (setq co 'game-lib-player2-face)
      (setq str1 (aref order-chaos-piece2 0))
      (setq str2 (aref order-chaos-piece2 1)))
     ((equal player 3) 
      (if (= tyrant-turn 1)
	  (setq co 'game-lib-player1-face)
	(setq co 'game-lib-player2-face))
      (setq str1 (aref order-chaos-piece-win 0))
      (setq str2 (aref order-chaos-piece-win 1))))
    
    (save-excursion
      (game-lib-insert-string (order-chaos-xy2index x y) str1 co)
      (game-lib-insert-string (+ (order-chaos-xy2index x y) 72) str2 co)
      )))

(defun order-chaos-xy2index (x y)
  "change x y position to absolute buffer address"

  (+ (+ (+ 15 (* x 7)) (+ (* y 216) 1)) 144))

(defun order-chaos-swap-turns ()
  "Swap turns in order and chaos"

  (let ((s (format "Player %s's move." (if (= tyrant-turn 1) "ORDER" "CHAOS"))))
    (game-lib-swap-turns s s)))

(defun order-chaos-quit ()
  "quit order and chaos"
  
  (interactive)
  (game-lib-quit t))

;;; end of lisp

(provide 'order-chaos)

