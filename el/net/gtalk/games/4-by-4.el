;;; 4x4 game, similar to drop4 in 4 extra dimensions (emacs talk optional)
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
;;; $Id: 4-by-4.el,v 1.1 1994/08/29 23:46:52 zappo Exp $
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

(defvar 4-by-4-map nil
  "Keymap used in playing 4 by 4")

(if 4-by-4-map
    ()
  (setq 4-by-4-map (make-sparse-keymap))
  (define-key 4-by-4-map "" '4-by-4-move)  
  (define-key 4-by-4-map "f" '4-by-4-move)
  (define-key 4-by-4-map "" '4-by-4-move)
  (define-key 4-by-4-map "b" '4-by-4-move)
  (define-key 4-by-4-map "" '4-by-4-move)
  (define-key 4-by-4-map "p" '4-by-4-move)
  (define-key 4-by-4-map "" '4-by-4-move)
  (define-key 4-by-4-map "n" '4-by-4-move)
  (define-key 4-by-4-map " " '4-by-4-place-piece)
  (define-key 4-by-4-map "q" '4-by-4-quit)
  (game-lib-add-mouse-support 4-by-4-map)
)

(defconst 4-by-4-piece1 "##"
  "String as piece 1")

(defconst 4-by-4-piece2 "::"
  "String as piece 2")

(defconst 4-by-4-piece-win "><"
  "String as piece 2")

(defvar 4-by-4-x 0
  "Current X position in 4x4 board")

(defvar 4-by-4-y 0
  "Current Y position in 4x4 board")

(defun 4-by-4 ()
  "Mode for playing a game an awful lot like connect 4 with 4
directional gravity.  4-by-4 is played by placing pieces on the board
to get 4 in a row.  Legal moves are any that can attach via a straight
line to any border."

  (interactive)
  (switch-to-buffer (get-buffer-create "4 BY 4"))
  (setq mode-name "4X4")
  (setq major-mode '4-by-4)
  (game-lib-clear-buffer)
  (insert "+----+----+----+----+----+----+----+----+
|    |    |    |    |    |    |    |    |
+----+----+----+----+----+----+----+----+
|    |    |    |    |    |    |    |    |
+----+----+----+----+----+----+----+----+
|    |    |    |    |    |    |    |    |
+----+----+----+----+----+----+----+----+
|    |    |    |    |    |    |    |    |
+----+----+----+----+----+----+----+----+
|    |    |    |    |    |    |    |    |
+----+----+----+----+----+----+----+----+
|    |    |    |    |    |    |    |    |
+----+----+----+----+----+----+----+----+
|    |    |    |    |    |    |    |    |
+----+----+----+----+----+----+----+----+
|    |    |    |    |    |    |    |    |
+----+----+----+----+----+----+----+----+")
  (delete-other-windows (selected-window))
  (use-local-map 4-by-4-map)
  (make-local-variable 'tyrant-turn)
  (setq tyrant-turn 1)
  (setq 4-by-4-x 0)
  (setq 4-by-4-y 0)
  (make-local-variable 'tyrant-mouse-function)
  (setq tyrant-mouse-function '4-by-4-mouse-support)
  (make-local-variable 'etalk-tyrant-brief-help)
  (setq etalk-tyrant-brief-help
"4 BY 4: [C-f,f] Forward [C-b,b] Back [C-p,p] Previous [C-n,n] Next [SPC] GO")
  (setq tyrant-player1-hook 
	'(lambda ()
	   (message "You are player 1")))
  (setq tyrant-player2-hook
	'(lambda ()
	   (setq etalk-tyrant-enabled-console nil)
	   (message "Your are player 2"))) ;player2 goes 2nd
  )

(defun 4-by-4-mouse-support(p e m)
  "Reads in a mouse event from the game-lib driver, and allows a
player to click on a square."

  ;; find which square we are closest to and go there
  (if (and (not (memq 'drag m)) (integerp p))
      (let ((first-line 0)
	    (line-width 42)
	    (block-width 4)
	    (block-height 1)
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
	      (setq 4-by-4-x x)
	      (setq 4-by-4-y y)
	      (goto-char (+ (4-by-4-xy2index x y) 1))
	      (if (memq 'click m)
		  (4-by-4-place-piece)))))
    )
  )

(defun 4-by-4-place-piece ()
  "Put a piece onto the board."

  (interactive)
  (if (4-by-4-owned 4-by-4-x 4-by-4-y)
      (error "Player %s is already there!" (4-by-4-owned 4-by-4-x 4-by-4-y))
    (if (4-by-4-legal)
	(progn
	  (4-by-4-put 4-by-4-x 4-by-4-y)
	  (if (4-by-4-win)
	      (game-lib-win tyrant-turn "%P wins!!!!" "Player %d wins!")
	    (4-by-4-swap-turns)))
      (error "That location is not correctly supported!!!"))))

(defun 4-by-4-win ()
  "return t if that is a winning move"
  (cond
   ((>= (+ 1 (4-by-4-check-dir 0 1 tyrant-turn) (4-by-4-check-dir 0 -1 tyrant-turn)) 4)
    (4-by-4-check-dir 0 1 tyrant-turn t) (4-by-4-check-dir 0 -1 tyrant-turn t)
    (4-by-4-put 4-by-4-x 4-by-4-y 1)
    t)
   ((>= (+ 1 (4-by-4-check-dir 1 0 tyrant-turn) (4-by-4-check-dir -1 0 tyrant-turn)) 4)
    (4-by-4-check-dir 1 0 tyrant-turn t) (4-by-4-check-dir -1 0 tyrant-turn t)
    (4-by-4-put 4-by-4-x 4-by-4-y 1)
    t)
   ((>= (+ 1 (4-by-4-check-dir 1 1 tyrant-turn) (4-by-4-check-dir -1 -1 tyrant-turn)) 4)
    (4-by-4-check-dir 1 1 tyrant-turn t) (4-by-4-check-dir -1 -1 tyrant-turn t)
    (4-by-4-put 4-by-4-x 4-by-4-y 1)
    t)
   ((>= (+ 1 (4-by-4-check-dir -1 1 tyrant-turn) (4-by-4-check-dir 1 -1 tyrant-turn)) 4)
    (4-by-4-check-dir -1 1 tyrant-turn t) (4-by-4-check-dir 1 -1 tyrant-turn t)
    (4-by-4-put 4-by-4-x 4-by-4-y 1)
    t)
   (t nil)))

(defun 4-by-4-legal ()
  "return if the location is a legal move."
  (let ((x 4-by-4-x) (y 4-by-4-y))
    (if (or (= x 0) (= x 7) (= y 0) (= y 7))
	t
      (or (4-by-4-check-dir 0  1) (4-by-4-check-dir 0 -1)
	  (4-by-4-check-dir 1  0) (4-by-4-check-dir -1 0)))))

(defun 4-by-4-check-dir (dx dy &optional count place)
  "if count: returns t if supported by a side
else returns # of peices of player counts in that direction"
  (let ((x (+ 4-by-4-x dx)) (y (+ 4-by-4-y dy)) (number 0))
    (while (and (and (and (>= x 0) (<= x 7))  ;; positionally ok
		     (and (>= y 0) (<= y 7)))
		(or  (and (not count) (4-by-4-owned x y))
		     (and count (eq (4-by-4-owned x y) count))))
      (setq number (+ number 1))
      (if place (4-by-4-put x y 1))
      (setq x (+ x dx))
      (setq y (+ y dy)))
    (if count
	number
      (or (= x -1) (= x 8) (= y -1) (= y 8)))))

(defun 4-by-4-owned (x y)
  "returns nil if empty, 1 if player 1 peice, and 2 if player 2 peice"
  (save-excursion
    (goto-char (+ (4-by-4-xy2index x y) 1))
    (cond
     ((= (following-char) ?\ )
      nil)
     ((= (following-char) (string-to-char 4-by-4-piece1))
      1)
     ((= (following-char) (string-to-char 4-by-4-piece2))
      2)
     (t nil))))

(defun 4-by-4-move ()
  "Move the cursor to a new position"
  (interactive)
  (cond
   ((or (= last-input-char ?\C-f)
	(= last-input-char ?f))
    (if (< 4-by-4-x 7)
	(setq 4-by-4-x (+ 4-by-4-x 1))
      (error "Can't go farther right!")))
   ((or (= last-input-char ?\C-b)
	(= last-input-char ?b))
    (if (> 4-by-4-x 0)
	(setq 4-by-4-x (- 4-by-4-x 1))
      (error "Can't go farther left!")))
   ((or (= last-input-char ?\C-p)
	(= last-input-char ?p))
    (if (> 4-by-4-y 0)
	(setq 4-by-4-y (- 4-by-4-y 1))
      (error "Can't go farther up!")))
   ((or (= last-input-char ?\C-n)
	(= last-input-char ?n))
    (if (< 4-by-4-y 7)
	(setq 4-by-4-y (+ 4-by-4-y 1))
      (error "Can't go farther down!"))))
  (4-by-4-place-cursor))

(defun 4-by-4-place-cursor ()
  "Place the cursor on the correct spot on the board..."

  (goto-char (+ 1 (4-by-4-xy2index 4-by-4-x 4-by-4-y))))

(defun 4-by-4-put (x y &optional off)
  "Based on variable \"turn\" place peice there."

  (game-lib-insert-string (+ 1 (4-by-4-xy2index x y))
			  (if (equal off t) "  "
			    (if (equal off 1)
				4-by-4-piece-win
			      (if (= tyrant-turn 1)
				  4-by-4-piece1
				4-by-4-piece2)))
			  (if (= tyrant-turn 1)
			      'game-lib-player1-face-R
			    'game-lib-player2-face-R)))

(defun 4-by-4-xy2index (x y)
  "change x y position to absolute buffer address"

  (+ (+ (+ 1 (* x 5)) (+ (* y 84) 1)) 42))

(defun 4-by-4-swap-turns ()
  "Swap turns in connect 4"
  (game-lib-swap-turns "It is now %P's turn."
		       "Player %d's turn."))


(defun 4-by-4-quit ()
  "quit 4 by 4"
  
  (interactive)
  (game-lib-quit t))

;;; end of lisp

(provide '4-by-4)
