;;; torversi game interface (emacs talk optional)
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
;;; $Id: torversi.el,v 1.3 1995/11/17 23:05:53 zappo Exp $
;;; History:
;;; zappo@choochoo 11/17/95
;;; Added features used to create -ai for reversi.
;;;
;;; zappo@choochoo 10/19/94
;;; Added reversi-pass to binding "P" which I forgot to do when I
;;; first wrote the thing. ;(
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
;;;  Under tyrant-ai.  To run, load the library "tyrn-ai" and then use   ;;;
;;;  the function "tyrant-play-computer" and choose this game.  An AI    ;;;
;;;  function must be installed on the system for this to work.          ;;;
;;;                                                                      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'game-lib "games/game-lib")

(defvar torversi-map nil
  "Keymap used in playing torus reversi")

(if torversi-map
    ()
  (setq torversi-map (make-sparse-keymap))
  (define-key torversi-map "" 'torversi-move)  
  (define-key torversi-map "f" 'torversi-move)
  (define-key torversi-map "" 'torversi-move)
  (define-key torversi-map "b" 'torversi-move)
  (define-key torversi-map "" 'torversi-move)
  (define-key torversi-map "p" 'torversi-move)
  (define-key torversi-map "" 'torversi-move)
  (define-key torversi-map "n" 'torversi-move)
  (define-key torversi-map "g" 'torversi-pass)
  (define-key torversi-map " " 'torversi-place-piece)
  (define-key torversi-map "q" 'torversi-quit)
  (define-key torversi-map "P" 'toreversi-pass)
  (game-lib-add-mouse-support torversi-map)
)

(defconst torversi-piece1 "##"
  "String as piece 1")

(defconst torversi-piece2 "::"
  "String as piece 2")

(defconst torversi-buff "TORUS REVERSI")

(defconst torversi-stat "Torus Reversi Stats")

(defvar torversi-x nil
  "X position of cursor")

(defvar torversi-y nil
  "Y position of cursor")

(defvar torversi-p1-score nil
  "Player 1 score")

(defvar torversi-p2-score nil
  "Player 2 score")

(defvar torversi-delta nil
  "delta when calculating how many pieces just stolen")

(defun torversi ()
  "Mode for playing torus reversi
Rules:  You may only move to an empty square in which an oponants peice is 
        between current square and another one of your peices.
        Taking this square takes the peices of the oponant between your
        pieces.  Winner has the most peices.  Because the board is a
        torus, you can move over an edge legally.
Playing:
        C-fbnp moves point
        P      pass
        SPC    takes square"

  (interactive)
  (switch-to-buffer (get-buffer-create torversi-buff))
  (setq mode-name "Torus Reversi")
  (setq major-mode 'torversi)
  (game-lib-clear-buffer)
  (game-lib-insert-string 1
			  "+-/\\-+-/\\-+-/\\-+-/\\-+-/\\-+-/\\-+-/\\-+-/\\-+
<    |    |    |    |    |    |    |    >
+----+----+----+----+----+----+----+----+
<    |    |    |    |    |    |    |    >
+----+----+----+----+----+----+----+----+
<    |    |    |    |    |    |    |    >
+----+----+----+----+----+----+----+----+
<    |    |    | ## | :: |    |    |    >
+----+----+----+----+----+----+----+----+
<    |    |    | :: | ## |    |    |    >
+----+----+----+----+----+----+----+----+
<    |    |    |    |    |    |    |    >
+----+----+----+----+----+----+----+----+
<    |    |    |    |    |    |    |    >
+----+----+----+----+----+----+----+----+
<    |    |    |    |    |    |    |    >
+-\\/-+-\\/-+-\\/-+-\\/-+-\\/-+-\\/-+-\\/-+-\\/-+" nil)
  (delete-other-windows (selected-window))
  (use-local-map torversi-map)
  (make-local-variable 'tyrant-turn)
  (setq tyrant-turn 2)
  (torversi-put 4 3) (torversi-put 3 4)
  (setq tyrant-turn 1)
  (torversi-put 3 3) (torversi-put 4 4)
  (setq torversi-x 0)
  (setq torversi-y 0)
  (setq torversi-p1-score 2)
  (setq torversi-p2-score 2)
  (make-local-variable 'tyrant-mouse-function)
  (setq tyrant-mouse-function 'torversi-mouse-support)
  (make-local-variable 'etalk-tyrant-brief-help)
  (setq etalk-tyrant-brief-help
"TORVERSI: [C-f] Forward [C-b] Back [C-p] Previous [C-n] Next [g] pass [SPC] GO")
  (make-local-variable 'etalk-tyrant-quit-string)
  (fset 'etalk-tyrant-quit-string 'torversi-win-string)
  (split-window (selected-window) 50 t)
  (other-window 1)
  (switch-to-buffer (get-buffer-create torversi-stat))
  (game-lib-clear-buffer)
  (insert "Player 1 score:
  2

Player 2 score:
  2

Last move:
  - -

Delta:
  - -


")
  (other-window 1)
  (setq tyrant-player1-hook 
	'(lambda ()
	   (message "You are player 1")
	   (save-window-excursion
	     (switch-to-buffer torversi-stat)
	     (make-local-variable 'etalk-tag)
	     (setq etalk-tag t)
	     (goto-line 1)
	     (delete-region (point) (save-excursion (end-of-line) (point)))
	     (insert (tyrant-format "%n score:"))
	     (goto-line 4)
	     (delete-region (point) (save-excursion (end-of-line) (point)))
	     (insert (tyrant-format "%N score:")))))
  (setq tyrant-player2-hook
	'(lambda ()
	   (setq etalk-tyrant-enabled-console nil)
	   (message "Your are player 2")
	   (save-window-excursion
	     (switch-to-buffer torversi-stat)
	     (make-local-variable 'etalk-tag)
	     (setq etalk-tag t)
	     (goto-line 1)
	     (delete-region (point) (save-excursion (end-of-line) (point)))
	     (insert (tyrant-format "%n score:"))
	     (goto-line 4)
	     (delete-region (point) (save-excursion (end-of-line) (point)))
	     (insert (tyrant-format "%N score:")))))
  )

(defun torversi-mouse-support(p e m)
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
	      (setq torversi-x x)
	      (setq torversi-y y)
	      (goto-char (+ (torversi-xy2index x y) 1))
	      (if (memq 'click m)
		  (torversi-place-piece)))))
    )
  )

(defun torversi-place-piece ()
  "Put a piece onto the board."

  (interactive)
  (if (not (= (torversi-owned torversi-x torversi-y) 0))
      (error "Player %s is already there!" (torversi-owned torversi-x torversi-y))
    (if (torversi-legal)
	(progn
	  (setq torversi-delta 0)
	  (torversi-put torversi-x torversi-y)
	  (if (= tyrant-turn 1)
	      (setq torversi-p1-score (1+ torversi-p1-score))
	    (setq torversi-p2-score (1+ torversi-p2-score)))
	  (if (torversi-check-dir 0  1) (torversi-check-dir 0  1 t))
	  (if (torversi-check-dir 0 -1) (torversi-check-dir 0 -1 t))
	  (if (torversi-check-dir 1  0) (torversi-check-dir 1  0 t))
	  (if (torversi-check-dir -1 0) (torversi-check-dir -1 0 t))
	  (if (torversi-check-dir 1  1) (torversi-check-dir 1  1 t))
	  (if (torversi-check-dir -1 -1) (torversi-check-dir -1 -1 t))
	  (if (torversi-check-dir 1 -1) (torversi-check-dir 1  -1 t))
	  (if (torversi-check-dir -1 1) (torversi-check-dir -1 1 t))
	  (if (torversi-win)
	      (if (and (boundp 'etalk-tyrannical-mode) etalk-tyrannical-mode)
		  (etalk-usurp-tyrant (torversi-win-string))
		(message (torversi-win-string)))
	    (torversi-swap-turns)))
      (error "Not a legal move."))))

(defun torversi-win-string ()
  "generates a string declairing who won."
  (torversi-swap-turns)
  (if (and (boundp 'etalk-tyrannical-mode) etalk-tyrannical-mode)  
      (tyrant-format (if (> torversi-p1-score torversi-p2-score)
			 "%n wins!!!!"
		       "%N wins!!!"))
    (message "Player %s wins!" (if (> torversi-p1-score torversi-p2-score) 1 2))))

(defun torversi-win ()
  "return t if that is a winning move"
  (or (= (+ torversi-p1-score torversi-p2-score) 64)
      (= torversi-p1-score 0)
      (= torversi-p2-score 0))
  )

(defun torversi-legal ()
  "return if the location is a legal move."
  (let ((x torversi-x) (y torversi-y))
    (or (torversi-check-dir 0  1) (torversi-check-dir 0 -1)
	(torversi-check-dir 1  0) (torversi-check-dir -1 0)
	(torversi-check-dir 1  1) (torversi-check-dir -1 -1)
	(torversi-check-dir 1  -1) (torversi-check-dir -1 1))))

(defun torversi-check-dir (dx dy &optional place ret-zero)
  "returns t if it is a legal direction.  Legal being when the
neighboring pieces are the opponant, and the last peice is yours.  If
place then snarf them for your own.  If RET-ZERO then return 0 instead
of nil when there are no pieces to take"

  (let ((x (+ torversi-x dx)) (y (+ torversi-y dy)) (number 0) (flag nil))
    (if (< x 0) (setq x 7))
    (if (> x 7) (setq x 0))
    (if (< y 0) (setq y 7))
    (if (> y 7) (setq y 0))
    (while (and (or (/= x torversi-x) (/= y torversi-y))
		(or  (= (if (= tyrant-turn 1) 2 1)
			(torversi-owned x y)))) ;not me first
      (if place
	  (progn
	    (torversi-put x y)
	    (setq torversi-delta (1+ torversi-delta))
	    (if (= tyrant-turn 1)
		(progn
		  (setq torversi-p1-score (1+ torversi-p1-score))
		  (setq torversi-p2-score (1- torversi-p2-score)))
	      (setq torversi-p2-score (1+ torversi-p2-score))
	      (setq torversi-p1-score (1- torversi-p1-score)))))
      (setq number (1+ number))
      (setq x (+ x dx))
      (if (< x 0) (setq x 7))
      (if (> x 7) (setq x 0))
      (setq y (+ y dy))
      (if (< y 0) (setq y 7))
      (if (> y 7) (setq y 0)))
    (if (and (< 0 number) (= tyrant-turn (torversi-owned x y)))
	number
      (if ret-zero 0 nil))))

(defun torversi-owned (x y)
  "returns nil if empty, 1 if player 1 peice, and 2 if player 2 peice"
  (if (or (< x 0) (> x 7) (< y 0) (> y 7))
      0
    (save-excursion
      (goto-char (+ (torversi-xy2index x y) 1))
      (cond
       ((= (following-char) ?\ )
	0)
       ((= (following-char) (string-to-char torversi-piece1))
	1)
       ((= (following-char) (string-to-char torversi-piece2))
	2)
       (t 0)))))

(defun torversi-move ()
  "Move the cursor to a new position"
  (interactive)
  (cond
   ((or (= last-input-char ?\C-f)
	(= last-input-char ?f))
    (if (< torversi-x 7)
	(setq torversi-x (+ torversi-x 1))
      (setq torversi-x 0)))
   ((or (= last-input-char ?\C-b)
	(= last-input-char ?b))
    (if (> torversi-x 0)
	(setq torversi-x (- torversi-x 1))
      (setq torversi-x 7)))
   ((or (= last-input-char ?\C-p)
	(= last-input-char ?p))
    (if (> torversi-y 0)
	(setq torversi-y (- torversi-y 1))
      (setq torversi-y 7)))
   ((or (= last-input-char ?\C-n)
	(= last-input-char ?n))
    (if (< torversi-y 7)
	(setq torversi-y (+ torversi-y 1))
      (setq torversi-y 0)))) 
  (torversi-place-cursor))

(defun torversi-place-cursor ()
  "Place the cursor on the correct spot on the board..."

  (goto-char (+ 1 (torversi-xy2index torversi-x torversi-y))))

(defun torversi-put (x y &optional off)
  "Based on variable \"turn\" place peice there."

  (game-lib-insert-string (+ 1 (torversi-xy2index x y))
			  (if (equal off t)
			      "  "
			    (if (= tyrant-turn 1)
				torversi-piece1
			      torversi-piece2))
			  (if (= tyrant-turn 1)
			      'game-lib-player1-face
			    'game-lib-player2-face)))
  
(defun torversi-xy2index (x y)
  "change x y position to absolute buffer address"

  (+ (+ (+ 1 (* x 5)) (+ (* y 84) 1)) 42))

(defun torversi-pass ()
  "Pass the buck"
  (interactive)
  (setq torversi-delta 0)
  (torversi-swap-turns))

(defun torversi-swap-turns ()
  "Swap turns in torus reversi"

  (save-excursion
    (let ((s1 (format "  %d blocks" torversi-p1-score))
	  (s2 (format "  %d blocks" torversi-p2-score))
	  (s3 (if (and (boundp 'etalk-tyrannical-mode) etalk-tyrannical-mode)
		  (tyrant-format " %:1P moved to %d, %d" torversi-x torversi-y)
		(format " Player %d moved to %d, %d" tyrant-turn torversi-x torversi-y)))
	  (s4 (if (and (boundp 'etalk-tyrannical-mode) etalk-tyrannical-mode)
		  (tyrant-format " %:1P steals %d blocks" torversi-delta)
		(format " Player %d steals %d blocks" tyrant-turn 
			torversi-delta))))
      (set-buffer torversi-stat)
      (goto-line 2)
      (delete-region (point) (save-excursion (end-of-line) (point)))
      (insert s1)
      (goto-line 5)
      (delete-region (point) (save-excursion (end-of-line) (point)))
      (insert s2)
      (goto-line 8)
      (delete-region (point) (save-excursion (end-of-line) (point)))
      (insert s3)
      (goto-line 11)
      (delete-region (point) (save-excursion (end-of-line) (point)))
      (insert s4)))
  (game-lib-swap-turns "It is now %P's turn." "Player %d's move."))

(defun torversi-quit ()
  "quit torversi"
  
  (interactive)
  (delete-window (get-buffer-window torversi-stat))
  (kill-buffer torversi-stat)
  (game-lib-quit t))

;;; end of lisp
(provide 'torversi)
