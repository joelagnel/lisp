;;; reversi game interface
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
;;; $Id: reversi.el,v 1.5 1995/11/17 23:05:29 zappo Exp $
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

(defvar reversi-map nil
  "Keymap used in playing reversi")

(if reversi-map
    ()
  (setq reversi-map (make-sparse-keymap))
  (define-key reversi-map "" 'reversi-move)  
  (define-key reversi-map "f" 'reversi-move)
  (define-key reversi-map "" 'reversi-move)
  (define-key reversi-map "b" 'reversi-move)
  (define-key reversi-map "" 'reversi-move)
  (define-key reversi-map "p" 'reversi-move)
  (define-key reversi-map "" 'reversi-move)
  (define-key reversi-map "n" 'reversi-move)
  (define-key reversi-map "g" 'reversi-pass)
  (define-key reversi-map " " 'reversi-place-piece)
  (define-key reversi-map "q" 'reversi-quit)
  (define-key reversi-map "P" 'reversi-pass)
  (game-lib-add-mouse-support reversi-map)
)

(defconst reversi-piece1 "##"
  "String as piece 1")

(defconst reversi-piece2 "::"
  "String as piece 2")

(defconst reversi-buff "REVERSI")

(defconst reversi-stat "Reversi Stats")

(defvar reversi-x nil
  "X position of cursor")

(defvar reversi-y nil
  "Y position of cursor")

(defvar reversi-p1-score nil
  "Player 1 score")

(defvar reversi-p2-score nil
  "Player 2 score")

(defvar reversi-delta nil
  "delta when calculating how many pieces just stolen")

(defun reversi ()
  "Mode for playing reversi
Rules:  You may only move to an empty square in which an oponants peice is 
        between current square and another one of your peices.
        Taking this square takes the peices of the oponant between your
        pieces.  Winner has the most peices.
Playing:
        C-fbnp moves point
        P      pass
        SPC    takes square"

  (interactive)
  (switch-to-buffer (get-buffer-create reversi-buff))
  (setq mode-name "Reversi")
  (setq major-mode 'reversi)
  (game-lib-clear-buffer)
  (game-lib-insert-string 1
			  "+----+----+----+----+----+----+----+----+
|    |    |    |    |    |    |    |    |
+----+----+----+----+----+----+----+----+
|    |    |    |    |    |    |    |    |
+----+----+----+----+----+----+----+----+
|    |    |    |    |    |    |    |    |
+----+----+----+----+----+----+----+----+
|    |    |    | ## | :: |    |    |    |
+----+----+----+----+----+----+----+----+
|    |    |    | :: | ## |    |    |    |
+----+----+----+----+----+----+----+----+
|    |    |    |    |    |    |    |    |
+----+----+----+----+----+----+----+----+
|    |    |    |    |    |    |    |    |
+----+----+----+----+----+----+----+----+
|    |    |    |    |    |    |    |    |
+----+----+----+----+----+----+----+----+" nil)
  (delete-other-windows (selected-window))
  (use-local-map reversi-map)
  (make-local-variable 'tyrant-turn)
  (setq tyrant-turn 2)
  (reversi-put 4 3) (reversi-put 3 4)
  (setq tyrant-turn 1)
  (reversi-put 3 3) (reversi-put 4 4)
  (setq reversi-x 0)
  (setq reversi-y 0)
  (setq reversi-p1-score 2)
  (setq reversi-p2-score 2)
  (make-local-variable 'tyrant-mouse-function)
  (setq tyrant-mouse-function 'reversi-mouse-support)
  (make-local-variable 'etalk-tyrant-brief-help)
  (setq etalk-tyrant-brief-help
"REVERSI: [C-f] Forward [C-b] Back [C-p] Previous [C-n] Next [g] pass [SPC] GO")
  (make-local-variable 'etalk-tyrant-quit-string)
  (fset 'etalk-tyrant-quit-string 'reversi-win-string)
  (split-window (selected-window) 50 t)
  (other-window 1)
  (switch-to-buffer (get-buffer-create reversi-stat))
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
	     (switch-to-buffer reversi-stat)
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
	     (switch-to-buffer reversi-stat)
	     (make-local-variable 'etalk-tag)
	     (setq etalk-tag t)
	     (goto-line 1)
	     (delete-region (point) (save-excursion (end-of-line) (point)))
	     (insert (tyrant-format "%n score:"))
	     (goto-line 4)
	     (delete-region (point) (save-excursion (end-of-line) (point)))
	     (insert (tyrant-format "%N score:")))))
  )

(defun reversi-mouse-support(p e m)
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
	      (setq reversi-x x)
	      (setq reversi-y y)
	      (goto-char (+ (reversi-xy2index x y) 1))
	      (if (memq 'click m)
		  (reversi-place-piece)))))
    )
  )

(defun reversi-place-piece ()
  "Put a piece onto the board."

  (interactive)
  (if (not (= (reversi-owned reversi-x reversi-y) 0))
      (error "Player %s is already there!" (reversi-owned reversi-x reversi-y))
    (if (reversi-legal)
	(progn
	  (setq reversi-delta 0)
	  (reversi-put reversi-x reversi-y)
	  (if (= tyrant-turn 1)
	      (setq reversi-p1-score (1+ reversi-p1-score))
	    (setq reversi-p2-score (1+ reversi-p2-score)))
	  (if (reversi-check-dir 0  1) (reversi-check-dir 0  1 t))
	  (if (reversi-check-dir 0 -1) (reversi-check-dir 0 -1 t))
	  (if (reversi-check-dir 1  0) (reversi-check-dir 1  0 t))
	  (if (reversi-check-dir -1 0) (reversi-check-dir -1 0 t))
	  (if (reversi-check-dir 1  1) (reversi-check-dir 1  1 t))
	  (if (reversi-check-dir -1 -1) (reversi-check-dir -1 -1 t))
	  (if (reversi-check-dir 1 -1) (reversi-check-dir 1  -1 t))
	  (if (reversi-check-dir -1 1) (reversi-check-dir -1 1 t))
	  (if (reversi-win)
	      (if (and (boundp 'etalk-tyrannical-mode) etalk-tyrannical-mode)
		  (etalk-usurp-tyrant (reversi-win-string))
		(message (reversi-win-string)))
	    (reversi-swap-turns)))
      (error "Not a legal move."))))

(defun reversi-win-string ()
  "generates a string declairing who won."
  (reversi-swap-turns)
  (if (and (boundp 'etalk-tyrannical-mode) etalk-tyrannical-mode)  
      (tyrant-format (if (> reversi-p1-score reversi-p2-score)
			 "%n wins!!!!"
		       "%N wins!!!"))
    (message "Player %s wins!" (if (> reversi-p1-score reversi-p2-score) 1 2))))

(defun reversi-win ()
  "return t if that is a winning move"
  (or (= (+ reversi-p1-score reversi-p2-score) 64)
      (= reversi-p1-score 0)
      (= reversi-p2-score 0))
  )

(defun reversi-legal ()
  "return if the location is a legal move."
  (let ((x reversi-x) (y reversi-y))
    (or (reversi-check-dir 0  1) (reversi-check-dir 0 -1)
	(reversi-check-dir 1  0) (reversi-check-dir -1 0)
	(reversi-check-dir 1  1) (reversi-check-dir -1 -1)
	(reversi-check-dir 1  -1) (reversi-check-dir -1 1))))

(defun reversi-check-dir (dx dy &optional place ret-zero)
  "returns # pieces to capture if it is a legal direction.  Legal
being when the neighboring pieces are the opponant, and the last peice
is yours.  If place then snarf them for your own.  If RET-ZERO is t,
return 0 instead of nil when apropriate."

  (let ((x (+ reversi-x dx)) (y (+ reversi-y dy)) (number 0) (flag nil))
    (while (and (and (and (>= x 0) (<= x 7))  ;; positionally ok
		     (and (>= y 0) (<= y 7)))
		(or  (= (if (= tyrant-turn 1) 2 1)
			(reversi-owned x y)))) ;not me first
      (if place 
	  (progn
	    (reversi-put x y)
	    (setq reversi-delta (1+ reversi-delta))
	    (if (= tyrant-turn 1)
		(progn
		  (setq reversi-p1-score (1+ reversi-p1-score))
		  (setq reversi-p2-score (1- reversi-p2-score)))
	      (setq reversi-p2-score (1+ reversi-p2-score))
	      (setq reversi-p1-score (1- reversi-p1-score)))))
      (setq number (1+ number))
      (setq x (+ x dx))
      (setq y (+ y dy)))
    (if (and (< 0 number) (= tyrant-turn (reversi-owned x y)))
	number
      (if ret-zero 0 nil))))

(defun reversi-owned (x y)
  "returns 0 if empty, 1 if player 1 peice, and 2 if player 2 peice"
  (if (or (< x 0) (> x 7) (< y 0) (> y 7))
      0
    (save-excursion
      (goto-char (+ (reversi-xy2index x y) 1))
      (cond
       ((= (following-char) ?\ )
	0)
       ((= (following-char) (string-to-char reversi-piece1))
	1)
       ((= (following-char) (string-to-char reversi-piece2))
	2)
       (t 0)))))

(defun reversi-move ()
  "Move the cursor to a new position"
  (interactive)
  (cond
   ((or (= last-input-char ?\C-f)
	(= last-input-char ?f))
    (if (< reversi-x 7)
	(setq reversi-x (+ reversi-x 1))
      (error "Can't go farther right!")))
   ((or (= last-input-char ?\C-b)
	(= last-input-char ?b))
    (if (> reversi-x 0)
	(setq reversi-x (- reversi-x 1))
      (error "Can't go farther left!")))
   ((or (= last-input-char ?\C-p)
	(= last-input-char ?p))
    (if (> reversi-y 0)
	(setq reversi-y (- reversi-y 1))
      (error "Can't go farther up!")))
   ((or (= last-input-char ?\C-n)
	(= last-input-char ?n))
    (if (< reversi-y 7)
	(setq reversi-y (+ reversi-y 1))
      (error "Can't go farther down!"))))
  (reversi-place-cursor))

(defun reversi-place-cursor ()
  "Place the cursor on the correct spot on the board..."

  (goto-char (+ 1 (reversi-xy2index reversi-x reversi-y))))

(defun reversi-put (x y &optional off)
  "Based on variable \"turn\" place peice there."

  (game-lib-insert-string (+ 1 (reversi-xy2index x y))
			  (if (equal off t)
			      "  "
			    (if (= tyrant-turn 1)
				reversi-piece1
			      reversi-piece2))
			  (if (= tyrant-turn 1)
			      'game-lib-player1-face
			    'game-lib-player2-face)))
  
(defun reversi-xy2index (x y)
  "change x y position to absolute buffer address"

  (+ (+ (+ 1 (* x 5)) (+ (* y 84) 1)) 42))

(defun reversi-pass ()
  "Pass the buck"
  (interactive)
  (setq reversi-delta 0)
  (message "I pass!") (sit-for 1)
  (reversi-swap-turns))

(defun reversi-swap-turns ()
  "Swap turns in reversi"

  (save-excursion
    (let ((s1 (format "  %d blocks" reversi-p1-score))
	  (s2 (format "  %d blocks" reversi-p2-score))
	  (s3 (if (and (boundp 'etalk-tyrannical-mode) etalk-tyrannical-mode)
		  (tyrant-format " %:1P moved to %d, %d" reversi-x reversi-y)
		(format " Player %d moved to %d, %d" tyrant-turn reversi-x reversi-y)))
	  (s4 (if (and (boundp 'etalk-tyrannical-mode) etalk-tyrannical-mode)
		  (tyrant-format " %:1P steals %d blocks" reversi-delta)
		(format " Player %d steals %d blocks" tyrant-turn 
			reversi-delta))))
      (set-buffer reversi-stat)
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

(defun reversi-quit ()
  "quit reversi"
  
  (interactive)
  (delete-window (get-buffer-window reversi-stat))
  (kill-buffer reversi-stat)
  (game-lib-quit t))

;;; end of lisp
(provide 'reversi)
