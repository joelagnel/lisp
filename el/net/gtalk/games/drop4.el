;;; Drop 4 game, similar to tic-tac-toe with gravity, and 4 in a row
;;;
;;; Copyright (C) 1992 1993 1994 Free Software Foundation
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
;;; $Id: drop4.el,v 1.1 1994/08/29 23:50:12 zappo Exp $
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

(defvar drop4-map nil
  "Keymap used in playing drop 4")

(if drop4-map
    ()
  (setq drop4-map (make-sparse-keymap))
  (define-key drop4-map "1" 'drop4-select-column)
  (define-key drop4-map "2" 'drop4-select-column)
  (define-key drop4-map "3" 'drop4-select-column)
  (define-key drop4-map "4" 'drop4-select-column)
  (define-key drop4-map "5" 'drop4-select-column)
  (define-key drop4-map "6" 'drop4-select-column)
  (define-key drop4-map "7" 'drop4-select-column)
  (define-key drop4-map "8" 'drop4-select-column)
  (define-key drop4-map "9" 'drop4-select-column)
  (define-key drop4-map "0" 'drop4-select-column)
  (define-key drop4-map "" 'drop4-move-forward)  
  (define-key drop4-map "f" 'drop4-move-forward)
  (define-key drop4-map "." 'drop4-move-forward)
  (define-key drop4-map "" 'drop4-move-back)
  (define-key drop4-map "b" 'drop4-move-back)
  (define-key drop4-map "," 'drop4-move-back)
  (define-key drop4-map "" 'drop4-drop-piece)
  (define-key drop4-map " " 'drop4-drop-piece)
  (define-key drop4-map "q" 'drop4-quit)
  (game-lib-add-mouse-support drop4-map)
)

(defconst drop4-coord2point 
  [ [ 74   81   88   95   102  109  116  123  130  137 ]
    [ 290  297  304  311  318  325  332  339  346  353 ]
    [ 506  513  520  527  534  541  548  555  562  569 ]
    [ 722  729  736  743  750  757  764  771  778  785 ]
    [ 938  945  952  959  966  973  980  987  994  1001 ]
    [ 1154 1161 1168 1175 1182 1189 1196 1203 1210 1217 ]
    [ 1370 1377 1384 1391 1398 1405 1412 1419 1426 1433 ] ]
  "Big array to tell me where to go to print a piece.")

(defconst drop4-linelen 72
  "Length of a board")

(defconst drop4-player1-chars [ " /##\\ " " \\##/ " ] )
(defconst drop4-player2-chars [ " /--\\ " " \\__/ " ] )
(defconst drop4-winning-chars [ " /\\/\\ " " \\/\\/ " ] )

(defvar drop4-moving-piece-pos nil
  "Holds column number of cursor")

(defvar drop4-i-won nil
  "Holds flag for when game has been won.")


(defun drop4 ()
  "Mode to play drop4 in.  To play, move the piece side to side to
select a column, and then drop the piece, hopefully getting 4 in a
row.  Keys are:
  , C-b b  - Move left
  . C-f f  - Move right
  0-9      - Select that column
  RET SPC  - Drop piece."

  (interactive)
  (switch-to-buffer (set-buffer (get-buffer-create "DROP 4")))
  (setq mode-name "Drop4")
  (setq major-mode 'drop4)
  (game-lib-clear-buffer)
  (game-lib-insert-string 1 "|  0      1       2      3      4      5     6      7      8      9   |
|                                                                     |
|                                                                     |
+------+------+------+------+------+------+------+------+------+------+
|      |      |      |      |      |      |      |      |      |      |
|      |      |      |      |      |      |      |      |      |      |
+------+------+------+------+------+------+------+------+------+------+
|      |      |      |      |      |      |      |      |      |      |
|      |      |      |      |      |      |      |      |      |      |
+------+------+------+------+------+------+------+------+------+------+
|      |      |      |      |      |      |      |      |      |      |
|      |      |      |      |      |      |      |      |      |      |
+------+------+------+------+------+------+------+------+------+------+
|      |      |      |      |      |      |      |      |      |      |
|      |      |      |      |      |      |      |      |      |      |
+------+------+------+------+------+------+------+------+------+------+
|      |      |      |      |      |      |      |      |      |      |
|      |      |      |      |      |      |      |      |      |      |
+------+------+------+------+------+------+------+------+------+------+
|      |      |      |      |      |      |      |      |      |      |
|      |      |      |      |      |      |      |      |      |      |
+------+------+------+------+------+------+------+------+------+------+"
			  nil)

  (delete-other-windows (selected-window))
  (use-local-map drop4-map)
  (make-local-variable 'tyrant-turn)
  (setq tyrant-turn 1)
  (setq drop4-moving-piece-pos 1)
  (drop4-place-piece drop4-moving-piece-pos 0 tyrant-turn)
  (make-local-variable 'tyrant-mouse-function)
  (setq tyrant-mouse-function 'drop4-mouse-support)
  (make-local-variable 'etalk-tyrant-brief-help)
  (setq etalk-tyrant-brief-help
	"DROP 4: [, b C-b] Move left [. f C-f] Move right [RET SPC] Drop")
  (setq drop4-i-won nil)
  
  ;; ok.. setup hooks dependant on wether you are player1 or player2
  (setq tyrant-player1-hook 
	'(lambda ()
	   (message "You are player 1")))
  (setq tyrant-player2-hook
	'(lambda ()
	   (setq etalk-tyrant-enabled-console nil)
	   (message "Your are player 2"))) ;player2 goes 2nd
  (setq tyrant-ai-hook '(lambda () (drop4-ai-hook1)))

  (message "<-[b,] SPC drop [f.]-> or a number for a column.")
)

(defun drop4-mouse-support(p e m)
  "Reads in a mouse event from the game-lib driver, and allows a
player to click on a square."

  ;; find which square we are closest to and go there
  (if (and (not (memq 'drag m)) (integerp p))
      (let ((line-width 72)
	    (block-hsep 1)
	    (block-width 6)
	    x xt
	    )
	(setq xt (- (% p line-width) 1))
	(setq x (/ xt (+ block-width block-hsep)))
	
	(if (not (< (% xt (+ block-width block-hsep)) block-hsep))
	    (progn
	      (drop4-place-piece drop4-moving-piece-pos 0 0)
	      (drop4-place-piece x 0 tyrant-turn)
	      (setq drop4-moving-piece-pos x)
	      (if (memq 'click m)
		  (drop4-drop-piece)))))
    )
  )
  
(defun drop4-place-piece (x y player)
  "Place a piece made of char at position x and y"
  
  (let ((str1 nil) (str2 nil) (tchar nil) (c nil))
    (cond
     ((equal player 0) (setq str1 "      ") (setq str2 "      "))
     ((equal player 1) 
      (setq c 'game-lib-player1-face)
      (setq str1 (aref drop4-player1-chars 0))
      (setq str2 (aref drop4-player1-chars 1)))
     ((equal player 2) 
      (setq c 'game-lib-player2-face)
      (setq str1 (aref drop4-player2-chars 0))
      (setq str2 (aref drop4-player2-chars 1)))
     ((equal player 3) 
      ;; winners color
      (if (= tyrant-turn 1)
	  (setq c 'game-lib-player1-face)
	(setq c 'game-lib-player2-face))
      (setq str1 (aref drop4-winning-chars 0))
      (setq str2 (aref drop4-winning-chars 1))))
    
    (setq tchar (aref (aref drop4-coord2point y) x))
    (game-lib-insert-string tchar str1 c)
    (game-lib-insert-string (+ tchar drop4-linelen) str2 c)
    ))

(defun drop4-select-column ()
  "Place the current piece into column *blat*"

  (interactive)
  (let ((col nil))
    (if drop4-i-won (error "The game is over man!"))
    (setq col (- last-input-char ?0))
    (drop4-place-piece drop4-moving-piece-pos 0 0)
    (drop4-place-piece col 0 tyrant-turn)
    (setq drop4-moving-piece-pos col)))

(defun drop4-move-forward ()
  "move the piece to the right"
  
  (interactive)
  (let ((col nil))
    (if drop4-i-won (error "The game is over man!"))
    (if (equal drop4-moving-piece-pos 9)
	(error "Can't go any farther right."))
    (setq col (+ 1 drop4-moving-piece-pos))
    (drop4-place-piece drop4-moving-piece-pos 0 0)
    (drop4-place-piece col 0 tyrant-turn)
    (setq drop4-moving-piece-pos col)))

(defun drop4-move-back ()
  "move the piece to the left"
  
  (interactive)
  (let ((col nil))
    (if drop4-i-won (error "The game is over man!"))
    (if (equal drop4-moving-piece-pos 0)
	(error "Can't go any farther left."))
    (setq col (- drop4-moving-piece-pos 1))
    (drop4-place-piece drop4-moving-piece-pos 0 0)
    (drop4-place-piece col 0 tyrant-turn)
    (setq drop4-moving-piece-pos col)))
  
(defun drop4-drop-piece ()
  "Drop the current piece from column [drop4-moving-piece-pos] to bottom."

  (interactive)
  (let ((row nil))
    (if drop4-i-won (error "The game is over man!"))
    (setq row 0)
    (if (not (equal (drop4-owner drop4-moving-piece-pos 1) 0 ))
	(error "Can't drop a piece there!")
      (while (and (< row 6)
		  (equal (drop4-owner drop4-moving-piece-pos (+ 1 row)) 0))
	(drop4-place-piece drop4-moving-piece-pos row 0)
	(setq row (+ 1 row))
	(drop4-place-piece drop4-moving-piece-pos row tyrant-turn)
	(sit-for 0)))
    (if (drop4-winning-move drop4-moving-piece-pos row)
	(progn
	  (game-lib-win tyrant-turn "%P wins!" "Player %d wins!")
	  (setq drop4-i-won t))
      (drop4-swap-turns)
      (drop4-place-piece drop4-moving-piece-pos 0 tyrant-turn)
      )))

(defun drop4-winning-move (x y)
  "is the position x y allow for a win?"

  (cond
   ;; check verticle win
   ((equal 3 (+ (drop4-mine x (+ y 1))
		(drop4-mine x (+ y 2))
		(drop4-mine x (+ y 3))))
    (drop4-place-piece x y 3)
    (drop4-numline x y 0 1 t)
    t)
   ;; check the horizontal win
   ((<= 3 (+ (drop4-numline x y -1 0)
	     (drop4-numline x y 1 0)))
    (drop4-place-piece x y 3)
    (drop4-numline x y -1 0 t)
    (drop4-numline x y 1 0 t)
    t)
   ;; check upangle
   ((<= 3 (+ (drop4-numline x y -1 1)
	     (drop4-numline x y 1 -1)))
    (drop4-place-piece x y 3)
    (drop4-numline x y -1 1 t)
    (drop4-numline x y 1 -1 t)
    t)
   ;; check downangle
   ((<= 3 (+ (drop4-numline x y -1 -1)
	     (drop4-numline x y 1 1)))
    (drop4-place-piece x y 3)
    (drop4-numline x y -1 -1 t)
    (drop4-numline x y 1 1 t)
    t)
   ;; else
   (t nil)
   ))
	
(defun drop4-numline (x y xchange ychange &optional change) 
  "Return the number of piece between x and y and next non-owned piece
When change is t convert the parts in the piece to display."

  (let ((cx (+ x xchange))
	(cy (+ y ychange))
	(num 0))
    (while (equal 1 (drop4-mine cx cy))
      (setq num (+ 1 num))
      (if change
	  (drop4-place-piece cx cy 3))
      (setq cx (+ cx xchange))
      (setq cy (+ cy ychange)))
    num))
      
(defun drop4-swap-turns ()
  "Swap turns in drop 4"
  (game-lib-swap-turns "It is now %P's turn." 
		       "Player %d's move."))

(defun drop4-mine (x y)
  "do i own pos x y?"

  (cond
   ((< x 0) 0)	 ; all bogus numbers....
   ((< 9 x) 0)
   ((< y 0) 0)
   ((< 6 y) 0)
   ((equal (drop4-owner x y) tyrant-turn) 1)
   (t 0)))

(defun drop4-owner (x y)
  "Who owns pos X Y? 0-nobody"

  (save-excursion
    (goto-char (aref (aref drop4-coord2point y) x))
    (forward-char 3)
    (cond
     ((equal (following-char) ?\ ) 0)
     ((equal (following-char) ?# ) 1)
     ((equal (following-char) ?- ) 2))))

(defun drop4-quit ()
  "quit drop4"
  
  (interactive)
  (setq drop4-i-won t)
  (game-lib-quit t))


;;; end lisp

(provide 'drop4)

