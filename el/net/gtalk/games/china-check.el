;;; chineese checkers interface (emacs talk optional)
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
;;; $Id: china-check.el,v 1.1 1994/08/29 23:49:18 zappo Exp $
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

(defvar china-check-map nil
  "Keymap used in playing chineese checkers")

(if china-check-map
    ()
  (setq china-check-map (make-sparse-keymap))
  (define-key china-check-map "" 'china-check-move)
  (define-key china-check-map "f" 'china-check-move)
  (define-key china-check-map "" 'china-check-move)
  (define-key china-check-map "b" 'china-check-move)
  (define-key china-check-map "" 'china-check-move)
  (define-key china-check-map "p" 'china-check-move)
  (define-key china-check-map "" 'china-check-move)
  (define-key china-check-map "n" 'china-check-move)
  (define-key china-check-map "y" 'china-check-move)
  (define-key china-check-map "u" 'china-check-move)
  (define-key china-check-map "g" 'china-check-move)
  (define-key china-check-map "j" 'china-check-move)
  (define-key china-check-map "b" 'china-check-move)
  (define-key china-check-map "n" 'china-check-move)
  (define-key china-check-map " " 'china-check-grab-piece)
  (define-key china-check-map "h" 'china-check-grab-piece)
  (define-key china-check-map "q" 'china-check-quit)
)

(defconst china-check-start-1
  '( (13 . 0) (12 . 1) (14 . 1) (11 . 2) (13 . 2) (15 . 2)
     (10 . 3) (12 . 3) (14 . 3) (16 . 3) (9  . 4) (11 . 4) 
     (13 . 4) (15 . 4) (17 . 4) )
  "List of start point in china check for player 1")

(defconst china-check-start-2
  '( (9  . 14) (11 . 14) (13 . 14) (15 . 14) (17 . 14) (10 . 15)
     (12 . 15) (14 . 15) (16 . 15) (11 . 16) (13 . 16) (15 . 16)
     (12 . 17) (14 . 17) (13 . 18) )
  "List of start positions for player 2")

(defconst china-check-piece1 "O"
  "String as piece 1")

(defconst china-check-piece2 "X"
  "String as piece 2")

(defvar china-check-x nil
  "X position of cursor")

(defvar china-check-y nil
  "Y position of cursor")

(defvar china-check-1x nil
  "Secondary X for player 1")

(defvar china-check-1y nil
  "Secondary Y for player 1")

(defvar china-check-2x nil
  "Secondary X for player 2")

(defvar china-check-2y nil
  "Secondary Y for player 2")

(defvar china-check-p1-score nil
  "Player 1s score")

(defvar china-check-p2-score nil
  "Player 2s score")

(defvar china-grab nil
  "Flag for when a peice has been grabbed.")

(defun china-check ()
  "Mode for playing chinese checkers"
  (interactive)
  (switch-to-buffer (get-buffer-create "Chinese Checkers"))
  (setq mode-name "C-C")
  (setq major-mode 'china-check)
  (game-lib-clear-buffer)
  (game-lib-insert-string 1
"+------- Chinese Checkers! -------+
|              / \\                |
| Move Score  / . \\ Directionals  |
|            / . . \\     Y U      |
|           / . . . \\   G H J     |
|          / . . . . \\   B N      |
|_________/ . . . . . \\___________|
|\\ . . . . . . . . . . . . . . . /|
| \\ . . . . . . . . . . . . . . / |
|  \\ . . . . . . . . . . . . . /  |
|   \\ . . . . . . . . . . . . /   |
|    > . . . . . . . . . . . <    |
|   / . . . . . . . . . . . . \\   |
|  / . . . . . . . . . . . . . \\  |
| / . . . . . . . . . . . . . . \\ |
|/ . . . . . . . . . . . . . . . \\|
|---------\\ . . . . . /-----------|
|          \\ . . . . /            |
|           \\ . . . / Move Score  |
|            \\ . . /              |
|             \\ . /               |
|              \\ /                |
+---------------------------------+
" nil)
  (delete-other-windows (selected-window))
  (use-local-map china-check-map)
  (make-local-variable 'tyrant-turn)
  (setq tyrant-turn 2)
  (let ((l china-check-start-2))
    (while l
      (china-check-put (car (car l)) (cdr (car l)) nil nil)
      (setq l (cdr l))))
  (setq tyrant-turn 1)
  (let ((l china-check-start-1))
    (while l
      (china-check-put (car (car l)) (cdr (car l)) nil nil)
      (setq l (cdr l))))
  (make-local-variable 'china-check-x)
  (setq china-check-x 13)
  (make-local-variable 'china-check-y)
  (setq china-check-y 0)
  (make-local-variable 'china-check-1x)
  (setq china-check-1x 13)
  (make-local-variable 'china-check-1y)
  (setq china-check-1y 0)
  (make-local-variable 'china-check-2x)
  (setq china-check-2x 13)
  (make-local-variable 'china-check-2y)
  (setq china-check-2y 18)
  (make-local-variable 'china-check-p1-score)
  (setq china-check-p1-score 0)
  (make-local-variable 'china-check-p2-score)
  (setq china-check-p2-score 0)
  (make-local-variable 'china-grab)
  (setq china-grab nil)
  (make-local-variable 'etalk-tyrant-brief-help)
  (setq etalk-tyrant-brief-help
"ChinaCheck: Directional move [SPC] Grab, Directionals move piece [H] done.")
  (china-check-place-cursor)
  (setq tyrant-player1-hook
	'(lambda ()
	   (message "You are player 1")))
  (setq tyrant-player2-hook
	'(lambda ()
	   (setq etalk-tyrant-enabled-console nil)
	   (message "Your are player 2"))) ;player2 goes 2nd
 )

(defun china-check-grab-piece ()
  "Mark the peice as nabbed for future reference"
  (interactive)
  (if china-grab
      (if (listp china-grab)
	  (progn
	    (china-check-put (nth 0 china-grab) (nth 1 china-grab) t t)
	    (china-check-put (nth 0 china-grab) (nth 1 china-grab))
	    (setq china-grab nil))
	(china-check-put china-check-x china-check-y t t)
	(china-check-put china-check-x china-check-y)
	(setq china-grab nil)
	(china-check-swap-turns))
    (if (not (= (china-check-owned china-check-x china-check-y) tyrant-turn))
	(error "You don't have a piece in that location!")
      (setq china-grab (list china-check-x china-check-y))
      (china-check-put china-check-x china-check-y t))))
      
(defun china-check-move ()
  "Move the cursor to a new position"
  (interactive)
  (let ((dx 0) (dy 0))
    (cond
     ((or (= last-input-char ?\C-f)
	  (= last-input-char ?f)
	  (= last-input-char ?j))
      (if (china-check-owned (+ china-check-x 2) china-check-y)
	  (progn (setq dx 2) (setq dy 0))
	(error "Can't go farther right!")))
     ((or (= last-input-char ?\C-b)
	  (= last-input-char ?g))
      (if (china-check-owned (- china-check-x 2) china-check-y)
	  (progn (setq dx -2) (setq dy 0))
	(error "Can't go farther left!")))
     ((or (= last-input-char ?\C-p)
	  (= last-input-char ?p)
	  (= last-input-char ?y))
      (if (china-check-owned (1- china-check-x) (1- china-check-y))
	  (progn (setq dy -1) (setq dx -1))
	(error "Can't go farther up!")))
     ((= last-input-char ?u)
      (if (china-check-owned (1+ china-check-x) (1- china-check-y))
	  (progn (setq dy -1) (setq dx +1))
	(error "Cant go farther up!")))
     ((or (= last-input-char ?\C-n)
	  (= last-input-char ?n))
      (if (china-check-owned (1+ china-check-x) (1+ china-check-y))
	  (progn (setq dy +1) (setq dx +1))
	(error "Can't go farther down!")))
     ((= last-input-char ?b)
      (if (china-check-owned (1- china-check-x) (1+ china-check-y))
	  (progn (setq dy +1) (setq dx -1))
	(error "Can't go farther down!"))))
    (if (not china-grab)
	(progn
	  (setq china-check-x (+ china-check-x dx))
	  (setq china-check-y (+ china-check-y dy)))
      (if (= (china-check-owned (+ china-check-x dx) (+ china-check-y dy)) 0)
	  (if (not (listp china-grab))
	      (error "You may only jump now...")
	    (china-check-put china-check-x china-check-y t t)
	    (setq china-check-x (+ china-check-x dx))
	    (setq china-check-y (+ china-check-y dy))
	    (china-check-put china-check-x china-check-y)
	    (setq china-grab nil)
	    (if (= tyrant-turn 1)
		(setq china-check-p1-score (+ china-check-p1-score dy))
	      (setq china-check-p2-score (- china-check-p2-score dy)))
	    (china-check-swap-turns))
	(if (eq (china-check-owned (+ china-check-x dx dx) (+ china-check-y dy dy)) 0)
	    (progn
	      (china-check-put china-check-x china-check-y t t)
	      (setq china-check-x (+ china-check-x dx dx))
	      (setq china-check-y (+ china-check-y dy dy))
	      (setq china-grab t)
	      (china-check-put china-check-x china-check-y t)
	      (if (= tyrant-turn 1)
		  (setq china-check-p1-score (+ china-check-p1-score (* dy 2)))
		(setq china-check-p2-score (- china-check-p2-score (* dy 2)))))
	  (error "Can't move your stone that way!")))))
  (china-check-place-cursor))

(defun china-check-owned (x y)
  "returns nil if empty, 1 if player 1 peice, and 2 if player 2 peice"
  (save-excursion
    (goto-char (+ (china-check-xy2index x y) 1))
    (cond
     ((= (following-char) ?.)
      0)
     ((= (following-char) (string-to-char china-check-piece1))
      1)
     ((= (following-char) (string-to-char china-check-piece2))
      2)
     (t nil))))

(defun china-check-place-cursor ()
  "Place the cursor on the correct spot on the board..."

  (message "You are on %d %d" china-check-x china-check-y)
  (goto-char (+ 1 (china-check-xy2index china-check-x china-check-y))))

(defun china-check-put (x y &optional marked off)
  "Based on variable \"turn\" place peice there."

  (game-lib-insert-string (china-check-xy2index x y)
			  (if (equal off t) " . "
			    (if (= tyrant-turn 1)
				(if marked
				    (concat ">" china-check-piece1 "<")
				  (concat " " china-check-piece1 " "))
			      (if marked 
				  (concat ">" china-check-piece2 "<")
				(concat " " china-check-piece2 " "))))
			  (if (equal off t)
			      nil
			    (if (= tyrant-turn 1)
				'game-lib-player1-face
			      'game-lib-player2-face))))

(defun china-check-xy2index (x y)
  "change x y position to absolute buffer address"

  (+ (+ (+ 6 x) (+ (* y 36) 1)) 68))

(defun china-check-win-string ()
  "generates a string declairing who won."
  (china-check-swap-turns)
  (if (and (boundp 'etalk-tyrannical-mode) etalk-tyrannical-mode)  
      (tyrant-format (if (> china-check-p1-score china-check-p2-score)
			 "%n wins!!!!"
		       "%N wins!!!"))
    (message "Player %s wins!" (if (> china-check-p1-score china-check-p2-score) 1 2))))

(defun china-check-swap-turns ()
  "Swap turns in chineese checkers"

  (cond
   ((= tyrant-turn 1) 
    (setq tyrant-turn 2)
    (setq china-check-1x china-check-x) (setq china-check-1y china-check-y)
    (setq china-check-x china-check-2x) (setq china-check-y china-check-2y))
   ((= tyrant-turn 2) 
    (setq tyrant-turn 1)
    (setq china-check-2x china-check-x) (setq china-check-2y china-check-y)
    (setq china-check-x china-check-1x) (setq china-check-y china-check-1y)))
  (if (or (= china-check-p1-score 194) (= china-check-p2-score 194))
      (progn
	(if (and (boundp 'etalk-tyrannical-mode) etalk-tyrannical-mode)
	    (etalk-usurp-tyrant (china-check-win-string))
	  (message (china-check-win-string)))))
  (goto-char 113)
  (delete-char 4)
  (insert (format "%4d" china-check-p1-score))
  (goto-char 709)
  (delete-char 4)
  (insert (format "%4d" china-check-p2-score))
  ;; tyrant mode support:
  ;; flip turns accross net each time.
  (if (boundp 'etalk-tyrant-enabled-console)
      (progn
	(message (tyrant-format "It is now %P's turn."))
	(setq etalk-tyrant-enabled-console (not etalk-tyrant-enabled-console)))
    (message "Player %d's move." tyrant-turn))
  (china-check-place-cursor))

(defun china-check-quit ()
  "quit china-check"
  
  (interactive)
  (game-lib-quit t))




;;; end of lisp
(provide 'china-check)

