;;; checkers game interface (emacs talk optional)
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
;;; $Id: checkers.el,v 1.3 1995/09/21 02:08:19 zappo Exp $
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

(defvar checkers-map nil
  "Keymap used in playing checkers")

(if checkers-map
    ()
  (setq checkers-map (make-sparse-keymap))
  (define-key checkers-map "" 'checkers-move)  
  (define-key checkers-map "f" 'checkers-move)
  (define-key checkers-map "" 'checkers-move)
  (define-key checkers-map "b" 'checkers-move)
  (define-key checkers-map "" 'checkers-move)
  (define-key checkers-map "p" 'checkers-move)
  (define-key checkers-map "" 'checkers-move)
  (define-key checkers-map "n" 'checkers-move)
  (define-key checkers-map " " 'checkers-mark)
  (define-key checkers-map "q" 'checkers-quit)
  (game-lib-add-mouse-support checkers-map)
)

(defvar checkers-stat "Checkers Stat"
  "Buffer name for checkers statistics buffer.")

(defconst checkers-piece1 "# "
  "String as piece 1")

(defconst checkers-king1 "##"
  "string as king 1")

(defconst checkers-piece2 ": "
  "String as piece 2")

(defconst checkers-king2 "::"
  "String as king 2")

(defvar checkers-x nil
  "Defines the X position of cursor")

(defvar checkers-y nil
  "Defines the Y position of cursor")

(defvar checkers-grabbed nil
  "Flag for when a piece is checkers-grabbed for moving.")

(defvar checkers-p1-score nil
  "Player 1s score")

(defvar checkers-p2-score nil
  "Player 2s score")

(defvar checkers-jump-flag nil
  "Flag for when player jumped.  Signals turn changer to not switch turns.")

;;; If we have a window system, load in cool colors to use on the game board
(if game-lib-use-colors
    (progn
      (game-lib-load-color 'checkers-player1-face
			   "black" "orangered3"
			   "black" "orangered3" nil)
      (game-lib-load-color 'checkers-player2-face
			   "white" "orangered3"
			   "white" "orangered3" nil)
      (game-lib-load-color 'checkers-black-face
			   "gray" "black"
			   "gray" "black" nil)
      ))

(defun checkers ()
  "Mode for playing checkers"

  (interactive)
  (switch-to-buffer (get-buffer-create "Checkers"))
  (setq mode-name "Check")
  (setq major-mode 'checkers)
  (game-lib-clear-buffer)
  (game-lib-insert-string 1 "+----+----+----+----+----+----+----+----+
|%%%%|    |%%%%|    |%%%%|    |%%%%|    |
+----+----+----+----+----+----+----+----+
|    |%%%%|    |%%%%|    |%%%%|    |%%%%|
+----+----+----+----+----+----+----+----+
|%%%%|    |%%%%|    |%%%%|    |%%%%|    |
+----+----+----+----+----+----+----+----+
|    |%%%%|    |%%%%|    |%%%%|    |%%%%|
+----+----+----+----+----+----+----+----+
|%%%%|    |%%%%|    |%%%%|    |%%%%|    |
+----+----+----+----+----+----+----+----+
|    |%%%%|    |%%%%|    |%%%%|    |%%%%|
+----+----+----+----+----+----+----+----+
|%%%%|    |%%%%|    |%%%%|    |%%%%|    |
+----+----+----+----+----+----+----+----+
|    |%%%%|    |%%%%|    |%%%%|    |%%%%|
+----+----+----+----+----+----+----+----+
" 'checkers-black-face)
  (make-local-variable 'tyrant-turn)
  (setq tyrant-turn 2)
  (checkers-put 0 5 nil) (checkers-put 2 5 nil) (checkers-put 4 5 nil) (checkers-put 6 5 nil)
  (checkers-put 1 6 nil) (checkers-put 3 6 nil) (checkers-put 5 6 nil) (checkers-put 7 6 nil)
  (checkers-put 0 7 nil) (checkers-put 2 7 nil) (checkers-put 4 7 nil) (checkers-put 6 7 nil)
  (setq tyrant-turn 1)
  (checkers-put 1 0 nil) (checkers-put 3 0 nil) (checkers-put 5 0 nil) (checkers-put 7 0 nil)
  (checkers-put 0 1 nil) (checkers-put 2 1 nil) (checkers-put 4 1 nil) (checkers-put 6 1 nil)
  (checkers-put 1 2 nil) (checkers-put 3 2 nil) (checkers-put 5 2 nil) (checkers-put 7 2 nil)
  ;; when windowd, lets put special color on black parts
  (if (and game-lib-use-colors window-system)
      (progn
	(checkers-put 0 3 nil nil t) (checkers-put 2 3 nil nil t) 
	(checkers-put 4 3 nil nil t) (checkers-put 6 3 nil nil t)
	(checkers-put 1 4 nil nil t) (checkers-put 3 4 nil nil t) 
	(checkers-put 5 4 nil nil t) (checkers-put 7 4 nil nil t)
	))
  (delete-other-windows (selected-window))
  (use-local-map checkers-map)
  (make-local-variable 'checkers-x)
  (setq checkers-x 1)
  (make-local-variable 'checkers-y)
  (setq checkers-y 0)
  (make-local-variable 'checkers-grabbed)
  (setq checkers-grabbed nil)
  (make-local-variable 'checkers-p1-score)
  (setq checkers-p1-score 12)
  (make-local-variable 'checkers-p2-score)
  (setq checkers-p2-score 12)
  (make-local-variable 'checkers-jump-flag)
  (setq checkers-jump-flag nil)
  (make-local-variable 'tyrant-mouse-function)
  (setq tyrant-mouse-function 'checkers-mouse-support)
  (make-local-variable 'etalk-tyrant-brief-help)
  (setq etalk-tyrant-brief-help
"Checkers: ")
  (split-window (selected-window) 50 t)
  (other-window 1)
  (switch-to-buffer (get-buffer-create checkers-stat))
  (game-lib-clear-buffer)
  (insert "Player 1 score:
  12 pieces

Player 2 score:
  12 pieces

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
	     (switch-to-buffer checkers-stat)
	     (make-local-variable 'etalk-tag)
	     (setq etalk-tag t)
	     (goto-line 1)
	     (delete-region (point) (save-excursion (end-of-line) (point)))
	     (insert (save-excursion
		       (set-buffer "Checkers")
		       (tyrant-format "%n score:")))
	     (goto-line 4)
	     (delete-region (point) (save-excursion (end-of-line) (point)))
	     (insert (save-excursion
		       (set-buffer "Checkers")
		       (tyrant-format "%N score:"))))))
  (setq tyrant-player2-hook
	'(lambda ()
	   (setq etalk-tyrant-enabled-console nil)
	   (message "Your are player 2")
	   (save-window-excursion
	     (switch-to-buffer checkers-stat)
	     (make-local-variable 'etalk-tag)
	     (setq etalk-tag t)
	     (goto-line 1)
	     (delete-region (point) (save-excursion (end-of-line) (point)))
	     (insert (save-excursion
		       (set-buffer "Checkers")
		       (tyrant-format "%n score:")))
	     (goto-line 4)
	     (delete-region (point) (save-excursion (end-of-line) (point)))
	     (insert (save-excursion
		       (set-buffer "Checkers")
		       (tyrant-format "%N score:"))))))
  )

(defun checkers-mouse-support(p e m)
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
	      (if (= ?% (save-excursion
			  (goto-char (+ (checkers-xy2index x y) 1))
			  (following-char)))
		  (message "Illegal move.")
		(setq checkers-x x)
		(setq checkers-y y)
		(goto-char (+ (checkers-xy2index x y) 1))
		(if (memq 'click m)
		    (checkers-mark))))))
    )
  )

(defun checkers-mark ()
  "When SPC, we do marks, or check to see if move is legal."

  (interactive)
  (if checkers-grabbed
      (if (equal checkers-grabbed (list checkers-x checkers-y))
	  (progn
	    (checkers-put checkers-x checkers-y (nth 1 (checkers-owned checkers-x checkers-y)))
	    (checkers-stat-display "releases")
	    (setq checkers-grabbed nil)
	    (if checkers-jump-flag
		(checkers-swap-turns)))
	(let ((x (car checkers-grabbed)) (y (nth 1 checkers-grabbed))
	      (king (nth 1 (checkers-owned (nth 0 checkers-grabbed) (nth 1 checkers-grabbed)))))
	  (if (car (checkers-owned checkers-x checkers-y))
	      (error "Someone is already there!"))
	  (if (and (not (and (or (= (- checkers-x x) 1) (= (- checkers-x x) -1))
			     (or (and (= tyrant-turn 1) (= (- checkers-y y) 1))
				 (and (= tyrant-turn 2) (= (- checkers-y y) -1)))))
		   (not (and king
			     (and (or (= (- checkers-x x) 1) (= (- checkers-x x) -1))
				  (or (= (- checkers-y y) 1) (= (- checkers-y y) -1))))))
	      ;; Ok, this means it is not a legal smarmy move
	      ;; lets check for jumps
	      (if (and (not (and (or (= (- checkers-x x) 2) (= (- checkers-x x) -2))
				 (or (and (= tyrant-turn 1) (= (- checkers-y y) 2))
				     (and (= tyrant-turn 2) (= (- checkers-y y) -2)))))
		       (not (and king
				 (and (or (= (- checkers-x x) 2) (= (- checkers-x x) -2))
				      (or (= (- checkers-y y) 2) (= (- checkers-y y) -2))))))
		  ;; Ok, so this isn't true either
		  (error "That is an illegal move!")
		;; but can we jump over something?
		(let* ((jx (if (< (- checkers-x x) 0) (1- x) (1+ x)))
		       (jy (if (< (- checkers-y y) 0) (1- y) (1+ y)))
		       (jumpee (car (checkers-owned jx jy))))
		  (if (or king
			  (and (not king) (not (eq tyrant-turn jumpee))))
		      (progn
			(setq checkers-jump-flag t)
			(checkers-put x y king nil t)
			(if (not (= jumpee tyrant-turn))
			    (progn
			      (checkers-put jx jy nil nil t)
			      (if (= tyrant-turn 1)
				  (progn
				    (setq checkers-p2-score (1- checkers-p2-score))
				    (if (= checkers-p2-score 0)
					(etalk-usurp-tyrant
					 (checkers-win-string))))
				(setq checkers-p1-score (1- checkers-p1-score))
				(if (= checkers-p1-score 0)
				    (etalk-usurp-tyrant
				     (checkers-win-string)))
				)))
			(checkers-put checkers-x checkers-y king t)
			(let ((can-king (if (and (= tyrant-turn 1) (= checkers-y 7)) t 
					  (if (and (= tyrant-turn 2) (= checkers-y 0)) 
					      t nil))))
			  (if can-king
			      (checkers-put checkers-x checkers-y t checkers-grabbed))
			  (checkers-stat-display "jumps" checkers-grabbed 
						 (if (not (= jumpee 
							     tyrant-turn))
						     1 0)
						 (if can-king "King me!" 
						   nil))
			  (setq checkers-grabbed (list checkers-x checkers-y))
			  ))
		    (error "You can't jump that person!"))
		  )
		)
	    ;; this run on an adjacent move
	    (if checkers-jump-flag
		(error "You may only jump now!"))
	    (checkers-put x y nil nil t)
	    (checkers-put checkers-x checkers-y king)
	    (let ((can-king (if (and (= tyrant-turn 1) (= checkers-y 7)) t 
			      (if (and (= tyrant-turn 2) (= checkers-y 0)) 
				  t nil))))
	      (if can-king
		  (checkers-put checkers-x checkers-y t))
	      (checkers-stat-display "moves" checkers-grabbed nil
				     (if can-king "King me!" nil)))
	    (setq checkers-grabbed nil)
	    (checkers-swap-turns))))
    (if (not (eq (car (checkers-owned checkers-x checkers-y)) tyrant-turn))
	(error "You don't own that piece!"))
    (setq checkers-grabbed (list checkers-x checkers-y))
    (checkers-stat-display "Grabs")
    (checkers-put checkers-x checkers-y (nth 1 (checkers-owned checkers-x checkers-y)) t)))

(defun checkers-win-string ()
  "Generate a string declairing a winner!"
  (if (< checkers-p1-score checkers-p2-score)
      (tyrant-format "%N wins!")
    (tyrant-format "%n wins!")
    ))

(defun checkers-stat-display (action &optional start change special)
  "Modifies the stats display.  Assumes we are in the checkers buffer
when running.  ACTION is a string representing the name of an action."
  (let* ((tyrn (and (boundp 'etalk-tyrannical-mode) etalk-tyrannical-mode))
	 (s1 (format " %d pieces" checkers-p1-score))
	 (s2 (format " %d pieces" checkers-p2-score))
	 (s3-1 (if (not start) (format "%s  (%d,%d)\n" action checkers-x checkers-y)
		 (format "%s \n from (%d,%d) to (%d,%d)" action
			 (nth 0 start) (nth 1 start) checkers-x checkers-y)))
	 (s3 (if tyrn (tyrant-format "%:1p %s" s3-1)
	       (format " Player %d %s" tyrant-turn s3-1)))
	 (s4 (if start (if change (format " %d pieces snarfed" change)
			 " 0 pieces snarfed")
	       " Contemplating."))
	 (s5 (concat " " special)))
    (save-excursion
      (set-buffer checkers-stat)
      (goto-line 2)
      (delete-region (point) (save-excursion (end-of-line) (point)))
      (insert s1)
      (goto-line 5)
      (delete-region (point) (save-excursion (end-of-line) (point)))
      (insert s2)
      (goto-line 8)
      (delete-region (point) (save-excursion (forward-line 1) 
					     (end-of-line) (point)))
      (insert s3)
      (goto-line 12)
      (delete-region (point) (save-excursion (end-of-line) (point)))
      (insert s4)
      (goto-line 14)
      (delete-region (point) (save-excursion (end-of-line) (point)))
      (insert (if s5 s5 ""))))
  )

(defun checkers-owned (x y)
  "returns list, c1 nil if empty, 1 if player 1 peice, and 2 if player 2 peice
c2 is t if king, nil otherwise"

  (save-excursion
    (list
     (progn (goto-char (+ (checkers-xy2index x y) 1))
	    (cond
	     ((= (following-char) (string-to-char checkers-piece1))
	      1)
	     ((= (following-char) (string-to-char checkers-piece2))
	      2)
	     (t nil)))
     (progn (goto-char (+ (checkers-xy2index x y) 2))
	    (cond
	     ((or (= (following-char) (string-to-char checkers-piece1))
		  (= (following-char) (string-to-char checkers-piece2)))
	      t)
	     (t nil))))))
(defun checkers-move ()
  "Move the cursor to a new position"
  (interactive)
  (cond
   ((or (= last-input-char ?\C-f)
	(= last-input-char ?f))
    (if (< checkers-x 6)
	(setq checkers-x (+ checkers-x 2))
      (error "Can't go farther right!")))
   ((or (= last-input-char ?\C-b)
	(= last-input-char ?b))
    (if (> checkers-x 1)
	(setq checkers-x (- checkers-x 2))
      (error "Can't go farther left!")))
   ((or (= last-input-char ?\C-p)
	(= last-input-char ?p))
    (if (> checkers-y 0)
	(progn
	  (if (= (% checkers-x 2) 0)
	      (setq checkers-x (+ checkers-x 1))
	    (setq checkers-x (- checkers-x 1)))
	  (setq checkers-y (- checkers-y 1)))
      (error "Can't go farther up!")))
   ((or (= last-input-char ?\C-n)
	(= last-input-char ?n))
    (if (< checkers-y 7)
	(progn
	  (if (= (% checkers-x 2) 0)
	      (setq checkers-x (+ checkers-x 1))
	    (setq checkers-x (- checkers-x 1)))
	  (setq checkers-y (+ checkers-y 1)))
      (error "Can't go farther down!"))))
  (checkers-place-cursor))

(defun checkers-place-cursor ()
  "Place the cursor on the correct spot on the board..."

  (goto-char (+ 1 (checkers-xy2index checkers-x checkers-y))))

(defun checkers-put (x y king &optional mark off)
  "Based on variable \"turn\" place peice there."

  (game-lib-insert-string (checkers-xy2index x y)
			  (concat
			   (if mark ">" " ")
			   (if (equal off t)
			       "  "
			     (if (= tyrant-turn 1)
				 (if king checkers-king1 checkers-piece1)
			       (if king checkers-king2 checkers-piece2)))
			   (if mark "<" " "))
			  (if (= tyrant-turn 1)
			      'checkers-player1-face
			    'checkers-player2-face)))
  
(defun checkers-xy2index (x y)
  "change x y position to absolute buffer address"

  (+ (+ (+ 1 (* x 5)) (+ (* y 84) 1)) 42))

(defun checkers-swap-turns ()
  "Swap turns in checkers"
  (setq checkers-jump-flag nil)
  (game-lib-swap-turns "It is now %P's turn" "Player %d's move"))

(defun checkers-quit ()
  "quit checkers game"
  (interactive)
  (delete-window (get-buffer-window checkers-stat))
  (kill-buffer checkers-stat)
  (game-lib-quit t))

;;; end of lisp

(provide 'checkers)

