;;; game for playing ataxx (emacs talk optional)
;;;
;;; Copyright (C) 1992 Eric M. Ludlam
;;; Copyright (C) 1994, 1995 Free Software Foundation
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
;;; $Id: ataxx.el,v 1.3 1995/12/10 15:47:05 zappo Exp $
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

(defvar ataxx-map nil
  "Keymap used in playing ataxx")

(if ataxx-map
    ()
  (setq ataxx-map (make-sparse-keymap))
  (define-key ataxx-map "\C-f" 'ataxx-move)  
  (define-key ataxx-map "f" 'ataxx-move)
  (define-key ataxx-map "\C-b" 'ataxx-move)
  (define-key ataxx-map "b" 'ataxx-move)
  (define-key ataxx-map "\C-p" 'ataxx-move)
  (define-key ataxx-map "p" 'ataxx-move)
  (define-key ataxx-map "\C-n" 'ataxx-move)
  (define-key ataxx-map "n" 'ataxx-move)
  (define-key ataxx-map " " 'ataxx-place-piece)
  (define-key ataxx-map "q" 'ataxx-quit)
  (define-key ataxx-map "g" 'ataxx-pass)
  (define-key ataxx-map "P" 'ataxx-pass)
  (game-lib-add-mouse-support ataxx-map)
)

(defconst ataxx-piece1 "##"
  "String as piece 1")

(defconst ataxx-piece2 "::"
  "String as piece 2")

(defconst ataxx-stat "Ataxx statistics"
  "Buffer name of statistics window.")

(defvar ataxx-x nil
  "The current X position of cursor")

(defvar ataxx-y nil
  "The current Y position of cursor")

(defvar ataxx-p1-score nil
  "Player 1's score")

(defvar ataxx-p2-score nil
  "Player 2's score")

(defvar ataxx-grabbed nil
  "Define when a piece has been ataxx-grabbed for movement.")


(defun ataxx ()
  "Mode for playing ataxx.
Rules:  A person may either \"drool\" adjacently, and create a new peice.
          or \"jump\" up to 2 places away and move a peices.  When you move
          adjacent to an opponants peice, you take it over.  Take the most 
          peices.
Playing:
  \\<ataxx-map>
  [C-]fbnp	Move cursor
  \\[ataxx-place-piece]		Grab a piece and then move it to a new location,
		or let go of it.
  \\[ataxx-pass]		pass turn to other player
  \\[ataxx-quit]		quit attax
"
  (interactive)
  (switch-to-buffer (get-buffer-create "Ataxx"))
  (setq mode-name "Ataxx")
  (setq major-mode 'ataxx)
  (game-lib-clear-buffer)
  (insert "+----+----+----+----+----+----+----+
|    |    |    |    |    |    |    |
+----+----+----+----+----+----+----+
|    |    |    |    |    |    |    |
+----+----+----+----+----+----+----+
|    |    |    |    |    |    |    |
+----+----+----+----+----+----+----+
|    |    |    |    |    |    |    |
+----+----+----+----+----+----+----+
|    |    |    |    |    |    |    |
+----+----+----+----+----+----+----+
|    |    |    |    |    |    |    |
+----+----+----+----+----+----+----+
|    |    |    |    |    |    |    |
+----+----+----+----+----+----+----+")
  (delete-other-windows (selected-window))
  (use-local-map ataxx-map)
  (setq tyrant-turn 2)
  (ataxx-put 6 0)
  (ataxx-put 0 6)
  (setq tyrant-turn 1)
  (ataxx-put 0 0)
  (ataxx-put 6 6)
  (setq ataxx-x 0)
  (setq ataxx-y 0)
  (setq ataxx-p1-score 2)
  (setq ataxx-p2-score 2)
  (setq ataxx-grabbed nil)
  (make-local-variable 'tyrant-mouse-function)
  (setq tyrant-mouse-function 'ataxx-mouse-support)
  (make-local-variable 'etalk-tyrant-brief-help)
  (setq etalk-tyrant-brief-help
"ATAXX:[C-f]Forward [C-b]Back [C-p]Previous [C-n]Next [g]Pass [SPC] Grab/Drop")
  (split-window (selected-window) 50 t)
  (make-local-variable 'etalk-tyrant-quit-string)
  (fset 'etalk-tyrant-quit-string 'ataxx-win-string)
  (other-window 1)
  (switch-to-buffer (get-buffer-create ataxx-stat))
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
	     (switch-to-buffer ataxx-stat)
	     ;; we must make a special effort to tag this buffer.
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
	   (message "Your are player 2") ;player2 goes 2nd
	   (save-window-excursion
	     (switch-to-buffer ataxx-stat)
	     (make-local-variable 'etalk-tag)
	     (setq etalk-tag t)
	     (goto-line 1)
	     (delete-region (point) (save-excursion (end-of-line) (point)))
	     (insert (tyrant-format "%n score:"))
	     (goto-line 4)
	     (delete-region (point) (save-excursion (end-of-line) (point)))
	     (insert (tyrant-format "%N score:")))))
  )

(defun ataxx-mouse-support(p e m)
  "Reads in a mouse event from the game-lib driver, and allows a
player to click on a square."

  ;; find which square we are closest to and go there
  (if (and (not (memq 'drag m)) (integerp p))
      (let ((first-line 0)
	    (line-width 37)
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
	      (setq ataxx-x x)
	      (setq ataxx-y y)
	      (goto-char (+ (ataxx-xy2index x y) 1))
	      (if (memq 'click m)
		  (ataxx-place-piece)))))
    )
  )

(defun ataxx-place-piece ()
  "Put a piece onto the board."

  (interactive)
  (if ataxx-grabbed
      (let ((xd (- ataxx-x (car ataxx-grabbed)))
	    (yd (- ataxx-y (nth 1 ataxx-grabbed)))
	    (newscore 0))
	(if (and (= xd 0) (= yd 0))
	    (progn
	      (ataxx-put (nth 0 ataxx-grabbed) (nth 1 ataxx-grabbed) t t) ;clear
	      (ataxx-put (nth 0 ataxx-grabbed) (nth 1 ataxx-grabbed)) ;reset
	      (setq ataxx-grabbed nil)
	      )	      
	  (if (ataxx-owned ataxx-x ataxx-y)
	      (error "You can't go on top of someone elses peice!")
	    (if (and (<= xd 1) (>= xd -1) (<= yd 1) (>= yd -1))
		;; the we may drool to a neighboring square
		(progn
		  (ataxx-put (nth 0 ataxx-grabbed) (nth 1 ataxx-grabbed) t t) ;clear
		  (ataxx-put (nth 0 ataxx-grabbed) (nth 1 ataxx-grabbed)) ;reset
		  (ataxx-put ataxx-x ataxx-y)		;new
		  (setq newscore (ataxx-snarf-pieces ataxx-x ataxx-y))
		  (let* ((name (if (and (boundp 'etalk-tyrannical-mode)
					etalk-tyrannical-mode)
				   (tyrant-format "%:1P")
				 (format "Player %d" tyrant-turn)))
			 (s1 (format " %s drools!\n  %d, %d ==> %d, %d" name
				     (nth 0 ataxx-grabbed) (nth 1 ataxx-grabbed) ataxx-x ataxx-y))
			 (s2 (format " %s gains %d stones" name (1+ newscore))))
		    (save-excursion
		      (set-buffer ataxx-stat)
		      (goto-line 8)
		      (delete-region (point)
				     (save-excursion (forward-line 1)
						     (end-of-line) (point)))
		      (insert s1)
		      (goto-line 12)
		      (delete-region (point) (save-excursion 
					       (end-of-line) (point)))
		      (insert s2)))
		  (if (= tyrant-turn 1)
		      (progn
			(setq ataxx-p1-score (+ ataxx-p1-score 1 newscore))
			(setq ataxx-p2-score (- ataxx-p2-score newscore)))
		    (setq ataxx-p2-score (+ ataxx-p2-score 1 newscore))
		    (setq ataxx-p1-score (- ataxx-p1-score newscore))))
	      (if (and (<= xd 2) (>= xd -2) (<= yd 2) (>= yd -2)) ;a jump
		  (progn
		    (ataxx-put (nth 0 ataxx-grabbed) (nth 1 ataxx-grabbed) t t) ;clear
		    (ataxx-put ataxx-x ataxx-y)		;new
		    (setq newscore (ataxx-snarf-pieces ataxx-x ataxx-y))
		    (let* ((name (if (and (boundp 'etalk-tyrannical-mode)
					  etalk-tyrannical-mode)
				     (tyrant-format "%:1P")
				   (format "Player %d" tyrant-turn)))
			   (s1 (format " %s jumps!\n  %d, %d ==> %d, %d" name
				       (nth 0 ataxx-grabbed) (nth 1 ataxx-grabbed) ataxx-x ataxx-y))
			   (s2 (format " %s gains %d stones" name newscore)))
		      (save-excursion
			(set-buffer ataxx-stat)
			(goto-line 8)
			(delete-region (point)
				       (save-excursion (forward-line 1)
						       (end-of-line) (point)))
			(insert s1)
			(goto-line 12)
			(delete-region (point) (save-excursion 
						 (end-of-line) (point)))
			(insert s2)))
		    (if (= tyrant-turn 1)
			(progn
			  (setq ataxx-p1-score (+ ataxx-p1-score newscore))
			  (setq ataxx-p2-score (- ataxx-p2-score newscore)))
		      (setq ataxx-p2-score (+ ataxx-p2-score newscore))
		      (setq ataxx-p1-score (- ataxx-p1-score newscore))))
		(error "That is neither a jump nor drool!")))
	    (setq ataxx-grabbed nil)
	    (ataxx-swap-turns))))
    (if (not (eq (ataxx-owned ataxx-x ataxx-y) tyrant-turn))
	(error "That isn't one of your pieces silly!")
      (ataxx-put ataxx-x ataxx-y nil t)
      (setq ataxx-grabbed (list ataxx-x ataxx-y))
      (let ((s1 (format "Grabbing (%d,%d)\n" ataxx-x ataxx-y)))
	(save-excursion
	  (set-buffer ataxx-stat)
	  (goto-line 8)
	  (delete-region (point) (save-excursion (forward-line 1)
						 (end-of-line) (point)))
	  (insert s1)
	  (goto-line 12)
	  (delete-region (point) (save-excursion (end-of-line) (point)))))))
  (if (ataxx-win)
      (if (and (boundp 'etalk-tyrannical-mode) etalk-tyrannical-mode)  
	  (etalk-usurp-tyrant (ataxx-win-string))
	(message (ataxx-win-string)))))

(defun ataxx-snarf-pieces (x y)
  "Pretend to move to xy, then change all adjacent owned peices to yourself!"
  (let ((dx -1) (dy -1) (got 0))
    (while (< dx 2)
      (setq dy -1)
      (while (< dy 2)
	(if (and (ataxx-owned (+ dx x) (+ dy y))
		 (not (= (ataxx-owned (+ dx x) (+ dy y)) tyrant-turn)))
	    (progn
	      (setq got (1+ got))
	      (ataxx-put (+ dx x) (+ dy y))))
	(setq dy (1+ dy)))
      (setq dx (1+ dx)))
    got))

(defun ataxx-win-string ()
  "return a string declairing the winner"
  (ataxx-swap-turns)
  (game-lib-win (if (> ataxx-p1-score ataxx-p2-score) 1 2)
		"%P wins!" "Player %d wins!"))

(defun ataxx-win ()
  "return t if that is a winning move"
  (or (= ataxx-p1-score 0) (= ataxx-p2-score 0) (= (+ ataxx-p1-score ataxx-p2-score) 49))
)

(defun ataxx-owned (x y)
  "returns nil if empty, 1 if player 1 peice, and 2 if player 2 peice"
  (save-excursion
    (goto-char (+ (ataxx-xy2index x y) 1))
    (cond
     ((= (following-char) ?\ )
      nil)
     ((= (following-char) (string-to-char ataxx-piece1))
      1)
     ((= (following-char) (string-to-char ataxx-piece2))
      2)
     (t nil))))

(defun ataxx-move ()
  "Move the cursor to a new position"
  (interactive)
  (cond
   ((or (= last-input-char ?\C-f)
	(= last-input-char ?f))
    (if (< ataxx-x 6)
	(setq ataxx-x (+ ataxx-x 1))
      (error "Can't go farther right!")))
   ((or (= last-input-char ?\C-b)
	(= last-input-char ?b))
    (if (> ataxx-x 0)
	(setq ataxx-x (- ataxx-x 1))
      (error "Can't go farther left!")))
   ((or (= last-input-char ?\C-p)
	(= last-input-char ?p))
    (if (> ataxx-y 0)
	(setq ataxx-y (- ataxx-y 1))
      (error "Can't go farther up!")))
   ((or (= last-input-char ?\C-n)
	(= last-input-char ?n))
    (if (< ataxx-y 6)
	(setq ataxx-y (+ ataxx-y 1))
      (error "Can't go farther down!"))))
  (ataxx-place-cursor))

(defun ataxx-place-cursor ()
  "Place the cursor on the correct spot on the board..."

  (goto-char (+ 1 (ataxx-xy2index ataxx-x ataxx-y))))

(defun ataxx-put (x y &optional off mark)
  "Based on variable \"turn\" place peice there."

    (let* ((coff (if mark 0 1))
	   (deln (if mark 4 2))
	   (str (if off "  " 
		  (if (= tyrant-turn 1) ataxx-piece1 ataxx-piece2)))
	   (pstr (if mark (if off 
			      (concat " " str " ")
			    (concat ">" str "<"))
		   str))
	   (co (if (= tyrant-turn 1)
		   'game-lib-player1-face
		   'game-lib-player2-face)))
      (game-lib-insert-string (+ coff (ataxx-xy2index x y))
			      pstr co)))
  
(defun ataxx-xy2index (x y)
  "change x y position to absolute buffer address"

  (+ (+ (+ 1 (* x 5)) (+ (* y 74) 1)) 37))

(defun ataxx-swap-turns ()
  "Swap turns in ataxx"

  (game-lib-swap-turns "It is now %P's turn" "Player %d's move")
  (save-excursion
    (let ((s1 (format "  %d blocks" ataxx-p1-score))
	  (s2 (format "  %d blocks" ataxx-p2-score)))
      (set-buffer ataxx-stat)
      (goto-line 2)
      (delete-region (point) (save-excursion (end-of-line) (point)))
      (insert s1)
      (goto-line 5)
      (delete-region (point) (save-excursion (end-of-line) (point)))
      (insert s2))))

(defun ataxx-pass ()
  "Pass turn to next player in ataxx"
  
  (interactive)
  (ataxx-swap-turns)
  (save-excursion
    (set-buffer ataxx-stat)
    (goto-line 8)
    (delete-region (point)
		   (save-excursion (forward-line 1)
				   (end-of-line) (point)))
    (insert "Turn passed\n")
    (goto-line 12)
    (delete-region (point) (save-excursion 
			     (end-of-line) (point)))
    (insert "Turn passed"))) 

(defun ataxx-quit ()
  "quit ataxx"
  
  (interactive)
  (delete-window (get-buffer-window ataxx-stat))
  (kill-buffer ataxx-stat)
  (game-lib-quit t))

;;; end of lisp

(provide 'ataxx)
