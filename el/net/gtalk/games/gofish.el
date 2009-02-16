;;; gofish game (emacs talk, and tyrant-ai optional)
;;;
;;; Copyright (C) 1992 1993 1994 Amy E. Ludlam
;;;
;;; Author: Amy E. Ludlam <zappo@gnu.ai.mit.edu>
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
;;; Please send bug reports, etc. to Amy CO zappo@gnu.ai.mit.edu.
;;;
;;; $Id: gofish.el,v 1.1 1994/08/29 23:51:07 zappo Exp $
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
;;;  Under tyrant-ai.  To run, load the library "tyrn-ai" and then use   ;;;
;;;  the function "tyrant-play-computer" and choose this game.  An AI    ;;;
;;;  function must be installed on the system for this to work.          ;;;
;;;                                                                      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'card-lib "games/card-lib")

(defvar gofish-buff-name "Go Fish"
  "name of buffer to play GoFish in")

(defvar gofish-mode-map nil)
(if gofish-mode-map
    ()
  (setq gofish-mode-map (make-sparse-keymap))
  (define-key gofish-mode-map "s" 'gofish-sort-hand)
  (define-key gofish-mode-map "S" 'gofish-sort-hand)
  (define-key gofish-mode-map "\C-f" 'gofish-move-cursor)
  (define-key gofish-mode-map "f" 'gofish-move-cursor)
  (define-key gofish-mode-map "F" 'gofish-move-cursor)
  (define-key gofish-mode-map "\C-b" 'gofish-move-cursor)
  (define-key gofish-mode-map "b" 'gofish-move-cursor)
  (define-key gofish-mode-map "B" 'gofish-move-cursor)
  (define-key gofish-mode-map " " 'gofish-select-card)
  (define-key gofish-mode-map "d" 'gofish-display-hand)
  (define-key gofish-mode-map "D" 'gofish-display-hand)
  (define-key gofish-mode-map "x" 'gofish-quit)
  (define-key gofish-mode-map "X" 'gofish-quit)
  (define-key gofish-mode-map "a" 'gofish-select-card-keyed)
  (define-key gofish-mode-map "A" 'gofish-select-card-keyed)
  (define-key gofish-mode-map "2" 'gofish-select-card-keyed)
  (define-key gofish-mode-map "3" 'gofish-select-card-keyed)
  (define-key gofish-mode-map "4" 'gofish-select-card-keyed)
  (define-key gofish-mode-map "5" 'gofish-select-card-keyed)
  (define-key gofish-mode-map "6" 'gofish-select-card-keyed)
  (define-key gofish-mode-map "7" 'gofish-select-card-keyed)
  (define-key gofish-mode-map "8" 'gofish-select-card-keyed)
  (define-key gofish-mode-map "9" 'gofish-select-card-keyed)
  (define-key gofish-mode-map "0" 'gofish-select-card-keyed)
  (define-key gofish-mode-map "1" 'gofish-select-card-keyed)
  (define-key gofish-mode-map "j" 'gofish-select-card-keyed)
  (define-key gofish-mode-map "q" 'gofish-select-card-keyed)
  (define-key gofish-mode-map "k" 'gofish-select-card-keyed)
  (define-key gofish-mode-map "J" 'gofish-select-card-keyed)
  (define-key gofish-mode-map "Q" 'gofish-select-card-keyed)
  (define-key gofish-mode-map "K" 'gofish-select-card-keyed)
)

(defvar gofish-hand1 nil
  "Player 1s hand")

(defvar gofish-hand2 nil
  "Player 2s hand")

(defvar gofish-hand1-score nil
  "Player 1's score")

(defvar gofish-hand2-score nil
  "Player 2s score")

(defvar gofish-hand-size nil
  "Cards in a hand")

(defvar gofish-player1 nil
  "Flag for if I am player 1.  For tyrants, this means we send our
deck to remote user.")

(defvar gofish-cursor-card1 nil
  "Card user 1 is on")

(defvar gofish-cursor-card2 nil
  "Card user 2 is on")

(defvar gofish-card-xoffset nil
  "X offset of card")

(defvar gofish-card-yoffset nil
  "Y offset of card")

(defvar gofish-unique1 nil
  "Unique cards from hand 1")

(defvar gofish-unique2 nil
  "Unique cards from hand 2")

(defun gofish ()
  "GoFishies for tyrant mode:
'f' and 'b'     : forward and backward
' '             : space selects that card to ask
's'             : sort your hand and redisplay

For non-tyranted version:
'x'             : quit and annihalate all created buffers
"

  (interactive)
  (switch-to-buffer (get-buffer-create gofish-buff-name))
  (game-lib-clear-buffer)
  (delete-other-windows (selected-window))
  (setq major-mode 'gofish)
  (setq mode-name "GoFishies!")
  (use-local-map gofish-mode-map)

  (make-local-variable 'gofish-hand1)
  (setq gofish-hand1 '())
  (make-local-variable 'gofish-hand2)
  (setq gofish-hand2 '())
  (make-local-variable 'gofish-hand1-score)
  (setq gofish-hand1-score 0)
  (make-local-variable 'gofish-hand2-score)
  (setq gofish-hand2-score 0)
  (make-local-variable 'gofish-hand-size)
  (setq gofish-hand-size 7)
  (make-local-variable 'tyrant-turn)
  (make-local-variable 'gofish-player1)
  (setq gofish-player1 t)
  (make-local-variable 'gofish-cursor-card1)
  (setq gofish-cursor-card1 0)
  (make-local-variable 'gofish-cursor-card2)
  (setq gofish-cursor-card2 0)
  (make-local-variable 'gofish-card-xoffset)
  (setq gofish-card-xoffset 2)
  (make-local-variable 'gofish-card-yoffset)
  (setq gofish-card-yoffset 3)
  (make-local-variable 'gofish-unique1)
  (setq gofish-unique1 '())
  (make-local-variable 'gofish-unique2)
  (setq gofish-unique2 '())

  (setq tyrant-player1-hook
	'(lambda ()
	   (gofish-send-hands)
	   (message "You are player 1.")))
  (setq tyrant-player2-hook
	'(lambda ()
	   (setq gofish-player1 nil)
	   (setq etalk-tyrant-enabled-console nil)
	   (setq tyrant-call-interpreter 'gofish-do-message)
	   (game-lib-clear-buffer)
	   (message "You are player 2.")))

  (cards-shuffle)
  (setq tyrant-turn 1)
  (setq gofish-hand1 (gofish-discard-duplicates (cards-deal gofish-hand-size)))
  (setq gofish-hand2 (gofish-discard-duplicates (cards-deal gofish-hand-size)))
  (setq mode-line-buffer-identification 
	(list "Emacs" ": %7b"
	      (format " p1 [%2d] p2 [%2d]" 
		      gofish-hand1-score
		      gofish-hand2-score)))
  (set-buffer-modified-p (buffer-modified-p))
  (gofish-display-hand)
  (run-hooks 'gofish-hooks)
)

(defun gofish-quit ()
  "Quit gofish when not in talk-tyrannical-mode.  This allows you to quickly
leave the game and destroy all the buffers at the same time."
  
  (interactive)
  (if (not (gofish-check-tyrant))
      (progn
	(if (string= (read-input "Really quit Go Fishies? (y or n) ") "n")
	    ()
	  (kill-all-local-variables)
	  (kill-buffer gofish-buff-name)))
    (message "Type C-cC-c to exit."))
)

(defun gofish-check-tyrant ()
  "This functions simply checks if you are in tyrant mode."
  
  (and (boundp 'etalk-tyrannical-mode) etalk-tyrannical-mode)
)

(defun gofish-other-turn ()
  "Return the opponents turn number."

  (cond
   ((= tyrant-turn 1)
    2)
   ((= tyrant-turn 2)
    1))
)

(defun gofish-message (fished check-card)
  "Send the gofish message to both terminals.  fished is t if they
went gofish otherwise nil."

  (if fished
      (if (and (gofish-check-tyrant) gofish-player1)
	  (gofish-message-both
	   (cond
	    ((= tyrant-turn 1)
	     (tyrant-format 
	      "%p must Go Fish!!! Requested %s from %U" check-card))
	    ((= tyrant-turn 2)
	     (tyrant-format 
	      "%p must Go Fish!!! Requested %s from %u" check-card))))
	(message (format "Player %d must Go Fish!!! Requested %s" 
			 tyrant-turn check-card)))
    (if (and (gofish-check-tyrant) gofish-player1)
	(gofish-message-both
	 (cond
	  ((= tyrant-turn 1)
	   (tyrant-format "%p stole %s's from %U!" 
			  check-card))
	  ((= tyrant-turn 2)
	   (tyrant-format "%p stole %s's from %u!" 
			  check-card))))
      (message "Player %d stole %s's from Player %d!"
	       tyrant-turn check-card (gofish-other-turn))))
)


(defun gofish-message-both (mess)
  "Display the same message on both terminals."
  
  (if (and (gofish-check-tyrant) gofish-player1)
      (progn
	(gofish-send-hands 10 mess)
	(message mess))
    (message mess))
)


(defun gofish-do-message (mess)
  "In tyrant mode, player 2 hook uses this function to interpolate the
messages which are passed to it.  Allows games to run easier."

  (cond
   ((= (string-to-char mess) 5)
    (setq gofish-hand1 (cards-message-to-hand 
			(substring mess 1 (length mess))))
    (gofish-display-hand))
   ((= (string-to-char mess) 6)
    (setq gofish-hand2 (cards-message-to-hand 
			(substring mess 1 (length mess))))
    (gofish-display-hand))
   ((= (string-to-char mess) 7)
    (setq gofish-hand1-score (string-to-int (substring mess 1 (length mess))))
    (setq mode-line-buffer-identification 
	  (list "Emacs" ": %7b"
		(format " p1 [%2d] p2 [%2d]" 
			gofish-hand1-score
			gofish-hand2-score)))
    (set-buffer-modified-p (buffer-modified-p)))
   ((= (string-to-char mess) 8)
    (setq gofish-hand2-score (string-to-int (substring mess 1 (length mess))))
    (setq mode-line-buffer-identification 
	  (list "Emacs" ": %7b"
		(format " p1 [%2d] p2 [%2d]" 
			gofish-hand1-score
			gofish-hand2-score)))
    (set-buffer-modified-p (buffer-modified-p)))
   ((= (string-to-char mess) 9)
    (gofish-swap-turns))
   (t ;; all else is something not covered above (obviously)
    (message mess)))
)

(defun gofish-send-hands (&optional which mess)
  "Send the hands from player1 to player2 so they have the current
status.  It determines what to send by the parameter."

  (if (and (gofish-check-tyrant)
	   gofish-player1)
      (if which
	  (cond 
	   ;; gofish-hand1
	   ((= which 5)
	    (tyrant-send-message (concat (char-to-string 5)
				  (cards-hand-to-message gofish-hand1))))
	   ;; gofish-hand2
	   ((= which 6)
	    (tyrant-send-message (concat (char-to-string 6)
				  (cards-hand-to-message gofish-hand2))))
	   ;; gofish-hand1-score
	   ((= which 7)
	    (tyrant-send-message (concat (char-to-string 7)
				  gofish-hand1-score)))
	   ;; gofish-hand2-score
	   ((= which 8)
	    (tyrant-send-message (concat (char-to-string 8)
					 gofish-hand2-score)))
	   ;; swap-turns
	   ((= which 9)
	    (tyrant-send-message (char-to-string 9)))
	   ;; message for minibuffer
	   ((= which 10)
	    (tyrant-send-message mess)))

	;; send all of the above
	(tyrant-send-message (concat (char-to-string 5)
			      (cards-hand-to-message gofish-hand1)))
	(tyrant-send-message (concat (char-to-string 6)
			      (cards-hand-to-message gofish-hand2)))
	(tyrant-send-message (concat (char-to-string 7)
			      gofish-hand1-score))
	(tyrant-send-message (concat (char-to-string 8)
			      gofish-hand2-score))))
)

(defun gofish-sort-hand ()
  "Sort the current players hand and redisplay.  Only the player whose turn it
is needs to redisplay the hand."
  
  (interactive)
  (if (= tyrant-turn 1)
      (progn
	(setq gofish-hand1 (cards-sort-by-face gofish-hand1))
	(setq gofish-unique1 '())
	(if (or (and (gofish-check-tyrant)
		     etalk-tyrant-enabled-console)
		(not (gofish-check-tyrant)))
	    (gofish-display-hand)))
    (setq gofish-hand2 (cards-sort-by-face gofish-hand2))
    (setq gofish-unique2 '())
    (if (or (and (gofish-check-tyrant)
		 etalk-tyrant-enabled-console)
	    (not (gofish-check-tyrant)))
	(gofish-display-hand)))
)

(defun gofish-turn-change ()
  "Redisplay the turn change in the buffer."

  (save-excursion
    (goto-line 2)
    (beginning-of-line)
    (delete-region (point) (save-excursion (end-of-line) (point)))
    (if (gofish-check-tyrant)
	(insert (tyrant-format "%p's turn"))
      (insert "Player " (int-to-string tyrant-turn) "'s turn")))
)

(defun gofish-swap-turns ()
  "Procedure to swap the turn variables and tyrant variables."

  (if (and gofish-hand1 gofish-hand2)
      (progn
	(cond ((= tyrant-turn 1) 
	       (setq tyrant-turn 2))
	      ((= tyrant-turn 2) 
	       (setq tyrant-turn 1)))
	(if (boundp 'etalk-tyrant-enabled-console)
	    (setq etalk-tyrant-enabled-console
		  (not etalk-tyrant-enabled-console)))
	(if (gofish-check-tyrant)
	    (progn
	      (gofish-send-hands 9)
	      (gofish-turn-change))
	  (gofish-display-hand)))
    (if (gofish-check-tyrant)
	(progn
	  (setq gofish-hand1-score (- gofish-hand1-score (length gofish-hand1)))
	  (setq gofish-hand2-score (- gofish-hand2-score (length gofish-hand2)))
	  (gofish-send-hands 9)
	  (setq mode-line-buffer-identification 
		(list "Emacs" ": %7b"
		      (format " p1 [%2d] p2 [%2d]" 
			      gofish-hand1-score
			      gofish-hand2-score)))
	  (set-buffer-modified-p (buffer-modified-p))
	  (gofish-display-hand)
	  (if (and (> gofish-hand1-score gofish-hand2-score)
		   (= 2 tyrant-turn))
	      (progn
		(setq tyrant-turn 1)
		(gofish-send-hands 9)))
	  (if (and (> gofish-hand2-score gofish-hand1-score)
		   (= 1 tyrant-turn))
	      (progn
		(setq tyrant-turn 2)
		(gofish-send-hands)))
	      (etalk-usurp-tyrant (tyrant-format "Player %p wins!")))
      (gofish-display-hand)
      (message "Player %d wins!" tyrant-turn)))
)

(defun gofish-take-cards (check-card)
  "Get some cards for your hand."

  (let ((card-list (cards-match-by-face check-card (if (= tyrant-turn 1)
						       gofish-hand2
						     gofish-hand1)))
	(doswap nil))
    (if gofish-player1
	(progn
	  (setq gofish-unique1 '())
	  (setq gofish-unique2 '())
	  (if (not card-list)
	      (progn
		(setq card-list (cards-deal 1))
		(gofish-message t check-card))
	    (if (gofish-check-tyrant)
		(gofish-message nil check-card)))
	  (if (not (cards-match-by-face check-card card-list))
	      (setq doswap t))
	  (if (= tyrant-turn 1)
	      (setq gofish-hand2 (cards-remove-from-list card-list
							 gofish-hand2))
	    (setq gofish-hand1 (cards-remove-from-list card-list
						       gofish-hand1)))
	  (while card-list
	    (if (= tyrant-turn 1)
		(setq gofish-hand1 (cons (car card-list) gofish-hand1))
	      (setq gofish-hand2 (cons (car card-list) gofish-hand2)))
	    (setq card-list (cdr card-list)))
	  (let ((sent nil))
	    (cond
	     ((= tyrant-turn 1)
	      (setq sent (gofish-update-score 
			  (copy-sequence gofish-hand1) 1))
	      (if (not sent)
		  (gofish-send-hands 5))
	      (gofish-send-hands 6))
	     ((= tyrant-turn 2)
	      (setq sent (gofish-update-score 
			  (copy-sequence gofish-hand2) 2))
	      (if (not sent)
		  (gofish-send-hands 6))
	      (gofish-send-hands 5))))
	  (gofish-display-hand)
	  (if (or doswap (not (and gofish-hand1 gofish-hand2)))
	      (gofish-swap-turns)))))
)

(defun gofish-update-score (hand num)
  "See if the hand can remove duplicates, and then update the score
appropriately."

  (if gofish-player1
      (let ((temphand (gofish-discard-duplicates hand)))
	(if (< (length temphand) (length hand))
	    (progn
	      (cond
	       ((= num 1)
		(setq gofish-hand1-score (+ gofish-hand1-score 
					    (- (length gofish-hand1) 
					       (length temphand))))
		(setq gofish-hand1 temphand)
		(setq mode-line-buffer-identification 
		      (list "Emacs" ": %7b"
			    (format " p1 [%2d] p2 [%2d]" 
				    gofish-hand1-score
				    gofish-hand2-score)))
		(set-buffer-modified-p (buffer-modified-p))
		(gofish-send-hands 5)
		(gofish-send-hands 7))
	       ((= num 2)
		(setq gofish-hand2-score (+ gofish-hand2-score
					    (- (length gofish-hand2)
					       (length temphand))))
		(setq gofish-hand2 temphand)
		(setq mode-line-buffer-identification 
		      (list "Emacs" ": %7b"
			    (format " p1 [%2d] p2 [%2d]" 
				    gofish-hand1-score
				    gofish-hand2-score)))
		(set-buffer-modified-p (buffer-modified-p))
		(gofish-send-hands 6)
		(gofish-send-hands 8)))
	      t)
	  nil)))
)

(defun gofish-discard-duplicates (&optional inhand)
  "This function removes all duplicates from a gofish hand, which is
the object behind the game.  :) "

  (interactive)
  (let ((dup-list '())
	(dothis inhand)
	(hand (if (not inhand)
		  (if (= tyrant-turn 1)
		      gofish-hand1
		    gofish-hand2)
		inhand)))
    (setq dup-list (gofish-unique-faces (copy-sequence hand) t))
    (while dup-list
      (setq hand (cards-remove-from-list (cards-match-by-face (car dup-list)
							      hand) hand))
      (setq dup-list (cdr dup-list)))
    hand)
)

(defun gofish-display-hand ()
  "Figure out which hand to display where on the screen and then display it.
Also display whose turn it is."
  
  (interactive)
  (game-lib-clear-buffer)
  (insert "\n\n")
  (gofish-turn-change)
  (if (or (and (gofish-check-tyrant) gofish-player1)
	  (and (not (gofish-check-tyrant))
	       (= tyrant-turn 1)))
      (progn
	(gofish-draw-hand (copy-sequence gofish-hand1))
	(gofish-draw-hand (copy-sequence gofish-hand2) t))
    (gofish-draw-hand (copy-sequence gofish-hand2))
    (gofish-draw-hand (copy-sequence gofish-hand1) t))
  (gofish-place-cursor)
) 

(defun gofish-draw-hand (cardlist &optional one-row)
  "Draw a hand of gofish in a neat format."

  (let ((curcard nil)
	(matches '())
	(ind 0)
	(num 0)
	(x 0))
    (save-excursion
      (goto-char (point-max))
      ;; draw a divider line
      (if one-row
	  (progn
	    (insert "\n\n")
	    (while (< x 38)
	      (insert " =")
	      (setq x (+ x 1)))
	    (insert "\n")))
      ;; find out the max number of duplicates in the hand
      (setq x 0)
      (while cardlist
	(if one-row
	    (setq matches cardlist)
	  (setq matches (cards-match-by-face (car cardlist) cardlist)))
	(setq ind 0)
	(while matches
	  (if (< x (length matches))
	      (setq x (length matches)))
	  (if (> num 65)
	      (progn
		(setq num 0)
		(setq ind (+ ind 2))))
	  (if one-row
	      (cards-draw-card num ind (car matches) one-row)
	    (cards-draw-card num ind (car matches) 1))
	  (if one-row
	      (setq num (+ num cards-cardwidth))
	    (setq ind (+ ind 2)))
	  (setq cardlist (cards-remove-from-list (car matches) cardlist))
	  (setq matches (cdr matches)))
	(if (not one-row)
	    (setq num (+ num cards-cardwidth))))
      (if (not one-row)
	  (progn
	    (goto-char (point-max))
	    (cond 
	     ((= x 0)
	      (insert "\n\n\n\n\n\n\n\n"))
	     ((= x 1)
	      (insert "\n\n\n\n"))
	     ((= x 2)
	      (insert "\n\n")))))))
)

(defun gofish-move-cursor ()
  "Tell where to put the cursor next."

  (interactive)
  (cond
   ((or (= (upcase last-input-char) ?F)
	(= last-input-char ?\C-f))
    (if (= tyrant-turn 1)
	(setq gofish-cursor-card1 (1+ gofish-cursor-card1))
      (setq gofish-cursor-card2 (1+ gofish-cursor-card2))))
   ((or (= (upcase last-input-char) ?B)
	(= last-input-char ?\C-b))
    (if (= tyrant-turn 1)
	(setq gofish-cursor-card1 (1- gofish-cursor-card1))
      (setq gofish-cursor-card2 (1- gofish-cursor-card2)))))
  (gofish-place-cursor)
)

(defun gofish-check-cursor ()
  "Check to make sure the cursor is on a valid card."

  (if (and (= tyrant-turn 1) (not gofish-unique1))
      (setq gofish-unique1 (gofish-unique-faces (copy-sequence gofish-hand1)))
    (if (and (= tyrant-turn 2) (not gofish-unique2))
	(setq gofish-unique2 (gofish-unique-faces (copy-sequence
						   gofish-hand2)))))

  (if (= tyrant-turn 1)
      (cond ((> gofish-cursor-card1 (1- (length gofish-unique1)))
	     (setq gofish-cursor-card1 0))
	    ((< gofish-cursor-card1 0)
	     (setq gofish-cursor-card1 (1- (length gofish-unique1)))))
    (cond ((> gofish-cursor-card2 (1- (length gofish-unique2)))
	   (setq gofish-cursor-card2 0))
	  ((< gofish-cursor-card2 0)
	   (setq gofish-cursor-card2 (1- (length gofish-unique2))))))
)

(defun gofish-select-card ()
  "select the face value of the current card."

  (interactive)
  (let ((unique (gofish-unique-faces (copy-sequence 
				      (if (= tyrant-turn 1) gofish-hand1 
					gofish-hand2)))))
    (gofish-take-cards (cards-get-face (nth (if (= tyrant-turn 1)
						gofish-cursor-card1
			      gofish-cursor-card2) unique))))
)

(defun gofish-select-card-keyed ()
  "select the face value of the card indicated by a given keystroke."
  (interactive)

  (let ((target (cards-match-by-face 
		 (cdr (assoc last-input-char cards-normal-faces-char))
		 (if (= tyrant-turn 1) gofish-hand1 gofish-hand2))))
    (if target
	(gofish-take-cards 
	 (cards-get-face (car target)))
      (message "Player %d!  You don't have that card!" tyrant-turn))))

(defun gofish-place-cursor ()
  "Put the cursor on the card from gofish-place-cursor."
  
  (gofish-check-cursor)
  (goto-char (point-min))
  (let ((cursor (if (and (boundp 'etalk-tyrannical-mode) etalk-tyrannical-mode)
		    (if gofish-player1 gofish-cursor-card1
		      gofish-cursor-card2)
		  ;;otherwise make sure we go to that hand
		  (if (= tyrant-turn 1) gofish-cursor-card1
		    gofish-cursor-card2))))
    (if (>= cursor 0)
	(progn
	  (forward-line gofish-card-yoffset)
	  (forward-char (+ (* cards-cardwidth cursor)
			   gofish-card-xoffset)))))
)

(defun gofish-unique-faces (hand &optional dup)
  "return a list containing one of each of the unique face values
currently in hand."

  (let ((unique '()))
    (while hand
      (let ((match (cards-match-by-face (cards-get-face (car hand))
					hand)))
	(if (or (not dup) 
		(and dup (= (length match) 4)))
	    (setq unique (cons (car match) unique)))
	(setq hand (cards-remove-from-list match hand))))
    (reverse unique))
)

;;; end of lisp
(provide 'gofish)
