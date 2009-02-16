;;; Support routines for writing visual card games with color!
;;;
;;; Copyright (C) 1992 1993 1994 Amy Ludlam
;;;
;;; Author: Amy Ludlam <zappo@gnu.ai.mit.edu>
;;; Version: 1.0
;;; Keywords: extensions, games
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

;;; $Id: card-lib.el,v 1.3 1995/09/21 02:10:39 zappo Exp $
;;; History:
;;;

;;; Commentary:
;;;
;;;  This package supports several card decks, plus flexible drawing
;;;  routines which allow a programmer to work on the higher level of
;;;  a card game program with these routines supplying shuffling,
;;;  displays with color, and even conversion into net-packets for
;;;  multi-user games under etalk.
;;;  These extensions require the game-lib.el package from etalk.

(require 'game-lib "games/game-lib")

(defvar cards-deck-type nil)
;; the above variable is set in cards-shuffle and used throught this
;; file to determine what functions to use for various decks of cards.

(defvar cards-normal '(    0  1  2  3  4  5  6  7  8  9 10 11 12 
			  13 14 15 16 17 18 19 20 21 22 23 24 25 
			  26 27 28 29 30 31 32 33 34 35 36 37 38 
			  39 40 41 42 43 44 45 46 47 48 49 50 51 
			  )
  "The available deck of cards...it is not shuffled since they are
drawn at random instead."
)

(defvar cards-pinochle '(1 2 3 4 5 6 7 8 9 10 11 12
			   13 14 15 16 17 18 19 20 21 22 23 24
			   25 26 27 28 29 30 31 32 33 34 35 36
			   37 38 39 40 41 42 43 44 45 46 47 48
			   )
  "The deck of available cards for pinochle.  It is randomly dealt
from, so there is no need to shuffle it."
)

(defvar cards-normal-faces-char
  '((?a . "A") (?A . "A") (?1 . "10") (?2 . "2") (?3 . "3") (?4 . "4")
    (?5 . "5") (?6 . "6") (?7 . "7") (?8 . "8") (?9 . "9") (?0 . "10")
    (?j . "J") (?q . "Q") (?k . "K") (?J . "J") (?Q . "Q") (?K . "K")
    )
  "Association list of characters matched to card faces.  Used
primarilly for matching agains keystrokes to see what face was
chosen.")

(defvar cards-normal-faces '("A" "2" "3" "4" "5" "6" "7" "8" "9" "10"
			     "J" "Q" "K") 
  "Face value of cards based on 13 values")

(defvar cards-pinochle-faces '("9" "10" "J" "Q" "K" "A")
  "Face value based on a deck of pinochle cards.")

(defvar cards-normal-suits '("H" "C" "D" "S")
  "Suits of cards based on 4 values")

(defvar cards-back-side "///"
  "What to display on the back of a card.")

(defvar cards-current-deck nil
  "This is made locally each time a deck is shuffled.")

(defvar cards-cardwidth nil
  "Made local per game to determine the size of a given card for display")

(defvar cards-cardheight nil
  "Made local per game to determine the size of a given card for display")

;;; Stuffed color info into the region!
;;; EML  6/27/94

;;; If we have a window system, load in cool colors to use on the game board
(if game-lib-use-colors
    (progn
      (game-lib-load-color 'cards-backing "blue" "white" "blue" "white" t)
      (game-lib-load-color 'cards-reds "red" "white" "red" "white" t)
      (game-lib-load-color 'cards-blacks "black" "white" "black" "white" t)))

(defun cards-shuffle (&optional deck-type)
  "Initialize a deck of cards within a buffer"

  (setq cards-deck-type deck-type)
  (make-local-variable 'cards-current-deck)
  (cond
   ((not deck-type)
    (setq cards-current-deck (copy-sequence cards-normal))
    (setq cards-deck-type "normal"))
   ((string= deck-type "pinochle")
    (setq cards-current-deck (copy-sequence cards-pinochle))
    (setq cards-deck-type "pinochle")))
  (make-local-variable 'cards-cardwidth)
  (setq cards-cardwidth 6)
  (make-local-variable 'cards-cardheight)
  (setq cards-cardheight 5)
)

(defun cards-to-string-normal (card)
  "Turn a card integer into a string; returns the string"

  (concat "" (nth (% card 13) cards-normal-faces) 
	  (nth (/ card 13) cards-normal-suits))
)

(defun cards-list-to-string-normal (cardlist)
  "Turn an entire list of cards into strings."

  (let ((newlist '()))
    (while cardlist
      (setq newlist (cons (cards-to-string-normal (car cardlist)) newlist))
      (setq cardlist (cdr cardlist)))
    newlist)
)

(defun cards-to-string-pinochle (card)
  "Turn a card integer into a string. Returns the string based on the
deck of pinochle cards (9-A) twice per suit.  The suits are the
same as those in a normal deck of cards."
  (concat "" (nth (% card 6) cards-pinochle-faces) 
	  (nth (/ card 12) cards-normal-suits))
)

(defun cards-list-to-string-pinochle (cardlist)
  "Turn an entire list of pinochle cards into strings."
  
  (let ((newlist '()))
    (setq newlist '())
    (while cardlist
      (setq newlist (cons (cards-to-string-normal (car cardlist)) newlist))
      (setq cardlist (cdr cardlist)))
    newlist)
)

(defun cards-deal (num)
  "deal NUM cards from the deck"

  (let ((cardlist '())
	(cnum nil))
    (setq cardlist '())
    (if cards-current-deck
	(progn
	  (while (not (= num 0))
	    (setq cnum (% (random) (length cards-current-deck)))
	    (if ( < cnum 0) (setq cnum (- 0 cnum)))
	    (setq cardlist (cons (nth cnum cards-current-deck) cardlist))
	    (if (= cnum 0)
		(setq cards-current-deck (cdr cards-current-deck))
	      (setcdr (nthcdr (- cnum 1) cards-current-deck)
		      (nthcdr (+ cnum 1) cards-current-deck)))
	    (setq num (- num 1)))
	  cardlist)
      (message "No more cards in the deck")
      nil))
)
	      
(defun cards-sort-by-face (cardlist)
  "This function will take a list of cards from any deck and sort them
based on number alone, ties between 9H and 9C will not be put in any
order based on suit.  (See also cards-sort-by-suit,
cards-sort-by-both)"

  (cond
   ((string= cards-deck-type "normal")
    (sort cardlist '(lambda (a b) (< (% a 13) (% b 13)))))
   ((string= cards-deck-type "pinochle")
    (sort cardlist '(lambda (a b) (< (% a 6) (% b 6))))))
)

(defun cards-sort-by-suit (cardlist)
  "This function will take a list of cards from any deck and sort them
based on suit alone, ties between 9D and 6D will not be put in
numerical order. (See also cards-sort-by-number, cards-sort-by-both)"

  (cond 
   ((string= cards-deck-type "normal")
    (sort cardlist '(lambda (a b) (< (/ a 13) (/ b 13)))))
   ((string= cards-deck-type "pinochle")
    (sort cardlist '(lambda (a b) (< (/ a 12) (/ b 12))))))
)

(defun cards-sort-by-both (cardlist)
  "This function will sort based on suit, then put the numbers of each
suit in order, so 9D and 6D will become 6D, 9D.  (See also
cards-sort-by-number, cards-sort-by-suit)"

  (sort cardlist '<)
)

(defun cards-display-hand (hand &optional which)
  "This displays cards in a hand to a buffer somewhere, and does not
return it since that would mean it would have to be saved and it isn't."

  (while hand
    (insert (format " %-3s" (if which
			      "X"
			    (cards-to-string-normal (car hand)))))
    (setq hand (cdr hand)))
  (insert "\n")
)

(defun cards-remove-from-list (card hand)
  "Remove the given card (a unique value) from the given hand.  Return
the resulting hand.  If the given hand does not contain the given
card, return the same hand."

  (cond 
   ;; see if it is a single card
   ((integerp card)
    (let ((index 0)
	  (temp hand))
      (while (and temp (not (= (car temp) card)))
	(setq temp (cdr temp))
	(setq index (+ index 1)))
      (if (< index (length hand))
	  (if (= index 0)
	      (setq hand (cdr hand))
	    (setcdr (nthcdr (- index 1) hand)
		    (nthcdr (+ index 1) hand))))))
   ;; otherwise remove a list from a list, with how stating how what
   ;; to compare: 't' - compare face values; 'nil' compare suits
   (t
    (let ((index 0))
      (while (and card (< index (length hand)))
	(if (= (car card) (nth index hand))
	    (progn
	      (if (= index 0)
		  (setq hand (cdr hand))
		(setcdr (nthcdr (- index 1) hand)
			(nthcdr (+ index 1) hand)))
	      (setq card (cdr card))
	      (setq index 0))
	  (setq index (1+ index)))))))
  ;; return the new hand to the calling routine
  hand
)

(defun cards-get-face (card)
  "Return the face value of the card to the calling routine."
  
  (cond
   ((string= cards-deck-type "normal")
    (concat "" (nth (% card 13) cards-normal-faces)))
   ((string= cards-deck-type "pinochle")
    (concat "" (nth (% card 6) cards-normal-faces))))
)

(defun cards-get-suit (card)
  "Return the suit of the current card to the calling routine."

  (cond
   ((string= cards-deck-type "normal")
    (nth (/ card 13) cards-normal-suits))
   ((string= cards-deck-type "pinochle")
    (nth (/ card 12) cards-normal-suits)))
)

(defun cards-hand-to-message (hand)
  "Turn the hand into something that can be passed as a message."
  
  (let ((newhand ""))
    (while hand
      (setq newhand (concat (char-to-string (+ (car hand) ?\ ))
			    newhand))
      (setq hand (cdr hand)))
    newhand)
)

(defun cards-message-to-hand (mess)
  "Turn a message passed over to a useable hand."

  (let ((newhand '()))
    (while (> (length mess) 0)
      (setq newhand (cons (- (string-to-char mess) ?\ ) newhand))
      (setq mess (substring mess 1 (length mess))))
    newhand)
)

(defun cards-match-by-face (card hand)
  "Return a list of cards in the hand that have the same face value as
the card given."

  (let ((final-list '()))
    (while hand
      (if (or (and (integerp card) 
		   (string= (cards-get-face (car hand)) (cards-get-face card)))
	      (and (stringp card)
		   (string= (cards-get-face (car hand)) card)))
	  (setq final-list (cons (car hand) final-list)))
      (setq hand (cdr hand)))
    final-list)
	 
)

(defun cards-match-all (card hand)
  "Return t if card is in the hand, nil if it is not."

  (let ((this (copy-sequence hand))
	(found nil))
    (while (and hand (not found))
      (if (= (car hand) card)
	  (setq found t)
	(setq hand (cdr hand))))
    found)
)

(defun cards-valid-card (card)
  "Validify that the given card is in the current deck."

  (let* ((new (upcase card))
	 (val (char-to-string (aref new 0)))
	 (suit (char-to-string (aref new (1- (length new))))))
    (cond
     ((string= cards-deck-type "normal")
      (and (or (string-match val "23456789JQKA")
	       (string= (substring 0 2 new) "10"))
	   (string-match val "HDSC")))
     ((string= cards-deck-type "pinochle")
      (if (string-match "\\([ajqkAJQK90][HDSChdsc]\\)" card)
	  t
	nil))))
)

(defun cards-string-to-card-normal (stringcard)
  "Turn a string into the correct card value."

  (let* ((str (upcase stringcard))
	 (val (aref str 0))
	 (val2 (char-to-string (aref str (1- (length str))))))
    (1- (+ (cond
	    ((and (>= val ?2)
		  (<= val ?9))
	     (- val ?0))
	    ((= val ?1)
	     10)
	    ((= val ?J)
	     11)
	    ((= val ?Q)
	     12)
	    ((= val ?K)
	     13)
	    ((= val ?A)
	     1))
	   
	   ;; Also, this order may have to be changed
	   ;; if the order is changed in the above list of suits.
	   (* 13 (string-match val2 "HCDS")))))
)

(defun cards-draw-card (x y card &optional blank)
  "Draw a card on the screen at position line x, column y. If blank is
't' then it will print a pretty back to the card."

  (save-excursion
    (let ((something y)
	  (co (if (eq blank t)
		  'cards-backing
		(if (or (= (/ card 13) 0) (= (/ card 13) 2))
		    'cards-reds 'cards-blacks)))
	  (game-lib-replace nil))	;turn of deletion in game lib
      (while (< 0 something)
	(end-of-line)
	(if (eobp)
	    (insert "\n")
	  (forward-line))
	(setq something (- something 1)))
      (end-of-line)

      (while (> x (current-column))
	(insert " "))
      (while (< x (current-column))
	(forward-char (- x (current-column))))
      (if (and (not (= (save-excursion (end-of-line) (current-column))
		  (current-column)))
	       (not (eobp)))
	  (delete-char 5))
      (game-lib-insert-string (point) "+---+" co)
      (end-of-line)

      (if (eobp)
	  (insert "\n"))
      (forward-line)
      (end-of-line)
      (while (> x (current-column))
	(insert " "))
      (while (< x (current-column))
	(forward-char (- x (current-column))))
      (if (and (not (= (save-excursion (end-of-line) (current-column))
		       (current-column)))
	       (not (eobp)))
	  (delete-char 5))
      (game-lib-insert-string 
       (point) (format "|%3s|" (cond
				((stringp blank) blank)
				((and (not (integerp blank)) blank) "///")
				((and (integerp blank) (= blank 1))
				 (cards-to-string-normal card))
				(t "   "))) co)
      (end-of-line)

      (if (eobp)
	  (insert "\n"))
      (forward-line)
      (end-of-line)
      (while (> x (current-column))
	(insert " "))
      (while (< x (current-column))
	(forward-char (- x (current-column))))
      (if (and (not (= (save-excursion (end-of-line) (current-column))
		       (current-column)))
	       (not (eobp)))
	  (delete-char 5))
      (game-lib-insert-string 
       (point) (format "|%3s|" (cond 
				((stringp blank) blank)
				((and (not (integerp blank)) blank) "///")
				((and (integerp blank) (= blank 2))
				 (cards-to-string-normal card))
				(t "   "))) co)
      (end-of-line)
      
      (if (eobp)
	  (insert "\n"))
      (forward-line)
      (end-of-line)
      (while (> x (current-column))
	(insert " "))
      (while (< x (current-column))
	(forward-char (- x (current-column))))
      (if (and (not (= (save-excursion (end-of-line) (current-column))
		       (current-column)))
	       (not (eobp)))
	  (delete-char 5))
      (game-lib-insert-string
       (point) (format "|%3s|" (cond 
				((stringp blank) blank)
				((and (not (integerp blank)) blank) "///")
				((and (integerp blank) (= blank 3))
				 (cards-to-string-normal card))
				(t "   "))) co)
      (end-of-line)
      
      (if (eobp)
	  (insert "\n"))
      (forward-line)
      (end-of-line)
      (while (> x (current-column))
	(insert " "))
      (while (< x (current-column))
	(forward-char (- x (current-column))))
      (if (and (not (= (save-excursion (end-of-line) (current-column))
		       (current-column)))
	       (not (eobp)))
	  (delete-char 5))
      (game-lib-insert-string (point) "+---+" co)))
  )

;;; end of lisp
(provide 'card-lib)
