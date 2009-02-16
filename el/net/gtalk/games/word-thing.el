;;; word thing game interface (emacs talk optioanl)
;;;
;;; Copyright (C) 1994, 1999 Free Software Foundation
;;; Copyright (C) 1992 Eric M. Ludlam
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
;;; $Id: word-thing.el,v 1.3 1999/08/26 11:53:12 zappo Exp $
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

(defvar word-thing-map nil
  "Keymap used in playing Word-Thing")

(if word-thing-map
    ()
  (setq word-thing-map (make-sparse-keymap))
  (define-key word-thing-map "q" 'word-thing-quit)
  (define-key word-thing-map "." 'word-thing-select-tile)
  (define-key word-thing-map "," 'word-thing-select-tile)
  (define-key word-thing-map "<" 'word-thing-move-tile)
  (define-key word-thing-map ">" 'word-thing-move-tile)
  (define-key word-thing-map "s" 'word-thing-sort-hand)
  (define-key word-thing-map "" 'word-thing-move-word)
  (define-key word-thing-map "" 'word-thing-move-word)
  (define-key word-thing-map "" 'word-thing-move-word)
  (define-key word-thing-map "" 'word-thing-move-word)
  (define-key word-thing-map "f" 'word-thing-move-word)
  (define-key word-thing-map "b" 'word-thing-move-word)
  (define-key word-thing-map "p" 'word-thing-move-word)
  (define-key word-thing-map "n" 'word-thing-move-word)
  (define-key word-thing-map "t" 'word-thing-turn-word)
  (define-key word-thing-map "g" 'word-thing-swap-turns)
  (define-key word-thing-map "G" 'word-thing-get-new-hand)
  (define-key word-thing-map "\C-m" 'word-thing-put-word)
)

(defvar word-thing-board-map nil
  "Keymap for capturing stuff in the word thing board buffer.")

(if word-thing-board-map
    ()
  (setq word-thing-board-map (make-sparse-keymap)))
;;; This multi-buffer stuff is incompatible with tyrant mode.
;;;  (game-lib-add-mouse-support word-thing-board-map))

(defvar word-thing-command-name "WORD THING COMMAND"
  "Name of the word-thing command buffer")

(defvar word-thing-board-name "WORD THING BOARD"
  "Name of the word-thing command buffer")

(defvar word-thing-tiles '( ?A ?A ?A ?A ?A ?A ?B ?B ?B ?C ?C ?C ?D ?D ?D ?D
			     ?E ?E ?E ?E ?E ?E ?E ?E ?F ?F ?F ?G ?G ?G ?H
			     ?H ?H ?H ?I ?I ?I ?I ?I ?I ?I ?J ?J ?K ?K ?L
			     ?L ?M ?M ?M ?N ?N ?N ?O ?O ?O ?O ?O ?O ?P ?P
			     ?P ?Q ?R ?R ?R ?R ?S ?S ?S ?S ?S ?T ?T ?T
			     ?T ?U ?U ?U ?U ?U ?V ?U ?V ?W ?W ?X ?X ?Y ?Y
			     ?Z
			   )
  "The list of all the tiles available in word-thing.")
 
;;                        a b c d e f g h i j k l m n o p q r s t u v
(defvar word-thing-scores '(1 2 2 2 1 2 3 3 1 4 5 3 2 2 1 2 8 3 1 2 1 5
;;			  w x y z
			  6 7 7 8)
  "The scores associated with each letter.")

(defvar word-thing-2letter-words '(("AD") ("AH") ("AM") ("AN") ("AS")
				   ("AT") ("AX") ("BE") ("BI") ("BO")
				   ("BY") ("CD") ("DO") ("ED") ("FA") 
				   ("EH") ("GO") ("HA") ("HE") ("HI")
				   ("HO") ("ID") ("IF") ("IM") ("IN")
				   ("IQ") ("IS") ("IT") ("LA") ("LO")
				   ("LS") ("MA") ("ME") ("MI") ("MY")
				   ("NO") ("OF") ("OH") ("OK") ("ON")
				   ("OR") ("OS") ("PA") ("RE") ("SO")
				   ("TA") ("TI") ("TO") ("VI") ("PI")
				   ("UP") ("US") ("YA") ("EH") ("OX")
				   ("WE")) 
  "List of legal two letter words for word-thing since \"spell-string\"
seems to think all 2 letter words are legal.")

(defvar word-thing-pieces nil
  "pieces list during word-thing game")

(defvar word-thing-x nil
  "X position in word thing board.")

(defvar word-thing-y nil
  "Y position in word thing board.")

(defvar word-thing-direction nil
  "T means horizontal direction, nil means vertical direction for word
placement.")

(defvar word-thing-hand1 nil
  "Player 1s hand")

(defvar word-thing-hand2 nil
  "Player 2s hand")

(defvar word-thing-selected-tile-index nil
  "currently selected tile's index")

(defvar word-thing-player1-flag nil
  "Player 1 flag.  T means we send data to remote under tyrant.")

(defvar word-thing-player-1-score nil
  "Player 1s score")

(defvar word-thing-player-2-score nil
  "Player 2s score")

(defun word-thing ()
  "Word-Thing for tyrant mode:
',' '.'      : move cursor left or right from tile to tile.
 <   >       : Move tile left or right.
f C-f  b C-b : Move word outline forwards or backwards on board
p C-p  n C-n : Move word outline previous line or next line on board.
RET          : Test word agains dictionary and place.
t            : change orientation of word.
g            : Give up your turn.
G            : Give back your hand for a new one.
"
  
  (interactive)
  (switch-to-buffer (set-buffer (get-buffer-create word-thing-board-name)))
  (setq mode-name "Word-Thing")
  (use-local-map word-thing-board-map)
  (game-lib-clear-buffer)
  (game-lib-insert-string 1
":  :  :  :  :  :  :  :  :  :  :  :  :  :  :  :  +-------------------------+
:  :  :  :  :  :  :  :  :  :  :  :  :  :  :  :  | A  1      N  2          |
:  :  :  :  :  :  :  :  :  :  :  :  :  :  :  :  | B  2      O  1          |
:  :  :  :  :  :  :  :  :  :  :  :  :  :  :  :  | C  2      P  2          |
:  :  :  :  :  :  :  :  :  :  :  :  :  :  :  :  | D  2      Q  8          |
:  :  :  :  :  :  :  :  :  :  :  :  :  :  :  :  | E  1      R  3          |
:  :  :  :  :  :  :  :  :  :  :  :  :  :  :  :  | F  2      S  1          |
:  :  :  :  :  :  :  :**:  :  :  :  :  :  :  :  | G  3      T  2          |
:  :  :  :  :  :  :  :  :  :  :  :  :  :  :  :  | H  3      U  1          |
:  :  :  :  :  :  :  :  :  :  :  :  :  :  :  :  | I  1      V  5          |
:  :  :  :  :  :  :  :  :  :  :  :  :  :  :  :  | J  4      W  6          |
:  :  :  :  :  :  :  :  :  :  :  :  :  :  :  :  | K  5      X  7          |
:  :  :  :  :  :  :  :  :  :  :  :  :  :  :  :  | L  3      Y  7          |
:  :  :  :  :  :  :  :  :  :  :  :  :  :  :  :  | M  2      Z  8          |
:  :  :  :  :  :  :  :  :  :  :  :  :  :  :  :  +-------------------------+"
nil)
  (make-local-variable 'tyrant-mouse-function)
  (setq tyrant-mouse-function 'word-thing-mouse-support)
  (goto-char 1)
  (delete-other-windows (selected-window))
  (make-local-variable 'word-thing-x)
  (setq word-thing-x 0)
  (make-local-variable 'word-thing-y)
  (setq word-thing-y 0)
  (make-local-variable 'word-thing-direction)
  (setq word-thing-direction nil)			;t means verticle
  (split-window (selected-window) 16)
  
  (other-window 1)
  (switch-to-buffer (set-buffer (get-buffer-create word-thing-command-name)))
  (setq major-mode 'word-thing)
  (game-lib-clear-buffer)
  (use-local-map word-thing-map)
  (setq truncate-lines t)
  (make-local-variable 'tyrant-turn)
  (setq tyrant-turn 1)
  (make-local-variable 'word-thing-pieces)
  (setq word-thing-pieces (copy-sequence word-thing-tiles))
  (make-local-variable 'word-thing-hand1)
  (setq word-thing-hand1 (cons 0 (word-thing-get-tiles 7)))
  (make-local-variable 'word-thing-hand2)
  (setq word-thing-hand2 (cons 0 (word-thing-get-tiles 7)))
  (make-local-variable 'word-thing-selected-tile-index)
  (setq word-thing-selected-tile-index 0)
  (make-local-variable 'word-thing-player1-flag)
  (setq word-thing-player1-flag t)			;if 2 person one screen
					;alaways p1
  (make-local-variable 'word-thing-player-1-score)
  (setq word-thing-player-1-score 0)
  (make-local-variable 'word-thing-player-2-score)
  (setq word-thing-player-2-score 0)

  (make-local-variable 'tyrant-mouse-function)
  (setq tyrant-mouse-function 'word-thing-mouse-support)
  (make-local-variable 'etalk-tyrant-brief-help)
  (setq etalk-tyrant-brief-help
	"[,.]Tile Select [<>]Tile move [[C-]fbnp]Move word [t]urn [g] pass [G] get new.")
  (make-local-variable 'etalk-tyrant-quit-string)
  (fset 'etalk-tyrant-quit-string 'word-thing-win-string)
  (word-thing-display-tiles)
  (word-thing-cursor-on-tile)

  ;; ok.. setup hooks dependant on wether you are player1 or player2
  (setq tyrant-player1-hook 
	'(lambda ()
	    (message "You are player 1")
	    (save-window-excursion
	      (set-buffer word-thing-board-name)
	      (make-local-variable 'etalk-tag)
	      (setq etalk-tag t))
	    (sleep-for 1)
	    ;; placeing 3 messages in one string prevents interrupts
	    ;; during interpolation.
	    (tyrant-send-message 
	     (format "%c%s\n%c%s\nYou are Player 2!" 
		     5 (word-thing-word-thing-hand2string word-thing-hand1)
		     6 (word-thing-word-thing-hand2string word-thing-hand2)))
	    (word-thing-display-tiles)
	    (word-thing-cursor-on-tile)
	    (sleep-for 1)))

  (setq tyrant-player2-hook
	'(lambda ()
	   (setq word-thing-player1-flag nil)
	   (save-window-excursion
	     (set-buffer word-thing-board-name)
	     (make-local-variable 'etalk-tag)
	     (setq etalk-tag t))
	   (setq etalk-tyrant-enabled-console nil)
	   (setq tyrant-call-interpreter 'word-thing-player2-message-handler)
	   )) ;player2 goes 2nd

  (goto-char 1)
  (forward-char (+ (* 6 word-thing-selected-tile-index) 3)))

(defun word-thing-mouse-support(p e m)
  "Reads in a mouse event from the game-lib driver, and allows a
player to click on a square."

  (save-excursion
    (set-buffer word-thing-board-name)
    ;; find which square we are closest to and go there
    (if (and (not (memq 'drag m)) (integerp p))
	(let ((first-line 0)
	      (line-width 76)
	      (block-width 2)
	      (block-height 1)
	      (block-vsep 0)			;vertical separator width
	      (block-hsep 1)			;horizontal sep width
	      x y xt yt
	      )
	  (setq yt (/ (- p (* first-line line-width)) line-width))
	  (setq xt (- (% p line-width) 1))
	  (setq y (/ yt (+ block-height block-vsep)))
	  (setq x (/ xt (+ block-width block-hsep)))
	  
	  (if (> x 15)
	      (message "Mouse %d %d off board!" x y)

	    (if (not (or (< (% xt (+ block-width block-hsep)) block-hsep)
			 (< (% yt (+ block-height block-vsep))
			    block-vsep)))
		(progn
		  (word-thing-outline-word nil)
		  (setq word-thing-x x)
		  (setq word-thing-y y)
		  (word-thing-outline-word t)))))
      )
    )
  )

(defun word-thing-player1-message-handler (mess)
  "message handler for player 1.  A placeholder."
)

(defun word-thing-player2-message-handler (mess)
  "message handler for player 2.  Must receive messages informing of
new hands, word query's and things like that."
  ;; don't need to set buffer because filter does it for us.
  (cond
   ((equal (string-to-char mess) 5)
    (setq word-thing-hand1 (word-thing-string2hand (substring mess 1 (length mess))))
    (word-thing-display-tiles))
   ((equal (string-to-char mess) 6)
    (setq word-thing-hand2 (word-thing-string2hand (substring mess 1 (length mess))))
    (word-thing-display-tiles))
   ((equal (string-to-char mess) 7)
    (if (equal tyrant-turn 1)
	(progn
	  (setq word-thing-player-1-score (+ word-thing-player-1-score 
			  (string-to-int (substring mess 1 (length mess)))))
	  (message (tyrant-format
		    "%n: Words score [ %d ] Total score [ %d ]" 
		   (string-to-int (substring mess 1 (length mess)))
		   word-thing-player-1-score)))
      (setq word-thing-player-2-score (+ word-thing-player-2-score 
		      (string-to-int (substring mess 1 (length mess)))))
      (message (tyrant-format
		"%N: Words score [ %d ] Total score [ %d ]" 
	       (string-to-int (substring mess 1 (length mess)))
	       word-thing-player-2-score)))
    ;; mystery assignment
    ;; (setq etalk-tyrant-filter-message nil)
    (word-thing-display-tiles)
    (word-thing-cursor-on-tile))
   ((equal (string-to-char mess) 8)
    (if (equal (aref mess 1) ?n)
	(progn
	  ;;(setq etalk-tyrant-filter-message nil)
	  (error (substring mess 2 (length mess)))))
    (message mess)
    ;;(setq etalk-tyrant-filter-message nil)
    (word-thing-put-word t))
   ((equal (string-to-char mess) 9)
    (save-excursion
      (set-buffer word-thing-command-name)
      (setq mode-name (format "W-T(%d)" (- (aref mess 1) ?A)))))
   (t ;; anything else is just a message!
    (message mess)
   )))

(defun word-thing-string2hand (hand)
  "Take a string and turn it into a hand."

  (let ((newhand '()))
    (while (> (length hand) 0)
      (setq newhand (cons (string-to-char hand) newhand))
      (setq hand (substring hand 1 (length hand))))
    ;; append 0 to beginning since this function only called for new
    ;; hand which will only be done when 0 is first anyways.
    (cons 0 newhand)))

(defun word-thing-word-thing-hand2string (h2shand)
  "Turn a hand into a string to be decoded by prev function."

  (let ((scrab-newstr ""))
    (while h2shand
      (if (not (equal (car h2shand) 0))
	  (setq scrab-newstr (concat (char-to-string (car h2shand)) 
				     scrab-newstr)))
      (setq h2shand (cdr h2shand)))
    scrab-newstr))

(defun word-thing-win-string ()
  "tyrant only function, generates a string delairing someone the winner"
  (save-excursion
    (set-buffer word-thing-command-name)
    (if (> word-thing-player-1-score word-thing-player-2-score)
	(tyrant-format "%n is the winner!")
      (tyrant-format "%N is the winner!"))))

(defun word-thing-put-word (&optional valid)
  "Put the current word from current hand onto the board."

  (interactive)
  ;; if valid on, then skip, else, remote will eventually send player
  ;; 2 a valid or invalid message
  (if (not valid)
      (if word-thing-player1-flag 
	  (word-thing-validify-word))
    (message "Spelling ..."))

  (if (or word-thing-player1-flag valid)
      (save-excursion
	(set-buffer word-thing-board-name)
	(let ((spw-list (save-excursion
			  (set-buffer word-thing-command-name)
			  (cond ((equal tyrant-turn 1) word-thing-hand1) 
				((equal tyrant-turn 2) word-thing-hand2))))
	      (tx word-thing-x) (ty word-thing-y))
	  (word-thing-outline-word nil)
	  (if (not (word-thing-word-fit 
		    (word-thing-word-len spw-list) word-thing-x word-thing-y word-thing-direction))
	      (error "Can't put word there!")
	    (while (not (equal (car spw-list) 0))
	      (goto-char (+ 1 (word-thing-xy2index tx ty)))
	      (if (not (word-thing-board-letter tx ty))
		  (progn
		    (delete-char 2)
		    (insert " " (char-to-string (car spw-list)))
		    (setq spw-list (cdr spw-list))))
	      (if word-thing-direction
		  (setq tx (+ tx 1))
		(setq ty (+ ty 1)))))
	  (save-excursion
	    (set-buffer word-thing-command-name)
	    (cond 
	     ((equal tyrant-turn 1)
	      (setq word-thing-hand1 
		    (append spw-list (word-thing-get-tiles 
				      (- 8 (length spw-list))))))
	     ((equal tyrant-turn 2) 
	      (setq word-thing-hand2 
		    (append spw-list (word-thing-get-tiles 
				      (- 8 (length spw-list)))))))
	    (if (and (and (boundp 'etalk-tyrannical-mode) etalk-tyrannical-mode)
		     word-thing-player1-flag)
		(progn
		  (tyrant-send-message 
		   (format "%c%s\n%c%s" 
			   5 (word-thing-word-thing-hand2string word-thing-hand1)
			   6 (word-thing-word-thing-hand2string word-thing-hand2)))))))
	(word-thing-swap-turns))))

(defun word-thing-validify-word ()
  "check to see if a word is ok via the following things:
 *  all words at least 3 letters long
 *  attaches to another word or on the ** square
 *  is a valid word
 *  doesn't make neighboring invalid words"

  (interactive)
  (let ((testhand (save-excursion
		    (set-buffer word-thing-command-name)
		    (cond ((equal tyrant-turn 1) word-thing-hand1) ((equal tyrant-turn 2) word-thing-hand2))))
	(words '()) 
	(scorehand 0))
    (save-excursion
      (set-buffer word-thing-board-name)
      (if (not (word-thing-word-fit (word-thing-word-len testhand) 
				  word-thing-x word-thing-y word-thing-direction))
	  (word-thing-tyrant-error "That word doesn't fit on the board!"))
      (if (= 0 (car testhand))
	  (word-thing-tyrant-error "Get a life.. I mean word.."))
      (message "Examining potential layout.")
      (setq words (word-thing-word-list testhand))
      (if (and (equal (length words) 1) 
	       (equal (length (car words)) (word-thing-word-len testhand)))
	  (if (not (word-thing-on-star))
	      (word-thing-tyrant-error 
	       "You are not overlapping a word, or the *.")))
      (while words
	(message "Spelling %s ..." (car words))
	(if (< (length (car words)) 2)
	    (word-thing-tyrant-error 
	     (format "%s is too short a word!  Must be 2 or more characters."
		     (car words))))
	(if (not (word-thing-spell (car words)))
	    (word-thing-tyrant-error 
	     (format "Spelling %s ... Invalid!" (car words)))
	  (message "Spelling %s ... Ok" (car words)))
	(setq scorehand (+ scorehand (word-thing-word-worth (car words))))
	(setq words (cdr words))))
    (save-excursion
      (set-buffer word-thing-command-name)
      (if (equal tyrant-turn 1)
	  (setq word-thing-player-1-score (+ word-thing-player-1-score scorehand))
	(setq word-thing-player-2-score (+ word-thing-player-2-score scorehand)))
      (message "Words score [ %d ] Total score [ %d ]" scorehand
	       (if (equal tyrant-turn 1) word-thing-player-1-score word-thing-player-2-score))
    ;; if tyrant mode, send results
      (if (not (and (boundp 'etalk-tyrannical-mode) etalk-tyrannical-mode))
	  ()
	(tyrant-send-message (format "%d\nOk" scorehand))))))

(defun word-thing-spell (test)
  "Check to see if TEST is a valid word.  Use a special two letter word
checker, or either `spell-string' or some ispell code to accomplish this."

  (if (= (length test) 2)
      ;; check to make sure it's in 2 letter word list
      (if (assoc test word-thing-2letter-words)
	  t
	nil)
    ;; otherwise do normal check.
    (if (file-exists-p "/usr/dict/words")
	;; using spell-string
	(if (string-match "incorrect" (spell-string test))
	    nil
	  t)
      ;; no spell string, try ispell.  If it errors out, tough noogies
      (require 'ispell)
      ;; The following bit if ispellery is out of the ispell.el file
      ;; which comes with emacs 19.34
      (ispell-accept-buffer-local-defs)	; use the correct dictionary
      (process-send-string ispell-process "%\n") ;put in verbose mode
      (process-send-string ispell-process (concat "^" test "\n"))
      ;; wait until ispell has processed word
      (while (progn
	       (accept-process-output ispell-process)
	       (not (string= "" (car ispell-filter)))))
      ;;(process-send-string ispell-process "!\n") ;back to terse mode.
      (setq ispell-filter (cdr ispell-filter))
      (if (listp ispell-filter)
	  (setq poss (ispell-parse-output (car ispell-filter))))
      (or (eq poss t) (stringp poss)))))
     

(defun word-thing-on-star ()
  "Return * if the current word overlaps the * symbol on the board,
nil otherwise."

  (save-excursion
    (set-buffer word-thing-board-name)
    (let ((soshand (save-excursion
		     (set-buffer word-thing-command-name)
		     (cond ((equal tyrant-turn 1) word-thing-hand1) ((equal tyrant-turn 2) word-thing-hand2))))
	  (sosx word-thing-x) (sosy word-thing-y))
      (setq soshand (cdr soshand)) ;; mathamatical fix
      (while (and (not (equal (save-excursion
				(goto-char (+ (word-thing-xy2index sosx sosy) 2))
				(following-char))
			      ?*))
		  (not (equal (car soshand) 0)))
	(if word-thing-direction (setq sosx (+ sosx 1)) (setq sosy (+ sosy 1)))
	(setq soshand (cdr soshand)))
      (if (equal (save-excursion
		   (goto-char (+ (word-thing-xy2index sosx sosy) 2))
		   (following-char))
		 ?*)
	  ?*
	nil))))

(defun word-thing-word-worth (word)
  "how much is a word worth???"

  (let ((sww-val 0))
    (while (not (equal (length word) 0))
      (setq sww-val (+ sww-val
		       (nth (- (string-to-char word) ?A)
			    word-thing-scores)))
      (setq word (substring word 1 (length word))))
    sww-val))

(defun word-thing-word-list (hand)
  "If default buffer is WORD THING BOARD then take current hand, and
return all subwords (main, sideways etc) created by it in a list"

  ;; look at main word for start
  (let* ((reslist  (cons (word-thing-find-word hand hand) '()))
	 ;; now blip down the list and find the rest
	 (swlx word-thing-x) (swly word-thing-y)
	 (hnd hand)
	 (wl (word-thing-word-len hnd))
	 (wrd nil))
    (while (not (equal wl 0))
      (if (not (word-thing-board-letter swlx swly))
	  (progn
	    (if (setq wrd (word-thing-find-word (car hnd) hand swlx swly))
		(setq reslist (cons wrd reslist)))
	    (setq hnd (cdr hnd))
	    (setq wl (- wl 1))))
      (if word-thing-direction (setq swlx (+ swlx 1)) (setq swly (+ swly 1))))
    reslist))

(defun word-thing-find-word (hand-or-char hand &optional tx ty)
  "if given a hand, find the whole word it builds, if given a char,
put the char at x y and see if word > 1, and return if true."

  (let* ((teststr "")
	 (l hand)
	 (x (if (and tx ty)
		tx word-thing-x))
	 (y (if (and tx ty)
		ty word-thing-y))
	 (tc 0) (ll 0)
	 (teststr nil)
	 (c nil))
    ;; work backwards from the character
    (if (and tx ty)
	(if (not word-thing-direction) (setq x (- x 1)) (setq y (- y 1)))
      (if word-thing-direction (setq x (- x 1)) (setq y (- y 1))))
    (while (setq tc (word-thing-board-letter x y))
      (setq teststr (concat (char-to-string tc) teststr))
      (if (and tx ty)
	  (if (not word-thing-direction) (setq x (- x 1)) (setq y (- y 1)))
	(if word-thing-direction (setq x (- x 1)) (setq y (- y 1)))))
    ;; now take the character and work away from the space
    (if (and tx ty)
	(progn
	  (setq x tx) (setq y ty))
      (progn
	(setq x word-thing-x) (setq y word-thing-y)))
    (if (and tx ty)
	(setq ll 1)
      (setq ll (word-thing-word-len hand)))
    (while (or (not (equal ll 0))  ;; checking has been done for end of
	       (word-thing-board-letter x y));; board
      (if (setq tc (word-thing-board-letter x y))
	  (setq teststr (concat teststr (char-to-string tc)))
	(setq teststr (concat teststr 
			      (if (and tx ty)
				  (progn
				    (setq ll 0)
				    (char-to-string hand-or-char))
				(setq c (car hand-or-char))
				(setq hand-or-char (cdr hand-or-char))
				(setq ll (- ll 1))
				(char-to-string c)))))
      (if (and tx ty)
	  (if (not word-thing-direction) (setq x (+ x 1)) (setq y (+ y 1)))
	(if word-thing-direction (setq x (+ x 1)) (setq y (+ y 1)))))
    (if (equal (length teststr) 1)
	nil
      teststr)))

(defun word-thing-move-word ()
  "Depending on the key pressed, move a tile."

  (interactive)
  (save-excursion
    (set-buffer  word-thing-board-name)
    (let ((curhand (save-excursion
		     (set-buffer word-thing-command-name)
		     (cond ((equal tyrant-turn 1) word-thing-hand1) ((equal tyrant-turn 2) word-thing-hand2)))))

      (word-thing-outline-word nil)
      (cond
       ((or (equal last-input-char ?f)
	    (equal last-input-char ?))
	(if (or
	     (and (equal (car curhand) 0) (< word-thing-x 14))
	     (word-thing-word-fit (word-thing-word-len curhand) 
				(+ 1 word-thing-x) word-thing-y word-thing-direction))
	    (setq word-thing-x (+ 1 word-thing-x))
	  (error "You are on the edge of the board.")))
       ((or (equal last-input-char ?b)
	    (equal last-input-char ?))
	(if (< word-thing-x 1)
	    (error "You are on the edge of the board.")
	  (setq word-thing-x (- word-thing-x 1))))
       ((or (equal last-input-char ?p)
	    (equal last-input-char ?))
	(if (< word-thing-y 1)
	    (error "You are on the edge of the board.")
	  (setq word-thing-y (- word-thing-y 1))))
       ((or (equal last-input-char ?n)
	    (equal last-input-char ?))
	(if (or
	     (and (equal (car curhand) 0) (< word-thing-y 14))
	     (word-thing-word-fit (word-thing-word-len curhand) 
				word-thing-x (+ 1 word-thing-y) word-thing-direction))
	    (setq word-thing-y (+ word-thing-y 1))
	  (error "You are on the edge of the board.")))
       (t
	(error "Bad key %c" last-input-char)))
      (word-thing-outline-word t))
    (word-thing-display-tiles))
  (word-thing-cursor-on-tile))

(defun word-thing-turn-word ()
  "Make the direction of the word lay change"

  (interactive)
  (word-thing-outline-word nil)
  (save-excursion
    (set-buffer word-thing-board-name)
    (let ((curhand (save-excursion
		    (set-buffer word-thing-command-name)
		    (cond ((equal tyrant-turn 1) word-thing-hand1) ((equal tyrant-turn 2) word-thing-hand2)))))
      (if (or
	   (equal (car curhand) 0)
	   (word-thing-word-fit (word-thing-word-len curhand) 
			      word-thing-x word-thing-y (not word-thing-direction)))
	  (setq word-thing-direction (not word-thing-direction))
	(error "Can't turn word there!")))
    (word-thing-outline-word t)
    (word-thing-display-tiles))
  (word-thing-cursor-on-tile))

(defun word-thing-outline-word (on-off)
  "Place outline marks on where the current word would go on the board."

  (save-excursion
    (set-buffer word-thing-board-name)
    (let ((list (save-excursion
		  (set-buffer word-thing-command-name)
		  (cond ((equal tyrant-turn 1) word-thing-hand1) ((equal tyrant-turn 2) word-thing-hand2))))
	  (l ":")
	  (tx word-thing-x) (ty word-thing-y)
	  (c nil))
      (if on-off
	  (progn
	    (if (equal (car list) 0)
		(setq l "|")
	      (setq l "#"))
	    (setq c 'game-lib-player1-face)))
      (if (equal (car list) 0)
	  (progn
	    (game-lib-insert-string (word-thing-xy2index tx ty) l c)
	    (game-lib-insert-string (+ (word-thing-xy2index tx ty) 3) l c)))
      (while (not (equal (car list) 0))
	(if (or (> tx 14) (> ty 14))
	    (message "Word will not fit there...")
	  (game-lib-insert-string (word-thing-xy2index tx ty) l c)
	  (game-lib-insert-string (+ (word-thing-xy2index tx ty) 3) l c))
	(if (not (word-thing-board-letter tx ty))
	    (setq list (cdr list)))	;only go to next if there is a
					;letter there
	(if word-thing-direction
	    (setq tx (+ tx 1))
	  (setq ty (+ ty 1)))))))

(defun word-thing-word-fit (wlen x y dir)
  "check to see if a word of length wlen fits on the board at position X Y
qin the word-thing-direction dir.  Returns # of total tiles in length if fit, or
nil if not fit."
  
  (let ((flag 0))
    (if (or (< x 0) (> x 14) (< y 0) (> y 14))
	(setq flag nil)
      (while (and (not (input-pending-p)) 
		  (not (equal wlen 0)))
	(if (not (word-thing-board-letter x y))
	    (setq wlen (- wlen 1)))
	(if dir
	    (setq x (+ x 1))
	  (setq y (+ y 1)))
	(setq flag (+ flag 1))
	(if (or (> x 15) (> y 15))
	    (progn (setq wlen 0)
		   (setq flag nil)))))
    flag))

(defun word-thing-word-len (hand)
  "Return the length of a hand (to end of word marker)"
  
  (let ((cnt 0))
    (while (not (equal (car hand) 0))
      (setq cnt (+ 1 cnt))
      (setq hand (cdr hand)))
    cnt))

(defun word-thing-xy2index (x y)
  "return index from X Y position"
  
  (+ 1 (* y 76) (* x 3)))

(defun word-thing-select-tile ()
  "Depending on the key pressed, select a tile."

  (interactive)
  (let ((curhand (cond ((equal tyrant-turn 1) word-thing-hand1) 
		       ((equal tyrant-turn 2) word-thing-hand2)))
	(charnum nil)
	(curnum nil))
    (cond
     ((equal last-input-char ?\,)
      (if (equal 0 word-thing-selected-tile-index)
	  (error "You are on the first tile.")
	(setq word-thing-selected-tile-index (- word-thing-selected-tile-index 1))))
     ((equal last-input-char ?\.)
      (if (equal (- (length curhand) 1) word-thing-selected-tile-index)
	  (error "You are on the last tile.")
	(setq word-thing-selected-tile-index (+ word-thing-selected-tile-index 1))))
     ((and (> 0 (setq charnum (- ?0 last-input-char))) (< 9 charnum))
      (if (< (length curhand) charnum)
	  (error "Don't have that many tiles!")
	(setq word-thing-selected-tile-index curnum)))
     (t
      (error "Bad key sequence")))
    (word-thing-cursor-on-tile)))

(defun word-thing-index2offset (ind)
  "When given a tile index, returns the # of tiles in between."
  
  (save-excursion
    (set-buffer word-thing-board-name)
    (let ((offs 0))
      (if word-thing-direction
	  (while (> ind 0)
	    (if (not (word-thing-board-letter (+ word-thing-x offs) word-thing-y))
		(setq ind (- ind 1)))
	    (setq offs (+ 1 offs)))
	(while (> ind 0)
	  (if (not (word-thing-board-letter word-thing-x (+ word-thing-y offs)))
	      (setq ind (- ind 1)))
	  (setq offs (+ 1 offs))))
      (if word-thing-direction
	  (while (word-thing-board-letter (+ word-thing-x offs) word-thing-y)
	    (setq offs (+ offs 1)))
	(while (word-thing-board-letter word-thing-x (+ word-thing-y offs))
	  (setq offs (+ offs 1))))
      offs)))

(defun word-thing-cursor-on-tile ()
  "Place the cursor on a specific tile in the command window."

  (goto-char 1)
  (forward-line 2)
  (forward-char (+ (* 6 (word-thing-index2offset word-thing-selected-tile-index)) 3)))

(defun word-thing-sort-hand ()
  "Sort the letters in the hand"

  (interactive)
  (word-thing-outline-word nil)
  (cond 
   ((equal tyrant-turn 1) 
    (setq word-thing-hand1 (sort word-thing-hand1 '(lambda (a b) (< a b)))))
   ((equal tyrant-turn 2) 
    (setq word-thing-hand2 (sort word-thing-hand2 '(lambda (a b) (< a b))))))
  (word-thing-display-tiles)
  (word-thing-cursor-on-tile))

(defun word-thing-get-new-hand ()
  "Return your hand to the tile pool and recieve a new batch."
  (interactive)
  (if word-thing-player1-flag
      (let ((curhand (cond ((equal tyrant-turn 1) word-thing-hand1) ((equal tyrant-turn 2) word-thing-hand2))))
	(while curhand
	  (if (= (car curhand) 0)
	      ()
	    (setq word-thing-pieces (cons (car curhand) word-thing-pieces)))
	  (setq curhand (cdr curhand)))
	(if (= tyrant-turn 1)
	    (setq word-thing-hand1 (cons 0 (word-thing-get-tiles 7)))
	  (setq word-thing-hand2 (cons 0 (word-thing-get-tiles 7))))
	(if (and (boundp 'etalk-tyrannical-mode) etalk-tyrannical-mode)
	    (progn
	      (tyrant-send-message 
	       (format "%c%s\n%c%s" 
		       5 (word-thing-word-thing-hand2string word-thing-hand1)
		       6 (word-thing-word-thing-hand2string word-thing-hand2)))))))
  (word-thing-swap-turns))

(defun word-thing-move-tile ()
  "Depending on the key pressed, move a tile."

  (interactive)
  (let ((curhand (cond ((equal tyrant-turn 1) word-thing-hand1) ((equal tyrant-turn 2) word-thing-hand2))))
    (word-thing-outline-word nil)
    (cond
     ((equal last-input-char ?\<)
      (if (equal 0 word-thing-selected-tile-index)
	  (error "You are on the first tile.")
	(word-thing-tile-swap curhand word-thing-selected-tile-index 
			    (- word-thing-selected-tile-index 1))
	(setq word-thing-selected-tile-index (- word-thing-selected-tile-index 1))))
     ((equal last-input-char ?\>)
      (if (equal (- (length curhand) 1) word-thing-selected-tile-index)
	  (error "You are on the last tile.")
	(word-thing-tile-swap curhand word-thing-selected-tile-index
			    (+ 1 word-thing-selected-tile-index))
	(setq word-thing-selected-tile-index (+ word-thing-selected-tile-index 1))))
     (t
      (error "Bad key %c" last-input-char)))
    (word-thing-display-tiles)
    (word-thing-cursor-on-tile)))

(defun word-thing-tile-swap (list ind1 ind2)
  "Swap two elements in a list"
  
  (let ((letter (nth ind1 list)))
    (setcar (nthcdr ind1 list) (nth ind2 list))
    (setcar (nthcdr ind2 list) letter)))

(defun word-thing-display-tiles (&optional tile-list)
  "Print the tiles into the word-thing buffer"

  (save-excursion
    (set-buffer word-thing-board-name)
    (let ((mx word-thing-x) (my word-thing-y) (dir word-thing-direction))
      (save-excursion
	(set-buffer word-thing-command-name)
	(if (not tile-list)
	    (setq tile-list (cond
			     ((equal tyrant-turn 1) word-thing-hand1)
			     ((equal tyrant-turn 2) word-thing-hand2))))
	(game-lib-clear-buffer)
	(insert"\n\n\n\n\n")
	(while tile-list
	  (if (word-thing-board-letter mx my)
	      (progn
		(goto-char 1)    (end-of-line) (insert " . . .") 
		(forward-char 1) (end-of-line) (insert " .   .")
		(forward-char 1) (end-of-line) 
		(insert " . " )
		(let ((game-lib-replace nil))
		  (game-lib-insert-string (point)
					  (char-to-string 
					   (word-thing-board-letter mx my))
					  'game-lib-player1-face))
		(insert " .")
		(forward-char 1) (end-of-line) (insert " .   .")
		(forward-char 1) (end-of-line) (insert " . . ."))
	    (goto-char 1)    (end-of-line) (insert " +---+") 
	    (forward-char 1) (end-of-line) 
	    (if (equal (car tile-list) 0)
		(let ((game-lib-replace nil))
		  (insert " |")
		  (game-lib-insert-string (point) "eow" 'game-lib-player1-face)
		  (insert "|"))
	      (insert " |   |"))
	    (forward-char 1) (end-of-line) 
	    (insert " | ") 
	    (let ((game-lib-replace nil))
	      (game-lib-insert-string 
	       (point)
	       (if (equal (car tile-list) 0) 
		   " " 
		 (if (and
		      (and (boundp 'etalk-tyrannical-mode)
			   etalk-tyrannical-mode)
		      (and (boundp 'etalk-tyrant-enabled-console)
			   (not etalk-tyrant-enabled-console)))
		     "?"
		   (char-to-string (car tile-list))))
	       (if (and
		    (and (boundp 'etalk-tyrannical-mode)
			 etalk-tyrannical-mode)
		    (and (boundp 'etalk-tyrant-enabled-console)
			 (not etalk-tyrant-enabled-console)))
		   'game-lib-player1-face
		 'game-lib-player2-face)))
	    (insert " |")
	    (forward-char 1) (end-of-line) (insert " |   |")
	    (forward-char 1) (end-of-line) (insert " +---+")
	    (setq tile-list (cdr tile-list)))
	  (if dir
	      (setq mx (+ mx 1))
	    (setq my (+ my 1))))
	(word-thing-outline-word t)
	(goto-char (point-max))
	(if (and (boundp 'etalk-tyrant-enabled-console) 
		 (not etalk-tyrant-enabled-console))
	    (insert "Your letters -> " (word-thing-word-thing-hand2word 
					(if (equal tyrant-turn 1) word-thing-hand2 word-thing-hand1))
		    "<-")
	  (insert "Word is now -> " (word-thing-word-thing-hand2word) " <-"))
	(insert
	 (if (and (boundp 'etalk-tyrannical-mode) etalk-tyrannical-mode)
	     (tyrant-format "Score %u [%d] %U [%d]"
			    word-thing-player-1-score word-thing-player-2-score)
	   (format
	    (if (equal tyrant-turn 1)
		"Score You [%d] Them [%d]"
	      "Score Them [%d] You [%d]")
	   word-thing-player-1-score word-thing-player-2-score)))))))

(defun word-thing-board-letter (x y)
  "Get the letter associated with position X Y"

  (save-excursion
    (set-buffer word-thing-board-name)
    (if (or (> x 14) (> y 14) (< x 0) (< y 0))
	nil
      (goto-char (+ (word-thing-xy2index x y) 2))
      (if (or (equal (following-char) ?2)
	      (equal (following-char) ?3)
	      (equal (following-char) ?*)
	      (equal (following-char) ?\ ))
	  nil
	(following-char)))))

(defun word-thing-word-thing-hand2word (&optional hand)
  "Turn someones hand into a string."
  
  (save-excursion
    (set-buffer word-thing-command-name)
    (let* ((thand (if (not hand)
		      (cond ((equal tyrant-turn 1) word-thing-hand1) ((equal tyrant-turn 2) word-thing-hand2))
		    hand))
	   (str ""))
      (while thand
	(setq str (concat str (if (equal (car thand) 0)
				  " " (char-to-string (car thand)))))
	(setq thand (cdr thand)))
      str)))

(defun word-thing-get-tiles (num)
  "find NUM tiles and return them"
  
  (let ((nlist '())
	(rnum 0))
    (while (not (equal num 0))
      (if (not word-thing-pieces)
	  (progn
	    (message "Out of tiles!")
	    (setq num 0))
	(setq rnum (% (random t) (length word-thing-pieces)))
	(if ( < rnum 0) (setq rnum (- 0 rnum)))
	(setq nlist (cons (nth rnum word-thing-pieces) nlist))
	(if (equal rnum 0)
	    (setq word-thing-pieces (cdr word-thing-pieces))
	  (setcdr (nthcdr (- rnum 1) word-thing-pieces)
		  (nthcdr (+ rnum 1) word-thing-pieces)))
	(setq num (- num 1))))
    (save-excursion
      (set-buffer word-thing-command-name)
      (if (and (and (boundp 'etalk-tyrannical-mode) etalk-tyrannical-mode)
	       word-thing-player1-flag)
	  (tyrant-send-message (format "%c%c" 9 
				       (+ (length word-thing-pieces) ?A))))
      (setq mode-name (format "W-T(%d)" (length word-thing-pieces))))
    nlist))
      
(defun word-thing-swap-turns ()

  (interactive)
  (set-buffer word-thing-command-name)
  (game-lib-swap-turns "It is now %P's turn." "Player %d's move.")
  (word-thing-display-tiles)
  (word-thing-cursor-on-tile))

(defun word-thing-tyrant-error (mess)
  "run error in minibuffer, and send error message to remote as well."

  (save-excursion
    (set-buffer word-thing-command-name)
    (if (and (boundp 'etalk-tyrannical-mode) etalk-tyrannical-mode)
	(progn
	  (tyrant-send-message (format "%c%c%s" 8 ?n mess))
	  (sleep-for 1))))
  (error mess))

(defun word-thing-quit ()
  "quit word-thing"
  
  (interactive)
  (delete-window (get-buffer-window word-thing-board-name))
  (kill-buffer word-thing-board-name)
  (game-lib-quit t))

;;; end of lisp

(provide 'word-thing)

