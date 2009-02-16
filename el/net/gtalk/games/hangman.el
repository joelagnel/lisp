;;; Hangman game for emacs.  Emacs talk required.
;;;
;;; Copyright (C) 1992 Eric M. Ludlam
;;; Copyright (C) 1994 Free Software Foundation
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
;;; $Id: hangman.el,v 1.2 1994/09/10 16:20:00 zappo Exp $
;;; History:
;;; <joe@opus.ohio-state.edu> 9/10/94
;;; Fixed startup routines trouble from 0.8.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                      ;;;
;;;                          TYRANT MODE GAME                            ;;;
;;;                                                                      ;;;
;;;  This program contains a program designed for use with               ;;;
;;; tyrant-mode.  This program may be used under the following           ;;;
;;; software conditions.                                                 ;;;
;;;                                                                      ;;;
;;;  Under tyrant-mode for TALK.  To run this way, use "etalk"           ;;;
;;;  and once a connection is established, use the game playing          ;;;
;;;  function "etalk-initiate-special-function" bound to C-c g           ;;;
;;;  to start.  Must be installed on all systems.                        ;;;
;;;                                                                      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'game-lib "games/game-lib")

(defvar hang-map nil
  "Keymap used in hangman.")
(if hang-map
    ()
  (let ((tl "a"))
    (setq hang-map (make-sparse-keymap))
    (while (not (equal tl "z"))
      (define-key hang-map tl 'hang-guess)
      (setq tl (char-to-string (+ 1 (string-to-char tl)))))
    (define-key hang-map tl 'hang-guess)	;get the "z" key
    ))

(defvar well-hung-parts
  '( (lambda () (goto-char 100) (delete-char 3) (insert "( )"))
     (lambda () (goto-char 123) (delete-char 1) (insert "|"))
     (lambda () (goto-char 145) (delete-char 1) (insert "|"))
     (lambda () (goto-char 122) (delete-char 1) (insert "/"))
     (lambda () (goto-char 124) (delete-char 1) (insert "\\"))
     (lambda () (goto-char 166) (delete-char 1) (insert "/"))
     (lambda () (goto-char 168) (delete-char 1) (insert "\\")))
  "Defines the locations where body-parts are added to the body.")

(defvar hangman-lost-body-parts nil
  "body parts lost so far")

(defvar hangman-guess-string nil
  "String we need to guess")

(defvar hangman-to-guess-string nil
  "String we need to guess")

(defvar hangman-guessed-string nil
  "String of characters we've guessed")

(defun hangman ()
  "Mode designed explicitly to be used over talk mode.  
Press a lowercase letter to make a guess.  Game ends when you figure
the word out, or when you run out of body parts."

  (interactive)
  (switch-to-buffer (set-buffer (get-buffer-create "HANGMAN")))
  (setq mode-name "Hangman")
  (setq major-mode 'hangman)
  (game-lib-clear-buffer)
  (game-lib-insert-string 1
			  "#####################
#                   #
#  ----------       #
#  | /      |       #
#  |/               #
#  |                #
#  |                #
#  |                #
#  |                #
# ================  #   -
#                   #
#####################

" nil)

  (delete-other-windows (selected-window))
  (use-local-map hang-map)
  (make-local-variable 'hangman-lost-body-parts)
  (setq hangman-lost-body-parts 0)
  (make-local-variable 'hangman-guess-string)
  (setq hangman-guess-string nil)
  (make-local-variable 'hangman-guessed-string)
  (setq hangman-guessed-string nil)
  (make-local-variable 'hangman-to-guess-string) ;nil for guesser (duh)
  (setq hangman-to-guess-string "                               ")

  ;; Ok now make the hooks!  In this game, initiator answers
  ;; questions, but can't type.
  
  (setq tyrant-player1-hook
	'(lambda ()
	   (while (> (length hangman-to-guess-string) 30)
	     (setq hangman-to-guess-string 
		   (read-string "Enter the word for remote to guess: "))
	     (message "Interpolating [%s]..." hangman-to-guess-string) )
	   (make-local-variable 'etalk-tyrant-brief-help)
	   (setq etalk-tyrant-brief-help
     "Hangman : C-c m to send a minibuffer message, otherwise just watch.")
	   (setq etalk-tyrant-enabled-console nil) ;player 1 don't
					;guess.
	   (hang-make-hangman-guess-string hangman-to-guess-string)
	   (goto-char 220)		;where the message should
					;start
	   (kill-line)
	   (let ((game-lib-replace nil))
	     (game-lib-insert-string (point)
				     (concat "    " hangman-guess-string)
				     nil))
	   (goto-char (point-max))
	   (insert "String to guess: " hangman-to-guess-string "\nGuessed chars: ")))

  (setq tyrant-player2-hook
	'(lambda ()
	   (setq etalk-tyrant-enabled-console nil)
	   (setq tyrant-call-interpreter 'hang-do-message)
	   (make-local-variable 'etalk-tyrant-brief-help)
	   (setq etalk-tyrant-brief-help
     "Hangman : Press any lower case letter to guess. C-c m to send message.")
	   )) ; hangman defun
)

(defun hang-make-hangman-guess-string (str)
  "make a guess string from a string"

  (let (( cnt 0))
    (setq hangman-guess-string "")
    (while (< cnt (length str))
      (if (and (<= (aref str cnt) ?z) (>= (aref str cnt) ?a))
	  (setq hangman-guess-string (concat hangman-guess-string "_ "))
	(setq hangman-guess-string (concat hangman-guess-string (char-to-string (aref str cnt))
					   " " )))
      (setq cnt (+ 1 cnt)))
    (tyrant-send-message hangman-guess-string)
    hangman-guess-string))

(defun hang-guess ()
  "Make a guess as to a letter in the string."
  (interactive)
  (goto-char (point-max))
  (if (and hangman-guessed-string
	   (string-match (char-to-string last-input-char)
			 hangman-guessed-string))
      (message "Already guessed %c!" last-input-char)
    (let ((game-lib-replace nil))
      (game-lib-insert-string (point) (char-to-string last-input-char)
			      'game-lib-player1-face))
    (setq hangman-guessed-string
	  (concat (or hangman-guessed-string "")
		  (char-to-string last-input-char))))
    (if (and (boundp etalk-tyrant-enabled-console)
	     etalk-tyrant-enabled-console)
	(message "Guessing %c ..." last-input-char)
      (if (string-match (format "\\(%s\\)" (char-to-string last-input-char))
			hangman-to-guess-string)
	  (let ((cnt 0))
	    (while (< cnt (length hangman-to-guess-string))
	      (if (equal (aref hangman-to-guess-string cnt) last-input-char)
		  (aset hangman-guess-string (* 2 cnt) last-input-char))
	      (setq cnt (+ cnt 1)))
	    (tyrant-send-message hangman-guess-string)
	    (save-excursion
	      (set-buffer (get-buffer "HANGMAN"))
	      (goto-char 220)			;where the message should
	      (kill-line)
	      (insert "    " hangman-guess-string))
	    (if (not (string-match "\\(_\\)" hangman-guess-string))
		(if (and (boundp 'etalk-tyrannical-mode)
			 etalk-tyrannical-mode)  
		    ;; usurp tyrant and send said info to remote
		    (progn 
		      (etalk-usurp-tyrant (tyrant-format "%U won!")))) )
	    )
	(tyrant-send-message "")		;you goofed.
	(hang-lost-bodypart)
      )))

(defun hang-do-message (mesg)
  "interpolate a message.. which basically means redo display to add a
head, or whatever, or reprint the guess string."

  (if (not hangman-guessed-string)
      (progn
	(goto-char (point-max))
	(insert "Chars guessed: ")
	(setq etalk-tyrant-enabled-console t)
	(message "Press any letter to guess"))
    (message "Guessing %c ... done" last-input-char))
  (if (equal 0 (length mesg))
      ;; Oh shit.  we lost a body part.
      (hang-lost-bodypart)
    (progn
      (setq hangman-guess-string mesg)
      (save-excursion
	(set-buffer (get-buffer "HANGMAN"))
	(goto-char 220)			;where the message should
					;start
	(kill-line)
	(let ((game-lib-replace nil))
	  (game-lib-insert-string (point)
				  (concat "    " hangman-guess-string)
				  nil)))
      (if (not (string-match "\\(_\\)" hangman-guess-string))
	  (if (and (boundp 'etalk-tyrannical-mode) etalk-tyrannical-mode)  
	      ;; usurp tyrant and send said info to remote
	      (progn
		(etalk-usurp-tyrant (tyrant-format "%U wins!")))))
	)))

(defun hang-lost-bodypart ()
  "Remove a bodypart"

  (if (> (length well-hung-parts) hangman-lost-body-parts)
      (progn
	(funcall (nth hangman-lost-body-parts well-hung-parts))
	(setq hangman-lost-body-parts (+ 1 hangman-lost-body-parts))
	)
    (message "Out of body parts!  Press key to quit...")
    (if (and (boundp 'etalk-tyrannical-mode) etalk-tyrannical-mode)  
	;; usurp tyrant and send said info to remote
	(etalk-usurp-tyrant "Out of body parts!")
      ;; otherwise zap the buffer
      (kill-buffer (current-buffer)))))

;;; end of lisp
(provide 'hangman)

