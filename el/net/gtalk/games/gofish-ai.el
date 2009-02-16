;;;
;;; Copyright (C) 1992 1993 1994 Amy E. Ludlam
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
;;; $Id: gofish-ai.el,v 1.2 1995/11/17 23:07:45 zappo Exp $
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
;;;  Under tyrant-ai.  To run, load the library "tyrn-ai" and then use   ;;;
;;;  the function "tyrant-play-computer" and choose this game without    ;;;
;;;  the "-ai" suffix.  There must be file of matching name to run the   ;;;
;;;  interface.                                                          ;;;
;;;                                                                      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'gofish "games/gofish")

(defun gofish-ai (&optional skill)
  "something neat and new"

  (while (and (gofish-check-tyrant)
	      (= tyrant-turn 2)
	      gofish-hand2)

    (let ((card nil)
	  (num -1)
	  (guess ""))

      ;; find a singleton
      (let ((hand (copy-sequence gofish-hand2))
	    (blah '()))
	(while hand
	  (setq blah (cards-match-by-face (cards-get-face (car hand)) hand))
	  (if (> (length blah) 1)
	      (while blah 
		(setq hand (cards-remove-from-list (car blah) hand))
		(setq blah (cdr blah)))
	    (setq card (car blah))
	    (setq hand '()))))

      ;; if no singleton found, then take a random one
      (if (not card)
	  (progn
	    (setq num (random t))
	    (while (< num 0)
	      (setq num (random t)))
	    (setq num (% num (length gofish-hand2)))
	    (setq card (nth num gofish-hand2))))

      ;; now turn it into something useful
      (setq guess (cards-get-face card))
      (if (string= "10" guess)
	  (tyrant-ai-input-string "0")
	(tyrant-ai-input-string guess))))
)

;;; end of lisp
(provide 'gofish-ai)

