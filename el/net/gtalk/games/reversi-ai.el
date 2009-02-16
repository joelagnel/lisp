;;; reversi-ai.el - Play reversi wi
;;;
;;; Copyright (C) 1995 Eric M. Ludlam
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
;;; $Id: reversi-ai.el,v 1.1 1995/11/17 23:08:26 zappo Exp $
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

(require 'reversi "games/reversi")

(defun reversi-ai (&optional skill)
  "AI function, which, when called within a tyranted reversi game,
will calculate a good move, and then perform it on the board."

  (let ((best (make-vector 3 -1)))
    ;; First thing first, create a list of moves, and thier score
    (let ((tx 0))
      (while (< tx 8)
	(let ((ty 0))
	  (while (< ty 8)
	    (let* ((reversi-x tx) (reversi-y ty)
		   (score
		    (cond 
		     ;; first, we can't go in used spots
		     ((/= (reversi-owned tx ty) 0)
		      -2)
		     ;; lastly, calculate how good it is
		     (t
		      (+ (reversi-check-dir 0  1 nil t)
			 (reversi-check-dir 0 -1 nil t)
			 (reversi-check-dir 1  0 nil t)
			 (reversi-check-dir -1 0 nil t)
			 (reversi-check-dir 1  1 nil t)
			 (reversi-check-dir -1 -1 nil t)
			 (reversi-check-dir 1  -1 nil t)
			 (reversi-check-dir -1 1 nil t))))))
	      ;; Adjust the score based on good and bad positions
	      (setq score
		    (cond
		     ;; check for the corners .. very good to have
		     ((and (> score 0)
			   (or (and (= tx 0) (= ty 0))
			       (and (= tx 0) (= ty 7))
			       (and (= tx 7) (= ty 0))
			       (and (= tx 7) (= ty 7))))
		      64)
		     ;; avoid the squares near a corner .. very bad
		     ((and (> score 0)
			   (or (and (= tx 0) (= ty 1))
			       (and (= tx 0) (= ty 6))
			       (and (= tx 1) (= ty 0))
			       (and (= tx 1) (= ty 1))
			       (and (= tx 1) (= ty 6))
			       (and (= tx 1) (= ty 7))
			       (and (= tx 6) (= ty 0))
			       (and (= tx 6) (= ty 1))
			       (and (= tx 6) (= ty 6))
			       (and (= tx 6) (= ty 7))
			       (and (= tx 7) (= ty 1))
			       (and (= tx 7) (= ty 6))))
		      1)
		     ;; increment score because a score of 1 (above)
		     ;; indicates a legal, but very undesireable move
		     ((> score 0)
		      (1+ score))
		     (t
		      0)))
	      ;; is this the best so far?
	      (if (<= (aref best 0) score)
		  (progn
		    (aset best 0 score)
		    (aset best 1 tx)
		    (aset best 2 ty))))
	    (setq ty (1+ ty))))
	(setq tx (1+ tx))))
    ;; go to this cool place
    (if (<= (aref best 0) 0)
	(tyrant-ai-input-string "P")
      (setq reversi-x (aref best 1)
	    reversi-y (aref best 2))
      (reversi-place-cursor)
      (tyrant-ai-input-string " "))))

;; end of lisp
(provide 'reversi-ai)
