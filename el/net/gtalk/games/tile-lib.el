;;; tile-lib - library game code for tile-based games

;;; Author: Eric M. Ludlam <zappo@gnu.ai.mit.edu>
;;; Version: 0.1
;;; Keywords: games, extensions
;;;
;;; Copyright (C) 1994, 1995 Free Software Foundation
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

;;; Comentary:
;;; 
;;; Tile lib is used by any game which has adjacent tiles which are
;;; arbitrarilly placed on the screen.  The basis is quite simple,
;;; with descriptions of the tile size to use, a board in which a
;;; program may store tile positions, and general board access
;;; routines.  In addition, drawing routines (goto/draw etc) is
;;; available.  To make overlapping edges, make the width-height 1
;;; less that the length of the tile lists (list of tile parts).
;;; When the tiles are placed, the lower/rightmost edges are
;;; overlapped.  Erasing pieces are non-trivial, and easier done by
;;; clearing the screen, and re-drawing all pieces.
;;;
;;; Colored tiles are not currently supported yet.
;;;
;;; $Id: tile-lib.el,v 1.4 1995/06/01 22:53:21 zappo Exp $
;;; History:
;;;

(require 'game-lib "games/game-lib")

(defvar tile-lib-width nil
  "Width of tiles used in the current tile using program.  It should
be the width of the tile with one shared edge.")

(defvar tile-lib-height nil
  "Hieght of tiles used in the current tile using program.  It should
be the heigth of the tile with one shared edge.")

(defvar tile-lib-board nil
  "This should be a double vector of the current board being used.  It
should be set by the application using it.")

(defvar tile-lib-tile-list nil
  "This should be the list of tiles indexed by the integers stored in
the board.")

(defvar tile-lib-origin-x nil
  "This is the origin (0, 0) which is always the upper left edge of
the board.  This can change in some apps depending on the size of the
board.")

(defvar tile-lib-origin-y nil
  "This is the origin (0, 0) which is always the upper left edge of
the board.  This can change in some apps depending on the size of the
board.")

(defvar tile-lib-max-x nil
  "This is the distance away from the origin for the opposite side of
the existing board.")

(defvar tile-lib-max-y nil
  "This is the distance away from the origin for the opposite side of
the existing board.")

(defun tile-lib-get (x y)
  "Return the tile at position x y in the tile board."
  (aref (aref tile-lib-board y) x))

(defun tile-lib-set (x y tile)
  "Set at position X and Y in tile-lib-board the TILE."
  ;; Only need MAX parts because when the origin is changed, then the
  ;; board is redrawn, and the origin recalculated.
  (if (>= x tile-lib-max-x) (setq tile-lib-max-x (1+ x)))
  (if (>= y tile-lib-max-y) (setq tile-lib-max-y (1+ y)))
  (aset (aref tile-lib-board y) x tile))

(defun tile-lib-width ()
  "Return the width of the defined board.  Because the board build
with make-board is square, we just need to look at the first line."
  (length (aref tile-lib-board 0)))

(defun tile-lib-height ()
  "Return the height of the board by getting the length of the height
vector in the board."
  (length tile-lib-board))

(defun tile-lib-make-board (width height)
  "Create and return a board structure as used by tile-lib."
  (let ((board (make-vector height nil))
	(cnt 0))
    (while (< cnt height)
      (aset board cnt (make-vector width nil))
      (setq cnt (1+ cnt)))
    board))

(defun tile-lib-goto (x y &optional center extraline)
  "Move point to a relative X and Y position.  X and Y are defined by
the width of the tile being draw (defined by tile-lib-width, and
tile-lib-height).  If CENTER is used and true, the move cursor the the
center of the desired tile.  If EXTRALINE, the actually go down by
that many lines, guaraneeing that the characters are there to move the
cursor outwards."
  (if (or (not tile-lib-width) (not tile-lib-height))
      (error "tile-lib-draw: cannot draw without tile-lib-width or tile-lib-height defined."))
  (if (not extraline) (setq extraline 0))
  (let* ((line (+ (* y tile-lib-height) (if center (/ tile-lib-height 2) 0)
		  extraline 2))
	 (col (+ (* x tile-lib-width) (if center (/ tile-lib-width 2) 0) 2))
	 ;; turn of tabs for indentation!
	 (indent-tabs-mode nil)
	 (num (goto-line line)))
    (if (and (= 0 num) (/= 0 (current-column))) (newline 1))
    (if (eobp) (newline num))
    ;; Now, a quicky column moveto/forceto method.
    (or (= (move-to-column col) col) (indent-to col))))

(defun tile-lib-draw (x y tile &optional no-edge-overlap)
  "Move to the X Yth position on the screen relative to the width of
the tiles.  Then draw TILE which is a list of equal length strings.
If no-edge-overlap is t, then don't overwrite any edge pieces."
  (let ((tilelen (length tile))
	(nexts nil))
    (if no-edge-overlap (setq tile (cdr tile)))
    (while tile
      (setq nexts (car tile))
      ;; Zap outer edge of the string
      (if no-edge-overlap 
	  (setq nexts (substring nexts 1 (- (length nexts) 1))))
      ;; tile length can be longer that tile height, this allows
      ;; overlapping edges!
      (tile-lib-goto x y nil (- tilelen (length tile)))
      (if (and no-edge-overlap (= (length tile) 1))
	  nil
	(if no-edge-overlap (if (eolp) (insert " ") (forward-char 1)))
	(delete-region
	 (point)
	 (save-excursion
	   (move-to-column (+ (current-column) (length nexts)))
	   (point)))
	(insert nexts))
      (setq tile (cdr tile)))))

(defun tile-lib-draw-board (&optional find-origin)
  "This function will clear the current screen, and draw all tiles
stored in the board."
  (if (or (not tile-lib-board) (not (vectorp tile-lib-board)))
      (error "tile-lib-draw-board: tile-lib-board invalid type."))
  (game-lib-clear-buffer)
  (let ((orig-x 0)
	(orig-y 0)
	(count-x 0)
	(count-y 0)
	(max-x 0)
	(max-y)
	)
    ;; find origin if we need it
    (while (and 
	    find-origin
	    (< orig-y (tile-lib-height))
	    (not (eval (cons 'or (mapcar 'eval (aref tile-lib-board orig-y))))))
      (setq orig-y (1+ orig-y)))
    ;; find the end y part now
    (setq max-y (1+ orig-y))
    (while (and
	    find-origin
	    (< max-y (tile-lib-height))
	    (eval (cons 'or (mapcar 'eval (aref tile-lib-board max-y)))))
      (setq max-y (1+ max-y)))
    ;; find the x origin
    (while (and
	    find-origin
	    (< orig-x (tile-lib-width))
	    (not (eval (cons 'or
			     (mapcar '(lambda (e) (aref e orig-x)) 
				     tile-lib-board)))))
      (setq orig-x (1+ orig-x)))
    ;; find the max x size
    (setq max-x (1+ orig-x))
    (while (and
	    find-origin
	    (< max-x (tile-lib-width))
	    (eval (cons 'or
			(mapcar '(lambda (e) (aref e max-x)) 
				tile-lib-board))))
      (setq max-x (1+ max-x)))
    ;; Save the origin found herin...
    (setq tile-lib-origin-x orig-x)
    (setq tile-lib-origin-y orig-y)
    (setq tile-lib-max-x max-x)
    (setq tile-lib-max-y max-y)
    ;; First, find the first tile on left/right, and make that the new
    ;; origin of the board.  Do this only if the correct option is on.
    (setq count-y orig-y)
    (while (< count-y (tile-lib-height))
      (setq count-x orig-x)
      (while (< count-x (tile-lib-width))
	(if (tile-lib-get count-x count-y)
	    (progn
	      (tile-lib-draw (- count-x orig-x) (- count-y orig-y)
			     (nth (tile-lib-get count-x count-y) 
				  tile-lib-tile-list)
			     (equal (tile-lib-get count-x count-y) 0))
	      ;; (sit-for 1)
	      ))
	(setq count-x (1+ count-x)))
      (setq count-y (1+ count-y)))
    (if find-origin
	(progn
	  (goto-char (point-min))
	  (insert "  ")
	  (let ((cnt orig-x))
	    (while (< cnt max-x)
	      (insert 
	       (substring
		(concat " " (int-to-string (- cnt orig-x -1)) "        ")
		0 tile-lib-width))
	      (setq cnt (1+ cnt))))
	  (let ((cnt orig-y))
	    (while (< cnt max-y)
	      (goto-line (+ 3 (* (- cnt orig-y) tile-lib-height)))
	      (beginning-of-line)
	      (delete-char 1)
	      (insert
	       (substring
		(format "%c" (+ ?A (- cnt orig-y)))
		0 1))
	      (setq cnt (1+ cnt))))
	  )
      )
    )
)

;;; End of lisp
(provide 'tile-lib)


