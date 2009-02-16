;;; Time-stamp: <2006-05-06 17:46:45 jcgs>
;;; Old-time-stamp: <96/09/06 14:19:16 john>

;;  This program is free software; you can redistribute it and/or modify it
;;  unde r the terms of the GNU General Public License as published by the
;;  Free Software Foundation; either version 2 of the License, or (at your
;;  option) any later version.

;;  This program is distributed in the hope that it will be useful, but
;;  WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;  General Public License for more details.

;;  You should have received a copy of the GNU General Public License along
;;  with this program; if not, write to the Free Software Foundation, Inc.,
;;  59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

(provide 'swimmers)

(defvar swim-delay .1
  "The delay between frames of the swimming pool animation.")

(defvar swimming-pool-colour "black"
  "The colour to draw for the water.")

(defvar swimmers-begin-hook nil
  "Hooks to run when starting the swimming screen saver.")

(defvar swimmers-end-hook nil
  "Hooks to run when finishing the swimming screen saver.")

(defun swabs (a) (if (< a 0) (* -1 a) a))

(defun swevenp (a) (zerop (mod a 2)))

(defun overwrite-rectangle-carelessly (rectangle)
  "Overwrite text of RECTANGLE with upper left corner at point.
RECTANGLE's first line is overwritten at point,
its second line is overwritten at a point vertically under point, etc.
RECTANGLE should be a list of strings.
This version of overwrite-rectangle does not check about going off
the end of the line."
  (let* ((lines rectangle)
	 (linelength (length (car lines)))
	 (insertcolumn (current-column))
	 (first t))
    (while lines
      (or first (forward-line 1))
      (move-to-column insertcolumn)
      (delete-region (point) (+ (point) linelength))
      (insert (car lines))
      (setq lines (cdr lines) 
	    first nil))))

(defun make-list-circular (list)
  "Replace the nil at the end of LIST with LIST."
  (let* ((p list)
	 (q (cdr p)))
    (while q
      (setq p q
	    q (cdr p)))
    (rplacd p list))
  list)

;; These picture variables are (optionally circular) lists of rectangles.
;; They should have two rows or columns of white space on their trailing edge.

(mapcar 'makunbound '(breast-stroke-right breast-stroke-left))

(defun colour-swimmers (swimmers)
  "Fill in the background colour on SWIMMERS."
  (mapcar (lambda (swimmer)
	    (mapcar (lambda (row)
		      (propertize row
				  'face (cons 'background-color swimming-pool-colour)))
		    swimmer))
	  swimmers))

(defvar breast-stroke-right
  (colour-swimmers
   '(("  \\    / "
      "   >--<o "
      "  /    \\ ")  ("  __   |  "
      "  __>--+o "
      "       |  ")  ("      \\   "
      "  ==>-->o "
      "      /   ") ("  __   |  "
      "  __>--+o "
      "       |  ")))
  "A (circular?) list of rectangles showing an asciiperson swimming to the right.")

(defvar breast-stroke-left
  (colour-swimmers
   '(("\\    /   "
      "o>--<    "
      "/    \\   ") (" |   __  "
      "o+--<__  "
      " |       ") ("  /      "
      "o<--<==  "
      "  \\      ") (" |   __  "
      "o+--<__  "
      " |       ")))
  "A (circular?) list of rectangles showing an asciiperson swimming to the left.")

(defun make-swimmer (stroke x y dx dy) (vector stroke (nthcdr (mod (swabs (random)) 4) stroke) x y dx dy))
(defmacro swimmer-base-graphic (a) (list 'aref a 0))
(defmacro swimmer-graphic (a) (list 'aref a 1))
(defmacro swimmer-x (a) (list 'aref a 2))
(defmacro swimmer-y (a) (list 'aref a 3))
(defmacro swimmer-dx (a) (list 'aref a 4))
(defmacro swimmer-dy (a) (list 'aref a 5))
(defmacro set-swimmer-base-graphic (a v) (list 'aset a 0 v))
(defmacro set-swimmer-graphic (a v) (list 'aset a 1 v))
(defmacro set-swimmer-x (a v) (list 'aset a 2 v))
(defmacro set-swimmer-y (a v) (list 'aset a 3 v))
(defmacro set-swimmer-dx (a v) (list 'aset a 4 v))
(defmacro set-swimmer-dy (a v) (list 'aset a 5 v))

(makunbound 'swimmers)

(defvar swimmers (list
		  (make-swimmer breast-stroke-right 12 16 1 0)
		  (make-swimmer breast-stroke-right 24 8 1 0)
		  (make-swimmer breast-stroke-right 36 4 1 0)
		  (make-swimmer breast-stroke-right 48 12 1 0))
  "The list of swimmers")

(defun make-random-swimmer-list ()
  (setq swimming-pool-width (- (screen-width) (+ 4 (length (car (car breast-stroke-left))))))
  (let ((swimlist nil)
	(xmax (- swimming-pool-width 3))
	(ymax (- (screen-height) 7))
	(y 2))
    (while (< y ymax)
      (setq swimlist
	    (let ((direction (swevenp (random))))
	      (cons (make-swimmer (if direction breast-stroke-left breast-stroke-right)
				  (+ 3 (mod (swabs (random)) xmax))
				  y
				  (* (if direction -1 1)
				     (if (swevenp (random)) 1 2))
				  0)
		    swimlist))
	    y (+ y 5)))
    swimlist))

(makunbound 'swimming-pool-width)

(defun swimmers-move (swimmer-list)
  "Advance SWIMMER-LIST one step each."
  (while swimmer-list
    (let ((swimmer (car swimmer-list)))
  (let* ((graphic (swimmer-graphic swimmer))
	 (x (swimmer-x swimmer))
	 (y (swimmer-y swimmer))
	 (dx (swimmer-dx swimmer))
	 (dy (swimmer-dy swimmer)))
    (setq x (+ x dx)
	  y (+ y dy)
	  graphic (cdr graphic))
    (if (>= x swimming-pool-width)
	(progn (set-swimmer-base-graphic swimmer breast-stroke-left)
	       (setq dx (* dx -1)
		     graphic nil)))
    (if (<= x 2)
	(progn (set-swimmer-base-graphic swimmer breast-stroke-right)
	       (setq dx (* dx -1)
		     graphic nil)))
    (if (null graphic) (setq graphic (swimmer-base-graphic swimmer)))
    (goto-line y)
    (move-to-column x)
    (overwrite-rectangle-carelessly (car graphic))
    (set-swimmer-graphic swimmer graphic)
    (set-swimmer-x swimmer x)
    (set-swimmer-dx swimmer dx)
    (set-swimmer-y swimmer y)
    (set-swimmer-dy swimmer dy)))
  (setq swimmer-list (cdr swimmer-list))))

(defun swimming ()
  "Display a simple swimming pool."
  (interactive)
  (let ((pool (get-buffer-create "*Swimming pool*"))
	(old-cursor-colour (frame-parameter nil 'cursor-color)))
    (save-window-excursion
      (setq swimmers (make-random-swimmer-list))
      (switch-to-buffer pool)
      (delete-other-windows nil)
      (set-buffer pool)
      ;(buffer-flush-undo pool)
      (erase-buffer)
      (run-hooks 'swimmers-begin-hook)
      (let* ((i (- (screen-height) 4))
	     (w (- (screen-width) 4))
	     (s (concat "|" (make-string w 32) "|"))
	     (e (concat "+" (make-string w ?-) "+")))
	(insert e 10)
	(while (> i 0)
	  (insert s 10)
	  (setq i (1- i)))
	(insert e))
      (put-text-property (point-min) (point-max) '
			 face (cons 'background-color swimming-pool-colour))
      (set-cursor-color swimming-pool-colour)
      (message "Press any key to return to editing")
      (while (not (input-pending-p)) 
	(swimmers-move swimmers)
	(sit-for swim-delay))
      (read-char)
      (run-hooks 'swimmers-end-hook)
      (set-cursor-color old-cursor-colour)
      (bury-buffer pool))))

;;; end of swimmers.el
