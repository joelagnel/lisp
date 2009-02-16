
;;;  check.el -- My first elisp program, checkers!

;;  Copyright (C) 2000 Free Software Foundation

;;  Author: Ashton Trey Belew <abelew@wesleyan.edu>
;;  Version: 0.2
;;  Created: 2000-05-24

;;; Commentary:

;;  2000-06-05 So far only implementing simple board and checkers.

(eval-when-compile
  (require 'cl))
(require 'gamegrid)

;; ;;;;;;;;;;;;; customization variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar check-use-glyphs t
  "Non-nil means use glyphs when available")
(make-variable-buffer-local 'check-use-glyphs)
(defvar check-use-color t
  "Non-nil means use color when available")
(make-variable-buffer-local 'check-use-color)
(defvar check-buffer-name "*Check*"
  "Name used for Check buffer")
(make-variable-buffer-local 'check-buffer-name)
(defvar check-buffer-width 70
  "Width of used portion of buffer")
(make-variable-buffer-local 'check-buffer-width)
(defvar check-buffer-height 70
  "Height of used portion of buffer")
(make-variable-buffer-local 'check-buffer-height)
(defvar check-width 70
  "Width of playing area")
(make-variable-buffer-local 'check-width)
(defvar check-height 70
  "Height of playing area")
(make-variable-buffer-local 'check-height)
(defvar check-initial-x 10
  "Initial X position of check")
(make-variable-buffer-local 'check-initial-x)
(defvar check-initial-y 10
  "Initial Y position of check")
(make-variable-buffer-local 'check-initial-y)
(defvar check-mode-hook nil
  "Hook run upon starting Check")
(make-variable-buffer-local 'check-mode-hook)
(defvar square-width 4
  "The  width of each square")
(make-variable-buffer-local 'square-width)
(defvar square-height 4
  "The height of each square")
(make-variable-buffer-local 'square-height)
(defvar check-moves 0
  "The starting number of moves accomplished")
(make-variable-buffer-local 'check-moves)
(defvar check-score-x 0
  "The position of the score on the x axis")
(make-variable-buffer-local 'check-score-x)
(defvar check-score-y 33
  "The position of the score on the y axis")
(make-variable-buffer-local 'check-score-y)
(defvar check-x 0
  "The initial checker position on the x axis")
(make-variable-buffer-local 'check-x)
(defvar check-y 0
  "The initial checker position on the y axis")
(make-variable-buffer-local 'check-y)
(defvar check-matrix-x 0
  "The initial checker position on the x axis within the matrix")		
(make-variable-buffer-local 'check-matrix-x)
(defvar check-matrix-y 0
  "The initial checker position on the y axis within the matrix")	
(make-variable-buffer-local 'check-matrix-y)
(defvar currently-selected-from	'(10 10 0 0)
  "The initial selected from-list")
(make-variable-buffer-local 'currently-selected-from)
(defvar currently-selected-to		'(10 10 0 0)
  "The initial selected to-list")
(make-variable-buffer-local 'currently-selected-to)

;;;;;;;;;;;;;;;;  Testing Variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar test-mid-y	nil
  "The middle y coordinate of a jump")
(make-variable-buffer-local 'test-mid-y)
(defvar my-test-var	10
  "The testing variable")


;;;;;;;;;;;;;;  Testing Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;; display options ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar check-black-options
  '(((glyph colorize)
     (t ?\040))
    ((color-x color-x)
     (mono-x grid-x)
     (color-tty color-tty))
    (((glyph color-x) [0 0 0])
     (color-tty "black"))))

(defvar check-check-options
  '(((glyph colorize)
     (emacs-tty ?O)
     (t ?\040))
    ((color-x color-x)
     (mono-x mono-x)
     (color-tty color-tty)
     (mono-tty mono-tty))
    (((glyph color-x) [1 1 0])
     (color-tty "yellow"))))

(defvar check-red-options
  '(((glyph colorize)
     (t ?\*))
    ((color-x color-x)
     (mono-x grid-x)
     (color-tty color-tty))
    (((glyph color-x) [1 0 0])
     (color-tty "red"))))

(defvar check-blue-options
  '(((glyph colorize)
     (t ?\*))
    ((color-x color-x)
     (mono-x grid-x)
     (color-tty color-tty))
    (((glyph color-x) [0 0 1])
     (color-tty "blue"))))

(defvar check-green-options
  '(((glyph colorize)
     (t ?\*))
    ((color-x color-x)
     (mono-x grid-x)
     (color-tty color-tty))
    (((glyph color-x) [0 1 0])
     (color-tty "green"))))

(defvar check-border-options
  '(((glyph colorize)
     (t ?\+))
    ((color-x color-x)
     (mono-x grid-x))
    (((glyph color-x) [0.7 0.7 0.7])
     (color-tty "blue"))))

(defvar check-space-options
  '(((t ?\040))
    nil
    nil))

;;;;;;;;;;;;;;; constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst check-black	0)
(defconst check-check	1)
(defconst check-red	2)
(defconst check-border	3)
(defconst check-space	4)
(defconst check-blue	5)
(defconst check-green	6)
(defconst blue-pawn	1)
(defconst green-pawn	2)
(defconst blue-king		3)
(defconst green-king	4)

;;;;;;;;;;;;;;; variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar check-positions	nil)
(make-variable-buffer-local 'check-positions)
(defvar check-cycle	0)
(make-variable-buffer-local 'check-cycle)

(setq check-board	;;;   Each cell is made of four elements, x coord, y coord, color, occupied type.
  '( (0 0 0 1) (1 0 1 0) (2 0 0 1) (3 0 1 0) (4 0 0 1) (5 0 1 0) (6 0 0 1) (7 0 1 0)
     (0 1 1 0) (1 1 0 1) (2 1 1 0) (3 1 0 1) (4 1 1 0) (5 1 0 1) (6 1 1 0) (7 1 0 1)
     (0 2 0 1) (1 2 1 0) (2 2 0 1) (3 2 1 0) (4 2 0 1) (5 2 1 0) (6 2 0 1) (7 2 1 0)
     (0 3 1 0) (1 3 0 0) (2 3 1 0) (3 3 0 0) (4 3 1 0) (5 3 0 0) (6 3 1 0) (7 3 0 0)
     (0 4 0 0) (1 4 1 0) (2 4 0 0) (3 4 1 0) (4 4 0 0) (5 4 1 0) (6 4 0 0) (7 4 1 0)
     (0 5 1 0) (1 5 0 2) (2 5 1 0) (3 5 0 2) (4 5 1 0) (5 5 0 2) (6 5 1 0) (7 5 0 2)
     (0 6 0 2) (1 6 1 0) (2 6 0 2) (3 6 1 0) (4 6 0 2) (5 6 1 0) (6 6 0 2) (7 6 1 0)
     (0 7 1 0) (1 7 0 2) (2 7 1 0) (3 7 0 2) (4 7 1 0) (5 7 0 2) (6 7 1 0) (7 7 0 2)
))
(make-variable-buffer-local 'check-board)

(defvar empty-board
  '( (0 0 0 0) (1 0 1 0) (2 0 0 0) (3 0 1 0) (4 0 0 0) (5 0 1 0) (6 0 0 0) (7 0 1 0)
     (0 1 1 0) (1 1 0 0) (2 1 1 0) (3 1 0 0) (4 1 1 0) (5 1 0 0) (6 1 1 0) (7 1 0 0)
     (0 2 0 0) (1 2 1 0) (2 2 0 0) (3 2 1 0) (4 2 0 0) (5 2 1 0) (6 2 0 0) (7 2 1 0)
     (0 3 1 0) (1 3 0 0) (2 3 1 0) (3 3 0 0) (4 3 1 0) (5 3 0 0) (6 3 1 0) (7 3 0 0)
     (0 4 0 0) (1 4 1 0) (2 4 0 0) (3 4 1 0) (4 4 0 0) (5 4 1 0) (6 4 0 0) (7 4 1 0)
     (0 5 1 0) (1 5 0 0) (2 5 1 0) (3 5 0 0) (4 5 1 0) (5 5 0 0) (6 5 1 0) (7 5 0 0)
     (0 6 0 0) (1 6 1 0) (2 6 0 0) (3 6 1 0) (4 6 0 0) (5 6 1 0) (6 6 0 0) (7 6 1 0)
     (0 7 1 0) (1 7 0 0) (2 7 1 0) (3 7 0 0) (4 7 1 0) (5 7 0 0) (6 7 1 0) (7 7 0 0)
))
(make-variable-buffer-local 'empty-board)

;;;;;;;;;;;;;;; keymaps ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar check-mode-map
  (make-sparse-keymap 'check-mode-map))
(define-key check-mode-map "n"		'check-start-game)
(define-key check-mode-map "q"		'check-end-game)
(define-key check-mode-map "p"		'check-pause-game)
(define-key check-mode-map [left]	'check-move-left)
(define-key check-mode-map [right]	'check-move-right)
(define-key check-mode-map [up]		'check-move-up)
(define-key check-mode-map [down]	'check-move-down)
(defvar check-null-map	(make-sparse-keymap 'check-null-map))
(define-key check-null-map "n"		'check-start-game)
(define-key check-mode-map [button1]	'check-mouse-event-start)
(define-key check-mode-map [button2]	'check-mouse-event-start)
(define-key check-mode-map [button1up] 'check-mouse-event-end)
(define-key check-mode-map [button2up] 'check-mouse-event-end)
(define-key check-mode-map [down-mouse-1]	'check-mouse-event-start)
(define-key check-mode-map [down-mouse-2]	'check-mouse-event-start)
(define-key check-mode-map [mouse-1] 'check-mouse-event-end)
(define-key check-mode-map [mouse-2] 'check-mouse-event-end)
(define-key check-mode-map [(control ?/)]	'check-undo)

;;;;;;;;;;;;;;;;;; game functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;  Functions to examine the data of each checker cell.
;;;  Each cell has 4 parts:	(xcoord  ,  ycoord  ,  color of board  ,  type of checker on cell)
(defun get-x (lst)
  "Get the X coordinate from a list"
  (car lst))
(defun get-y (lst)
  "Get the Y coordinate from a list

See also: get-x"
  (cadr lst))
(defun get-color (lst)
  "Get the color from a list where 0 is black and 1 is red"
  (caddr lst))
(defun get-occupied (lst)
  "Get from a list its fourth elemet, 0 is unoccupied, 1 is blue pawn, 2 is green pawn, 3 is blue king, and 4 is green king."
  (cadddr lst))
(defun get-status	(xcoord	ycoord)
  "A function to acquire the checker status at point x,y."
  (get-status-aux xcoord ycoord check-board))
(defun get-status-aux	(xcoord ycoord board)
  "Get the status of x y at board"
  (if (= xcoord (get-x	(car	board)))
      (if (= ycoord (get-y	(car	board)))		;;;  Then we are on the same checker!
	  (car board)
	(get-status-aux xcoord ycoord (cdr board)))
    (get-status-aux xcoord ycoord (cdr board))))
(defun occupiedp	(xcoord ycoord)
  "A function to say whether a given x/y coord is taken by anything"
  (if (= 0 (get-occupied (get-status xcoord ycoord)))
      nil
    t))
(defun find-color (an-x	an-y)
  "Find what color is at x,y"
  (find-color-aux	an-x an-y check-board))
(defun find-color-aux (ax ay board)
  "Find the color at x,y for a given board"
  (if (= ax (get-x (car board)))
      (if (= ay (get-y (car board)))
	  (get-color (car board))
	(find-color-aux ax ay (cdr board)))
    (find-color-aux ax ay (cdr board))))

;;;;;;;;;;;;;;;;;;;;;  Functions to change the status of a cell.
(defun set-occupied (new-value	xcoord	ycoord)
  "Given the status list, change it to the new value"
  (set-occupied-aux	new-value	xcoord	ycoord	check-board	'()))
(defun set-occupied-aux	(new-value	a-x	a-y	board	result-list)
  (setq	work-lst	(get-status a-x a-y))
  (let*	((xcoord		(get-x work-lst))
	 (ycoord		(get-y work-lst))
	 (color		(get-color work-lst)))
    (if	(null board)
	(setq	check-board	result-list)		;;;  Note that this says check-board.
      (if (= xcoord	(get-x	(car board)))
	  (if	(= ycoord  (get-y	(car board)))
	      (set-occupied-aux   new-value   a-x   a-y   (cdr board)  (append result-list (list (list xcoord ycoord color new-value))))
	    (set-occupied-aux    new-value    a-x    a-y    (cdr board)    (append result-list (list (car board))))
	    )
	(set-occupied-aux    new-value    a-x    a-y    (cdr board)    (append result-list (list (car board))))))))
	

;;;;;;;;;;;;;;;;;;;;;  Some display functions
(defun check-display-options ()
  (let ((options (make-vector 256 nil)))
    (loop for c from 0 to 255 do
      (aset options c
	    (cond 
	     ((= c check-black)
	      check-black-options)
	     ((= c check-check)
	      check-check-options)
	     ((= c check-red)
	      check-red-options)
	     ((= c check-border)
	      check-border-options)
	     ((= c check-space)
	      check-space-options)
	     ((= c check-blue)
	      check-blue-options)
	     ((= c check-green)
	      check-green-options)
	     (t
	      '(nil nil nil)))))
    options))
(defun check-init-buffer ()
  "This draws the initial board and the checkers therein"
  (gamegrid-init-buffer    check-buffer-width    check-buffer-height    check-space)
  (recurse-board check-board))
(defun recurse-board (a-lst)
  "Recurse over a board and display stuff accordingly"
  (dolist (lst a-lst)
    (make-square (get-x lst) (get-y lst) (get-color lst))
    (color-taken (get-x lst) (get-y lst) (get-occupied lst))))
(defun  make-square (x y color)
  "Make a square with x and y coords and color."
  (loop for xsize from (* square-width x) to (* square-width (+ x 1)) do
    (if (>= 31 xsize)
	(loop for ysize from (* square-height y) to (* square-height (+ y 1)) do
	  (if (>= 31 ysize)
	      (if 	  (= color 0)
		  (gamegrid-set-cell xsize ysize check-red)
		(gamegrid-set-cell xsize ysize check-black)))))))
(defun color-taken (x y player)
  "Color in the squares owned by a particular player.  0 is nothing 1 is blue, 2 is green"
  (cond
   ((= player 1)	(make-pawn x y check-blue))
   ((= player 2)	(make-pawn x y check-green))
   ((= player 3)	(make-king x y check-blue))
   ((= player 4)	(make-king x y check-green))))
(defun make-pawn (x y color)
  "Make a pawn at x and y of color"
  (loop for xsize from (+ 1 (* square-width x)) to (- (* square-width (+ x 1)) 2) do
    (loop for ysize from (+ 1 (* square-height y)) to (- (* square-height (+ y 1)) 2) do
       (gamegrid-set-cell   xsize   ysize   color))))
(defun make-king (x y color)
  "Make a king at x and y of color"
    (loop for xsize from (* square-width x) to (* square-width (+ x 1)) do
    (loop for ysize from (* square-height y) to (* square-height (+ y 1)) do
       (gamegrid-set-cell   xsize   ysize   color))))

;;;;;;;;;;;;;;;;;  Functions to call at each mouse event, where all decisions are made.
(defun check-mouse-event-start (event)
  "A function called at each mouse press."
  (interactive "e"))
(defun find-matrix (coord)
  "Translates the mouse coordinates to coordinates of the checkerboard."
  (cond
   ((< coord 4)	0)
   ((< coord 8)	1)
   ((< coord 12)	2)
   ((< coord 16)	3)
   ((< coord 20)	4)
   ((< coord 24)	5)
   ((< coord 28)	6)
   ((< coord 33)	7)
   (else		10)))
(defun check-mouse-event-end (event)
  "Called after letting go of the mouse."
  (interactive "e")
  (let* (
	 (x (gamegrid-event-x event))
	 (y (gamegrid-event-y event))
	 )
  (setq check-matrix-x  (find-matrix x))
  (setq check-matrix-y  (find-matrix y))
  )
  (check-move-work)	;;;  A large-ish composite function to look to see if this is a valid move and such.
  (incf check-moves)
  (check-next))		;;;  Make the next checker board display.


;;;;;;;;;;;;;;;;;;;;;;;  Functions which handle the logic of checkers.
(defun check-move-work ()
  "The function which does all the real work"
  ;;;  First we have to see if the current move is even or odd.
  ;;;  If it is even, then the current move should be to select a new checker.
  ;;;  Thus we need to test to see that the person picked a checker
  (if (evenp check-moves)	;;  This is true when check-moves is 0.
      (if (occupiedp check-matrix-x check-matrix-y)
	  (setq		   currently-selected-from	(get-status check-matrix-x  check-matrix-y))	
	;;;  If it is occupied, then save that checker in memory until the next mouse event, which will be odd.
	(decf check-moves)		;;;  If is it not occupied, screw em and decrement the counter so they have to try again.
	)
;;;  Everything after this is the odd move -- which should be the setting down of a checker.
    (if (occupiedp check-matrix-x check-matrix-y)
	(decf check-moves)	;;;  Eg try again.
      (if (jumpp	check-matrix-x check-matrix-y currently-selected-from)
	  (do-jump check-matrix-x check-matrix-y currently-selected-from)
	(if (legalp		check-matrix-x check-matrix-y currently-selected-from)
	    (do-move	check-matrix-x check-matrix-y currently-selected-from)
	  (- check-moves 2))))))
(defun	jumpp	(xcoord ycoord old-position)
  "A tester to see if a move is a jump."
  ;;;  The coordinates of the jump are the average of the old an new
  (let*	((mid-x	(/ (+ xcoord (get-x old-position)) 2))
	 (mid-y	(/ (+ ycoord (get-y old-position)) 2))
	 (distance-x	(abs (- xcoord (get-x old-position))))
	 (distance-y	(abs (- ycoord (get-y old-position))))
	 )
    ;;;  If it is a jump, the distance x or/and y must equal 2 or -2
    (if	(= 2 distance-y)
	t
      nil)))
(defun	do-jump	(xcoord ycoord old-position)
  "A function to perform the work of a jump"
  (let*	((mid-x	(/ (+ xcoord (get-x old-position)) 2))
	 (mid-y	(/ (+ ycoord (get-y old-position)) 2))
	 (old-x	(get-x old-position))
	 (old-y	(get-y old-position))
	 )
    (set-occupied	0	old-x old-y)
    (set-occupied	0	mid-x mid-y)
    (if (move-kingp xcoord ycoord old-position)
	(if (= 1 (get-occupied old-position))
	    (set-occupied 3 xcoord ycoord)
	  (set-occupied 4  xcoord ycoord))
      (set-occupied	(get-occupied old-position)	xcoord ycoord))))
(defun	legalp	(xcoord ycoord old-position)
  "Test to see if a given move is legal."
  (if (forwardp xcoord ycoord old-position)
      t
    nil))		;;;  FIXME
(defun forwardp	(xcoord ycoord old-position)
  (let*	((check-type	(get-occupied old-position))
	 (old-x		(get-x old-position))
	 (old-y		(get-y old-position)))
    (if (= 1 (abs (- old-x xcoord)))
	(cond	
	 ((= 1 check-type)
	  (if (= -1 (- old-y ycoord))
	      t
	    nil))
	 ((= 2 check-type)
	  (if (= 1 (- old-y ycoord))
	      t
	    nil))
	 ((= 3 check-type)
	  (if (= 1 (abs (- old-y ycoord)))
	      t
	    nil))
	 ((= 4 check-type)
	  (if (= 1 (abs (- old-y ycoord)))
	      t
	    nil))
	 (else	nil))
      nil)))
(defun do-move	(new-x new-y old-position)
  "A function to do the manipulations needed to move a checker"
  (let*	((old-x		(get-x old-position))
	 (old-y		(get-y old-position))
	 (old-color	(get-color old-position))
	 (check-type	(get-occupied old-position))
	 )
    (set-occupied 0 old-x old-y)
   (if (move-kingp	new-x new-y old-position)
       (if (= 1 (get-occupied old-position))
	   (set-occupied 3 new-x new-y)
	 (set-occupied 4 new-x new-y))
     (set-occupied (get-occupied old-position) new-x new-y))))
(defun move-kingp	(xcoord ycoord old-position)
  "Test to see if a move creates a king"
  (or 
   (and (= 1 (get-occupied old-position))	(= 7 ycoord))
   (and (= 2 (get-occupied old-position))	(= 0 ycoord))))
(defun do-king	(xcoord ycoord old-position)
  "A function to create a king from a pawn"
  (if (= 1 (get-occupied old-position))   ;; If blue
      (set-occupied 2 xcoord ycoord)
    (set-occupied 4 xcoord ycoord)))
  
;;;;;;;;;;;;;;;;;;;;;;; Overall start/stop functions
(defun check-reset-game ()
  "Reset the checkers game."
  (check-init-buffer)
  (setq 
   check-paused		nil
   blue-killed		0
   green-killed		0
   check-moves		0
   currently-selected-from	'(0 0 0 0)
   )
  (check-draw-score)
  (let ((x check-initial-x)
	(y check-initial-y))))
(defun check-next ()
  "Redraw the board and score at each move"
  (check-init-buffer)
  (check-draw-score))
(defun check-draw-score ()
  "Draw the score"
  (let ((strings (vector 
		  (format "Moves:  %d" check-moves)
		  (format "Currently selected from: %s"	currently-selected-from)
		  )))
    (loop for y from 0 to 1 do
      (let* ((string (aref strings y))
	     (len (length string)))
	(loop for x from 0 to (1- len) do
	  (gamegrid-set-cell (+ check-score-x x)
			     (+ check-score-y y)
			     (aref string x))))))
  (setq mode-line-format
	(format "Check:   Moves: %d  Mouse-x: %d    Mouse-y: %d"
		check-moves	check-matrix-x	check-matrix-y))
  (force-mode-line-update))
(defun check-end-game ()
  "Terminates the current game"
  (interactive)
  (gamegrid-kill-timer)
  (use-local-map check-null-map))
(defun check-start-game ()
  "Starts a new game of Checkers"
  (interactive)
  (check-reset-game)
  (use-local-map check-mode-map))
(defun check-active-p ()
  "Check to see if the map is the checkers map"
  (eq (current-local-map) check-mode-map))
(defun check-mode ()
  "A mode for playing Check.
check-mode keybindings:
   \\{check-mode-map}
"
  (kill-all-local-variables)
  (make-local-hook 'kill-buffer-hook)
  (add-hook 'kill-buffer-hook 'gamegrid-kill-timer nil t)
  (use-local-map check-null-map)
  (setq major-mode 'check-mode)
  (setq mode-name "Check")
  (setq mode-popup-menu
	'("Check Commands"
	  ["Start new game"	check-start-game]
	  ["End game"		check-end-game
	   (check-active-p)]
	  ["Pause"		check-pause-game
	   (and (check-active-p) (not check-paused))]
	  ["Resume"		check-pause-game
	   (and (check-active-p) check-paused)]))
  (setq gamegrid-use-glyphs check-use-glyphs)
  (setq gamegrid-use-color check-use-color)
  (gamegrid-init (check-display-options))
  (run-hooks 'check-mode-hook))

;;;###autoload

(defun check ()
  "Check

Play Checkers!

check-mode keybindings:
   \\<check-mode-map>
\\[check-start-game]	Starts a new game of Check
\\[check-end-game]	Terminates the current game
\\[check-pause-game]	Pauses (or resumes) the current game
\\[check-move-left]	Makes the check move left
\\[check-move-right]	Makes the check move right
\\[check-move-up]		Makes the check move up
\\[check-move-down]	Makes the check move down

"
  (interactive)

  (switch-to-buffer check-buffer-name)
  (gamegrid-kill-timer)
  (check-mode)
  (check-start-game))

(provide 'check)

;;; check.el ends here
