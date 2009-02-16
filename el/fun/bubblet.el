; From: "Paul Du Bois" <dubois-at-infinite-machine-dot-com@microsoft.com>
; Subject: bubblet.el 0.73
; Newsgroups: gnu.emacs.sources
; Date: Fri, 16 Mar 2001 17:42:34 -0800
; Organization: Posted via Supernews, http://www.supernews.com
; Message-ID: <tb5g8emtl61914@corp.supernews.com>
; X-Priority: 3
; X-MSMail-Priority: Normal
; X-Newsreader: Microsoft Outlook Express 5.50.4133.2400
; X-MimeOLE: Produced By Microsoft MimeOLE V5.50.4133.2400
; X-Complaints-To: newsabuse@supernews.com
; Lines: 714
; Xref: news.wam.umd.edu gnu.emacs.sources:9414

;;; bubblet.el --- a bubble-popping game
;; $Id: //depot/tools/lisp/bubblet.el#4 $

;; Copyright (C) 2000 Paul Du Bois

;; Filename: bubblet.el
;; Author: Paul Du Bois <dubois@infinite-machine.com>
;; Maintainer: Paul Du Bois <dubois@infinite-machine.com>
;; Keywords: game, puzzle
;; Version: 
;; URL: http://www.gelatinous.com/pld/bubblet.html

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA

;;; Commentary:

;; Inspired by the original Palm Bubblet by Frank Fejes (www.oopdreams.com),
;; and by the win32 Bubblet by Daniel Klein (www.hobsoft.de).
;;
;; Requires emacs 20 or higher.  Load it and M-x bubblet to start playing.
;;
;; Pop as many bubbles as possible.  You can only pop groups of 2 or more
;; bubbles.  Groups are formed from vertically or horizontally adjacent
;; bubbles of the same color.  The larger the group, the more points you
;; score.  The score for popping a group of size N is N * (N-1)
;;
;; Usage:
;;   M-x bubblet
;;   M-x customize-group bubblet
;;
;; Bindings:
;;
;; n new game
;; h help
;; u undo (multiple undo supported)
;; q quit
;; C-l refresh

;;; Bugs/todo:

;; - Internet high score
;; - Supports multiple concurrent games, but no way to start up new buffers
;; - Colors are only appropriate for light-on-dark displays?
;; - Needs better docs and some reorg
;; - Only save scores when user asks?  Delay at end of game is annoying.
;; - Only "continuous" mode is supported.  But I don't like the others, so
;; there.

;;; History:

;; 0.73 Documentation.  First release on gnu.emacs.sources.
;; 0.72 A stab at supporting xemacs.
;; 0.71 Seed RNG used for new board creation on load -- prevent duplicate
;; boards
;; 0.7 Added bubblet-wildcard-percentage and bubblet-save-scores
;; 0.62 Undo now rolls back the RNG state
;; 0.61 alternate rendering style -- see bubblet-draw-connected
;; 0.6  remove some flicker; stats for avg group size
;; 0.5 add menu, history
;; 0.4 add high scores, end-of-game detection
;; 0.1  first released version

;;; Code:

(require 'cl)
(require 'cus-edit)

(defgroup bubblet nil
  "Bubblet game."
  :group 'external)

(defcustom bubblet-draw-connected t
  "* If non-nil, adjacent identical bubbles 'merge' into each other.
This also makes selecting groups easier."
  :group 'bubblet
  :type 'boolean)

(defcustom bubblet-quick-remove nil
  "* If non-nil, bubbles only take one click to remove."
  :group 'bubblet
  :type 'boolean)

(defcustom bubblet-save-scores t
  "* If non-nil, will save scores to your .emacs file."
  :group 'bubblet
  :type 'boolean)

(defcustom bubblet-verbose-display nil
  "* If non-nil, display score and other vitals in buffer as well as in
modeline."
  :group 'bubblet
  :type 'boolean)

(defcustom bubblet-wildcard-percentage 0.0
  "* Percentage chance of creating a wildcard bubble."
  :group 'bubblet
  :type 'float)

(defcustom bb-cell-height 2
  "* Height of a bubble, in characters."
  :group 'bubblet
  :type 'integer)

(defcustom bb-cell-width 5
  "* Width of a bubble, in characters."
  :group 'bubblet
  :type 'integer)

(defgroup bubblet-faces nil
  "Faces used by Bubblet."
  :group 'bubblet)

(defface bb-face-r '((t (:background "red4" :foreground "white")))
  "Face used for red bubbles."
  :group 'bubblet-faces)

(defface bb-face-rr '((t (:background "red" :foreground "white" :bold t)))
  "Face used for selected red bubbles."
  :group 'bubblet-faces)

(defface bb-face-g '((t (:background "forestgreen" :foreground "white")))
  "Face used for green bubbles."
  :group 'bubblet-faces)

(defface bb-face-gg '((t (:background "green" :foreground "black" :bold t)))
  "Face used for selected green bubbles."
  :group 'bubblet-faces)

(defface bb-face-b '((t (:background "blue1" :foreground "white")))
  "Face used for blue bubbles."
  :group 'bubblet-faces)

(defface bb-face-bb '((t (:background "cyan" :foreground "black")))
  "Face used for selected blue bubbles."
  :group 'bubblet-faces)

(defface bb-face-y '((t (:background "yellow3" :foreground "black")))
  "Face used for yellow bubbles."
  :group 'bubblet-faces)

(defface bb-face-yy '((t (:background "yellow" :foreground "black")))
  "Face used for selected yellow bubbles."
  :group 'bubblet-faces)

(defface bb-face-w '((t (:background "grey75" :foreground "black")))
  "Face used for white bubbles."
  :group 'bubblet-faces)

(defface bb-face-ww '((t (:background "white" :foreground "black")))
  "Face used for selected white bubbles."
  :group 'bubblet-faces)

(defface bb-face-* '((t (:background "black" :foreground "red")))
  "Face used for wildcard bubbles."
  :group 'bubblet-faces)

(defface bb-face-** '((t (:background "gray50" :foreground "red")))
  "Face used for selected wildcard bubbles."
  :group 'bubblet-faces)

(defvar bubblet-num-games 0 "Number of games played.")
(defvar bubblet-high-score 0 "High score.")
(defvar bubblet-high-stats '(0 0) "(MOVES BUBBLES) for high score game.")
(defvar bubblet-total-score 0 "Total score -- used to calculate average.")
(defvar bubblet-total-stats '(0 0) "(MOVES BUBBLES) for all games.")

;;; ----------------------------------------------------------------------
;;; Board interface
;;; ----------------------------------------------------------------------

;; dynamically-scoped variables, here to appease compiler
(defvar bb--colnum 0)
(defvar bb--rownum 0)
(defvar bb--board nil)

;;; public routines

(defsubst bb-aref (b row col) (elt (elt b col) row))
(defsubst bb-aset (b row col val) (setf (elt (elt b col) row) val))
(defun bb-print-board (b)
  "Print the board B.  Assumes an empty-ish buffer."
  (let ((bb--colnum 0)
 (bb--board b))
    (mapcar (lambda (c)
       (bb-print-column c)
       (bb-right-cell 1)
       (setq bb--colnum (1+ bb--colnum)))
     b)))

(defun bb-make-board (&optional state)
  "Construct a new board."
  (apply 'vector (mapcar (lambda (x) (bb-new-column state)) (make-vector 10
t))))


;;; private stuff

;; board format
;; board is a sequence of columns, from left to right
;; column is a sequence of cells, from top to bottom
;; a cell is a symbol, the first letter of a color, doubled if selected

(defconst bb-num-cols 10)
(defconst bb-num-rows 7)
(defconst bb-color-array [r g b y w])
(defconst bb-num-colors (length bb-color-array)) ;blue yellow white green
					;red

(defun bb-new-column (&optional state)
  "Return a new column with random bubbles."
  (apply 'vector
  (mapcar (lambda (x)
     (if (< (random* 100.0 state) bubblet-wildcard-percentage)
         '*
       (aref bb-color-array (random* bb-num-colors state))))
   (make-vector 7 'c))))

;; internal display stuff
;; uses dynamically-scoped vars bb--colnum, bb--rownum, bb--board

(defun bb-down-cell (num) (next-line (1+ bb-cell-height)))
(defun bb-right-cell (num) (forward-char (* num (1+ bb-cell-width))))

(defun bb-print-column (column)
  (let ((bb--rownum 0))
    (save-excursion
      (mapcar (lambda (cell)
  (save-excursion
    (bb-print-cell cell
     'bb-cell (cons bb--rownum bb--colnum)))
  (bb-down-cell 1)
  (setq bb--rownum (1+ bb--rownum)))
       column))))

(defun bb-print-cell (cell &rest plist)
  (let* ((str (make-string (1+ bb-cell-width) ?\ ))
  (hspace (make-string (1+ bb-cell-width) ?\ ))
  (this (bb-aref bb--board bb--rownum bb--colnum))
  (down (and (< bb--rownum (1- bb-num-rows))
      (bb-aref bb--board (1+ bb--rownum) bb--colnum)))
  (left (and (> bb--colnum 0)
      (bb-aref bb--board bb--rownum (1- bb--colnum))))
  down-identical-p
  left-identical-p)
    (when (and this bubblet-draw-connected)
      ;; Can't think of a good way to detect what a selected wildcard should
      ;; attach to, so just match anything else that's selected...
      (setq down-identical-p (eq this down)
     left-identical-p (eq this left))
      (cond ((eq this '**)
      (setq down-identical-p (= (length (symbol-name down)) 2)
     left-identical-p (= (length (symbol-name left)) 2)))
      ((= 2 (length (symbol-name this)))
       (setq down-identical-p (or (eq down this) (eq down '**))
      left-identical-p (or (eq left this) (eq left '**))))
      (t (setq down-identical-p (eq down this)
        left-identical-p (eq left this)))))

    ;; put together string to render
    (when (not (null cell))
      (let* ((color (symbol-name cell))
      (face (intern-soft (concat "bb-face-" color)))
      (next-line-add-newlines t))
 (if (= (length color) 1)
     (aset str (/ (length str) 2) (or ?: (aref color 0))))
 (if (not face) (error "Bubblet: no face for color %s" color))
 (put-text-property 1 (length str) 'face face str)
 (when left-identical-p
   (aset str 0 ?|)
   (put-text-property 0 1 'face face str))
 (when down-identical-p
   (setq hspace (concat " " (make-string bb-cell-width ?\-)))
   (put-text-property 1 (length hspace) 'face face hspace))
 (while plist
   (put-text-property (if left-identical-p 0 1)
        (length str) (car plist) (cadr plist) str)
   (if down-identical-p
       (put-text-property 1 (length hspace) (car plist) (cadr plist)
hspace))
   (setq plist (cddr plist)))))

    ;; render
    (let ((h bb-cell-height))
      (while (> h 0)
 (insert  str)
 (if (eobp) (insert "\n")) (forward-line 1) (end-of-line)
 (setq h (1- h))))
    (insert hspace)
    (if (eobp) (insert "\n")) (forward-line 1) (end-of-line)))

;;; ----------------------------------------------------------------------
;;; board update and "physics"
;;; ----------------------------------------------------------------------

(defun bb-select (board row col)
  "Mutates board to select cells around ROW and COL.
Returns number of cells selected."
  (let* ((sym (bb-aref board row col))
  (str (substring (symbol-name sym) 0 1))
  (selected-sym (intern (concat str str))))
    (bb-select-helper board row col sym selected-sym)))

(defun bb-selected-p (board row col)
  (let* ((sym (bb-aref board row col))
  (str (symbol-name sym)))
    (> (length str) 1)))

(defun bb-deselect (board)
  "Mutates board to deselect all cells."
  (let ((row 0))
    (while (< row bb-num-rows)
      (let ((col 0))
 (while (< col bb-num-cols)
   (let ((cell (bb-aref board row col)))
     (if (and (not (null cell))
       (> (length (symbol-name cell)) 1))
  (bb-aset board row col (intern (substring (symbol-name cell) 0 1)))))
   (setq col (1+ col))))
      (setq row (1+ row)))))

(defun bb-select-helper (board row col sym selected-sym)
  ;; returns number of cells modified
  (if (and (>= row 0) (< row bb-num-rows)
    (>= col 0) (< col bb-num-cols))
      (let ((elt (bb-aref board row col)))
 (if (or (eq elt '*) (eq elt sym))
     (progn
       (bb-aset board row col
         (if (eq elt '*) '** selected-sym))
       (+ 1
   (bb-select-helper board (1+ row) col sym selected-sym)
   (bb-select-helper board (1- row) col sym selected-sym)
   (bb-select-helper board row (1+ col) sym selected-sym)
   (bb-select-helper board row (1- col) sym selected-sym)))
   0))
    0))

(defun bb-has-adjacents-p (b)
  "Return non-nil if there exist adjacent cells of the same color."
  (catch 'found
    (let ((rm bb-num-rows) (cm bb-num-cols) (r 0) (c 0))
      (while (< r rm)
 (setq c 0)
 (while (< c cm)
   (let ((this (bb-aref b r c))
  (right (and (< c (1- cm)) (bb-aref b r (1+ c))))
  (down  (and (< r (1- rm)) (bb-aref b (1+ r) c))))
     (if (and this
       (or (eq this right) (eq this down)
    (eq this '*) (eq right '*) (eq down '*)))
  (throw 'found (cons r c))))
   (setq c (1+ c)))
 (setq r (1+ r)))
      nil)))

(defun bb-remove-cells (board row col &optional state)
  "Return a list:
  New board with cells around row and col removed.
  Number of new columns generated."
  (let ((list-board
  (mapcar (lambda (c) (mapcar 'identity c)) board))
 (sym (bb-aref board row col))
 new-cols)
    (if (= (length (symbol-name sym)) 1) (error "Trying to remove
non-selected <%s>" sym))
    (setq list-board
   (delq nil
  (mapcar (lambda (c)
     ;; xxx wasteful?
     ;; remove selected wildcards, empties, and SYM
     (setq c (delq '** (delq nil (delq sym c))))
     (if (= (length c) 0) c
       (nconc (make-list (- bb-num-rows (length c)) nil) c)))
   list-board)))
    (setq new-cols (- bb-num-cols (length list-board)))
    (while (< (length list-board) bb-num-cols)
      (setq list-board (cons (bb-new-column state) list-board)))
    (list
     (apply 'vector (mapcar (lambda (c)
         (if (vectorp c) c (apply 'vector c)))
       list-board))
     new-cols)))

;;; ----------------------------------------------------------------------
;;; Mode
;;; ----------------------------------------------------------------------

;; Variables not strictly associated with current game state
(defvar bbg-modeline "" "String to display in the modeline.")
(make-variable-buffer-local 'bbg-modeline)

;; local game context vars
(defvar bbc-board nil)
(defvar bbc-old-high-score 0)
(defvar bbc-old-high-stats '(0 0))
(defvar bbc-score 0 "Current score.")
(defvar bbc-num-selected 0 "Num bubbles currently selected.")
(defvar bbc-num-moves 0 "Num moves made so far.")
(defvar bbc-num-bubbles 0 "Num bubbles popped so far.")
(defvar bbc-num-columns 0 "Num columns removed so far.")
(defvar bbc-game-over nil "Non-nil if game is over.")
(defvar bbc-undo-list nil)
(defvar bbc-initial-state nil "Initial state of the RNG.")
(defvar bbc-current-state nil "Current state of the RNG.")
(make-variable-buffer-local 'bbc-board)
(make-variable-buffer-local 'bbc-old-high-score)
(make-variable-buffer-local 'bbc-score)
(make-variable-buffer-local 'bbg-modeline)
(make-variable-buffer-local 'bbc-num-selected)
(make-variable-buffer-local 'bbc-num-moves)
(make-variable-buffer-local 'bbc-num-bubbles)
(make-variable-buffer-local 'bbc-num-columns)
(make-variable-buffer-local 'bbc-game-over)
(make-variable-buffer-local 'bbc-undo-list)
(make-variable-buffer-local 'bbc-initial-state)
(make-variable-buffer-local 'bbc-current-state)

(defvar bubblet-menu nil)
(if bubblet-menu nil
  (let ((map (make-sparse-keymap "Bubblet")))
    (define-key map [bubblet-quit] '("Quit" . bubblet-quit))
    (define-key map [bubblet-separator] '("--"))
    (define-key map [bubblet-undo] '(menu-item "Undo" bubblet-undo :enable
bbc-undo-list))
    (define-key map [bubblet-help] '("Help" . describe-mode))
    (define-key map [bubblet-new-game] '("New Game" . bubblet-new-game))
    (setq bubblet-menu map)))

(defvar bubblet-mode-map nil)
(if bubblet-mode-map nil
  (let ((map (make-keymap)))
    (suppress-keymap map)
    (define-key map "\C-l" 'bubblet-refresh-display)
    (define-key map "h"  'describe-mode)
    (define-key map "n"  'bubblet-new-game)
    (define-key map "u"  'bubblet-undo)
    (define-key map "q"  'bubblet-quit)
    (define-key map [menu-bar bubblet] (cons "Bubblet" bubblet-menu))
    (define-key map [mouse-1] 'ignore)
    (define-key map [down-mouse-1] 'bubblet-mouse-select)

    (setq bubblet-mode-map map)))

(defun bubblet ()
  "Start a new Bubblet game."
  (interactive)
  (let ((buf (get-buffer-create "*Bubblet*")))
    (set-buffer buf)
    (when (not (eq major-mode 'bubblet-mode))
      (bubblet-mode)
      (bubblet-new-game t))
    (switch-to-buffer buf)))

(defun bubblet-mode ()
  "Major mode for playing Bubblet games.

Click on contiguous sequences of bubbles.  Click again to remove them,
or customize `bubblet-quick-remove'.  The bigger the group, the more
points you get.

Type \\[bubblet-new-game] to start a new game.
Type \\[bubblet-quit] to quit this game.
Type \\[bubblet-undo] to undo one (or more) moves.

Scores are saved to `custom-file' at the end of a game (this is
the source of the pause).  If you undo the last move, the score
will be removed."
  (kill-all-local-variables)
  (use-local-map bubblet-mode-map)
  (setq major-mode 'bubblet-mode
 mode-name "Bubblet"
 buffer-read-only t)
  (setq mode-line-format
 '("-" mode-line-frame-identification
   mode-line-buffer-identification
   bbg-modeline
   "--" (-3 . "%p") "-%-"))
  (font-lock-mode -1))

(defvar bbc-board-state (make-random-state t))
(defun bubblet-new-game (&optional force)
  "Start a new bubblet game."
  (interactive)
  (cond ((not (eq major-mode 'bubblet-mode))
  (error "Not in bubblet mode."))
 ((not (or force bbc-game-over (y-or-n-p "Abort the current game? ")))
  (error "New game aborted.")))
  (bubblet-mode)   ; inits local state
  (setq bbc-board (bb-make-board bbc-board-state)
 bbc-initial-state (make-random-state t)
 bbc-current-state (make-random-state bbc-initial-state)
 bbc-old-high-score bubblet-high-score
 bbc-old-high-stats bubblet-high-stats)
  (bubblet-refresh-display))

(defun bubblet-refresh-modeline ()
  (let ((score (number-to-string bbc-score))
 (moves (number-to-string bbc-num-moves))
 (bubbles (number-to-string bbc-num-bubbles))
 (moves-pl (if (= bbc-num-moves 1) "" "s"))
 (sel (number-to-string bbc-num-selected))
 (points (number-to-string (bubblet-points bbc-num-selected))))
    (setq bbg-modeline
   (list "Score: " score
  " (+" points ")   "
  "Selected: " sel "   "
  "Moves: " moves "   "
  "Popped: " bubbles "  "
  ))))

(defun bubblet-refresh-display ()
  (interactive)
  (let ((inhibit-read-only t))
    (bubblet-refresh-modeline)
    (erase-buffer)
    (insert "\n")
    (bb-print-board bbc-board)
    (goto-char (point-max))
    (when (or bubblet-verbose-display bbc-game-over)
      (insert (format "Score: %5d   Bubble points: %3d -- %d Bubbles
selected.\nMoves: %5d   Popped: %5d   Avg group: %5.2f\n\n"
        bbc-score
        (bubblet-points bbc-num-selected)
        bbc-num-selected
        bbc-num-moves
        bbc-num-bubbles
        (/ bbc-num-bubbles (float bbc-num-moves)))))
;;     (insert (format "num %d   score %d    hs %d"
;;       bubblet-num-games bubblet-total-score bubblet-high-score))
    (when bbc-game-over
      (insert (format " ==== GAME OVER %s==== \n"
        (if (> bbc-score bbc-old-high-score)
     "-- NEW HIGH SCORE! " "")))
      (insert (format "Games: %4d    " bubblet-num-games))
      (insert (format "Avg: %4d / %.2f    "
        (if (= bubblet-num-games 0) 0
   (/ bubblet-total-score bubblet-num-games))
        (if (= (elt bubblet-total-stats 0) 0) 0
   (/ (elt bubblet-total-stats 1)
      (float (elt bubblet-total-stats 0))))))
      (insert (format "High: %4d / %.2f\n"
        bubblet-high-score
        (/ (elt bubblet-high-stats 1)
    (float (elt bubblet-high-stats 0)))))
      (insert (substitute-command-keys
        "\nHit '\\[bubblet-new-game]' to start a new game.\n"))))
  (set-buffer-modified-p nil)
  (goto-char (point-min))
  (recenter)
  (goto-char (point-max)))

(defun bubblet-mouse-select (evt)
  "Select the bubbles at the mouse click."
  (interactive "e")
  ;; )@($* different interfaces between xemacs and fsf emacs :-/
  (if (and (boundp 'running-xemacs)
     (event-over-text-area-p evt))
      (progn
 (select-window (event-window evt))
 (set-buffer (event-buffer evt))
 (bubblet-select (event-closest-point evt)))
    (let ((pos (event-start evt)))
      (select-window (posn-window pos))
      (set-buffer (window-buffer (posn-window pos)))
      (bubblet-select (posn-point pos)))))

(defun bubblet-select (point)
  "Select the bubbles at point."
  (interactive "d")
  (let ((cell (get-text-property point 'bb-cell)))
    (if (not cell)
 (progn (message "No cell associated with this position")
        (when (> bbc-num-selected 0)
   (bb-deselect bbc-board)
   (setq bbc-num-selected 0)
   (bubblet-refresh-display)))
      (let* ((row (car cell))
      (col (cdr cell))
      (old-num-selected bbc-num-selected)
      (selected-p (bb-selected-p bbc-board row col)))
 (bb-deselect bbc-board)
 (setq bbc-num-selected (bb-select bbc-board row col))
 (cond
  ;; if not clicking on a group, just deselect
  ((<= bbc-num-selected 1)
   (setq bbc-num-selected 0)
   (when (> old-num-selected 0)
     (bb-deselect bbc-board)
     (bubblet-refresh-display)))

  ;; if clicking on a selected group, make it go away
  ((or bubblet-quick-remove selected-p)
   (bubblet-push-state)
   (let* ((tmp (bb-remove-cells bbc-board row col bbc-current-state))
   (new-board (car tmp))
   (removed (cadr tmp)))
     (setq bbc-board new-board
    bbc-num-columns (+ bbc-num-columns removed)
    bbc-score (+ bbc-score (bubblet-points bbc-num-selected))
    bbc-num-moves (1+ bbc-num-moves)
    bbc-game-over (not (bb-has-adjacents-p bbc-board))
    bbc-num-bubbles (+ bbc-num-bubbles bbc-num-selected)
    bbc-num-selected 0))
   (bubblet-refresh-display)
   ;; this should only be hit once per game
   (when (and bubblet-save-scores bbc-game-over)
     (bubblet-add-score bbc-score (bbc-stats))))

  (t (bubblet-refresh-display)))))))

(defun bbc-stats ()
  ;; pulled out so I can extend stats in the future...
  (list bbc-num-moves bbc-num-bubbles))

(defun bubblet-push-state ()
  (let ((state (list bbc-board bbc-score bbc-num-selected bbc-num-bubbles
bbc-num-columns)))
    (setq bbc-undo-list (cons state bbc-undo-list))))

(defun bubblet-undo ()
  "Undo one move."
  (interactive)
  (if (null bbc-undo-list) (message "No more moves to undo!")
    (let* ((state (car bbc-undo-list))
    (removed (elt state 4)))
      (when (and bubblet-save-scores bbc-game-over)
 (bubblet-remove-score bbc-score (bbc-stats) bbc-old-high-score
bbc-old-high-stats))
      ;; roll back the RNG if any columns were removed
      ;; (more precisely, reinit RNG and roll it forward)
      (when (/= removed bbc-num-columns)
 (setq bbc-num-columns removed)
 (setq bbc-current-state (make-random-state bbc-initial-state))
 (let ((i bbc-num-columns))
   (while (> i 0)
     (bb-new-column bbc-current-state)
     (setq i (1- i)))))
      (setq bbc-undo-list (cdr bbc-undo-list)
     bbc-board (elt state 0)
     bbc-score (elt state 1)
     bbc-num-selected (elt state 2)
     bbc-num-bubbles (elt state 3)
     bbc-game-over nil
     bbc-num-moves (1- bbc-num-moves))
      (bubblet-refresh-display))))

(defun bubblet-reset ()
  (setq bubblet-high-score 0 bubblet-num-games 0 bubblet-total-score 0
 bubblet-high-stats '(0 0) bubblet-total-stats '(0 0)))

(defun bubblet-quit ()
  "Quit the current Bubblet game."
  (interactive)
  (kill-buffer (current-buffer)))

;; utils

;; I should really just do (customize-save-variable foo val) three times,
;; but each one saves out the entire custom list again

(defun bubblet-add-score (score stats)
  ;; Add a score to the score table
  ;;(message "add score %s" score)
  (setq bubblet-num-games (1+ bubblet-num-games)
 bubblet-total-score (+ bubblet-total-score score)
 bubblet-total-stats (mapcar* '+ bubblet-total-stats stats))
  (when (> score bubblet-high-score)
    (setq bubblet-high-score score
   bubblet-high-stats stats))

  (mapcar (lambda (sym) (put sym 'saved-value (list (custom-quote
(symbol-value sym)))))
   '(bubblet-num-games
     bubblet-total-score bubblet-total-stats
     bubblet-high-score  bubblet-high-stats))
  (custom-save-all))

(defun bubblet-remove-score (score stats old-high-score old-high-stats)
  ;; Remove a score from the score table
  (setq bubblet-num-games (1- bubblet-num-games)
 bubblet-total-score (- bubblet-total-score score)
 bubblet-total-stats (mapcar* '- bubblet-total-stats stats))
  (when (> score old-high-score)
    (setq bubblet-high-score old-high-score
   bubblet-high-stats old-high-stats))

  (mapcar (lambda (sym) (put sym 'saved-value (list (custom-quote
(symbol-value sym)))))
   '(bubblet-num-games
     bubblet-total-score bubblet-total-stats
     bubblet-high-score  bubblet-high-stats))
  (custom-save-all))

(defun bubblet-points (size)
  ;; Return the number of points of a group of size SIZE
  (* size (1- size)))

(provide 'bubblet)


