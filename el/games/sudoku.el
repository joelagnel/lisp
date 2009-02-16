;; sudoku.el -- Simple sudoku game, can download puzzles from the web.

;; Filename: sudoku.el
;; Copyright (C) 2005 Jesse Rosenthal
;; Author: Jesse Rosenthal <jesse.k.rosenthal@gmail.com>
;; Maintainer: Jesse Rosenthal <jesse.k.rosenthal@gmail.com>
;; Created: 29 Oct 2005
;; Description: Uses either local or downloaded sudoku for a simple puzzle game
;; Version 0.3 (5 Jan 2006)

;; Latest version always available from:
;; www.columbia.edu/~jr2075/elisp/sudoku.el

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 2, or (at your
;; option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; `sudoku-mode' is a major mode for solving sudoku puzzles. The rules are
;; simple: exactly one of each number in each of the nine rows, columns, and
;; subsquares. The puzzles are of four levels: easy, medium, hard, and evil.
;; sudoku-mode has a few nifty features, the most notable being that you can
;; download puzzles on the fly from websudoku.com (there are parsing functions
;; to take care of the html). The 200 included puzzles were generated using
;; the python-based generator from Thomas Hinkle's gnome-sudoku
;; (http://gnome-sudoku.sourceforge.net).
;;
;; I've added customization options, and you can also now customize via the
;; dropdown menu. If you really want to write in your .emacs file, though,
;; the three variables are `sudoku-level' ({"easy"|"medium"|"hard"|"evil"}),
;; `sudoku-puzzle-source' ({"built-in"|"web-site"|"custom"}),
;; `sudoku-download-method' ({"lynx"|"wget"|"w3"|"native-url-lib"}). But you
;; can do all this interactively. The main thing you really need to add is:
;;
;; (require 'sudoku)      (-- and be sure to put sudoku.elc in your load path)
;;
;; Note: Those who plan to use wget to download puzzles may also add:
;; (defvar sudoku-wget-process "<invocation pathname to your copy of wget>")
;;
;; To start puzzle solving (once the sudoku file is loaded), evaluate the
;; expression (sudoku) (say, in the *scratch* buffer), to set up and view
;; the special *sudoku* puzzle-solving buffer.
;;
;; UPDATE: Downloading files no longer requires lynx! Four options are now
;; offered: lynx, wget, the native url library in emacs >=22, and
;; w3 (which seems to be largely obsolete). This is set through the
;; `sudoku-download-method' variable, which is also available through the
;; configuration options. The default is to use "native-url-lib" if gnu emacs
;; is above version 22, and lynx otherwise. If anyone has any suggestions for
;; why another option should be the default, please let me know.
;;
;; The defaults are for `sudoku-level' to be "easy" and `sudoku-puzzle-source'
;; to be "built-in". But there are only about fifty puzzles of each level
;; included, so the chances of you repeating one are pretty good. You're
;; probably better off setting the puzzle-source to "web-site", if you're
;; online.
;;
;;
;;; ChangeLog:
;; v0.1 - 29 0ct 2005
;;    * First version
;;
;; v0.1.1 - 30 Oct 2005 -
;;    * Added erase function.
;;    * Changed the (if (a) b) statements to (when (a) b). Thanks to
;;      Thomas Gehrlein for pointing this convention out to me.
;;    * Added some new directions (leftmost, downmost, rightmost,
;;      upmost) and associated keybinding.
;;    * Added credit to Thomas Hinkle for puzzle generator.
;;    * Changed (undo-copy-list) to (copy-tree)
;;    * Changed permanent online location to
;;      www.columbia.edu/~jr2075/sudoku.el
;;
;; v0.1.2 - 4 Nov 2005 -
;;    * Added radio buttons to menu.
;;    * Added hint function, bound to "/C-c/C-h"
;;    * Added Emacs Customization code
;;
;; v0.1.3 - 15 Nov 2005 -
;;    * Fixed an annoying issue where the
;;     compatibility-with-old-versions code called the sudoku-level
;;     variable too early, if it wasn't set in the .emacs file. Now it
;;     should work fine with or without .emacs customization. (Thanks
;;     to Andrew Scott for pointing this out to me.)
;;
;;
;; v0.2 - 18 Nov 2005 -
;;   * Downloading files no longer requires lynx!  There are now four
;;     options: the native url library (in emacs >= 22), w3, lynx, and
;;     wget. The first two are pure elisp, so they should be platform
;;     independent. Thanks to Wojciech Komornicki for getting me
;;     started on this.
;;   * Added a save-options entry to the drop-down menu. You really
;;     don't need to (and maybe shouldn't) customize by hand anymore.
;;     Either use the dropdown menu, or customiz-group <RET> sudoku.
;;
;; v0.2.1 - 13 Dec 2005 -
;;
;;   * Added a small fix to sudoku-html-to-list to get rid of an empty
;;     line at the top of the input entries, which was screwing up the
;;     board.
;;
;;
;; v0.3 - 5 Jan 2006 -
;;    Lewis G. Creary contributed a package of bugfixes and enhancements that:
;;    * removed troublesome dependencies on fns propertize and format.
;;    * enabled use of hyphen as blank-cell value.
;;    * handled previously undealt-with error conditions in functions
;;      sudoku-move-point and sudoku-hint.
;;    * fixed function sudoku-cell-possibles to include an already filled-in
;;      value as a possibility.
;;    * improved the appearance and informativeness of onscreen instructions.
;;    * introduced a second kind of hint (show interesting cells) that
;;      displays between the puzzle grid and the onscreen instructions.
;;    * enabled the Emacs *sudoku* buffer mode-line to show what puzzle cell
;;      the cursor is in, in a format that matches the new hint display.
;;    * allowed the program to accept manually entered puzzles for solution
;;      and to perform some basic correctness checking on them.
;;    * enabled the user to make menu choices after finishing a puzzle
;;      but before beginning another one.
;;    * changed the beginning cursor position to be the center of the grid.
;;    * added upfront defvar declarations of most sudoku global variables
;;      to the program file in order to permit clean byte-compiling.
;;    * provided a more robust [html -> puzzle] extraction function.
;;    * used common lisp functionality to make sudoku-subsquare function 
;;      more legible
;;    * changed `reverse' to `nreverse' in functions `sudoku-column', 
;;      `sudoku-get-cell-points', and `sudoku-cells-of-interest'

;;; TODO:
;;  - Add a solver
;;  - Continue to clean up code

;;; Code:

(require 'cl)
(require 'easymenu)

;; LGC- declare sudoku global variables upfront for clean byte-compiling
(defvar sudoku-mode-map)
(defvar current-board)
(defvar start-board)
(defvar sudoku-onscreen-instructions)
(defvar cell-point-list)
(defvar easy-puzzles)
(defvar medium-puzzles)
(defvar hard-puzzles)
(defvar evil-puzzles)
(defvar old-board)
(defvar sudoku-puzzle-source)
(defvar sudoku-win-state)
(if (eq system-type 'windows-nt)
    ;; If you are using windows, you may need to alter the location of
    ;; the executable.
    (defvar sudoku-wget-process "c:/Program Files/Wget/wget.exe")
  ;; else
  (defvar sudoku-wget-process "wget"))

;; Custom puzzles can be manually copied out of newspapers or magazines,
;;  or constructed from scratch by hand, or ...
(defvar custom-puzzles '(
 ((7 0 0 0 1 0 0 0 2)
 (0 9 0 0 0 3 8 4 0)
 (0 0 8 0 0 0 0 3 0)
 (5 1 0 0 3 0 0 0 9)
 (0 0 0 7 0 2 0 0 0)
 (6 0 0 0 9 0 0 5 3)
 (0 7 0 0 0 0 6 0 0)
 (0 4 2 9 0 0 0 1 0)
 (3 0 0 0 8 0 0 0 4)) )
 "A custom list of, say, manually copied or constructed puzzles." )

;; This has some compatibility things built in, like propertize...
(when (featurep 'xemacs)
 (require 'easy-mmode))

(defgroup sudoku nil
 "Sudoku - web-enabled puzzle game"
 :group  'games
 :prefix "sudoku-")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customizable variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom sudoku-level "easy"
 "*Level of difficulty for sudoku."
 :group 'sudoku
 :type '(radio (const "easy")
              (const "medium")
              (const "hard")
              (const "evil")))

;; This is just a compatibility thing from the old setup, when the
;; difficulty level was a symbol instead of a string
(when (symbolp sudoku-level)
 (setq sudoku-level (symbol-name sudoku-level)))

(defcustom sudoku-puzzle-source "built-in"
 ;; LGC- added this on 05 Dec 2005
 "*Where should sudoku get puzzles from for presentation?"
 :group 'sudoku
 :type '(radio (const "built-in")
              (const "web-site")
              (const "custom") ))

(defcustom sudoku-download-method "lynx"
 "*Method for downloading new puzzles."
 :group 'sudoku
 :type '(radio (const "native-url-lib")
               (const "w3")
               (const "lynx")
               (const "wget")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Non-customizable variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar sudoku-mode 'nil)
(make-variable-buffer-local 'sudoku-mode)

(defvar sudoku-mode-hooks 'nil)
(make-variable-buffer-local 'sudoku-mode-hooks)

(defvar sudoku-current-cell nil
 "Contains a string displayed in the Emacs *sudoku* buffer mode-line.")

;; LGC- changed blank-cell value to hyphen (was underbar)
(defconst blank-cell "-")

(defconst sudoku-buffer-name "*sudoku*")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic mode functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sudoku ()
 "Used to start a new game and change to sudoku-mode"
 (interactive)
 (boldface-onscreen-instructions) ;; LGC- added 29 Nov 2005
 (sudoku-new)
 (sudoku-mode))

(defun sudoku-mode ()
 "A mode for playing `sudoku' The key bindings for sudoku-mode
are: \\{sudoku-mode-map}"
 (interactive)
 ;(sudoku-new)
 (kill-all-local-variables)
 (use-local-map sudoku-mode-map)
 (setq major-mode 'sudoku-mode
      mode-name  "sudoku")
 (run-hooks 'sudoku-mode-hook)  ;; LGC- added 30 Nov 2005
 (setq buffer-read-only t)
 (buffer-disable-undo))

(defun sudoku-set-up-mode-line ()
 ;; LGC- added this fn on 30 Nov 2005
 "Defines a mode-line-format for Sudoku that shows what cell the cursor is in."
 (setq mode-line-format
      '("-" mode-line-mule-info mode-line-modified
        mode-line-frame-identification mode-line-buffer-identification
        "   " global-mode-string "   %[(" mode-name mode-line-process
        minor-mode-alist "%n" ")%]  "
        ("Cell" sudoku-current-cell "      ")
        (which-func-mode ("" which-func-format "  "))
        (-3 . "%p")) ))

(defvar sudoku-mode-hook '(sudoku-set-up-mode-line)) ;; LGC- modified 30 Nov 2005

(defvar sudoku-mode-map
 (let ((map (make-sparse-keymap)))
  (define-key map "\C-c\C-n" 'sudoku-new)
  (define-key map "\C-c\C-c" 'sudoku-quit)
  (define-key map "\C-c\C-r" 'sudoku-restart)

  (define-key map [up] 'sudoku-move-point-up)
  (define-key map "\C-p" 'sudoku-move-point-up)
  (define-key map "k" 'sudoku-move-point-up)
  (define-key map [down] 'sudoku-move-point-down)
  (define-key map "\C-n" 'sudoku-move-point-down)
  (define-key map "j" 'sudoku-move-point-down)
  (define-key map [return] 'sudoku-move-point-down)
  (define-key map [left] 'sudoku-move-point-left)
  (define-key map "\C-b" 'sudoku-move-point-left)
  (define-key map "h" 'sudoku-move-point-left)
  (define-key map [right] 'sudoku-move-point-right)
  (define-key map "\C-f" 'sudoku-move-point-right)
  (define-key map "l" 'sudoku-move-point-right)
  (define-key map [tab] 'sudoku-move-point-right)

  (define-key map "\C-a" 'sudoku-move-point-leftmost)
  (define-key map "\C-e" 'sudoku-move-point-rightmost)
  (define-key map [prior] 'sudoku-move-point-upmost)
  (define-key map [next] 'sudoku-move-point-downmost)
  (define-key map [home] '(lambda () (interactive) ;; LGC- reformatted 2 defs
                           (sudoku-move-point-upmost)
                           (sudoku-move-point-leftmost)))
  (define-key map [end] '(lambda () (interactive)
                           (sudoku-move-point-downmost)
                           (sudoku-move-point-rightmost)))

  (define-key map "\C-d" 'sudoku-cell-erase)
  (define-key map blank-cell 'sudoku-cell-erase)  ;; LGC- modified 29 Nov 2005
  (define-key map " " 'sudoku-cell-erase)
  (define-key map "0" 'sudoku-cell-erase)
  (define-key map [backspace] 'sudoku-cell-erase)

  (define-key map "\C-c\C-h" 'sudoku-hint)
  (define-key map "\M-h" 'sudoku-hint)  ;; LGC- added 3 defs
  (define-key map "\C-\M-h" 'sudoku-list-interesting-cells)
  (define-key map [?\S-\C-h] 'sudoku-remove-intrstg-cells-hint)

  ;; Disabled in sudoku mode

  (define-key map "\C-v" 'sudoku-disabled-key)
  (define-key map "\M-v" 'sudoku-disabled-key)
  (define-key map [mouse-1] 'sudoku-disabled-key)
  (define-key map [down-mouse-1] 'sudoku-disabled-key)
  (define-key map [drag-mouse-1] 'sudoku-disabled-key)
  (define-key map [double-mouse-1] 'sudoku-disabled-key)

  (define-key map "\C-k" 'sudoku-disabled-key)

  ;;I want to figure out how to make it only go to valid cells, but
  ;;for the time being...

  (define-key map "1" '(lambda () (interactive) (sudoku-change-point 1)))
  (define-key map "2" '(lambda () (interactive) (sudoku-change-point 2)))
  (define-key map "3" '(lambda () (interactive) (sudoku-change-point 3)))
  (define-key map "4" '(lambda () (interactive) (sudoku-change-point 4)))
  (define-key map "5" '(lambda () (interactive) (sudoku-change-point 5)))
  (define-key map "6" '(lambda () (interactive) (sudoku-change-point 6)))
  (define-key map "7" '(lambda () (interactive) (sudoku-change-point 7)))
  (define-key map "8" '(lambda () (interactive) (sudoku-change-point 8)))
  (define-key map "9" '(lambda () (interactive) (sudoku-change-point 9)))
  map)
 "Keymap for sudoku mode")

(easy-menu-add-item nil '("tools" "games") ["Sudoku" sudoku t])

(easy-menu-define sudoku-mode-menu sudoku-mode-map "sudoku menu."
 '("Sudoku"
  ["New puzzle"               sudoku-new t]
  ["Restart puzzle"       sudoku-restart t]
  ["Quit game"               sudoku-quit t]
  "---"
  ("Set level"
    :active (or (string= sudoku-puzzle-source "built-in")
                (string= sudoku-puzzle-source "web-site"))
   ["Easy"  (setq sudoku-level "easy")
    :style radio :selected (string= sudoku-level "easy")]
   ["Medium" (setq sudoku-level "medium")
    :style radio :selected (string= sudoku-level "medium")]
   ["Hard"  (setq sudoku-level "hard")
    :style radio :selected (string= sudoku-level "hard")]
   ["Evil" (setq sudoku-level "evil")
    :style radio :selected (string= sudoku-level "evil")])
  ;; LGC- the following generalizes the old "Download: On/Off" menu
  ("Choose puzzle source"
   ["Web site" (setq sudoku-puzzle-source "web-site")
    :style radio :selected (string= sudoku-puzzle-source "web-site")]
   ["Built-in" (setq sudoku-puzzle-source "built-in")
    :style radio :selected (string= sudoku-puzzle-source "built-in")]
   ["Custom" (setq sudoku-puzzle-source "custom")
    :style radio :selected (string= sudoku-puzzle-source "custom")])
   ("Download Method"
    :active (string= sudoku-puzzle-source "web-site")
    ["lynx"  (setq sudoku-download-method "lynx")
     :style radio :selected (string= sudoku-download-method "lynx")]
    ["w3"  (setq sudoku-download-method "w3")
     :style radio :selected (string= sudoku-download-method "w3")]
    ["Native Url Library (cvs only)"  (setq sudoku-download-method
                                            "native-url-lib")
     :style radio :selected (string= sudoku-download-method "native-url-lib")]
    ["wget"  (setq sudoku-download-method "wget")
     :style radio :selected (string= sudoku-download-method "wget")])
   "---"
   ["Save Options"
    (mapcar #'(lambda (var) (eval `(customize-save-variable (quote ,var) ,var)))
            '(sudoku-level
              sudoku-puzzle-source
              sudoku-download-method
              sudoku-wget-process))]))

(defun sudoku-new ()
 "Sets the \"current-board\" global variable, using the
 \"sudoku-get-new-board\" function, and then runs
 \"sudoku-initialize\", which does the rest."
 (interactive)
 (let ((new-board (sudoku-get-new-board sudoku-puzzle-source sudoku-level)))
  (cond ((listp new-board)
           (setq current-board new-board)
           (sudoku-initialize) )
        ((stringp new-board)
           ;; LGC- report problem with new puzzle.
           (ding)
           (message new-board) ))))

(defun sudoku-initialize ()
 "Makes the board, based on the \"current board\" variable, and
 sets the buffer for read-only. Used by \"sudoku-new\"."
 (switch-to-buffer (get-buffer-create sudoku-buffer-name))
 (when buffer-read-only
    (setq buffer-read-only nil))
 (erase-buffer)
 ;;(sudoku-mode)
 (setq start-board current-board
      ;; LGC- and terminate any current win state.
      sudoku-win-state nil )
 (sudoku-board-print current-board sudoku-onscreen-instructions)
 (setq cell-point-list (sudoku-get-cell-points))
 (sudoku-goto-cell '(4 4))  ; LGC- start off at center of grid
 (when (null buffer-read-only)
    (setq buffer-read-only t)))

(defun sudoku-get-new-board (puzzle-source &optional level)
 ;; LGC- renamed this fn from "sudoku-current-board" for clarity.
 "Checks the \"puzzle-source\" value, and possibly the \"level\" value.
 Uses these to either choose a random included board (if puzzle-source
 is \"built-in\"), or to download one from websudoku.com (if puzzle-source
 is \"web-site\"), or to choose a random custom puzzle (if puzzle-source
 is \"custom\"). "
 ;; LGC- removed side-effects to match the way this fn has been used.
 (cond ((string= puzzle-source "web-site")
       (cond ((string= level 'easy)
              (sudoku-download-new-puzzle 1))
             ((string= level 'medium)
              (sudoku-download-new-puzzle 2))
             ((string= level 'hard)
              (sudoku-download-new-puzzle 3))
             ((string= level 'evil)
              (sudoku-download-new-puzzle 4))))

      ;; LGC- allow custom puzzles to be chosen.
      ((string= sudoku-puzzle-source "custom")
         (if (not (and (listp custom-puzzles)
                       (> (length custom-puzzles) 0)))
           "ERROR: Global var custom-puzzles is NOT a non-empty list!"
           (let* ((puzzle (if (= 1 (length custom-puzzles))
                            (car custom-puzzles)
                            (let ((n (mod (random t)
                                          (length custom-puzzles))))
                              (nth n custom-puzzles) )))
                (message (sudoku-test-custom-puzzle puzzle)) )
             (if (string= message "OK")
               puzzle
               message ))))

      ;; LGC- make last clause sensitive to varying sizes of puzzle lists.
      (t (let* ((pzl-list
                  (cond ((string= level 'easy)   easy-puzzles)
                        ((string= level 'medium) medium-puzzles)
                        ((string= level 'hard)   hard-puzzles)
                        ((string= level 'evil)   hard-puzzles) ))
                (pzl-num (length pzl-list))
                (n (mod (random t) pzl-num)) )
                 ;; This is about as close to a good random number as
                 ;; we can get in emacs
           (nth n pzl-list) ))))

(defun sudoku-quit-immediately ()
 "Quit without a prompt. Designed to be used by other functions."
 (kill-buffer sudoku-buffer-name))

(defun sudoku-quit ()
 "Quit with confirmation."
 (interactive)
 (when (y-or-n-p "Are you sure you want to quit sudoku? ")
    (sudoku-quit-immediately)))

(defun sudoku-restart ()
 "Return the board to its original state."
 (interactive)
 (if (null start-board)
  (message "You have to start before you can restart.")
  ;; else
  (setq current-board start-board)
  (sudoku-initialize)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic sudoku functions (can also be used for a solver)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sudoku-row (board n)
 "Returns the nth row of a board."
 (nth n board))

(defun sudoku-column (board n)
 "Returns the nth column of a board"
 (let ((column nil))
  (dolist (row board)
    (setq column (cons (nth n row) column)))
  (nreverse column)))

(defun sudoku-subsquare (board region-num)
 ;; LGC-  16 Dec 2005
 "Returns the (0-based) nth region of a board (as a flat list)."
 (let* ((row-root (* 3   (/ region-num 3)))
     (col-root (* 3 (mod region-num 3)))
     (board-rows (butlast (nthcdr row-root board) (- 6 row-root))) )
   ;; Use of `function' construct enables byte-compilation of lambda-expr.
   (mapcan (function (lambda (row)
             (butlast+ (nthcdr col-root row) (- 6 col-root)) ))
        board-rows )))

(defun butlast+ (list n)
 ;; LGC-  16 Dec 2005
 "A fully copying version of butlast -- always safe to nconc the value."
 (if (< n 1)
   (copy-list list)
   (butlast list n) )) 

;; (defun sudoku-subsquare (board n)
;;  "Returns the nth subsquare of a board (as a flat list)"
;;  (let ((rows (list (sudoku-row board (* 3 (/ n 3)))
;;                   (sudoku-row board (+ (* 3 (/ n 3)) 1))
;;                   (sudoku-row board (+ (* 3 (/ n 3)) 2))))
;;       (col-start-num (* (mod n 3) 3))
;;       (subsquare nil)
;;       (subsquare-flat nil))
;;   (dolist (row rows)
;;     (setq subsquare
;;           (cons
;;            (butlast (nthcdr col-start-num row) (- 6 col-start-num))
;;            subsquare)))
;;   (dolist (row subsquare)
;;     (dolist (elt (reverse row))
;;       (setq subsquare-flat (cons elt subsquare-flat))))
;;   subsquare-flat))

(defun sudoku-cell (board x y)
 "Returns the (x,y) cell of a board"
 (nth x (sudoku-row board y)))

(defun sudoku-cell-elts (board x y)
 "Returns the row, column, and subsquare containing cell (x,y)"
 (let ((subsquare-num
       (+ (* (/ y 3) 3) (/ x 3))))
  (list (sudoku-row board y)
        (sudoku-column board x)
        (sudoku-subsquare board subsquare-num))))

(defun sudoku-cell-elts-flat (board x y)
 "Returns the row, column and subsquare containing cell (x,y) in
a flat list"
 (let ((result nil))
  (dolist (elt (sudoku-cell-elts board x y))
    (dolist (atom elt)
      (setq result (cons atom result))))
  result))

(defun sudoku-cell-possibles (board x y)
 ;; LGC- cleaned up and added comments to this fn
 "Returns all the possible values for a cell (i.e., those not
already in the row, column, and subsquare containing it."
 (let ((possibilities nil)
      (board-cell-value (sudoku-cell board x y)) )
  (if (/= (sudoku-cell start-board x y) 0)
    ; value of cell was originally "given"
    (setq possibilities (cons board-cell-value possibilities))
    ; else value was originally 0 (empty)
    (let ((non-zero-elts (remove 0 (sudoku-cell-elts-flat board x y)))
          (n 1) )
      (when (/= board-cell-value 0)
        ;; allow a filled-in non-zero value to be a possibility (recall that
        ;; the program doesn't allow impossible values to be filled in).
        ;; LGC- this statement removes a logical bug from this fn.
        (setq non-zero-elts (remove board-cell-value non-zero-elts)) )
      (while (<= n 9)
        ;; collect the possibile values
        (when (not (member n non-zero-elts))
          (setq possibilities (cons n possibilities)) )
        (setq n (1+ n)) )))
  possibilities ))

(defun sudoku-cell-valid (board x y input)
 "Tests to see if a cell's input is valid."
 (if (member input (sudoku-cell-possibles board x y))
    t
  nil))

(defun sudoku-test-custom-puzzle (puzzle)
 ;; LGC- added this fn on 05 Dec 2005
 "Tests whether puzzle meets some basic necessary conditions of correctness."
 ;; first, check for correct dimensions of puzzle and sizes of numbers
 (let ((rows puzzle) ;; a puzzle is a list of rows
      (row-index 0)
      (message "OK") )
  (if (not (listp rows))
    (setq message "ERROR: custom puzzle is not a list!"
          rows nil )
    ;; else
    (unless (= 9 (length rows))
      (setq message
         (format "ERROR: custom puzzle has the wrong number of rows: %s"
                 (length rows) )
            rows nil )))
  (while rows
    (let ((nums (car rows))) ;; a row is a list of nums
      (if (not (listp nums))
        (setq message
              (format "ERROR: row %s of custom puzzle is not a list!"
                      row-index )
              nums nil rows nil )
        ;; else
        (unless (= 9 (length nums))
          (setq message
                (format
        "ERROR: 0-based row %s of custom puzzle has the wrong length: %s"
                        row-index (length nums) )
                nums nil rows nil )))
      (while nums
        (let ((num (car nums)))
          (unless (and (natnump num) (<= num 9))
            (setq message
                  (format
        "ERROR: 0-based row %s of custom puzzle has a bad number: %s"
                    row-index num )
                  nums nil rows nil ))
          (setq nums (cdr nums)) ))
      (setq row-index (1+ row-index) rows (cdr rows)) ))

  (cond ((not (string= message "OK"))
           ;; return error msg and quit if appropriate
           message )
        ;; else check that there are no wrongly duplicated "givens"
        ((dotimes (index 9 nil)
           (unless (unique-non-negs-p (sudoku-row puzzle index))
             (setq message
                   (format
              "ERROR: 0-based row: %s of custom puzzle has duplicate givens!"
                           index ))
             (return t) )
           (unless (unique-non-negs-p (sudoku-column puzzle index))
             (setq message
                   (format
           "ERROR: 0-based column: %s of custom puzzle has duplicate givens!"
                           index ))
             (return t) )
           (unless (unique-non-negs-p (sudoku-subsquare puzzle index))
             (setq message
                   (format
           "ERROR: 0-based region: %s of custom puzzle has duplicate givens!"
                           index ))
             (return t) ))
           ;; return error message
           message )
        (t ;; finally, check that every blank cell has some possible values
           (let ((cells)
                 (possibles) )
             (dotimes (index (* (length puzzle) (length (car puzzle))) message)
               (let* ((cell (sudoku-number-to-cell index))
                      (x (car cell))
                      (y (cadr cell)))
                 (when (= (sudoku-cell puzzle x y) 0)
                   (setq possibles (sudoku-cell-possibles puzzle x y))
                   (unless (>= (length possibles) 1)
                     (setq message
                           (format
      "ERROR: 0-based (row col):%s of custom puzzle has no possible fillers!"
                             (reverse cell) ))
                     (return message) )))))) )))

(defun unique-non-negs-p (integer-list)
 ;; LGC- added this fn on 14 Dec 2005
 "*Determines whether a given list of integers has no duplicates among
 its positive members."
 (let ((int-tail integer-list)
       (int-set) (int) (dups) )
   (while int-tail
     (setq int (car int-tail))
     (if (> int 0)
       (cond ((not (memq int int-set))
                (setq int-set (cons int int-set)) )
             (t (setq dups t
                      int-tail nil ))))
     (setq int-tail (cdr int-tail)) )
   (not dups) ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions for displaying the board on-screen
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defconst sudoku-onscreen-instructions
 ;; LGC- removed some tabs for better readability of instructions,
 ;;      and added some useful instructions for existing commands.
 "
       Commands:

Arrow Keys:\tMove around the board
C-a (or C-e):\tMove to beginning (or end) of a row
pg-up (pg-dn):\tMove to top (or bottom) of a column
home (or end):\tMove to beginnning (or end) of the puzzle array
1-9:\t\tEnter a numerical value into the current cell
\[SPC\]:\t\tRemove the numerical value from the current cell
C-c C-h, M-h:\tHint (possible values)
C-M-h:\t\tHint (interesting cells)
S-C-h:\t\tRemove interesting cells hint
C-c C-c:\tQuit the game
C-c C-n:\tStart a new puzzle
C-c C-r:\tRestart the current puzzle
")

(defun boldface-onscreen-instructions ()
 ;; LGC- added this fn on 29 Nov 2005
 "Boldfaces appropriate portions of the sudoku-onscreen-instructions."
 (let* ((start (string-match "Commands" sudoku-onscreen-instructions))
       (end (match-end 0)) )
  (put-text-property start end 'face 'bold sudoku-onscreen-instructions)
  ;--------------------------------------------------------------------
  (setq start (string-match "Arrow Keys" sudoku-onscreen-instructions)
        end (match-end 0) )
  (put-text-property start end 'face 'bold sudoku-onscreen-instructions)
  ;--------------------------------------------------------------------
  (setq start (string-match "C-a" sudoku-onscreen-instructions)
        end (match-end 0) )
  (put-text-property start end 'face 'bold sudoku-onscreen-instructions)
  (setq start (+ 8 start) end (+ 8 end))
  (put-text-property start end 'face 'bold sudoku-onscreen-instructions)
  ;--------------------------------------------------------------------
  (setq start (string-match "pg-up" sudoku-onscreen-instructions)
        end (match-end 0) )
  (put-text-property start end 'face 'bold sudoku-onscreen-instructions)
  (setq start (+ 7 start) end (+ 7 end))
  (put-text-property start end 'face 'bold sudoku-onscreen-instructions)
  ;--------------------------------------------------------------------
  (setq start (string-match "home" sudoku-onscreen-instructions)
        end (match-end 0) )
  (put-text-property start end 'face 'bold sudoku-onscreen-instructions)
  (setq start (+ 9 start) end (+ 8 end))
  (put-text-property start end 'face 'bold sudoku-onscreen-instructions)
  ;--------------------------------------------------------------------
  (setq start (string-match "1" sudoku-onscreen-instructions)
        end (match-end 0) )
  (put-text-property start end 'face 'bold sudoku-onscreen-instructions)
  (setq start (+ 2 start) end (+ 2 end))
  (put-text-property start end 'face 'bold sudoku-onscreen-instructions)
  ;--------------------------------------------------------------------
  (setq start (string-match "\\[SPC\\]" sudoku-onscreen-instructions)
        end (match-end 0) )
  (put-text-property start end 'face 'bold sudoku-onscreen-instructions)
  ;--------------------------------------------------------------------
  (setq start (string-match "C-c C-h, M-h" sudoku-onscreen-instructions)
        end (match-end 0) )
  (put-text-property start end 'face 'bold sudoku-onscreen-instructions)
  ;--------------------------------------------------------------------
  (setq start (string-match "C-M-h" sudoku-onscreen-instructions)
        end (match-end 0) )
  (put-text-property start end 'face 'bold sudoku-onscreen-instructions)
  ;--------------------------------------------------------------------
  (setq start (string-match "S-C-h" sudoku-onscreen-instructions)
        end (match-end 0) )
  (put-text-property start end 'face 'bold sudoku-onscreen-instructions)
  ;--------------------------------------------------------------------
  (setq start (string-match "C-c C-c" sudoku-onscreen-instructions)
        end (match-end 0) )
  (put-text-property start end 'face 'bold sudoku-onscreen-instructions)
  ;--------------------------------------------------------------------
  (setq start (string-match "C-c C-n" sudoku-onscreen-instructions)
        end (match-end 0) )
  (put-text-property start end 'face 'bold sudoku-onscreen-instructions)
  ;--------------------------------------------------------------------
  (setq start (string-match "C-c C-r" sudoku-onscreen-instructions)
        end (match-end 0) )
  (put-text-property start end 'face 'bold sudoku-onscreen-instructions)
  ))

(defun sudoku-row-output (row row-num separator)
 "This takes care of most of the outputting work. It makes a
 string with a separator, and then three numbers, another
 separator, and so on. It also replaces all zeros with the
 `blank-cell' constant. So, if separator and blank-cell are
 \"|\" and \"_\", we would get:

 (1 2 0 3 4 0 5 6 0) -> | 1 2 _ | 3 4 _ | 5 6 _ |"

 (let ((output-string nil))
  (setq output-string separator)
  (dotimes (i 3)
    (dotimes (j 3)
      (let ((value (nth (+ (* 3 i) j) row)))
        (cond ((= value 0)
               ;; If it's equal to 0, we use the blank-cell
               ;; character.
               (setq output-string (concat output-string
                                           (format " %s " blank-cell))))
              ((/= (sudoku-cell start-board (+ (* 3 i) j) row-num) 0)
               ;; If it's one of the original numbers, we bold it.
                ;;; LGC- replaced call to fn propertize (unavailable in some
                ;;;  versions of Emacs), and removed call on format (which
                ;;;  wasn't preserving text-props) on 28 Nov 2005
               (let ((string (int-to-string value)))
                 (put-text-property 0 (length string) 'face 'bold string)
                 (setq output-string (concat output-string
                                             " " string " " ))))
              (t
               ;; If it's any other number, we just input.
               (setq output-string (concat output-string
                                      (format " %s " value )))))))
    (setq output-string (concat output-string separator)))
  output-string))

(defun sudoku-board-output (board)
 "Outputs the visible board. Uses sudoku-row-output to do most
of the work."
 (let ((corner "+")
      (horiz "---")
      (vert "|")
      (top-piece nil)
      (output-string nil))
  (dotimes (i 3)
    (setq top-piece (concat top-piece corner))
    (dotimes (j 3)
      (setq top-piece (concat top-piece horiz))))
  (setq top-piece (concat top-piece corner))
  (setq output-string (concat top-piece "\n"))
  (dotimes (i 3)
    (dotimes (j 3)
      (let ((row-num (+ (* 3 i) j)))
      (setq output-string (concat output-string
                                  (sudoku-row-output
                                   (sudoku-row board row-num) row-num vert)
                                  "\n"))))
    (setq output-string (concat output-string top-piece "\n")))
  output-string))

(defun sudoku-board-print (board &optional message)
 ;; LGC- made message an optional argument
 "Prints the board and the (optional) message beneath the board
 together. Usually the message will be the moves. The only other
 message right now is the \"You Win!\" message."
 (save-excursion
  (goto-char (point-min))
  (insert (sudoku-board-output board))
  (when message (insert message)) ))

(defun sudoku-board-erase ()
 ;; LGC- added this fn on 29 Nov 2005
 "Erases the 13-line Sudoku grid and following blank line."
 (save-excursion
  (goto-line 14)
  (end-of-line)
  (delete-region (point-min) (point)) ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions relating to changing cells
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sudoku-change-cell (board x y input)
 "Changes a specific cell"
 (cond (sudoku-win-state
         ;; LGC- permit no changes after puzzle is complete.
         (message "Puzzle is complete.  Can't change.")
         board )
      (t (let ((newboard (copy-tree board)))
           ((lambda (lst index val)
              (setcar (last lst (- (length lst) index)) val))
            (sudoku-row newboard y) x input)
           newboard))))

(defun sudoku-test-and-change (board x y input)
 "Tests whether a change is valid. If it is, enters the cell and
redraws the board."
 (cond ((member input (sudoku-cell-possibles board x y))
         (setq old-board board)
         (setq current-board (sudoku-change-cell board x y input))
         (sudoku-board-erase) ;; LGC- erase just board, not whole buffer
         ;; LGC- avoid reprinting onscreen command instructions
         (sudoku-board-print current-board))
      (t (if (/= (sudoku-cell start-board x y) 0)
           (message "Original value.  Can't change.")
           (message "Not a valid move") ))))

(defun sudoku-get-cell-points ()
 "This reads a printed board and returns the point of each
 number, counting from 0. So, for a 9x9 board, we should get 81
 pairs. We can then use this to turn each number into a cell
 [e.g. 0 -> (0 0), 1 -> (1 0), 9 -> (0 1)] so we can match up
 each cell with a point on the screen. This function is used once
 at initialization to make the cell-point-list, and then that is
 consulted, so we don't have to keep running this over and over
 again."
 (save-excursion
 (goto-char (point-min))
 (let ((counter 0)
      (point-list nil))
  ;; LGC- modified following loop to avoid problems with hyphen blank-cell.
  (dotimes (i (length current-board))
    (beginning-of-line)
    (unless (looking-at "|") (forward-line))
    (while (not (eolp))
      (cond ((or (looking-at "[0-9]") (looking-at blank-cell))
               (setq point-list (cons (list (point) counter) point-list))
               (incf counter)))
      (forward-char))
    (forward-line))
  (nreverse point-list))))

(defun sudoku-number-to-cell (num)
 "This takes the numbers from 0 to 80 and turns them into
 coords.\n TODO: Abstract this using (length board) to make this
 not be 9-dependent"
 (let ((x (mod num 9))
      (y (/ num 9)))
  (list x y)))

(defun sudoku-cell-to-number (coords)
 "This turns any cell into a number from 0 to 80."
 (let ((x (car coords))
      (y (car (cdr coords))))
  (+ (* 9 y) x)))

(defun sudoku-get-cell-from-point (num)
 "This uses the \"cell-point-list\" made at initialization to
 return a cell for a point on the screen."
 (let ((result nil))
  (dolist (i cell-point-list)
    (when (= num (car i))
        (setq result (car (cdr i)))))
  (if (null result)
      nil
    (sudoku-number-to-cell result))))

(defun sudoku-get-point-from-cell (coords)
 "Returns a point on the screen for a given cell."
 (let ((result nil))
  (dolist (i cell-point-list)
    (when (= (sudoku-cell-to-number coords) (car (cdr i)))
        (setq result (car i))))
  result))

(defun sudoku-change-point (input)
 "Changes the value at a point, after running tests to ensure
 that the change is a valid one. Checks to see how many cells
 are remaining. If none are, runs the
 sudoku-completion-routine (i.e., \"You Win!\")."
 (let ((cell (sudoku-get-cell-from-point (point))))
  (save-excursion
    (when buffer-read-only
        (setq buffer-read-only nil))
    (when (not (null cell))
        (let* ((cell (sudoku-get-cell-from-point (point)))
               (x (car cell))
               (y (car (cdr cell))))
          (sudoku-test-and-change current-board x y input))))
  (sudoku-goto-cell cell))
 (when (null buffer-read-only)
    (setq buffer-read-only t))
 (let ((remaining (sudoku-remaining-cells current-board)))
  (when (and (not sudoku-win-state) ;; LGC- added check on win state.
             (= remaining 0))
      (sudoku-completion-routine)
    ;(message "%d empty cells left" remaining)
    )))

(defun sudoku-cell-erase ()
 (interactive)
 (let* ((cell (sudoku-get-cell-from-point (point)))
         (x (car cell))
         (y (car (cdr cell))))
    (if (= (sudoku-cell start-board x y) 0)
        (setq current-board (sudoku-change-cell current-board x y 0))
      (message "Original value. Can't erase."))
    (setq buffer-read-only nil)
    (sudoku-board-erase)  ;; LGC- erase just board, not whole buffer
    ;; LGC- avoid reprinting onscreen command instructions
    (sudoku-board-print current-board)
    (sudoku-goto-cell cell)
    (setq buffer-read-only t)))

(defun sudoku-remaining-cells (board)
 "Tests to see how many cells are remaining"
 (let ((remaining 0))
  (dolist (row board)
    (setq remaining (+ remaining (count 0 row))))
  remaining))

(defun sudoku-insert-victory-msg ()
 ;; LGC- added this fn on 06 Dec 2005
 (let ((11-spaces "           ")
      (victory-message "YOU WIN!"))
  (put-text-property 0 8 'face 'bold victory-message)
  (save-excursion
    (goto-line 15)
    (ding)
    (insert ?\n 11-spaces victory-message ?\n ?\n ?\n))))

(defun sudoku-completion-routine ()
 "Runs when there are no cells remaining. Gives a message of
victory, and then asks if you want to play again."
 (setq sudoku-win-state t  buffer-read-only nil)
 (sudoku-insert-victory-msg) ;; LGC- changed this part of fn
 (setq buffer-read-only t)
 (if (y-or-n-p "Start another puzzle? ")
  ;; LGC- provided helpful responses to user's answer.
  (let* ((source (if (string= sudoku-puzzle-source "web-site")
                   "downloaded"
                   sudoku-puzzle-source ))
         (pzl-type (if (string= source "custom")
                     (concat " " source)
                     (concat ", " sudoku-level ", " source)) ))
    (sudoku-new)
    (message "Ok, started a new%s puzzle." pzl-type) )
  ;; LGC- Don't quit immediately here; the user will not always want to.
  ;; For example, (s)he may want to make a menu choice and then continue.
  ;; But the y/n prompt for a new game precludes an immediate menu choice.
  ;; So the only option would be to answer no, use menu, then start a new
  ;; puzzle.  Use of new sudoku-win-state now prevents the user from causing
  ;; various problems for the program by tampering with a completed board.
  (message "Ok, Sudoku menu options may now be changed.")
  ))

(defun sudoku-hint ()
 (interactive)
 (let* ((cell (sudoku-get-cell-from-point (point)))
       (cell-x (car cell))
       (cell-y (car (cdr cell)))
        ;; LGC- next line avoids calling sudoku-cell-possibles twice.
       (poss-vals (sudoku-cell-possibles current-board cell-x cell-y)) )
  (cond ((/= (sudoku-cell start-board cell-x cell-y) 0)
           (message "Original value.  No other possibilities."))
        ((null poss-vals)
           ;; LGC- added this clause to handle certain errors that can arise.
           ;;      This provides the user with valuable info, and avoids a
           ;;      potential bug in the following cond-clause.
           (message "There are no possible values.  Something is wrong!!")
           (ding))
        (t (let ((string nil))
           (dolist (n (cdr poss-vals))
             (setq string (concat (int-to-string n) "," string)))
           (let ((last (int-to-string (car poss-vals))))
             (setq string (concat string last)))
           (message "Possible values: %s" string))) )))

(defun sudoku-cells-of-interest ()
 "Finds some interesting cells (with only one possible filler)"
 ;; LGC- added this fn on 30 Nov 2005
 ;; New simpler version 09 Dec 2005
 ;; LGC- This fn currently uses only those constraints on a cell that arise
 ;;  from its own proper row, column, and region. In the future, I hope to
 ;;  enhance it to take account of some other well known, but often
 ;;  tedious-to-search-for, constraints that make use of information from
 ;;  other parts of the puzzle.
 (let ((singles)
       (possibles))
   (dotimes (index (length cell-point-list))
     (let* ((cell (sudoku-number-to-cell index))
            (x (car cell))
            (y (cadr cell)))
       (when (= (sudoku-cell start-board x y) 0)
         (setq possibles (sudoku-cell-possibles current-board x y))
         (if (and (= 1 (length possibles))
                  (= (sudoku-cell current-board x y) 0))
           (setq singles (cons cell singles))) )))
   (nreverse singles) ) )

(defun sudoku-list-interesting-cells ()
 ;; LGC- added this fn on 30 Nov 2005
 "Lists some interesting cells (with only one possible filler)
 beneath the sudoku display grid."
 (interactive)
 (let ((intrst-list (sudoku-cells-of-interest))
       (intro "Interesting cells (row col):") )
   (setq buffer-read-only nil)
   (save-excursion
     (goto-line 15)
     (when (and (not (looking-at "Interesting cells"))
                (search-forward "(row col):" nil t) )
       ;; if hint is present below a "YOU WIN!" display, remove it.
       (sudoku-remove-intrstg-cells-hint)
       (goto-line 15) )
     (cond ((looking-at "Interesting cells")
              (move-to-column 28)
              ;; LGC- kill rest of line if there's something to kill.
              (if (not (eolp)) (kill-line))
              (forward-line)
              (while (looking-at "(" )
                (kill-line 1) ))
           (t (put-text-property 0 (length intro) 'face 'bold intro)
              (insert intro ?\n ?\n ?\n) ))
     (goto-line 15)
     (end-of-line)
     (while intrst-list
       (insert (sudoku-get-cell-rc-string (car intrst-list)))
       (setq intrst-list (cdr intrst-list))
       (if intrst-list (insert ", "))
       (if (and intrst-list (> (current-column) 70)) (insert ?\n)) )
     (setq buffer-read-only t) )))

(defun sudoku-remove-intrstg-cells-hint ()
 ;; LGC- added this fn on 30 Nov 2005
 "Removes the \"Interesting Cells\" hint from the *Sudoku* buffer."
 (interactive)
 (let ((ro-save-state buffer-read-only))
   (setq buffer-read-only nil)
   (save-excursion
     (goto-line 15)  ;; the normal start of the "Interesting cells" display
     (while (and (not (looking-at "Interest\\|Arrow"))
                 (not (eobp)))
       ;; move through the "YOU WIN!" display if present
       (forward-line) )
     (when (looking-at "Interesting cells")
       ;; remove display of hint
       (kill-line 1)
       (while (looking-at "(" )
         (kill-line 1) )
       (kill-line 2) ))
   (setq buffer-read-only ro-save-state) ))

(defun sudoku-get-cell-rc-string (cell)
 ;; LGC- added this fn on 30 Nov 2005
 "Constructs a display string for cell in (row column) notation:
 (2 3) => (3 2)"
 (concat "(" (int-to-string (cadr cell))
         " " (int-to-string (car cell)) ")" ) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Motion functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sudoku-goto-cell (coords)
 "Moves to a given pair of coordinates."
 ;; LGC- start by updating the Emacs mode-line to new cell
 (setq sudoku-current-cell (sudoku-get-cell-rc-string coords))
 (force-mode-line-update)
 ;; LGC- then move to cell
 (goto-char (sudoku-get-point-from-cell coords)))

(defun sudoku-move-point (direction)
 "Moves in one of four directions: either left, right, up, or
down. Uses sudoku-goto-cell, but doesn't let you go outside the
bounds of the board."
 (let* ((cell (sudoku-get-cell-from-point (point)))
             (x (car cell))
             (y (car (cdr cell))))
  (cond ((null cell)
         ; LGC- added to handle error condition that sometimes arises
          (message "Cursor strayed off grid!  Returning to center.")
          (ding)
          (setq x 4 y 4))
        ((string= direction "left")
         (when (> x 0)
             (setq x (- x 1))))
        ((string= direction "leftmost")
         (setq x 0))
        ((string= direction "right")
         (when (< x 8)
             (setq x (+ x 1))))
        ((string= direction "rightmost")
         (setq x 8))
        ((string= direction "up")
         (when (> y 0)
             (setq y (- y 1))))
        ((string= direction "upmost")
         (setq y 0))
        ((string= direction "down")
         (when (< y 8)
             (setq y (+ y 1))))
        ((string= direction "downmost")
         (setq y 8)))
  (sudoku-goto-cell (list x y))))

(defun sudoku-move-point-left ()
 "Moves the point one cell left."
 (interactive)
 (sudoku-move-point "left"))

(defun sudoku-move-point-leftmost ()
 "Moves the point to the leftmost cell."
 (interactive)
 (sudoku-move-point "leftmost"))

(defun sudoku-move-point-right ()
 "Moves the point one cell right."
 (interactive)
 (sudoku-move-point "right"))

(defun sudoku-move-point-rightmost ()
 "Moves the point to the rightmost cell."
 (interactive)
 (sudoku-move-point "rightmost"))

(defun sudoku-move-point-up ()
 "Moves the point one cell up."
 (interactive)
 (sudoku-move-point "up"))

(defun sudoku-move-point-upmost ()
 "Moves the point to the upmost cell."
 (interactive)
 (sudoku-move-point "upmost"))

(defun sudoku-move-point-down ()
 "Moves the point one cell down."
 (interactive)
 (sudoku-move-point "down"))

(defun sudoku-move-point-downmost ()
 "Moves the point to the downmost cell."
 (interactive)
 (sudoku-move-point "downmost"))

(defun sudoku-disabled-key ()
 (interactive)
 (message "Disabled in Sudoku mode"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;For downloading new puzzles (requires lynx)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sudoku-download-new-puzzle (level)
 "Uses sudoku-get-board and the parsing functions to return a
 new board from the web. The levels can be either \"easy\",
 \"medium\", \"hard\", or \"evil\"."
 (save-excursion
   (let ((source (concat "http://play.websudoku.com/?level="
                         (int-to-string level) )))
     (cond ((string= sudoku-download-method "native-url-lib")
            (get-board-native source))
           ((string= sudoku-download-method "w3")
            (get-board-w3 source))
           ((string= sudoku-download-method "lynx")
            (get-board-lynx source))
           ((string= sudoku-download-method "wget")
            (get-board-wget source))))))

(defun get-board-lynx (source)
 "Downloads a websudoku html file into a temp buffer using lynx
and turns it into a list. Used by sudoku-download-new-puzzle."
 (with-temp-buffer
   (call-process "lynx"
                 nil t nil
                 "--source" source)
   (sudoku-html-to-list)))

(defun get-board-wget (source)
 "Downloads a websudoku html file into a temp buffer using wget
and turns it into a list. Used by sudoku-download-new-puzzle."
 (with-temp-buffer
   (call-process sudoku-wget-process
                 nil t nil
                 "-q" "-O" "-" source)
   (sudoku-html-to-list)))

(defun get-board-native (source)
 "Downloads a websudoku html file into a temp buffer using the
native emacs url library (emacs >= 22), or downloaded from cvs
and turns it into a list. Used by sudoku-download-new-puzzle."
 (unless (featurep 'url)
   (require 'url))
 (save-excursion
   (set-buffer (url-retrieve-synchronously source))
   (sudoku-html-to-list)))

;; Adapted from code submitted by Wojciech Komornicki. THANKS!
(defun get-board-w3 (source)
 "Downloads a websudoku html file into a temp buffer using the
url retrieval library from w3/emacs (seems to be obsolete now),
and turns it into a list. Used by sudoku-download-new-puzzle."
   (unless (featurep 'w3)
    (require 'w3))
   (with-temp-buffer
     (url-retrieve source)
     (sudoku-html-to-list)))

(defun sudoku-html-to-list ()
 "Assumes you are in a separate buffer, into which the websudoku
html has been downloaded. Split out because some of the routines
can use `with-temp-buffer' and others seem to require a
`set-buffer'. Used by the different get-board-* functions."
 ;; LGC- simplified this fn on 14 Dec 2005
 (cut-to-table)
 (sudoku-get-board-from-html-table) )

(defun cut-to-table ()
 "Cuts everything out but the html table containing the puzzle. Used by
 sudoku-html-to-list"
 (save-excursion
   (goto-char (point-min))
   (delete-region (point-min)
                  (search-forward
                     "<FORM NAME=\"board\" METHOD=POST ACTION=\"./\">"))
   (kill-line 1)
   (delete-region (search-forward "</TABLE>") (point-max))))

(defun sudoku-get-board-from-html-table ()
 ;; LGC- added this fn on 14 Dec 2005
 "Extracts a Sudoku puzzle-matrix from a downloaded HTML table."
 (let ((ic-begin t)
       (class) (val-begin) (value) (values) (board) )
   ;; collect all 81 cell-values in a list.
   (goto-char (point-min))
   (while ic-begin
     (setq ic-begin (re-search-forward "<INPUT CLASS=\\([ds]0\\)" nil t)
           class (buffer-substring (match-beginning 1) (match-end 1)) )
     (when (string= class "s0")
       (setq val-begin (re-search-forward "VALUE=\"\\([1-9]\\)\"" nil t)
             value (string-to-number (buffer-substring (match-beginning 1)
                                                    (match-end 1) )) ))
     (when ic-begin
       (setq values (cons (cond ((string= class "s0") value)
                                ((string= class "d0") 0) )
                          values ))))
   ;; divide the cell-value list up into 9-member rows.
   (while values
     (let ((row)
           (index 0) )
       (while (and values (< index 9))
         (setq row (cons (car values) row)
               index (1+ index)
               values (cdr values) ))
       (setq board (cons row board)
             row nil
             index 1 )))
   board ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Puzzles. About 200.
;;50 each of easy, medium, hard, and evil
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar evil-puzzles '(
((0 0 0 0 0 6 0 8 0) (0 1 0 0 2 0 6 0 0) (8 0 0 0 0 1 0 0 2) (3 0
0 0 0 0 0 1 7) (0 4 0 3 0 5 0 2 0) (7 6 0 0 0 0 0 0 8) (4 0 0 7 0
0 0 0 5) (0 0 2 0 4 0 0 6 0) (0 3 0 5 0 0 0 0 0))

((0 1 0 0 4 0 3 0 2) (0 0 9 0 3 0 0 0 0) (7 0 0 0 0 0 0 8 0) (0 0 7 0
2 1 0 0 0) (9 0 0 0 8 0 0 0 6) (0 0 0 3 5 0 1 0 0) (0 5 0 0 0 0 0 0 7)
(0 0 0 0 7 0 6 0 0) (8 0 4 0 6 0 0 3 0))

((0 0 0 7 0 0 3 0 0) (0 5 0 0 0 4 2 0 0) (8 0 0 2 0 5 0 0 7) (0 0 4 0
0 0 7 0 8) (3 0 0 0 0 0 0 0 5) (6 0 8 0 0 0 4 0 0) (4 0 0 6 0 7 0 0 1)
(0 0 2 8 0 0 0 7 0) (0 0 1 0 0 2 0 0 0))

((0 0 4 0 0 0 0 5 8) (0 0 0 5 0 0 0 0 0) (5 0 0 8 0 3 9 6 0) (0 0 0 0
7 0 0 2 0) (0 8 2 0 0 0 5 1 0) (0 9 0 0 6 0 0 0 0) (0 5 9 7 0 6 0 0 4)
(0 0 0 0 0 1 0 0 0) (8 2 0 0 0 0 7 0 0))

((5 0 0 0 0 0 0 0 0) (0 0 0 8 0 0 7 0 5) (9 0 0 1 0 0 3 8 4) (0 0 0 9
1 0 0 7 0) (0 0 3 0 0 0 4 0 0) (0 7 0 0 6 2 0 0 0) (3 5 6 0 0 1 0 0 2)
(1 0 2 0 0 6 0 0 0) (0 0 0 0 0 0 0 0 1))

((0 2 3 1 9 0 0 4 0) (0 0 0 4 0 0 0 0 3) (0 0 0 6 0 0 0 1 0) (8 0 0 0
0 0 7 0 0) (0 0 6 0 0 0 1 0 0) (0 0 2 0 0 0 0 0 5) (0 5 0 0 0 8 0 0 0)
(6 0 0 0 0 4 0 0 0) (0 7 0 0 3 5 6 9 0))

((9 0 0 0 0 0 0 0 0) (8 0 0 7 0 0 1 5 2) (0 0 0 1 0 0 0 4 9) (0 0 0 8
7 0 4 0 0) (0 5 0 0 0 0 0 2 0) (0 0 4 0 6 3 0 0 0) (7 3 0 0 0 6 0 0 0)
(5 6 9 0 0 7 0 0 3) (0 0 0 0 0 0 0 0 7))

((4 0 0 0 0 6 0 9 0) (0 0 0 3 0 0 1 0 0) (1 6 0 0 8 0 2 0 0) (0 0 8 0
0 1 0 0 0) (0 2 0 8 0 5 0 4 0) (0 0 0 4 0 0 8 0 0) (0 0 3 0 4 0 0 2 5)
(0 0 7 0 0 9 0 0 0) (0 5 0 6 0 0 0 0 3))

((0 6 0 1 7 0 0 0 0) (0 9 0 0 0 0 7 0 0) (2 0 0 0 6 0 0 0 1) (0 8 0 0
0 0 0 0 0) (7 5 3 0 1 0 6 8 4) (0 0 0 0 0 0 0 9 0) (9 0 0 0 8 0 0 0 3)
(0 0 4 0 0 0 0 2 0) (0 0 0 0 9 6 0 4 0))

((0 8 5 0 6 0 0 0 0) (0 6 0 0 9 2 0 0 1) (9 0 0 8 0 0 0 0 0) (3 0 0 0
0 0 9 1 0) (0 0 8 0 0 0 7 0 0) (0 7 6 0 0 0 0 0 3) (0 0 0 0 0 8 0 0 5)
(5 0 0 2 3 0 0 6 0) (0 0 0 0 7 0 2 3 0))

((0 0 0 0 7 0 5 0 0) (5 0 0 9 0 0 0 3 0) (0 0 8 0 0 4 0 0 9) (0 1 2 0
0 0 7 0 0) (0 7 0 0 2 0 0 1 0) (0 0 5 0 0 0 4 2 0) (8 0 0 6 0 0 1 0 0)
(0 4 0 0 0 2 0 0 3) (0 0 3 0 1 0 0 0 0))

((6 0 0 0 0 0 0 0 0) (0 9 5 8 6 2 0 0 0) (0 3 7 4 0 0 0 0 0) (0 6 0 0
0 0 4 0 3) (0 0 0 7 0 4 0 0 0) (2 0 1 0 0 0 0 8 0) (0 0 0 0 0 7 6 2 0)
(0 0 0 2 8 5 9 7 0) (0 0 0 0 0 0 0 0 1))

((0 0 3 0 8 1 0 0 0) (0 5 6 0 0 9 0 0 0) (2 0 0 6 0 0 0 9 0) (7 0 0 0
0 0 0 3 0) (0 6 8 0 0 0 5 1 0) (0 3 0 0 0 0 0 0 7) (0 2 0 0 0 6 0 0 1)
(0 0 0 8 0 0 7 5 0) (0 0 0 5 7 0 2 0 0))

((0 0 8 0 0 6 0 0 2) (0 9 0 0 0 4 0 0 0) (0 4 0 0 5 0 0 0 0) (0 0 3 0
0 5 0 2 6) (5 0 0 0 2 0 0 0 3) (7 6 0 1 0 0 4 0 0) (0 0 0 0 3 0 0 8 0)
(0 0 0 4 0 0 0 3 0) (8 0 0 5 0 0 6 0 0))

((8 0 0 0 0 0 1 0 0) (9 0 4 0 0 1 0 0 0) (0 6 0 0 0 4 0 0 0) (0 0 0 0
9 5 0 8 0) (6 0 9 0 7 0 2 0 3) (0 2 0 6 4 0 0 0 0) (0 0 0 5 0 0 0 7 0)
(0 0 0 8 0 0 3 0 6) (0 0 1 0 0 0 0 0 2))

((9 2 0 0 5 7 0 0 0) (0 0 7 2 0 0 0 0 0) (4 0 3 0 0 0 0 0 0) (2 0 0 0
0 9 0 1 6) (0 0 4 0 0 0 5 0 0) (7 1 0 6 0 0 0 0 2) (0 0 0 0 0 0 3 0 9)
(0 0 0 0 0 2 7 0 0) (0 0 0 8 4 0 0 5 1))

((2 0 4 0 5 0 0 7 0) (0 0 0 3 0 0 0 2 0) (1 0 0 0 0 4 9 0 0) (0 5 0 0
0 2 0 0 0) (0 0 7 5 0 8 1 0 0) (0 0 0 1 0 0 0 5 0) (0 0 8 4 0 0 0 0 3)
(0 6 0 0 0 9 0 0 0) (0 3 0 0 1 0 7 0 8))

((0 9 0 0 7 0 0 1 0) (0 0 4 2 0 5 0 0 0) (8 0 0 1 0 0 0 0 6) (7 0 0 0
0 4 0 5 0) (0 0 5 0 0 0 4 0 0) (0 4 0 7 0 0 0 0 9) (9 0 0 0 0 1 0 0 7)
(0 0 0 5 0 9 3 0 0) (0 1 0 0 2 0 0 9 0))

((0 4 0 0 8 5 0 9 0) (0 0 0 7 0 0 0 0 5) (0 0 0 0 2 0 0 0 8) (6 0 0 0
0 4 3 0 0) (0 5 3 0 0 0 2 7 0) (0 0 8 6 0 0 0 0 1) (7 0 0 0 6 0 0 0 0)
(1 0 0 0 0 7 0 0 0) (0 3 0 5 4 0 0 2 0))

((1 0 0 4 0 0 0 0 0) (0 5 0 0 7 0 0 0 9) (0 0 7 2 0 0 4 0 0) (9 0 2 0
0 0 3 0 0) (7 0 0 1 0 4 0 0 5) (0 0 1 0 0 0 2 0 6) (0 0 3 0 0 6 5 0 0)
(6 0 0 0 5 0 0 9 0) (0 0 0 0 0 9 0 0 3))

((6 0 0 0 0 4 1 9 0) (0 0 0 0 0 0 0 0 0) (1 4 0 3 8 0 2 0 0) (2 0 0 0
0 0 8 0 0) (9 0 0 4 0 6 0 0 5) (0 0 4 0 0 0 0 0 6) (0 0 1 0 5 8 0 7 2)
(0 0 0 0 0 0 0 0 0) (0 7 5 2 0 0 0 0 3))

((0 0 0 7 0 3 9 6 0) (0 2 0 0 0 0 0 0 0) (3 0 0 0 4 5 0 0 7) (6 0 8 0
0 0 0 3 5) (0 0 0 0 0 0 0 0 0) (2 7 0 0 0 0 6 0 8) (8 0 0 3 2 0 0 0 9)
(0 0 0 0 0 0 0 8 0) (0 6 4 8 0 7 0 0 0))

((0 0 0 0 0 0 9 6 0) (0 0 0 8 3 0 1 0 4) (0 0 0 0 0 7 0 2 0) (1 0 2 5
0 0 7 0 0) (0 3 0 0 0 0 0 4 0) (0 0 7 0 0 9 5 0 1) (0 2 0 7 0 0 0 0 0)
(7 0 9 0 4 2 0 0 0) (0 6 3 0 0 0 0 0 0))

((0 3 0 0 0 5 7 0 0) (8 0 0 3 0 6 0 0 9) (0 0 0 7 0 0 8 0 0) (0 0 9 0
0 0 5 0 6) (4 0 0 0 0 0 0 0 2) (5 0 3 0 0 0 9 0 0) (0 0 2 0 0 3 0 0 0)
(3 0 0 4 0 7 0 0 5) (0 0 7 9 0 0 0 4 0))

((0 0 0 0 3 7 4 5 0) (0 6 0 0 0 0 8 0 0) (0 0 1 0 4 0 0 9 0) (3 5 0 0
0 0 0 2 0) (0 0 0 5 0 1 0 0 0) (0 1 0 0 0 0 0 8 4) (0 9 0 0 7 0 6 0 0)
(0 0 8 0 0 0 0 4 0) (0 4 7 9 8 0 0 0 0))

((0 0 0 0 0 9 3 0 0) (0 0 1 4 3 0 0 7 0) (0 0 0 0 7 0 0 9 2) (3 1 0 0
0 0 5 0 0) (6 0 0 0 0 0 0 0 9) (0 0 5 0 0 0 0 6 7) (4 5 0 0 6 0 0 0 0)
(0 7 0 0 5 4 2 0 0) (0 0 2 9 0 0 0 0 0))

((6 0 3 0 0 0 0 0 0) (0 0 0 0 0 3 4 6 1) (4 0 0 0 0 0 9 0 3) (0 0 8 5
9 0 0 7 0) (0 0 0 0 0 0 0 0 0) (0 4 0 0 2 8 1 0 0) (1 0 7 0 0 0 0 0 5)
(2 3 5 9 0 0 0 0 0) (0 0 0 0 0 0 3 0 8))

((0 0 0 0 0 0 7 3 0) (0 2 0 0 8 0 4 0 0) (6 0 5 0 0 1 0 0 0) (0 4 0 3
1 0 0 0 5) (0 0 3 0 0 0 1 0 0) (9 0 0 0 2 7 0 4 0) (0 0 0 1 0 0 8 0 7)
(0 0 8 0 3 0 0 9 0) (0 5 2 0 0 0 0 0 0))

((0 4 0 0 8 0 0 0 5) (8 0 0 9 4 0 0 0 0) (9 1 0 6 0 0 0 0 0) (0 0 4 0
0 0 1 0 0) (5 3 0 0 0 0 0 6 7) (0 0 8 0 0 0 3 0 0) (0 0 0 0 0 3 0 7 1)
(0 0 0 0 2 8 0 0 9) (6 0 0 0 1 0 0 2 0))

((0 0 0 1 0 0 6 0 0) (0 4 1 0 0 0 0 0 0) (5 7 0 9 0 0 0 3 0) (0 0 0 5
0 6 3 0 9) (9 0 0 0 0 0 0 0 8) (2 0 4 7 0 9 0 0 0) (0 2 0 0 0 1 0 5 6)
(0 0 0 0 0 0 2 7 0) (0 0 9 0 0 5 0 0 0))

((0 0 6 4 0 0 0 1 0) (2 0 0 0 3 9 0 0 0) (4 7 0 0 0 1 0 0 0) (0 0 5 0
0 0 0 2 0) (3 4 0 0 0 0 0 9 7) (0 2 0 0 0 0 5 0 0) (0 0 0 3 0 0 0 7 5)
(0 0 0 7 5 0 0 0 6) (0 6 0 0 0 4 9 0 0))

((0 0 0 0 0 1 7 3 0) (0 0 0 9 6 4 8 2 0) (0 0 0 0 0 0 0 0 6) (7 1 0 0
0 0 6 0 0) (0 0 0 1 0 3 0 0 0) (0 0 4 0 0 0 0 5 9) (5 0 0 0 0 0 0 0 0)
(0 8 3 2 4 9 0 0 0) (0 6 9 3 0 0 0 0 0))

((0 5 0 0 0 1 6 0 0) (0 8 0 0 2 0 0 5 0) (4 0 0 0 0 0 0 0 0) (0 6 1 5
0 0 0 0 9) (0 0 0 1 0 8 0 0 0) (3 0 0 0 0 6 5 7 0) (0 0 0 0 0 0 0 0 7)
(0 3 0 0 4 0 0 8 0) (0 0 9 7 0 0 0 2 0))

((4 0 0 0 0 0 0 0 0) (0 0 9 7 0 6 8 0 0) (0 6 0 3 9 0 0 0 4) (0 4 3 0
8 0 0 0 0) (9 0 0 0 0 0 0 0 1) (0 0 0 0 5 0 7 8 0) (3 0 0 0 1 4 0 5 0)
(0 0 6 5 0 7 1 0 0) (0 0 0 0 0 0 0 0 9))

((7 0 0 0 3 0 0 0 0) (0 0 0 0 8 5 0 6 2) (0 0 2 0 4 0 1 0 0) (0 0 0 0
0 0 0 9 0) (0 2 5 0 9 0 6 4 0) (0 6 0 0 0 0 0 0 0) (0 0 3 0 7 0 9 0 0)
(1 7 0 2 5 0 0 0 0) (0 0 0 0 6 0 0 0 4))

((0 3 0 0 7 1 0 0 0) (8 0 0 0 0 0 0 3 0) (0 0 5 9 0 2 0 0 7) (2 0 0 0
0 0 0 1 0) (0 0 9 0 1 0 7 0 0) (0 8 0 0 0 0 0 0 4) (6 0 0 1 0 5 3 0 0)
(0 5 0 0 0 0 0 0 6) (0 0 0 7 9 0 0 2 0))

((5 0 1 0 7 0 0 0 0) (0 4 0 0 0 8 0 0 0) (7 3 0 4 0 1 0 0 0) (0 0 4 0
0 9 0 2 0) (0 6 0 0 0 0 0 7 0) (0 1 0 3 0 0 4 0 0) (0 0 0 9 0 4 0 8 3)
(0 0 0 8 0 0 0 6 0) (0 0 0 0 6 0 1 0 5))

((0 0 4 0 0 0 8 0 7) (0 3 0 6 0 0 0 0 0) (1 0 0 2 0 0 0 0 0) (0 7 0 4
0 0 0 0 2) (8 2 0 0 7 0 0 9 6) (6 0 0 0 0 2 0 3 0) (0 0 0 0 0 9 0 0 8)
(0 0 0 0 0 5 0 7 0) (5 0 2 0 0 0 1 0 0))

((0 1 0 0 0 6 0 7 0) (0 0 6 0 5 0 1 0 0) (0 0 0 9 0 1 0 0 4) (0 0 2 7
0 0 0 1 0) (9 0 0 0 0 0 0 0 2) (0 7 0 0 0 2 9 0 0) (2 0 0 5 0 9 0 0 0)
(0 0 1 0 7 0 6 0 0) (0 3 0 6 0 0 0 8 0))

((0 0 0 0 0 0 8 0 6) (0 0 0 7 0 4 0 0 0) (0 0 0 0 1 0 0 2 7) (3 0 0 0
5 0 2 0 0) (0 4 0 1 3 8 0 7 0) (0 0 9 0 6 0 0 0 5) (6 9 0 0 7 0 0 0 0)
(0 0 0 9 0 3 0 0 0) (8 0 2 0 0 0 0 0 0))

((0 0 0 1 0 0 9 8 0) (0 0 0 0 0 6 0 0 5) (1 0 0 5 8 0 0 0 0) (5 6 0 7
0 0 0 0 0) (0 0 7 0 1 0 8 0 0) (0 0 0 0 0 4 0 6 7) (0 0 0 0 7 3 0 0 9)
(9 0 0 8 0 0 0 0 0) (0 2 6 0 0 5 0 0 0))

((0 0 0 0 6 1 5 0 0) (5 0 0 0 0 0 7 0 0) (0 6 0 0 9 0 0 4 0) (0 0 0 0
0 0 6 0 0) (4 3 8 0 2 0 9 5 1) (0 0 9 0 0 0 0 0 0) (0 7 0 0 1 0 0 2 0)
(0 0 6 0 0 0 0 0 3) (0 0 1 2 3 0 0 0 0))

((8 0 0 0 5 0 0 0 9) (0 0 3 0 6 0 0 0 0) (0 0 0 0 2 1 8 4 0) (0 0 0 0
0 0 0 7 0) (1 8 0 0 7 0 0 5 4) (0 4 0 0 0 0 0 0 0) (0 3 9 8 1 0 0 0 0)
(0 0 0 0 4 0 5 0 0) (6 0 0 0 3 0 0 0 7))

((0 1 6 0 0 8 0 0 0) (3 0 0 0 0 6 0 0 0) (0 7 0 0 0 0 8 0 0) (0 0 0 0
1 5 0 0 7) (0 3 1 0 4 0 9 2 0) (9 0 0 3 6 0 0 0 0) (0 0 8 0 0 0 0 9 0)
(0 0 0 5 0 0 0 0 4) (0 0 0 7 0 0 2 3 0))

((7 8 2 3 0 0 0 0 0) (3 0 5 0 0 0 0 0 2) (0 0 0 0 0 0 3 0 8) (0 4 0 0
5 6 1 0 0) (0 0 0 0 0 0 0 0 0) (0 0 7 1 9 0 0 2 0) (1 0 3 0 0 0 0 0 0)
(6 0 0 0 0 0 4 0 7) (0 0 0 0 0 5 6 3 9))

((1 0 0 0 0 0 0 0 4) (0 0 0 3 0 0 8 0 0) (4 9 0 0 0 8 0 1 5) (0 0 0 7
0 0 0 4 2) (0 0 0 8 0 2 0 0 0) (2 4 0 0 0 3 0 0 0) (5 8 0 2 0 0 0 9 6)
(0 0 2 0 0 5 0 0 0) (9 0 0 0 0 0 0 0 1))

((0 0 0 0 0 9 4 1 0) (0 5 0 0 0 0 3 0 0) (0 1 2 8 6 0 0 0 0) (0 9 0 0
8 0 7 0 0) (0 6 0 0 0 0 0 9 0) (0 0 1 0 7 0 0 2 0) (0 0 0 0 3 1 8 4 0)
(0 0 5 0 0 0 0 7 0) (0 4 6 7 0 0 0 0 0))

((6 0 4 0 0 0 0 0 0) (5 0 0 3 0 0 0 0 0) (0 3 1 0 2 5 0 0 0) (0 0 3 0
0 1 9 7 0) (4 0 0 0 0 0 0 0 2) (0 7 5 9 0 0 3 0 0) (0 0 0 8 4 0 7 2 0)
(0 0 0 0 0 3 0 0 5) (0 0 0 0 0 0 1 0 6))

((0 0 1 0 0 0 3 6 0) (0 0 8 0 7 1 0 0 0) (0 0 0 4 0 0 0 0 7) (5 7 9 0
0 0 0 0 0) (0 0 2 0 0 0 8 0 0) (0 0 0 0 0 0 6 4 5) (8 0 0 0 0 6 0 0 0)
(0 0 0 3 9 0 7 0 0) (0 9 5 0 0 0 2 0 0))

((6 5 0 0 4 0 1 0 0) (0 0 0 0 3 0 0 4 0) (0 0 8 0 0 0 0 0 3) (0 0 0 1
8 0 0 7 0) (9 0 0 0 6 0 0 0 4) (0 3 0 0 2 7 0 0 0) (3 0 0 0 0 0 6 0 0)
(0 9 0 0 1 0 0 0 0) (0 0 7 0 5 0 0 1 2))

((0 0 0 0 3 0 0 9 4) (0 0 0 0 0 0 8 7 0) (0 0 0 2 0 4 0 0 0) (0 1 0 0
9 0 4 0 0) (3 0 0 7 2 5 0 0 6) (0 0 8 0 1 0 0 2 0) (0 0 0 6 0 3 0 0 0)
(0 9 7 0 0 0 0 0 0) (8 3 0 0 5 0 0 0 0))

((0 6 0 0 0 0 9 0 0) (0 0 0 5 2 9 1 0 0) (0 0 0 0 0 4 0 0 3) (0 0 0 7
0 0 8 9 0) (0 8 0 0 0 0 0 5 0) (0 2 1 0 0 6 0 0 0) (3 0 0 9 0 0 0 0 0)
(0 0 9 1 4 8 0 0 0) (0 0 7 0 0 0 0 8 0))

((0 0 6 0 7 0 5 0 0) (0 0 3 0 0 1 0 4 0) (1 0 0 0 0 0 0 0 0) (0 2 1 9
0 0 0 0 5) (0 0 0 6 0 8 0 0 0) (4 0 0 0 0 2 9 8 0) (0 0 0 0 0 0 0 0 7)
(0 9 0 8 0 0 2 0 0) (0 0 2 0 3 0 6 0 0))

((8 1 0 0 0 0 3 0 0) (0 3 0 0 9 4 0 0 0) (0 0 4 0 7 0 0 0 8) (0 0 0 0
0 5 0 0 6) (0 6 8 0 0 0 7 4 0) (4 0 0 7 0 0 0 0 0) (6 0 0 0 5 0 1 0 0)
(0 0 0 6 1 0 0 9 0) (0 0 3 0 0 0 0 5 2))))

(defvar hard-puzzles '( ((3 0 0 0 9 0 4 0 0) (0 9 2 0 0 3 0 0 1) (0 5
0 8 0 0 0 0 0) (0 0 0 0 8 0 0 3 0) (9 0 4 7 0 1 6 0 8) (0 6 0 0 3 0 0
0 0) (0 0 0 0 0 8 0 6 0) (5 0 0 1 0 0 2 7 0) (0 0 9 0 2 0 0 0 5))

((0 0 0 3 1 6 0 0 8) (9 0 0 2 0 0 0 6 0) (8 0 6 0 0 0 0 3 0) (3 0 0 1
0 0 0 0 0) (6 1 0 0 0 0 0 2 9) (0 0 0 0 0 4 0 0 7) (0 6 0 0 0 0 8 0 4)
(0 2 0 0 0 5 0 0 3) (1 0 0 7 8 3 0 0 0))

((0 7 2 8 0 0 0 0 0) (0 0 8 5 0 9 0 6 0) (5 0 0 0 1 0 0 0 3) (0 4 0 6
9 0 0 0 0) (0 6 0 0 0 0 0 4 0) (0 0 0 0 4 7 0 3 0) (6 0 0 0 3 0 0 0 2)
(0 3 0 4 0 6 1 0 0) (0 0 0 0 0 8 3 7 0))

((0 0 9 2 0 0 0 0 0) (6 0 0 1 0 0 0 0 2) (0 2 1 0 3 0 0 0 0) (0 3 7 0
0 0 0 1 0) (0 5 0 9 0 3 0 8 0) (0 9 0 0 0 0 4 3 0) (0 0 0 0 2 0 3 5 0)
(9 0 0 0 0 7 0 0 1) (0 0 0 0 0 8 9 0 0))

((0 0 5 0 0 4 0 0 3) (0 0 1 9 0 6 0 0 0) (8 4 0 0 0 0 0 0 9) (0 3 0 6
0 2 0 0 0) (0 6 0 7 0 1 0 8 0) (0 0 0 4 0 3 0 7 0) (4 0 0 0 0 0 0 5 1)
(0 0 0 5 0 7 2 0 0) (5 0 0 3 0 0 4 0 0))

((0 0 0 0 0 3 0 0 0) (0 0 1 0 7 0 5 8 0) (9 0 0 0 6 0 0 2 0) (5 0 0 0
0 0 4 0 7) (0 2 0 0 0 0 0 5 0) (6 0 8 0 0 0 0 0 9) (0 5 0 0 3 0 0 0 1)
(0 7 9 0 4 0 6 0 0) (0 0 0 8 0 0 0 0 0))

((0 0 7 9 0 0 4 6 0) (0 8 0 0 7 0 3 0 0) (9 0 0 2 0 1 0 0 0) (7 0 0 0
0 0 9 1 0) (0 0 0 0 0 0 0 0 0) (0 9 6 0 0 0 0 0 8) (0 0 0 1 0 8 0 0 7)
(0 0 8 0 4 0 0 5 0) (0 1 2 0 0 5 6 0 0))

((0 0 1 0 0 9 0 0 0) (0 0 8 0 0 4 0 3 7) (6 0 0 0 7 0 0 8 0) (0 5 0 0
0 3 0 0 2) (8 0 9 0 0 0 3 0 6) (7 0 0 9 0 0 0 1 0) (0 3 0 0 2 0 0 0 8)
(5 6 0 4 0 0 7 0 0) (0 0 0 6 0 0 2 0 0))

((7 0 0 0 1 2 8 0 0) (0 0 2 6 5 0 0 9 0) (0 0 0 0 0 9 0 0 1) (0 2 0 3
0 0 0 0 0) (0 6 3 0 0 0 1 7 0) (0 0 0 0 0 1 0 2 0) (4 0 0 5 0 0 0 0 0)
(0 9 0 0 2 6 4 0 0) (0 0 8 1 4 0 0 0 9))

((1 0 0 0 0 2 0 9 4) (0 9 0 0 3 1 0 5 0) (0 0 6 0 0 0 1 0 0) (0 0 0 3
9 0 0 0 7) (0 0 0 0 5 0 0 0 0) (7 0 0 0 8 4 0 0 0) (0 0 9 0 0 0 8 0 0)
(0 5 0 6 2 0 0 4 0) (6 7 0 9 0 0 0 0 5))

((0 0 2 0 0 0 0 0 1) (0 1 0 0 8 0 0 0 6) (8 0 7 0 0 9 0 0 0) (0 0 0 0
9 0 0 3 4) (0 0 8 4 7 2 6 0 0) (4 5 0 0 3 0 0 0 0) (0 0 0 1 0 0 9 0 5)
(5 0 0 0 4 0 0 2 0) (7 0 0 0 0 0 4 0 0))

((9 0 3 0 0 6 0 0 0) (0 2 0 0 4 0 0 1 0) (5 8 0 0 0 0 0 0 0) (0 9 0 7
0 2 1 0 0) (1 0 0 0 0 0 0 0 2) (0 0 7 8 0 5 0 4 0) (0 0 0 0 0 0 0 7 6)
(0 6 0 0 7 0 0 8 0) (0 0 0 6 0 0 4 0 5))

((0 0 0 7 0 0 0 5 0) (0 0 0 0 3 0 0 2 8) (0 0 1 0 0 2 9 0 6) (0 1 0 6
0 7 0 0 0) (0 0 7 0 0 0 4 0 0) (0 0 0 8 0 9 0 1 0) (7 0 6 4 0 0 3 0 0)
(1 8 0 0 9 0 0 0 0) (0 4 0 0 0 3 0 0 0))

((6 0 0 4 7 2 0 0 0) (9 2 0 0 0 8 0 0 0) (0 1 0 0 0 0 0 0 0) (2 4 0 0
0 0 0 0 6) (0 6 0 0 3 0 0 7 0) (1 0 0 0 0 0 0 5 3) (0 0 0 0 0 0 0 1 0)
(0 0 0 2 0 0 0 9 4) (0 0 0 6 8 9 0 0 5))

((3 0 0 5 0 0 8 0 4) (0 2 0 7 0 8 0 0 0) (0 0 5 0 9 0 0 0 7) (0 7 0 0
0 0 6 0 3) (0 0 0 0 0 0 0 0 0) (6 0 8 0 0 0 0 2 0) (1 0 0 0 2 0 7 0 0)
(0 0 0 8 0 4 0 6 0) (9 0 3 0 0 6 0 0 2))

((5 0 0 0 0 0 0 1 6) (6 0 0 8 4 0 0 0 0) (0 1 7 0 0 0 0 0 0) (0 0 2 4
3 6 1 8 0) (0 0 0 0 0 0 0 0 0) (0 6 4 9 7 8 3 0 0) (0 0 0 0 0 0 9 6 0)
(0 0 0 0 5 3 0 0 1) (2 8 0 0 0 0 0 0 7))

((0 0 7 0 0 0 0 0 3) (0 0 9 0 0 2 0 1 0) (0 0 4 0 1 0 7 0 5) (8 0 2 3
0 0 0 0 0) (0 0 0 2 8 1 0 0 0) (0 0 0 0 0 6 9 0 2) (6 0 5 0 4 0 2 0 0)
(0 7 0 9 0 0 3 0 0) (4 0 0 0 0 0 8 0 0))

((0 5 0 0 8 0 0 0 9) (9 0 6 0 0 0 5 0 0) (0 0 8 0 1 0 0 0 0) (0 0 0 5
0 0 0 0 4) (3 9 0 0 6 0 0 7 2) (4 0 0 0 0 2 0 0 0) (0 0 0 0 3 0 4 0 0)
(0 0 1 0 0 0 8 0 3) (7 0 0 0 2 0 0 1 0))

((5 9 7 0 0 0 2 0 0) (0 0 6 0 5 2 0 0 0) (0 0 0 4 0 0 8 5 0) (0 0 0 0
3 0 6 0 9) (0 3 0 0 0 0 0 1 0) (4 0 2 0 6 0 0 0 0) (0 6 4 0 0 9 0 0 0)
(0 0 0 8 1 0 4 0 0) (0 0 8 0 0 0 1 9 3))

((3 6 0 0 7 0 0 0 0) (2 0 4 6 0 0 8 0 0) (0 9 0 0 0 1 0 0 0) (0 0 0 1
0 2 0 8 0) (0 0 5 0 0 0 1 0 0) (0 8 0 4 0 3 0 0 0) (0 0 0 7 0 0 0 5 0)
(0 0 7 0 0 5 2 0 1) (0 0 0 0 4 0 0 3 8))

((0 3 4 0 0 2 0 0 0) (0 0 1 3 7 5 0 0 0) (0 9 0 0 0 0 0 0 0) (0 1 6 0
0 0 9 0 0) (0 8 0 0 6 0 0 5 0) (0 0 5 0 0 0 2 4 0) (0 0 0 0 0 0 0 9 0)
(0 0 0 2 8 4 5 0 0) (0 0 0 7 0 0 3 2 0))

((0 0 0 0 4 2 5 0 0) (8 0 0 0 0 6 0 0 3) (4 0 0 8 0 0 0 0 7) (0 0 0 0
0 0 3 0 0) (0 0 9 6 5 4 2 0 0) (0 0 5 0 0 0 0 0 0) (3 0 0 0 0 1 0 0 4)
(7 0 0 4 0 0 0 0 2) (0 0 2 7 9 0 0 0 0))

((0 3 0 0 0 0 0 5 0) (1 0 0 2 9 0 0 0 7) (4 0 2 3 0 0 1 0 0) (0 0 4 0
5 7 0 0 0) (0 0 0 0 1 0 0 0 0) (0 0 0 6 3 0 4 0 0) (0 0 8 0 0 9 7 0 3)
(3 0 0 0 6 8 0 0 1) (0 2 0 0 0 0 0 8 0))

((0 0 7 0 0 0 0 6 5) (0 6 0 5 4 7 0 0 0) (0 0 5 0 0 3 0 2 0) (0 0 0 0
0 4 0 7 0) (0 2 3 0 0 0 4 5 0) (0 1 0 8 0 0 0 0 0) (0 7 0 9 0 0 3 0 0)
(0 0 0 7 6 1 0 4 0) (6 8 0 0 0 0 5 0 0))

((0 0 0 0 0 0 0 0 0) (1 0 0 5 2 0 0 0 0) (0 4 8 0 0 0 9 3 0) (8 0 3 0
6 0 0 9 0) (2 0 0 0 5 0 0 0 3) (0 7 0 0 3 0 6 0 1) (0 3 1 0 0 0 4 8 0)
(0 0 0 0 1 4 0 0 5) (0 0 0 0 0 0 0 0 0))

((1 3 0 0 6 0 0 0 0) (0 0 0 0 0 9 0 2 0) (0 0 6 0 4 0 8 1 0) (0 5 2 0
0 4 0 0 7) (0 0 0 0 5 0 0 0 0) (7 0 0 8 0 0 1 4 0) (0 7 5 0 8 0 4 0 0)
(0 8 0 9 0 0 0 0 0) (0 0 0 0 2 0 0 5 8))

((7 0 0 0 0 3 0 0 2) (0 0 0 4 0 0 3 0 0) (4 1 0 0 6 9 0 0 0) (0 9 4 0
0 0 0 0 0) (6 0 2 0 7 0 4 0 3) (0 0 0 0 0 0 1 2 0) (0 0 0 7 1 0 0 5 8)
(0 0 8 0 0 2 0 0 0) (5 0 0 8 0 0 0 0 4))

((5 0 0 0 7 4 0 2 0) (0 0 0 2 0 0 0 0 5) (0 0 0 0 8 0 4 3 0) (0 4 7 0
2 0 0 9 0) (0 0 0 0 0 0 0 0 0) (0 6 0 0 4 0 2 5 0) (0 8 4 0 6 0 0 0 0)
(6 0 0 0 0 3 0 0 0) (0 3 0 4 9 0 0 0 1))

((0 0 0 0 6 0 8 0 7) (8 5 0 0 1 0 0 6 0) (9 0 0 4 0 0 0 0 0) (0 0 2 1
0 0 0 9 3) (0 0 0 0 3 0 0 0 0) (1 8 0 0 0 5 2 0 0) (0 0 0 0 0 4 0 0 5)
(0 1 0 0 5 0 0 3 2) (3 0 5 0 9 0 0 0 0))

((0 0 0 0 2 6 0 5 0) (0 0 0 3 0 0 0 4 0) (9 0 0 0 5 0 6 0 0) (6 0 0 0
0 0 4 3 0) (4 0 2 0 3 0 7 0 6) (0 1 3 0 0 0 0 0 2) (0 0 8 0 4 0 0 0 3)
(0 6 0 0 0 5 0 0 0) (0 2 0 9 8 0 0 0 0))

((1 0 0 0 0 0 0 0 0) (2 0 5 0 0 9 0 0 0) (0 0 6 4 8 2 0 0 0) (4 0 2 0
0 0 6 0 0) (6 0 0 0 3 0 0 0 8) (0 0 1 0 0 0 3 0 7) (0 0 0 6 9 5 7 0 0)
(0 0 0 2 0 0 4 0 5) (0 0 0 0 0 0 0 0 1))

((0 4 0 7 0 0 0 0 0) (6 0 0 5 4 0 0 1 0) (0 0 7 0 9 8 0 0 5) (0 0 0 0
0 2 5 0 0) (4 0 1 0 0 0 8 0 2) (0 0 5 4 0 0 0 0 0) (3 0 0 8 5 0 7 0 0)
(0 7 0 0 3 4 0 0 6) (0 0 0 0 0 9 0 3 0))

((4 0 0 0 0 0 0 0 7) (0 7 0 0 0 9 1 2 0) (0 0 1 0 6 7 3 0 0) (0 0 0 6
1 0 0 5 0) (0 0 0 0 3 0 0 0 0) (0 5 0 0 8 2 0 0 0) (0 0 3 4 9 0 2 0 0)
(0 4 5 1 0 0 0 3 0) (1 0 0 0 0 0 0 0 8))

((0 3 9 1 6 0 0 0 0) (1 0 4 7 0 5 0 0 0) (0 0 0 0 0 0 0 0 0) (9 0 0 0
0 0 8 0 2) (8 0 0 0 7 0 0 0 6) (5 0 2 0 0 0 0 0 4) (0 0 0 0 0 0 0 0 0)
(0 0 0 3 0 2 9 0 5) (0 0 0 0 5 4 2 3 0))

((0 8 0 0 9 0 2 0 0) (0 0 0 8 3 0 0 1 0) (0 0 7 0 0 0 0 0 5) (0 3 0 9
0 0 4 0 6) (0 6 4 0 0 0 5 8 0) (1 0 8 0 0 4 0 3 0) (4 0 0 0 0 0 1 0 0)
(0 1 0 0 7 8 0 0 0) (0 0 2 0 4 0 0 5 0))

((0 0 0 9 0 0 2 7 0) (0 3 0 7 2 0 0 0 0) (0 6 0 0 5 0 3 0 0) (3 0 0 0
0 5 0 0 9) (0 2 0 0 3 0 0 8 0) (1 0 0 8 0 0 0 0 3) (0 0 4 0 9 0 0 6 0)
(0 0 0 0 1 4 0 5 0) (0 9 1 0 0 8 0 0 0))

((8 7 0 0 0 9 5 1 0) (0 5 3 0 8 0 0 4 0) (0 0 0 5 0 0 0 0 0) (2 0 0 0
0 0 8 0 0) (6 0 0 0 2 0 0 0 1) (0 0 5 0 0 0 0 0 3) (0 0 0 0 0 3 0 0 0)
(0 9 0 0 7 0 3 5 0) (0 8 6 9 0 0 0 7 4))

((9 4 5 1 0 0 0 0 3) (0 0 0 9 0 0 0 5 2) (0 0 0 0 0 0 8 0 0) (0 0 0 6
8 0 9 0 0) (7 0 0 0 4 0 0 0 8) (0 0 1 0 9 5 0 0 0) (0 0 3 0 0 0 0 0 0)
(6 7 0 0 0 4 0 0 0) (2 0 0 0 0 9 1 3 7))

((0 0 0 3 6 2 5 0 0) (0 0 4 7 0 0 0 0 2) (0 2 5 0 0 0 0 0 3) (0 0 3 6
0 0 0 0 0) (6 0 2 0 0 0 4 0 7) (0 0 0 0 0 9 8 0 0) (2 0 0 0 0 0 9 5 0)
(7 0 0 0 0 1 3 0 0) (0 0 6 8 5 3 0 0 0))

((0 0 0 0 2 0 5 0 8) (6 0 0 0 0 1 0 0 2) (0 0 7 4 0 9 0 0 0) (0 0 0 0
0 0 0 3 0) (1 0 3 0 0 0 6 0 9) (0 8 0 0 0 0 0 0 0) (0 0 0 6 0 2 4 0 0)
(4 0 0 8 0 0 0 0 7) (9 0 2 0 5 0 0 0 0))

((6 0 3 8 0 0 0 5 0) (9 0 0 0 2 0 7 0 0) (0 0 0 6 0 0 0 2 0) (0 0 5 4
0 0 0 0 1) (0 4 7 0 0 0 6 9 0) (3 0 0 0 0 9 2 0 0) (0 1 0 0 0 4 0 0 0)
(0 0 6 0 5 0 0 0 7) (0 7 0 0 0 8 5 0 9))

((0 0 0 0 0 0 0 0 0) (0 1 7 9 0 3 0 0 0) (3 9 0 5 7 0 0 0 0) (0 0 5 0
0 0 7 9 0) (0 0 4 0 2 0 6 0 0) (0 6 9 0 0 0 1 0 0) (0 0 0 0 4 8 0 1 3)
(0 0 0 7 0 2 8 5 0) (0 0 0 0 0 0 0 0 0))

((0 0 8 2 0 0 0 0 0) (1 0 2 0 5 0 0 0 3) (0 0 0 6 4 0 0 1 0) (0 0 0 3
0 0 5 0 0) (3 9 0 0 8 0 0 6 4) (0 0 7 0 0 6 0 0 0) (0 1 0 0 7 3 0 0 0)
(5 0 0 0 6 0 4 0 8) (0 0 0 0 0 5 9 0 0))

((0 0 7 2 0 0 0 5 0) (0 4 5 1 0 3 0 0 0) (0 0 0 0 9 0 3 0 0) (0 5 0 0
2 0 4 0 0) (0 0 1 0 8 0 6 0 0) (0 0 8 0 6 0 0 9 0) (0 0 6 0 5 0 0 0 0)
(0 0 0 4 0 2 5 6 0) (0 3 0 0 0 7 2 0 0))

((0 0 3 0 1 0 0 4 2) (0 0 0 8 3 0 0 0 0) (0 0 0 0 0 0 9 3 1) (3 0 0 0
7 2 0 0 0) (0 7 0 5 0 6 0 8 0) (0 0 0 3 9 0 0 0 5) (7 5 2 0 0 0 0 0 0)
(0 0 0 0 2 3 0 0 0) (9 3 0 0 5 0 4 0 0))

((0 0 0 1 3 0 6 0 0) (4 0 6 0 0 7 0 0 0) (0 0 1 0 0 0 3 8 7) (0 6 2 0
4 0 0 0 0) (8 0 0 0 0 0 0 0 3) (0 0 0 0 8 0 4 7 0) (7 9 5 0 0 0 2 0 0)
(0 0 0 6 0 0 1 0 9) (0 0 4 0 9 2 0 0 0))

((0 9 7 0 0 6 0 0 0) (0 4 0 0 7 0 0 0 8) (5 0 0 0 1 0 0 2 0) (0 0 1 0
2 0 0 6 0) (4 0 0 6 0 1 0 0 3) (0 2 0 0 9 0 1 0 0) (0 5 0 0 3 0 0 0 6)
(3 0 0 0 8 0 0 4 0) (0 0 0 7 0 0 5 3 0))

((0 0 4 0 0 2 7 0 0) (0 9 0 3 7 0 0 0 0) (0 0 1 6 0 0 2 0 0) (0 1 0 0
0 0 0 0 0) (0 3 0 7 9 6 0 8 0) (0 0 0 0 0 0 0 9 0) (0 0 3 0 0 7 4 0 0)
(0 0 0 0 8 4 0 3 0) (0 0 7 5 0 0 1 0 0))

((5 0 0 0 0 2 0 0 0) (9 0 0 3 0 0 2 1 0) (0 0 8 6 0 0 3 0 0) (0 0 0 0
0 1 0 4 0) (8 0 7 0 3 0 1 0 6) (0 4 0 7 0 0 0 0 0) (0 0 1 0 0 5 7 0 0)
(0 6 5 0 0 7 0 0 3) (0 0 0 2 0 0 0 0 1))

((0 0 9 0 0 7 0 0 0) (3 8 0 0 5 0 0 0 0) (7 0 0 8 2 0 9 0 0) (1 0 0 0
7 0 0 2 8) (0 0 0 0 0 0 0 0 0) (9 7 0 0 8 0 0 0 6) (0 0 4 0 1 8 0 0 3)
(0 0 0 0 6 0 0 8 5) (0 0 0 3 0 0 6 0 0))

((1 0 4 0 0 0 5 0 0) (0 2 0 8 1 0 0 0 0) (0 0 0 0 0 5 0 0 1) (9 0 3 2
0 0 8 7 0) (0 0 0 0 0 0 0 0 0) (0 5 7 0 0 8 2 0 4) (3 0 0 9 0 0 0 0 0)
(0 0 0 0 6 1 0 8 0) (0 0 8 0 0 0 7 0 6))

((0 0 0 0 3 0 9 0 0) (0 0 1 8 2 0 0 4 0) (0 5 0 0 0 0 0 6 8) (0 0 0 0
0 2 0 0 0) (9 0 6 0 7 0 2 0 4) (0 0 0 5 0 0 0 0 0) (6 7 0 0 0 0 0 9 0)
(0 1 0 0 4 8 3 0 0) (0 0 5 0 1 0 0 0 0))

((7 0 4 0 0 8 0 9 0) (8 0 0 0 0 1 0 0 2) (0 0 0 7 0 0 0 5 0) (0 0 3 4
0 0 0 0 0) (4 1 0 0 8 0 0 2 6) (0 0 0 0 0 6 3 0 0) (0 4 0 0 0 7 0 0 0)
(6 0 0 5 0 0 0 0 4) (0 8 0 6 0 0 1 0 5))))

(defvar medium-puzzles '( ((0 0 0 8 7 0 1 0 2) (0 0 1 0 4 0 9 0 6) (0
0 0 0 0 6 0 0 0) (0 0 0 0 8 0 0 5 3) (5 0 6 0 3 0 4 0 9) (8 2 0 0 5 0
0 0 0) (0 0 0 1 0 0 0 0 0) (1 0 5 0 2 0 3 0 0) (7 0 4 0 6 8 0 0 0))

((0 5 0 0 0 8 0 1 0) (0 9 0 0 0 5 0 0 6) (0 0 0 7 4 0 9 0 8) (0 0 0 0
0 0 3 0 5) (0 1 4 0 0 0 8 6 0) (5 0 8 0 0 0 0 0 0) (1 0 9 0 7 6 0 0 0)
(7 0 0 2 0 0 0 8 0) (0 2 0 3 0 0 0 9 0))

((0 0 0 0 2 8 0 0 3) (0 0 0 0 0 7 1 0 2) (0 3 2 0 1 0 0 9 0) (1 0 0 0
0 0 7 2 0) (4 0 0 0 0 0 0 0 5) (0 8 6 0 0 0 0 0 4) (0 6 0 0 9 0 3 7 0)
(9 0 4 7 0 0 0 0 0) (3 0 0 2 5 0 0 0 0))

((9 8 4 0 0 3 5 0 0) (0 7 0 0 4 0 0 1 0) (0 0 3 0 0 0 0 0 4) (5 0 0 0
9 0 0 7 8) (0 0 0 2 0 8 0 0 0) (3 6 0 0 1 0 0 0 2) (4 0 0 0 0 0 8 0 0)
(0 3 0 0 8 0 0 5 0) (0 0 5 4 0 0 7 2 1))

((4 0 0 0 1 0 0 0 2) (0 0 7 2 0 0 4 0 9) (0 0 0 8 0 0 0 6 0) (0 0 0 6
0 8 1 7 0) (0 0 3 0 4 0 2 0 0) (0 6 1 3 0 2 0 0 0) (0 2 0 0 0 9 0 0 0)
(7 0 4 0 0 5 6 0 0) (9 0 0 0 8 0 0 0 7))

((0 0 0 3 7 0 0 0 4) (0 7 1 0 0 0 8 0 5) (0 0 3 4 0 0 0 0 7) (0 9 0 8
2 0 0 0 0) (3 0 7 0 0 0 5 0 2) (0 0 0 0 3 7 0 4 0) (7 0 0 0 0 1 2 0 0)
(2 0 8 0 0 0 3 6 0) (1 0 0 0 8 3 0 0 0))

((4 0 0 5 6 0 0 0 0) (0 6 0 0 7 8 3 0 0) (0 3 0 1 0 0 0 7 0) (6 0 0 0
0 0 0 0 3) (9 0 1 0 0 0 8 0 4) (2 0 0 0 0 0 0 0 6) (0 2 0 0 0 4 0 6 0)
(0 0 9 3 5 0 0 2 0) (0 0 0 0 2 1 0 0 7))

((6 2 1 0 0 9 0 0 4) (8 5 0 0 0 0 0 0 0) (7 0 0 0 0 6 9 1 0) (9 0 0 0
7 0 4 0 2) (0 8 0 0 0 0 0 9 0) (3 0 4 0 1 0 0 0 5) (0 9 2 8 0 0 0 0 3)
(0 0 0 0 0 0 0 4 7) (4 0 0 3 0 0 5 2 9))

((0 0 0 8 3 0 0 6 0) (0 5 0 0 9 0 0 3 7) (0 0 0 0 0 0 5 0 0) (2 0 0 0
8 0 0 0 3) (5 3 8 0 1 0 2 4 9) (1 0 0 0 5 0 0 0 6) (0 0 3 0 0 0 0 0 0)
(4 2 0 0 6 0 0 1 0) (0 7 0 0 2 1 0 0 0))

((0 0 9 5 8 0 6 0 0) (0 3 0 0 0 9 0 4 0) (7 0 0 0 4 0 5 0 0) (0 0 4 3
0 0 0 6 5) (5 0 0 9 0 8 0 0 1) (2 1 0 0 0 7 3 0 0) (0 0 2 0 3 0 0 0 6)
(0 6 0 2 0 0 0 3 0) (0 0 7 0 9 6 1 0 0))

((6 5 0 9 8 0 0 0 2) (0 0 0 0 0 1 9 0 0) (0 0 0 7 6 2 0 0 0) (0 9 7 0
0 0 3 0 0) (8 0 0 0 1 0 0 0 4) (0 0 2 0 0 0 6 5 0) (0 0 0 6 7 8 0 0 0)
(0 0 3 1 0 0 0 0 0) (5 0 0 0 9 3 0 2 8))

((0 0 1 0 8 0 2 0 0) (0 0 0 4 0 3 0 0 5) (0 0 2 9 0 0 0 0 0) (0 1 3 0
0 0 0 2 7) (0 6 5 0 7 0 1 4 0) (7 4 0 0 0 0 5 6 0) (0 0 0 0 0 5 9 0 0)
(3 0 0 2 0 6 0 0 0) (0 0 9 0 1 0 4 0 0))

((8 0 0 0 0 4 0 0 0) (7 6 0 8 5 0 0 4 0) (4 0 3 0 2 0 0 0 7) (0 1 0 0
9 0 0 5 0) (3 0 0 6 0 2 0 0 1) (0 8 0 0 4 0 0 6 0) (5 0 0 0 3 0 6 0 9)
(0 3 0 0 6 5 0 7 8) (0 0 0 7 0 0 0 0 5))

((3 2 0 0 1 0 7 0 0) (1 5 9 6 0 3 0 0 2) (6 0 0 0 0 2 1 0 0) (0 0 0 0
0 0 4 0 1) (0 0 0 7 0 5 0 0 0) (9 0 8 0 0 0 0 0 0) (0 0 3 2 0 0 0 0 7)
(2 0 0 9 0 1 5 8 4) (0 0 1 0 8 0 0 6 3))

((0 0 2 0 5 0 9 4 0) (7 3 0 8 0 1 0 0 0) (0 0 5 0 0 0 0 3 0) (8 9 0 0
0 0 1 2 0) (3 0 0 0 0 0 0 0 6) (0 2 7 0 0 0 0 8 3) (0 7 0 0 0 0 6 0 0)
(0 0 0 5 0 7 0 1 9) (0 1 3 0 9 0 2 0 0))

((0 0 0 1 6 0 0 2 0) (0 0 7 3 0 0 5 0 0) (2 1 0 0 0 8 0 0 9) (0 0 1 0
9 5 0 0 6) (9 5 0 0 0 0 0 8 4) (7 0 0 2 4 0 9 0 0) (3 0 0 5 0 0 0 6 7)
(0 0 5 0 0 7 8 0 0) (0 7 0 0 3 6 0 0 0))

((0 9 8 4 5 0 0 0 2) (0 0 4 8 0 0 6 0 0) (0 0 0 0 0 0 0 8 5) (0 0 5 9
0 0 7 4 0) (1 0 0 0 0 0 0 0 9) (0 4 3 0 0 5 2 0 0) (2 6 0 0 0 0 0 0 0)
(0 0 1 0 0 6 9 0 0) (5 0 0 0 3 2 1 6 0))

((9 0 0 0 2 0 0 5 7) (0 0 0 7 9 3 0 0 4) (0 0 0 0 8 0 2 0 0) (0 8 0 0
0 0 0 0 0) (1 2 0 8 3 9 0 7 6) (0 0 0 0 0 0 0 3 0) (0 0 9 0 7 0 0 0 0)
(5 0 0 2 1 6 0 0 0) (2 6 0 0 4 0 0 0 3))

((0 0 4 7 0 0 0 0 0) (0 0 0 0 2 0 7 3 0) (9 7 8 5 0 0 1 0 0) (0 0 0 0
6 5 3 1 0) (1 0 0 0 7 0 0 0 6) (0 8 5 3 4 0 0 0 0) (0 0 9 0 0 3 2 7 4)
(0 3 2 0 9 0 0 0 0) (0 0 0 0 0 7 6 0 0))

((3 0 0 0 0 0 0 0 6) (0 0 0 4 3 0 1 0 0) (0 0 2 0 0 0 9 0 0) (0 0 9 3
8 0 0 0 5) (6 0 8 7 4 9 3 0 2) (7 0 0 0 5 1 6 0 0) (0 0 4 0 0 0 2 0 0)
(0 0 7 0 2 8 0 0 0) (5 0 0 0 0 0 0 0 9))

((7 0 5 6 1 0 0 0 0) (1 0 2 0 4 0 0 0 7) (0 0 0 8 0 0 0 0 0) (0 7 0 0
0 0 6 0 0) (8 0 4 1 9 6 5 0 2) (0 0 6 0 0 0 0 4 0) (0 0 0 0 0 1 0 0 0)
(9 0 0 0 2 0 7 0 3) (0 0 0 0 3 9 8 0 4))

((0 0 8 0 0 4 5 0 0) (0 1 0 5 9 0 0 6 0) (5 0 0 0 8 0 0 4 0) (0 8 0 6
0 0 1 0 4) (1 0 0 3 0 9 0 0 2) (2 0 5 0 0 8 0 7 0) (0 2 0 0 7 0 0 0 6)
(0 5 0 0 3 2 0 9 0) (0 0 7 9 0 0 8 0 0))

((0 7 1 0 4 0 0 2 0) (0 9 8 2 7 0 0 0 0) (0 0 0 5 0 0 0 0 0) (9 0 0 0
0 0 3 0 0) (0 4 6 3 2 5 9 8 0) (0 0 3 0 0 0 0 0 1) (0 0 0 0 0 8 0 0 0)
(0 0 0 0 5 3 6 1 0) (0 1 0 0 9 0 4 5 0))

((5 0 7 0 4 0 0 0 0) (0 0 0 0 0 1 2 0 0) (0 0 4 0 0 5 7 8 1) (9 0 3 5
8 0 0 0 0) (0 6 0 0 1 0 0 2 0) (0 0 0 0 2 3 5 0 6) (1 4 9 3 0 0 6 0 0)
(0 0 8 1 0 0 0 0 0) (0 0 0 0 7 0 1 0 5))

((0 0 4 8 0 0 0 0 0) (3 6 0 9 0 1 0 0 4) (1 0 0 3 0 0 2 7 0) (0 0 0 1
0 0 9 2 0) (7 8 0 0 0 0 0 4 3) (0 4 9 0 0 3 0 0 0) (0 1 5 0 0 7 0 0 2)
(4 0 0 2 0 8 0 1 6) (0 0 0 0 0 5 7 0 0))

((0 2 5 8 0 0 0 0 0) (7 0 0 0 0 0 8 0 3) (0 0 1 0 7 0 6 0 4) (0 0 0 9
0 3 0 4 0) (0 0 2 0 0 0 5 0 0) (0 7 0 4 0 5 0 0 0) (5 0 4 0 3 0 1 0 0)
(2 0 3 0 0 0 0 0 6) (0 0 0 0 0 6 4 3 0))

((3 0 9 0 6 4 0 0 7) (4 0 0 0 0 0 9 3 0) (5 0 0 3 0 0 0 0 0) (0 4 0 0
7 8 0 6 0) (0 0 0 0 0 0 0 0 0) (0 6 0 4 5 0 0 8 0) (0 0 0 0 0 7 0 0 6)
(0 5 1 0 0 0 0 0 4) (6 0 0 2 1 0 7 0 5))

((0 0 0 0 0 5 4 0 3) (0 0 6 8 7 0 0 0 0) (0 0 0 0 0 0 1 7 0) (0 0 7 4
8 0 2 3 0) (4 6 0 5 0 2 0 9 8) (0 3 2 0 9 1 6 0 0) (0 2 3 0 0 0 0 0 0)
(0 0 0 0 5 7 8 0 0) (5 0 8 6 0 0 0 0 0))

((0 4 0 0 0 0 0 0 7) (3 2 0 0 7 0 0 0 5) (0 0 0 6 0 1 9 4 0) (6 5 0 0
0 0 1 3 0) (0 0 8 0 0 0 4 0 0) (0 1 4 0 0 0 0 5 9) (0 6 3 9 0 7 0 0 0)
(5 0 0 0 3 0 0 6 4) (8 0 0 0 0 0 0 9 0))

((0 0 0 0 0 0 0 0 0) (3 5 7 0 0 9 4 0 0) (0 9 0 8 2 0 7 0 0) (5 0 0 0
0 0 1 7 0) (7 3 0 0 8 0 0 2 9) (0 1 8 0 0 0 0 0 5) (0 0 5 0 7 8 0 6 0)
(0 0 6 2 0 0 8 5 7) (0 0 0 0 0 0 0 0 0))

((9 0 0 8 0 0 0 0 0) (0 0 0 2 0 1 0 7 0) (3 0 0 0 4 0 0 0 9) (1 0 3 0
0 0 9 5 0) (7 0 6 0 5 0 2 0 3) (0 5 2 0 0 0 6 0 7) (8 0 0 0 3 0 0 0 2)
(0 1 0 9 0 6 0 0 0) (0 0 0 0 0 7 0 0 8))

((0 0 2 0 0 5 3 8 0) (0 0 8 1 9 0 0 0 0) (0 6 1 0 7 8 0 0 0) (0 2 0 0
0 0 9 0 0) (0 8 0 0 2 0 0 7 0) (0 0 7 0 0 0 0 2 0) (0 0 0 5 4 0 1 9 0)
(0 0 0 0 8 1 4 0 0) (0 1 6 2 0 0 8 0 0))

((4 0 2 9 0 0 0 0 8) (0 0 0 0 5 0 0 0 4) (1 0 0 0 8 3 2 0 0) (2 0 0 0
0 5 0 9 0) (0 0 5 0 0 0 1 0 0) (0 8 0 3 0 0 0 0 5) (0 0 4 2 9 0 0 0 6)
(3 0 0 0 7 0 0 0 0) (5 0 0 0 0 1 7 0 2))

((0 0 7 8 0 6 3 0 4) (0 5 0 3 0 0 0 0 1) (6 2 0 0 7 0 0 0 0) (0 0 0 0
0 1 4 0 3) (8 0 0 0 0 0 0 0 6) (2 0 5 6 0 0 0 0 0) (0 0 0 0 1 0 0 6 2)
(5 0 0 0 0 3 0 4 0) (7 0 1 2 0 9 5 0 0))

((8 0 0 6 2 0 0 0 1) (0 0 0 0 0 0 0 0 0) (0 1 3 0 5 8 0 2 0) (0 0 0 0
1 3 7 0 0) (0 0 5 7 4 6 2 0 0) (0 0 4 5 9 0 0 0 0) (0 2 0 1 7 0 9 4 0)
(0 0 0 0 0 0 0 0 0) (5 0 0 0 8 9 0 0 6))

((0 6 0 0 0 4 1 9 3) (0 0 0 0 0 1 0 8 0) (0 1 7 0 5 0 0 0 0) (0 7 6 4
2 0 0 0 0) (2 0 0 0 1 0 0 0 6) (0 0 0 0 8 7 9 4 0) (0 0 0 0 3 0 7 5 0)
(0 2 0 1 0 0 0 0 0) (8 5 1 7 0 0 0 3 0))

((0 0 2 0 0 7 0 8 0) (6 0 9 1 0 5 0 0 2) (0 0 0 0 6 0 1 3 0) (2 0 1 3
0 0 0 0 0) (0 0 4 0 0 0 3 0 0) (0 0 0 0 0 6 7 0 8) (0 1 3 0 9 0 0 0 0)
(9 0 0 4 0 3 8 0 7) (0 2 0 7 0 0 6 0 0))

((7 0 0 5 9 0 1 0 0) (2 8 0 0 0 0 9 0 0) (0 9 0 4 2 0 0 0 0) (0 0 0 0
0 0 6 5 7) (0 7 0 0 0 0 0 8 0) (4 5 6 0 0 0 0 0 0) (0 0 0 0 8 7 0 2 0)
(0 0 7 0 0 0 0 1 5) (0 0 9 0 5 2 0 0 3))

((0 0 1 3 8 0 0 7 0) (0 0 0 0 0 0 0 0 0) (2 7 6 0 0 1 0 9 0) (6 0 0 0
0 0 7 5 0) (7 0 2 0 3 0 8 0 1) (0 3 5 0 0 0 0 0 6) (0 4 0 8 0 0 6 3 7)
(0 0 0 0 0 0 0 0 0) (0 6 0 0 7 3 4 0 0))

((0 0 0 7 0 0 0 0 0) (0 8 5 0 0 0 0 0 0) (7 1 2 9 3 0 0 0 0) (8 2 0 3
0 0 0 7 0) (0 7 1 4 5 9 3 8 0) (0 4 0 0 0 7 0 9 1) (0 0 0 0 4 1 7 3 9)
(0 0 0 0 0 0 5 2 0) (0 0 0 0 0 3 0 0 0))

((5 8 0 0 0 9 0 0 7) (0 0 3 8 4 0 0 0 6) (0 0 0 1 0 0 4 0 0) (8 0 0 3
6 0 0 2 0) (0 0 7 0 1 0 9 0 0) (0 1 0 0 7 8 0 0 4) (0 0 9 0 0 1 0 0 0)
(2 0 0 0 5 3 6 0 0) (7 0 0 2 0 0 0 4 5))

((0 0 0 2 7 9 0 4 0) (0 0 0 0 1 0 0 0 6) (0 7 0 0 6 0 3 2 0) (0 0 1 0
0 0 0 0 0) (0 8 6 1 9 7 2 5 0) (0 0 0 0 0 0 9 0 0) (0 6 5 0 4 0 0 9 0)
(7 0 0 0 2 0 0 0 0) (0 3 0 6 8 5 0 0 0))

((0 0 8 4 7 9 0 0 0) (0 1 9 0 6 0 7 0 0) (6 0 0 0 3 0 0 0 0) (0 0 0 0
0 0 0 3 0) (0 9 5 7 4 3 2 6 0) (0 4 0 0 0 0 0 0 0) (0 0 0 0 9 0 0 0 7)
(0 0 4 0 8 0 6 5 0) (0 0 0 5 2 6 1 0 0))

((0 0 2 0 7 5 0 0 1) (0 0 0 0 0 0 0 0 0) (0 0 4 1 0 0 2 9 6) (2 0 3 0
0 0 0 6 0) (7 1 0 0 5 0 0 2 9) (0 6 0 0 0 0 5 0 3) (6 2 5 0 0 7 8 0 0)
(0 0 0 0 0 0 0 0 0) (8 0 0 5 2 0 6 0 0))

((0 0 0 2 0 6 0 5 1) (1 0 9 0 5 0 3 0 0) (6 0 0 0 0 0 8 0 0) (3 0 6 0
0 0 0 9 4) (0 9 0 0 0 0 0 8 0) (5 4 0 0 0 0 1 0 3) (0 0 2 0 0 0 0 0 9)
(0 0 3 0 2 0 5 0 7) (9 6 0 4 0 1 0 0 0))

((4 0 0 0 0 0 9 6 5) (6 7 0 0 0 0 0 0 0) (0 0 3 8 0 0 0 0 1) (0 4 0 2
0 7 0 0 0) (2 3 0 6 0 4 0 7 8) (0 0 0 5 0 3 0 2 0) (1 0 0 0 0 5 8 0 0)
(0 0 0 0 0 0 0 5 2) (7 9 5 0 0 0 0 0 4))

((0 0 0 0 0 0 4 0 0) (0 0 0 7 9 0 0 0 5) (7 0 0 0 6 0 0 1 9) (0 6 0 0
3 0 0 7 0) (1 8 9 0 7 0 2 3 4) (0 4 0 0 2 0 0 9 0) (4 5 0 0 8 0 0 0 3)
(6 0 0 0 4 2 0 0 0) (0 0 3 0 0 0 0 0 0))

((2 0 0 5 0 0 0 0 3) (3 0 0 0 8 1 0 6 0) (0 0 4 9 3 0 0 0 0) (0 0 2 0
0 0 3 0 0) (0 7 5 0 0 0 6 9 0) (0 0 1 0 0 0 2 0 0) (0 0 0 0 2 8 5 0 0)
(0 1 0 7 4 0 0 0 2) (4 0 0 0 0 9 0 0 1))

((7 0 0 2 0 0 0 0 0) (0 0 0 5 0 8 0 6 0) (4 0 0 0 3 0 0 0 7) (2 0 5 0
0 0 4 9 0) (3 0 4 0 9 0 5 0 2) (0 9 8 0 0 0 3 0 6) (8 0 0 0 1 0 0 0 3)
(0 2 0 6 0 4 0 0 0) (0 0 0 0 0 7 0 0 8))

((0 0 0 2 7 0 0 8 4) (2 1 0 3 0 0 0 9 0) (0 0 0 0 5 8 0 2 0) (0 5 0 0
0 0 0 0 9) (7 0 0 0 9 0 0 0 2) (9 0 0 0 0 0 0 7 0) (0 6 0 8 2 0 0 0 0)
(0 2 0 0 0 9 0 4 8) (5 8 0 0 6 3 0 0 0))))


(defvar easy-puzzles '( ((8 0 0 5 0 6 9 4 0) (0 6 0 0 9 0 5 3 0) (0 0
5 7 0 4 0 0 6) (0 0 0 0 2 0 1 0 4) (0 0 0 1 0 8 0 0 0) (6 0 2 0 7 0 0
0 0) (9 0 0 2 0 7 6 0 0) (0 2 6 0 5 0 0 7 0) (0 7 8 6 0 3 0 0 1))

((0 7 0 2 1 8 4 0 6) (0 0 0 5 0 4 0 0 0) (2 0 0 0 0 0 0 9 5) (4 0 8 6
5 0 3 0 7) (0 0 7 0 0 0 6 0 0) (6 0 1 0 8 7 2 0 9) (7 6 0 0 0 0 0 0 4)
(0 0 0 4 0 6 0 0 0) (1 0 5 8 2 9 0 6 0))

((0 0 0 0 4 0 0 0 0) (9 0 0 3 0 5 0 0 4) (3 0 7 8 0 2 1 0 6) (0 9 4 5
0 3 0 8 0) (0 0 2 0 8 0 3 0 0) (0 3 0 9 0 4 5 6 0) (7 0 1 2 0 8 4 0 3)
(8 0 0 4 0 7 0 0 2) (0 0 0 0 6 0 0 0 0))

((0 0 0 0 1 2 0 8 0) (0 4 0 7 9 0 0 0 6) (2 5 0 0 4 6 0 3 0) (8 0 0 0
0 0 9 0 0) (0 6 5 9 7 8 2 1 0) (0 0 2 0 0 0 0 0 8) (0 8 0 1 2 0 0 6 4)
(9 0 0 0 5 3 0 7 0) (0 3 0 4 8 0 0 0 0))

((6 0 0 1 9 3 0 0 8) (1 0 0 0 8 0 6 0 0) (0 0 0 6 0 0 0 4 9) (9 0 0 0
4 6 8 0 0) (2 0 6 0 0 0 5 0 4) (0 0 3 7 5 0 0 0 1) (3 5 0 0 0 2 0 0 0)
(0 0 2 0 6 0 0 0 5) (8 0 0 4 3 5 0 0 2))

((0 3 1 0 6 0 0 2 8) (0 0 0 0 3 4 0 0 0) (0 4 0 2 0 0 5 0 0) (0 0 7 0
5 0 0 9 1) (9 1 5 0 7 0 3 4 2) (4 6 0 0 2 0 8 0 0) (0 0 6 0 0 2 0 1 0)
(0 0 0 5 4 0 0 0 0) (2 8 0 0 9 0 7 6 0))

((4 0 0 0 0 0 1 0 0) (6 0 7 3 1 0 4 9 8) (0 9 0 8 0 0 0 0 0) (7 0 8 0
0 1 6 5 0) (0 0 3 4 0 7 8 0 0) (0 6 9 5 0 0 2 0 4) (0 0 0 0 0 5 0 2 0)
(9 1 4 0 2 3 5 0 6) (0 0 5 0 0 0 0 0 7))

((0 0 0 0 0 1 5 0 0) (0 3 0 0 2 7 0 9 0) (2 1 6 0 0 0 0 8 0) (0 0 7 0
0 4 0 3 8) (6 4 0 7 0 8 0 1 5) (9 5 0 3 0 0 6 0 0) (0 6 0 0 0 0 3 5 1)
(0 8 0 1 3 0 0 7 0) (0 0 1 4 0 0 0 0 0))

((0 0 2 9 8 0 0 1 0) (6 0 1 0 0 0 3 0 9) (0 0 9 0 0 6 4 8 0) (0 0 7 0
0 4 0 3 0) (8 3 0 1 0 2 0 9 4) (0 2 0 8 0 0 5 0 0) (0 6 8 5 0 0 9 0 0)
(1 0 3 0 0 0 8 0 2) (0 9 0 0 6 8 1 0 0))

((0 2 0 5 0 0 0 1 0) (8 0 4 0 6 1 0 0 7) (0 1 0 0 7 8 5 0 0) (0 0 9 8
0 0 0 0 5) (1 0 2 0 5 0 9 0 8) (5 0 0 0 0 7 4 0 0) (0 0 8 4 2 0 0 5 0)
(6 0 0 7 3 0 2 0 4) (0 4 0 0 0 9 0 7 0))

((9 0 0 0 4 1 0 3 0) (0 6 5 0 0 0 7 0 0) (3 2 0 0 5 0 9 0 0) (0 3 8 2
6 0 0 0 0) (6 0 9 0 7 0 3 0 4) (0 0 0 0 1 3 6 7 0) (0 0 6 0 9 0 0 1 3)
(0 0 2 0 0 0 4 6 0) (0 4 0 7 8 0 0 0 9))

((0 0 0 0 7 0 8 0 2) (0 0 5 9 0 2 0 3 0) (8 3 0 6 1 0 9 0 0) (5 0 6 0
0 8 0 0 1) (0 0 1 0 5 0 4 0 0) (2 0 0 3 0 0 5 0 9) (0 0 7 0 4 5 0 9 3)
(0 5 0 7 0 9 6 0 0) (9 0 8 0 3 0 0 0 0))

((0 3 0 0 0 8 0 0 0) (0 6 2 0 0 3 7 0 1) (0 9 7 0 1 0 0 8 4) (0 0 0 0
4 2 0 6 9) (0 8 0 1 0 5 0 3 0) (4 7 0 8 3 0 0 0 0) (9 4 0 0 2 0 1 5 0)
(6 0 8 5 0 0 2 7 0) (0 0 0 3 0 0 0 4 0))

((0 3 7 6 0 0 0 0 4) (0 0 5 7 2 9 6 0 0) (0 0 0 0 3 5 0 7 0) (0 0 0 0
1 6 7 0 9) (3 0 9 0 0 0 1 0 8) (6 0 4 8 9 0 0 0 0) (0 9 0 2 5 0 0 0 0)
(0 0 3 1 7 4 9 0 0) (7 0 0 0 0 8 3 1 0))

((0 0 5 8 9 0 0 0 0) (0 0 4 2 7 0 6 0 8) (2 0 0 0 1 3 7 0 0) (0 1 0 0
0 0 0 0 5) (0 8 9 5 3 1 2 6 0) (5 0 0 0 0 0 0 8 0) (0 0 3 4 6 0 0 0 1)
(7 0 2 0 8 9 5 0 0) (0 0 0 0 5 7 4 0 0))

((0 0 1 8 0 0 0 6 0) (0 0 2 0 0 3 7 0 4) (9 0 3 7 2 6 8 0 0) (0 0 0 9
4 0 5 8 0) (0 0 9 0 0 0 6 0 0) (0 2 5 0 3 8 0 0 0) (0 0 4 2 5 1 3 0 8)
(3 0 7 4 0 0 1 0 0) (0 1 0 0 0 7 9 0 0))

((0 9 2 0 4 0 0 0 5) (0 0 0 0 0 7 3 0 0) (0 4 3 0 0 6 0 8 9) (0 0 0 1
2 0 4 5 0) (0 1 0 4 6 5 0 7 0) (0 5 4 0 7 3 0 0 0) (9 2 0 5 0 0 8 4 0)
(0 0 6 7 0 0 0 0 0) (4 0 0 0 8 0 9 1 0))

((0 4 0 3 0 8 1 0 9) (8 0 0 1 0 5 0 7 0) (0 0 1 0 6 0 5 0 8) (0 0 0 0
1 0 0 8 5) (0 0 0 9 0 4 0 0 0) (4 2 0 0 5 0 0 0 0) (6 0 3 0 7 0 8 0 0)
(0 8 0 2 0 1 0 0 6) (7 0 2 8 0 6 0 9 0))

((2 0 4 0 0 0 0 0 6) (0 3 0 0 1 8 9 0 0) (0 9 7 0 2 0 0 0 3) (5 0 9 7
4 0 0 0 0) (3 4 0 0 6 0 0 1 9) (0 0 0 0 8 9 6 0 4) (4 0 0 0 3 0 8 9 0)
(0 0 1 6 5 0 0 3 0) (7 0 0 0 0 0 4 0 1))

((0 0 7 0 9 8 0 4 0) (8 0 0 7 0 0 9 3 6) (0 9 0 1 0 6 2 0 0) (4 2 0 0
0 0 1 0 0) (0 0 6 0 0 0 7 0 0) (0 0 9 0 0 0 0 8 5) (0 0 8 4 0 1 0 5 0)
(1 3 2 0 0 5 0 0 7) (0 5 0 6 7 0 8 0 0))

((0 0 6 5 4 0 0 9 0) (0 4 0 2 0 0 8 0 0) (0 0 5 0 9 0 2 0 4) (1 2 0 6
0 0 0 8 0) (0 0 9 1 0 2 7 0 0) (0 8 0 0 0 5 0 3 2) (4 0 8 0 1 0 9 0 0)
(0 0 1 0 0 7 0 5 0) (0 6 0 0 5 9 4 0 0))

((7 6 8 0 0 9 0 0 0) (0 1 4 0 0 5 0 7 0) (5 0 0 6 0 0 0 0 4) (1 0 3 0
2 0 0 0 9) (0 0 5 0 4 0 3 0 0) (2 0 0 0 9 0 1 0 7) (3 0 0 0 0 4 0 0 6)
(0 8 0 1 0 0 4 9 0) (0 0 0 3 0 0 7 5 1))

((0 3 0 1 0 2 0 6 0) (0 0 0 8 0 4 7 0 1) (1 5 0 0 0 7 4 0 0) (0 2 0 0
0 8 0 0 5) (7 8 0 0 0 0 0 1 3) (3 0 0 6 0 0 0 8 0) (0 0 6 5 0 0 0 9 2)
(5 0 2 4 0 9 0 0 0) (0 7 0 2 0 6 0 5 0))

((0 5 0 0 0 4 8 1 0) (0 0 7 3 8 0 4 0 2) (0 0 0 0 5 6 0 0 3) (0 0 9 4
0 5 2 0 7) (0 0 0 0 2 0 0 0 0) (4 0 6 7 0 3 1 0 0) (5 0 0 8 4 0 0 0 0)
(7 0 4 0 6 9 5 0 0) (0 1 8 5 0 0 0 9 0))

((2 0 0 0 0 9 0 1 3) (7 5 3 2 1 8 6 0 0) (0 1 0 0 5 0 0 0 0) (0 0 0 0
9 0 0 0 5) (0 9 7 1 0 3 4 8 0) (3 0 0 0 8 0 0 0 0) (0 0 0 0 4 0 0 6 0)
(0 0 8 5 6 1 3 7 4) (6 7 0 9 0 0 0 0 8))

((3 0 8 1 4 0 6 9 0) (0 0 0 0 0 0 0 7 0) (0 0 0 0 8 9 0 0 4) (0 8 3 0
0 5 2 0 0) (6 0 1 2 0 7 9 0 8) (0 0 9 8 0 0 7 4 0) (8 0 0 6 7 0 0 0 0)
(0 2 0 0 0 0 0 0 0) (0 1 6 0 2 3 4 0 7))

((6 0 0 4 9 0 2 0 3) (0 9 0 0 0 0 6 0 0) (0 0 2 7 6 0 0 0 0) (5 1 0 8
0 0 7 0 2) (2 3 0 9 0 5 0 6 1) (4 0 8 0 0 7 0 3 9) (0 0 0 0 8 9 4 0 0)
(0 0 5 0 0 0 0 2 0) (9 0 6 0 5 2 0 0 7))

((0 0 0 5 0 0 6 0 0) (5 0 7 1 4 6 0 0 0) (0 6 0 0 9 0 2 0 4) (7 0 2 3
6 0 0 0 8) (6 0 4 0 0 0 7 0 3) (9 0 0 0 2 7 4 0 1) (8 0 6 0 1 0 0 4 0)
(0 0 0 4 3 8 9 0 6) (0 0 3 0 0 5 0 0 0))

((0 0 0 0 0 3 5 0 7) (4 0 1 0 0 0 9 0 0) (7 2 5 0 9 4 0 0 3) (0 0 0 0
8 9 6 2 0) (8 0 0 3 0 2 0 0 1) (0 9 2 1 6 0 0 0 0) (9 0 0 5 2 0 4 3 6)
(0 0 4 0 0 0 1 0 8) (3 0 8 7 0 0 0 0 0))

((0 0 0 0 8 0 0 0 0) (1 0 0 2 0 3 0 0 8) (2 0 5 6 0 4 7 0 9) (0 1 8 3
0 2 0 6 0) (0 0 4 0 6 0 2 0 0) (0 2 0 1 0 8 3 9 0) (5 0 7 4 0 6 8 0 2)
(6 0 0 8 0 5 0 0 4) (0 0 0 0 9 0 0 0 0))

((0 5 3 1 8 0 6 0 0) (0 7 0 0 6 3 0 0 5) (0 0 0 0 0 0 0 4 1) (8 0 7 0
0 4 1 5 2) (0 0 0 0 0 0 0 0 0) (2 6 1 8 0 0 4 0 9) (6 2 0 0 0 0 0 0 0)
(7 0 0 5 3 0 0 1 0) (0 0 4 0 7 8 5 9 0))

((4 0 8 3 0 0 5 0 0) (0 0 3 1 6 5 4 0 2) (0 5 0 0 0 8 9 0 0) (0 1 6 0
4 2 0 0 0) (0 0 9 0 0 0 7 0 0) (0 0 0 9 3 0 6 2 0) (0 0 5 2 0 0 0 7 0)
(9 0 4 8 1 7 2 0 0) (0 0 1 0 0 4 8 0 3))

((3 0 6 0 0 0 7 0 9) (0 0 0 7 0 9 0 0 0) (0 0 8 0 1 2 6 0 0) (2 0 1 9
0 5 3 0 7) (0 4 0 0 3 0 0 2 0) (6 0 3 2 0 7 1 0 5) (0 0 2 5 7 0 8 0 0)
(0 0 0 4 0 3 0 0 0) (1 0 7 0 0 0 4 0 2))

((0 0 3 8 9 0 0 4 5) (0 4 0 0 0 0 0 0 0) (0 0 2 3 4 1 0 7 0) (2 0 6 5
0 0 9 0 0) (0 1 9 4 0 8 5 6 0) (0 0 4 0 0 3 7 0 2) (0 9 0 7 5 6 4 0 0)
(0 0 0 0 0 0 0 9 0) (4 8 0 0 3 9 1 0 0))

((0 1 0 7 0 9 0 0 8) (6 0 0 3 8 0 0 2 0) (4 8 7 0 0 2 3 0 0) (0 9 0 0
0 0 6 0 1) (0 2 0 0 0 0 0 7 0) (3 0 5 0 0 0 0 8 0) (0 0 2 5 0 0 9 1 4)
(0 3 0 0 2 7 0 0 5) (5 0 0 9 0 6 0 3 0))

((0 5 7 0 0 0 1 0 0) (0 3 0 5 1 4 7 0 0) (2 0 6 0 0 3 0 0 4) (0 0 3 0
9 0 0 0 5) (8 9 0 0 0 0 0 4 7) (5 0 0 0 4 0 2 0 0) (6 0 0 4 0 0 5 0 1)
(0 0 5 6 8 9 0 2 0) (0 0 4 0 0 0 8 9 0))

((2 0 3 8 5 0 0 0 0) (0 5 0 0 7 0 0 6 0) (7 8 0 0 0 9 3 2 5) (1 0 0 7
6 0 2 0 0) (0 0 2 0 0 0 1 0 0) (0 0 5 0 3 2 0 0 9) (5 3 6 4 0 0 0 8 2)
(0 1 0 0 2 0 0 3 0) (0 0 0 0 8 6 5 0 1))

((0 0 8 1 0 0 0 2 7) (9 0 0 5 0 2 0 0 6) (0 2 1 8 0 3 0 0 0) (0 7 0 3
0 0 0 0 5) (2 6 0 0 0 0 0 1 3) (3 0 0 0 0 9 0 6 0) (0 0 0 4 0 8 5 7 0)
(7 0 0 9 0 5 0 0 1) (4 5 0 0 0 7 9 0 0))

((0 5 0 0 1 0 0 0 0) (3 1 8 7 5 2 9 0 0) (7 0 0 0 0 4 0 5 8) (0 0 0 0
4 0 0 0 1) (0 4 3 5 0 8 6 2 0) (8 0 0 0 2 0 0 0 0) (9 3 0 4 0 0 0 0 2)
(0 0 2 1 9 5 8 3 6) (0 0 0 0 6 0 0 9 0))

((4 0 0 0 1 2 6 0 0) (0 2 1 0 5 9 0 7 0) (9 0 0 8 0 0 0 0 2) (0 0 2 9
0 0 0 4 0) (0 6 8 0 4 0 1 3 0) (0 4 0 0 0 6 8 0 0) (3 0 0 0 0 4 0 0 1)
(0 9 0 3 7 0 2 6 0) (0 0 4 6 9 0 0 0 3))

((7 1 8 0 9 6 0 3 0) (3 9 5 0 0 0 6 0 0) (0 0 6 0 0 1 9 0 0) (0 3 0 8
6 0 0 0 0) (0 0 0 7 0 2 0 0 0) (0 0 0 0 4 5 0 6 0) (0 0 7 9 0 0 2 0 0)
(0 0 9 0 0 0 7 1 4) (0 4 0 1 7 0 5 9 6))

((2 5 0 1 7 0 0 0 0) (7 0 0 0 0 4 9 0 5) (0 3 4 0 0 9 1 0 7) (0 4 0 0
0 2 0 7 1) (6 0 0 0 0 0 0 0 3) (9 2 0 6 0 0 0 4 0) (4 0 3 9 0 0 8 6 0)
(5 0 6 7 0 0 0 0 4) (0 0 0 0 4 6 0 5 9))

((7 0 5 6 0 0 0 4 2) (0 0 0 7 0 0 6 0 0) (0 0 3 0 0 4 0 7 5) (0 3 0 0
0 0 1 5 8) (0 1 6 0 0 0 4 2 0) (4 5 7 0 0 0 0 6 0) (6 2 0 9 0 0 5 0 0)
(0 0 9 0 0 8 0 0 0) (5 8 0 0 0 6 7 0 9))

((7 0 0 0 0 6 0 0 0) (0 2 5 0 0 7 6 0 3) (0 6 3 2 0 0 0 0 9) (1 3 4 0
0 0 0 9 0) (2 5 0 0 0 0 0 1 7) (0 7 0 0 0 0 2 3 6) (3 0 0 0 0 8 7 5 0)
(6 0 8 7 0 0 3 4 0) (0 0 0 4 0 0 0 0 8))

((0 0 2 0 6 7 0 8 0) (0 3 0 9 1 0 0 2 0) (8 0 0 0 0 0 1 0 6) (0 5 8 0
0 2 0 0 9) (0 4 6 0 9 0 5 3 0) (7 0 0 5 0 0 2 1 0) (5 0 9 0 0 0 0 0 1)
(0 8 0 0 2 9 0 5 0) (0 6 0 8 7 0 9 0 0))

((3 0 0 2 5 0 1 0 4) (0 2 0 0 0 9 0 6 0) (0 0 9 4 3 0 0 2 0) (9 0 0 0
0 4 8 0 0) (4 0 8 0 9 0 6 0 2) (0 0 1 3 0 0 0 0 9) (0 9 0 0 6 1 4 0 0)
(0 3 0 8 0 0 0 1 0) (1 0 6 0 7 3 0 0 5))

((0 0 0 0 7 0 6 0 0) (6 0 0 8 0 3 2 1 0) (4 0 0 1 0 2 0 0 0) (2 0 5 3
0 0 0 4 0) (0 4 6 0 8 0 5 3 0) (0 8 0 0 0 5 1 0 6) (0 0 0 9 0 8 0 0 1)
(0 9 3 4 0 1 0 0 7) (0 0 4 0 5 0 0 0 0))

((6 7 0 2 0 0 8 0 5) (0 0 0 0 7 8 3 2 0) (8 3 0 4 0 0 0 7 0) (0 2 9 8
0 0 7 0 0) (0 8 0 0 0 0 0 6 0) (0 0 7 0 0 9 4 1 0) (0 4 0 0 0 7 0 3 2)
(0 9 3 1 4 0 0 0 0) (7 0 6 0 0 2 0 4 1))

((3 7 0 0 9 8 0 0 4) (4 0 9 0 5 7 0 0 1) (0 1 0 3 0 0 7 0 0) (0 0 0 0
2 5 9 8 0) (0 0 0 0 0 0 0 0 0) (0 2 7 9 8 0 0 0 0) (0 0 6 0 0 1 0 4 0)
(8 0 0 2 6 0 1 0 7) (7 0 0 8 3 0 0 5 2))

((6 8 0 7 0 0 9 0 5) (0 0 9 4 0 0 0 0 0) (0 1 0 0 5 0 3 0 8) (1 0 5 0
3 2 0 0 0) (4 0 0 1 7 5 0 0 2) (0 0 0 9 4 0 5 0 1) (2 0 8 0 6 0 0 5 0)
(0 0 0 0 0 4 7 0 0) (5 0 6 0 0 1 0 8 3))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'sudoku)

;;;;;;;;;;;;;;;;;