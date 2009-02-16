;; spread.el -- A very simple spreadsheet mode for GNU emacs
;; by Benjamin C. Pierce
;;
;; LCD Archive Entry:
;; spread|Benjamin C. Pierce|bcp@dcs.ed.ac.uk|
;; A simple spreadsheet for Emacs |
;; Dec-1993|Version 1.5|~/modes/spread.el.Z|
;;
;; Installation instructions:
;;     - Place spread.el in a directory on your emacs load path
;;     - Add the following lines to your .emacs file:
;;         (setq auto-mode-alist (cons '("\\.sp$"  . spread-mode) 
;;                                      auto-mode-alist))
;;         (autoload 'spread-mode "spread")
;;
;; Complete documentation appears in the header of the spread-mode 
;; function, at the top of this file.  
;;
;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.
;;   Spread-mode is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;   You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA. 


(defun spread-mode ()
  "Major mode for simple spreadsheets.

Quick reference:
    recalculate                            \\[spread-recalc]
    toggle overwrite-mode                  \\[overwrite-mode]
    align next => with last                \\[spread-format-next-cell]
    jump to => on this line (or add one)   \\[spread-jump-to-formula] 


OVERVIEW
--------

A spreadsheet is an ordinary text buffer with embedded \"cells\" of
the form

       VALUE <= FORMULA 

or

       VALUE <= FORMULA => NAME

where

    * VALUE, the current value of the cell, is a single word (typically a 
      number);
    * FORMULA is an arbitrary lisp expression, used to recalculate VALUE; and
    * NAME, if present, is a lisp variable to which VALUE is assigned 
      after each recomputation.

A single recalculation step consists of scanning the buffer, recalculating
each cell by replacing the current VALUE by the result of evaluating FORMULA.
A complete recalculation, triggered by typing \\[spread-recalc], iterates this process
until the buffer stops changing.

When a old value is replaced, the first character of the newly
computed value is placed in the same column as the first character of
the old.  If the values are numeric, the new value is truncated to the
same number of decimal places as the old.  The spacing of the
remainder of the line is preserved.


FORMULAS
--------

The formula associated with a cell may be just a constant.  This form is 
useful for making names for common constants; e.g.:

        10 <= 10 => length

More generally, a formula may involve arbitrary arithmetic calculations
including variables whose values are set by other cells:

        555   <= 555 => breadth
        5550  <= (* length breadth) => area
       $29137 <= (* area 5.25)   => total-cost

One very useful lisp function is predefined for use in formulas: the
expression (total) returns the sum of the column of numbers appearing
above the value part of the current cell.  (More precisely, it moves
the cursor upwards, beginning at the leftmost character of the current
value, till it finds a number; then it continues upward until it fails
to find a number beginning in this column.  The result is the sum of
all the numbers in between.)  For example:

                  $25   
                  $29137                 <= total-cost
                  $55   
                  $888  
                  $-20
                  ========
     total cost:  $30085                 <= (total)

Note that:
   1) All the numbers in the list must be left-justified for
      (total) to work properly.
   2) The dollar signs can be contiguous with the numbers, since they are 
      not considered part of the values.
   3) The line above the total is composed of equal-signs, not dashes,
      since these would be misinterpreted as negation-signs.

In Emacs version 19 (both FSF and Lucid), floating-point numbers may
also be used in formulas.  If the value part of a formula is written
with a decimal point, new values will be truncated to the same length 
when it is updated.

Another useful function is (date-and-time), which returns the current
date and time compressed into a single word:

              Nov-18-1993-19:18    <= (date-and-time)  


EDITING
-------

Spread-mode provides some simple support for editing spreadsheets:
    * \\[overwrite-mode] toggles overwrite mode (q.v.)
    * \\[spread-format-next-cell] formats the next cell so that the => appears 
      exactly under the => of the previous cell
    * \\[spread-jump-to-formula] moves the cursor to the formula of the cell on the current
      line, or (if the current line does not contain =>) jumps to the
      same column as the previous cell's => and adds a =>.

Besides the keybindings described above, the bindings of spread-mode
are just like those of text-mode.


CUSTOMIZATION
-------------

Invoking spread-mode calls the value of text-mode-hook and then of
spread-mode-hook, if they are non-nil."

  (interactive)
  (kill-all-local-variables)
  (setq truncate-lines t)
  (use-local-map spread-mode-map)
  (setq mode-name "Spread")
  (setq major-mode 'spread-mode)
  (define-abbrev-table 'text-mode-abbrev-table ())
  (setq local-abbrev-table text-mode-abbrev-table)
  (set-syntax-table text-mode-syntax-table)
  (modify-syntax-entry ?- "w")
  (modify-syntax-entry ?. "w")
  (modify-syntax-entry ?: "w")
  (modify-syntax-entry ?$ "_")
  (run-hooks 'text-mode-hook 'spread-mode-hook)
  (auto-fill-mode nil)
  (spread-initialize-variables)
)

;; --------------------------------------------------------------------------
;; Set-up and keybindings

(defvar spread-running-18 (string-match "^18" emacs-version))
(defvar spread-running-FSF19 
  (and (string-match "^19" emacs-version) 
       (not (string-match "Lucid" (emacs-version)))))

(defvar spread-mode-map nil "")

(if spread-mode-map nil
    (setq spread-mode-map (copy-keymap text-mode-map))
  (define-key spread-mode-map "\C-c\C-c" 'spread-recalc)
  (define-key spread-mode-map "\C-c\C-o" 'overwrite-mode)
  (define-key spread-mode-map "\C-c1"    'spread-recalc-once)
  (define-key spread-mode-map "\C-c\C-v" 'spread-initialize-variables)
  (define-key spread-mode-map "\C-c\C-i" 'spread-format-next-cell)
  (define-key spread-mode-map "\C-c\C-x" 'spread-jump-to-formula)
)

(defun spread-initialize-variables ()
  (interactive)
  (let (val name pos)
    (spread-debug "Initializing variables")
    (save-excursion
      (goto-char (point-min))
      (while (search-forward "=>" (point-max) t)
        (setq pos (point))
        (search-backward "<=")
        (re-search-backward "[-0-9.]")
        (setq val (spread-number-under-cursor))
        (goto-char pos)
        (re-search-forward "\\w+")
        (setq name 
              (intern (buffer-substring (match-beginning 0) (match-end 0))))
        (spread-debug "%s := %s" name val)
        (make-variable-buffer-local name)
        (set name val)))))

;; --------------------------------------------------------------------------
;; Recalculation

(defvar spread-recalc-limit 40 
  "*Maximum iterations of spreadsheet recalculation")

(defun spread-recalc ()
  "Recalculate all computed cells in buffer, iterating until all cells'
values have stabilized or for SPREAD-RECALC-LIMIT iterations, whichever
comes first."
  (interactive)
  (message "Recalculating... ")
  (let ((limit 0))
    (while (save-excursion (spread-recalc-once))
      (message "Recalculating... (%s)" limit)
      (sit-for 0)
      (setq limit (+ limit 1))
      (if (= limit spread-recalc-limit)
          (spread-error "Recalculation looping!"))))
    (message "Recalculating... done"))

(defun spread-get-next-cell (cont)
  (if (search-forward "<=" (point-max) t)
      (let (after eol res start end contents var formula formula-start)
        (setq formula-start (point))
        (forward-word -1)
        (setq start (point))
        (forward-word 1)
        (setq end (point))
        (setq contents (buffer-substring start end))
        (end-of-line)
        (setq eol (point))
        (goto-char formula-start)
        (setq res (read-from-string (buffer-substring formula-start eol)))
        (setq formula (car res))
        (forward-char (cdr res))
        (setq after (point))
        (if (looking-at "[ \t]*=>[ \t]*\\(\\w+\\)")
            (let ((b (match-beginning 1))
                  (e (match-end 1)))
              (setq after e)
              (setq var (intern (buffer-substring b e)))))
        (if spread-running-FSF19
            (spread-fontify-cell start end after))
        (funcall cont start end contents var formula after)
        t)
    nil))

(defun spread-recalc-once ()
  "Recalculate all computed cells in buffer.  Return T if any of them
change their values, NIL otherwise."
  (interactive)
  (spread-debug "Recalculating once")
  (let ((any-changes nil)
        cell)
    (goto-char (point-min))
    (while (spread-get-next-cell
            '(lambda (cell-start cell-end contents var formula formula-end)
               (goto-char cell-start)
               (setq new (spread-eval formula))
               (setq new-string (spread-format-like contents new))
               (setq new-length (length new-string))
               (spread-debug "%s  <---  %s    from %s" 
                             contents new-string formula)
               (if (not (string= new-string contents))
                   (progn 
                     (setq any-changes t)
                     (goto-char cell-start)
                     (if (>= new-length (length contents))
                         (progn
                           (delete-region cell-start (+ cell-start new-length))
                           (insert new-string))
                       (progn
                         (delete-region cell-start cell-end)
                         (insert 
                          new-string
                          (make-string (- (length contents) new-length) 32)))
                       )))
               (if var 
                   (progn
                     (spread-debug "%s := %s" var new)
                     (set var new)))
               (goto-char formula-end)
               )))
    any-changes))

(defun spread-eval (exp)
  (condition-case err
      (eval exp)
    (void-variable 
     (let ((r 88888))
       (spread-warning 
        (format "Variable \"%s\" is unbound; using value %d" 
                (car (cdr err)) r))
       r))
    (error 
     (let ((r 99999))
       (spread-warning 
        (format "Evaluation failed with \"%s\"; using value %d" err r))
       r))))

;; ----------------------------------------------------------------------
;; Formatting and editing

(defun spread-format-next-cell ()
  (interactive)
  (search-forward "<=")
  (let ((after (point))
        (prev-indent (spread-last-formula-column)))
    (forward-char -2)
    (re-search-backward "[^ \t]")
    (forward-char 1)
    (delete-region (point) (- after 2))
    (indent-to prev-indent)
    (forward-char 2)
    ))

(defun spread-last-formula-column ()
  (let ((here (point))
        there)
    (beginning-of-line)
    (if (not (search-backward "<=" (point-min) t))
        (error "No previous cell to imitate!"))
    (setq there (current-column))
    (goto-char here)
    there))

(defun spread-jump-to-formula ()
  (interactive)
  (let ((eol (progn (end-of-line) (point))))
    (beginning-of-line)
    (if (search-forward "<=" eol t)
        (re-search-forward "\\b")
      (progn
        (goto-char eol)
        (insert " <= ")
        (forward-char -4)
        (spread-format-next-cell)
        (end-of-line)))))

;; ----------------------------------------------------------------------
;; Some useful recalculation functions

(defun total ()
  "Return the total of the list of numbers above this position,
not including the number under the cursor."
  (let ((sum 0))
    (while (spread-find-number-above)
      (setq sum (+ sum (spread-number-under-cursor))))
    sum))

(defun date-and-time ()
  "Returns the current date and time as a string, stripping the seconds
and substituting dashes for blanks"
  (interactive)
  (concat (substring (current-time-string) 4 7)
          "-"
          (if (string= (substring (current-time-string) 8 9) " ")
              ""
            (substring (current-time-string) 8 9))
          (substring (current-time-string) 9 10)
          "-"
          (substring (current-time-string) 20)
          "-"
          (substring (current-time-string) 11 16)
          ))

;; ----------------------------------------------------------------------
;; Utility functions

(defun spread-find-number-above ()
  "Moves directly up until either a number or a blank line is found.  Returns
T for a number, NIL for a blank line."
  (interactive)
  (let ((done nil)
        (result t))
    (while (not done)
      (let ((here (point)))
        (previous-line 1)
        (cond
         ((= (point) here)
          (setq result nil 
                done t))
          ((looking-at "[ \t\n]")
           (setq result nil
                 done t))
          ((looking-at "[-0-9.]")
           (setq done t)))))
    result))

(defun spread-number-under-cursor ()
  (interactive)
    (save-excursion
      (goto-char (+ (point) 1))
      (re-search-backward "\\(^\\|[^-0-9.]\\)\\([-0-9.]\\)")
      (let ((begin (match-beginning 2)))
        (goto-char begin)
        (re-search-forward "[^-0-9.]")
        (string-to-int
         ;; (in V19, this actually returns a float if necessary!)
         (buffer-substring begin (match-beginning 0))))))

(defun spread-format-like (old new)
  (if spread-running-18
      ;; If we're running emacs 18, then floating point numbers
      ;; do not make sense anyway, so just format it as an integer
      (int-to-string new)
    (progn
      (let ((old-decimal (string-match "\\." old)))
        (setq new 
              (cond
               ((stringp new) new)
               ((numberp new)
                (if old-decimal
                    (let ((oldprecision (- (length old) 1 old-decimal)))
                      (spread-float-to-string-with-precision 
                       new oldprecision))
                  (int-to-string (truncate new))))
               (t 
                (prin1-to-string new))))
        (if (spread-contains-char new 32)
            (setq new (concat "\"" new "\"")))
        new
        ))))

(defun spread-float-to-string-with-precision (n p)
  (let ((float-output-format (concat "%." (int-to-string p) "f")))
    (format "%s" (float n))))

(defun spread-contains-char (s c)
  (let ((len (length s))
        (i 0)
        (found nil))
    (while (and (not found) (< i len))
      (if (char-equal (elt s i) c)
          (setq found t)
        (setq i (+ i 1))))
    found))

;; ----------------------------------------------------------------------
;; Font support for FSF19

; (if (string-match "^18" emacs-version) 
;     (error "Sorry: spread.el requires Emacs version 19 (FSF or Lucid)"))

(defun spread-fontify-cell (val-start val-end cell-end)
  (add-text-properties val-start val-end '(face bold))
  (add-text-properties (+ 1 val-end) cell-end '(face italic))
)

;; ----------------------------------------------------------------------
;; Debugging and error reporting

(defvar spread-debugging nil "*Debugging for spreadsheet recalculations")

(defun spread-debug (&rest args)
  (if spread-debugging
      (progn
        (save-window-excursion
          (switch-to-buffer "*Spread*")
          (goto-char (point-max))
          (insert (apply 'format args))
          (insert "\n")
        ))))

(defun spread-error (m)
  (error "Spreadsheet error: %s" m))

(defun spread-warning (m)
  (message "Warning: %s" m)
  (beep)
  (sit-for 2))

