;;; Debugging info for self: Saved through ges-version 1.5dev
;;; ;;; From: Guillaume Marceau <nnnczvv2n1bhf3001@sneakemail.com>
;;; ;;; Subject: spread.el - a spreadsheet in an Emacs buffer
;;; ;;; Newsgroups: gnu.emacs.sources
;;; ;;; Date: Fri, 26 Jul 2002 14:02:06 -0400
;;; ;;; Organization: IBM Global Services North -- Burlington, Vermont, USA

;;; Allows to make a spreadsheet out of an ordinary text buffer. `Cells' are text of
;;; the form:

;;;         VALUE <= FORMULA
;;; or
;;;         VALUE <= FORMULA => NAME

;;; where

;;;      * VALUE, the current value of the cell, is a single word (typically a
;;;        number), or a quoted string;
;;;      * FORMULA is an arbitrary lisp expression, used to recalculate VALUE; and
;;;      * NAME, if present, is a lisp variable to which VALUE is assigned
;;;        after each recomputation.

;;; A single recalculation step consists of scanning the buffer, recalculating
;;; each cell by replacing the current VALUE by the result of evaluating FORMULA.
;;; A complete recalculation, triggered by typing \\[spread-recalc], iterates this process
;;; until the buffer stops changing.

;;; The buffer can also contain simple free-form text. It will not interfer will the recalculations.

;;; ;; spread.el v1.0 -- A very simple spreadsheet mode for GNU emacs
;;; ;; written by Guillaume Marceau (zfhrdop2ww001@sneakemail.com) from
;;; ;; Benjamin C. Pierce original code and idea.
;;; ;;
;;; ;; Installation instructions:
;;; ;;     - Place spread.el in a directory on your emacs load path
;;; ;;     - Add the following lines to your .emacs file:
;;; ;;         (setq auto-mode-alist (cons '("\\.sp$"  . spread-mode)
;;; ;;                                      auto-mode-alist))
;;; ;;         (autoload 'spread-mode "spread")
;;; ;;
;;; ;; Complete documentation appears in the header of the spread-mode
;;; ;; function, at the top of this file.
;;; ;;
;;; ;; New stuff:
;;; ;; .  Less greedy cell parsing
;;; ;; .  Cell navigation key bindings : next, previous, up and down
;;; ;; .  You can now anonymously refer to other cell via spread-ref
;;; ;; .  Results too large to fit in their cell now appear as `#' and
;;; ;;      only cause an error if they are referenced to
;;; ;; .  Nicer handling of tab caracters around cells
;;; ;; .  Electric alignment of cell deals with tabs and also switch
;;; ;;      the fields of the current cell
;;; ;; .  Making grand-totals out of subtotals
;;; ;; .  Customization
;;; ;; .  Font-lock highlighting of cells
;;; ;;
;;; ;;
;;; ;; This package is free software; you can redistribute it and/or modify
;;; ;; it under the terms of the GNU General Public License as published by
;;; ;; the Free Software Foundation; either version 1, or (at your option)
;;; ;; any later version.
;;; ;;   Spread-mode is distributed in the hope that it will be useful,
;;; ;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; ;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; ;; GNU General Public License for more details.
;;; ;;   You should have received a copy of the GNU General Public License
;;; ;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; ;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

(defgroup spread-mode nil
   "Spread-mode turns your emacs buffer into a spread sheet"
   :group 'languages)

(defcustom spread-debugging nil "*If t, debugging of spreadsheet recalculations info goes to buffer *Spread*"
   :group 'spread-mode :type 'boolean)

(defcustom spread-recalc-limit 40
   "*Maximum iterations of spreadsheet recalculation"
   :group 'spread-mode :type 'integer)

(defface spread-cell-face
   '((((class grayscale) (background light)) (:foreground "LightGray" :bold t))
     (((class grayscale) (background dark)) (:foreground "DimGray" :bold t))
     (((class color) (background light)) (:foreground "Purple"))
     (((class color) (background dark)) (:foreground "Blue" :bold t))
     (t (:bold t)))
     "Spread-mode face used to highlight cells"
     :group 'spread-mode)



(defun spread-mode ()
   "Major mode for simple spreadsheets.

Quick reference:
     recalculate                                    \\[spread-recalc]
     jump between fields of a cell                  \\[spread-electric-equal]
     create a new cell aligned on the previous one  \\[spread-electric-equal]
     jump to next cell                              \\[spread-next-cell]
     jump to previous cell                          \\[spread-previous-cell]
     jump to next cell down                         \\[spread-cell-down]
     jump to next cell up                           \\[spread-cell-up]
     toggle overwrite-mode                          \\[overwrite-mode]


OVERVIEW
--------

A spreadsheet is an ordinary text buffer with embedded \"cells\" of
the form

        VALUE <= FORMULA

or

        VALUE <= FORMULA => NAME

where

     * VALUE, the current value of the cell, is a single word (typically a
       number), or a quoted string;
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
       $ 29137 <= (* area 5.25)   => total-cost

One very useful lisp function is predefined for use in formulas: the
expression (total) returns the sum of the column of numbers appearing
above the value part of the current cell.  (More precisely, it moves
the cursor upwards, beginning at the leftmost character of the current
value, till it finds a number; then it continues upward until it fails
to find a number beginning in this column.  The result is the sum of
all the numbers in between.)  For example:

                   $ 25
                   $ 29137                 <= total-cost
                   $ 55
                   $ 888
                   $ -20
                   ========
      total cost:  $ 30085                 <= (total)

Note that all the numbers in the list must be left-justified for
'total' to work properly. There is also a 'subtotal' function which
works just the same as 'total'. Then there is a 'grand-total' which
will only add subtotals and allow itself to skip over a given number
of blank spaces.

In Emacs version 19 (both FSF and Lucid), floating-point numbers may
also be used in formulas.  If the value part of a formula is written
with a decimal point, new values will be truncated to the same length
when it is updated.

Another useful function is (date-and-time), which returns the current
date and time compressed into a single word:

               Nov-18-1993-19:18    <= (date-and-time)

It is possible to refer to value of other cell by position rather than
via the name of a function. Take note that the functions `spread-ref'
and `spread-abs-ref' will return the displayed value of the cell, as
truncated if a truncation took place.

EDITING
-------

Spread-mode provides some simple support for editing spreadsheets:

       * \\[overwrite-mode] toggles overwrite mode (q.v.)
       * \\[spread-electric-equal] flip between fields of a cell.  When used
              where no cell is present, it will create a cell aligned on the
              previous one

Besides the keybindings described above, the bindings of spread-mode
are just like those of text-mode.


CUSTOMIZATION
-------------

Invoking spread-mode calls the value of spread-mode-hook, if non-nil."

   (interactive)
   (let ((old-tab-width tab-width))
     (kill-all-local-variables)
     (setq tab-width old-tab-width))
   (hscroll-mode t)
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
   (spread-initialize-variables)
   (make-local-variable 'font-lock-defaults)
   (setq font-lock-defaults '(spread-font-lock-keywords t t nil 'spread-previous-cell))
   (run-hooks 'spread-mode-hook)
)

;; --------------------------------------------------------------------------
;; Set-up and keybindings

(defvar spread-running-18 (string-match "^18" emacs-version))

(defvar spread-mode-map nil "")

(defvar spread-font-lock-keywords
   '((spread-font-lock-matcher 0 'spread-cell-face))
   "Default highlighting for spread mode")

(if spread-mode-map nil
   (setq spread-mode-map (copy-keymap text-mode-map))
   (define-key spread-mode-map "\C-c\C-c" 'spread-recalc)
   (define-key spread-mode-map "\C-c\C-o" 'overwrite-mode)
   (define-key spread-mode-map "\C-c1"    'spread-recalc-once)
   (define-key spread-mode-map "\C-c\C-v" 'spread-initialize-variables)
   (define-key spread-mode-map [tab] '(lambda () (interactive) (insert ?\t)))
   (define-key spread-mode-map [\C-tab] 'spread-electric-equal)
   (define-key spread-mode-map [\C-\S-tab] '(lambda () (interactive) (spread-electric-equal -1)))
   (define-key spread-mode-map [\M-down]  'spread-cell-down)
   (define-key spread-mode-map [\M-up]    'spread-cell-up)
   (define-key spread-mode-map [\M-right] 'spread-next-cell)
   (define-key spread-mode-map [\M-left]  'spread-previous-cell)
   )

(defun spread-initialize-variables ()
   (interactive)
   (let (cell-bounds symbol)
     (spread-debug "Initializing variables")
     (save-excursion
       (goto-char (point-min))
       (while (setq cell-bounds (and (spread-next-cell 1 nil t) (spread-cell-at-point)))
         (progv '(content first-blank function second-blank variable)
             (spread-cell-content cell-bounds)
           (if variable (progn (spread-debug "%s := %s" variable content)
                               (setq symbol (intern variable))
                               (make-variable-buffer-local symbol)
                               (set symbol content)))
           (goto-char (nth 5 cell-bounds)))))))

;; ----------------------------------------------------------------------
;; Motion

(defun spread-next-cell (&optional n bound noerror)
   (interactive "p")
   (setq n (or n 1))
   (cond ((< n 0) (spread-previous-cell (- n) bound noerror))
         ((> n 0) (condition-case nil
                      (let ((current-cell (spread-cell-under-point)))
                        (if current-cell (goto-char (nth 5 current-cell)))
                        (if (search-forward-regexp spread-cell-regexp bound noerror n)
                            (progn (spread-skip-to-char (match-beginning 0))
                                   (spread-cell-at-point))))
                    (error (end-of-buffer) (error "End of buffer"))))
         (t (spread-cell-at-point))))

(defun spread-previous-cell (&optional n bound noerror)
   (interactive "p")
   (setq n (or n 1))
   (cond ((< n 0) (spread-next-cell (- n) bound noerror))
         ((> n 0) (condition-case nil
                      (let ((current-cell (spread-cell-under-point)))
                        (if current-cell (goto-char (car current-cell)))
                        (if (search-backward-regexp spread-cell-regexp bound noerror n)
                            (progn (spread-skip-to-char (match-beginning 0))
                                   (spread-cell-at-point))))
                    (error (beginning-of-buffer) (error "Beginning of buffer"))))
         (t (spread-cell-at-point))))


(defun spread-cell-down (&optional n)
   (interactive "p")
   (setq n (or n 1))
   (let ((column (current-column))
         (p (point)))
     (while (/= n 0)
       (condition-case err
           (progn (while (let ((cell-bounds (spread-next-cell (signum n))))
                           (save-excursion
                             (move-to-column column)
                             (not (and cell-bounds
                                       (spread-cell-contains cell-bounds (point))))))
                    nil)
                  (move-to-column column))
         (error (goto-char p) (signal (car err) (cdr err))))
       (setq n (- n (signum n))))))


(defun spread-cell-up (&optional n)
   (interactive "p") (spread-cell-down (if n (- n) -1)))

;; --------------------------------------------------------------------------
;; Cell parsing

(defvar spread-cell-regexp
   (let* ((quoted-string-regex "\\(\"\\([^\"\\\\\n]\\|\\\\\"\\|\\\\\\\\\\)*\"\\)")
          (word-regex "[^ \t\n\"]+")
          (spread-value-regex (concat "\\(" quoted-string-regex "\\|" word-regex "\\)")))
     (concat "\\(^\\|[ \t]\\)"
             spread-value-regex
             "[ \t]*\\(<=\\)[ \t]*"
             "\\(\\w+\\|(\\)")))

(defun spread-cell-at-point ()
   "Returns nil if there the character at the point do not begin a cell. Otherwise returns
the cell-bounds list : (cell-beginning cell-content-end cell-blank-end cell-function-end
cell-variable-beginning cell-end). Cell-variable is nil is the cell
does not have a variable assignement.
"
   (let (cell-beginning content-end blank-end function-end variable-beginning cell-end)
     (save-excursion

       (or (bolp) (backward-char))
       (if (not (looking-at spread-cell-regexp)) nil
         (setq cell-beginning (match-beginning 2))
         (setq content-end (match-end 2))
         (setq blank-end (+ (match-beginning 5) 2))
         (setq function-end (if (string= (match-string 6) "(")
                                (scan-sexps (match-beginning 6) 1)
                              (match-end 6)))
         (setq variable-beginning
               (progn (goto-char function-end)
                      (and (looking-at "[ \\t]*=>[ \\t]*\\(\\w+\\)")
                           (match-beginning 1))))
         (setq cell-end (if variable-beginning (match-end 0) function-end))
         (list cell-beginning content-end blank-end function-end variable-beginning cell-end)))))

(defun spread-cell-contains (cell-bounds pos)
   (and (>= pos (nth 0 cell-bounds))
        (< pos (nth 5 cell-bounds))))

(defun spread-cell-under-point ()
   (save-excursion
     (let ((p (point)))
       (while (and (not (looking-at spread-cell-regexp))
                   (not (bolp)))
         (backward-char))
       (if (looking-at spread-cell-regexp)
           (progn (spread-skip-to-char (match-beginning 0))
                  (let ((cell-bounds (spread-cell-at-point)))
                    (if (spread-cell-contains cell-bounds p) cell-bounds)))))))


(defun spread-font-lock-matcher (bound)
   (condition-case nil
       (let ((cell-bounds (spread-next-cell 1 bound t)))
         (if (not cell-bounds) nil
           (let ((beg-marker (car cell-bounds))
                 (end-marker (nth 5 cell-bounds)))
             (store-match-data (list beg-marker end-marker))
             t)))
     (error nil)))




(defun spread-cell-content (cell-bounds)
   "Convert a cell-bounds list, as returned by
`spread-search-cell-raw', into a list (content first-blank function
second-blank variable). In the absence of a variable, both
second-blank and variable are nil. Quotes at not processed at this
level. For instance, the text of the cell can be reproduced with
(apply concat (spread-cell-content cell-bounds))."
   (if (not cell-bounds) nil
     (progv '(cell-beginning content-end blank-end function-end variable-beginning cell-end)
         cell-bounds
       (list (buffer-substring cell-beginning content-end)
             (buffer-substring content-end blank-end)
             (buffer-substring blank-end function-end)
             (and variable-beginning (buffer-substring function-end variable-beginning))
             (and variable-beginning (buffer-substring variable-beginning cell-end))))))



;; --------------------------------------------------------------------------
;; Recalculation


(defun spread-recalc ()
   "Recalculate all computed cells in buffer, iterating until all cells'
values have stabilized or for SPREAD-RECALC-LIMIT iterations, whichever
comes first."
   (interactive)
   (message "Recalculating... ")
   (let ((limit 0))
     (while (spread-recalc-once)
       (message "Recalculating... (%s)" limit)
       (setq limit (+ limit 1))
       (if (= limit spread-recalc-limit)
           (error "recalculation looping!")))
     (message "Recalculating... done")
     (if (and (> limit 0) (assoc 'font-lock-mode minor-mode-alist)) (font-lock-fontify-buffer))))

(defun spread-recalc-once ()
   (interactive)
   (let ((start-point (point)) any-changes cell new-value new-content new-content-and-blank
         new-cell-string content-growt discardable-blank-length)

     (goto-char (point-min))

     (while (setq cell-bounds (and (spread-next-cell 1 nil t) (spread-cell-at-point)))
       (progv '(cell-beginning content-end blank-end function-end variable-beginning cell-end)
           cell-bounds
         (progv '(content first-blank function second-blank variable)
             (spread-cell-content cell-bounds)

           (spread-debug "%s\t%s" function content)

           (goto-char cell-beginning)
           (condition-case err (setq new-value (eval (car (read-from-string function))))
             (void-variable
              (error "Spreadsheet error: variable `%s' is unbound" (car (cdr err))))
             (error
              (error "Spreadsheet error: %s" (car (cdr err)))))

           (goto-char cell-beginning)

           ; convert string to numeric if possible
           (setq new-value (or (and (stringp new-value) (spread-string-to-int new-value))
                               new-value))

           (setq new-content (concat "" (spread-format-like content new-value)))

           (setq discardable-blank-length
                 (progn (string-match "^ *" first-blank)
                        (match-end 0)))

           (setq content-growt (- (length new-content) (length content)))

           (let (content-does-not-fit)
             (setq new-content-and-blank
                   (cond ((<= content-growt 0)
                          ;; Contents srinks
                          (concat new-content
                                  (make-string (- content-growt) 32)
                                  first-blank ))

                         ((<= content-growt discardable-blank-length)
                          ;; Content did not grow too much
                          (concat new-content (substring first-blank content-growt)))

                         ((string-match "\t" first-blank)
                          ;; Blank contains a tab which takes care of the alignement
                          (concat new-content
                                  (substring first-blank discardable-blank-length)))

                         ;; Uh oh, new cell value does not fit
                         (t (progn (setq content-does-not-fit t)
                                   (concat "#" (make-string (1- (length content)) 32)
                                           first-blank)))))
             (let ((var-differ (and variable
                                    (condition-case nil
                                        (not (equal (eval (intern variable)) new-value))
                                      (void-variable t))))
                   (content-differ (not (string= (buffer-substring cell-beginning blank-end)
                                                 new-content-and-blank))))

               (if (or var-differ content-differ)
                   (progn
                     (if variable (set (intern variable) new-value))
                     (setq any-changes t)

                     (spread-debug "%s\t%s -> %s" function content new-content)

                     (setq new-cell-string (concat new-content-and-blank function second-blank variable))
                     (delete-region cell-beginning cell-end)
                     (insert new-cell-string))))

             (if content-does-not-fit
                 (put-text-property cell-beginning (1+ cell-beginning) 'spread-mode 'content-does-not-fit))))



         (goto-char cell-end)))

     (goto-char start-point)
     any-changes))

;; ----------------------------------------------------------------------
;; Formatting and editing


(defun spread-electric-equal (&optional direction)
   "Switch between the fields of the current cell. If there isn't a cell where the point
    is, it creates one. The cell thus created is aligned with the previous cell, or with
    the cell up if any"
   (interactive)
   (if (not direction) (setq direction 1))
   (let ((cell-bounds (or (spread-cell-under-point)
                          (save-excursion (backward-char) (spread-cell-under-point)))))

     (if cell-bounds
         (progv '(cell-beginning cell-content-end cell-blank-end
                                 cell-function-end cell-variable-beginning cell-end)
             cell-bounds
           ;; Motion
           (let ((pos (cond ((< (point) cell-blank-end) 0)
                            ((< (point) cell-function-end) 1)
                            (t 2))))
             (setq pos (mod (+ pos direction) 3))
             (cond ((= pos 0) (goto-char cell-beginning))
                   ((= pos 1) (progn (spread-skip-to-char cell-blank-end)))
                   ((= pos 2) (spread-skip-to-char
                               (or cell-variable-beginning cell-function-end))))))
       (progn
         ;; Insertion
         (let ((starting-col (current-column)))
           (skip-syntax-forward "w")
           (let* ((previous-cell-bounds (save-excursion (spread-previous-cell 1 nil t)))
                  (up-cell-bounds (save-excursion
                                    (condition-case nil
                                        (progn (spread-cell-up)
                                               (spread-cell-under-point))
                                      (error nil))))
                  (reference-cell-bounds (or up-cell-bounds previous-cell-bounds))

                  content-col symbol-col)

             (if (not reference-cell-bounds) (insert " <= ")
               (progv '(cell-beginning content-end blank-end function-end variable-beginning cell-end)
                   reference-cell-bounds

                                         ; pick up the reference column numbers
                 (save-excursion
                   (goto-char (- blank-end 2))
                   (setq symbol-col (current-column))
                   (goto-char cell-beginning)
                   (setq content-col (current-column)))

                                         ; goto to desired content column
                 (move-to-column content-col)
                 (if (= (char-before cell-beginning) ?\t)
                     (while (> content-col (current-column))
                       (insert "\t"))
                   (move-to-column content-col t))

                                         ; add some dummy content if there isn't any already
                 (if (and (or (bobp) (= (char-syntax (char-before)) 32))
                          (or (eobp) (= (char-syntax (char-after)) 32)))
                     (insert "#"))

                                         ; goto to desired function column
                 (move-to-column symbol-col)
                 (if (= (char-before (- blank-end 2)) ?\t)
                     (while (> symbol-col (current-column))
                       (insert "\t"))
                   (move-to-column symbol-col t))

                 (cond ((looking-at "<=")
                                         ; this is a partial cell, cycle its partial fields
                        (if (> (current-column) starting-col) (move-to-column (+ symbol-col 3))
                          (move-to-column content-col) (skip-syntax-backward "w")))

                       ((or (or (bobp) (= (char-syntax (char-before)) 32))
                            (or (eobp) (= (char-syntax (char-after)) 32)))
                                         ; free space for insertion
                        (insert "<=")
                        (if (= (char-syntax (char-after blank-end)) 32)
                            (insert (char-after blank-end))))

                       (t (error "There is already some text at alignment point")))))))))))



;; ----------------------------------------------------------------------
;; Some useful calculation functions

(defalias 'subtotal 'total)
(defalias 'ref 'spread-ref)
(defalias 'absref 'spread-abs-ref)

(defun self ()
   "Returns the current content of the cell itself."
   (spread-ref))

(defun spread-ref (&rest args)
   "Move to the specified cell and returns its content. ARGS is of the form:
    NUMBER-NEXT-CELL-MOVES NUMBER-DOWN-CELL-MOVES ...
    The numbers can be negative."
   (save-excursion
     (let (vertical)
       (while args
         (if vertical (spread-cell-down (car args))
           (spread-next-cell (car args)))
         (setq vertical (not vertical))
         (setq args (cdr args))))

     (let ((cell-bounds (spread-cell-at-point)))
       (progv '(cell-beginning content-end blank-end function-end variable-beginning cell-end)
           cell-bounds
         (progv '(content first-blank function second-blank variable)
             (spread-cell-content cell-bounds)

           (if (eq (get-text-property cell-beginning 'spread-mode) 'content-does-not-fit)
               (error "refering to cell \"%s\" which did not fit" function))
           (spread-interpret-content (car (spread-cell-content cell-bounds))))))))


(defun spread-abs-ref (&rest args)
   "Same as spread-ref but counts from the beginning of the buffer"
   (save-excursion
     (goto-char (point-min))
     (apply 'spread-ref args)))

(defun spread-abs-cross-ref (filename &rest args)
   "Same as spread-abs-ref with the possibility of refering to another file"
   (save-excursion
     (if (not (file-exists-p filename)) (error "cannot find file `%s' for cross reference" filename))
     (set-buffer (find-file-noselect filename t))
     (apply 'spread-abs-ref args)))

(defun spread-data-below (&optional direction max-blank-lines)
   (interactive)
   (save-excursion
     (let ((column (current-column)) (data nil) (spaces 0))
       (while (cond ((> spaces max-blank-lines) nil)
                    ((/= (forward-line direction) 0) nil)
                    ((or (/= (move-to-column column) column)
                         (looking-at "[ \t \n]"))
                     (progn (setq spaces (1+ spaces))
                            (/= spaces max-blank-lines)))
                    (t (setq data (cons (thing-at-point 'word) data)) t)))
       data)))

(defun total ()
   "Return the total of the list of numbers above this position,
not including the number under the cursor."
   (interactive)
     (let ((sum 0) (data (spread-data-below -1 0)))
       (while data
         (setq sum (+ sum (string-to-int (car data))))
         (setq data (cdr data)))
       sum))

(defun grand-total (n)
   "Return the total of cells above this position which
are computed with subtotal. Allows N blanks between groups"
   (interactive "p")
   (save-excursion
     (let ((column (current-column)) (sum 0) (spaces 0))
       (while (cond ((= spaces n) nil)
                    ((/= (forward-line -1) 0) nil)
                    ((or (/= (move-to-column column) column)
                         (looking-at "[ \t \n]"))
                     (progn (setq spaces (1+ spaces))
                            (/= spaces n)))
                    (t (let* ((cell-bounds (spread-cell-under-point))
                              (cell-content (spread-cell-content cell-bounds)))
                         (setq spaces 0)
                         (if (and cell-bounds
                                  (string-match "subtotal" (nth 2 cell-content)))
                             (progn (setq sum (+ sum (string-to-int (car cell-content))))))
                         t))))
       sum)))


(defun spread-interpret-date (date)
   (let* ((tokens (split-string date))
          (day (string-to-int (nth 2 tokens)))
          (month-name (downcase (nth 1 tokens)))
          (month-names '("january" "febuary" "mars" "april" "may"
"june" "july" "august" "september" "october" "november" "december"))
          ;; ;; While-break loop
          month)

     (let ((i 1) (current month-names))
       (while (cond ((not current) (error "Bad month name `%s'" month-name))
                    ((string= month-name (car current)) (setq month i) nil)
                    (t (setq current (cdr current)) (setq i (1+ i)) t))))

          ;; ;; C++'s standard issue loop
          ;;int i=1;
          ;;for(Link *current = month_names; current; current = current->next, i++)
          ;;    if (strcmp(month_name, current->data) == 0) return i;
          ;;throw "Bad month name"

          ;; ;; Java's loops
          ;;int i = 1;
          ;;for(Iterator monthIt = monthNames.iterator(); monthIt.hasNext(); )
          ;;{
          ;;    String current = (String)monthIt.next();
          ;;    if (monthName.equals(current)) return i;
          ;;    i++;
          ;;}
          ;;throw new RuntimeException("Bad month name");

          ;; ;; Emacs' imperative while loop returning nil :
          ;;(month (or (let ((i 0) (current month-names) rtn done)
          ;;             (while (not done)
          ;;               (cond ((not current) (setq done t))
          ;;                     ((string= month-name (car current)) (setq rtn i) (setq done t))
          ;;                     (t (setq i (1+ i)) (setq current (cdr current)))))
          ;;             rtn)
          ;;           (error "Bad month name")))

          ;; ;; Emacs' imperative while loop which can't return nil easily :
          ;;(month (let ((i 0) (current month-names) rtn)
          ;;         (while (not (cond ((not current) (error "Bad month name"))
          ;;                           ((string= month-name (car current)) (setq rtn i))
          ;;                           (t (setq i (1+ i)) (setq current (cdr current)) nil))))
          ;;         rtn))

          ;; ;; Recursive :
          ;;(month (progn (defun loop-fn (i current)
          ;;                (cond ((not current) (error "Bad month names"))
          ;;                      ((string= month-name (car current)) 1)
          ;;                      (t (loop-fn (1+ i) (cdr current)))))
          ;;              (loop-fn 1 month-names)))

          ;; ;; Anonymous spinner :
          ;;(month (spinner ((i 1) (current month-names))
          ;;         (cond ((not current) (error "Bad month names"))
          ;;               ((string= month-name (car current)) 1)
          ;;               (t (spin (1+ i) (cdr current))))))

     (list month day 2002)))



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

(defun spread-string-to-int (value)
   (let ((num (string-to-int value)))
     (cond ((/= num 0) num)
           ((progn (and (= (string-match "\\s-*\\(\\(0+\\.?0*\\)\\|\\(0*\\.0+\\)\\)\\s-*" value) 0)
                        (= (match-end 0) (length value)))) 0)
           (t nil))))

(defun spread-interpret-content (value)
   (let ((unquoted (if (and (stringp value)
                            (string-match "^\".*\"$" value))
                       (read value) value)))
     (or (spread-string-to-int unquoted)
         unquoted)))

(defun spread-skip-to-char (n)
   (goto-char n)
   (if (looking-at "[ \t]+[^ \t\n]") (skip-syntax-forward "-")))


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

         (if (string-match " \\|\"" new) (setq new (pp-to-string new)))
         new
         ))))

(defun spread-float-to-string-with-precision (n p)
   (let ((float-output-format (concat "%." (int-to-string p) "f")))
     (format "%s" (float n))))


;; ----------------------------------------------------------------------
;; Font support for FSF19

(defun spread-fontify-cell (val-start val-end cell-end)
   (add-text-properties val-start val-end '(face bold))
   (add-text-properties (+ 1 val-end) cell-end '(face italic))
)

;; ----------------------------------------------------------------------
;; Debugging and error reporting

(defun spread-debug (&rest args)
   (if spread-debugging
       (progn
         (let ((line-number-tag (format "Line %d" (count-lines 1 (point)))))
           (save-window-excursion
             (switch-to-buffer "*Spread*")
             (goto-char (point-max))
             (insert line-number-tag " ")
             (insert (apply 'format args))
             (insert "\n")
             )))))

(provide 'spread)
;; spread.el v1.0 -- A very simple spreadsheet mode for GNU emacs
;; written by Guillaume Marceau (zfhrdop2ww001@sneakemail.com) from
;; Benjamin C. Pierce original code and idea.
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
;; New stuff:
;; .  Less greedy cell parsing
;; .  Cell navigation key bindings : next, previous, up and down
;; .  You can now anonymously refer to other cell via spread-ref
;; .  Results too large to fit in their cell now appear as `#' and
;;      only cause an error if they are referenced to
;; .  Nicer handling of tab caracters around cells
;; .  Electric alignment of cell deals with tabs and also switch
;;      the fields of the current cell
;; .  Making grand-totals out of subtotals
;; .  Customization
;; .  Font-lock highlighting of cells
;; 
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

(defgroup spread-mode nil
  "Spread-mode turns your emacs buffer into a spread sheet"
  :group 'languages)

(defcustom spread-debugging nil "*If t, debugging of spreadsheet recalculations info goes to buffer *Spread*"
  :group 'spread-mode :type 'boolean)

(defcustom spread-recalc-limit 40 
  "*Maximum iterations of spreadsheet recalculation"
  :group 'spread-mode :type 'integer)

(defface spread-cell-face
  '((((class grayscale) (background light)) (:foreground "LightGray" :bold t))
    (((class grayscale) (background dark)) (:foreground "DimGray" :bold t))
    (((class color) (background light)) (:foreground "Purple"))
    (((class color) (background dark)) (:foreground "Blue" :bold t))
    (t (:bold t)))
    "Spread-mode face used to highlight cells"
    :group 'spread-mode)



(defun spread-mode ()
  "Major mode for simple spreadsheets.

Quick reference:
    recalculate                                    \\[spread-recalc]
    jump between fields of a cell                  \\[spread-electric-equal]
    create a new cell aligned on the previous one  \\[spread-electric-equal]
    jump to next cell                              \\[spread-next-cell]
    jump to previous cell                          \\[spread-previous-cell]
    jump to next cell down                         \\[spread-cell-down]
    jump to next cell up                           \\[spread-cell-up]
    toggle overwrite-mode                          \\[overwrite-mode]


OVERVIEW
--------

A spreadsheet is an ordinary text buffer with embedded \"cells\" of
the form

       VALUE <= FORMULA 

or

       VALUE <= FORMULA => NAME

where

    * VALUE, the current value of the cell, is a single word (typically a 
      number), or a quoted string;
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
      $ 29137 <= (* area 5.25)   => total-cost

One very useful lisp function is predefined for use in formulas: the
expression (total) returns the sum of the column of numbers appearing
above the value part of the current cell.  (More precisely, it moves
the cursor upwards, beginning at the leftmost character of the current
value, till it finds a number; then it continues upward until it fails
to find a number beginning in this column.  The result is the sum of
all the numbers in between.)  For example:

                  $ 25   
                  $ 29137                 <= total-cost
                  $ 55   
                  $ 888  
                  $ -20
                  ========
     total cost:  $ 30085                 <= (total)

Note that all the numbers in the list must be left-justified for
'total' to work properly. There is also a 'subtotal' function which
works just the same as 'total'. Then there is a 'grand-total' which
will only add subtotals and allow itself to skip over a given number 
of blank spaces.

In Emacs version 19 (both FSF and Lucid), floating-point numbers may
also be used in formulas.  If the value part of a formula is written
with a decimal point, new values will be truncated to the same length 
when it is updated.

Another useful function is (date-and-time), which returns the current
date and time compressed into a single word:

              Nov-18-1993-19:18    <= (date-and-time)  

It is possible to refer to value of other cell by position rather than
via the name of a function. Take note that the functions `spread-ref'
and `spread-abs-ref' will return the displayed value of the cell, as
truncated if a truncation took place.

EDITING
-------

Spread-mode provides some simple support for editing spreadsheets: 

      * \\[overwrite-mode] toggles overwrite mode (q.v.)  
      * \\[spread-electric-equal] flip between fields of a cell.  When used
             where no cell is present, it will create a cell aligned on the
             previous one

Besides the keybindings described above, the bindings of spread-mode
are just like those of text-mode.


CUSTOMIZATION
-------------

Invoking spread-mode calls the value of spread-mode-hook, if non-nil."

  (interactive)
  (let ((old-tab-width tab-width))
    (kill-all-local-variables)
    (setq tab-width old-tab-width))
  (hscroll-mode t)
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
  (spread-initialize-variables)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(spread-font-lock-keywords t t nil 'spread-previous-cell))
  (run-hooks 'spread-mode-hook)
)

;; --------------------------------------------------------------------------
;; Set-up and keybindings

(defvar spread-running-18 (string-match "^18" emacs-version))

(defvar spread-mode-map nil "")

(defvar spread-font-lock-keywords
  '((spread-font-lock-matcher 0 'spread-cell-face))
  "Default highlighting for spread mode")

(if spread-mode-map nil
  (setq spread-mode-map (copy-keymap text-mode-map))
  (define-key spread-mode-map "\C-c\C-c" 'spread-recalc)
  (define-key spread-mode-map "\C-c\C-o" 'overwrite-mode)
  (define-key spread-mode-map "\C-c1"    'spread-recalc-once)
  (define-key spread-mode-map "\C-c\C-v" 'spread-initialize-variables)
  (define-key spread-mode-map [tab] '(lambda () (interactive) (insert ?\t)))
  (define-key spread-mode-map [\C-tab] 'spread-electric-equal)
  (define-key spread-mode-map [\C-\S-tab] '(lambda () (interactive) (spread-electric-equal -1)))
  (define-key spread-mode-map [\M-down]  'spread-cell-down)
  (define-key spread-mode-map [\M-up]    'spread-cell-up)
  (define-key spread-mode-map [\M-right] 'spread-next-cell)
  (define-key spread-mode-map [\M-left]  'spread-previous-cell)
  )

(defun spread-initialize-variables ()
  (interactive)
  (let (cell-bounds symbol)
    (spread-debug "Initializing variables")
    (save-excursion
      (goto-char (point-min))
      (while (setq cell-bounds (and (spread-next-cell 1 nil t) (spread-cell-at-point)))
        (progv '(content first-blank function second-blank variable)
            (spread-cell-content cell-bounds)
          (if variable (progn (spread-debug "%s := %s" variable content)
                              (setq symbol (intern variable))
                              (make-variable-buffer-local symbol)
                              (set symbol content)))
          (goto-char (nth 5 cell-bounds)))))))

;; ----------------------------------------------------------------------
;; Motion

(defun spread-next-cell (&optional n bound noerror)
  (interactive "p")
  (setq n (or n 1))
  (cond ((< n 0) (spread-previous-cell (- n) bound noerror))
        ((> n 0) (condition-case nil
                     (let ((current-cell (spread-cell-under-point)))
                       (if current-cell (goto-char (nth 5 current-cell)))
                       (if (search-forward-regexp spread-cell-regexp bound noerror n)
                           (progn (spread-skip-to-char (match-beginning 0))
                                  (spread-cell-at-point))))
                   (error (end-of-buffer) (error "End of buffer"))))
        (t (spread-cell-at-point))))

(defun spread-previous-cell (&optional n bound noerror)
  (interactive "p")
  (setq n (or n 1))
  (cond ((< n 0) (spread-next-cell (- n) bound noerror))
        ((> n 0) (condition-case nil
                     (let ((current-cell (spread-cell-under-point)))
                       (if current-cell (goto-char (car current-cell)))
                       (if (search-backward-regexp spread-cell-regexp bound noerror n) 
                           (progn (spread-skip-to-char (match-beginning 0))
                                  (spread-cell-at-point))))
                   (error (beginning-of-buffer) (error "Beginning of buffer"))))
        (t (spread-cell-at-point))))


(defun spread-cell-down (&optional n)
  (interactive "p")
  (setq n (or n 1))
  (let ((column (current-column))
        (p (point)))
    (while (/= n 0)
      (condition-case err  
          (progn (while (let ((cell-bounds (spread-next-cell (signum n))))
                          (save-excursion 
                            (move-to-column column)
                            (not (and cell-bounds
                                      (spread-cell-contains cell-bounds (point))))))
                   nil)
                 (move-to-column column))
        (error (goto-char p) (signal (car err) (cdr err))))
      (setq n (- n (signum n))))))
    

(defun spread-cell-up (&optional n)
  (interactive "p") (spread-cell-down (if n (- n) -1)))

;; --------------------------------------------------------------------------
;; Cell parsing

(defvar spread-cell-regexp 
  (let* ((quoted-string-regex "\\(\"\\([^\"\\\\\n]\\|\\\\\"\\|\\\\\\\\\\)*\"\\)")
         (word-regex "[^ \t\n\"]+")
         (spread-value-regex (concat "\\(" quoted-string-regex "\\|" word-regex "\\)")))
    (concat "\\(^\\|[ \t]\\)" 
            spread-value-regex
            "[ \t]*\\(<=\\)[ \t]*"
            "\\(\\w+\\|(\\)")))

(defun spread-cell-at-point ()
  "Returns nil if there the character at the point do not begin a cell. Otherwise returns
the cell-bounds list : (cell-beginning cell-content-end cell-blank-end cell-function-end 
cell-variable-beginning cell-end). Cell-variable is nil is the cell
does not have a variable assignement.
"
  (let (cell-beginning content-end blank-end function-end variable-beginning cell-end)
    (save-excursion
      
      (or (bolp) (backward-char))
      (if (not (looking-at spread-cell-regexp)) nil
        (setq cell-beginning (match-beginning 2))
        (setq content-end (match-end 2))
        (setq blank-end (+ (match-beginning 5) 2))
        (setq function-end (if (string= (match-string 6) "(") 
                               (scan-sexps (match-beginning 6) 1)
                             (match-end 6)))
        (setq variable-beginning
              (progn (goto-char function-end)
                     (and (looking-at "[ \\t]*=>[ \\t]*\\(\\w+\\)")
                          (match-beginning 1))))
        (setq cell-end (if variable-beginning (match-end 0) function-end))
        (list cell-beginning content-end blank-end function-end variable-beginning cell-end)))))

(defun spread-cell-contains (cell-bounds pos)
  (and (>= pos (nth 0 cell-bounds))
       (< pos (nth 5 cell-bounds))))

(defun spread-cell-under-point ()
  (save-excursion
    (let ((p (point)))
      (while (and (not (looking-at spread-cell-regexp))
                  (not (bolp)))
        (backward-char))
      (if (looking-at spread-cell-regexp)
          (progn (spread-skip-to-char (match-beginning 0))
                 (let ((cell-bounds (spread-cell-at-point)))
                   (if (spread-cell-contains cell-bounds p) cell-bounds)))))))


(defun spread-font-lock-matcher (bound)
  (condition-case nil
      (let ((cell-bounds (spread-next-cell 1 bound t)))
        (if (not cell-bounds) nil
          (let ((beg-marker (car cell-bounds))
                (end-marker (nth 5 cell-bounds)))
            (store-match-data (list beg-marker end-marker))
            t)))
    (error nil)))




(defun spread-cell-content (cell-bounds)
  "Convert a cell-bounds list, as returned by
`spread-search-cell-raw', into a list (content first-blank function
second-blank variable). In the absence of a variable, both
second-blank and variable are nil. Quotes at not processed at this
level. For instance, the text of the cell can be reproduced with
(apply concat (spread-cell-content cell-bounds))."
  (if (not cell-bounds) nil
    (progv '(cell-beginning content-end blank-end function-end variable-beginning cell-end)
        cell-bounds
      (list (buffer-substring cell-beginning content-end)
            (buffer-substring content-end blank-end)
            (buffer-substring blank-end function-end)
            (and variable-beginning (buffer-substring function-end variable-beginning))
            (and variable-beginning (buffer-substring variable-beginning cell-end))))))



;; --------------------------------------------------------------------------
;; Recalculation


(defun spread-recalc ()
  "Recalculate all computed cells in buffer, iterating until all cells'
values have stabilized or for SPREAD-RECALC-LIMIT iterations, whichever
comes first."
  (interactive)
  (message "Recalculating... ")
  (let ((limit 0))
    (while (spread-recalc-once)
      (message "Recalculating... (%s)" limit)
      (setq limit (+ limit 1))
      (if (= limit spread-recalc-limit)
          (error "recalculation looping!")))
    (message "Recalculating... done")
    (if (and (> limit 0) (assoc 'font-lock-mode minor-mode-alist)) (font-lock-fontify-buffer))))

(defun spread-recalc-once ()
  (interactive)
  (let ((start-point (point)) any-changes cell new-value new-content new-content-and-blank
        new-cell-string content-growt discardable-blank-length)

    (goto-char (point-min))
    
    (while (setq cell-bounds (and (spread-next-cell 1 nil t) (spread-cell-at-point)))
      (progv '(cell-beginning content-end blank-end function-end variable-beginning cell-end)
          cell-bounds
        (progv '(content first-blank function second-blank variable)
            (spread-cell-content cell-bounds)
          
          (spread-debug "%s\t%s" function content)

          (goto-char cell-beginning)
          (condition-case err (setq new-value (eval (car (read-from-string function))))
            (void-variable 
             (error "Spreadsheet error: variable `%s' is unbound" (car (cdr err))))
            (error
             (error "Spreadsheet error: %s" (car (cdr err)))))

          (goto-char cell-beginning)
          
          ; convert string to numeric if possible
          (setq new-value (or (and (stringp new-value) (spread-string-to-int new-value))
                              new-value))

          (setq new-content (concat "" (spread-format-like content new-value)))

          (setq discardable-blank-length 
                (progn (string-match "^ *" first-blank)
                       (match-end 0)))
          
          (setq content-growt (- (length new-content) (length content)))

          (let (content-does-not-fit)
            (setq new-content-and-blank
                  (cond ((<= content-growt 0)
                         ;; Contents srinks
                         (concat new-content 
                                 (make-string (- content-growt) 32)
                                 first-blank ))
                        
                        ((<= content-growt discardable-blank-length)
                         ;; Content did not grow too much
                         (concat new-content (substring first-blank content-growt)))
                        
                        ((string-match "\t" first-blank)
                         ;; Blank contains a tab which takes care of the alignement
                         (concat new-content
                                 (substring first-blank discardable-blank-length)))
                        
                        ;; Uh oh, new cell value does not fit
                        (t (progn (setq content-does-not-fit t)
                                  (concat "#" (make-string (1- (length content)) 32)
                                          first-blank)))))
            (let ((var-differ (and variable
                                   (condition-case nil
                                       (not (equal (eval (intern variable)) new-value))
                                     (void-variable t))))
                  (content-differ (not (string= (buffer-substring cell-beginning blank-end)
                                                new-content-and-blank))))
              
              (if (or var-differ content-differ)
                  (progn
                    (if variable (set (intern variable) new-value))
                    (setq any-changes t)
                    
                    (spread-debug "%s\t%s -> %s" function content new-content)
                    
                    (setq new-cell-string (concat new-content-and-blank function second-blank variable))
                    (delete-region cell-beginning cell-end)
                    (insert new-cell-string))))
            
            (if content-does-not-fit
                (put-text-property cell-beginning (1+ cell-beginning) 'spread-mode 'content-does-not-fit))))

        

        (goto-char cell-end)))
    
    (goto-char start-point)
    any-changes))

;; ----------------------------------------------------------------------
;; Formatting and editing


(defun spread-electric-equal (&optional direction)
  "Switch between the fields of the current cell. If there isn't a cell where the point
   is, it creates one. The cell thus created is aligned with the previous cell, or with
   the cell up if any"
  (interactive)
  (if (not direction) (setq direction 1))
  (let ((cell-bounds (or (spread-cell-under-point)
                         (save-excursion (backward-char) (spread-cell-under-point)))))

    (if cell-bounds
        (progv '(cell-beginning cell-content-end cell-blank-end 
                                cell-function-end cell-variable-beginning cell-end)
            cell-bounds
          ;; Motion
          (let ((pos (cond ((< (point) cell-blank-end) 0)
                           ((< (point) cell-function-end) 1)
                           (t 2))))
            (setq pos (mod (+ pos direction) 3))
            (cond ((= pos 0) (goto-char cell-beginning))
                  ((= pos 1) (progn (spread-skip-to-char cell-blank-end)))
                  ((= pos 2) (spread-skip-to-char 
                              (or cell-variable-beginning cell-function-end))))))
      (progn 
        ;; Insertion
        (let ((starting-col (current-column)))
          (skip-syntax-forward "w")
          (let* ((previous-cell-bounds (save-excursion (spread-previous-cell 1 nil t)))
                 (up-cell-bounds (save-excursion
                                   (condition-case nil 
                                       (progn (spread-cell-up)
                                              (spread-cell-under-point))
                                     (error nil))))
                 (reference-cell-bounds (or up-cell-bounds previous-cell-bounds))
                 
                 content-col symbol-col)
            
            (if (not reference-cell-bounds) (insert " <= ")
              (progv '(cell-beginning content-end blank-end function-end variable-beginning cell-end)
                  reference-cell-bounds
                
                                        ; pick up the reference column numbers
                (save-excursion
                  (goto-char (- blank-end 2))
                  (setq symbol-col (current-column))
                  (goto-char cell-beginning)
                  (setq content-col (current-column)))
                
                                        ; goto to desired content column
                (move-to-column content-col)
                (if (= (char-before cell-beginning) ?\t)
                    (while (> content-col (current-column))
                      (insert "\t"))
                  (move-to-column content-col t))
                
                                        ; add some dummy content if there isn't any already
                (if (and (or (bobp) (= (char-syntax (char-before)) 32))
                         (or (eobp) (= (char-syntax (char-after)) 32)))
                    (insert "#"))
                
                                        ; goto to desired function column
                (move-to-column symbol-col)
                (if (= (char-before (- blank-end 2)) ?\t)
                    (while (> symbol-col (current-column))
                      (insert "\t"))
                  (move-to-column symbol-col t))

                (cond ((looking-at "<=") 
                                        ; this is a partial cell, cycle its partial fields
                       (if (> (current-column) starting-col) (move-to-column (+ symbol-col 3))
                         (move-to-column content-col) (skip-syntax-backward "w")))
                      
                      ((or (or (bobp) (= (char-syntax (char-before)) 32))
                           (or (eobp) (= (char-syntax (char-after)) 32)))
                                        ; free space for insertion
                       (insert "<=")
                       (if (= (char-syntax (char-after blank-end)) 32)
                           (insert (char-after blank-end))))
                      
                      (t (error "There is already some text at alignment point")))))))))))
  
  

;; ----------------------------------------------------------------------
;; Some useful calculation functions

(defalias 'subtotal 'total)
(defalias 'ref 'spread-ref)
(defalias 'absref 'spread-abs-ref)

(defun self () 
  "Returns the current content of the cell itself."
  (spread-ref))

(defun spread-ref (&rest args)
  "Move to the specified cell and returns its content. ARGS is of the form:
   NUMBER-NEXT-CELL-MOVES NUMBER-DOWN-CELL-MOVES ...
   The numbers can be negative."
  (save-excursion
    (let (vertical)
      (while args
        (if vertical (spread-cell-down (car args))
          (spread-next-cell (car args)))
        (setq vertical (not vertical))
        (setq args (cdr args))))
    
    (let ((cell-bounds (spread-cell-at-point)))
      (progv '(cell-beginning content-end blank-end function-end variable-beginning cell-end)
          cell-bounds
        (progv '(content first-blank function second-blank variable)
            (spread-cell-content cell-bounds)
          
          (if (eq (get-text-property cell-beginning 'spread-mode) 'content-does-not-fit)
              (error "refering to cell \"%s\" which did not fit" function))
          (spread-interpret-content (car (spread-cell-content cell-bounds))))))))


(defun spread-abs-ref (&rest args) 
  "Same as spread-ref but counts from the beginning of the buffer"
  (save-excursion
    (goto-char (point-min))
    (apply 'spread-ref args)))

(defun spread-abs-cross-ref (filename &rest args)
  "Same as spread-abs-ref with the possibility of refering to another file"
  (save-excursion
    (if (not (file-exists-p filename)) (error "cannot find file `%s' for cross reference" filename))
    (set-buffer (find-file-noselect filename t))
    (apply 'spread-abs-ref args)))

(defun spread-data-below (&optional direction max-blank-lines)
  (interactive)
  (save-excursion
    (let ((column (current-column)) (data nil) (spaces 0))
      (while (cond ((> spaces max-blank-lines) nil)
                   ((/= (forward-line direction) 0) nil)
                   ((or (/= (move-to-column column) column)
                        (looking-at "[ \t \n]"))
                    (progn (setq spaces (1+ spaces))
                           (/= spaces max-blank-lines)))
                   (t (setq data (cons (thing-at-point 'word) data)) t)))
      data)))

(defun total ()
  "Return the total of the list of numbers above this position,
not including the number under the cursor."
  (interactive)
    (let ((sum 0) (data (spread-data-below -1 0)))
      (while data
        (setq sum (+ sum (string-to-int (car data))))
        (setq data (cdr data)))
      sum))

(defun grand-total (n)
  "Return the total of cells above this position which
are computed with subtotal. Allows N blanks between groups"
  (interactive "p")
  (save-excursion
    (let ((column (current-column)) (sum 0) (spaces 0))
      (while (cond ((= spaces n) nil)
                   ((/= (forward-line -1) 0) nil)
                   ((or (/= (move-to-column column) column)
                        (looking-at "[ \t \n]"))
                    (progn (setq spaces (1+ spaces))
                           (/= spaces n)))
                   (t (let* ((cell-bounds (spread-cell-under-point))
                             (cell-content (spread-cell-content cell-bounds)))
                        (setq spaces 0)
                        (if (and cell-bounds 
                                 (string-match "subtotal" (nth 2 cell-content)))
                            (progn (setq sum (+ sum (string-to-int (car cell-content))))))
                        t))))
      sum)))


(defun spread-interpret-date (date)
  (let* ((tokens (split-string date))
         (day (string-to-int (nth 2 tokens)))
         (month-name (downcase (nth 1 tokens)))
         (month-names '("january" "febuary" "mars" "april" "may" "june" "july" "august" "september" "october" "november" "december"))
         ;; ;; While-break loop
         month)

    (let ((i 1) (current month-names))
      (while (cond ((not current) (error "Bad month name `%s'" month-name))
                   ((string= month-name (car current)) (setq month i) nil)
                   (t (setq current (cdr current)) (setq i (1+ i)) t))))
         
         ;; ;; C++'s standard issue loop
         ;;int i=1;
         ;;for(Link *current = month_names; current; current = current->next, i++)
         ;;    if (strcmp(month_name, current->data) == 0) return i;
         ;;throw "Bad month name"

         ;; ;; Java's loops
         ;;int i = 1;
         ;;for(Iterator monthIt = monthNames.iterator(); monthIt.hasNext(); )
         ;;{
         ;;    String current = (String)monthIt.next();
         ;;    if (monthName.equals(current)) return i;
         ;;    i++;
         ;;}
         ;;throw new RuntimeException("Bad month name");

         ;; ;; Emacs' imperative while loop returning nil :
         ;;(month (or (let ((i 0) (current month-names) rtn done)
         ;;             (while (not done)
         ;;               (cond ((not current) (setq done t))
         ;;                     ((string= month-name (car current)) (setq rtn i) (setq done t))
         ;;                     (t (setq i (1+ i)) (setq current (cdr current)))))
         ;;             rtn)
         ;;           (error "Bad month name")))

         ;; ;; Emacs' imperative while loop which can't return nil easily :
         ;;(month (let ((i 0) (current month-names) rtn)
         ;;         (while (not (cond ((not current) (error "Bad month name"))
         ;;                           ((string= month-name (car current)) (setq rtn i))
         ;;                           (t (setq i (1+ i)) (setq current (cdr current)) nil))))
         ;;         rtn))

         ;; ;; Recursive :
         ;;(month (progn (defun loop-fn (i current)
         ;;                (cond ((not current) (error "Bad month names"))
         ;;                      ((string= month-name (car current)) 1)
         ;;                      (t (loop-fn (1+ i) (cdr current)))))
         ;;              (loop-fn 1 month-names)))

         ;; ;; Anonymous spinner :
         ;;(month (spinner ((i 1) (current month-names))
         ;;         (cond ((not current) (error "Bad month names"))
         ;;               ((string= month-name (car current)) 1)
         ;;               (t (spin (1+ i) (cdr current))))))
         
    (list month day 2002)))



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

(defun spread-string-to-int (value)
  (let ((num (string-to-int value)))
    (cond ((/= num 0) num)
          ((progn (and (= (string-match "\\s-*\\(\\(0+\\.?0*\\)\\|\\(0*\\.0+\\)\\)\\s-*" value) 0)
                       (= (match-end 0) (length value)))) 0)
          (t nil))))

(defun spread-interpret-content (value)
  (let ((unquoted (if (and (stringp value)
                           (string-match "^\".*\"$" value))
                      (read value) value)))
    (or (spread-string-to-int unquoted)
        unquoted)))

(defun spread-skip-to-char (n)
  (goto-char n)
  (if (looking-at "[ \t]+[^ \t\n]") (skip-syntax-forward "-")))


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

        (if (string-match " \\|\"" new) (setq new (pp-to-string new)))
        new
        ))))

(defun spread-float-to-string-with-precision (n p)
  (let ((float-output-format (concat "%." (int-to-string p) "f")))
    (format "%s" (float n))))


;; ----------------------------------------------------------------------
;; Font support for FSF19

(defun spread-fontify-cell (val-start val-end cell-end)
  (add-text-properties val-start val-end '(face bold))
  (add-text-properties (+ 1 val-end) cell-end '(face italic))
)

;; ----------------------------------------------------------------------
;; Debugging and error reporting

(defun spread-debug (&rest args)
  (if spread-debugging
      (progn
        (let ((line-number-tag (format "Line %d" (count-lines 1 (point)))))
          (save-window-excursion
            (switch-to-buffer "*Spread*")
            (goto-char (point-max))
            (insert line-number-tag " ")
            (insert (apply 'format args))
            (insert "\n")
            )))))

(provide 'spread)

