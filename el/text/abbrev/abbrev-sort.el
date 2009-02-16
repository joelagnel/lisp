;;; abbrev-sort.el --- Sort abbrevs when editing or saving.

;; Copyright (C) 1999 Will Mengarini

;; Author: Will Mengarini <seldon@eskimo.com>
;; URL: <http://www.eskimo.com/~seldon>
;; Created: We 04 Aug 99
;; Version: 0.50
;; Keywords: abbrev convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; If you have a large number of abbrevs (I have 471) you may want to put
;; them under version control.  If you do that with standard Emacs, you'll
;; find that you can't meaningfully compare the differences between
;; successive versions (what has been inserted or removed, how have use
;; counts changed) because the versions aren't sorted; the sequence of the
;; abbrevs, and even of the abbrev tables, is random, so between successive
;; versions it appears that almost everything has changed.  This is also an
;; inconvenience when editing abbrevs (M-x edit-abbrevs).

;; This package implements a minor mode, abbrev-sort-mode, that, when
;; enabled, causes abbrevs to be written sorted.  Within each abbrev table,
;; they're sorted on expansion; the tables themselves are sorted according to
;; their names, except that global-abbrev-table comes first.

;; The format in which the table is written to disk is otherwise identical to
;; standard Emacs and completely compatible with it in both directions, so an
;; Emacs using this mode can read an unsorted abbrev table, and a Emacs not
;; using this mode can read a table that was written while this mode was
;; enabled (although of course when that Emacs writes the table back out it
;; will become unsorted again).  The internal data structures in which
;; abbrevs are stored, and the manner of their use, are not affected.

;; You can use this package either interactively or from your .emacs file.
;; In either case, first you'll need to copy this file to a directory that
;; appears in your load-path.  `load-path' is the name of a variable that
;; contains a list of directories Emacs searches for files to load.
;; To prepend another directory to load-path, put a form like
;; (add-to-list 'load-path "c:/My_Directory") in your .emacs file.
;; Then, use M-x byte-compile-file to compile it (important for efficiency).

;; Because this is a minor mode, you can toggle it on and off.  That's
;; useful for checking whether this mode is too inefficient for your
;; system.  Toggle by invoking M-x abbrev-sort-mode, then check
;; efficiency by invoking M-x edit-abbrevs or M-x write-abbrev-file.
;; The mode line will indicate whether abbrev-sort-mode is currently
;; enabled.  (I've tried this mode on two systems, both using GNU Emacs
;; 19.34.6.  On a 96M PII/233 + NT 4.0 it feels instantaneous even when
;; the code isn't byte-compiled, but on a 16M 486/50 + Windows 95 it's
;; noticeable, taking 2..4 seconds to prepare an edit-abbrevs buffer.)

;; If you do decide to use this, you will normally want it to always be
;; enabled automatically.  To make that happen, put the forms
;;   (require 'abbrev-sort)
;;   (abbrev-sort-mode 1)
;; in your .emacs file.  Once you've done so, it can become an annoyance that
;; there's a mode-line display for the minor mode, because it's wasting space
;; telling you about something you don't need to be reminded of any more.
;; Several minor modes, such as mouse-avoidance-mode, have this problem
;; because of the intended manner of their use and the paucity of space on
;; the mode line.  Because this is a general problem, I created a general
;; solution, "diminished modes", <http://www.eskimo.com/~seldon/diminish.el>.
;; A diminished mode is a minor mode that has had its mode line display
;; diminished, usually to nothing, although diminishing to a shorter word or
;; a single letter is also supported.

;;; Code:

(eval-when-compile
  (require 'cl))

;;;###autoload
(defvar abbrev-sort-mode nil
  "Non-nil turns on sorting of abbrevs for editing or saving.")

(or (assq 'abbrev-sort-mode minor-mode-alist)
    (push '(abbrev-sort-mode " AbbrSort") minor-mode-alist))

;;;###autoload
(defun abbrev-sort-mode (arg)
  "Toggle abbrev-sort mode, which sorts abbrevs for editing or saving.
With argument ARG, turn abbrev-sort mode on iff ARG is positive.

This is a minor mode only for testing convenience; you can toggle it off
to see whether that significantly speeds up the functions it affects.
\(That is unlikely on a modern computer, assuming this code is
byte-compiled.)  Normally, you'll turn this on by putting
\(abbrev-sort-mode 1) in your .emacs file, then forget about it.

The two functions you should see affected are `write-abbrev-file'
\(\\[write-abbrev-file]) and `edit-abbrevs' (\\[edit-abbrevs]).
These will write or display the abbrev tables in sorted form.  The
tables themselves are sorted by the name of the mode to which they
apply, with global-abbrev-table coming first; then, within each
table, the abbrevs are sorted case-insensitively on the expansion.

Use diminish-mode in diminish.el (see <http://www.eskimo.com/~seldon>)
to eliminate the mode-line \"AbbrSort\" display."
  (interactive "P")
  (when (setq abbrev-sort-mode
              (if arg
                  (> (prefix-numeric-value arg) 0)
                (not abbrev-sort-mode)))
    (run-hooks 'abbrev-sort-mode-hook))
  (force-mode-line-update)
  (if abbrev-sort-mode
      (abbrev-sort-do-advice)
    (abbrev-sort-undo-advice)))

(defun abbrev-sort-do-advice ()
  "Internal function used by abbrev-sort-mode to turn mode on."
  (require 'advice)
  (defadvice prepare-abbrev-list-buffer (around abbrev-sort-prepare act)
    "Modified by abbrev-sort-mode to sort output."
    (setq ad-return-value (abbrev-sort-prepare-abbrev-list-buffer)))
  (defadvice write-abbrev-file (around abbrev-sort-write act)
    "Modified by abbrev-sort-mode to sort output."
    (abbrev-sort-write-abbrev-file file)))

(defun abbrev-sort-undo-advice ()
  "Internal function used by abbrev-sort-mode to turn mode off."
  ;; Just deactivating advice isn't actually equivalent to removing it,
  ;; since there are circumstances where all or a whole group of advices
  ;; will be activated or deactivated simultaneously; for example,
  ;;   (defun wm-emacs-lisp-byte-compile-and-load-safely ()
  ;;     "Do emacs-lisp-byte-compile-and-load while advices are deactivated."
  ;;     ;; From the documentation in advice.el:
  ;;     ;;   IMPORTANT: With Advice loaded always do `M-x ad-deactivate-all'
  ;;     ;;   before you byte-compile a file, because advised special forms
  ;;     ;;   and macros can lead to unwanted compilation results. When you
  ;;     ;;   are done compiling use `M-x ad-activate-all' to go back to the
  ;;     ;;   advised state of all your advised functions.
  ;;     (interactive)
  ;;     (require 'advice)
  ;;     (ad-deactivate-all)
  ;;     (unwind-protect
  ;;         (emacs-lisp-byte-compile-and-load)
  ;;       (ad-activate-all)))
  ;; The approach in this function, actually removing the advice instead
  ;; of simply deactivating it, seems inefficient, but in interpreted code
  ;; (which I prefer during debugging), this function executes
  ;; instantaneously on a 486/50 that's so slow 19.34.6 often can't keep
  ;; up with echoing keystrokes; efficiency is irrelevant here.
  (loop
   for (function                      advice)
   in '((prepare-abbrev-list-buffer   abbrev-sort-prepare)
        (write-abbrev-file            abbrev-sort-write))
   do
   (ad-remove-advice function 'around advice)
   (ad-update function)))

(defun abbrev-sort-write-abbrev-file (file)
  "Like builtin `write-abbrev-file' but sorting everything.
This is useful when the abbrev table is under version control.  For
editing convenience I sort global-abbrev-table first; the other tables
are alphabetized by name.  The argument FILE is the file name to write."
  (interactive
   (list
    (read-file-name "Write abbrev file: "
                    (file-name-directory (expand-file-name abbrev-file-name))
                    abbrev-file-name)))
  (or (and file (> (length file) 0))
      (setq file abbrev-file-name))
  (save-excursion
    (set-buffer (get-buffer-create " write-abbrev-file"))
    (erase-buffer)
    (loop
     for table in (cons 'global-abbrev-table
                         (delete 'global-abbrev-table
                                  (sort (append abbrev-table-name-list ())
                                        'string<)))
     do
     (abbrev-sort-insert-abbrev-table-description table))
    (write-region 1 (point-max) file)
    (erase-buffer)))

(defun abbrev-sort-prepare-abbrev-list-buffer ()
  "Like builtin `prepare-abbrev-list-buffer' but sorting everything.
This is useful when the abbrev table is under version control.
For editing convenience I sort global-abbrev-table first; the other
tables are alphabetized by name."
  (save-excursion
    (set-buffer (get-buffer-create "*Abbrevs*"))
    (erase-buffer)
    (loop
     for table in (cons 'global-abbrev-table
                         (delete 'global-abbrev-table
                                  (sort (append abbrev-table-name-list ())
                                        'string<)))
     as separator = "" then "\n"
     do
     (insert separator)
     (abbrev-sort-insert-abbrev-table-description table t))
    (goto-char (point-min))
    (set-buffer-modified-p nil)
    (edit-abbrevs-mode))
  (get-buffer-create "*Abbrevs*"))

(defun abbrev-sort-insert-abbrev-table-description (abbrev-table
                                                    &optional readable)
  "Similar to the builtin but sorting on expansion.
Builtin (insert-abbrev-table-description NAME &optional READABLE) inserts
in whatever order the args were present in the abbrev table, which is
effectively random since the abbrev table is an obarray."
  ;; Efficiency matters in this function.  It's irritatingly slow on a
  ;; 16M 486/50 + Windows 95 + 19.34.6; byte-compiled, it takes 1.5 .. 3
  ;; seconds just to process a 417-abbrev global-abbrev-table.  However,
  ;; on a 96M PII/233 + NT 4.0 + 19.34.6 it feels instantaneous even in
  ;; interpreted code, so I think modern computers won't have a problem.

  ;; In an earlier version, the sort lambda upcased the strings; this caused
  ;; the upcase to be done once for each comparison, rather than once for
  ;; each abbrev.  What hurt most about that was probably not repeating
  ;; `upcase' itself, but repeatedly allocating more space for throwaway
  ;; string copies.  This version conses the symbol onto the upcase before
  ;; sorting.  The effect in interpreted code was to speed up processing of
  ;; that global-abbrev-table by 14%.

  ;; Note how important it could be to have a case-insensitive
  ;; `string-lessp'.  (In Emacs Lisp that is a synonym for `string<'; in
  ;; Common Lisp the spelled-out form (for which all inequalities are
  ;; provided, not just `<') is case-insensitive.)  The singular glory of
  ;; Common Lisp is the richness of its lexicon.  Scheme reminds me of
  ;; Esperanto, which I have forgotten, having discovered, after learning it
  ;; many years ago, that it has no literature, and (and probably because of)
  ;; a dogmatic traditionalist user community preventing language growth.
  ;; For programming languages, the pursuit of elegance (pace Garp) has
  ;; always been a lethal affectation.  I think Richard Gabriel is right
  ;; (<http://www.amazon.com/exec/obidos/ASIN/0195121236/willmengarshomep>)
  ;; in asserting that our greatest successes in creating reusable software
  ;; have been the domain-independent libraries of programming languages, and
  ;; that the significance of this argument in the advocacy of large
  ;; languages is decisive.  See also this delightful essay by Steele,
  ;; <http://www.sunlabs.com/research/java-topics/pubs/98-oopsla-growing.ps>.
  ;; What matters about Java is most of all beans, then the JVM, syntax last.

  ;; I looked at the expansion of the loop macros, and where it matters the
  ;; expansion is almost identical to what I'd code by hand with `while', so
  ;; I think using `loop' instead does no harm.

  ;; Header---
  (insert (format (if readable
                      "(%S)\n"
                    "(define-abbrev-table '%S '(\n")
                  abbrev-table))
  ;; Abbrevs---
  (loop
   for (symbol . upcase) in
   (sort (loop
          for symbol being the symbols of
          (symbol-value abbrev-table)
          collect (cons symbol
                        (upcase (symbol-value symbol))))
         (lambda (x y)
           (string< (cdr x)
                    (cdr y))))
   do
   (if (not readable)
       (insert (format "    (%S %S %S %S)\n"
                       (symbol-name symbol)        ;abbrev
                       (symbol-value symbol)       ;expansion
                       (symbol-function symbol)    ;hook
                       (symbol-plist symbol)))     ;use count
     ;; Canonical columns are 0, 15, 20, 45.
     (insert (format "\n%S" (symbol-name symbol))) ;abbrev
     ;; The use count is canonically formatted as %d, but I seem to be
     ;; able to get away with right-justifying it in its column.
     (indent-to 14) ;because spaces will lead
     (insert (format "%5d" (symbol-plist symbol))) ;use count
     (indent-to 20)
     (insert (format "%S" (symbol-value symbol)))  ;expansion
     (let ((lambda (symbol-function symbol)))      ;hook
       (when lambda
         (indent-to 45)
         (insert (format "%S" lambda))))))
  ;; Footer---
  ;; In the builtin output, every insertion is followed by \n\n, even when
  ;; that's at the end of the buffer.  2 adjacent empty abbrev tables are
  ;; separated by \n\n\n.
  (insert (if readable "\n\n" "    ))\n\n")))

(run-hooks 'abbrev-sort-load-hook)
(provide 'abbrev-sort)

;;; abbrev-sort.el ends here