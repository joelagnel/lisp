;;; counter.el --- Insert series of numbers into text

;; Copyright (C) 1998 Will Mengarini

;; Author: Will Mengarini <seldon@eskimo.com>
;; URL: <http://www.eskimo.com/~seldon>
;; Created: We 25 Mar 98
;; Version: 0.12, We 13 May 98
;; Keywords: abbrev

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

;; This package implements a function, `counter', that can be invoked to
;; insert series of numbers into text.  You'll probably want to bind that
;; function to a key sequence; I'll use C-c c as an example in this
;; documentation.  Detailed instructions on how to do that key binding, and
;; on everything else, are below, but here's a summary of what that binding
;; will allow.  Every time you type C-c c, counter inserts a number into the
;; buffer, then increments the value it's up to, so the next C-c c inserts
;; the next number.  You can type C-u C-c c to initialize the starting
;; number, the increment, and the format of the insertion (leading or
;; trailing spaces, for example, or other decoration).  C-u 5 C-c c will
;; insert five numbers; that's the fastest way to insert a series of numbers
;; if you want them all together in the text, separated by nothing but the
;; format decoration.  But you can also use C-c c in keyboard macros,
;; allowing you to do things like insert columns of numbers.  There is also a
;; facility to allow you to renumber a possibly-noncontiguous column, such as
;; occurs with numbered paragraphs.

;; To use this package, first you'll need to copy this file to a directory
;; that appears in your load-path.  `load-path' is the name of a variable
;; that contains a list of directories Emacs searches for files to load.
;; To prepend another directory to load-path, put a line like
;; (add-to-list 'load-path "c:/My_Directory") in your .emacs file.

;; Then you'll need to bind the `counter' function.  This code in your .emacs
;;   (global-set-key "\C-cc" 'counter)
;; will do that binding.  Also include this code
;;   (autoload 'counter "counter" nil t)
;; to tell Emacs where to find the `counter' function.  During the Emacs
;; session in which you type those lines into your .emacs file, you can cause
;; each line to be executed when point is after its final ")" by typing
;; C-x C-e there.

;; That's it, counter is installed, and you're ready to experiment with it.
;; It's initialized by default to reasonable values, so for example you can
;; just type C-x b *scratch* to switch to the *scratch* buffer, and type
;; C-u 4 C-c c to insert a series of 4 numbers.  Then C-u 3 C-c c will insert
;; 3 more numbers without resetting the counter; it doesn't get reset until
;; you reinitialize it with C-u C-c c.

;; C-u C-c c prompts for 3 values: the number to start counting from, the
;; amount by which to increment each new number, and the format for
;; insertion.  The first 2 entries are actually evaluated as Lisp code, so
;; although most of the time you'll just use integers, you can also use, for
;; example, Emacs global variables.  The third entry is also evaluated;
;; usually it's a constant string, but since it's evaluated you can store
;; multiple useful formats in variables and refer to them by name.  The
;; string needs to be double-quoted when it's entered literally; this is
;; helpful when you're looking at the default value or the minibuffer
;; history, since useful formats usually have leading or trailing whitespace,
;; and the quotes make that clear.

;; The second entry is usually a constant increment, but actually has more
;; flexibility than that; it's a Lisp form that's evaluated each time it's
;; used, so the amount of the increment can keep changing.  Also, the form is
;; only construed as an increment if it evaluates to a number.  Otherwise,
;; it's assumed to be a function of one argument that is to be applied to the
;; current value of the counter to produce the next value, so entering
;; (lambda (x) (* x 2)) would allow you to insert a series of numbers each of
;; which is double the previous.

;; The third value is a format string that would be suitable as a first
;; argument to the Emacs Lisp function `format'.  If you're not familiar with
;; that but know C++ or Perl, you can probably just wing it, since it's
;; pretty much the same as in those languages.  The default of " %d", for
;; example, inserts a decimal integer preceded by a single space.

;; These values are normally global to Emacs, so if you run `counter' in one
;; buffer, then move to another buffer without reinitializing and run it
;; again, it'll continue counting from where it left off.  However, in any
;; buffer, you can run M-x counter-localize to make all the counter state
;; variables local in that buffer.  Another option is to put the line
;;   (setq counter-local-by-default t)
;; in your .emacs file; the effect will be that all buffers have their own
;; copies of the counter state variables.

;; Note that because of the generality of the formatting and "incrementing"
;; mechanisms you aren't actually limited to numbers at all; those are just
;; the most common things to insert series of.  A format string of "%S" will
;; insert a printed representation of an arbitrary Lisp object, and the
;; "increment" can perform any arbitrary calculation of what's next.

;; That's all you need to know for the simplest use of `counter', which is
;; just inserting series of numbers or other objects.  There are additional
;; features available for solving more complicated problems.

;; If you have a bunch of numbered items, like
;;      1.  foo       2.  bar       3.  baz
;;      4.  woo       5.  hoo       6.  spoo
;; and want to insert another line, "warga blegga pizza", in the middle,
;; so it gets the 4..6 numbering and "woo hoo spoo" becomes 7..9, then it's
;; not enough to be able to *insert* numbers; you need to be able to
;; *replace* them.  Invoking `counter' as C-u - C-c c causes it to search the
;; region (the range of the buffer between point and mark) for a match to the
;; `counter-target' variable, which is a regular expression that defaults to
;; "[0-9]+" (a nonempty digit string).  If it finds a match, it replaces it
;; with its insertion; otherwise, it doesn't do the insertion at all, or the
;; increment.  This is useful in keyboard macros; in the above example, you
;; can code a macro like
;;   M-@         ;; mark-word
;;   C-x C-x     ;; exchange-point-and-mark
;;   C-u         ;; universal-argument
;;   -           ;; [make that a negative argument]
;;   C-c c       ;; counter
;; to mark each word and run C-u - C-c c on it; if the word contains a
;; number, it'll be replaced by whatever number `counter' is up to, but if
;; not, the macro will have no effect except to move to the next word.  Then
;; you can insert the line
;;      0.  warga     0.  blegga    0.  pizza
;; in the middle, reinitialize counter with a format of "%d" (no surrounding
;; whitespace), and run the keyboard macro 18 times to renumber everything.
;; (Of course you want to be able to do that by just leaning on a single key,
;; not trilling on C-x e, so presumably you've bound `call-last-kbd-macro' to
;; a function key, or are using my vi-dot.el package (see my URL above).)

;; The `counter-target' variable isn't modified by C-u C-c c because most
;; users will never read this far in the documentation and won't know what it
;; is.  You can modify it with M-x set-variable.

;; If your formatting needs are too complex for an Emacs regexp, you may need
;; to code your own Lisp function to decide whether `counter' should replace
;; an object (remember it doesn't need to be a number) in the region with the
;; next object in the series `counter' is inserting.  Every time `counter' is
;; about to insert something, it runs counter-pre-insert-hook, a normal
;; hook.  If any hook function sets the variable `counter-skip' to non-nil,
;; then `counter' skips the insertion, just resetting `counter-skip' to nil.
;; (This is the internal mechanism by which the C-u - argument is handled.)
;; For symmetry, there is also a counter-post-insert-hook run after an
;; insertion.  Whenever an insertion is skipped, the incrementation is also
;; skipped, and so is the running of counter-post-insert-hook.

;; The hooks aren't localized by `counter-localize', since when you're
;; dealing with local hooks you need to use optional arguments to `add-hook',
;; of which check the documentation, along with that of `make-local-hook', if
;; you really need it, which you probably don't even if you're localizing the
;; other counter state variables.

;;; Epigraph:

;;      "[...] my sentiments at heart were insular and mediaeval.
;;         This was my conversion to the baroque.  Here under that
;;       high and insolent dome, under those tricky ceilings;
;;       here, as I passed through those arches and broken pediments
;;       to the pillared shade beyond and sat, hour by hour,
;;       before the fountain, probing its shadows, tracing
;;       its lingering echoes, rejoicing in all its clustered feats
;;       of daring and invention, I felt a whole new system of nerves
;;       alive within me, as though the water that spurted and bubbled
;;       among its stones was indeed a life-giving spring."
;;            --Evelyn Waugh, /Brideshead Revisited/

;;; Code:

(eval-when-compile (require 'cl))

;;;###autoload
(defvar counter-value 1
  "The next value the `counter' function will insert.
Most invocations of `counter' increment this value after inserting it.")

;;;###autoload
(defvar counter-step 1
  "Expression to evaluate to calculate the next value of `counter-value'.
Usually this evaluates to a number, in which case it's construed as an
increment.  If it doesn't evaluate to a number, it's construed as a function
of one argument to be applied to `counter-value' to return its next value.")

;;;###autoload
(defvar counter-format " %d"
  "Format in which the `counter' function will insert `counter-value'.
If you usually use a format different from the default, you could set
your own default with a form like (setq counter-format \" %g\") in
your .emacs file.  Alternatively, you could put something like
\(set (make-local-variable 'counter-format) \"$04x\") in the mode hook
of major modes to which you want to give a mode-specific default,
leaving the global default unchanged.")

;;;###autoload
(defvar counter-insert-function nil
  "If non-nil, the function to use to insert a string into the buffer.
By default this is nil, in which case the builtin `insert' is used.
Setting this can be useful if you want to use a function that respects
overwrite-mode or auto-fill-mode.")

;;;###autoload
(defvar counter-target "[0-9]+"
  "*The pattern the `counter' function recognizes as a target to be replaced.
When `counter' is invoked with a negative argument, it searches the region
for a match to this pattern.  If it finds a match, it replaces the match
with whatever it would normally have inserted; otherwise, it does nothing,
inserting nothing and not incrementing the `counter-value' variable.")

;;;###autoload
(defvar counter-pre-insert-hook nil
  "Normal hook run by the `counter' function before each insertion.
If `counter' has an argument telling it to do multiple insertions, this
hook is run before each of them.  If any of the hook functions sets the
variable `counter-skip' to t, then the only remaining action `counter' takes
for this \"insertion\" is to reset `counter-skip' to nil; `counter-value'
is not inserted or incremented.  However, the count of multiple insertions
is independent of `counter-skip', so (counter 5) will always run this hook 5
times regardless of what the hook functions do to `counter-skip', and they
must (setq counter-skip t) for each separate insertion they wish to disable.")

;;;###autoload
(defvar counter-post-insert-hook nil
  "Normal hook run by the `counter' function after each insertion.
It is run only if an insertion actually occurred; if one would have occurred
but was prevented by a non-nil `counter-skip', this hook isn't run.")

;;;###autoload
(defvar counter-local-by-default nil
  "Non-nil means each buffer will have its own `counter' state.")

;;; No user options below this point

(defvar counter-skip nil
  "Non-nil means `counter' skips the next insertion.
It also resets `counter-skip' to nil.")

;;;###autoload
(defun counter-localize (&optional function)
  "Localize the state variables used by the `counter' function.
Normally, as when invocation is with \\[counter-localize], localization
is done with the `make-local-variable' function, so it affects only the
state variables in the current buffer; but if the optional FUNCTION argument
is set to 'make-variable-buffer-local, it affects all buffers.
The hooks are not localized; use `make-local-hook' if you need that."
  ;; GNU Emacs 19.34 has nothing like a `make-hook-buffer-local' function, &
  ;; it probably shouldn't, since adding to a localized hook requires extra
  ;; thought about whether the particular function being added should go on
  ;; the local or global hook variable, & the default invocation of
  ;; `add-hook' adds to the global hook even if a local one exists.  Given
  ;; all that, it seems best to require the user to think deliberately about
  ;; local hooks if they're wanted, which they probably won't be even if
  ;; localized state variables are wanted.  The alternative would have
  ;; disallowed the flexibility of the FUNCTION argument, complicating the
  ;; processing of the `counter-local-by-default' variable.
  (interactive)
  (mapc (or function 'make-local-variable) [counter-value
                                            counter-step
                                            counter-format
                                            counter-target
                                            counter-skip]))

(when counter-local-by-default
  (counter-localize 'make-variable-buffer-local))

;;;###autoload
(defun counter (P)
  "Insert series of numbers or other objects into the buffer.
Invoke as \\[universal-argument] \\[counter] to initialize the series.
The initial value can be a number, an Emacs global variable, or any Lisp
expression (it will be evaluated to determine the initial value); the
increment or function can be a number that will be an increment, or a
function of one argument that calculates the next value from the current one,
or any Lisp expression (such as a variable) that evaluates to either of these
things; the format must evaluate to a control string in which
%d means print a number in decimal (%o octal, %x hex, %04x pad with zeroes),
%e means print a number in exponential notation (%15e pad with spaces),
%f means print a number in decimal-point notation (%.2f 2 decimal places),
%g means %e or %f (whichever uses fewer characters),
%c means print a number as a single character,
%s means print a string argument (actually any object, using `princ'),
%S means print any object as a re(read)able s-expression (using `prin1'),
%% means print a single %,
so enter e.g. \"\\t%f\" to separate insertions by tabs.  The quotes are
required when typing in a literal string, because it's a Lisp expression.
Invoke as e.g. \\[universal-argument] 5 \\[counter] to insert a series of 5
numbers.  The next invocation will continue the series from where it left
off, so if the control string format doesn't have enough power to position
the elements of the series where you want them, you can just invoke as
\\[counter] from inside a keyboard macro to insert the next item from the
series.  Within a keyboard macro, it can be useful to mark a region that
might contain a number you want to replace (such as a range of columns on a
line), then invoke \\[universal-argument] - \\[counter], which will search
that region for the first string matching the regexp in `counter-target', and
if found will replace the match with its insertion, but if not found will do
nothing, so you can invoke the keyboard macro on each of a range of lines
only some of which contain numbers you want to replace."
  (interactive "P")
  (if (consp P)
      (call-interactively 'counter-reinitialize)
    (when (minusp (callf prefix-numeric-value P))
      (counter-delete-number-from-region-or-skip (region-beginning)
                                                 (region-end))
      (callf abs P))
    (counter-insert P)))

;;;###autoload
(defun counter-delete-number-from-region-or-skip (start end)
  "Delete region's first match of `counter-target', or (setq counter-skip t)."
  (interactive "r")
  (let ((entry (point)))
    ;; Can't use 'save-excursion because return to entry point only occurs
    ;; if no match was found.
    (goto-char start)
    (if (search-forward-regexp counter-target end t)
        (replace-match "" t t)
      (setq counter-skip t)
      (goto-char entry))))

;;;###autoload
(defun counter-insert (how-many-insertions)
  "Insert `counter-value' in `counter-format', then apply `counter-step'.
Do that HOW-MANY-INSERTIONS times, making proper use of any non-nil values
of `counter-{p{re,ost}-insert-hook,insert-function,skip}'.
This function is normally invoked by the `counter' function, not by users."
  (interactive "p")
  (loop repeat how-many-insertions do
        (run-hooks 'counter-pre-insert-hook)
        (if counter-skip
            (setq counter-skip nil)
          (funcall (or counter-insert-function 'insert)
                   (format counter-format counter-value))
          (run-hooks 'counter-post-insert-hook)
          (let ((step (eval counter-step)))
            (if (numberp step)
                (incf counter-value step)
              (callf2 funcall step counter-value))))))

(defvar counter-value-history (list (prin1-to-string counter-value))
  "Minibuffer history for user reinitializations of `counter-value'.")

(defvar counter-step-history (list (prin1-to-string counter-step))
  "Minibuffer history for user reinitializations of `counter-step'.")

(defvar counter-format-history (list (prin1-to-string counter-format))
  "Minibuffer history for user reinitializations of `counter-format'.")

;; I considered making those history variables buffer-local whenever the
;; state variables are, but it seems just as likely that values from other
;; buffers will be useful as that they'll get in the way.

;;;###autoload
(defun counter-reinitialize (value step format)
  "Reinitialize `counter-{value,step,format}'.
This function is normally invoked by the `counter' function, not by users;
see \\[describe-function] counter for help on interactive reinitialization.
If desired, this function can be invoked noninteractively from Lisp."
  (interactive (labels ((ask (prompt default history)
                          (let* ((prompt (format prompt default))
                                 (input (read-from-minibuffer prompt
                                                              () () nil
                                                              history)))
                            (if (equal "" input)
                                default
                              (read input)))))
                 (list (ask "Initial value (default %d): "
                            1
                            'counter-value-history)
                       (ask "Increment or function (default %S): "
                            counter-step
                            'counter-step-history)
                       (ask "Format (default %S): "
                            ;; ?? Try using minibuffer-help-form here
                            counter-format
                            'counter-format-history))))
  (setq counter-value  (eval value)
        counter-step         step
        counter-format (eval format)))

(provide 'counter)

;;; counter.el ends here