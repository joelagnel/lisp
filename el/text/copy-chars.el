;;; copy-chars.el --- automated typing by copying characters

;; Copyright (C) 2001 Riku Saikkonen

;; Author: Riku Saikkonen <Riku.Saikkonen@hut.fi>
;; Version: 0.8
;; Keywords: abbrev convenience

;; This file is *NOT* part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
;; USA

;;; Commentary:
;;;
;;; This package adds support for copying data from a buffer to
;;; another a few characters at a time. It is meant mostly as an
;;; amusement to impress your friends with ("wow, do you really type
;;; that fast?").
;;;
;;; To use it, load this package and:
;;; 1. Visit a buffer with some data that you want to copy.
;;;    Go to the start of your data and execute the command
;;;    M-x copychars-start-copy-at-point.
;;; 2. Go somewhere where you want to copy data (e.g., create an empty
;;;    file). Run M-x copychars-mode, and start pressing the function
;;;    keys f5, f6, f7 and f8, with or without Shift. Repeatedly pressing
;;;    f5 to f8 looks kind of like you're typing the characters yourself.
;;;
;;; See the description for `copychars-mode' for more information.
;;;
;;; You may want to set `copychars-default-regexp' to the value in its
;;; description, to make the keys always jump over whitespace.

;;; Change Log:
;;; $Id: copy-chars.el,v 1.5 2001/10/28 19:16:16 rjs Exp $
;;;
;;; Version 0.8 (October 28th, 2001)
;;;  * Initial public release.

;;; Todo:
;;;  * The minor mode should define a different set of keys, as the
;;;    current keys are actually reserved for user customisation.
;;;    They probably should be single keys to make this mode easier to
;;;    use, but there aren't very many single keys free...
;;;  * A minor mode where any self-inserting character you type copies
;;;    data?
;;;  * insert whitespace at the beginning of a line in a single chunk,
;;;    so it looks like Tab was pressed (in modes that support it)
;;;  * perhaps an automated mode that uses a timer to type characters
;;;    at certain (random) intervals

;;; Code:

(eval-when-compile (require 'easy-mmode))
(require 'easy-mmode)

(defvar copychars-marker nil
  "*Marker to next character to be copied.
Set by `copychars-start-copy-at-point', reset by `copychars-end-copy'.
Used and moved forward by `copychars-insert-char'.")

(defvar copychars-max-insert 5
  "Maximum number of characters to insert.
Maximum number of characters (or regexp matches) to copy with
`copychars-insert-some-chars'.")

(defvar copychars-default-regexp nil
  "Regexp of things to copy at once.
The default regexp for `copychars-insert-char', which will copy
anything that matches this regexp in one go.
If nil, only single characters will be copied.
A nice value to set this to is \"[ \\t]+\" to copy whitespace as
if it was just one character.")

(defun copychars-start-copy-at-point ()
  "Start copying at point in the current buffer.
After this, `copychars-insert-char' will insert characters from here."
  (interactive)
  (if copychars-marker
      (set-marker copychars-marker (point))
    (setq copychars-marker (point-marker)))
  (message "Copy start point set."))

(defun copychars-end-copy ()
  "End copying and release the marker."
  (interactive)
  (when copychars-marker
    (set-marker copychars-marker nil)
    (setq copychars-marker nil)))

(defun copychars-insert-char (&optional regexp)
  "Copy (insert) the next character from the marker.
Inserts the character at the current `copychars-marker' into the
current buffer, and moves the marker forward.
If REGEXP is non-nil and the text to be copied matches that regular
expression, copy everything that matches instead of just one
character. The default for REGEXP is `copychars-default-regexp'.
Gives an error if there are no more characters."
  (interactive)
  (if (not copychars-marker)
      (error "Copying not started"))
  (unless regexp
    (setq regexp copychars-default-regexp))
  (insert
   (save-excursion
     (set-buffer (marker-buffer copychars-marker))
     (or (when regexp
           (goto-char (marker-position copychars-marker))
           (when (and (looking-at regexp)
                      (not (= (point) (match-end 0))))
             (set-marker copychars-marker (match-end 0))
             (buffer-substring-no-properties (point) (match-end 0))))
         (prog1 (char-to-string
                 (or (char-after copychars-marker)
                     (progn
                       (copychars-end-copy)
                       (error "End of buffer reached while copying"))))
           (set-marker copychars-marker (1+ copychars-marker)))))))

(defun copychars-insert-some-chars ()
  "Copy (insert) a few characters from the marker.
Does `copychars-insert-char' 1 to `copychars-max-insert' times."
  (interactive)
  (let ((timesleft (random copychars-max-insert)))
    (while (>= timesleft 0)
      (copychars-insert-char)
      (setq timesleft (1- timesleft)))))

(defun copychars-insert-rest-of-line ()
  "Copy (insert) the rest of the current line.
Includes the newline."
  (interactive)
  (copychars-insert-char "[^\n]*\n?"))

(defun copychars-insert-rest-of-word ()
  "Copy (insert) the rest of a word."
  (interactive)
  (copychars-insert-char "\\w+"))

(defun copychars-insert-whitespace ()
  "Copy (insert) as much whitespace as there is."
  (interactive)
  (copychars-insert-char "[ \t]+"))

(defun copychars-insert-to-whitespace ()
  "Copy (insert) until the next whitespace character."
  (interactive)
  (copychars-insert-char "[^ \t\n]+"))

(defun copychars-insert-to-closing-paren ()
  "Copy (insert) to the next ), ] or } character."
  (interactive)
  (copychars-insert-char "[^])}]*[])}]"))

(easy-mmode-define-minor-mode copychars-mode
  "Toggle Copy Chars minor mode.
With no argument, this command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode.

When Copy Chars mode is enabled, the keys f5 to f8 and S-f5 to S-f8
are bound to copy various pieces of a predefined buffer to where the
point is:
 * f8 copies a single character
 * f7 copies as much whitespace as there is, or a single
   non-whitespace character
 * f6 and f5 copy 1 to 5 characters at a time
 * S-f8 copies until the end of the current word (over \"abc\")
 * S-f7 copies until the next whitespace character (over \"abc-def\")
 * S-f6 copies to the next closing parenthesis (one of ])})
 * S-f5 copies to the end of the current line
All of the keys always copy at least one character.
When there is no data left to copy, pressing the keys signals an
error, which displays a message in the echo area."
  nil
  nil
  '(([f5] . copychars-insert-some-chars)
    ([f6] . copychars-insert-some-chars)
    ([f7] . copychars-insert-whitespace)
    ([f8] . copychars-insert-char)
    ([S-f5] . copychars-insert-rest-of-line)
    ([S-f6] . copychars-insert-to-closing-paren)
    ([S-f7] . copychars-insert-to-whitespace)
    ([S-f8] . copychars-insert-rest-of-word)))

(provide 'copy-chars)
