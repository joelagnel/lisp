




; Hi all

; This is my first programme in emacs lisp, written some time ago. This
; was written at a time when I understood very little about local
; variables in elisp. However it was fun learning to write this
; program.

; I would be glad if people could pass on their comments and criticism
; to me. Thank you and have fun.

;;; Time-stamp: <23 March 2000 14:46 mohan from grain>

;;; $Log: natsel.el,v $
;;; Revision 1.4  1998/03/19 15:26:18  mohan
;;; Some more trials today.
;;;
;;; Revision 1.3  1998/03/19 01:04:31  mohan
;;; After some more improvements
;;;
;;; Revision 1.2  1998/03/19 00:22:36  mohan
;;; Great changes done
;;;
;;; Revision 1.1  1998/03/19 00:02:02  mohan
;;; Initial revision
;;;

;;; This file is NOT part of GNU Emacs but the same permissions apply.
;;
;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139,
;; USA.
;;


;;; This program is an implementation of the game Richard Dawkins
;;; invented for his book ``The Selfish Gene'', or was it ``The Blind
;;; Watchmaker''!

;;; To use this, load this file in Emacs, eval the buffer and M-x
;;; natsel-game.

;;; Let us declare some variables first

(defvar natsel-vlist-abc
  '("a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p"
    "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"))

(defvar natsel-vlist-ABC
  '("A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P"
    "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z"))

(defvar natsel-vlist-123
  '("1" "2" "3" "4" "5" "6" "7" "8" "9" "0" " "))

(defvar natsel-vlist-punct
  '("," "." "?" ";" "$" "&" "!" ":" "-" "`" "'" "\"" "#" "@" "^" "*"
    "(" ")" "[" "]" "{" "}" "^" "\\"))

(defvar natsel-vlist
  (append natsel-vlist-abc
          natsel-vlist-ABC
          natsel-vlist-123
          natsel-vlist-punct
          ))

;;; Let us calculate the length of this list.

(defvar natsel-vlist-length (length natsel-vlist))

;;; Getting the string

(defvar natsel-aim-string "")
(defvar natsel-string-length nil)

(defun natsel-get-string ()
  (setq natsel-aim-string
        (read-string "String you want to generate: ")))

;;; generating a random string of length natsel-string-length.

(defvar natsel-initial-random-string "")

(defun natsel-generate-initial-random-string ()
  "Generate a random string from which to start iterating to the final
string"
  (setq natsel-initial-random-string
        (substring (yow) 0 natsel-string-length)))

;;; Generating strings---the darwinian way.

(defun natsel-generate-contiguous-string (natsel-prestring)
  "The main function that implements the darwinian selection of
strings"
  (setq len 1)
  (setq natsel-poststring nil)
  (while (<= len natsel-string-length)
    ;; If character matches let us keep it. Else we move on to create
    ;; another random character in place of that hoping it would be
    ;; better.
    (if (string=
         (substring natsel-aim-string (1- len) len)
         (substring natsel-prestring (1- len) len))
        ;; It matched.
        (setq len-string (substring natsel-aim-string (1- len) len))
      ;; No it did not match.
      (setq len-string (nth (random natsel-vlist-length)
                            natsel-vlist)))
    ;; Let us concat the results.
    (setq natsel-poststring (concat natsel-poststring (format "%s"
							      len-string)))
    (setq len (1+ len))))


;;; The function call sequence.

(defvar natsel-poststring)

(defvar natsel-trial 0)

(defun natsel-game (&optional natsel-aim-string)
  "This is a elisp simulation of meant to illustrate the theory of
natural selection. Use M-x natsel-game to get a feel for it. When
calling in a program use \(natsel \"string to be selected\"\)."
  (interactive)
  (setq natsel-trial 0)
  (get-buffer-create "*Dawkins*")
  (switch-to-buffer "*Dawkins*")
  (erase-buffer)
  (if (eq natsel-aim-string nil)
      (natsel-get-string))
  (setq natsel-string-length (length natsel-aim-string))
  (insert natsel-aim-string "\n")
  (natsel-generate-initial-random-string)
  (setq natsel-prestring natsel-initial-random-string)
  (insert natsel-prestring "\n")
  (natsel-generate-contiguous-string natsel-prestring)
  (while (not (string= natsel-aim-string natsel-poststring))
    (natsel-generate-contiguous-string natsel-poststring)
    (setq natsel-trial (1+ natsel-trial))
    (insert (format "%04s  %s\n" natsel-trial natsel-poststring))))


