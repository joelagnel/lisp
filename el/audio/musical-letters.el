;;; Saved through ges-version 0.3.3dev at 2004-11-20 18:06
;;; ;;; From: Joe Corneli <jcorneli@math.utexas.edu>
;;; ;;; Subject: musical-letters.el (revision)
;;; ;;; Newsgroups: gmane.emacs.sources
;;; ;;; Cc: FluidSynth Devel <fluid-dev@nongnu.org>
;;; ;;; Date: Sat, 12 Jun 2004 22:05:06 -0500

;;; Here is a revision to musical-letters.el and the
;;; musical-letters-mode it proves.  Probably I will have further
;;; updates before long, including an improved way to switch soundfonts
;;; and change which instuments are attached to which midi channel.

;;; But the current version is an improvement to the previous one I
;;; posted.  The code is cleaner and more of the fluidsynth features are
;;; implemented.

;;; Also, there are still a few minor bugs that seem worth fixing, so
;;; posting the code will give me something to refer to as I ask for
;;; help with that.


;;; musical-letters.el -- play the fluidsynth software synthesizer

;; Copyright (C) 2004 Joe Corneli <jcorneli@math.utexas.edu>

;; Time-stamp: <jac -- Sat Jun 12 21:57:47 CDT 2004>

;; This file is not part of GNU Emacs, but it is distributed under
;; the same terms as GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 2, or (at your
;; option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; The flow in `musical-letters-mode' is from the keyboard to Emacs,
;; and from Emacs to Fluidsynth.  At the Emacs stage, two things
;; happen: When a printing character (hereafter, a "letter") is input
;; by the user, it is inserted into the buffer; then a code associated
;; with this letter is sent to fluidsynth, which plays a note.  If
;; `musical-letters-navigation' is non-nil, navigating the buffer with
;; left and right arrow keys, backspace and delete keys also causes
;; notes to be played.

;; The user can change the code values that are sent to fluidsynth by
;; setting the variable `musical-letters-tuning'.  Currently only one
;; sound font file at a time is supported; the user can change this
;; font by setting the variable `musical-letters-font'.

;; Some known issues:

;; Since we are having trouble binding to non-ASCII keys, that isn't
;; really consequential right now anyway.  We could scrape around
;; inside of ASCII for things like `~' that haven't been mapped yet,
;; but I don't think that there are many other things we could
;; use... oh yes, "`".  Well, there are actually more things, but I
;; just currently have them embedded in my keyboard, not arrayed
;; around the periphery. Will need to get the non-ASCII characters to
;; work if I want these extra keys to be usable.

;; Rather than going through each binding and adding them back in
;; explicitly, it might be more effective to have a way to make the
;; default binding run before/after whatever sound is associated with
;; that action plays.  But I don't know how to get at the default
;; action.  This can be a problem when there are several minor modes
;; going at once; for example, musical letters mode and edebug.  In
;; this case, since `q' (for example) doesn't have the default action,
;; it won't be sent to edebug properly.

;;; History:

;; The author began experimenting with the idea of musical letters in
;; the summer of 2002.  We now have a demo system that combines music
;; with typing a very _fluid_ and, I think, elegant way.  Its even
;; fairly platform-independent, thanks to the efforts of the
;; fluidsynth and emacs developers -- wow!  Future plans include
;; extending this mode to record typed music using proper musical
;; notation.

;;; Code:

(defvar musical-letters-tuning "default"
  "Controls which keys produce which notes.
Set with the command `musical-letters-set-tuning'.")

;; this should be customizable
(defvar musical-letters-font-path "~/sf2/"
  "Path to directory that contains sound font files.")

;; this list should be customizable
(defvar musical-letters-fonts '("VintageDreamsWaves-v2.sf2"
                                "fenderbass.sf2"
                                "rhodespianoice.sf2")
  "Sound fonts to load for use in `musical-letters-mode'.")

(defvar musical-letters-active-chan 0
  "Active channel for `musical-letters-mode'.")

(defun musical-letters-set-active-chan (chan)
  "Set `musical-letters-active-chan' to CHAN."
  (interactive "nChannel: ")
  (setq musical-letters-active-chan (or chan
                                        0)))

(defun musical-letters-inc-active-chan ()
  "Increment `musical-letters-active-chan' by one."
  (interactive "p")
  (setq musical-letters-active-chan (1+ musical-letters-active-chan)))

(defvar musical-letters-navigation t
  "Whether or not left, right, backspace, and delete will play notes.
Set with the command `musical-letters-toggle-nav-sounds'. If non-nil,
these navigation keys will play the next letter encountered as the
cursor moves through the buffer.")

;; this are probably better ways to turn notes off.  It would be good to
;; show which notes are active and which are silent.
(defvar musical-letters-silencer nil
  "Whether or not left and right turns sounds off.")

(defvar musical-letters-transcribe-notation nil
  "Whether or not to transcribe keypresses in a separate buffer.")

(defvar musical-letters-silence-previous-sound nil
  "Whether or not to turn previous sound off when a new sound is played.")

(defvar fs-left-hook '()
  "*Functions to run when cursor moves left.")

(defvar fs-right-hook '()
  "*Functions to run when cursor moves right.")

(defvar fs-inserting-hook '()
  "*Functions to run just before a letter is inserted.")

(defun musical-letters-toggle-nav-sounds ()
  "Toggle audible output for left, right, backspace, and delete."
  (interactive)
  (if musical-letters-navigation
      (progn
        (setq musical-letters-navigation nil)
        ;; ideally this should probably have the optional local switch
        ;; set to t
        (remove-hook 'fs-left-hook 'fs-play-char-backwards)
        (remove-hook 'fs-right-hook 'fs-play-char-forwards))
    (setq musical-letters-navigation t)
    (add-hook 'fs-left-hook 'fs-play-char-backwards)
    (add-hook 'fs-right-hook 'fs-play-char-forwards)))

(defun musical-letters-toggle-nav-silencer ()
  "Toggle silencing functionality for left and right."
  (interactive)
  (if musical-letters-silencer
      (progn
        (setq musical-letters-silencer nil)
        (remove-hook 'fs-left-hook 'fs-silence-char-backwards)
        (remove-hook 'fs-right-hook 'fs-silence-char-forwards))
    (setq musical-letters-navigation t)
    (add-hook 'fs-left-hook 'fs-silence-char-backwards)
    (add-hook 'fs-right-hook 'fs-silence-char-forwards)))

(defun musical-letters-toggle-silence-previous ()
  "Toggle silencing functionality for inserting.
If active, the previously played note will be silenced."
  (interactive)
  (if musical-letters-silence-previous-sound
      (progn
        (setq musical-letters-silence-previous-sound nil)
        (remove-hook 'fs-inserting-hook 'fs-silence-char-backwards))
    (setq musical-letters-silence-previous-sound t)
    (add-hook 'fs-inserting-hook 'fs-silence-char-backwards)))

(defun musical-letters-toggle-transcription ()
  "Toggle audible output for left, right, backspace, and delete."
  (interactive)
  (if musical-letters-transcribe-notation
      (setq musical-letters-transcribe-notation nil)
    (setq musical-letters-transcribe-notation t)))

;; additional fonts should be a customizable list.  Should figure out
;; how to make fluidsynth load them and how to make it switch between
;; them.

;; would be potentially a good idea to just have a command for sending
;; an arbitrary string to the inferior fluidsynth process.

;; the list of layouts should be customizable, and this should complete over
;; that list, or the union of that list and the built in stuff.
(defun musical-letters-set-tuning (tuning)
  "Chose TUNING by which notes correspond to keys.
The standard choices are \"default\", which is tuned
to the ASCII system, \"basic dvorak\", and  \"basic qwerty\".
The \"full dvorak\" and \"full qwerty\" tunings are 
present, but not all keys will work with all keyboards or
all layouts. The \"dchrom\" layout maps the layout of 
notes in a C chromatic harmonica to the dvorak layout
by a simple projection."
  (interactive (list (completing-read "Tuning: "
                                      '("default"
                                       "basic dvorak"
                                       "basic qwerty"
                                       "full dvorak"
                                       "full qwerty"
                                       "dchrom"
                                       "altdchrom"
                                       "simple dchrom"))))
  (setq musical-letters-tuning tuning))

;; we should have an option for verbose output that would cause the
;; transliteration of a given string to be printed in another window according
;; to the current transliteration mode (some candidates to consider would be
;; MusiXTeX, Lilypond, and some more graphical form of ascii-arted music).

(defun musical-letters-reset ()
  (interactive)
  (fs-send-command "reset"))

(defun musical-letters-send-fs-command (command)
  (interactive  (list (read-string "Command: ")))
  (fs-send-command command))

(define-minor-mode musical-letters-mode
  "Toggle musical letters mode.
With no argument, this command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode.

When musical letters mode is enabled, keypresses
insert letters and play notes through the fluidsynth
software synthesizer.

If `musical-letters-transcribe-notation' is non-nil,
music will be transcribed in the buffer *Musical-Log*.

Additional commands:
C-c#n  musical-letters-toggle-nav-sounds
C-c#s  musical-letters-toggle-nav-silencer
C-c#t  musical-letters-toggle-transcription
C-c#c  musical-letters-set-active-chan
C-c#r  musical-letters-reset"
  :init-value nil
  :lighter " Musical"
  :keymap '(((quote [right]) . fs-right)
            ((quote [left]) . fs-left)
            ((quote [delete]) . fs-delete-forwards)
            ("" . fs-delete)
            ;; I think that this is the correct binding
            ("#n" . musical-letters-toggle-nav-sounds)
            ("#s" . musical-letters-toggle-nav-silencer)
            ("#t" . musical-letters-toggle-transcription)
            ("#c" . musical-letters-set-active-chan)
            ("#r" . musical-letters-reset)
            ("a" . (lambda () (interactive) (fs-c "a")))
            ("b" . (lambda () (interactive) (fs-c "b")))
            ("c" . (lambda () (interactive) (fs-c "c")))
            ("d" . (lambda () (interactive) (fs-c "d")))
            ("e" . (lambda () (interactive) (fs-c "e")))
            ("f" . (lambda () (interactive) (fs-c "f")))
            ("g" . (lambda () (interactive) (fs-c "g")))
            ("h" . (lambda () (interactive) (fs-c "h")))
            ("i" . (lambda () (interactive) (fs-c "i")))
            ("j" . (lambda () (interactive) (fs-c "j")))
            ("k" . (lambda () (interactive) (fs-c "k")))
            ("l" . (lambda () (interactive) (fs-c "l")))
            ("m" . (lambda () (interactive) (fs-c "m")))
            ("n" . (lambda () (interactive) (fs-c "n")))
            ("o" . (lambda () (interactive) (fs-c "o")))
            ("p" . (lambda () (interactive) (fs-c "p")))
            ("q" . (lambda () (interactive) (fs-c "q")))
            ("r" . (lambda () (interactive) (fs-c "r")))
            ("s" . (lambda () (interactive) (fs-c "s")))
            ("t" . (lambda () (interactive) (fs-c "t")))
            ("u" . (lambda () (interactive) (fs-c "u")))
            ("v" . (lambda () (interactive) (fs-c "v")))
            ("w" . (lambda () (interactive) (fs-c "w")))
            ("x" . (lambda () (interactive) (fs-c "x")))
            ("y" . (lambda () (interactive) (fs-c "y")))
            ("z" . (lambda () (interactive) (fs-c "z")))
            ("A" . (lambda () (interactive) (fs-c "A")))
            ("B" . (lambda () (interactive) (fs-c "B")))
            ("C" . (lambda () (interactive) (fs-c "C")))
            ("D" . (lambda () (interactive) (fs-c "D")))
            ("E" . (lambda () (interactive) (fs-c "E")))
            ("F" . (lambda () (interactive) (fs-c "F")))
            ("G" . (lambda () (interactive) (fs-c "G")))
            ("H" . (lambda () (interactive) (fs-c "H")))
            ("I" . (lambda () (interactive) (fs-c "I")))
            ("J" . (lambda () (interactive) (fs-c "J")))
            ("K" . (lambda () (interactive) (fs-c "K")))
            ("L" . (lambda () (interactive) (fs-c "L")))
            ("M" . (lambda () (interactive) (fs-c "M")))
            ("N" . (lambda () (interactive) (fs-c "N")))
            ("O" . (lambda () (interactive) (fs-c "O")))
            ("P" . (lambda () (interactive) (fs-c "P")))
            ("Q" . (lambda () (interactive) (fs-c "Q")))
            ("R" . (lambda () (interactive) (fs-c "R")))
            ("S" . (lambda () (interactive) (fs-c "S")))
            ("T" . (lambda () (interactive) (fs-c "T")))
            ("U" . (lambda () (interactive) (fs-c "U")))
            ("V" . (lambda () (interactive) (fs-c "V")))
            ("W" . (lambda () (interactive) (fs-c "W")))
            ("X" . (lambda () (interactive) (fs-c "X")))
            ("Y" . (lambda () (interactive) (fs-c "Y")))
            ("Z" . (lambda () (interactive) (fs-c "Z")))
            ("'" . (lambda () (interactive) (fs-c "'")))
            ("," . (lambda () (interactive) (fs-c ",")))
            ("." . (lambda () (interactive) (fs-c ".")))
            (";" . (lambda () (interactive) (fs-c ";")))
            ("1" . (lambda () (interactive) (fs-c "1")))
            ("2" . (lambda () (interactive) (fs-c "2")))
            ("3" . (lambda () (interactive) (fs-c "3")))
            ("4" . (lambda () (interactive) (fs-c "4")))
            ("5" . (lambda () (interactive) (fs-c "5")))
            ("6" . (lambda () (interactive) (fs-c "6")))
            ("7" . (lambda () (interactive) (fs-c "7")))
            ("8" . (lambda () (interactive) (fs-c "8")))
            ("9" . (lambda () (interactive) (fs-c "9")))
            ("0" . (lambda () (interactive) (fs-c "0")))
            ("!" . (lambda () (interactive) (fs-c "!")))
            ("@" . (lambda () (interactive) (fs-c "@")))
            ("#" . (lambda () (interactive) (fs-c "#")))
            ("$" . (lambda () (interactive) (fs-c "$")))
            ("%" . (lambda () (interactive) (fs-c "%")))
            ("^" . (lambda () (interactive) (fs-c "^")))
            ("&" . (lambda () (interactive) (fs-c "&")))
            ("*" . (lambda () (interactive) (fs-c "*")))
            ("(" . (lambda () (interactive) (fs-c "(")))
            (")" . (lambda () (interactive) (fs-c ")")))
            ("?" . (lambda () (interactive) (fs-c "?")))
            ("/" . (lambda () (interactive) (fs-c "/")))
            ("_" . (lambda () (interactive) (fs-c "_")))
            ("-" . (lambda () (interactive) (fs-c "-")))
            ("<" . (lambda () (interactive) (fs-c "<")))
            (">" . (lambda () (interactive) (fs-c ">")))
            (":" . (lambda () (interactive) (fs-c ":")))
            ("[" . (lambda () (interactive) (fs-c "[")))
            ("]" . (lambda () (interactive) (fs-c "]")))
            ("{" . (lambda () (interactive) (fs-c "{")))
            ("}" . (lambda () (interactive) (fs-c "}")))
            ("~" . (lambda () (interactive) (fs-c "~")))
            ("`" . (lambda () (interactive) (fs-c "`")))
            (" " . (lambda () (interactive) (fs-c " ")))
            ("\"" . (lambda () (interactive) (fs-c "\"")))
            ("\\" . (lambda () (interactive) (fs-c "\\")))
            ;; These keys are for my full dvorak layout.
            ;; They don't seem to actually work however.
            ;; this is the case even though running
            ;; `(fs-c "<key>")' seems to work out fine.
            ;; so for some reason they don't seem to be
            ;; bound properly.  Should better isolate this
            ;; as a bug and see about reporting or even
            ;; fixing it.
            ("í" . (lambda () (interactive) (fs-c "í")))
            ("ì" . (lambda () (interactive) (fs-c "ì")))
            ("è" . (lambda () (interactive) (fs-c "è")))
            ("é" . (lambda () (interactive) (fs-c "é")))
            ("ù" . (lambda () (interactive) (fs-c "ù")))
            ("Í" . (lambda () (interactive) (fs-c "Í")))
            ("Ì" . (lambda () (interactive) (fs-c "Ì")))
            ("È" . (lambda () (interactive) (fs-c "È")))
            ("É" . (lambda () (interactive) (fs-c "É")))
            ("Ù" . (lambda () (interactive) (fs-c "Ù")))
            ("£" . (lambda () (interactive) (fs-c "£")))
            ("¥" . (lambda () (interactive) (fs-c "¥"))))
  ;; body

  ;; A bit more commentary on the mode:

  ;; We may want to load more than just one font in a more advanced
  ;; version of this mode.

  ;; Also, if for some reason we wanted to have different buffers to
  ;; have different fluidsynths associated with them, we'd have to
  ;; change the code around to support that.

  (if musical-letters-mode
      ;; (i.e. we just turned it on)
      (save-window-excursion 
;;;        (make-comint "fluid" "fluidsynth" nil (eval 
;;;                                               (cons 'concat (mapcar
;;;                                                              (lambda (font) 
;;;                                                                (concat 
;;;                                                                 musical-letters-font-path font " ")) 
;;;                                                              musical-letters-fonts))))
        (shell "*fluid*")
        ;; the next two sexps are necessary in order
        ;; to get the shell ready to read input.
        ;; I don't know if this is needed in the current/latest
        ;; version of Emacs...
        ;; \begin{kluge}
        (if (looking-at "^.")
            (forward-char 1))
        (if (not (eobp))
            (end-of-line))
        ;; \end{kluge}
        (insert 
         (concat "fluidsynth -n "
                 ;; load up the fonts
                 (eval 
                  (cons 'concat (mapcar
                                 (lambda (font) 
                                   (concat 
                                    musical-letters-font-path font " ")) 
                                 musical-letters-fonts)))))
        (comint-send-input))
    ;; there is perhaps no particular reason to kill the *fluid* buffer
    (kill-buffer "*fluid*")))

; This is what we _should_ probably run, but for some reason the
; fluidsynth that is instantiated by this code is totally silent.
; Weird.
;;;      (if (not (comint-check-proc "*fluid*"))
;;;          (make-comint "fluid" "fluidsynth" nil (eval 
;;;                  (cons 'concat (mapcar
;;;                                 (lambda (font) 
;;;                                   (concat 
;;;                                    musical-letters-font-path font " ")) 
;;;                                 musical-letters-fonts)))))
;;; Layouts:

;; If the user wants to write new layouts, they write them according
;; to this form.  For my part, I'll try to make `fs-play' accept new
;; user-defined layouts seamlessly.

;; note: I use my own slightly modified version of dvorak; there are a
;; few slight discrepancies with the usual one (though they aren't in
;; the core layout, but rather in the periphery).  The basic layout
;; (common to all dvoraks) is given here.

(defvar basic-dvorak-list
  '(("1" . 90)("2" . 91)("3" . 92)("4" . 93)("5" . 94)("6" . 95)("7" . 96)("8" . 97)("9" . 98)("0" . 99)
    ("'" . 80)("," . 81)("." . 82)("p" . 83)("y" . 84)("f" . 85)("g" . 86)("c" . 87)("r" . 88)("l" . 89)
    ("a" . 70)("o" . 71)("e" . 72)("u" . 73)("i" . 74)("d" . 75)("h" . 76)("t" . 77)("n" . 78)("s" . 79)
    (";" . 60)("q" . 61)("j" . 62)("k" . 63)("x" . 64)("b" . 65)("m" . 66)("w" . 67)("v" . 68)("z" . 69)
    ;; shifted
    ("!" . 50)("@" . 51)("#" . 52)("$" . 53)("%" . 54)("^" . 55)("&" . 56)("*" . 57)("(" . 58)(")" . 59)
   ("\"" . 40)("," . 41)("." . 42)("P" . 43)("Y" . 44)("F" . 45)("G" . 46)("C" . 47)("R" . 48)("L" . 49)
    ("A" . 30)("O" . 31)("E" . 32)("U" . 33)("I" . 34)("D" . 35)("H" . 36)("T" . 37)("N" . 38)("S" . 39)
    (":" . 20)("Q" . 21)("J" . 22)("K" . 23)("X" . 24)("B" . 25)("M" . 26)("W" . 27)("V" . 28)("Z" . 29))
  "The common Dvorak keyboard layout, set to music.")

;; Similarly, here is the common QWERTY layout.

(defvar basic-qwerty-list
  '(("1" . 90)("2" . 91)("3" . 92)("4" . 93)("5" . 94)("6" . 95)("7" . 96)("8" . 97)("9" . 98)("0" . 99)
    ("q" . 80)("w" . 81)("e" . 82)("r" . 83)("t" . 84)("y" . 85)("u" . 86)("i" . 87)("o" . 88)("p" . 89)
    ("a" . 70)("s" . 71)("d" . 72)("f" . 73)("g" . 74)("h" . 75)("j" . 76)("k" . 77)("l" . 78)(";" . 79)
    ("z" . 60)("x" . 61)("c" . 62)("v" . 63)("b" . 64)("n" . 65)("m" . 66)("," . 67)("." . 68)("/" . 69)
    ;; shifted
    ("!" . 50)("@" . 51)("#" . 52)("$" . 53)("%" . 54)("^" . 55)("&" . 56)("*" . 57)("(" . 58)(")" . 59)
    ("Q" . 40)("W" . 41)("E" . 42)("R" . 43)("T" . 44)("Y" . 45)("U" . 46)("I" . 47)("O" . 48)("P" . 49)
    ("A" . 30)("S" . 31)("D" . 32)("F" . 33)("G" . 34)("H" . 35)("J" . 36)("K" . 37)("L" . 38)(";" . 39)
    ("Z" . 20)("X" . 21)("C" . 22)("V" . 23)("B" . 24)("N" . 25)("M" . 26)("<" . 27)(">" . 28)("?" . 29))
  "The common QWERTY keyboard layout, set to music.")

; some mistakes in the first version of this.

;; blank spaces in the following layout indicate that I haven't got a key in
;; that spot in my xmodmap.

;;     `AA'        `A#'       `BB'      `CC'         `C#'      `DD'       `D#'       `EE'         `FF'       `F#'       `GG'         `G#'

(defvar full-dvorak-list
  '(("£" . 93) ("1"  . 94) ("2" . 95) ("3" . 96) ("4" . 97) ("5" . 98) ("6" . 99) ("7" . 100) ("8" . 101) ("9" . 102) ("0" . 103) ("/"  . 104)
    ("è" . 81) ("'"  . 82) ("," . 83) ("." . 84) ("p" . 85) ("y" . 86) ("f" . 87) ("g" . 88)  ("c" . 89)  ("r" . 90)  ("l" . 91)  ("é"  . 92)
    ("ù" . 69) ("a"  . 70) ("o" . 71) ("e" . 72) ("u" . 73) ("i" . 74) ("d" . 75) ("h" . 76)  ("t" . 77)  ("n" . 78)  ("s" . 79)  ("\\" . 80)
    (" " . 57) (";"  . 58) ("q" . 59) ("j" . 60) ("k" . 61) ("x" . 62) ("b" . 63) ("m" . 64)  ("w" . 65)  ("v" . 66)  ("z" . 67)  (" "  . 68)

    ("¥" . 45) ("!"  . 46) ("@" . 47) ("#" . 48) ("$" . 49) ("%" . 50) ("^" . 51) ("&" . 52)  ("*" . 53)  ("(" . 54)  (")" . 55)  ("?"  . 56)
    ("È" . 33) ("\"" . 34) ("<" . 35) (">" . 36) ("P" . 37) ("Y" . 38) ("F" . 39) ("G" . 40)  ("C" . 41)  ("R" . 42)  ("L" . 43)  ("É"  . 44)
    ("Ù" . 21) ("A"  . 22) ("O" . 23) ("E" . 24) ("U" . 25) ("I" . 26) ("D" . 27) ("H" . 28)  ("T" . 29)  ("N" . 30)  ("S" . 31)  ("|"  . 32)
    (" " .  9) (":"  . 10) ("Q" . 11) ("J" . 12) ("K" . 13) ("X" . 14) ("B" . 15) ("M" . 16)  ("W" . 17)  ("V" . 18)  ("Z" . 19)  (" "  . 20))
  "My full (modded) Dvorak-based keyboard layout, set to music.")

;; note: I don't know how the "standard" qwerty periphery is mapped, so watch out with the first and last columns of keys.
;; I should investigate this later, and also make the full dvorak layout more like the standard one.  (I can at least find
;; out what the layout looks like by checking out the layout that Kinesis provides.)

(defvar full-qwerty-list
  '(("£" . 93) ("1"  . 94) ("2" . 95) ("3" . 96) ("4" . 97) ("5" . 98) ("6" . 99) ("7" . 100) ("8" . 101) ("9" . 102) ("0" . 103) ("/"  . 104)
    ("è" . 81) ("q"  . 82) ("w" . 83) ("e" . 84) ("r" . 85) ("t" . 86) ("y" . 87) ("u" . 88)  ("i" . 89)  ("o" . 90)  ("p" . 91)  ("é"  . 92)
    ("ù" . 69) ("a"  . 70) ("s" . 71) ("d" . 72) ("f" . 73) ("g" . 74) ("h" . 75) ("j" . 76)  ("k" . 77)  ("l" . 78)  (";" . 79)  ("\\" . 80)
    (" " . 57) ("z"  . 58) ("x" . 59) ("c" . 60) ("v" . 61) ("b" . 62) ("n" . 63) ("m" . 64)  ("," . 65)  ("." . 66)  ("/" . 67)  (" "  . 68)

    ("¥" . 45) ("!"  . 46) ("@" . 47) ("#" . 48) ("$" . 49) ("%" . 50) ("^" . 51) ("&" . 52)  ("*" . 53)  ("(" . 54)  (")" . 55)  ("?"  . 56)
    ("È" . 33) ("Q"  . 34) ("W" . 35) ("E" . 36) ("R" . 37) ("T" . 38) ("Y" . 39) ("U" . 40)  ("I" . 41)  ("O" . 42)  ("P" . 43)  ("É"  . 44)
    ("Ù" . 21) ("A"  . 22) ("S" . 23) ("D" . 24) ("F" . 25) ("G" . 26) ("H" . 27) ("J" . 28)  ("K" . 29)  ("L" . 30)  (":" . 31)  ("|"  . 32)
    (" " .  9) ("Z"  . 10) ("X" . 11) ("C" . 12) ("V" . 13) ("B" . 14) ("N" . 15) ("M" . 16)  ("<" . 17)  (">" . 18)  ("?" . 19)  (" "  . 20))
  "A full QWERTY-based keyboard layout, set to music.")

; We should also add some diatonic scales, just for the sake of fun.  Other
; user-contributed tunings would be welcome too.

;; (while (re-search-forward "\\([0-9]+\\)" nil t)
;;   (let ((match (buffer-substring-no-properties (match-beginning 1) (match-end 1))))
;;   (replace-match (int-to-string (+ 40 (string-to-int match))))))

; draw+  D#          F#        A#          B#    |     D#         F#         A#        B#     |     D#         F#         A#          D
; draw-  D           F         A           B     |     D          F          A         B      |     D          F          A           B
; blow+  C#          E#        G#          C#    |     C#         E#         G#        C#     |     C#         E#         G#          C#
; blow-  C           E         G           C     |     C          E          G         C      |     C          E          G           C

(defvar dchrom-list
  '(("£" . 15) ("1"  . 18) ("2" . 22) ("3" . 24)  ("4" . 28) ("5" . 31) ("6" . 35) ("7" . 37)  ("8" . 41) ("9" . 44) ("0" . 48) ("/"  . 50)
    ("è" . 14) ("'"  . 17) ("," . 21) ("." . 23)  ("p" . 27) ("y" . 30) ("f" . 34) ("g" . 36)  ("c" . 40) ("r" . 43) ("l" . 47) ("é"  . 49)
    ("ù" . 13) ("a"  . 17) ("o" . 20) ("e" . 26)  ("u" . 26) ("i" . 30) ("d" . 33) ("h" . 39)  ("t" . 39) ("n" . 43) ("s" . 46) ("\\" . 52)
    (" " . 12) (";"  . 16) ("q" . 19) ("j" . 25)  ("k" . 25) ("x" . 29) ("b" . 32) ("m" . 38)  ("w" . 38) ("v" . 42) ("z" . 45) (" "  . 51)

    ("¥" . 55) ("!"  . 58) ("@" . 62) ("#" . 64)  ("$" . 68) ("%" . 71) ("^" . 75) ("&" . 77)  ("*" . 81) ("(" . 84) (")" . 88) ("?"  . 90)
    ("È" . 54) ("\"" . 57) ("<" . 61) (">" . 63)  ("P" . 67) ("Y" . 70) ("F" . 74) ("G" . 76)  ("C" . 80) ("R" . 83) ("L" . 87) ("É"  . 89)
    ("Ù" . 53) ("A"  . 57) ("O" . 60) ("E" . 66)  ("U" . 66) ("I" . 70) ("D" . 73) ("H" . 79)  ("T" . 79) ("N" . 83) ("S" . 86) ("|"  . 92)
    (" " . 52) (":"  . 56) ("Q" . 59) ("J" . 65)  ("K" . 65) ("X" . 69) ("B" . 72) ("M" . 78)  ("W" . 78) ("V" . 82) ("Z" . 85) (" "  . 91))
  "Notes in a C chromatic harmonica, mapped to the Dvorak keyboard.
The top row corresponds to \"draw, slide out\", the second row to 
\"draw, slide in\", the third row to \"blow, slide out\", and the
bottom row to \"blow, slide in\".")

;; shifted
; draw+  D#          F#        A#          B#    |     D#         F#         A#        B#     |     D#         F#         A#          D
; blow+  C#          E#        G#          C#    |     C#         E#         G#        C#     |     C#         E#         G#          C#

;; unshifted
; draw-  D           F         A           B     |     D          F          A         B      |     D          F          A           B
; blow-  C           E         G           C     |     C          E          G         C      |     C          E          G           C

(defvar simple-dchrom-list
  '(("£" . 15) ("\""  . 18) ("<" . 22) (">" . 24)  ("P" . 28) ("Y" . 31) ("F" . 35) ("G" . 37)  ("C" . 41) ("R" . 44) ("L" . 48) ("/"  . 50)
    ("Ù" . 13) ("A"  . 17)  ("O" . 20) ("E" . 26)  ("U" . 26) ("I" . 30) ("D" . 33) ("H" . 39)  ("T" . 39) ("N" . 43) ("S" . 46) ("\\" . 52)
    ("è" . 14) ("'"  . 17)  ("," . 21) ("." . 23)  ("p" . 27) ("y" . 30) ("f" . 34) ("g" . 36)  ("c" . 40) ("r" . 43) ("l" . 47) ("é"  . 49)
    (" " . 12) ("a"  . 16)  ("o" . 19) ("e" . 25)  ("u" . 25) ("i" . 29) ("d" . 32) ("h" . 38)  ("t" . 38) ("n" . 42) ("s" . 45) (" "  . 51))
  "Notes in a C chromatic harmonica on the home row.
Shifting corresponds to using the slide.")

(defvar alt-dchrom-list
  '(("£" . 18) ("1"  . 25) ("2" . 32) ("3" . 39)  ("4" . 46) ("5" . 53) ("6" . 17) ("7" . 24)   ("8" . 31) ("9" . 48) ("0" . 55) ("/"  . 62)
    ("è" . 17) ("'"  . 24) ("," . 30) ("." . 37)  ("p" . 44) ("y" . 51) ("f" . 16) ("g" . 23)   ("c" . 29) ("r" . 47) ("l" . 53) ("é"  . 60)
    ("ù" . 15) ("a"  . 22) ("o" . 29) ("e" . 36)  ("u" . 42) ("i" . 49) ("d" . 14) ("h" . 21)   ("t" . 28) ("n" . 45) ("s" . 52) ("\\" . 59)
    (" " . 13) (";"  . 20) ("q" . 27) ("j" . 34)  ("k" . 41) ("x" . 48) ("b" . 12) ("m" . 19)   ("w" . 26) ("v" . 33) ("z" . 50) (" "  . 57)

    ("¥" . 68) ("!"  . 75) ("@" . 82) ("#" . 89)  ("$" . 96) ("%" . 103) ("^" . 67) ("&" . 74)  ("*" . 81) ("(" . 98) (")" . 105) ("?"  . 112)
    ("È" . 67) ("\"" . 74) ("<" . 80) (">" . 87)  ("P" . 94) ("Y" . 101) ("F" . 66) ("G" . 73)  ("C" . 79) ("R" . 97) ("L" . 103) ("É"  . 110)
    ("Ù" . 65) ("A"  . 72) ("O" . 79) ("E" . 86)  ("U" . 92) ("I" . 99)  ("D" . 64) ("H" . 71)  ("T" . 78) ("N" . 95) ("S" . 102) ("|"  . 109)
    (" " . 63) (":"  . 70) ("Q" . 77) ("J" . 84)  ("K" . 91) ("X" . 98)  ("B" . 62) ("M" . 69)  ("W" . 76) ("V" . 83) ("Z" . 100) (" "  . 107))
  "Inspired by the chromatic harmonica, played on the Dvorak keyboard.
The left hand corresponds roughly to \"slide out\" and the right hand corresponds
to \"slide in\".")


;; This is the main sound generator!  Check it out.

;; I deal with the E#'s and B#'s by playing them at a higher velocity
;; (I don't know if this is the wisest way to go or not.)

;; the cond takes time linear in the number of layouts...  I wonder
;; when that would begin to give a noticable slow-down -- probably not
;; for a very long time...

(defun fs-send-command (command)
  (save-excursion
    (set-buffer (get-buffer "*fluid*"))
    (insert command)
    (comint-send-input)))

(defun fs-play (input)
  "Fluidsynth note-playing engine.
For given INPUT, create a fluidsynth command
according to the current tuning to turn the
corresponding note on, and send this command
to fluidsynth. Return the MIDI value associated with 
the key in the current tuning."
  (let* (datum letter key)
    (save-excursion
      (set-buffer (get-buffer "*fluid*"))
      ;; now we look at the different layout cases.
      (cond 
       ;; default
       ((equal musical-letters-tuning "default")
        (setq key (- (string-to-char input) 33))
        (insert (concat "noteon " (int-to-string musical-letters-active-chan) " ")
                (int-to-string key) " 95"))
       ;; basic dvorak
       ((equal musical-letters-tuning "basic dvorak")
        (setq key (cdr (assoc input basic-dvorak-list)))
        (musical-letters-common-handler key))
       ;; basic qwerty
       ((equal musical-letters-tuning "basic qwerty")
        (setq key (cdr (assoc input basic-qwerty-list)))
        (musical-letters-common-handler key))
       ;; full dvorak
       ((equal musical-letters-tuning "full dvorak")
        (setq key (cdr (assoc input full-dvorak-list)))
        (musical-letters-common-handler key))
       ;; full qwerty
       ((equal musical-letters-tuning "full qwerty")
        (setq key (cdr (assoc input full-qwerty-list)))
        (musical-letters-common-handler key))
       ;; altdchrom
       ((equal musical-letters-tuning "altdchrom") 
        (setq datum (assoc input alt-dchrom-list)
              letter (car (assoc input alt-dchrom-list))
              key (cdr  (assoc input alt-dchrom-list)))
        (musical-letters-chrom-handler letter key))
       ;; dchrom
       ((equal musical-letters-tuning "dchrom") 
        (setq datum (assoc input dchrom-list)
              letter (car (assoc input dchrom-list))
              key (cdr  (assoc input dchrom-list)))
        (musical-letters-chrom-handler letter key))
       ;; simple dchrom
       ((equal musical-letters-tuning "simple dchrom") 
        (setq datum (assoc input dchrom-list)
              letter (car (assoc input simple-dchrom-list))
              key (cdr  (assoc input simple-dchrom-list)))
        (musical-letters-chrom-handler letter key)))
      (comint-send-input)
    key)))

(defun musical-letters-common-handler (key)
  (insert (concat "noteon " 
                  (int-to-string musical-letters-active-chan) 
                  " ")
          (if key 
              (int-to-string key)
            "1") 
          " 95"))

(defun musical-letters-chrom-handler (key)
  (insert (concat "noteon " 
                  (int-to-string musical-letters-active-chan) 
                  " ")
          (if key
              (int-to-string key)
            "1")
          ;; this is how I deal with the E#'s that are present in the
          ;; chromatic harmonica-based layouts: F is played at a
          ;; higher velocity.
          (if (member letter '("'" "y" "r" "\"" "Y" "R"))
              " 125"
            " 95")))

(defun fs-silence (input)
  "Fluidsynth note-silencing engine.
For given INPUT, create a fluidsynth command
according to the current tuning to turn the
corresponding note off, and send this command
to fluidsynth. Return the MIDI value associated with 
the key in the current tuning."
  (let* (datum letter key)
    (save-excursion
      (set-buffer (get-buffer "*fluid*"))
      ;; now we look at the different layout cases.
      (cond 
       ;; altdchrom
       ((equal musical-letters-tuning "altdchrom") 
        (setq datum (assoc input alt-dchrom-list)
              letter (car (assoc input alt-dchrom-list))
              key (cdr  (assoc input alt-dchrom-list)))
        (insert (concat "noteoff " (int-to-string musical-letters-active-chan) " ")
                (if key
                    (int-to-string key)
                  "1")
                (if (member letter '("'" "y" "r" "\"" "Y" "R"))
                    " 125"
                  " 95")))
       ;; dchrom
       ((equal musical-letters-tuning "dchrom") 
        (setq datum (assoc input dchrom-list)
              letter (car (assoc input dchrom-list))
              key (cdr  (assoc input dchrom-list)))
        (insert (concat "noteoff " (int-to-string musical-letters-active-chan) " ")
                (if key
                    (int-to-string key)
                  "1")
                ;; this is how I deal with the E#'s -- F is played
                ;; at a higher velocity
                (if (member letter '("'" "y" "r" "\"" "Y" "R"))
                    " 125"
                  " 95")))
       ;; simple dchrom
       ((equal musical-letters-tuning "simple dchrom") 
        (setq datum (assoc input dchrom-list)
              letter (car (assoc input simple-dchrom-list))
              key (cdr  (assoc input simple-dchrom-list)))
        (insert (concat "noteoff " (int-to-string musical-letters-active-chan) " ")
                (if key
                    (int-to-string key)
                  "1")
                ;; this is how I deal with the E#'s -- F is played
                ;; at a higher velocity
                (if (member letter '("'" "y" "r" "\"" "Y" "R"))
                    " 125"
                  " 95")))
       ;; default
       ((equal musical-letters-tuning "default")
        (setq key (- (string-to-char input) 33))
        (insert (concat "noteoff " (int-to-string musical-letters-active-chan) " ")
                (int-to-string key) " 95"))
       ;; basic dvorak
       ((equal musical-letters-tuning "basic dvorak")
        (setq key (cdr (assoc input basic-dvorak-list)))
        (insert (concat "noteoff " (int-to-string musical-letters-active-chan) " ")
                (if key key "1") " 95"))
       ;; basic qwerty
       ((equal musical-letters-tuning "basic qwerty")
        (setq (cdr (assoc input basic-qwerty-list)))
        (insert (concat "noteoff " (int-to-string musical-letters-active-chan) " ")
                (if key key "1") " 95"))
       ;; full dvorak
       ((equal musical-letters-tuning "full dvorak")
        (setq key (cdr (assoc input full-dvorak-list)))
        (insert (concat "noteoff " (int-to-string musical-letters-active-chan) " ")
                (if key 
                    (int-to-string key)
                  "1") 
                " 95")))
      (comint-send-input)
    key)))

(defun fs-c (input)
  "Insert the INPUT letter and play it through `fs-play'.
If `musical-letters-transcribe-notation' is non-nil, print
a transcription in a separate buffer."
  (run-hooks 'fs-inserting-hook)
  (insert input)
  ;; maybe we should have an `fs-inserted-hook' to run
  ;; right after the letter is inserted
  (let ((key (fs-play input)))
  (if musical-letters-transcribe-notation
      (musical-letters-print key))))

(defun musical-letters-print (key)
  "Print a transcription of KEY in the logging buffer."
  (let ((curbuf (current-buffer))
        (note (cdr (assoc key musical-letters-midi-notes))))
  (save-excursion
    (pop-to-buffer (get-buffer-create "*Musical Log*"))
    (insert (if note note "--") " "))
  (pop-to-buffer curbuf)))

(defvar musical-letters-midi-notes
     '((0 . "C (-1)")
       (1 . "C# (-1)")
       (2 . "D (-1)")
       (3 . "D# (-1)")
       (4 . "E (-1)")
       (5 . "F (-1)")
       (6 . "F# (-1)")
       (7 . "G (-1)")
       (8 . "G# (-1)")
       (9 . "A (-1)")
       (10 . "A# (-1)")
       (11 . "B (-1)")
       (12 . "C (0)")
       (13 . "C# (0)")
       (14 . "D (0)")
       (15 . "D# (0)")
       (16 . "E (0)")
       (17 . "F (0)")
       (18 . "F# (0)")
       (19 . "G (0)")
       (20 . "G# (0)")
       (21 . "A (0)")
       (22 . "A# (0)")
       (23 . "B (0)")
       (24 . "C (1)")
       (25 . "C# (1)")
       (26 . "D (1)")
       (27 . "D# (1)")
       (28 . "E (1)")
       (29 . "F (1)")
       (30 . "F# (1)")
       (31 . "G (1)")
       (32 . "G# (1)")
       (33 . "A (1)")
       (34 . "A# (1)")
       (35 . "B (1)")
       (36 . "C (2)")
       (37 . "C# (2)")
       (38 . "D (2)")
       (39 . "D# (2)")
       (40 . "E (2)")
       (41 . "F (2)")
       (42 . "F# (2)")
       (43 . "G (2)")
       (44 . "G# (2)")
       (45 . "A (2)")
       (46 . "A# (2)")
       (47 . "B (2)")
       (48 . "C (3)")
       (49 . "C# (3)")
       (50 . "D (3)")
       (51 . "D# (3)")
       (52 . "E (3)")
       (53 . "F (3)")
       (54 . "F# (3)")
       (55 . "G (3)")
       (56 . "G# (3)")
       (57 . "A (3)")
       (58 . "A# (3)")
       (59 . "B (3)")
       (60 . "C (4)")
       (61 . "C# (4)")
       (62 . "D (4)")
       (63 . "D# (4)")
       (64 . "E (4)")
       (65 . "F (4)")
       (66 . "F# (4)")
       (67 . "G (4)")
       (68 . "G# (4)")
       (69 . "A (4)")
       (70 . "A# (4)")
       (71 . "B (4)")
       (72 . "C (5)")
       (73 . "C# (5)")
       (74 . "D (5)")
       (75 . "D# (5)")
       (76 . "E (5)")
       (77 . "F (5)")
       (78 . "F# (5)")
       (79 . "G (5)")
       (80 . "G# (5)")
       (81 . "A (5)")
       (82 . "A# (5)")
       (83 . "B (5)")
       (84 . "C (6)")
       (85 . "C# (6)")
       (86 . "D (6)")
       (87 . "D# (6)")
       (88 . "E (6)")
       (89 . "F (6)")
       (90 . "F# (6)")
       (91 . "G (6)")
       (92 . "G# (6)")
       (93 . "A (6)")
       (94 . "A# (6)")
       (95 . "B (6)")
       (96 . "C (7)")
       (97 . "C# (7)")
       (98 . "D (7)")
       (99 . "D# (7)")
       (100 . "E (7)")
       (101 . "F (7)")
       (102 . "F# (7)")
       (103 . "G (7)")
       (104 . "G# (7)")
       (105 . "A (7)")
       (106 . "A# (7)")
       (107 . "B (7)")
       (108 . "C (8)")
       (109 . "C# (8)")
       (110 . "D (8)")
       (111 . "D# (8)")
       (112 . "E (8)")
       (113 . "F (8)")
       (114 . "F# (8)")
       (115 . "G (8)")
       (116 . "G# (8)")
       (117 . "A (8)")
       (118 . "A# (8)")
       (119 . "B (8)")
       (120 . "C (9)")
       (121 . "C# (9)")
       (122 . "D (9)")
       (123 . "D# (9)")
       (124 . "E (9)")
       (125 . "F (9)")
       (126 . "F# (9)")
       (127 . "G (9)")))

(defun fs-play-char-backwards ()
  "Play the letter to the left of the cursor."
    (let* ((end (point))
           (beg (save-excursion
                  (backward-char 1)
                  (point)))
           (entry (buffer-substring-no-properties beg end)))
      (fs-play entry)))

(defun fs-play-char-forwards ()
  "Play the letter to the left of the cursor."
    (let* ((end (point))
           (beg (save-excursion
                  (forward-char 1)
                  (point)))
           (entry (buffer-substring-no-properties beg end)))
      (fs-play entry)))

(defun fs-silence-char-backwards ()
  "Silence the letter to the left of the cursor."
    (let* ((end (point))
           (beg (save-excursion
                  (backward-char 1)
                  (point)))
           (entry (buffer-substring-no-properties beg end)))
      (fs-silence entry)))

(defun fs-silence-char-forwards ()
  "Silence the letter to the left of the cursor."
    (let* ((end (point))
           (beg (save-excursion
                  (forward-char 1)
                  (point)))
           (entry (buffer-substring-no-properties beg end)))
      (fs-silence entry)))

(defun fs-delete ()
  (interactive)
  (delete-char -1)
  (unless (or (not musical-letters-navigation)
               (bobp))
    (fs-play-char-backwards)))

(defun fs-delete-forwards ()
  (interactive)
  (delete-char 1)
  (unless (or (not musical-letters-navigation)
               (eobp))
    (fs-play-char-forwards)))

;; these should either _play_ the character or turn it _off_ (or do
;; something else, depending on environmetal variables).

(defun fs-left ()
  (interactive)
  (backward-char)
  (unless (or (not musical-letters-navigation)
               (bobp))
    (run-hooks 'fs-left-hook)))

(defun fs-right ()
  (interactive)
  (forward-char 1)
  (unless (or (not musical-letters-navigation)
              (eobp))
    (run-hooks 'fs-right-hook)))

(provide 'musical-letters)

;;; musical-letters.el ends here

