;;; eev-glyphs.el - change how some special characters are displayed

;; Copyright (C) 1999,2000,2001,2002,2003,2004 Free Software
;; Foundation, Inc.
;;
;; This file is part of GNU eev.
;;
;; GNU eev is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; GNU eev is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;
;; Author:     Eduardo Ochs <eduardoochs@gmail.com>
;; Maintainer: Eduardo Ochs <eduardoochs@gmail.com>
;; Version:    2006oct02
;; Keywords:   display, glyphs
;; Latest version: <http://angg.twu.net/eev-current/eev-glyphs.el>
;;       htmlized: <http://angg.twu.net/eev-current/eev-glyphs.el.html>

;;; Commentary:

;; This library sets some "glyphs" in Emacs's default display table.
;; Glyphs are usually considered ugly hacks, but I find them convenient -
;; they are easy to use, to understand (when you think that a file is
;; a sequence of bytes and each byte is a character) and to explain to
;; people.
;;
;; The following glyphs are set:
;;
;;   appearance  position         how to type it
;;                8 (backspace)  C-q C-h
;;               12 (formfeed)   C-q C-l
;;            ;  13 (CR)         C-q C-m
;;               15 (^O)         C-q C-o
;;   ›           155 (CSI)
;;   « and »     171 and 187      M-, < < and M-, > >
;;
;; M-, is defined in <http://angg.twu.net/eev-current/eev-compose.el>
;;                   <http://angg.twu.net/eev-current/eev-compose.el.html>.
;;
;; (find-elnode "Display Tables")
;; (find-elnode "Glyphs")
;;
;; Known bugs:
;;   * Emacs treats most chars with codes below 32 as whitespace by
;;     default, and in some cases (for example when leaving
;;     picture-mode) it will remove our red stars together with the
;;     "real" whitespace.
;;   * When we move text with "«»"s from unibyte- to multibyte buffers
;;     Emacs will convert the "«»"s to characters with higher codes
;;     for which no glyphs were set; the converted "«»"s will be
;;     displayed in the default face.
;;
;; (find-elnode "Defining Faces")
;; (find-efunctiondescr 'defface)
;; (find-efunction 'defface)
;; (find-efunction 'custom-declare-face)
;; (find-efile "generic-x.el" "(background light)")

;; Note: older versions of this file used to behave specially when
;; Emacs was running on a Linux VT (i.e., when TERM=linux); using
;; string glyphs and character sequences that only worked on Linux's
;; "virtual consoles" they would create a glyph for ^O that would
;; appear as a small red square instead of a red star, and do other
;; hackish things involving characters that were tricky to display.
;;
;; (find-linux26file "drivers/char/vt.c")
;; (find-man "console_codes" "ECMA-48 Set Graphics Rendition" "foreground")
;; (find-man "console_codes" "CSI (0x9B) is equivalent to ESC [")
;; (find-man "4 console_codes" "ESC % @")
;; (find-man "4 console_codes" "ESC % G")
;; (find-man "4 console_codes" "straight to character ROM")
;; (find-man "7 charsets" "1110xxxx")
;; (find-man "7 utf-8" "0x00000800 - 0x0000FFFF:")
;; (find-man "8 consolechars" "straight-to-font zone")
;; (find-udfile "console-tools/lct.txt.gz" "from U+F000 to U+F1FF")

(defface eev-glyph-face-bluebg
  '((t (:background "blue")))
  "Face used for the glyph for backspace (char 8; a solid blue box).")

(defface eev-glyph-face-yellow-on-red
  '((t (:foreground "yellow" :background "red")))
  "Face used for the formfeed glyph (char 12).")

(defface eev-glyph-face-blue
  '((t (:foreground "blue")))
  "Face used for the glyph for CR (char 13).")

(defface eev-glyph-face-red
  '((t (:foreground "red")))
  "Face used for the red star glyph (char 15).")

(defface eev-glyph-face-bang
  '((t (:foreground "blue" :background "red")))
  "Face used for the CSI glyph (char 128+27=155).")

(defface eev-glyph-face-green
  '((((class color) (background dark))
     (:foreground "green"))
    (((class color) (background light))
     (:foreground "forest green"))
    (t (:bold t)))
  "Face used for the green glyphs (`<<' and `>>', chars 171 and 187).")

(defun ee-glyph (char &optional face)
  "Convert CHAR and FACE to a number that can be used in a glyph.
CHAR can be a number or a string (with length 1); FACE can be a
face name (a symbol) or nil.
See:\n
  (find-elnode \"Glyphs\")
  (find-elnode \"Display Tables\")
  (find-elnode \"Display Table Format\")\n
Example: the two sexps below set the display table entry for the
TAB character to a string:\n
  (aset standard-display-table 9 [?< ?t ?a ?b ?>])
  (aset standard-display-table 9
    (vector (logior ?< (ash (face-id 'highlight) 19))
	    (logior ?t (ash (face-id 'highlight) 19))
	    (logior ?a (ash (face-id 'highlight) 19))
	    (logior ?b (ash (face-id 'highlight) 19))
	    (logior ?> (ash (face-id 'highlight) 19))))\n
And this one makes tabs \"normal\" again:\n
  (aset standard-display-table 9 nil)\n
Using `ee-glyph' we can replace the \"(logior ...)\"s above by sexps
like:\n
  (ee-glyph ?< 'highlight)"
  (logior (if (numberp char) char (string-to-char char))
	  (ash (if face (face-id face) 0)
	       19)))

(defun eev-set-glyph (pos &optional char face)
  "Set a face glyph for the char at position POS in `standard-display-table'.
If both CHAR and FACE are non-nil then display POS as CHAR with face FACE.
If only CHAR is non-nil then display POS as CHAR with the default face.
If both CHAR and FACE are nil then display POS in the default way.

Use `let' to operate on another display table:
  (let ((standard-display-table my-display-table))
    (eev-glyph-set ?\\^A ?A 'eev-glyph-face-red))"
  (aset standard-display-table pos
	(if char (vector (ee-glyph char face)))))

(defun eev-set-default-glyphs ()
  "Set the default glyphs for eev on `standard-display-table'.
Note that the glyphs for \"«\" and \"»\" only work on raw-text or
unibyte buffers."
  (or standard-display-table
      (setq standard-display-table (make-display-table)))
  (eev-set-glyph    8 32 'eev-glyph-face-bluebg)
  (eev-set-glyph   12 ?L 'eev-glyph-face-yellow-on-red)
  (eev-set-glyph   13 ?M 'eev-glyph-face-blue)
  (eev-set-glyph ?\^O ?* 'eev-glyph-face-red)
  (eev-set-glyph  155 ?! 'eev-glyph-face-bang)
  (eev-set-glyph   ?« ?« 'eev-glyph-face-green)
  (eev-set-glyph   ?» ?» 'eev-glyph-face-green))

(provide 'eev-glyphs)

;; The comments below are remnants of previous versions and should be
;; deleted soon.

;; (find-elnode "Association Lists")
;; (find-efile "")

;; (find-elnode "Display Tables")
;;  (find-elnode "Glyphs")
;;  (find-elnode "Active Display Table")
;; (find-elnode "Usual Display")
;; (find-efile "disp-table.el")
;; (find-evardescr 'glyph-table)
;; (find-evardescr 'buffer-display-table)

;; (find-fline "~/LATEX/dout/doutface.el")
;; (find-efile "term/tty-colors.el")

;; A first attempt to modernize the code above (2004oct21)...
;; (find-elnode "Defining Faces")
;; (find-efunctiondescr 'defface)
;; (find-efunction 'defface)
;; (find-efunction 'custom-declare-face)
;; (find-efile "generic-x.el" "(background light)")
;;
;; (defface glyphs-face-green
;;   '((((class color) (background dark))  (:foreground "green"))
;;     (((class color) (background light)) (:foreground "forest green"))
;;     (t (:bold t)))
;;   "Face for the glyphs `<<' and `>>'.")
;;
;; (eev "emacs -bg white -fg black  ~/eev-current/glyphs.el &")
;; (eev "emacs -bg black -fg bisque ~/eev-current/glyphs.el &")

;; This is how I used to test the console codes for string glyphs:
;; (ee-write-string (glyphs-make-string "1;31;41" ?! nil) "/dev/tty3")

;; ›«»


;; Local Variables:
;; coding:               raw-text-unix
;; ee-anchor-format:     "«%s»"
;; End:
