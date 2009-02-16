;;; glyphs.el - change how some special characters are displayed

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
;; Author:     Eduardo Ochs <edrx@mat.puc-rio.br>
;; Maintainer: Eduardo Ochs <edrx@mat.puc-rio.br>
;; Version:    2005mar01
;; Keywords:   display, glyphs
;; Latest version: <http://angg.twu.net/eev-current/glyphs.el>
;;       htmlized: <http://angg.twu.net/eev-current/glyphs.el.html>

;;; Commentary:

;; This library sets some "glyphs" in Emacs's default display table.
;; Glyphs are usually considered ugly hacks, but I find them convenient -
;; they are easy to use, to understand (when you think that a file is
;; a sequence of bytes and each byte is a character) and to explain to
;; people. Unfortunately they're not very compatible with Emacs's
;; support for international alphabets and with "multibyte" encodings.
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
;; M-, is defined in <http://angg.twu.net/eev-current/compose.el>
;;                   <http://angg.twu.net/eev-current/compose.el.html>.
;; A raw CSI has confusing effects when sent to a Linux VT:
;; (find-man "4 console_codes" "CSI (0x9B) is equivalent to ESC [.")
;;
;; (find-elnode "Conditionals")
;; (find-elnode "Window Systems")
;;
;; Variables/flags (note: this package used to be compatible with emacs20)
;; Emacs20 can't display face glyphs in console mode
;; String glyphs only work in console mode
;; The usual X fonts only have chars in the ranges 32..126 and 160..254
;; In Linux VTs the chars in the range 128..159 (except for 155) are printable
;; In Linux VTs the chars 0..31, 127, 155, 255 can be printed using utf8
;; Some Emacs versions don't like string glyphs in the positions 128..159
;;
;; (find-k26file "drivers/char/vt.c")

(defvar glyphs-allow-string-glyphs (not window-system))
(defvar glyphs-allow-face-glyphs
  (or window-system
      (>= emacs-major-version 21)))
(defvar glyphs-allow-string-glyphs-in-dangerous-positions
  (cond ((= emacs-major-version 20)
	 (>= emacs-minor-version 4))
	((= emacs-major-version 21)
	 (>= emacs-minor-version 3))))
(defvar glyphs-allow-vt-chars (not window-system))
(defvar glyphs-allow-utf8-chars (equal (getenv "TERM") "linux"))

(defvar glyphs-prefer-string-glyphs nil)

(defvar glyphs-face-to-colorstr-alist ())

;; String glyphs
;; (find-man "console_codes" "ECMA-48 Set Graphics Rendition" "foreground")
;; (find-man "console_codes" "CSI (0x9B) is equivalent to ESC [")
;; (find-man "4 console_codes" "ESC % @")
;; (find-man "4 console_codes" "ESC % G")
;; (find-man "4 console_codes" "straight to character ROM")
;; (find-man "7 charsets" "1110xxxx")
;; (find-man "7 utf-8" "0x00000800 - 0x0000FFFF:")
;; (find-man "8 consolechars" "straight-to-font zone")
;; (find-udfile "console-tools/lct.txt.gz" "from U+F000 to U+F1FF")

(defun glyphs-make-string (colorstr char encode-utf8)
  (let ((str (if encode-utf8
		 (format "\e%%G\xef%c%c\e%%@"
			 (+ 128 (logand (lsh char -6) 7))
			 (+ 128 (logand char 63)))
	       (format "%c" char))))
    (if colorstr
	(setq str (format "\e[%sm%s\e[m" colorstr str)))
    str))
    
(defun glyphs-set-string-glyph (position colorstr char encode-utf8)
  (let ((str (glyphs-make-string colorstr char encode-utf8)))
    (aset standard-display-table
	  position (vector (create-glyph str)))
    str))

(defun glyphs-char-type (char)
  (cond ((or (and (>= char 32)  (<= char 126))
	     (and (>= char 160) (<= char 255)))
	 'normal)
	((and (>= char 128) (<= char 159) (not (= char 155)))
	 'vt)
	(t 'utf8)))

(defun glyphs-set-string-glyph-safe (position colorstr char)
  (if glyphs-allow-string-glyphs
      (let ((char-type (glyphs-char-type char)))
	(if (and (cond ((eq char-type 'normal) t)
		       ((eq char-type 'vt) glyphs-allow-vt-chars)
		       ((eq char-type 'utf8) glyphs-allow-utf8-chars))
		 (cond ((and (>= position 128) (<= position 159))
			glyphs-allow-string-glyphs-in-dangerous-positions)
		       (t t)))
	    (glyphs-set-string-glyph position colorstr char
				     (eq char-type 'utf8))))))

;; Functions for face glyphs
;;  (find-elnode "Glyphs")

(defun glyphs-set-face-glyph (position face char)
  (aset standard-display-table
	position (vector (if face (logior char (ash (face-id face) 19))
			   char))))

(defun glyphs-set-face-glyph-safe (position face char)
  (if (and glyphs-allow-face-glyphs
	   (eq (glyphs-char-type char) 'normal))
     (glyphs-set-face-glyph position face char)))

(defun glyphs-define-face (face &optional bg fg bold colorstr)
  (make-face face)
  (if fg (set-face-foreground face fg))
  (if bg (set-face-background face bg))
  (if bold (set-face-bold-p face bold))
  (setq glyphs-face-to-colorstr-alist
	(cons (cons face colorstr) glyphs-face-to-colorstr-alist)))

(defun glyphs-colorstr-for (face)
  (cdr (assoc face glyphs-face-to-colorstr-alist)))




;; Functions for examining glyphs
;; Note that they just return a value that is displayed in the minibuffer.

(defun ee-faceid-to-face (faceid facelist)
  (if facelist
      (if (= faceid (face-id (car facelist)))
	  (car facelist)
	(ee-faceid-to-face faceid (cdr facelist)))))

(defun ee-faceandchar-to-list (n)
  (let* ((faceid (ash n -19))
	 (face (ee-faceid-to-face faceid (face-list)))
	 (char (logand n 524287))
	 (charstr (format "%c" char)))
    `(,n -> (,faceid -> ,face) (,char = ,charstr))))

(defun ee-n-to-stringglyph (n)
  (if (and glyph-table (< n (length glyph-table)))
      (let ((s (aref glyph-table n)))
	`(,n -> ,s))))

(defun find-eglyph (pos)
  (let* ((posstr (format "%c" pos))
	 (v (aref standard-display-table pos))
	 (descr
	  (if (arrayp v)
	      (let ((v0 (aref v 0)))
		(or (ee-n-to-stringglyph v0)
		    (ee-faceandchar-to-list v0)))
	    '(no glyph))))
    `((,pos = ,posstr) -> ,v ,descr)))

;; (find-eglyph ?)
;; (find-eglyph ?«)
;; (find-eglyph ?%)

;; By the way:
;; . What happens when glyphs are vectors of length != 1?
;; . My glyph-table only has strings - can it have numbers or vectors?
;; . The `default' face has face-id=0 - doesn't that interfere with
;;   the encoding of string glyphs?

;; (find-elnode "Association Lists")
;; (find-efile "")

;; (find-elnode "Display Tables")
;;  (find-elnode "Glyphs")
;;  (find-elnode "Active Display Table")
;; (find-elnode "Usual Display")
;; (find-efile "disp-table.el")
;; (find-evardescr 'glyph-table)
;; (find-evardescr 'buffer-display-table)

;; ?\^O ?*  "31"
;; ?«   ?«  "32"     
;; ?»   ?»  "32"     

;; 155  ?! "1;31;41"	; 155=128+ESC=CSI, dont display it directly!
;; ?\^H 32 "44"     
;; 12   ?L "1;33;41"      ; formfeed
;; 13   ?M "34"           ; CR (^M)

;; (find-fline "~/LATEX/dout/doutface.el")
;; (find-efile "term/tty-colors.el")


;; High-level functions

(defun glyphs-set-safe-1 (face pos char)
  (if glyphs-prefer-string-glyphs
      (or (glyphs-set-string-glyph-safe pos (glyphs-colorstr-for face) char)
	  (glyphs-set-face-glyph-safe position face char))
    (or (glyphs-set-face-glyph-safe position face char)
	(glyphs-set-string-glyph-safe pos (glyphs-colorstr-for face) char))))

(defun glyphs-set (face position char &rest rest)
  (let ((rslt (glyphs-set-safe-1 face position char)))
    (if rest (apply 'glyphs-set face rest)
      rslt)))

(defun glyphs-set-basic ()
  (standard-display-8bit 160 254)
  (glyphs-define-face 'glyphs-face-red           nil    "red"    nil "31")
;;(glyphs-define-face 'glyphs-face-green         nil    "green"  nil "32")
  (glyphs-define-face 'glyphs-face-blue          nil    "blue"   nil "34")
  (glyphs-define-face 'glyphs-face-bluebg        "blue" nil      nil "44")
  (glyphs-define-face 'glyphs-face-yellow-on-red "red"  "yellow" t   "1;33;41")
  (glyphs-define-face 'glyphs-face-bang          "red"  "blue"   nil "1;31;41")
  (glyphs-set 'glyphs-face-bluebg  8 ?\ )
  (glyphs-set 'glyphs-face-blue   13 ?M)
  (glyphs-set 'glyphs-face-red    15 ?*)
  (glyphs-set 'glyphs-face-bang  155 ?!)
  (glyphs-set 'glyphs-face-yellow-on-red ?\^L ?L)
  (glyphs-set 'glyphs-face-green ?« ?« ?» ?»)
  )


;; A first attempt to modernize the code above (2004oct21)...
;; (find-elnode "Defining Faces")
;; (find-efunctiondescr 'defface)
;; (find-efunction 'defface)
;; (find-efunction 'custom-declare-face)
;; (find-efile "generic-x.el" "(background light)")
;;
(defface glyphs-face-green
  '((((class color) (background dark))  (:foreground "green"))
    (((class color) (background light)) (:foreground "forest green"))
    (t (:bold t)))
  "Face for the glyphs `<<' and `>>'.")
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
