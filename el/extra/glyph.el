;;; glyphs.el --- display some characters in special ways.

;; Copyright (C) 2000, 2001 Eduardo Ochs.

;; Author:     Eduardo Ochs <edrx@mat.puc-rio.br>
;; Maintainer: Eduardo Ochs <edrx@mat.puc-rio.br>
;; Version:    2001mar20
;; Keywords:   glyphs, display, eev
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
;; License, or (at your option) any later version.

;;; Commentary:

;; I use this to put colored characters in e-scripts, and sometimes
;; also in TeX texts or in general mathematical notes. See
;; <http://angg.twu.net/emacs.html>.
;; (find-shttpw3 "angg.twu.net/emacs.html")
;; (find-shttpw3 "angg.twu.net/vtutil.html")

;; (find-elnode "Autoload" "magic autoload comment")
;; (find-elnode "Comment Tips")
;; (find-elnode "Library Headers")
;; (find-elnode "Documentation Tips")
;; (find-elnode "Documentation Tips" "single-quotes")
;; (find-elnode "Keys in Documentation")
;; (find-es "escripts" "eev-tutorial")

;;; History:

;; 2001mar20: rewrote from scratch.

;; «.glyphs-lowlevel»	(to "glyphs-lowlevel")
;; «.glyphs-medlevel»	(to "glyphs-medlevel")
;; «.glyphs-highlevel»	(to "glyphs-highlevel")
;; «.glyphs-sysfeatures»  (to "glyphs-sysfeatures")
;; «.five_disptables»	(to "five_disptables")
;; «.face-glyphs»	(to "face-glyphs")
;; «.string-glyphs»	(to "string-glyphs")

;;; Code:

;; «glyphs-lowlevel»  (to ".glyphs-lowlevel")
;; The main low-level function - essentially a wrapper around `aset'.
;;
(defun glyph-set_ (disptable dtpos glyph)
  (if (not disptable) (setq disptable standard-display-table))
  (if (listp disptable)
      (mapcar (list 'lambda '(dt) (list 'glyph-set_ 'dt dtpos glyph))
	      disptable)
    (aset disptable dtpos glyph)))


;; «glyphs-medlevel»  (to ".glyphs-medlevel")
;; Medium-level glyphs functions and variables,
;; for both string and face glyphs.
;;
;; GNU Emacsen 20.{1,2,3} and 21.0.{<=93} (at least) can crash if we
;; set a string glyph in the range 128..159 and try to display it, so
;; we create the variable `glyphs-allow-128-to-159' to avoid
;; triggering the bug.  On using UTF-8 to display weird chars, see:
;;  (find-es "console" "glyphs512")
;;  (find-man "console_codes" "ECMA-48 Set Graphics Rendition")
;;  (find-man "7 utf-8")

(setq glyphs-allow-128-to-159 t)
(setq glyphs-allow-unicode t)

(defun glyph-need-unicode (n)
  (or (< n 32) (>= n 256) (= n 155)))

(defun glyph-string (n colorstr)
  (let (str)
    (setq str (if (glyph-need-unicode n)
		  (if glyphs-allow-unicode
		      (format "\e%%G\xef%c%c\e%%@"
			      (+ 128 (logand (lsh n -6) 7))
			      (+ 128 (logand n 63)))
		    "?")
		(format "%c" n)))
    (if colorstr (setq str (format "\e[%sm%s\e[m" colorstr str)))
    str))

(defun glyph-set-string (disptable dtpos n colorstr)
  (if (or glyphs-allow-128-to-159 
	  (< dtpos 128)
	  (>= dtpos 160))
      (glyph-set_ disptable
		  dtpos (vector (create-glyph (glyph-string n colorstr))))))

(defun glyph-set-face (disptable dtpos n face)
  (glyph-set_ disptable
	      dtpos (vector (if face (logior n (ash (face-id face) 19))
			      n))))


;; «glyphs-highlevel»  (to ".glyphs-highlevel")
;; High-level glyph functions.
;;
(defun glyphs-set-face (disptable dtpos n face &rest rest)
  (glyph-set-face disptable dtpos n face)
  (if rest (apply 'glyphs-set-face disptable rest)))

(defun glyphs-set-string (disptable dtpos n face &rest rest)
  (glyph-set-string disptable dtpos n face)
  (if rest (apply 'glyphs-set-string disptable rest)))




;; «glyphs-sysfeatures»  (to ".glyphs-sysfeatures")
;; Disallow some features on buggy Emacsen and crippled systems.

(if (= emacs-major-version 20)
    (if (< emacs-minor-version 4)
	(setq glyphs-allow-128-to-159 nil)))

;; (find-elnode "Text Comparison")
(defun emacs-version-in (lowerlim upperlim)
  (not (or (string< emacs-version lowerlim)
	   (string< upperlim emacs-version))))

;; 21.2.99 is not a real Emacs version, but I noticed that Emacs
;; 21.3.50.1 (a CVS version) allows glyphs in the 128..159 range...
;;
(if (= emacs-major-version 21)
    (if (emacs-version-in "21.1" "21.2.99")
	(setq glyphs-allow-128-to-159 nil)
      (setq glyphs-allow-128-to-159 t)))

;; on 21.2.1 the 128..159 bug is only half-fixed...
;; More on that later.
;; (huh, what did I mean when I wrote that? (2004jan19))



;; «five_disptables»  (to ".five_disptables")
;;
;; (find-efile "disp-table.el" "g1")
;; (find-efile "disp-table.el" "standard-display-european")
;; (find-efile "disp-table.el" "standard-display-8bit")
;; (find-elnode "Sequence Functions" "copy-sequence")

;; Note that the chars 128..159 are blank on the usual X (or latin) fonts.

(if (not (boundp 'disptable-base))
    (progn
      (if glyphs-allow-128-to-159
	  (standard-display-8bit 128 254)
	(standard-display-8bit 160 254))
      (setq disptable-base         (copy-sequence standard-display-table))
      (setq disptable-string-850   (copy-sequence disptable-base))
      (setq disptable-string-latin (copy-sequence disptable-base))
      (setq disptable-face-850     (copy-sequence disptable-base))
      (setq disptable-face-latin   (copy-sequence disptable-base))))

(setq disptable-is-latin t)
(setq disptable-uses-faces
      (or window-system (= emacs-major-version 21)))

(defun disptable-select () (interactive)
  (if disptable-uses-faces
      (setq disptable-latin disptable-face-latin
	    disptable-850   disptable-face-850)
    (setq   disptable-latin disptable-string-latin
	    disptable-850   disptable-string-850))
  (setq standard-display-table
	(if disptable-is-latin
	    disptable-latin
	  disptable-850)))

(disptable-select)


;; «face-glyphs»  (to ".face-glyphs")
;;
          (make-face 'eev-green)
(set-face-foreground 'eev-green "green")
          (make-face 'eev-red)
(set-face-foreground 'eev-red   "red")
          (make-face 'eev-blue)
(set-face-foreground 'eev-blue  "blue")
          (make-face 'eev-yellow-on-red)
(set-face-foreground 'eev-yellow-on-red "yellow")
(set-face-background 'eev-yellow-on-red "red")

(if (null window-system)
    (set-face-bold-p 'eev-yellow-on-red t)) ; For Emacs21 in console mode

(glyphs-set-face
 (list disptable-face-850 disptable-face-latin)
 12 ?L 'eev-yellow-on-red
 13 ?M 'eev-blue
 15 ?* 'eev-red
 16 ?> 'eev-red
 17 ?< 'eev-red
 )
(glyphs-set-face
 disptable-face-850
 174 (if window-system 171 174) 'eev-green
 175 (if window-system 187 175) 'eev-green
 )
(glyphs-set-face
 disptable-face-latin
 171 171 'eev-green
 187 187 'eev-green
 )


;; «string-glyphs»  (to ".string-glyphs")
;;
(glyphs-set-string
 (list disptable-string-850 disptable-string-latin)
 155  ?! "1;31;41"	; 155=128+ESC=CSI, dont display it directly!
 ?\^C ?ð "34"     	; Calligraphic
 ?\^G ?' "32"     	; Greek
 ?\^H 32 "44"     
 12   ?L "1;33;41"      ; formfeed
 13   ?M "34"           ; CR (^M)
 ?\^S ?s "32"     	; Script
 )
(glyphs-set-string
 disptable-string-850
 ?\^B ?ú  "1;33;41"	; Blackboard bold
 ?\^O 254 "31"     	; small red square
 ?®   ?®  "32"     
 ?¯   ?¯  "32"     
 19   ?ð  "35"     	; ^S: 
 )
(glyphs-set-string
 disptable-string-latin
 ?\^B 183 "1;33;41"	; Blackboard bold
 ?\^O ?*  "31"     	; small red square (no, a "*" for compatibility)
 ?«   ?«  "32"     
 ?»   ?»  "32"     
 19   173 "35"     	; ^S: 
 )
;; (find-elnode "Terminal-Specific")
(if (equal (getenv "TERM") "linux")
    (glyphs-set-string
     (list disptable-string-850 disptable-string-latin)
     16    16 "33"
     17    17 "33"
     23    23 nil 	; amalg
     26    26 nil 	; ->
     27    27 nil 	; <-
     ?\^R ? nil 
     ?\^E ? nil 
     ?\^T ? nil 
     ?\^D ? nil 
     ?\^F ? nil 
     ?\^^ ? nil 
     ?\^_ ? nil 
     ?\^N ? nil
     ?   ? nil
     ))

;; Functions to inspect disptables with glyphs.
;; This is just the skeleton of more serious code that is yet to come.

' (let* ((dt disptable-string-latin)
	 (dtc (aref dt ?\^O))
	 (dtc0 (aref dtc 0))
	 (str (aref glyph-table dtc0)))
    (list dtc dtc0 str))

' (let* ((dt disptable-face-latin)
	 (dtc (aref dt ?\^O))
	 (dtc0 (aref dtc 0))
	 (char (logand dtc0 (- (ash 1 19) 1)))
	 (faceid (ash dtc0 -19)))
    (list dtc dtc0 char faceid (face-id 'eev-red)))


;; (aref disptable-string-latin ?\^O)
;; (aref (aref disptable-string-latin ?\^O) 0)
;; (aref glyph-table (aref (aref disptable-string-latin ?\^O) 0))
;; (aref (aref glyph-table (aref (aref disptable-string-latin ?\^O) 0)) 5)
;;
;; (eq standard-display-table disptable-string-latin)
;; (eq standard-display-table disptable-string-850)
;; (eq standard-display-table disptable-face-latin)
;; (eq standard-display-table disptable-face-850)
;;
;; (setq standard-display-table disptable-string-latin)
;; (setq standard-display-table disptable-string-850)
;; (setq standard-display-table disptable-face-latin)
;; (setq standard-display-table disptable-face-850)

;; (find-elfile "startup.el")
;; (find-efile "disp-table.el")
;; (find-e21file "lisp/disp-table.el")
;; (describe-display-table standard-display-table)

;; (global-set-key "\M-D" 'edebug-defun)

;; (debug-on-entry 'glyphs-set-face)
;; (describe-display-table standard-display-table)
;; (describe-display-table disptable-face-850)
;; (describe-display-table disptable-face-latin)
;; (describe-display-table disptable-string-850)
;; (describe-display-table disptable-string-latin)
;; (ascstr 0 255)



;; (find-angg "eev-extras.el")

;; Local Variables:
;; coding:               no-conversion
;; ee-comment-format:	 ";; %s\n"
;; ee-anchor-format:     "«%s»"
;; ee-charset-indicator: "Ñ"
;; End: