;;; eev-langs.el --- support for several extra languages in eev

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
;; Version:    2006oct02
;; Keywords:   help, hypertext, hyperlinks, e-scripts, languages,
;;             macros, php, Lua, Tcl

;;; Commentary:

;; See the individual sections. Huh, sorry; at the moment very few of
;; these are documented.


;; «.php»		(to "php")
;; «.lua»		(to "lua")
;; «.luaw»		(to "luaw")
;; «.tcl»		(to "tcl")
;; «.mp»		(to "mp")
;; «.ps»		(to "ps")
;; «.fvwm»		(to "fvwm")

;; «.eek»		(to "eek")
;; «.eesteps»		(to "eesteps")





;;;
;;; gud "hyperlinks"
;;;

(if (not (fboundp 'define-minor-mode))  ; for Emacs20
    (defalias 'define-minor-mode 'easy-mmode-define-minor-mode))

(defun eegud-kill-buffer (&optional s)
  ;; (interactive "sConfirm killing: ")
  (interactive)
  (let ((buffer (current-buffer)))
    (condition-case nil (delete-window (selected-window)) (error nil))
    (kill-buffer buffer)))

(defun eegud-show-gud-buffer ()
  (interactive)
  (if (and (bufferp gud-comint-buffer)
	   (buffer-name gud-comint-buffer)
	   (not (eq (current-buffer) gud-comint-buffer)))
      (progn
	(delete-other-windows)
	(split-window-vertically)
	(switch-to-buffer gud-comint-buffer))))

;; (find-evariable 'gud-comint-buffer)
;; (find-evardescr 'overlay-arrow-position)
;; (find-elnode "Killing Buffers" "`buffer-name' of a killed buffer")
;; Note: `eegud-show-gud-buffer' is a temporary hack.
;; The gud-comint-buffer should split windows automatically when the
;; debugged program starts running; that's not happening, but we can
;; force it by hand with a `M-G' (note the uppercase - it's `M-S-g').

(define-minor-mode eegud-keys-mode
 "eegud keys mode"
 nil
 " eegudk"
 '(("\M-s" . gud-step)
   ("\M-n" . gud-next)
   ("\M-c" . gud-cont)
   ("\M-f" . gud-finish)
   ("\M-k" . eegud-kill-buffer)))

;; (find-efile "progmodes/gud.el" "gdb --annotate=3")
;; (find-node "(gdb)Mode Options" "`-annotate LEVEL'")

(defun ee-gdb-start (dir &optional fname)
  (if (= emacs-major-version 21)
      (gdb (format "gdb %s%s" dir (or fname "")))
    (gdb (format "gdb --annotate=3 %s%s" dir (or fname ""))))
  (eegud-keys-mode))

(defun eeb-gdb-start (dir &optional fname)
  (eegdb-bounded)
  (ee-gdb-start dir fname))

(defun eeb-perldb-start (dir &optional fname)
  (if do-eeg-bounded (eeg-bounded))
  (perldb (format "perl %s%s" dir (or rest "")))
  (eegud-keys-mode))

;; what about pydb? I can't make it work...




;;;;;
;;
;; PHP
;;
;;;;;

;; «php»  (to ".php")
;;
(code-c-d "phpdoc" "/usr/doc/phpdoc/html/")
(defun find-phpdocpage (page &rest rest)
  (apply 'find-w3m (format "/usr/doc/phpdoc/html/%s.html" page) rest))

(defvar ee-file-php "/var/www/ee-tmp.php")
(defvar ee-url-php  "http://127.0.0.1/ee-tmp.php")
(defun eephp (s &optional e)
  (eev (format "wget -q -O - %s" ee-url-php))
  (ee-write-with-nl s e "<?\n" "?>" ee-file-php))
(defun eephp+ (s &optional e)
  (eev (format "w3m %s" ee-url-php))
  (ee-write-with-nl s e "<?\n" "?>" ee-file-php))

(eeb-define 'eeb-php  'eephp  'ee-delimiter-hash nil t t)
(eeb-define 'eeb-php+ 'eephp+ 'ee-delimiter-hash nil t t)

(eeb-define 'eephp-bounded  'eephp  'ee-delimiter-hash nil t t)
(eeb-define 'eephp+-bounded 'eephp+ 'ee-delimiter-hash nil t t)



;;;;;
;;
;; Lua
;;
;;;;;

;; «lua»  (to ".lua")
;;
(defvar ee-file-lua "$EEVTMPDIR/ee.lua")
(defun eelua (s &optional e)
  (interactive "r")
  (ee-write-with-nl s e "" "" ee-file-lua)
  (find-sh (format "lua50 %s" ee-file-lua)))

(defun eelua0 (s &optional e)
  (interactive "r")
  (ee-write-with-nl s e "" "" ee-file-lua)
  (find-sh0 (format "lua50 %s" ee-file-lua)))

(eeb-define 'eeb-lua  'eelua  "\n--\n" nil t t)
(eeb-define 'eeb-lua0 'eelua0 "\n--\n" nil t t)

(eeb-define 'eelua-bounded  'eelua  "\n--\n" nil t t)
(eeb-define 'eelua0-bounded 'eelua0 "\n--\n" nil t t)



;;;;;
;;
;; Tcl
;;
;;;;;

;; «eetcl»  (to ".eetcl")
;;
(defvar ee-file-tcl "$EEVTMPDIR/ee.tcl")
(defun eetcl (s &optional e)
  (interactive "r")
  (ee-write-with-nl s e "" "" ee-file-tcl))

(eeb-define 'eeb-tcl       'eetcl "\n#\n" nil t t)
(eeb-define 'eetcl-bounded 'eetcl "\n#\n" nil t t)

(defun find-tcl (str &rest rest)
  (eetcl str)
  (apply 'find-sh (format "tclsh %s" ee-file-tcl) rest))
(defun find-tcl0 (str)
  (eetcl str)
  (find-sh0 (format "tclsh %s" ee-file-tcl)))

(defun find-expcommand (str &rest rest)
  (interactive "sExpect command: ")
  (apply 'find-man "1 expect" "\nCOMMANDS"
	 (format "\n       %s" str) rest))

;; (find-man  "3tcl file" "file tail")
;; (find-tcl  "puts [file tail /foo/bar/blip/bletch]")
;; (find-tcl0 "puts [file tail /foo/bar/blip/bletch]")
;; (find-expcommand "interact")

;; (find-es "tcl")
;; (find-es "expect")




;;;;;
;;
;; MetaPost
;;
;;;;;

;; «eeb-mp»  (to ".eeb-mp")
;; (find-eevtmpfile "tmp.mp")

(setq ee-file-mp    "$EEVTMPDIR/ee.mp")
(setq ee-script-mp "cd $EEVTMPDIR/ &&
  mpost tmp.mp &&
  awk '{if (NR==1) {print \"%!PS-Adobe-2.0 EPSF-1.2\"} else print}' \\
    < tmp.0 > tmp.0.eps")

;; && regv tmp.0.eps -scale 10

(defun eemp (s &optional e)
  (interactive "r")
  (ee-write-with-nl s e "" "" ee-file-mp)
  (ee-write-with-nl ee-script-mp nil "" "" ee-file)
  (format "eemp: wrote %s and %s" ee-file-mp ee-file))

(defun eemp-now (s &optional e)
  (interactive "r")
  (ee-write-with-nl s e "" "" ee-file-mp)
  (find-sh ee-script-mp))

(eeb-define 'eeb-mp     'eemp     'ee-delimiter-percent nil t t)
(eeb-define 'eeb-mp-now 'eemp-now 'ee-delimiter-percent nil t t)

(eeb-define 'eemp-bounded     'eemp     'ee-delimiter-percent nil t t)
(eeb-define 'eemp-now-bounded 'eemp-now 'ee-delimiter-percent nil t t)

;; (eemp "pickup pencircle scaled 4pt; draw (0,0)--(0,30)--(30,0)--(0,0);")
;; (find-sh0 ". $EE")
;; (start-process-shell-command "gv" nil "gv -watch -scale +10 $EEVTMPDIR/tmp.0.eps")
;;
;; (eemp "pickup pencircle scaled 4pt; draw (0,0)--(0,20)--(30,0)--(0,0);")
;; (find-sh0 ". $EE")
;; (eemp "pickup pencircle scaled 4pt; draw (0,0)--(0,30)--(30,0)--(0,0);")
;; (find-sh0 ". $EE")




;;;;;
;;
;; PostScript
;;
;;;;;

;; «eepsrun»  (to ".eepsrun")

(setq ps-run-x '("gs" "-r36" "-sPAPERSIZE=a4"))

(defun eepsrun (s &optional e)
  (interactive "r")
  (require 'ps-mode)
  (unless (equal major-mode 'ps-mode) (ps-mode))
  (unless (equal (process-status "ps-run") 'run)
    (let ((pop-up-windows t))
      (ps-run-start)))
  (ps-run-region s e))

(eeb-define 'eeb-psrun       'eepsrun 'ee-delimiter-percent nil t t)
(eeb-define 'eepsrun-bounded 'eepsrun 'ee-delimiter-percent nil t t)




;;;;;
;;
;; Icon
;;
;;;;;

;; (find-es "icon")
;; (find-man "1 icont")
;; (find-fline "~/ICON/")
;; (find-fline "~/ICON/tmp.icn")

(setq ee-file-icon   "~/ICON/ee.icn")
(setq ee-script-icon "cd ~/ICON/ && icont -s tmp.icn -x")
(defun eeicon (s &optional e)
  (interactive "r")
  (ee-write-with-nl s e "" "" ee-file-icon)
  (ee-write-with-nl ee-script-icon nil "" "" ee-file)
  (format "eeicon: wrote %s and %s" ee-file-icon ee-file))

(defun eeicon-ow (s &optional e)
  (interactive "r")
  (ee-write-with-nl s e "" "" ee-file-icon)
  (find-progoutput-ow ee-script-icon))

(eeb-define 'eeb-icon    'eeicon    'ee-delimiter-hash nil t t)
(eeb-define 'eeb-icon-ow 'eeicon-ow 'ee-delimiter-hash nil t t)

(eeb-define 'eeicon-bounded    'eeicon    'ee-delimiter-hash nil t t)
(eeb-define 'eeicon-ow-bounded 'eeicon-ow 'ee-delimiter-hash nil t t)




;;;;;
;;
;; LilyPond
;;
;;;;;

;; (find-es "music")
;; (find-angg "LILYPOND/")
;;
(defun eelily (s &optional e)
  (ee-write s e "
    \\score {
      \\notes {
" "
      }
      \\paper {  }
      \\midi { }
    }" "$EEVTMPDIR/ee-ly.ly")
  (eev "cd $EEVTMPDIR && lilypond ee-ly && rexdvi ee-ly.dvi &"))

(eeb-define 'eeb-lily       'eelily  'ee-delimiter-percent nil t t)
(eeb-define 'eelily-bounded 'eelily  'ee-delimiter-percent nil t t)

;; (find-node "(lilypond)Pitches")
;; (find-node "(lilypond)Durations")
;; A test: (eelily "a,8 b, c d e f g a b c' d'")
;; Another one (Bach's Suites for Cello Solo, I, Menuet; BWV1007 (?))
;; Needs many fixes.
'
(eelily "\\key g \\major \\time 3/4
   g,8^0()d b4 a8^0 b16()c' |  b8()a g^4()fis g()d | e8()g c'()a^0 fis()b |
   <g, d b>2\\trill <d a>4 | \\break
   % 5
   a,8()fis^3 c'4^2 b8 c'16()d' | c'8()b a^4()g fis^3()e |
   fis8^1 g16()a g8()fis^1 e^1()fis | d4^0 a, d, \\bar \":|:\" |
   d8()fis^3 a4^2 g8 a16()b | \\break
  ")






;;;;;
;;
;; Awk
;;
;;;;;

;; (find-es "awk")


;;;;;
;;
;; Ruby
;;
;;;;;


;;;;;
;;
;; Perl
;;
;;;;;

;; (find-es "perl")
;; (find-es "perl1")


;;;;;
;;
;; Python
;;
;;;;;

;; (find-es "python")



;;;;;
;;
;; MagicPoint
;;
;;;;;

;; (find-es "mgp")

(defun ee-mgp-command-line (fname &optional page)
  (format "cd %s && mgp -x vflib %s %s"
	  (file-name-directory (expand-file-name fname))
	  (if (and page (> page 0)) (format "-p %d" page) "")
	  (file-name-nondirectory fname)))

(defun find-mgp (fname &optional page now)
  (interactive "fMagicPoint file: ")
  (if now (shell-command-to-string (ee-mgp-command-line fname page))
    (eev (ee-mgp-command-line fname page))))

(defun find-mgpnow (fname &optional page)
  (interactive "fMagicPoint file: ")
  (shell-command-to-string (ee-mgp-command-line fname page)))

(defun eemgp-file-now (&optional page)
  (interactive "P") (find-mgpnow (buffer-file-name) page))

(defun ee-count-occurrences (str s e &optional show-it)
  (interactive "MString: \nr\np")
  (ee-maybe-showing-it
   (save-excursion
     (let ((count 0) (s (min s e)) (e (max s e)))
       (goto-char s)
       (while (search-forward str e t)
	 (setq count (1+ count)))
       count))))

(defun eemgp-this-page-number ()
  (ee-count-occurrences "\n%page" (point-min) (point)))

(defun eemgp-show (&optional show-it)
  (interactive "p")
  (save-buffer)
  (ee-maybe-showing-it
   (eemgp-file-now (eemgp-this-page-number))))



;;;;;
;;
;; Fvwm
;;
;;;;;

;; «fvwm»  (to ".fvwm")
;; (find-es "fvwm")
;; (find-angg ".fvwm/edrx.fvwm")
;; (find-eevrc ".fvwmrc")
;;
(defvar ee-file-fvwm "~/.fvwm/ee.fvwm"
  "See `eefvwm0'.")

(defun eefvwm (s &optional e)
  "Write the region between S and E into the temporary Fvwm script file.
See `ee-se-to-string' for the exact definition of \"region\" - if S
is a string instead of a number the use S as the \"region\".
The name of the temporary script file is taken from the variable
`ee-file-fvwm'.
See `eefvwm0'."
  (interactive "r")
  (ee-write-with-nl s e "" "" ee-file-fvwm)
  (format "eefvwm: wrote %s" ee-file-fvwm))

(defun eefvwm0 (s &optional e)
  "Execute the commands in the region between S and E as a Fvwm script.
This function calls `eefvwm' to save the \"region\" given by the
pair (S E) into a temporary script file, and then runs
\"FvwmCommand ee\" to make Fvwm execute the script. This only
works if the current window manager is Fvwm, and if it has been
prepared to react to commands sent by \"FvwmCommand\", and if it
interprets \"ee\" as \"read ee.fvwm\". The code that should be
added to the fvwm init file (usually ~/.fvwm/.fvwm2rc) to do all
that is:\n
  # See:    (find-man \"FvwmCommand\")
  # A test: (find-sh0 \"FvwmCommand 'CursorMove +5 +2'\")
  AddToFunc StartFunction \"I\" Module FvwmCommandS\n
  # Test:   (eefvwm0 \"CursorMove +5 +2\")
  DestroyFunc ee
  AddToFunc ee \"I\" read ee.fvwm"
  (interactive "r")
  (eefvwm s e)
  (find-sh0 "FvwmCommand ee"))

(eeb-define 'eeb-fvwm  'eefvwm  'ee-delimiter-hash nil t t)
(eeb-define 'eeb-fvwm0 'eefvwm0 'ee-delimiter-hash nil t t)

(eeb-define 'eefvwm-bounded  'eefvwm  'ee-delimiter-hash nil t t)
(eeb-define 'eefvwm0-bounded 'eefvwm0 'ee-delimiter-hash nil t t)

(defun find-fvwm0 (str)
  (eefvwm str)
  (find-sh0 "FvwmCommand ee"))

;; Tests: (find-fvwm0 "KillModule FvwmPager")
;;        (find-fvwm0 "Module FvwmPager")

(defvar ee-fvwmhelp-prefix "\n       ")
(defun find-fvwmhelp (&optional str &rest pos-spec-list)
  (interactive
   (let ((word (read-string "Fvwm command: " (word-at-point))))
     (list (if (equal word "") nil word))))
  (if str (apply 'find-man "1 fvwm" (concat ee-fvwmhelp-prefix str)
		 pos-spec-list)
    (find-man "1 fvwm")))

;; (find-fvwmhelp "KillModule modulename [modulealias]")

;; 2006sep04 - waddletron2k pointed this to me:
;; http://www.lair.be/projects_fvwm-mode.php
;; http://www.lair.be/files/fvwm-mode.el





;;;;;
;;
;; SmallTalk
;;
;;;;;




;;;;;
;;
;; html
;;
;;;;;

(defun eehtml (s &optional e) 
  (interactive "r")
  (ee-write s e
	    (format "<html>\n<head>\n<title>%s</title>\n</head>\n<body>\n"
		    ee-file-html)
	    "\n</body>\n</html>\n"
	    ee-file-html)
  ee-file-html)

(defun eeurl (url)
  (interactive (list (car (browse-url-interactive-arg "eeurl "))))
  (eehtml (format "<a href=\"%s\">\n%s\n</a>" url url)))

(eeb-define 'eeb-html       'eehtml 'ee-delimiter-hash nil t t)
(eeb-define 'eehtml-bounded 'eehtml 'ee-delimiter-hash nil t t)

;; (eeurl "http://www.cs.utah.edu/dept/old/texinfo/emacs18/emacs_21.html")




;;;;;
;;
;; erc and knowledgebots
;;
;;;;;

(defun ee-send-to-erc-channel (channel line)
  (if (not (get-buffer channel))
      (error "There's no buffer called \"%s\"" channel))
  (if (not (eq 'erc-mode (with-current-buffer channel major-mode)))
      (error "The buffer \"%s\" is not an ERC buffer"))
  (switch-to-buffer channel)
  (goto-char (point-max))
  (insert line)
  (erc-send-current-line))

(defun find-fsbot-answer (question)
  "Ask a question to fsbot, a knowledgebot at the #emacs channel of Freenode"
  (interactive "sQuestion: ")
  (ee-send-to-erc-channel "fsbot" question))

;; Example: (find-fsbot-answer "htmlize?")




;;;;;
;;
;; hyperlinks to images
;;
;;;;;

;; Example:
;; (eev "inkscape $EEVDIR/doc/find-fline.svg &")
;; (find-eimage0 "$EEVDIR/doc/find-fline.png" 2 3)
;; Note: I did some experiments with adding
;;   (keymap '(keymap (?q . eeimage-back-to-text)))
;; to the text properties - to let me get rid of the image by just
;; typing `q' on it - but I found that that wasn't worth the pain; now
;; I prefer to use `<f10> e t R' -> `facemenu-remove-all' instead and
;; keep my code very simple.
;; (key-binding [menu-bar edit props ra])

(defun eeimage-data (fname)
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally fname)
    (buffer-string)))

(defun eeimage-set (s e fname)
  (add-text-properties s e 
      `(display (image :type ,(image-type-from-file-header fname)
		       :data ,(eeimage-data fname)))))

(defun find-eimage0 (fname &optional nlines nchars &rest ignored)
  "Display the image given by FNAME in place of the text between point and bol.
This function is meant to be used with \\[eek-eval-sexp-eol], so
by default it will change the appearance of the entire current
line. If NLINES is non-nil then work on that number of lines
instead - e.g., 5 means change this line and the four previous
ones. A non-nil value of NCHARS means to use bol+NCHARS instead
of bol."
  (eeimage-set (point)
	       (+ (or nchars 0) (point-at-bol (- 2 (or nlines 1))))
	       (ee-expand fname)))



;; This is trivial, it's just to make the hyperlinks look nicer.
;; (find-inkscape "$EEVDIR/doc/find-fline.svg")
;; (find-eimage0  "$EEVDIR/doc/find-fline.png" 2 3)

(defun find-inkscape (fname)
  (interactive "FSVG file name: ")
  (eev (format "cd %s && inkscape %s &"
	       (file-name-directory (expand-file-name fname))
	       (file-name-nondirectory fname))))


;;;;;
;;
;; hyperlinks to swf animations
;;
;;;;;

(defun ee-swf-html-embed (url width height)
  (format "<embed src=\"%s\"
       width=\"%d\" height=\"%d\" type=\"application/x-shockwave-flash\">"
	  url width height))

(defun ee-swf-html-full (url width height)
  (format "<html><head>
<title>%s</title></html></head>
<body>
%s
</body>
</html>
" url (ee-anim-html-embed url width height)))

(code-c-d "eevanim" "$EEVDIR/anim/")

(defun find-eevanim (fname width height)
  (let ((url (ee-eevanimfile fname)))
    (write-region (ee-swf-html-full url width height) nil "/tmp/ee.html")
    (find-sh0 "firefox /tmp/ee.html")))







;; Local Variables:
;; coding:            raw-text-unix
;; ee-anchor-format:  "«%s»"
;; ee-anchor-format:  "defun %s "
;; no-byte-compile:   t
;; End:
