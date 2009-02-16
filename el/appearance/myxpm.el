;; Create and preview xpms and favicons using emacs.
;; Copyleft 2005, Eduardo Ochs <edrx@mat.puc-rio.br>.
;; First version:   2005nov05.
;; Current version: 2005nov05.
;; Source:   <http://angg.twu.net/elisp/myxpm.el>
;; Htmlized: <http://angg.twu.net/elisp/myxpm.el.html>

;; (find-angg ".emacs" "find-xpm")
;; (find-fline "/usr/X11R6/include/X11/xpm.h")
;; http://www.w3.org/People/danield/xpm_story.html
;; (find-w3m "$S/http/www.w3.org/People/danield/xpm_story.html" "multi syntax:")

(defun myxpm-quote (str) (format "\"%s\"" str))
(defun myxpm-strings (strs) (mapconcat 'myxpm-quote strs ",\n"))
(defun myxpm-block (ttl strs) (format "/* %s */\n%s" ttl (myxpm-strings strs)))
(defun myxpm-3blocks (varname numbers colors grid)
  (if (listp numbers) (setq numbers (apply 'format "%d %d %d %d" numbers)))
  (format "/* XPM */\nstatic char *%s[] = {\n%s,\n%s,\n%s\n};\n"
	  (or varname "noname")
	  (myxpm-block "width height ncolors chars_per_pixel" (list numbers))
	  (myxpm-block "colors" colors)
	  (myxpm-block "pixels" grid)))

(defun myxpm-ccode (varname colors grid)
  (myxpm-3blocks
   varname (list (length (car grid)) (length grid) (length colors) 1)
   colors grid))

(defun myxpm-xtimes (xscale str)
  (mapconcat (lambda (c) (concat (make-list xscale c))) str ""))
(defun myxpm-ytimes (yscale strs)
  (apply 'append (mapcar (lambda (str) (make-list yscale str)) strs)))
(defun myxpm-xytimes (xscale yscale grid)
  (myxpm-ytimes yscale (mapcar (lambda (str) (myxpm-xtimes xscale str)) grid)))

(defun find-myxpm (colors grid &optional convert-to)
  (ee-write-string (myxpm-ccode nil colors grid) "$EEVTMPDIR/ee.xpm")
  (find-fline "$EEVTMPDIR/ee.xpm")
  (if convert-to (find-sh0 (format "convert $EEVTMPDIR/ee.xpm %s" convert-to)))
  (list (length (car grid)) (length grid)))

(defun find-myxpmscaled (xscale yscale colors grid &optional convert-to)
  (find-myxpm colors (myxpm-xytimes xscale yscale grid) convert-to)
  (list (length (car grid)) (length grid)))

(defun find-myxpmscaled+ (colors grid &optional convert-to scale)
  (cond ((numberp scale) (find-myxpm colors (myxpm-xytimes scale scale grid)))
	(scale           (find-myxpm colors grid convert-to)) ; try to save
	(t               (find-myxpm colors grid)))	      ; no saving
  (list (length (car grid))
	(length grid)
	(cond ((numberp scale) `(scaled ,scale))
	      (scale `(save into ,convert-to)))))

;; Tests:
;; (myxpm-xtimes 3 "abcde")
;; (myxpm-ytimes 3 '("a" "bc"))
;; (myxpm-xytimes 2 3 '("a" "bc" "def"))
;; (setq myxpm-colors '(". c #ffbb00" "o c red" "- c lime green"))
;; (find-estring (myxpm-ccode nil myxpm-colors '("...." "..oo" "--oo")))
;; (find-myxpm           myxpm-colors '("...." "..oo" "--oo"))
;; (find-myxpmscaled 8 8 myxpm-colors '("...." "..oo" "--oo"))

;; (find-ecolors)
;; (find-sh "wish <(echo 'puts [tk_chooseColor]; exit')")

' (find-myxpmscaled+
    '("  c none"
      ". c olive drab"
      "O c #eeffe5")
    '("                "
      "............... "
      "............... "
      "............... "
      "............... "
      "............... "
      ".OO...OO..O...O "
      "O..O.O..O.O...O "
      "OOOO.OOOO..O.O. "
      "O....O.....O.O. "
      ".OO...OO....O.. "
      "............... "
      "............... "
      "............... "
      "............... "
      "............... ")
    "~/eev-current/eev-icon.png" t)

;; (eev "convert $EEVTMPDIR/ee.xpm /tmp/favicon.ico && Scp /tmp/favicon.ico angg.twu.net:slow_html/ && Scp /tmp/favicon.ico angg.twu.net:public_html/")
;; (eev "display $EEVTMPDIR/ee.xpm")
;; (eev "display /tmp/favicon.ico")

' (find-myxpmscaled+
    '("  c none"
      ". c #ffbb00"
      "O c #ff6600")
    '("                "
      "............... "
      "............... "
      "............... "
      "............... "
      "...OOO...OOO... "
      "...O.......O... "
      "...O.......O... "
      "...O.......O... "
      "...O.......O... "
      "...O.......O... "
      "...OOO...OOO... "
      "............... "
      "............... "
      "............... "
      "............... ")
    "~/blogme/blogme-icon.png" 4)

' (find-myxpmscaled+
    '(". c none"
      "  c black"
      "o c bisque"
      "4 c red")
    '("................"
      "                "
      "                "
      "                "
      "                "
      "   o o          "
      "   o o     4    "
      "  ooooo  4 4 4  "
      "   o o    444   "
      "  ooooo  4 4 4  "
      "   o o     4    "
      "   o o          "
      "                "
      "                "
      "                "
      "                ")
    "~/eev-current/eev-glyph-icon.png" 4)

' (find-myxpmscaled+
    '(". c none"
      "  c antiquewhite1"
      "a c sienna"
      "b c brown"
      "c c brown"
      "d c brown"
      "o c blue4")
    '(" b     o    aa  "
      " bb   ooooo   a "
      " b b o      aaa "
      " bbb  ooooo aaa "
      "       o        "
      " ooo        ooo "
      "  o          o  "
      "  o          o  "
      "  o          o  "
      " ooo        ooo "
      "  o          o  "
      "        o       "
      "  cc ooooo    d "
      " c        o  dd "
      " c   ooooo  d d "
      "  cc    o   ddd ")
    "~/dednat4/dednat4-icon.png" 4)

' (find-myxpmscaled+
    '(". c none"
      "  c white"
      "o c black")
    '("                "
      "                "
      "                "
      "             o  "
      "                "
      " o oo o oo   o  "
      " oo   oo     o  "
      " o    o      o  "
      " o    o      o  "
      " o    o      o  "
      " o    o   o  o  "
      "           oo   "
      "                "
      "                "
      "                "
      "                ")
    "~/IMAGES/rrj.png" 4)





;;
;; Local Variables:
;; coding:               raw-text-unix
;; ee-delimiter-hash:    "\n#\n"
;; ee-delimiter-percent: "\n%\n"
;; ee-anchor-format:     "«%s»"
;; ee-comment-prefix:    ";;"
;; End:
