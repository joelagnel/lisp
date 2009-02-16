;;; eev-math-glyphs.el -- mathematical glyphs for editing TeX texts in X
;;; See: (find-eev "eev-glyphs.el")
;;;      (find-eev "eev-compose.el")
;;;      (find-angg "vtutil4/mathchars.lua")
;;; To reload: (load-file "~/elisp/math-glyphs.el")
;;; Author and version: Edrx, 2005may28
;;; Public domain.

(require 'eev-glyphs)
(require 'eev-compose)
(if (not window-system)
    (error "eev-math-glyphs.el is for X only"))

;; Define several faces for glyphs in a few lines of code.
;; This is too rigid, but whatever.
;; (find-ecolors)
;;
(defun eev-glyphs-set-face (face fg bg)
  (make-face face)
  (set-face-foreground face fg)
  (set-face-background face bg))

(eev-glyphs-set-face 'eev-glyph-face-Greek   "orange"        "gray20")
(eev-glyphs-set-face 'eev-glyph-face-greek   "coral"         "gray20")
(eev-glyphs-set-face 'eev-glyph-face-logical "SteelBlue1"    "gray20")
(eev-glyphs-set-face 'eev-glyph-face-math    "RoyalBlue2"    "gray20")
(eev-glyphs-set-face 'eev-glyph-face-linear  "PaleVioletRed" "gray20")
(eev-glyphs-set-face 'eev-glyph-face-graphic "red"           "gray20")
(eev-glyphs-set-face 'eev-glyph-face-font    "gold"          "DarkOrange4")

;; Now defining glyphs can be as simple as this:
;; (eev-set-glyph ?Þ 332664 'eev-glyph-face-Greek)
;; but having many lines like that is not very convenient - it ignores
;; compose pairs and is too verbose...

;; Make adding new glyphs a reversible operation.
;;
(defvar eev-math-glyphs-standard-display-table-backup nil
  "A backup of standard-display-table (before adding user-defined glyphs)")
;;
(if (not eev-math-glyphs-standard-display-table-backup)
    (setq eev-math-glyphs-standard-display-table-backup
	  (copy-sequence standard-display-table)))

(defun eev-math-glyphs-reset ()
  "Restore the standard-display-table and empties eev-composes-localmath."
  (interactive)
  (setq standard-display-table
	(copy-sequence eev-math-glyphs-standard-display-table-backup))
  (setq eev-composes-localmath nil)
  (eev-composes-update))

;; A high-level way to set glyphs and compose pairs.

(defvar eev-math-glyphs-name-to-char
  '(("Theta"   . 332664)
    ("Pi"      . 332704)
    ("Sigma"   . 332707)
    ("Omega"   . 332713)
    ;;
    ("delta"   . 332724)
    ("epsilon" . 332725)
    ("theta"   . 332728)
    ("lambda"  . 343339)
    ("nu"      . 332733)
    ("rho"     . 332737)
    ("sigma"   . 332739)
    ("tau"     . 332740)
    ("omega"   . 343541)
    ;;
    ("top"     . 343268)
    ("bot"     . 343269)
    ("land"    . 343111)
    ("lor"     . 343112)
    ("supset"  . 343235)
    ("forall"  . 343072)
    ("exists"  . 343075)
    ("box"     . 299376)
    ("thin:"   . 343126)
    ("in"      . 343082)
    ("circ"    . 343096)
    ("cap"     . 343113)
    ("cup"     . 343114)
    ("Int"     . 343115)
    ("nabla"   . 343079)
    ("infty"   . 343102)
    ("ge"      . 343205)
    ("^1"      .    185)
    ("sqcap"   . 343251)
    ("ud&"     . 342827)
    ("oplus"   . 343253)
    ("otimes"  . 343259)
    ("to"      . 342898)
    ("dotli"   . 343375)
    ("nat"     . 299502)
    ("seblock" . 299223)
    ("neblock" . 299229)
    ("b"       .     ?b)
    ("r"       .     ?r)
    ("t"       .     ?t)
    ("s"       .     ?s))
  "An alist that translates char names\
 (for mathematical glyphs) to char codes.")

;; Inspect eev-math-glyphs-name-to-char:
' (progn (find-estring "")
         (mapc (lambda (p) (insert (format "%c %s\n" (cdr p) (car p))))
	       eev-math-glyphs-name-to-char))

(defun eev-math-glyphs-set (face names pairs chars &rest rest)
  "This is hard to explain, but there's an example at:...
    (find-efunction 'eev-math-glyphs-demo)"
  (setq names (split-string names " +"))
  (setq pairs (split-string pairs " +"))
  (setq chars (split-string chars " +"))
  (while names
    (let ((n (car names)) (p (car pairs)) (c (car chars)))
      (if (= 1 (length c))
	  (let ((c (string-to-char c)))
	    (eev-set-glyph c (ee-aref eev-math-glyphs-name-to-char n) face)
	    (setq eev-composes-localmath
		  (lax-plist-put eev-composes-localmath p c)))))
    (setq names (cdr names) pairs (cdr pairs) chars (cdr chars)))
  (if rest (apply 'eev-math-glyphs-set rest)
    (eev-composes-update)))

;; Demo:
(defun eev-math-glyphs-demo ()
  "Sets up a certain set of mathematical glyphs for TeX, with compose pairs."
  (eev-math-glyphs-set
   'eev-glyph-face-Greek
   "Theta Pi Sigma Omega"
   "Th    Pi Si    Om   "
   "Þ     å  Æ     Ø    "
   'eev-glyph-face-greek
   "delta epsilon theta lambda nu rho sigma tau omega"
   "dd    ee      te    ll     nu ro  ss    tt  ww   "
   "                 ð      Û             Ï    "
   'eev-glyph-face-logical
   "top bot land lor supset forall exists box thin: in circ"
   "TT  bo  la   lo  im     fa     ex     bo  ::    in oo  "
   "§   ®   ´       ¶      ý      Î      ñ   ¨     Ý  ¢   "
   'eev-glyph-face-math
   "cap cup Int nabla infty ge ^1"
   "ca  cu  In  na    88    >= -1"
   "Ì   þ   Å   ¿     ‚     ©  ³ "
   'eev-glyph-face-linear
   "sqcap ud& oplus otimes to"
   "ka    &&  o+    ox     -o"
   "     Ñ   ¥     ¤      ¸ "
   'eev-glyph-face-graphic
   "dotli nat seblock neblock"
   "..    bq  bl      ^^     "
   "÷     î   ­       £      "
   'eev-glyph-face-font
   "b  r  t  s "
   "bf rm tx ss"
   "¦  ¯  Ë  Ð "))

;; ÞåÆØðÛÏ
;; §®´¶ýÎñ¨Ý¢ÌþÅ¿‚©
;; Ñ¥¤¸÷î­£¦¯ËÐ

;; To set up a certain set of TeX glyphs:
;;   (eev-math-glyphs-demo)
;; To undo (i.e., to remove all local math glyphs):
;;   (eev-math-glyphs-reset)

(provide 'eev-math-glyphs)

;; Local Variables:
;; coding:               raw-text-unix
;; ee-anchor-format:     "«%s»"
;; End:
