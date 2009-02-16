;;; font-latex.el --- LaTeX fontification for Font Lock mode.

;; Copyright (C) 1996-2001 Peter S. Galbraith
 
;; Authors:    Peter S. Galbraith <GalbraithP@dfo-mpo.gc.ca>
;;                                <psg@debian.org>
;;             Simon Marshall <Simon.Marshall@esrin.esa.it>
;; Maintainer: Peter S. Galbraith <GalbraithP@dfo-mpo.gc.ca>
;;                                <psg@debian.org>
;; Created:    06 July 1996
;; Version:    0.800 (01 November 2001)
;; Keywords:   LaTeX faces

;; RCS $Id: font-latex.el,v 5.15 2002/12/12 00:23:06 psg Exp $
;; Note: RCS version number does not correspond to release number.

;;; This file is not part of GNU Emacs.

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;  This package enhances font-lock fontification patterns for LaTeX.

;; New versions of this package (if they exist) may be found at:
;;  http://people.debian.org/~psg/elisp/font-latex.el
;; or in auc-tex's CVS archive.

;; ** Infinite loops !? **
;;  If you get an infinite loop, send me a bug report!
;;  Then set the following in your ~/.emacs file to keep on working:
;;   (setq font-latex-do-multi-line nil)

;; Description:
;;  This package enhances font-lock fontification patterns for LaTeX.
;;  font-lock mode is a minor mode that causes your comments to be
;;  displayed in one face, strings in another, reserved words in another,
;;  and so on.
;;
;;  Please see the accompanying file font-latex.tex for a demo of what
;;  font-latex is supposed to do at different fontification levels.

;; Installation instructions:
;;
;;  AUC-TeX users:  <URL:http://www.nongnu.org/auctex/>
;;   You don't have to do anything special as it gets installed
;;   along with the rest of AUC-TeX and gets enabled by default via the
;;   customizable variable TeX-install-font-lock.
;;
;;  Other users:
;;   You should byte-compile font-latex.el (It runs faster when you
;;   byte-compile it!) :
;;     M-x byte-compile-file
;;   and put the resulting font-latex.elc file in a directory listed in your
;;   emacs load-path.  You may then enable it by adding this form to your
;;   ~/.emacs file:
;;     (if window-system
;;         (require 'font-latex))
;;
;; Turning on font-latex:
;;
;;  After font-latex is loaded (or `required'), it will be automatically
;;  used whenever you enter `font-lock-mode' on a LaTeX buffer.  This
;;  fontification is done automatically in recent versions of Emacs and
;;  XEmacs, e.g. via a toggle switch in the menu-bar's Option menu, or by
;;  customizing the variable global-font-lock-mode in Emacs:
;;    M-x customize-variable RET global-font-lock-mode RET
;;
;; Fontification Levels:
;;
;;  There are two levels of fontification, selected by the value of the
;;  font-lock variable font-lock-maximum-decoration.  There are ways
;;  documented in font-latex.el to set this differently for each mode that
;;  uses font-lock, but if you are unsure and are running on a fast enough
;;  machine, try putting this in your ~/.emacs file: 
;;    (setq font-lock-maximum-decoration t) 
;;  It probably best to put it before the `(require 'font-latex)' statement
;;  if you use that.
;;
;; Changing colours
;;
;;  Okay, so you hate the colours I picked.  How do you change them you ask?
;;  First, find the font name to change using the command:
;;    M-x list-text-properties-at
;;  Then, suppose you got `font-latex-math-face', edit ~/.Xdefaults and add:
;;    Emacs.font-latex-math-face.attributeForeground: blue
;;  without the semi-colon I'm using here ascomment delimiters, of course.
;;
;; ----------------------------------------------------------------------------
;;; Change log:
;; V0.801 07Dec02 David Kastrup
;; - (font-latex-setup): Better stab at verbatim handling.
;; V0.800 01Nov01 PSG
;;  - Added font-lock-syntactic-keywords to font-lock-defaults to handle
;;    verbatim environment, as suggested by Stefan Monnier 5 years ago (!)
;; V0.702 15Oct01 PSG
;;  - remove LaTeX-mode-hook self-installation, since auc-tex can now install
;;    font-latex by itself. 
;;  - cleanup the docs a bit, deleting stuff relevant only for emacs19
;;    since it's now more likely to confuse users.
;; V0.701 30Mar00 Stefan Monnier <monnier@rum.cs.yale.edu> (RCS V1.63)
;;    Removed tests against specific versions of Emacs, testing for 
;;    functions instead.
;; V0.700 20Dec99 PSG (RCS V1.62)
;;    Added customize support.
;; V0.603 02July98 PSG (RCS V1.61)
;;    Squashed another infinite loop.
;; V0.602 02July98 PSG (RCS V1.60)
;;    Added 'font and 'infont keywords to narrow cache triggers.
;; V0.601 02July98 PSG (RCS V1.59)
;;    Added new font-latex-find-matching-close function to replace scan-sexp.
;;    It now searches for matching {} or [] when scan-sexp fails.
;; V0.600 16June98 PSG (RCS V1.58)
;;    Rewrote the cache method again.
;; V0.512 07Apr98 Stephen R. Anderson <sra@bloch.ling.yale.edu> (RCS V1.57)
;;    xemacs beta 20.5 sets the major version to 21.
;; V0.511 07Apr98 PSG (RCS V1.55)
;;    {\bf ...} multi-line cache related infinite loop fixed.
;; V0.510 19Mar98 PSG (RCS V1.54)
;;    More multi-line cache related infinite loops fixed.
;; V0.509 20Feb98 PSG (RCS V1.53)
;;    XEmacs infinite loop in font-latex-match-font-inside-braces cache.
;; V0.508 06Feb98 PSG (RCS V1.51)
;;    Created font-latex-match-textual; changed font-latex-math-face colour.
;; V0.507 30Jan98 PSG (RCS V1.50)
;;    Removed font-latex-find-matching-close because it broke the cache.
;;    Rewrote the cache method.  Ouch!
;; V0.506 08Jan98 PSG (RCS V1.48)
;;    Added variables font-latex-match-variable, font-latex-match-function
;;    font-latex-match-reference (built using reexp-opt).
;; V0.505 07Jan98 PSG (RCS V1.47)
;;    XEmacs20 has defface.
;; V0.504 20Oct97 Kevin Ruland <kruland@seistl.com> (RCS V1.46)
;;    Fixed the real bug in font-latex-match-command-outside-arguments
;; V0.503 16Oct97 PSG (RCS V1.45)
;;    Patched font-latex-match-command-outside-arguments for allow for
;;    strange interaction with AUC-TeX's LaTeX-environment command.
;; V0.502 07Oct97 (RCS V1.44)
;;    Kevin Ruland <kevin@rodin.wustl.edu> edits font-latex-find-matching-close
;;    PSG: Changed OliveGreen for OliveDrab, found in rgb.txt
;; V0.501 24Sep97 (RCS V1.42)
;;    Kevin Ruland <kevin@rodin.wustl.edu> added font-latex-find-matching-close
;;    used instead of scan-sexp to find arguments containing extra brackets.
;; V0.500 23Sep97 PSG (RCS V1.41)
;;  - Support for Emacs-20 (No customize support yet)
;; V0.403 19Nov96 (RCS V1.37)
;;  - Christoph Wedler <wedler@fmi.uni-passau.de>
;;    XEmacs patch for local math-font 
;;  - Changed scheme for fontification of \section*{...}  
;; V0.402 13Nov96 PSG (RCS V1.35)
;;  - Embeded comments handled.
;;  - Better XEmacs initilisation.
;; V0.401 12Nov96 PSG (RCS V1.34) - Nothing fontified when commented-out. 
;; V0.400 11Nov96 PSG (RCS V1.33) 
;;  - Stab at on-the-fly multiline.
;;  - mono support: <Johannes.Weinert@Informatik.Uni-Oldenburg.DE>
;; V0.314 16Oct96 PSG - Support for dark background removed for XEmacs.
;; V0.313 07Oct96 PSG (RCS V1.31) - Support for dark background.
;; V0.312 26Aug96 PSG (RCS V1.30) - Added font-latex-commented-outp.
;; V0.311 22Aug96 PSG (RCS V1.29) - fixed for XEmacs.
;; V0.310 22Aug96 simon (RCS V1.27)
;;  - make font-latex-setup run font-lock-make-faces before variable trickery.
;;  - set font-latex-string-face to the global value of font-lock-string-face.
;; V0.309 21Aug96 PSG (RCS V1.26)
;;  - new font-latex-math-face done by string syntax.  User may modify it.
;;  - new font-latex-string-face.
;; V0.308 15Aug96 PSG (RCS V1.25) 
;;  - $$...$$ gets font-latex-math-face
;;  - font-latex-match-math-envII fixed.
;; V0.307 14Aug96 PSG (RCS V1.23) - setup okay if loaded in a latex-mode-hook
;; V0.306 14Aug96 PSG (RCS V1.22) - added "item" to font-latex-match-function
;; V0.305 14Aug96 PSG (RCS V1.20) - use keep in font-latex-match-math-envII
;; V0.304 14Aug96 PSG (RCS V1.18) - minor comment edits.
;; V0.303 14Aug96 simon (RCS V1.17)
;;  - rewrote font-latex-match-math-envII like font-latex-match-quotation
;; V0.302 12Aug96 PSG (RCS V1.16)
;;  - (goto-char end) in condition-case error to avoid infinite loops.
;; V0.301 08Aug96 PSG (RCS V1.14)
;;  - Better faces in XEmacs.
;; V0.300 07Aug96 PSG (RCS V1.12)
;;  - Changed font-latex-match-font-inside-braces again for stranded \bf
;;  - "[a-z]+box" changed
;;  - font-latex-match-math-env checks preceding-char for \\[
;;  - use eval-after-compile in font-latex-match-math-envII 
;; V0.201 05Aug96 PSG added \\(display\\)?math to Simon's changes 
;; V0.200 05Aug96 simon: (RCS V1.10)
;;  - fixed font-latex-match-command-outside-arguments
;;  - rewrote font-latex-match-font-outside-braces like above
;;  - rewrote font-latex-match-font-inside-braces like above
;; V0.101 01Aug96 PSG added \\(display\\)?math
;; V0.100 01Aug96 PSG - massive new test version
;; V0.061 23Jul96 PSG
;;  - Removed trailing "\\>" in warning-face regexp (fails with \\ \- \\*)
;; V0.06  23Jul96 PSG
;;  - fixed dobib in font-latex-labels.
;;  - shorter font regexp in levels 3+4.
;;  - removed \item and & from type
;;  - fixed font-latex-math-envII regexp
;; V0.05  22Jul96 PSG
;;  - changed \ref etc to reference-face.
;;  - \\b added in buggy \item[option] regexp (not really fixed).
;;  - font-latex-labels regexp bug
;; V0.041  simon:
;;  - added font-latex-match-command-outside-arguments
;;  - rewrote font-latex-match-quotation and font-latex-bib-highlight-mouse
;;  - rewrote then removed bib-cite functionality.
;;  - general top-level cleanup
;; V0.04 11Jul96 PSG
;;  - added font-lock-comment-start-regexp defined in 19.32
;;  - encoded 8-bit characters to 7-bit.
;; V0.03 10Jul96 PSG
;;  - font-latex-bib-cite-mouse-highlight-p can change after font-lock-defaults
;;    is constructed.
;; V0.02 09Jul96 PSG 
;;  - added font-latex-bib-cite-mouse-highlight-p
;;  - Fixed `overwrite' flags
;; V0.01 06Jul96 Peter S Galbraith - Created
;; ----------------------------------------------------------------------------
;;; Code:
(require 'font-lock)

(cond
 ((fboundp 'defcustom)
  (defgroup font-latex nil
    "Font-latex text highlighting package."
    :prefix "font-latex-"
    :group 'faces
    :group 'tex)
  (defgroup font-latex-highlighting-faces nil
    "Faces for highlighting text in font-latex."
    :prefix "font-latex-"
    :group 'font-latex)
  (defcustom font-latex-do-multi-line t
    "Nil means disable the multi-line fontification prone to infinite loops."
    :group 'font-latex
    :type 'boolean))
 (t
  (defvar font-latex-do-multi-line t
    "*Set this to nil to disable the multi-line fontification 
prone to infinite loop bugs.")))

(defvar font-latex-warning-face			'font-latex-warning-face
  "Face to use for LaTeX major keywords.")
(defvar font-latex-sedate-face			'font-latex-sedate-face
  "Face to use for LaTeX minor keywords.")
(defvar font-latex-italic-face			'font-latex-italic-face
  "Face to use for LaTeX italics.")
(defvar font-latex-bold-face			'font-latex-bold-face
  "Face to use for LaTeX bolds.")
(defvar font-latex-math-face			'font-latex-math-face
  "Face to use for LaTeX math environments.")

(defvar font-latex-match-variable
  (concat 
   "\\\\" "\\("
;;;(regexp-opt
;;; '("setlength" "settowidth" "setcounter" "addtolength" "addtocounter"))
   "addto\\(counter\\|length\\)\\|set\\(counter\\|length\\|towidth\\)"
   "\\)\\>")
  "font-latex regexp to match LaTeX variable keywords.")

(defvar font-latex-match-reference
  (concat 
   "\\\\" "\\("
;;;(regexp-opt
;;; '("nocite" "cite" "label" "pageref" "vref" "eqref" "ref"
;;;   "include" "input" "bibliography"
;;;   "index" "glossary" "footnote" "footnotemark" "footnotetext"))
   "bibliography\\|cite[a-zA-Z]*\\|eqref\\|footnote\\(mark\\|text\\)?\\|"
   "glossary\\|in\\(clude\\|dex\\|put\\)\\|label\\|nocite\\|pageref\\|ref\\|"
   "vref"
   "\\)\\>")
  "font-latex regexp to match reference keywords.")
  
(defvar font-latex-match-function
  (concat 
   "\\\\" "\\("
;;;(regexp-opt
;;; '("begin" "end"
;;;   "pagenumbering"
;;;   "thispagestyle" "pagestyle"
;;;   "nofiles" "includeonly"
;;;   "bibliographystyle" "documentstyle" "documentclass"
;;;   "newenvironment" "newcommand" "newlength" "newtheorem" "newcounter"
;;;   "renewenvironment" "renewcommand" "renewlength" "renewtheorem" 
;;;   "renewcounter"
;;;   "usepackage" "fbox" "mbox" "sbox" "vspace" "hspace"))
   "b\\(egin\\|ibliographystyle\\)\\|document\\(class\\|style\\)\\|"
   "end\\|fbox\\|hspace\\|includeonly\\|mbox\\|"
   "n\\(ew\\(co\\(mmand\\|unter\\)\\|environment\\|length\\|theorem\\)"
       "\\|ofiles\\)\\|"
    "page\\(numbering\\|style\\)\\|"
    "renew\\(co\\(mmand\\|unter\\)\\|environment\\|length\\|theorem\\)\\|"
    "sbox\\|thispagestyle\\|usepackage\\|vspace"
   "\\)\\>")
  "font-latex regexp to match LaTeX function keywords.")

(defvar font-latex-match-textual
  (concat 
   "\\\\" "\\("
;;;(regexp-opt
;;; '("item" ;;;FIXME: does not have an {arg} so should treated elsewhere.
;;;   "part" "chapter" "section" "subsection" "subsubsection" 
;;;   "paragraph" "subparagraph" "subsubparagraph" 
;;;   "title" "author" "date" "thanks" "address"
;;;   "caption"))
   "a\\(ddress\\|uthor\\)\\|c\\(aption\\|hapter\\)\\|date\\|item\\|"
   "par\\(agraph\\|t\\)\\|"
   "s\\(ection\\|"
       "ub\\(paragraph\\|s\\(ection\\|ub\\(paragraph\\|section\\)\\)\\)\\)\\|"
   "t\\(hanks\\|itle\\)"
   "\\)\\>")
  "font-latex regexp to match LaTeX function with text argument.")


(defvar font-latex-keywords-1
  (list
   ;; FIXME: Maybe I should put this in a function, use override but let
   ;;        the function determine if commented-out.
   (list (concat 
          "\\\\" "\\("
;;; (regexp-opt
;;;  '("nopagebreak" "pagebreak" "newpage" "clearpage" "cleardoublepage"
;;;    "enlargethispage" "nolinebreak" "linebreak" "newline"
;;;    "-" "\\" "\\*" "displaybreak" "allowdisplaybreaks"))
          "\\\\\\*\\|allowdisplaybreaks\\|clear\\(doublepage\\|page\\)\\|"
          "displaybreak\\|enlargethispage\\|linebreak\\|"
          "n\\(ew\\(line\\|page\\)\\|o\\(linebreak\\|pagebreak\\)\\)\\|"
          "pagebreak\\|[\\-]"
          
          "\\)")
	 '(0 font-latex-warning-face))
   '("\\$\\$\\([^$]+\\)\\$\\$" 1 font-latex-math-face)        ;;; $$...$$
   '(font-latex-match-quotation . font-latex-string-face)     ;;; ``...''
   '(font-latex-match-font-outside-braces		      ;;;\textit{text}
     (0 font-lock-keyword-face
        append                         ;Override? [t 'keep 'prepend 'append]
        ;; Can't use prepend because that overwrites syntax fontification
        ;; e.g. comments.
        t)                              ;Laxmatch? if t, do not signal error
     (1 font-latex-italic-face append t)
     (2 font-latex-bold-face append t)
     (3 font-lock-type-face append t))
   '(font-latex-match-font-inside-braces		      ;;;{\it text}
     (0 font-lock-keyword-face append t)
     (1 font-latex-italic-face append t)
     (2 font-latex-bold-face append t)
     (3 font-lock-type-face append t)))
  "Subdued level highlighting for LaTeX modes.")

(defvar font-latex-keywords-2
  (append font-latex-keywords-1
   '((font-latex-match-reference                              ;;;\cite
      (0 font-lock-keyword-face append t)
      (1 font-lock-variable-name-face append t)              ;;;    [opt]
      (2 font-lock-reference-face append t))                 ;;;         {key}
     (font-latex-match-function                               ;;;\documentclass
      (0 font-lock-keyword-face append t)
      (1 font-lock-variable-name-face append t)              ;;;   [opt]
      (2 font-lock-function-name-face append t))             ;;;        {text}
     (font-latex-match-textual                               ;;;\section
      (0 font-lock-keyword-face append t)
      (1 font-lock-variable-name-face append t)              ;;;   [opt]
      (2 font-lock-type-face append t))                      ;;;        {text}
     (font-latex-match-variable
      (0 font-lock-keyword-face nil t)
      (1 font-lock-variable-name-face append t)
      (2 font-lock-variable-name-face append t))
     (font-latex-match-math-env 
      (0 font-latex-math-face append t))         	      ;;;\(...\)
     (font-latex-match-math-envII                             ;;;Math environ.
      (0 font-latex-math-face append t))
     ("\\\\[@A-Za-z]+"                                        ;;;Other commands
      (0 font-latex-sedate-face append))))
  "High level highlighting for LaTeX modes.")

(defvar font-latex-keywords font-latex-keywords-1
  "Default expressions to highlight in TeX mode.")

;; End-User can stop reading here.

(defvar font-latex-string-face nil
  "Face to use for strings.  This is set by Font LaTeX.")

(defvar font-lock-comment-start-regexp nil
  "Regexp to match the start of a comment.")

(eval-when-compile
  (require 'cl))

(cond
 ((fboundp 'defface)
  (defface font-latex-bold-face
    '((((class grayscale) (background light)) (:foreground "DimGray" :bold t))
      (((class grayscale) (background dark)) (:foreground "LightGray" :bold t))
      (((class color) (background light)) 
       (:foreground "DarkOliveGreen" :bold t ))
      (((class color) (background dark)) (:foreground "OliveDrab" :bold t ))
      (t (:bold t)))
    "Font Lock mode face used to bold LaTeX."
    :group 'font-latex-highlighting-faces)
  
  (defface font-latex-italic-face
    '((((class grayscale) (background light)) 
       (:foreground "DimGray" :italic t))
      (((class grayscale) (background dark)) 
       (:foreground "LightGray" :italic t))
      (((class color) (background light)) 
       (:foreground "DarkOliveGreen" :italic t ))
      (((class color) (background dark)) 
       (:foreground "OliveDrab" :italic t ))
      (t (:italic t)))
    "Font Lock mode face used to highlight italic LaTeX."
    :group 'font-latex-highlighting-faces)

  (defface font-latex-math-face
    '((((class grayscale) (background light)) 
       (:foreground "DimGray" :underline t))
      (((class grayscale) (background dark)) 
       (:foreground "LightGray" :underline t))
      (((class color) (background light)) (:foreground "SaddleBrown"))
      (((class color) (background dark))  (:foreground "burlywood"))
      (t (:underline t)))
    "Font Lock mode face used to highlight math in LaTeX."
    :group 'font-latex-highlighting-faces)

  (defface font-latex-sedate-face
    '((((class grayscale) (background light)) (:foreground "DimGray"))
      (((class grayscale) (background dark))  (:foreground "LightGray"))
      (((class color) (background light)) (:foreground "DimGray"))
      (((class color) (background dark))  (:foreground "LightGray"))
   ;;;(t (:underline t))
      )
    "Font Lock mode face used to highlight sedate stuff in LaTeX."
    :group 'font-latex-highlighting-faces)

  (copy-face 'font-lock-string-face 'font-latex-string-face)
  (if (facep 'font-lock-warning-face)
      (copy-face 'font-lock-warning-face 'font-latex-warning-face)
    (defface font-latex-warning-face
      '((((class grayscale)(background light))(:foreground "DimGray" :bold t))
        (((class grayscale)(background dark))(:foreground "LightGray" :bold t))
        (((class color)(background light))(:foreground "red" :bold t ))
        (((class color)(background dark))(:foreground "red" :bold t ))
        (t (:bold t)))
      "Font Lock for LaTeX major keywords."
      :group 'font-latex-highlighting-faces)))
 ((and (fboundp 'font-lock-make-faces) (boundp 'font-lock-face-attributes))
  (if (not font-lock-face-attributes)
      ;; Otherwise I overwrite fock-lock-face-attributes.
      (font-lock-make-faces))
  (unless (assq 'font-latex-sedate-face font-lock-face-attributes)
    (cond 
     ;; FIXME: Add better conditions for grayscale.
     ((memq font-lock-display-type '(mono monochrome grayscale greyscale
                                     grayshade greyshade))
      (setq font-lock-face-attributes
            (append 
             font-lock-face-attributes
             (list '(font-latex-bold-face nil nil t nil nil)
                   '(font-latex-italic-face nil nil nil t nil)
                   '(font-latex-math-face nil nil nil nil t)
                   '(font-latex-sedate-face nil nil nil t nil)
                   (list
                    'font-latex-warning-face
                    (cdr (assq 'background-color (frame-parameters)))
                    (cdr (assq 'foreground-color (frame-parameters)))
                    nil nil nil)))))
     ((eq font-lock-background-mode 'light) ; light colour background
      (setq font-lock-face-attributes
           (append 
            font-lock-face-attributes
                 ;;;FIXME: These won't follow font-lock-type-face's changes.
                 ;;;       Should I change to a (copy-face) scheme?
            '((font-latex-bold-face "DarkOliveGreen" nil t nil nil)
              (font-latex-italic-face "DarkOliveGreen" nil nil t nil)
              (font-latex-math-face "SaddleBrown")
              (font-latex-sedate-face "grey50")
              (font-latex-warning-face "red" nil t nil nil)))))
    (t			; dark colour background
     (setq font-lock-face-attributes
           (append 
            font-lock-face-attributes
            '((font-latex-bold-face "OliveDrab" nil t nil nil)
              (font-latex-italic-face "OliveDrab" nil nil t nil)
              (font-latex-math-face "burlywood")
	      ;; good are > LightSeaGreen, LightCoral, coral, orchid, orange
              (font-latex-sedate-face "grey60")
              (font-latex-warning-face "red" nil t nil nil))))))))
 (t
  ;;; XEmacs19:
  (make-face 'font-latex-string-face "Face to use for LaTeX string.")
  (copy-face 'font-lock-string-face 'font-latex-string-face)

  (make-face 'font-latex-bold-face "Face to use for LaTeX bolds.")
  (copy-face 'font-lock-type-face 'font-latex-bold-face)
  (make-face-bold 'font-latex-bold-face)

  (make-face 'font-latex-italic-face "Face to use for LaTeX italics.")
  (copy-face 'font-lock-type-face 'font-latex-italic-face)
  (make-face-italic 'font-latex-italic-face)

  (make-face 'font-latex-math-face "Face to use for LaTeX math.")
  (make-face 'font-latex-sedate-face "Face to use for LaTeX minor keywords.")
  (make-face 'font-latex-warning-face "Face to use for LaTeX major keywords.")
  (make-face-bold 'font-latex-warning-face)
  ;; XEmacs uses a tag-list thingy to determine if we are using color
  ;;  or mono (and I assume a dark background).
  (set-face-foreground 'font-latex-math-face "green4" 'global nil 'append)
  (set-face-foreground 'font-latex-sedate-face "grey50" 'global nil 'append)
  (set-face-foreground 'font-latex-warning-face "red" 'global nil 'append)))

;;;###autoload
(defun font-latex-setup ()
  "Setup this buffer for LaTeX font-lock.  Usually called from a hook."
  ;; Trickery to make $$ fontification be in `font-latex-math-face' while
  ;; strings get whatever `font-lock-string-face' has been set to.
  (cond
   ((fboundp 'built-in-face-specifiers)
    ;; Cool patch from Christoph Wedler...
    (let (instance)
      (mapcar (lambda (property)
		(setq instance
		      (face-property-instance 'font-latex-math-face property
					      nil 0 t))
		(if (numberp instance)
		    (setq instance
			  (face-property-instance 'default property nil 0)))
		(or (numberp instance)
		    (set-face-property 'font-lock-string-face property
				       instance (current-buffer))))
	      (built-in-face-specifiers))))
   (t
    (if (fboundp 'font-lock-make-faces) (font-lock-make-faces))
    (make-local-variable 'font-lock-string-face)
    (setq font-lock-string-face font-latex-math-face
	  font-latex-string-face (default-value 'font-lock-string-face))))

  ;; Tell Font Lock about the support.
  (make-local-variable 'font-lock-defaults)
  ;; Parentheses () disabled because they should not delimit fontification
  ;; in LaTeX text.
  (setq font-lock-defaults
	'((font-latex-keywords font-latex-keywords-1 font-latex-keywords-2)
	  nil nil ((?\( . ".") (?\) . ".") (?$ . "\"")) nil
	  (font-lock-comment-start-regexp . "%")
	  (font-lock-mark-block-function . mark-paragraph)
          (font-lock-syntactic-keywords 
           . (("^\\\\begin *{verbatim\\*?}\\(.?\\).*\\(\n\\)" (1 "<") (2 "|"))
	      ("\\(\n\\)\\\\end *{verbatim\\*?}\\(.?\\)" (1 "|") (2 "<"))
	      ("\\\\verb\\*?\\([^a-z@]\\).*?\\(\\1\\)" (1 "\"") (2 "\""))
	      ))
          )))

;; Should not be necessary since XEmacs' font-lock also supports
;; Emacs' use of the `font-lock-defaults' local variable.   -Stefan
;; (when (save-match-data (string-match "XEmacs\\|Lucid" emacs-version)))
;;     (put 'latex-mode 'font-lock-defaults
;;          '((font-latex-keywords font-latex-keywords-1 font-latex-keywords-2)
;;            nil nil ((?\( . ".") (?\) . ".") (?$ . "\"")) nil
;;            (font-lock-comment-start-regexp . "%")
;;            (font-lock-mark-block-function . mark-paragraph)))
;;     (put 'latex-tex-mode	'font-lock-defaults 'latex-mode)
;;     (put 'LaTex-tex-mode	'font-lock-defaults 'latex-mode)
;;     (put 'LaTeX-mode        'font-lock-defaults 'latex-mode)
;;     (put 'japanese-LaTeX-mode 'font-lock-defaults 'latex-mode)
;;     (put 'LATeX-MoDe	'font-lock-defaults 'latex-mode)
;;     (put 'lATEx-mODe	'font-lock-defaults 'latex-mode))


(defun font-latex-match-reference (limit)
  (if font-latex-match-reference
      (font-latex-match-command-outside-arguments font-latex-match-reference
;;;   (eval-when-compile
;;;     (concat "\\\\" "\\("
;;;             (mapconcat 'identity 
;;;              '("[A-Za-z]*cite[A-Za-z]*" "label" "\\(page\\|v\\|eq\\)?ref"
;;;                "index" "glossary" "\\(footnote\\(mark\\|text\\)?\\)")
;;;              "\\|")
;;;      "\\)\\>"))
                                                  limit nil nil)))

(defun font-latex-match-function (limit)
  "Fontify things like \\documentclass{article}"
  (if font-latex-match-function
      (font-latex-match-command-outside-arguments font-latex-match-function
                                                  limit nil t)))
(defun font-latex-match-textual (limit)
  "Fontify things like \\section{text}"
  (if font-latex-match-textual
      (font-latex-match-command-outside-arguments font-latex-match-textual
                                                  limit nil t)))
(defun font-latex-match-variable (limit)
  "Fontify things like \\newcommand{stuff}"
  (if font-latex-match-variable
      (font-latex-match-command-outside-arguments font-latex-match-variable
                                                  limit t nil)))

;; font-latex-find-matching-close is a little helper function which
;; is used like scan-sexp.  It skips over matching
;; pairs of '{' and '}'.  As an added benefit, it ignores any characters
;; which occur after the tex comment character %.
(defun font-latex-find-matching-close (openchar closechar)
  "Skip over matching pairs of { } or [ ], ignoring comments"
  (let ((parse-sexp-ignore-comments t) ; scan-sexps ignores comments
        (init-point (point))
        (status))
    (if (condition-case nil
            (goto-char (scan-sexps (point) 1))
          (error))
        ;; No error code.  See if closechar is quoted
        (if (save-excursion (backward-char 1) (= (preceding-char) ?\\))
            (setq status nil)
          (setq status t))
      ;; Terminated in error -- Try ourselves
      (setq status nil))
    (if status
        t
      (goto-char init-point)
      (let ((target)
            (mycount 1))
        (save-excursion
          (save-match-data
            (forward-char 1)
            (while (and (> mycount 0)
                        (progn
                          (re-search-forward
                           (concat "["
                                   ;; closechar might be ]
                                   ;; and therefor must be first in regexp
                                   (char-to-string closechar)
                                   (char-to-string openchar)
                                   "]")
                           nil t)))
              (cond 
               ((font-latex-commented-outp)
                (forward-line 1))
               ((save-excursion (backward-char 1) (= (preceding-char) ?\\))
                nil)
               (t
                (setq mycount (if (= (preceding-char) openchar)
                                  (+ mycount 1)
                                (- mycount 1))))))
            (setq target (point))
            (if (not (= mycount 0))
                nil)))
        (if (= mycount 0)
            (goto-char target))))))

;; FIXME: --About font-latex-commented-outp--
;; Fontification is *slower* for affected functions (in particular
;; font-latex-match-function), so it will be worth it to increase
;; performance in the algorithm.
;;  - don't return (store-match-data (list nil nil)) in
;;    font-latex-match-command-outside-arguments, instead skip over
;;    commented-out parts internally.  
;;  - Perhaps handling outlined code is excessive and slows down the 
;;    search too much?
;;  - Is save-match-data expensive? The calling function could store
;;    the match-data before it calls (font-latex-commented-outp) knowing
;;    that is would trash the list.
(defun font-latex-commented-outp ()
  "Return t is comment character is found between bol and point."
  (save-excursion
    (let ((limit (point)))
      (save-match-data
        ;; Handle outlined code
        (re-search-backward "^\\|\C-m" (point-min) t)
        (if (re-search-forward "^%\\|[^\\]%" limit t)
            t
          nil)))))

;;;;------------------
;;;; Cache Method:
;;;
;;; This works:
;;; 
;;; (defun font-latex-set-cache (cache-id)
;;;   (let ((cache (intern cache-id)))
;;;     (set cache (list (point) (point-max)))))
;;; (defun font-latex-get-cache (cache-id item)
;;;   (let ((cache (intern cache-id)))
;;;     (nth item (symbol-value cache))))
;;; (font-latex-set-cache "font-latex-match-command-cache")
;;; (font-latex-get-cache "font-latex-match-command-cache" 1)
;;;
;;; but let's use symbols instead:

;;; Hacker's note: I haven't tested extensively using lazy-lock, which
;;; apparently fontifies the entire visble page instead of just the current
;;; line.  This could actually be slower than not using lazy-lock using the
;;; current code.  Perhaps there's an opportunity to take advantage of
;;; lazy-lock with alternate coding.

;;; Hacker's note: If this method leads to infinite loops again, I could
;;; change the cache method to something like:
;;;  - When the pattern is un-finished, simply store the limit in the cache.
;;;    and the regexp to match the termination.
;;;  - When checking the cache, check to see if we're at the limit, and if
;;;    so fontify the text directly like at point limit-1 (instead of
;;;    letting font-lock itself set the font!) until either the regexp match
;;;    is found or set another cache at the new limit
;;;  - the scheme must allow a newline to be correctly fontified, and well
;;;    as new characters on the same line as the first cache.  (How?)

;;; Hacker's note (2001-11-02) : It's possible that the caching system is
;;; no longer needed using font-lock-multiline in Emacs21.  I should
;;; disable it and try.  Also, now that I look at this, I wonder why I
;;; didn't use text-properties to be able to set many unterminated
;;; fontification matches in a given buffer.  Perhaps it was portability to
;;; XEmacs?

(defun font-latex-set-cache (cache-id kbeg kend limit keywords match-list)
  "cache in symbol CACHE-ID the following info:
KBEG and KEND: beginning and end points of the LaTeX keyword (e.g. \"section\")
LIMIT:         up to where fontification is done.
KEYWORDS:      the font-lock regexp that initiated the cache.
MATCH LIST:    the match list that was returned to font-lock

The INITIAL POINT from which we last moved is stored in the same cache, but 
it's done elsewhere.  We will never fontify the same MATCH LIST twice in a 
row from same INITIAL POINT."
;debug  (message "Setting cache!")
  (let ((ini-point (nth 5 (symbol-value cache-id)))
        (oldlimit (nth 6 (symbol-value cache-id))))
    (set cache-id 
         (list kbeg kend limit keywords match-list ini-point oldlimit))))

(defun font-latex-get-cache (cache-id item)
"Retrieve info from cache in symbol CACHE-ID
 0: kbegin
 1: kend
 2: limit
 3: keywords
 4: match-list from last succesful cache
 5: initial point from which we last moved
 6: limit when we last moved"
  (let ((cache (symbol-value cache-id)))
    (nth item cache)))

(defun font-latex-check-cache (cache-id keywords limit)
  "Check that current parameters are consistent with cache to move point.
If we move point, alter the last entry in the cache to indicate from where 
we moved and the current limit.
Return t if we move, false if we don't."
  (let ((the-point (point))
        (kbeg (font-latex-get-cache cache-id 0))
        (inip (or (font-latex-get-cache cache-id 5) 0))
        (oldlimit (or (font-latex-get-cache cache-id 6) 0)))
    (when 
        (and 
         font-latex-do-multi-line
         kbeg                           ;; Check that cache is actually set
         (equal keywords (font-latex-get-cache cache-id 3))
;debug   (message "1- cache: %s" (symbol-name cache-id))
;debug   (message "1- keywords are the same; next compare point %s to %s" 
;debug            the-point (font-latex-get-cache cache-id 1))
         (not (= the-point (font-latex-get-cache cache-id 1)))
;debug   (message "2- Not on end of keyword %s != %s; next after kbeg %s" 
;debug            the-point (font-latex-get-cache cache-id 1) kbeg)
         (< kbeg the-point)
;debug   (message "3- After beginning of keyword at %s; next within limit %s"
;debug            kbeg (font-latex-get-cache cache-id 2))
         (<= the-point (font-latex-get-cache cache-id 2))
;debug   (message "4- Within limit at %s" (font-latex-get-cache cache-id 2))
;debug   (message "5- Same limit as last time?: %s vs %s  Point greater? %s > %s" 
;debug            limit oldlimit the-point inip)
         (or (< the-point inip) (not (= limit oldlimit)))
;debug   (message "6- Is %s on same line as %s?" the-point kbeg)
         (font-latex-not-on-same-line-as kbeg))
;debug   (message "7- moving from %s to %s!" the-point kbeg)
      (goto-char kbeg)
      (let* ((cache (symbol-value cache-id))
             (e0 kbeg)
             (e1 (nth 1 cache))
             (e2 (nth 2 cache))
             (e3 (nth 3 cache))
             (e4 (nth 4 cache)))
        (set cache-id (list e0 e1 e2 e3 e4 the-point limit)))
      t)))

;;;;-----

(defvar font-latex-match-command-cache nil
  "Cache for font-latex-match-command")
(make-variable-buffer-local 'font-latex-match-command-cache)

;; FIXME - Note to myself 
;; In call to font-latex-match-command-outside-arguments, I could arrange
;; such that keywords which cannot use [options] have this set to nil.
;; LaTeX code wouldn't fontify if options are used illegally in commands,
;; cuing users in that they are doing something wrong.  (See RCS V1.11 for
;; useopt option)
;;
;; NOTE - Without an override flag, font-lock does not re-fontify the
;;  option `opt' when the `t' is typed-in in "\cite[opt".  The first `o'
;;  was fontified and now has a face, which font-lock-apply-highlight
;;  won't override.  The `p' and `t' get a face as they are typed by 
;;  inheriting from left-stickyness on the `o'.
;;  THEREFORE, I cannot rely on font-lock-apply-highlight to continue 
;;  multi-line incomplete patterns, because the first character of the 
;;  pattern on the first line has a face.  I must use `prepend'.
(defun font-latex-match-command-outside-arguments (keywords limit twoargs 
                                                   asterix)
  "Search for regexp command KEYWORDS[opt]{arg} before LIMIT.
If TWOARG is t, allow two arguments {arg1}{arg2}
If ASTERIX is t, fontify trailing asterix in command.
Sets `match-data' so that:
 subexpression 0 is the keyword, 
 subexpression 1 is the contents of any following [...] forms 
 subexpression 2 is the contents of any following {...} forms.  
Returns nil if none of KEYWORDS is found."
  (let ((we-moved (font-latex-check-cache 
                   'font-latex-match-command-cache keywords limit)))
    (when (re-search-forward keywords limit t)
      (cond
       ((font-latex-commented-outp)
        ;; Return a nul match such that we skip over this pattern.
        ;; (Would be better to skip over internally to this function)
        (store-match-data (list nil nil))
        t)
       (t
        (let ((kbeg (match-beginning 0)) 
              kend sbeg send cbeg cend
              cache-reset
              (parse-sexp-ignore-comments t)) ; scan-sexps ignores comments
          (goto-char (match-end 0))
          (if (and asterix (eq (following-char) ?\*))
              (forward-char 1)) 
          (skip-chars-forward " \n\t" limit)
          (setq kend (point))
          (while (eq (following-char) ?\[)
            (setq sbeg kend)
            (save-restriction
              ;; Restrict to LIMIT.
              (narrow-to-region (point-min) limit)
              (if (font-latex-find-matching-close ?\[ ?\])
                  (setq send (point))
                (setq cache-reset t)
                (setq send (point-max))
                (goto-char send))))
          (skip-chars-forward " \n\t" limit)
          (when (eq (following-char) ?\{)
            (setq cbeg (point))
            (save-restriction
              ;; Restrict to LIMIT.
              (narrow-to-region (point-min) limit)
              (if (font-latex-find-matching-close ?\{ ?\})
                  (setq cend (point))
                (setq cache-reset t)
                (setq cend (point-max))
                (goto-char cend))))
          (when twoargs
            (skip-chars-forward " \n\t" limit)
            (when (eq (following-char) ?\{)
              (save-restriction
                ;; Restrict to LIMIT.
                (narrow-to-region (point-min) limit)
                (if (font-latex-find-matching-close ?\{ ?\})
                    (setq cend (point))
                  (setq cache-reset t)
                  (setq cend (point-max))
                  (goto-char cend)))))
          (store-match-data (list kbeg kend sbeg send cbeg cend))
          
          ;; Handle cache
;          (if (and we-moved
;                   (equal (list kbeg kend sbeg send cbeg cend)
;                          (font-latex-get-cache 
;                           'font-latex-match-command-cache 4)))
;              (progn
;                (message "pattern cancelled... twice in a row")
;                nil) ;; Return a nul search (cancel this fontification)

          (when (and font-latex-do-multi-line cache-reset)
            (font-latex-set-cache 
             'font-latex-match-command-cache 
             kbeg kend limit keywords (list kbeg kend sbeg send cbeg cend)))
          t))))))

(defvar font-latex-match-font-cache nil
  "Cache start of unterminated LaTeX font-changing commands to fontify")
(make-variable-buffer-local 'font-latex-match-font-cache)

(defun font-latex-match-font-outside-braces (limit)
  "Search for font-changing command like \textbf{fubar} before LIMIT.  
Sets `match-data' so that:
 subexpression 0 is the keyword, 
 subexpression 1 is the content to fontify in italic.
 subexpression 2 is the content to fontify in bold.
 subexpression 3 is the content to fontify in type-face.
Returns nil if no font-changing command is found."
  (font-latex-check-cache 'font-latex-match-font-cache 'font limit)
  (when (re-search-forward
         (eval-when-compile
           (concat "\\\\" "\\("
                   "\\(emph\\)\\|"			      ;;; 2 - italic
                   "\\(text\\("
                               "\\(it\\|sl\\)\\|"	      ;;; 5 - italic
                               "\\(md\\|rm\\|sf\\|tt\\)\\|" ;;; 6 - type
                               "\\(bf\\|sc\\|up\\)"	      ;;; 7 - bold
                          "\\)\\)\\|"
                   "\\(boldsymbol\\|pmb\\)"		      ;;; 8 - bold
                   "\\)" "{"))
         limit t)
    (cond
     ((font-latex-commented-outp)
      ;; Return a nul match such that we skip over this pattern.
      ;; (Would be better to skip over internally to this function)
      ;; Using `prepend' won't help here, because the problem is that
      ;; scan-sexp *fails* to find a commented-out matching bracket!
      (store-match-data (list nil nil))
      t)
     (t
      (let ((kbeg (match-beginning 0)) (kend (match-end 1)) 
            (beg  (1- (match-end 0)))   ;Include openning bracket
            end itbeg itend bfbeg bfend ttbeg ttend
            (parse-sexp-ignore-comments t) ; scan-sexps ignores comments
            cache-reset)
        (goto-char kend)
        (save-restriction
          ;; Restrict to LIMIT.
          (narrow-to-region (point-min) limit)
          (if (font-latex-find-matching-close ?\{ ?\})
              (setq end (point))
            (setq cache-reset t)
            (setq end (point-max))
            (goto-char end)))
        (cond ((or (match-beginning 2) (match-beginning 5))
               (setq itbeg beg  itend end))
              ((match-beginning 6)
               (setq ttbeg beg  ttend end))
              (t
               (setq bfbeg beg  bfend end)))
        (store-match-data 
         (list kbeg kend itbeg itend bfbeg bfend ttbeg ttend))
        ;; Start the subsequent search immediately after this keyword.
          (goto-char kend)
          
        (when (and font-latex-do-multi-line cache-reset)
          (goto-char limit)             ;Avoid infinite loops?
          (font-latex-set-cache 
           'font-latex-match-font-cache 
           kbeg kend limit 'font 
           (list kbeg kend itbeg itend bfbeg bfend ttbeg ttend)))
        
        t)))))
  
(defvar font-latex-match-infont-cache nil
  "Cache start of unterminated LaTeX font-changing commands to fontify")
(make-variable-buffer-local 'font-latex-match-infont-cache)

(defun font-latex-match-font-inside-braces (limit)
  "Search for font-changing command like {\bf fubar} before LIMIT.  
Sets `match-data' so that:
 subexpression 0 is the keyword. 
 subexpression 1 is the content to fontify in italic.
 subexpression 2 is the content to fontify in bold.
 subexpression 3 is the content to fontify in type-face.
Returns nil if no font-changing command is found."
  (font-latex-check-cache 'font-latex-match-infont-cache 'infont limit)
  (when (re-search-forward
         (eval-when-compile
           (concat "\\\\" "\\("
                                                              ;;; 2 - italic
                   "\\(em\\|it\\(shape\\)?\\|sl\\(shape\\)?\\)\\|"
	                                                      ;;; 5 - bold
                   "\\(bf\\(series\\)?\\|upshape\\|sc\\(shape\\)?\\)\\|"
                   "mdseries\\|tt\\(family\\)?\\|"
                   "sf\\(family\\)?\\|rm\\(family\\)?\\|"
                   "tiny\\|scriptsize\\|footnotesize\\|"
                   "small\\|normalsize\\|large\\|Large\\|LARGE\\|huge\\|Huge"
                   "\\)\\>[ \t]*"))
         limit t)
    (cond
     ((font-latex-commented-outp)
      ;; Return a nul match such that we skip over this pattern.
      ;; (Would be better to skip over internally to this function)
      ;; Using `prepend' won't help here, because the problem is that
      ;; scan-sexp *fails* to find a commented-out matching bracket!
      (store-match-data (list nil nil))
      t)
     (t
      (let ((kbeg (match-beginning 0)) (kend (match-end 1)) 
            (beg  (match-end 0))
            end itbeg itend bfbeg bfend ttbeg ttend
            cache-reset
            (parse-sexp-ignore-comments t)) ; scan-sexps ignores comments
        (goto-char kbeg)
        (cond
         ((not (eq (preceding-char) ?\{))
          ;; Fontify only the keyword as bf/it/type (no argument found).
          (cond ((match-beginning 2) (setq itbeg kbeg itend kend))
                ((match-beginning 5) (setq bfbeg kbeg bfend kend))
                (t                   (setq ttbeg kbeg ttend kend)))
          (goto-char (match-end 0))
          (store-match-data 
           (list nil nil itbeg itend bfbeg bfend ttbeg ttend))
          t)
         (t
          ;; There's an opening bracket
          (save-restriction
            ;; Restrict to LIMIT.
            (narrow-to-region (point-min) limit)
            (forward-char -1)           ;Move on the opening bracket
            (if (font-latex-find-matching-close ?\{ ?\})
                (setq end (point))
              (setq cache-reset t)
              (setq end (point-max))
              (goto-char end))
            (cond ((match-beginning 2) (setq itbeg beg  itend end))
                  ((match-beginning 5) (setq bfbeg beg  bfend end))
                  (t                   (setq ttbeg beg  ttend end)))
            (store-match-data 
             (list kbeg kend itbeg itend bfbeg bfend ttbeg ttend))
            
            ;; Start the subsequent search immediately after this keyword.
            (goto-char kend)

            (when (and font-latex-do-multi-line cache-reset)
              (goto-char limit)             ;Avoid infinite loops?
              (font-latex-set-cache 
               'font-latex-match-infont-cache 
               kbeg kend limit 'infont
               (list kbeg kend itbeg itend bfbeg bfend ttbeg ttend)))

            t))))))))

(defun font-latex-not-on-same-line-as (cache-start)
  "Return t if point is not on same line as CACHE-START."
  (save-excursion
    (not (= (progn (beginning-of-line)(point))
            (progn (goto-char cache-start) (beginning-of-line)(point))))))

;;; FIXME: Add caches for math-env, math-envII and quotations.
(defun font-latex-match-math-env (limit)
  "Used for patterns like:
\\( F = ma \\)
\\ [ F = ma \\] but not \\\\ [len]"
  (when (re-search-forward "\\(\\\\(\\)\\|\\(\\\\\\[\\)" limit t)
    (goto-char (match-beginning 0))
    (if (eq (preceding-char) ?\\)       ; \\[ is not a math environment
        (progn 
          (goto-char (match-end 0))
          (store-match-data (list nil nil)) 
          t)
      (let ((b1start (point)))
        (search-forward (cond ((match-beginning 1) "\\)")
                              (t                   "\\]"))
                        limit 'move)
        (let ((b2end (or (match-end 0) (point))))
          (store-match-data (list b1start b2end))
          t)))))

(defun font-latex-match-math-envII (limit)
  "Used for patterns like:
\\begin{equation}
 fontified stuff
\\end{equation}
The \\begin{equation} and \\end{equation are not fontified here."
  (when (re-search-forward 
         (eval-when-compile 
           (concat "\\\\begin{\\(\\(display\\)?math\\|equation\\|eqnarray"
                   "\\|gather\\|multline\\|align\\|x*alignat"
                   "\\)\\*?}"))
         limit t)
    (let ((beg (match-end 0)) end)
      (if (search-forward (concat "\\end{" (buffer-substring 
                                            (match-beginning 1)(match-end 0)))
                          limit 'move)
          (setq end (match-beginning 0))
        (setq end (point)))
      (store-match-data (list beg end))
      t)))

(defun font-latex-match-quotation (limit)
  "Used for patterns like:
``this is a normal quote'' and these are multilingual quoted strings:
\"< french \"> and \"`german\"' quotes, << french >> and 8-bit french."
  (when (re-search-forward
	 (eval-when-compile
	   (concat "\\(``\\)\\|\\(\"<\\)\\|\\(\"`\\)\\|\\(<<\\)\\|"
		   "\\(" (char-to-string 171) "\\)")) ; An 8-bit "<<"
	 limit t)
    (let ((beg (match-beginning 0)))
      (search-forward
       (cond ((match-beginning 1) "''")
	     ((match-beginning 2) "\">")
	     ((match-beginning 3) "\"'")
	     ((match-beginning 4) ">>")
	     ((match-beginning 5) (eval-when-compile (char-to-string 187))))
       limit 'move)
      (store-match-data (list beg (point)))
      t)))

;; Install ourselves for non AUC-TeX
(add-hook 'latex-mode-hook 'font-latex-setup)
;; If font-latex is loaded using a latex-mode-hook, then the add-hook above
;; won't be called this time around.  Check for this now:
(if (eq major-mode 'latex-mode)
    (font-latex-setup))

;; Provide ourselves:
(provide 'font-latex)

;;; font-latex.el ends here
