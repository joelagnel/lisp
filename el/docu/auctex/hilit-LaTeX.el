;; hilit-LaTeX.el - Enhancements for LaTeX highlighting w/ hilit19.el or hl319
;;
;; Copyright (C) 1994 Peter S. Galbraith
 
;; Author:    Peter S. Galbraith <rhogee@bathybius.meteo.mcgill.ca>
;; Created:   03 March 1994
;; Version:   1.17 (06 Sep 95)
;; Keywords:  LaTeX, hilit19, hl319, highlight, auctex 

;; Everyone is granted permission to copy, modify and redistribute this
;; file provided:
;;   1. All copies contain this copyright notice.
;;   2. All modified copies shall carry a prominant notice stating who
;;      made modifications and the date of such modifications.
;;   3. The name of the modified file be changed.
;;   4. No charge is made for this software or works derived from it.
;;      This clause shall not be construed as constraining other software
;;      distributed on the same medium as this software, nor is a
;;      distribution fee considered a charge.

;; LCD Archive Entry:
;; hilit-LaTeX|Peter Galbraith|GalbraithP@dfo-mpo.gc.ca|
;; Enhancements for LaTeX highlighting w/ hilit19.el or hl319.el|
;; 06-Sep-1995|1.17|~/modes/hilit-LaTeX.el.gz|
;; ----------------------------------------------------------------------------
;;; Commentary:

;; New versions of this package (if they exist) may be found at:
;;  ftp://ftp.phys.ocean.dal.ca/users/rhogee/elisp/hilit-LaTeX.el

;; Description:
;;  This package enhances hilit19 (or hl319) highlighting patterns for LaTeX.
;;  hl319.el (or hl319-fsfmacs.el) is an enhanced version of hilit19.  
;;  I suggest that you get it:
;;   at ftp.netcom.com in /pub/st/stig/src/elisp/Beta/hl319-fsfmacs.el.gz
;;   (I could never get Mosaic to connect to it...)
;;  It is faster than hilit19 and allows on-the-fly highlighting

;; Installation instructions:
;;
;;  All you need to do is add this line to your .emacs file after you have 
;;  loaded hl319 or hilit19: 
;;
;;    (if window-system
;;        (require 'hilit-LaTeX))
;;
;;  But default, unknown LaTeX commands are highlighted (in light grey when
;;  in light background).  I like this feature, but it might slow down 
;;  hilit-LaTeX too much for you.  To disable this feature, add the following
;;  to your .emacs file *before* you require (or load) hilit-LaTeX.
;;
;;    (setq hilit-LaTeX-commands nil)
;;
;;  Additional highlights are available for AmS-LaTeX commands by setting
;;
;;    (setq hilit-AmSLaTeX-commands t)
;;
;;  And some multi-lingual quoted strings are highlighted by setting:
;;
;;    (setq hilit-multilingual-strings t)
;;
;;  These variables must all be set *before* hilit-LaTeX is loaded.

;; To add (?)
;;  - optinal highlighting of [options] and {arguments} after unknown commands.

;; ----------------------------------------------------------------------------
;;; Change log:
;; V1.00 01Mar94 Peter S Galbraith - Created
;; V1.01 14Mar94 PSG - Added {\sc } 
;;                   - Added fix for \[ ]\ excluding \\[len]
;; V1.02 15Mar94 PSG - Real dollar sign \$ won't hilit like math mode.
;; V1.03 16Mar94 Jonathan Katz <jkatz@weber.ucsd.edu>
;;                   - lateX2e-mode, \documentclass, \usepackage
;; V1.04 16Mar94 PSG - Added \sc as a declaration.
;; V1.05 17Mar94 Andrew Swann <swann@imada.ou.dk>
;;                   - ams-latex-mode, \eqref , \title[]{}, \numberwithin
;;               PSG - \[any]cite command that user makes up.
;; V1.06 06Apr94 Martin Maechler <maechler@stat.math.ethz.ch>
;;                   - \cite[anyword] commands that user makes up.        
;; V1.07 04Jan95 PSG - Added hilit-inside-bracket-region for LaTeX2e
;;                     commands \textrm \textbf and \emph.
;; V1.08 16Feb95 PSG - and support for many other LaTeX2e font commands.
;;                   - Fixed hl319 on-the-fly highlighting (sort of)  
;; V1.09 27Feb95 Andrew Swann <swann@imada.ou.dk>
;;                   - Fixed bug where slash was always hilighted.
;; V1.10 01Mar95 Andrew Swann <swann@imada.ou.dk> and PSG.
;;                   - Fixed embedded (quoted) dollar sign within formula.
;; V1.11 02Mar95 PSG - Bug report by Richard Staton <stanton@haas.berkeley.edu>
;;                   - Fixed highlighted character before %comment
;;                   - Fixed highlight for % in first column
;; V1.12 03Mar95 PSG - Allow options to cite commands.
;; V1.13 09Mar95 PSG - Suggestions from kevin@rodin.wustl.edu (Kevin Ruland)
;;                      extras for ams-latex-mode, equation environment, 
;;                      newpage-type commands.
;;                   - hilit-LaTeX-commands variable and multilingual strings.
;; V1.14 21Mar95 PSG   bug reported by Martin Maechler 
;;                      <maechler@stat.math.ethz.ch>
;;                     Fixed infinite loop on equation environments that fail
;;                     to have matching end (\begin{equation} \end {equation})
;; V1.15 03Aug95 PSG   bug reported by Alain Smette <asmette@astro.rug.nl> 
;;                     Infinite loops on on-the-fly equation environments
;;                     (No clean way to implement these, so disabled)
;; V1.16 08Aug95 PSG   yet another on-the-fly infinite loop found by
;;                     Laurent Bonnaud <Laurent.Bonnaud@irisa.fr>
;; V1.17 06Sep95 PSG   math regexp broken in V19.29.  Regexp matcher changed in emacs.
;; ----------------------------------------------------------------------------
;;; Code:

(require 'hilit19)

(defvar hilit-AmSLaTeX-commands nil
  "*Set to t if you want to hightlight AmSLaTeX commands.")

(defvar hilit-multilingual-strings nil
  "*Set to t if you want to hightlight multilingual quoted strings.
Highlights:  \"`german\"', \"< french \">, << french >> and « 8-bit french ».")

(defvar hilit-LaTeX-commands t
  "*Set to nil if you don't want to highlight unknown LaTeX commands")

(defvar hilit-on-the-fly-in-use nil
  "Used internally by hilit-LaTeX when on-the-fly highlighting is in use")

;; I need to modify hl319.el's on-the-fly highlighter so that it
;; tells me on-the-fly highlighting is in use.
(defun hilit-rehighlight-changed-lines (st en len)
  "Quietly rehighlight just this line.
Useful as an after change hook in VM/gnus summary buffers and dired buffers."
  (save-match-data
    (let ((hilit-on-the-fly-in-use t))
      ;; (> (- en st) hilit-max-change-rehighlight)
      (hilit-rehighlight-region st en 'quietly))))

 (defun hilit-bracket-region (open)
  "Find region within curly brackets for hilit pattern.
ARG is pattern for beginning of pattern and ends with {.
It cannot be white space.  
Patterns ends simply at the matching closing bracket.

Used for patterns like:
 \\chapter{stuff with matching {} ending with }"
  (if (re-search-forward open nil t)
      (let ((here (point))
            (the-start (match-beginning 0)))
        (backward-char 1)               ; point is on bracket
        (if hilit-on-the-fly-in-use
            ;; if hl319's on-the-fly hilighting is in use then we can't use 
            ;; forward-list because it uses the built-in scan-lists and will 
            ;; return an error if there is no matching bracket yet.
            ;; We don't need to set a max-point to search because range is
            ;; already restricted to one line while on-the-fly.
            (or (re-search-forward "}" nil t)
                (end-of-line))
          (forward-list 1))
        (cons the-start (point)))))

(defun hilit-inside-bracket-region (open)
  "Find region within curly brackets for hilit pattern.
ARG is pattern for beginning of pattern and ends with {.
It cannot be white space.  
Patterns ends simply at the matching closing bracket.

Used for patterns like:
 \\textbf{only stuff within bracket is highlited}"
  (if (re-search-forward open nil t)
      (let ((the-start (point)))
        (backward-char 1)               ; point is on bracket
        (if hilit-on-the-fly-in-use
            (or (re-search-forward "}" nil t)
                (end-of-line))
          (forward-list 1))
        (backward-char 1)
        (cons the-start (point)))))

(defun hilit-inside-environment (open)
  "Find region within curly brackets for hilit pattern.
ARG is pattern for \\begin{something}."
  (if (and (not hilit-on-the-fly-in-use)
           (re-search-forward open nil t))
      (let ((the-start (progn (forward-line 1)(point)))) ;Start on next line
        (if (search-forward 
             (concat 
              "\\end" (buffer-substring(+ 6 (match-beginning 0))(match-end 0)))
             nil t)
            (cons the-start (match-beginning 0))
          (end-of-line)               ;Mark the first line
          (cons the-start (point))))))

(defun hilit-bracket-wysiwyg (open)
  "Find region within curly brackets for hilit pattern.
ARG is pattern for beginning of pattern and starts with {.
Patterns ends simply at the matching closing bracket.

Used for patterns like:
 {\em stuff with matching {} ending with }"
  (if (re-search-forward open nil t)
      (let ((the-start (match-beginning 0)))
        (goto-char the-start)
        (if hilit-on-the-fly-in-use
            (or (re-search-forward "}" nil t)
                (end-of-line))
          (forward-list 1))
        (cons the-start (point)))))

(hilit-set-mode-patterns
 '(LaTeX-mode japanese-LaTeX-mode slitex-mode SliTeX-mode japanese-SliTeX-mode 
              FoilTeX-mode latex-mode latex2e-mode ams-latex-mode)
 (append
  '(("\\(^\\|[^\\]\\)\\(%.*\\)$" 2 comment)) ; comments
  (cond 
   (hilit-AmSLaTeX-commands
    '(("\\\\\\(\\(no\\)?pagebreak\\|\\(new\\|clear\\(double\\)?\\)page\\|enlargethispage\\|\\(no\\)?linebreak\\|newline\\|-\\|displaybreak\\|allowdisplaybreaks\\)"
     nil error)
      
      ("\\\\\\(\\(\\(text\\)?\\(rm\\|sf\\|tt\\|bf\\|md\\|it\\|sl\\|sc\\|up\\|em\\|emph\\)\\(series\\|family\\|shape\\)?\\)\\|\\(appendix\\|tableofcontents\\|listoffigures\\|listoftables\\|normalsize\\|small\\|footnotesize\\|scriptsize\\|tiny\\|large\\|Large\\|LARGE\\|huge\\|Huge\\|raggedright\\|makeindex\\|makeglossary\\|pmb\\|boldsymbol\\)\\)\\b" 
     nil decl)

    ;; various declarations/definitions
    ("\\\\\\(maketitle\\|setlength\\|settowidth\\|addtolength\\|setcounter\\|addtocounter\\)\\b" 
     nil define)
    ("\\\\\\([a-z]+box\\|text\\|intertext\\)\\b" nil keyword)))

   (t
    '(("\\\\\\(\\(no\\)?pagebreak\\|\\(new\\|clear\\(double\\)?\\)page\\|enlargethispage\\|\\(no\\)?linebreak\\|newline\\|-\\)"
     nil error)

      ("\\\\\\(\\(\\(text\\)?\\(rm\\|sf\\|tt\\|bf\\|md\\|it\\|sl\\|sc\\|up\\|em\\|emph\\)\\(series\\|family\\|shape\\)?\\)\\|\\(appendix\\|tableofcontents\\|listoffigures\\|listoftables\\|normalsize\\|small\\|footnotesize\\|scriptsize\\|tiny\\|large\\|Large\\|LARGE\\|huge\\|Huge\\|raggedright\\|makeindex\\|makeglossary\\)\\)\\b" 
       nil decl)

      ;; various declarations/definitions
      ("\\\\\\(maketitle\\|setlength\\|settowidth\\|addtolength\\|setcounter\\|addtocounter\\)\\b" 
       nil define)
      ("\\\\[a-z]+box\\b" nil keyword))))


  '(("``" "''" string))
  (and hilit-multilingual-strings
       '(("\"<" "\">" string)
         ("\"`" "\"'" string)
         ("<<" ">>" string)
         ("«" "»" string)))
    
  '(("\\\\\\(item\\(\\[.*\\]\\)?\\|\\\\\\(\*\\)?\\)" nil label) ;label, \\
    ("\\(^\\|[^\\\\]\\)\\(&+\\)" 2 label)     ; & within tables and such

    ;; "wysiwyg" emphasis
    (hilit-bracket-wysiwyg 
     "{\\\\\\(text\\)?\\(em\\|it\\|sl\\)\\(shape\\|family\\|series\\)?\\b" italic)
    (hilit-bracket-wysiwyg              ;Removed rm from list
     "{\\\\\\(text\\)?\\(bf\\|md\\|sc\\|up\\|tt\\|sf\\)\\(shape\\|family\\|series\\)?\\b" 
     bold))
    
  (cond 
   (hilit-AmSLaTeX-commands
    '((hilit-inside-bracket-region         ;also \boldsymbol{<>}, \pmb{<>},
       "\\\\\\(boldsymbol\\|pmb\\|text\\(bf\\|md\\|rm\\|sf\\|tt\\|sc\\|up\\)\\){" 
       bold)

      (hilit-bracket-region 
       "\\\\\\(\\(page\\|v\\|eq\\)?ref\\|tag\\|eqref\\|label\\|index\\|glossary\\|[A-Za-z]*cite[A-Za-z]*\\(\\[.*\\]\\)?\\){" 
       crossref)                          ; added \tag{} \eqref{}
      ("\\\\notag\\b" nil crossref)       ; and \notag

      (hilit-inside-environment
       "\\\\begin{\\(equation\\|eqnarray\\|gather\\|multline\\|align\\|x*alignat\\)\\(\*\\)?}" 
       glob-struct)))
   (t
    '((hilit-inside-bracket-region      ; Removed rm from list
       "\\\\\\(text\\(bf\\|md\\|sf\\|tt\\|sc\\|up\\)\\){" 
       bold)

      (hilit-bracket-region 
       "\\\\\\(\\(page\\|v\\)?ref\\|label\\|index\\|glossary\\|[A-Za-z]*cite[A-Za-z]*\\(\\[.*\\]\\)?\\){" 
       crossref)                            ; things that cross-reference

      (hilit-inside-environment "\\\\begin{\\(equation\\|eqnarray\\)\\(\*\\)?}"
                                glob-struct))))


  ;; FIXME: the following doesn't work.  Tried with nil and default. 
   ;(hilit-inside-bracket-region "\\\\\\(intertext\\|text\\|mbox\\){" default)
  ;; \intertext{<arbitrary text>} will set normal text. 
  ;;  And within any math mode \text{<>} acts like a 'smart' \mbox{}.

  '((hilit-inside-bracket-region "\\\\\\(text\\(it\\|sl\\)\\|emph\\){" italic)

    ("\\\\("  "\\\\)" glob-struct)           ; \( \)
    ("[^\\\\\\(\\\\begin{avm}\\)]\\\\\\[" "\\\\\\]" 
     glob-struct) ; \[ \] but not \\[len] or \begin{avm}\[

;;; ("[^$\\]\\($\\($[^$]*\\$\\|[^$]*\\)\\$\\)" 1 formula); '$...$' or '$$...$$'
    ("\\(^\\|[^\\]\\)\\(\\$\\($\\([^\\$]\\|\\\\.\\)*\\$\\|\\([^\\$]\\|\\\\.\\)*\\)\\$\\)"
     2 glob-struct) ; '$...$' or '$$...$$'

    ;; things that bring in external files
    (hilit-bracket-region  "\\\\\\(include\\|input\\|bibliography\\){" include)
    ;; (re-)define new commands/environments/counters

    (hilit-bracket-region 
    "\\\\\\(re\\)?new\\(environment\\|command\\|length\\|theorem\\|counter\\){"
     defun)

    (hilit-bracket-region 
     "\\\\\\(\\(v\\|h\\)space\\|footnote\\(mark\\|text\\)?\\|\\(sub\\)*\\(paragraph\\|section\\)\\|chapter\\|part\\)\\(\*\\)?\\(\\[.*\\]\\)?{"
     keyword)

    (hilit-bracket-region 
     "\\\\\\(title\\|author\\|date\\|thanks\\|address\\)\\(\\[.*\\]\\)?{" 
     define)

    (hilit-bracket-region 
     "\\\\\\(\\(\\this\\)?pagestyle\\|pagenumbering\\|numberwithin\\|begin\\|end\\|nofiles\\|includeonly\\|bibliographystyle\\|document\\(style\\|class\\)\\|usepackage\\)\\(\\[.*\\]\\)?{" 
     decl)
   
    (hilit-bracket-region "\\\\caption\\(\\[[^]]*\\]\\)?{" warning))
  
  (and hilit-LaTeX-commands
       '(("\\(^\\|[^\\\\]\\)\\(\\\\[a-zA-Z\\\\]+\\)" 2 summary-killed)))))

(provide 'hilit-LaTeX)
;;; hilit-LaTeX.el ends here
