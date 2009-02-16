


# «.texbook»		(to "texbook")
# «.find-texbookpage»	(to "find-texbookpage")
# «.source2e.dvi»	(to "source2e.dvi")
# «.find-source2epage»	(to "find-source2epage")

# «.diagxy»		(to "diagxy")




#####
#
# TeXing the TeXbook
# 2000feb29 / 2005jan01
#
#####

# «texbook»  (to ".texbook")
# http://www.ctan.org/tex-archive/systems/knuth/tex/texbook.tex
#
rm -Rv ~/usrc/texbook/
mkdir  ~/usrc/texbook/
cd     ~/usrc/texbook/
cp -v $S/http/www.ctan.org/tex-archive/systems/knuth/tex/texbook.tex .
patch texbook.tex <<'%%%'
7,10c7,10
< \loop\iftrue
<   \errmessage{This manual is copyrighted and should not be TeXed}\repeat
< \pausing1 \input manmac
< \ifproofmode\message{Proof mode is on!}\pausing1\fi
---
> %\loop\iftrue
> %  \errmessage{This manual is copyrighted and should not be TeXed}\repeat
> \input manmac %\pausing1 \input manmac
> %\ifproofmode\message{Proof mode is on!}\pausing1\fi
%%%

tex texbook     2>&1 | tee ott

#

;;
;; «find-texbookpage»  (to ".find-texbookpage")
;; (ee-once (eeb-eval))
;; (find-texbookfile "")
;; (find-texbookfile "texbook.tex" "1. The Name of the Game. 1.")
;; (find-texbookpage 'appendixI "Index")

(code-c-d "texbook" "~/usrc/texbook/" :xdvi)

(defvar ee-texbook-plist
  '(chapter1    1 chapter2    3 chapter3    7 chapter4   13 chapter5   19
    chapter6   23 chapter7   37 chapter8   43 chapter9   51 chapter10  57
    chapter11  63 chapter12  69 chapter13  85 chapter14  91 chapter15 109
    chapter16 127 chapter17 139 chapter18 161 chapter19 185 chapter20 199
    chapter21 221 chapter22 231 chapter23 251 chapter24 267 chapter25 285
    chapter26 289 chapter27 295
    appendixA 305 appendixB 339 appendixC 367 appendixD 373 appendixE 403
    appendixF 427 appendixG 441 appendixH 449 appendixI 457 appendixJ 483)
"A plist that says at which page of texbook.dvi each chapter begins.
The plist is hand-built from:
  (find-texbookfile \"texbook.tex\" \"1. The Name of the Game. 1.\")
and is used by `find-texbookpage'.")

(defun find-texbookpage (&optional pagespec add &rest rest)
  (interactive)
  (let* ((n (plist-get ee-texbook-plist pagespec))
	 (page (if n (+ 11 n (if (numberp add) add 0)) pagespec)))
    (find-texbookxdvi "texbook.dvi" page)))

;;










#####
#
# Typesetting LaTeX's source2e.dvi
# 2005jun06
#
#####

# «source2e.dvi»  (to ".source2e.dvi")
# (find-status   "tetex-src")
# (find-vldifile "tetex-src.list")
# (find-udfile   "tetex-src/")
#
rm -Rv   ~/usrc/latex/base/
mkdir -p ~/usrc/latex/base/
cd /usr/share/texmf/source/latex/base/
cp -v *  ~/usrc/latex/base/
cd       ~/usrc/latex/base/

# (find-lsrcfile "base/source2e.tex" "latex source2e.tex")
latex source2e.tex
makeindex -s source2e.ist source2e.idx
makeindex -s gglo.ist -o source2e.gls source2e.glo
latex source2e.tex
latex source2e.tex      2>&1 | tee ols

latex classes.dtx
latex classes.dtx       2>&1 | tee olc

#

;;
;; «find-source2epage»  (to ".find-source2epage")
;; (ee-once (eeb-eval))

(code-c-d "lsrc" "/usr/share/texmf/source/latex/")
(code-c-d "source2e" "~/usrc/latex/base/" :xdvi)

;; (find-es "tex" "source2e.dvi")
;; (find-source2exdvi "source2e.dvi")
;; (find-source2efile "ltlists.dtx")
;; (find-source2efile "")
;; (find-source2efile "source2e.ind")
;; (find-source2efile "source2e.toc")
;; (find-source2esh "egrep 'dtx|{Index}' source2e.toc")
;; (find-source2epage)
;; (find-source2epage 'ltdefns 2)
;; (find-source2epage 'lttab)
;; (find-source2epage 'index)

;; (find-source2exdvi "classes.dvi")

(code-c-d "source2e" "~/usrc/latex/base/" :xdvi)

(defvar ee-source2e-plist
  '(ltdirchk   1 ltplain   13 ltvers    24 ltdefns   26 ltalloc   40
    ltcntrl   42 lterror   46 ltpar     55 ltspace   57 ltlogos   69
    ltfiles   70 ltoutenc  80 ltcounts 112 ltlength 116 ltfssbas 117
    ltfsstrc 137 ltfsscmp 160 ltfssdcl 165 ltfssini 186 fontdef  192
    preload  209 ltfntcmd 213 ltpageno 221 ltxref   222 ltmiscen 226
    ltmath   235 ltlists  245 ltboxes  260 lttab    271 ltpictur 293
    ltthm    315 ltsect   319 ltfloat  329 ltidxglo 345 ltbibl   347
    ltpage   350 ltoutput 353 ltclass  408 lthyphen 426 ltfinal  428
    index    487)
"A plist that says at which page of source2e.dvi each .dtx file begins.
The plist is hand-built from (find-source2esh \"grep dtx source2e.toc\")
and is used by `find-source2epage'.")

(defun find-source2epage (&optional pagespec add &rest rest)
  (interactive)
  (let* ((n (plist-get ee-source2e-plist pagespec))
	 (page (if n (+ 9 n (if (numberp add) add 0)) pagespec)))
    (find-source2exdvi "source2e.dvi" page)))

;;



# I have a huge file with years of notes about TeX, LaTeX and friends
# at <http://angg.twu.net/e/tex.e.html>, but it's a mess.



# (find-node "(kpathsea)Top")
# (find-node "(kpathsea)Default expansion")
# (find-node "(kpathsea)Slow path searching")
# (find-node "(kpathsea)Debugging")
# (find-node "(kpathsea)Filename database")

# (find-fline "/usr/lib/texmf/texmf.cnf")

# (find-es "tex")
# (find-es "xypic")




#####
#
# diagxy (Michael Barr's front-end to xypic)
# 2005jun07
#
#####

# «diagxy»  (to ".diagxy")
#
# ftp://ftp.math.mcgill.ca/pub/barr/diagxy.zip
# (code-c-d "diagxy" "~/usrc/diagxy/" :xdvi)
# (code-dvi "diagxy" "~/usrc/diagxy/diaxydoc.dvi")
# (find-diagxyfile "")
# (find-diagxypage 1)
#
rm -Rv ~/usrc/diagxy/
mkdir  ~/usrc/diagxy/
cd     ~/usrc/diagxy/
unzip -a $S/ftp/ftp.math.mcgill.ca/pub/barr/diagxy.zip
latex diaxydoc.tex
latex diaxydoc.tex

#
cd /tmp/
cat > tmp.tex <<'%%%'
\documentclass{book}
\usepackage[latin1]{inputenc}
\usepackage{amsmath}
\usepackage{graphicx}
\def\HOME{/home/aleph}
\input \HOME/usrc/diagxy/diagxy.tex
\begin{document}
% \input ee.tex
\end{document}
%%%

latex tmp.tex

#


# (find-diagxyfile "diaxydoc.tex" "subsection{Nodes and arrows}")
# (find-diagxypage 19)

cp -v ~/usrc/diagxy/diagxy.tex ~/LATEX/
cp -v ~/usrc/diagxy/diaxydoc.dvi ~/LATEX/

#
# «diagxydemo0»  (to ".diagxydemo0")
# (find-fline "$usr_src/diagxy/diaxydoc.tex" "learn mainly by example")
# (find-diagxypage 10)
# (find-angg ".zshrc" "xydemo")
# (find-angg ".zshrc" "diagxydemo")

cat > ~/LATEX/diagxydemo.tex <<'%%%'
\documentclass{book}
\input diagxy
\xyoption{curve}
\begin{document}
$\bfig
\morphism[A`B;f]
\morphism(0,300)[A`B;f]
\morphism(0,600)|m|[A`B;f]
\morphism(0,900)/<-/[A`B;f]
\morphism(900,500)<0,-500>[A`B;f]
\morphism(1200,0)<0,500>[A`B;f]
\efig$
\end{document}
%%%
(cd ~/LATEX/; latex diagxydemo.tex && rexdvi diagxydemo.dvi)



cd $EEVTMPDIR


#



ftp://ftp.math.mcgill.ca/pub/barr/diagxy.zip





#  Local Variables:
#  coding:               raw-text-unix
#  ee-delimiter-hash:    "\n#\n"
#  ee-anchor-format:     "«%s»"
#  End:
