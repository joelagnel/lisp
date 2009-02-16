


#####
#
# ps-mode.el
#
#####

# «ps-mode.el»  (to ".ps-mode.el")
# http://odur.let.rug.nl/~kleiweg/postscript/
# (find-efile "progmodes/ps-mode.el")
# (find-efunction 'ps-mode)
# (find-evariable 'ps-run-x)

(setq ps-run-x '("gs" "-r36" "-sPAPERSIZE=a4"))
(require 'ps-mode)
(progn (ps-mode) (ps-run-start))
(ps-run-quit)

%
% (eeb-psrun)
% (eeb-invoke 'ps-run-region ee-delimiter-percent)
%
/dx 120 def
/dy 120 def
/x0 60 def
/y0 60 def
/myp {dy mul y0 add exch dx mul x0 add exch} def
/defp {myp 2 array astore cvx def} def
  /p7 0 3 defp  /p8 1 3 defp  /p9 2 3 defp
  /p4 0 2 defp  /p5 1 2 defp  /p6 2 2 defp
  /p1 0 1 defp  /p2 1 1 defp  /p3 2 1 defp
/foo { newpath p3 moveto p4 lineto p9 lineto closepath } def
foo gsave 1 1 0 setrgbcolor fill grestore
    gsave 8 setlinewidth stroke grestore

%
% (eeb-psrun)
/Times-Roman findfont
100 scalefont setfont
72 200 moveto
(typography) show

%





#  Local Variables:
#  coding:               raw-text-unix
#  ee-delimiter-hash:    "\n#\n"
#  ee-anchor-format:     "«%s»"
#  End:
