# -*- Mode: Makefile -*-

# Makefile --
#
# This file is part of ILISP.
# Please refer to the file COPYING for copyrights and licensing
# information.
# Please refer to the file ACKNOWLEGDEMENTS for an (incomplete) list
# of present and past contributors.
#
# $Id: Makefile,v 1.7 2002/08/17 16:20:15 kevinrosenberg Exp $

# Note: this makefile assumes GNU make

#==============================================================================
# Various Variables

Version = 5.12.0

# The SHELL variable is used only for making the distribution.
SHELL = /bin/sh

# The 'rm' command used (we redefine it mostly because it may be
# aliased
RM = /bin/rm -f

LN = /bin/ln

DVIPS = dvips

DocFiles = ilisp.texi ilisp-refcard.tex

# Debris (also known as temporary files) produced by texi2dvi

DVIDebris = *.aux *.cp *.cps *.fn *.fns *.ky *.kys *.log *.pg *.pgs \
            *.toc *.tp *.vr *.vrs

HTML_Target_Dir	= html/doc
HTML_Targets = ilisp.ps ilisp.pdf ilisp_toc.html ilisp.html

#==============================================================================
# Rules

# KMR: Add .PHONY so that make system will build html rule rather that look at html directory
.PHONY: html

docs: ps pdf info html

ps: ilisp.ps ilisp-refcard.ps

ilisp.ps: ilisp.dvi
	$(DVIPS) -o $@ $<

ilisp-refcard.ps: ilisp-refcard.dvi
	$(DVIPS) -o $@ $<

dvi: ilisp.dvi ilisp-refcard.dvi

info: ilisp.info

html: ilisp.texi
	-texi2html $<

ilisp_toc.html ilisp.html: html

pdf: ilisp.pdf

ilisp.pdf: ilisp.ps
	-ps2pdf ilisp.ps ilisp.pdf

ilisp.dvi: ilisp.texi

ilisp-refcard.dvi: ilisp-refcard.tex
	tex $<

# Administrative targets.

updatehtml: $(HTML_Targets)
	-(cd $(HTML_Target_Dir); $(RM) $(HTML_Targets))
	-$(LN) $(HTML_Targets) $(HTML_Target_Dir)

clean: 
	-$(RM) $(DVIDebris) \
               ilisp.dvi \
               ilisp.ps \
               ilisp*.html \
               ilisp.info* \
               ilisp.pdf \
	       ilisp-refcard.dvi \
	       ilisp-refcard.ps \
               *~

tmpclean:
	-$(RM) $(DVIDebris)


# end of file -- Makefile --
