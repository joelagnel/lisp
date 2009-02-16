# Makefile - for the AUC TeX distribution.
#
# Maintainer: Per Abrahamsen <auc-tex@sunsite.dk>
# Version: 11.14
#
# Edit the makefile, type `make', and follow the instructions.

##----------------------------------------------------------------------
##  YOU MUST EDIT THE FOLLOWING LINES 
##----------------------------------------------------------------------

# Where local software is found
prefix=/usr/local

# Where info files go.
infodir = $(prefix)/info

# Where local lisp files go.
lispdir = $(prefix)/share/emacs/site-lisp

# Where the AUC TeX emacs lisp files go.
aucdir=$(lispdir)/auctex

# Name of your emacs binary
EMACS=emacs

##----------------------------------------------------------------------
## YOU MAY NEED TO EDIT THESE
##----------------------------------------------------------------------

# Do not change the definition of autodir below, unless you also
# update TeX-auto-global in tex-init.el

# Where the automatically generated lisp files for your site go.
autodir=$(aucdir)/auto

# Using emacs in batch mode.
BATCH=$(EMACS) -batch -no-site-file -q -l lpath.el

# Specify the byte-compiler for compiling AUC TeX files
ELC= $(BATCH) -f batch-byte-compile

# Specify the byte-compiler for generating style files
AUTO= $(EMACS) -batch -l $(aucdir)/tex.elc \
	-l $(aucdir)/latex.elc -f TeX-auto-generate-global

# Specify the byte-compiler for compiling generated style files
AUTOC= $(ELC)

# How to move the byte compiled files to their destination.  
MV = mv

# How to copy the lisp files to their distination.
CP = cp -p

##----------------------------------------------------------------------
##  BELOW THIS LINE ON YOUR OWN RISK!
##----------------------------------------------------------------------

.SUFFIXES: .el .elc .texi 

SHELL = /bin/sh

FTPDIR = /home/tmp/auctex-ftp
#FTPDIR = /home/ftp/pub/Staff/Per.Abrahamsen/mirror/ftp/auctex

WWWDIR = /home/tmp/auctex-www
#WWWDIR = /home/ftp/pub/Staff/Per.Abrahamsen/mirror/www/auctex

REMOVE =  MSDOS VMS OS2 WIN-NT

MINMAPSRC = auc-menu.el maniac.el outln-18.el all.el multi-prompt.el

CONTRIB = hilit-LaTeX.el bib-cite.el tex-jp.el tex-fptex.el
CONTRIBELC = bib-cite.elc 

AUCSRC = auc-old.el tex.el tex-buf.el latex.el tex-info.el \
	texmathp.el multi-prompt.el tex-mik.el font-latex.el tex-font.el
AUCELC = auc-old.elc tex.elc tex-buf.elc latex.elc tex-info.elc \
	texmathp.elc multi-prompt.elc tex-mik.elc font-latex.elc tex-font.elc


STYLESRC = style/prosper.el \
	   style/slides.el    style/foils.el    style/amstex.el \
	   style/article.el   style/book.el     style/letter.el \
	   style/report.el    style/amsart.el   style/amsbook.el \
	   style/epsf.el      style/psfig.el    style/latexinfo.el \
	   style/dutch.el     style/german.el   style/dk.el \
	   style/j-article.el style/j-book.el   style/j-report.el \
	   style/jarticle.el  style/jbook.el    style/jreport.el \
	   style/dinbrief.el  style/virtex.el   style/plfonts.el \
	   style/plhb.el      style/harvard.el	style/swedish.el \
	   style/danish.el    style/slovak.el   style/czech.el \
	   style/amsmath.el   style/amstext.el  style/amsbsy.el \
	   style/amsopn.el    style/amsthm.el	style/natbib.el \
	   style/index.el     style/makeidx.el  style/multind.el \
	   style/varioref.el  style/fancyref.el	style/mdwlist.el \
	   style/ngerman.el   style/graphicx.el	style/graphics.el \
	   style/verbatim.el  style/scrbase.el  style/scrartcl.el \
	   style/scrbook.el   style/scrreprt.el	style/scrlttr2.el

DOCFILES = doc/Makefile doc/auc-tex.texi doc/intro.texi doc/install.texi \
	doc/changes.texi doc/tex-ref.tex doc/math-ref.tex doc/history.texi

EXTRAFILES = COPYING PROBLEMS IRIX Makefile ChangeLog \
	lpath.el tex-site.el $(CONTRIB)

all:	lisp

lisp:
	$(ELC) $(AUCSRC) $(STYLESRC)

some:	$(AUCELC) $(STYLESRC:.el=.elc)

install:	install-lisp

contrib:
	$(ELC) bib-cite.el
	$(ELC) font-latex.el
# 	$(ELC) tex-jp.el              # Doesn't compile without MULE
# 	$(ELC) hilit-LaTeX.el         # Doesn't compile without X

install-lisp:	some
	if [ ! -d $(lispdir) ]; then mkdir $(lispdir); else true; fi ;
	if [ -f $(lispdir)/tex-site.el ]; \
	then \
	    echo "Leaving old tex-site.el alone."; \
	else \
	    sed -e 's#@AUCDIR#$(aucdir)/#' tex-site.el \
	    > $(lispdir)/tex-site.el ; \
        fi
	if [ ! -d $(aucdir) ]; then mkdir $(aucdir); else true; fi ; 
	if [ `/bin/pwd` != `(cd $(aucdir) && /bin/pwd)` ] ; \
	then \
	    if [ ! -d $(aucdir)/style ]; then mkdir $(aucdir)/style; \
	                                 else true; fi ; \
	    $(MV) $(AUCELC) $(aucdir) ; \
	    $(MV) style/*.elc $(aucdir)/style ; \
	    $(CP) $(AUCSRC) $(aucdir) ; \
	    $(CP) style/*.el $(aucdir)/style ; \
	    touch $(aucdir)/style/.nosearch ; \
	    if [ ! -d $(aucdir)/auto ]; then mkdir $(aucdir)/auto; \
	                                else true; fi ; \
	    touch $(aucdir)/auto/.nosearch ; \
	else \
	    echo "Leaving compiled files in place."; \
	fi

install-contrib:	$(CONTRIBELC)
	$(MV) $(CONTRIBELC) $(aucdir)
	$(CP) $(CONTRIB) $(aucdir)

install-info:
	-(cd doc; $(MAKE) install infodir=$(infodir))


install-auto:
	@echo "Use \"M-x TeX-auto-generate-global RET\" instead."


.el.elc:
	$(ELC) $<

clean:
	rm -rf *~ #*# lex.yy.c idetex auctex
	(cd doc; $(MAKE) clean)

wc:
	wc $(AUCSRC) $(STYLESRC) 

dist:	
	@if [ "X$(TAG)" = "X" ]; then echo "*** No tag ***"; exit 1; fi
	if [ "X$(OLD)" = "X" ]; then echo "No patch"; exit 1; fi
	@echo "**********************************************************"
	@echo "** Making distribution of auctex for release $(TAG)"
	@echo "**********************************************************"
	if [ -d auctex-$(TAG) ]; then rm -r auctex-$(TAG) ; fi
	perl -pi.bak -e "s/Version: $(OLD)/Version: $(TAG)/" \
	    $(AUCSRC) $(EXTRAFILES)
	mv ChangeLog ChangeLog.old
	echo `date "+%Y-%m-%d "` \
	     " David Kastrup  <David.Kastrup@t-online.de>" > ChangeLog
	echo >> ChangeLog
	echo "	* Version" $(TAG) released. >> ChangeLog
	echo >> ChangeLog
	cat ChangeLog.old >> ChangeLog
	cvs commit -m 'Release_$(OLD)++' tex.el
	rm -f tex.el.orig
	mv tex.el tex.el.orig
	sed -e '/defconst AUC-TeX-date/s/"[^"]*"/"'"`date`"'"/' \
	    -e '/defconst AUC-TeX-version/s/"[^"]*"/"'$(TAG)'"/' \
	    < tex.el.orig > tex.el
	cvs commit -m 'Release_$(TAG)'
	cvs tag release_`echo $(TAG) | sed -e 's/[.]/_/g'`
	mkdir auctex-$(TAG) 
	mkdir auctex-$(TAG)/style
	mkdir auctex-$(TAG)/doc 
	cp $(AUCSRC) $(EXTRAFILES) auctex-$(TAG)
	cp $(STYLESRC) auctex-$(TAG)/style
	touch auctex-$(TAG)/style/.nosearch
	cp $(DOCFILES)  auctex-$(TAG)/doc
	(cd doc; $(MAKE) dist; cp auctex auctex-* ../auctex-$(TAG)/doc )
	(cd doc; cp INSTALLATION README CHANGES ../auctex-$(TAG)/ )
	cp doc/CHANGES $(FTPDIR)/CHANGES-$(TAG)
	cp doc/auc-tex.ps $(FTPDIR)
	cp ChangeLog $(FTPDIR)
	cp doc/*.html $(WWWDIR)/doc
	rm -f $(FTPDIR)/auctex-$(TAG).tar.gz $(FTPDIR)/auctex.tar.gz
	rm -f $(FTPDIR)/auctex.zip
	tar -cf - auctex-$(TAG) | gzip --best > $(FTPDIR)/auctex-$(TAG).tar.gz
	-zip -r $(FTPDIR)/auctex-$(TAG) auctex-$(TAG)

patch:
	diff -u auctex-$(OLD) auctex-$(TAG) > \
		$(FTPDIR)/auctex-$(OLD)-to-$(TAG).patch; exit 0

min-map:
	-cvs add $(MINMAPSRC) 
	cvs commit -m "Update"
	cp $(MINMAPSRC) doc/math-ref.tex $(FTPDIR) 
