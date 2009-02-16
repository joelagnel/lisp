# -*- Makefile -*-
#
# For dired-dd, Sun Dec 14 14:40:40 1997
# 

.SUFFIXES: .el .elc .texi .dvi .ps .html

#	$(emacs) -batch -q -f batch-byte-compile $<
.el.elc:
	echo '(setq load-path (cons "." load-path))' > ./lp.el
	$(emacs) -batch -q -l ./lp.el -f batch-byte-compile $<
	rm -f ./lp.el

.texi.info:
# I do this manually.
#	$(emacs) -batch -q $< -f texinfo-every-node-update -f texinfo-all-menus-update -f save-buffer
	echo '(setq make-backup-files nil)' > ./junk.el
	$(emacs) -batch -q $< -f texinfo-format-buffer -l ./junk.el -f save-buffer
	rm -f ./junk.el

.texi.dvi:
#	-rm -rf *.log *.aux *.cp *.fn *.vr *.toc *.tp *.ky *.pg *.cps *.fns *.vrs *.kys *.pgs *.tps
	texi2dvi $<
.dvi.ps:
#	dvi2ps $< | psnup -2 > $@
#	dvi2ps -F ./fontdesc $< > $@
	dvi2ps  $< > $@


#.texi.html:
#	texi2html $<

#emacs= emacs
# emacs= mule
emacs=/usr/local/emacs-20.7/bin/emacs

DOC= dired-dd.texi dired-dd.info dired-dd_toc.html dired-dd.html
FORMATTED= dired-dd_toc.html dired-dd.html dired-dd.info
EL= dired-dd.el dired-dd-b3-menu.el ls-japan.el dired-do-emacs-command.el
SPIN= bookmark-popmenu.el bookmark-thief.el emacs-menu-abuse.el locate-dired.el url-to-ange.el str-arg.el archie.el-v3.0.diff.gz mouse-drag.el.truncate-lines.patch.diff.gz jcol.pl
MISC= ChangeLog Makefile NEWS INSTALL COPYING README
ELC= dired-dd.elc dired-dd-b3-menu.elc

NONDIREDHANDLERS=dired-dd-mew.el dired-dd-insert-file.el dired-dd-insert-fname.el dired-dd-timidity.el timidity-mode-menu.el README.non-dired-drop lm.el dired-dd-lm.el dired-dd-w3m.el dired-dd-mpg123.el

BASEN= dired-dd-0.9.1.28
OLD= dired-dd-0.9.1.27
OLDARC=archive/$(OLD).tar.gz
TMP2=tmp2

TMPDIR= $(BASEN)
ARC= $(BASEN).tar.gz

ARCDIR=$(TMPDIR)
SPINDIR=$(TMPDIR)/spinout
NONDIREDDIR=$(TMPDIR)/non-dired-drop

elispdir=~/elisp

compile: $(ELC)
install: $(ELC)
	cp $(ELC) $(elispdir)

tardiff: tar diff

diff: $(OLDARC) $(ARC)
	rm -rf $(TMP2)
	mkdir -p $(TMP2)
	tar -C $(TMP2) -zxpf $(OLDARC)
	tar -C $(TMP2) -zxpf $(ARC)
	(cd $(TMP2); diff --exclude=dired-dd.ps -ruN $(OLD) $(ARCDIR) |\
		gzip -9c > ../$(BASEN).diff.gz)
	ln -sf $(BASEN).diff.gz dired-dd.diff.gz
	rm -rf $(TMP2)

tar: $(ARC)
$(ARC): $(ARCDIR)
	tar -zcf $(ARC) $(TMPDIR)
	ln -sf $(ARC) dired-dd.tar.gz

dir: $(ARCDIR)
$(ARCDIR): $(EL) $(SPIN) $(DOC) $(MISC) $(NONDIREDHANDLERS)
	rm -rf $(TMPDIR)
	mkdir -p $(SPINDIR) $(NONDIREDDIR)
	cp -p $(EL) $(MISC)  $(DOC) Makefile $(TMPDIR)
	cp -p  $(SPIN) $(SPINDIR)
	cp -p $(NONDIREDHANDLERS) $(NONDIREDDIR)
#	chmod 444 $(TMPDIR)/*

print: $(FORMATTED)
	lpr dired-dd.ps

docs: $(FORMATTED)

echo:
	@echo $(EL)

tag: etags
etags:
	etags $(EL)

# dired-dd_toc.html is generated as a side effect
dired-dd.html: dired-dd.texi
	texi2html dired-dd.texi
# This affect also tarball, which is not what I want.
# Now html version on mypage stays in plain background as well.
# Making two versions is too much annoying.
#	perl -i -p -e 's/<BODY>/<body background="back.jpg" text="#f6dfb4" link="#ffd800" vlink="#6b5ace" alink="#ff69b5">/' dired-dd_toc.html $@

dired-dd.info: dired-dd.texi
dired-dd.ps: dired-dd.dvi
dired-dd.dvi: dired-dd.texi

docclean:
	-rm -rf *.ps *.dvi
texclean:
	-rm -rf *.log *.aux *.cp *.fn *.vr *.toc *.tp *.ky *.pg *.cps *.fns *.vrs *.kys *.pgs *.tps

# Author should not use this in working directory.
elcclean:
	-rm -f *.elc

clean: docclean texclean
	-rm -rf ./*~ ./#* ./.#* $(ARC) $(UUE) $(TMPDIR)
