# Makefile, does build info from texi
# Requires makeinfo, assumes it to be in the path...
# Only works with TeXinfo 4.x.  (It may be sufficient to replace
# @command with @code to make it work with TeXinfo 3.x.)

INSTALL=install -m644
SINFO=/app/unido-i06/sun4_56/edit/emacs/20.5/share/emacs/site-info

all: tramp.info dvi tramp.html

clean:
	-rm -f *.info *.html *~

tramp.info: tramp.texi
	$(MAKEINFO) --footnote-style=end --no-split tramp.texi
#	install-info --info-dir=. tramp.info

dvi: tramp.dvi

tramp.dvi: tramp.texi
	texi2dvi tramp.texi

tramp.html: tramp.texi
	$(MAKEINFO) --html --no-split tramp.texi

site-info: tramp.info
	/usr/ucb/$(INSTALL) tramp.info $(SINFO)

install-html: tramp.html tramp_ja.html
	$(INSTALL) tramp.html tramp_ja.html $$HOME/public-html/emacs

# For Japanese manual
tramp_ja.info: tramp_ja.texi
	$(MAKEINFO) --footnote-style=end --no-split tramp_ja.texi
#	install-info --info=dir=. tramp_ja.info

tramp_ja.html: tramp_ja.texi
	$(MAKEINFO) --html --no-split tramp_ja.texi

sourceforge: tramp.html tramp_ja.html
	scp tramp.html tramp_ja.html kaig@tramp.sourceforge.net:/home/groups/t/tr/tramp/htdocs/
