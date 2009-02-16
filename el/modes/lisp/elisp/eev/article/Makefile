# (find-sh "unzip")
# (find-sh "pngcrush")
# (find-node "(make)Name Index")
# (find-node "(make)Text Functions" "$(VAR:SUFFIX=REPLACEMENT)")

SCREENSHOTS = \
	ss-m-h.png \
	ss-lua.png \
	ss-f9.png \
	ss-modular.png \
	ss-gdbwide.png

SCREENSHOTS_EPS = $(SCREENSHOTS:%.png=%.eps)
TARGETS = eev.dvi eev.2.ps.gz eev.ps.gz eev.pdf

.PRECIOUS: %.sty %.eps

all: $(TARGETS)
clean:
	rm -fv $(TARGETS) eev.aux eev.log
extraclean: clean
	rm -fv $(SCREENSHOTS_EPS) sbc-template-latex.zip sbc2003.sty

sbc-template-latex.zip:
	wget http://www.univates.br/sicompi/templates/sbc-template-latex.zip
sbc2003.sty: sbc-template-latex.zip
	unzip sbc-template-latex.zip sbc2003.sty && touch sbc2003.sty

%.png: %-uncrushed.png
	pngcrush -c 0 -m 0 $< $@
%.eps : %.png
	convert $< $@

eev.dvi: eev.tex sbc2003.sty $(SCREENSHOTS_EPS)
	rm -fv eev.aux; latex eev.tex && latex eev.tex

%.ps: %.dvi
	dvips -o $@ $<
%.2.ps: %.dvi
	dvired -o $@ $<
%.gz: %
	gzip -c9 < $< > $@ || { rm -f $@; false; }
%.pdf: %.dvi
	dvipdf $< $@

