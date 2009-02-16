VERSION=0.99a
SNAPDIR=eon-$(VERSION)

SPECIAL =
ALLSOURCE = $(wildcard *.el)
MISC = COPYING Makefile README

UNCOMPILED = eon-maint.el cl-guide.el
SOURCE= $(filter-out $(SPECIAL) $(UNCOMPILED), $(ALLSOURCE))
	
EMACS = emacs
SITEFLAG = --no-site-file
TARGET = $(patsubst %.el,%.elc,$(SOURCE))

PREFIX = /usr/local
ELISPDIR = $(PREFIX)/share/emacs/site-lisp/eon

all: $(TARGET)

%.elc: %.el
	@$(EMACS) -q $(SITEFLAG) -batch \
		-l eon-maint.el \
		-f batch-byte-compile $<

clean:
	-rm -f *~ *.elc

release: $(ALLSOURCE) $(MISC)
	(cd ..; rm -rf eon-$(VERSION); \
	mkdir eon-$(VERSION); \
	cp -v eon/COPYING eon/Makefile eon/README eon/*.el eon-$(VERSION); \
	tar cvzf eon-$(VERSION).tar.gz eon-$(VERSION); \
	mv eon-$(VERSION).tar.gz packages \
	)

install-bin: lisp
	install -d $(ELISPDIR)
	install -m 0644 $(ALLSOURCE) $(TARGET) $(ELISPDIR)

install: install-bin 

