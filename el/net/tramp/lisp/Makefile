INSTALL=install -m644
FTPDIR=/home-local/ftp/pub/src/emacs
SLISP=/app/unido-i06/sun4_56/edit/emacs/20.5/share/emacs/site-lisp

EMACS ?= emacs
EVAL = --eval "(add-to-list 'load-path \".\")" 

ifdef USE_CONTRIB
  EVAL += --eval "(add-to-list 'load-path \"./../contrib\")"
endif

EM = $(EMACS) -batch $(EVAL) -f batch-byte-compile 

all: tramp

tramp: tramp.elc tramp-vc.elc tramp-util.elc

install: all
	$(INSTALL) tramp.el $(FTPDIR)

site-lisp: all
	/usr/ucb/$(INSTALL) tramp.el $(SLISP)
	cd $(SLISP) && emacs-20.5 -batch -f batch-byte-compile tramp.el

clean:
	$(RM) *.elc


%.elc: %.el
	$(EM) $<

.SUFFIXES: .elc .el
