# Makefile to build TRAMP, such as it is...
# requires GNU make and GNU tar.
# This should be improved.

# If we seem to be in an XEmacs package hierarchy, build packages.
# Otherwise, use the upstream rules.
# #### I don't think we need to strip the result of $(wildcard ...)
ifeq (,$(wildcard ../../XEmacs.rules))

# This is not an XEmacs package.

# N.B.  Configuration of utilities for XEmacs packages is done in
# ../../Local.rules.  These have no effect on XEmacs's package build
# process (and thus live inside the conditional).
EMACS	 = emacs
MAKEINFO = makeinfo
DIRS	 = lisp texi

.PHONY: MANIFEST

all:
	for a in ${DIRS}; do						\
	    $(MAKE) -C $$a "EMACS=$(EMACS)" "MAKEINFO=$(MAKEINFO)" all;	\
	done

clean:
	rm -f MANIFEST tramp.tar.gz
	for a in ${DIRS}; do						\
	    $(MAKE) -C $$a "EMACS=$(EMACS)" "MAKEINFO=$(MAKEINFO)" clean; \
	done

MANIFEST:
	cd .. ;							\
	find tramp \( -name CVS -prune \)			\
		-o \( -name tmp -prune \)			\
		-o -type f \! -name "*~"			\
		-a \! -name "*.elc" -a \! -name "*.aux"		\
		-a \! -name "*.cp" -a \! -name "*.fn"		\
		-a \! -name "*.vr" -a \! -name "*.tp"		\
		-a \! -name "*.ky" -a \! -name "*.pg"		\
		-a \! -name "*.tmp" -a \! -name "*.log"		\
		-a \! -name "*.toc" -a \! -name "*,v"		\
		-a \! -name "*.tar.gz"				\
		-print > MANIFEST ;				\
	egrep -v 'tramp2/|test/' MANIFEST > MANIFEST.stable

tar: MANIFEST
	cd .. ; tar cvpfzT tramp/tramp.tar.gz MANIFEST ;	\
	cp tramp/tramp.tar.gz tramp/tramp1-development.tar.gz ;	\
	cp tramp/tramp.tar.gz tramp/tramp2-development.tar.gz ;	\
	tar cvpfzT tramp/tramp-stable.tar.gz MANIFEST.stable

xemacs:
	cp lisp/ChangeLog lisp/tramp*.el ../../xemacs/tramp/lisp
	cp texi/ChangeLog texi/tramp*.texi ../../xemacs/tramp/texi
	cp test/*.el ../../xemacs/tramp/test

dist: tar
	install -m644 tramp.tar.gz /home-local/ftp/pub/src/emacs
#	install -m644 lisp/tramp.el /home-local/ftp/pub/src/emacs

install-html:
	cd texi ; $(MAKE) install-html

sourceforge: dist
	cd texi ; $(MAKE) sourceforge
	scp tramp.tar.gz kaig@tramp.sourceforge.net:/home/groups/t/tr/tramp/htdocs/download
	( echo 'anonymous';				\
	  echo prompt;					\
	  echo hash;					\
	  echo cd incoming;				\
	  echo put tramp-stable.tar.gz;			\
	  echo put tramp1-development.tar.gz;		\
	  echo put tramp2-development.tar.gz;		\
	  echo quit ) | ftp upload.sourceforge.net

else

# This is an XEmacs package.
include Makefile.XEmacs
endif
