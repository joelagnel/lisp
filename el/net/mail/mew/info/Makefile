###
### Makefile for info
###
### Author:  Kazu Yamamoto <Kazu@Mew.org>
### Created: Aug 22, 1999
### Revised: 

###
### (1) make EMACS=xemacs
### (2) make install
###

################################################################
##
## EDIT THE FOLLOWINGS
##

prefix=/usr/local
infodir=$(prefix)/info

EMACS=emacs
#EMACS = xemacs
#EMACS = mule

INSTALLINFO = `which install-info`
#INSTALLINFO = /usr/local/bin/install-info

#INSTALLINFO_OPTIONS = --section Emacs Emacs
#INSTALLINFO_JOPTIONS =
#infodir = 
INSTALLINFO_ARGS  = --info-dir=$(infodir) mew.info
#INSTALLINFO_ARGS  = mew.info 
INSTALLINFO_JARGS = --info-dir=$(infodir) mew.jis.info


RM = rm -f
MKDIR = mkdir -p
TOUCH = touch
# To save Emacses on Windows
PWD =

################################################################
##
## DO NOT EDIT THE FOLLOWINGS
##

INFO  = mew.info mew.info-1 mew.info-2 mew.info-3 mew.info-4
JINFO = mew.jis.info mew.jis.info-1 mew.jis.info-2 mew.jis.info-3

all: info
info:
	sed -e 's/@setfilename mew.jis.info/@setfilename mew.info/' \
	    -e 's/@set jp/@set us/' mew.texi > tmp.texi
	mv tmp.texi mew.texi
	$(RM) mew.info*
	$(TOUCH) mew.info
	$(EMACS) -batch -q -no-site-file -l texinfmt \
		-f batch-texinfo-format mew.texi

jinfo:
	sed -e 's/@setfilename mew.info/@setfilename mew.jis.info/' \
	    -e 's/@set us/@set jp/' mew.texi > tmp.texi
	mv tmp.texi mew.texi
	$(RM) mew.jis.info*
	$(TOUCH) mew.jis.info
	$(EMACS) -batch -q -no-site-file -l texinfmt \
		-f batch-texinfo-format mew.texi

install: install-info
install-info:
	-@if [ ! -d $(infodir) ]; then \
		$(MKDIR) $(infodir); \
	fi; \
	cp $(INFO) $(infodir)
	-@if [ -x "$(INSTALLINFO)" ]; then \
		$(INSTALLINFO) $(INSTALLINFO_OPTIONS) $(INSTALLINFO_ARGS); \
	else \
		echo "Don't forget to edit $(infodir)/dir for Mew."; \
	fi

install-jinfo:
	-@if [ ! -d $(infodir) ]; then \
		$(MKDIR) $(infodir); \
	fi; \
	cp $(JINFO) $(infodir)
	-@if [ -x "$(INSTALLINFO)" ]; then \
		$(INSTALLINFO) $(INSTALLINFO_JOPTIONS) $(INSTALLINFO_JARGS); \
	else \
		echo "Don't forget to edit $(infodir)/dir for Mew."; \
	fi
##
## End of Makefile
##
