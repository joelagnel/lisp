# For compiling the add-on packages.
# $Id: Makefile,v 1.2 2002/01/02 13:55:15 kaig Exp $

clean:
	$(RM) *.elc

all: base64.elc format-spec.elc

base64.elc: base64.el
	$(EMACS) -batch -f batch-byte-compile base64.el

format-spec.elc: format-spec.el
	$(EMACS) -batch -f batch-byte-compile format-spec.el

