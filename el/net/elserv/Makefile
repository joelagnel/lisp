#
# Makefile for Elserv.
#

PACKAGE = elserv

RM	= /bin/rm -f
CP	= /bin/cp -p

EMACS	= emacs
XEMACS	= xemacs
FLAGS   = -batch -q -no-site-file -l ELSERV-MK

PREFIX	= NONE
LISP_DIR = NONE
PACKAGE_DIR = NONE
EXEC_DIR = NONE
ICON_DIR = NONE

GOMI	= *.elc auto-autoloads.el custom-load.el elservd

elc:
	$(EMACS) $(FLAGS) -f compile-elserv \
		$(PREFIX) $(LISP_DIR) $(EXEC_DIR) $(ICON_DIR)

install-1:	elc
	$(EMACS) $(FLAGS) -f install-elserv \
		$(PREFIX) $(LISP_DIR) $(EXEC_DIR) $(ICON_DIR)

install:	install-1

package:
	$(XEMACS) $(FLAGS) -f compile-elserv-package \
		$(PACKAGE_DIR) $(EXEC_DIR)

install-package-1: package
	$(XEMACS) $(FLAGS) -f install-elserv-package $(PACKAGE_DIR)

install-package: install-package-1

clean:
	-$(RM) $(GOMI)
