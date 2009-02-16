PREFIX = /usr/local

EMACS  = emacs
ELCDIR = $(PREFIX)/share/emacs/site-lisp/goby
SRC = goby-emacs.el  goby-vars.el  goby.el  goby-view.el  goby-ps.el
ELC = goby-emacs.elc goby-vars.elc goby.elc goby-view.elc goby-ps.elc

ALSRC = goby-emacs.el  goby-vars.el goby.el goby-view.el goby-ps.el 
CHECK = xemacs # to check unused variables only

MKDIR   = mkdir
RM      = rm -f
INSTALL = install -c -m 644

PTH = path.el
OTH = diff $(PTH)

all: compile

compile: $(SRC)
	echo '(setq load-path (cons "." load-path))' > $(PTH)
	for el in $(SRC); do \
	  $(EMACS) -q -no-site-file -batch -l $(PTH) -f batch-byte-compile $$el; \
	done
	$(RM) $(PTH)

check: $(ALSRC)
	echo '(setq load-path (cons "." load-path))' > $(PTH)
	for el in $(ALSRC); do \
	  $(CHECK) -q -no-site-file -batch -l $(PTH) -f batch-byte-compile $$el; \
	done

install:
	if [ ! -d $(ELCDIR) ]; then \
	  $(MKDIR) $(ELCDIR); \
	fi
	for el in $(SRC); do \
	  $(INSTALL) $$el $(ELCDIR); \
	done
	for elc in $(ELC); do \
	  $(INSTALL) $$elc $(ELCDIR); \
	done

clean:
	$(RM) *.elc $(OTH)

distclean:
	$(RM) *.elc $(OTH)

diff::
	-cvs diff -c -F "^(" > $@
