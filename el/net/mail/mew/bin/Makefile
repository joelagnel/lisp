###
### Makefile
###
### Author:  Kazu Yamamoto <Kazu@Mew.org>
### Created: Jul 7, 2000
### Revised: 

###
### make
### make install
###

srcdir = .


prefix = /home/ike/emacs-el/local
exec_prefix = ${prefix}
bindir = ${exec_prefix}/bin
mandir = ${prefix}/man/man1

CC = gcc
CFLAGS = -g -O2 -Wall
CPPFLAGS = 
LDFLAGS = 
COFLAG = -o $@ 
LIBS = -lz 

INSTALL = /usr/bin/install -c
LN = ln
RM = rm -f
MKDIR = mkdir -p

#INSTALL = copy
#LN = copy
#RM = del
#MKDIR = md

EXEEXT=
OBJEXT=.o

SRC1 = mewencode.c utils.c
OBJ1 = mewencode$(OBJEXT) utils$(OBJEXT)
PRG1 = mewencode$(EXEEXT)
ALT  = mewdecode$(EXEEXT) mewcat$(EXEEXT)
SRC2 = mewl.c utils.c pattern.c
OBJ2 = mewl$(OBJEXT) utils$(OBJEXT) pattern$(OBJEXT)
PRG2 = mewl$(EXEEXT)
SRC3 = incm.c
OBJ3 = incm$(OBJEXT) utils$(OBJEXT)
PRG3 = incm$(EXEEXT)

DO1 = utils$(OBJEXT)
SR2 = pattern.c
DO2 = pat$(OBJEXT)
DBG = pattern

MAN1 = mewencode.1
MALT = mewdecode.1 mewcat.1
MAN2 = mewl.1
MAN3 = incm.1

CNF = config.h config.status config.cache config.log configure.lineno
MK  = Makefile

.c$(OBJEXT):
	$(CC) -c $(DEFS) $(CFLAGS) $(CPPFLAGS) $<

all: $(PRG1) $(PRG2) $(PRG3)

$(PRG1) : $(OBJ1)
	$(CC) $(COFLAG) $(LDFLAGS) $(OBJ1) $(LIBS)
	$(RM) $(ALT)
	for f in $(ALT); do \
	  $(LN) $(PRG1) $${f}; done

$(PRG2): $(OBJ2)
	$(CC) $(COFLAG) $(LDFLAGS) $(OBJ2)

$(PRG3): $(OBJ3)
	$(CC) $(COFLAG) $(LDFLAGS) $(OBJ3)

$(DO2): $(SR2)
	$(CC) -DDEBUG -c $(CFLAG) -o $(DO2) $(SR2)

$(DBG): $(DO1) $(DO2)
	$(CC) $(CFLAGS) $(COFLAG) $(DO1) $(DO2)

clean:
	$(RM) $(OBJ1) $(PRG1) $(ALT) $(OBJ2) $(PRG2) $(OBJ3) $(PRG3) $(DBG) $(DO2)

distclean: clean
	$(RM) $(CNF) $(MK)

install: all
	-@if [ ! -d $(bindir) ]; then \
		$(MKDIR) $(bindir); \
	fi
	$(INSTALL) -m 555 $(PRG1) $(bindir)
	for f in $(ALT); do \
	  $(RM) $(bindir)/$${f}; $(LN) $(bindir)/$(PRG1) $(bindir)/$${f}; done
	$(INSTALL) -m 555 $(PRG2) $(bindir)
	$(INSTALL) -m 555 $(PRG3) $(bindir)
	-@if [ ! -d $(mandir) ]; then \
		$(MKDIR) $(mandir); \
	fi
	$(INSTALL) -m 444 $(MAN1) $(mandir)
	for f in $(MALT); do \
	  $(RM) $(mandir)/$${f}; $(LN) $(mandir)/$(MAN1) $(mandir)/$${f}; done
	$(INSTALL) -m 444 $(MAN2) $(mandir)
	$(INSTALL) -m 444 $(MAN3) $(mandir)

##
## End of Makefile
##
