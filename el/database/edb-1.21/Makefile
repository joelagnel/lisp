# Makefile for EDB.
# Copyright (C) 1994-5 Michael D. Ernst <mernst@theory.lcs.mit.edu>
# This file is part of EDB, the Emacs database.

# Contributors include 
#   Karl Eichwalder <ke@pertron.central.de>
#   Richard Kim <richard@fraud.tv.tek.com>
#   Thorsten Ohl <ohl@gnu.ai.mit.edu>
#   Michael Patton <MAP@nic.dsi.net>

# Type "make" to byte-compile EDB and create Info files.
# You must run this from the directory containing the EDB files.
# Optional arguments to make:
#   all       same as no argument
#   install   as above, but also install Info files
#   lisp      byte-compile EDB
#   info      create manuals in Info format
#   dvi       create manuals in dvi format
#   ps        create manuals in PostScript format
#   clean     remove all .elc, Info, and temporary files
#   tags      create TAGS file


###########################################################################
### Variables
###

PREFIX = /usr/local
LIBDIR = $(PREFIX)/lib
# LIBDIR = $(PREFIX)/share
EMACSDIR = $(LIBDIR)/emacs
EDB-ELC-DIR = $(EMACSDIR)/site-lisp/edb		# where *.elc go
INFODIR = $(PREFIX)/info			# where info files go

# Don't alias "emacs" to "emacs -q", lest -batch be interpreted as a filename.
# But note that -batch implies -q.
EMACS = emacs
# GNU diff
DIFF =  diff -N -C 3
# DVI2PS = dvi2ps
DVI2PS = dvips
ETAGS = etags
INSTALL = cp -p
# INSTALL = install
# INSTALL = install -m644
PWD = pwd
RM = rm -f
TEXI2DVI = texi2dvi

###
### Users shouldn't have to modify anything below this line.
###

# Yes, it's disgusting to have this hard-coded.
EDB-VERSION = 1.21

EL-FILES = database.el db-convert.el db-file-io.el db-format.el \
	db-interfa.el db-isbn.el db-rdb.el db-rep.el \
	db-search.el db-sort.el db-summary.el db-tagged.el \
	db-time.el db-two-dbs.el db-types.el db-util.el
TEST-EL-FILES = db-isbntst.el
# Generating these on the fly is less foolproof and portable.
ELC-FILES = database.elc db-convert.elc db-file-io.elc db-format.elc \
	db-interfa.elc db-isbn.elc db-rdb.elc db-rep.elc \
	db-search.elc db-sort.elc db-summary.elc db-tagged.elc \
	db-time.elc db-two-dbs.elc db-types.elc db-util.elc
INFO-FILES = edb.info edb.info-1 edb.info-2 edb.info-3 edb.info-4 \
	edb.info-5 edb.info-6
TEXINFO-FILES = edb.cp edb.cps edb.fn edb.fns edb.ky edb.kys \
	edb.pg edb.pgs edb.tp edb.tps edb.vr edb.vrs
TEX-FILES = edb.aux edb.dvi edb.log edb.toc
PS-FILES = edb.ps edb-od.ps edb-op.ps


###########################################################################
### Rules
###

all: lisp info

clean: cleandoc cleanelc
cleanelc:
	$(RM) $(ELC-FILES) BYTE-COMPILE.el
cleandoc:
	$(RM) $(INFO-FILES) $(TEXINFO-FILES) $(TEX-FILES) $(PS-FILES)


TAGS: tags
tags: $(EL-FILES) $(TEST-EL-FILES)
	$(ETAGS) $(EL-FILES) $(TEST-EL-FILES)

# Create a file BYTE-COMPILE.el, used by the "lisp" rule.
BYTE-COMPILE.el: Makefile
	echo ";; This file is used in byte-compilation of EDB." > BYTE-COMPILE.el
	echo ";; You may remove it with impunity." >> BYTE-COMPILE.el
	echo "(setq load-path (cons nil load-path))" >> BYTE-COMPILE.el 
	echo -n '(setq edb-directory "' >> BYTE-COMPILE.el 
	echo -n `$(PWD)` >> BYTE-COMPILE.el 
	echo '")' >> BYTE-COMPILE.el 

# Since not all echo programs support the -n option, you may need to substitute
# this for the last three lines above.
#	echo "(setq edb-directory \"`$(PWD)`\")" >> BYTE-COMPILE.el 

lisp: $(EL-FILES) BYTE-COMPILE.el
	$(EMACS) -batch -l ./BYTE-COMPILE.el -l database.el \
		-f byte-compile-database-all \
		-f batch-byte-compile database.el

install: info
	$(INSTALL) $(INFO-FILES) $(INFODIR)

install-elc: lisp
	$(INSTALL) $(ELC-FILES) $(EDB-ELC-DIR)

###
### Documentation
###

info: edb.info
edb.info: edb.texi
	makeinfo edb.texi

dvi: edb.dvi
edb.dvi: edb.texi
	$(TEXI2DVI) edb.texi

ps: edb.ps
edb.ps: edb.dvi
	dvips -o edb.ps edb.dvi > & /dev/null

### These create a manual with the pages in order.
### od = ordered from dvi, op = ordered from PostScript

edb-od.dvi: edb.dvi
	dviselect -i edb.dvi -o firsttwo.dvi =1,=2 >& /dev/null
	dviselect -i edb.dvi -o index.dvi :_1 >& /dev/null
	dviselect -i edb.dvi -o bodyplus.dvi =3: >& /dev/null
	dviselect -i bodyplus.dvi -o bodynew.dvi 1: >& /dev/null
	# last two lines equivalent to this:
	#  dviselect -i edb.dvi =3: | dviselect -o bodynew.dvi 1: >& /dev/null
	# but I want to catch error output for both invocations of dviselect
	dviconcat -o edb-od.dvi firsttwo.dvi index.dvi bodynew.dvi >& /dev/null
	$(RM) firsttwo.dvi index.dvi bodynew.dvi bodyplus.dvi edb.dvi
	mv edb-od.dvi edb.dvi

edb-od.ps: edb-od.dvi
	dvips -o edb-od.ps edb-od.dvi > & /dev/null

edb-op.ps: edb.ps
# # I can't get either of these to work.
# psselect 1-2,_5-_1,3-_6 edb.ps edb-op.ps
# 
# psselect 1-2,_5-_1,3- edb.ps edb-op-half.ps
# psselect -_6 edb-op-half.ps edb-op.ps


###
### FTP directory
###

FTPDIR = /u/mernst/emacs/edb/FTP

ftp: info ps
	$(RM) edbunref.log
	chmod og-w $(EL-FILES) $(TEST-EL-FILES)
	$(INSTALL) Makefile $(EL-FILES) $(TEST-EL-FILES) $(FTPDIR)/code
	$(INSTALL) README README-example edb.texi $(FTPDIR)/code
	$(INSTALL) README-ftp $(FTPDIR)/README
	$(INSTALL) changelog edb.dvi edb.info edb.info-? $(FTPDIR)
	$(INSTALL) edb.ps $(FTPDIR)/edb.ps
	cd $(FTPDIR); mv code edb-$(EDB-VERSION); tar cf edb-$(EDB-VERSION).tar edb-$(EDB-VERSION); mv edb-$(EDB-VERSION) code; gzip -f edb-$(EDB-VERSION).tar
	echo "Moved files to ftp directory"

ftpexample:
	cd Examples; $(MAKE)

ftpmde: 
	$(MAKE) ftpsetup
	$(MAKE) ftp
	$(MAKE) ftpunsetup

# Collect files from hither and yon in one directory
ftpsetup:
	$(INSTALL) Doc/README Doc/edb.texi .
	chmod u+w edb.texi
	$(INSTALL) Examples/README README-example
	$(INSTALL) Doc/changelog Doc/README-ftp .

# Get rid of all those files that belong elsewhere.
ftpunsetup: cleandoc
	$(RM) README README-example changelog README-ftp
	$(RM) edb.texi edb.ps
	$(RM) BYTE-COMPILE.el

diff:
	echo "Exit status (error code) 1 just means there was a difference"
	-$(DIFF) Last-release FTP/code | gzip > edb-diff.gz
	$(RM) edb-diff.gz.UUE
	uuencode edb-diff.gz edb-diff.gz > edb-diff.gz.UUE

# Local variables:
# eval: (auto-fill-mode nil)
# End:
