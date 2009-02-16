# -*- Mode: Makefile -*-

# Makefile --
#
# This file is part of ILISP.
# Please refer to the file COPYING for copyrights and licensing
# information.
# Please refer to the file ACKNOWLEGDEMENTS for an (incomplete) list
# of present and past contributors.
#
# $Id: Makefile,v 1.23 2003/12/13 23:43:44 rgrjr Exp $

# Note: this makefile assumes GNU make

#==============================================================================
# Various Variables

Version = 5.12.0

# Use whichever you like most
#EMACS = xemacs
#EMACS = /usr/local/bin/emacs
EMACS = emacs

# If your operating system does not support file links (e.g. Windows),
# change this to an ordinary copy command
LN = ln -s

# The SHELL variable is used only for making the distribution.
SHELL = /bin/csh

# The 'rm' command used (we redefine it mostly because it may be
# aliased
RM = /bin/rm -f


# These are used mostly for packaging the distribution
Ilisp_src_dir = $(shell pwd)
Ilisp_tar_dir = ilisp-$(Version)

OtherFiles = README         \
             HISTORY        \
             Makefile       \
             ilisp.emacs    \
             icompile.bat   \
             INSTALLATION   \
             COPYING        \
             GETTING-ILISP  \
             Welcome        \
	     ChangeLog      \
	     ACKNOWLEDGMENTS

# maybe add custom-ilisp.elc to LoadFiles later.
LoadFiles = ilisp-def.elc ilisp-sym.elc \
 ilisp-inp.elc ilisp-ind.elc ilisp-prc.elc ilisp-val.elc ilisp-out.elc \
 ilisp-mov.elc ilisp-key.elc ilisp-prn.elc ilisp-low.elc ilisp-doc.elc \
 ilisp-ext.elc ilisp-mod.elc ilisp-dia.elc ilisp-cmt.elc ilisp-rng.elc \
 ilisp-hnd.elc ilisp-utl.elc ilisp-cmp.elc ilisp-kil.elc ilisp-snd.elc \
 ilisp-xfr.elc ilisp-hi.elc ilisp-aut.elc \
 ilisp-cl.elc ilisp-cmu.elc ilisp-sbcl.elc ilisp-cl-easy-menu.elc\
 ilisp-acl.elc ilisp-kcl.elc ilisp-luc.elc ilisp-sch.elc ilisp-hlw.elc \
 ilisp-xls.elc ilisp-chs.elc ilisp-openmcl.elc ilisp-ccl.elc


DocFiles = docs/Makefile \
	   docs/README \
	   docs/doc-changes.txt \
           docs/ilisp-refcard.tex \
           docs/ilisp.texi


FaslFiles = *.fasl *.fas *.lib *.x86f *.sparcf *.pfsl

#==============================================================================
# Rules

compile:
	$(EMACS) -batch -l ilisp-mak.el

tags:
	etags *.el

docs: FORCE
	cd docs; $(MAKE)

clean: 
	-$(RM) *.elc *~ extra/*.elc extra/*~ TAGS \
	$(FaslFiles)
	(cd docs; $(MAKE) clean)

loadfile:
	@echo 'The "loadfile" target is no longer supported.'
	@exit 1

compress:
	gzip *.el $(OtherFiles)

FORCE:

#==============================================================================
# The following targets are used only to create a distribution file.

dist: tarring dist_compressing

tarring:
	@echo "ILISP dist: preparing tar file."
	@echo "            source directory: " $(Ilisp_src_dir)
	@echo "            tar directory:    " $(Ilisp_tar_dir)
	(cd $(Ilisp_src_dir)/..;                                        \
         if ( $(notdir $(Ilisp_src_dir)) != $(Ilisp_tar_dir) )          \
            ln -s $(notdir $(Ilisp_src_dir)) $(Ilisp_tar_dir) ;         \
         tar cvf $(Ilisp_tar_dir).tar                                   \
            $(patsubst %,$(Ilisp_tar_dir)/%,$(OtherFiles))              \
            $(Ilisp_tar_dir)/*.el                                       \
            $(Ilisp_tar_dir)/*.lisp                                     \
            $(Ilisp_tar_dir)/*.scm                                      \
            $(patsubst %,$(Ilisp_tar_dir)/%,$(DocFiles))                \
            $(Ilisp_tar_dir)/extra/README                               \
            $(Ilisp_tar_dir)/extra/hyperspec.el                         \
	    $(Ilisp_tar_dir)/extra/cltl2.el				\
            $(Ilisp_tar_dir)/pictures/ilisp-icon.*                      \
        )

dist_compressing:
	(cd $(Ilisp_src_dir)/.. ; gzip $(Ilisp_tar_dir).tar)

uuencoding: ../$(Ilisp_tar_dir).tar.gz
	(cd $(Ilisp_src_dir)/.. ;                                           \
         uuencode $(Ilisp_tar_dir).tar.gz $(Ilisp_tar_dir).tar.gz > il.uue)


# end of file -- Makefile --
