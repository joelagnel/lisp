prefix = /usr
datadir = ${prefix}/share
lispdir = $(datadir)/emacs/site-lisp/gnus
srcdir = .


EMACS = emacs
XEMACS = emacs

GZIP_PROG = gzip -f

# # # # # # # # # # Release variables (for maintainer only):
#
# Files removed from `cvs export' output before creating the tar-ball:
CVS_IGNORE_FILES = .cvsignore contrib/.cvsignore etc/.cvsignore	\
  lisp/.cvsignore texi/.cvsignore texi/ps/.cvsignore .arch-inventory
# Name and mail address in ChangeLog format for the release commit:
COMMITTER = Lars Magne Ingebrigtsen  <lars@ingebrigtsen.no>
# CODENAME for development releases (CODENAME must have a trailing space,
# (e.g. "Oort ")
CODENAME = 
# Codename initial (lower case), e.g. 'n' for "No Gnus".
CIN =
# Files with hard-coded versions numbers:
RELEASE_COMMIT_FILES = ChangeLog README \
  lisp/ChangeLog lisp/gnus.el \
  texi/ChangeLog texi/gnus.texi texi/message.texi texi/gnus-faq.texi
# Pattern matching previous version(s):
OLD_PATTERN = \(5.10.[0-9]*\|0\.[1-9][0-9]*\)
# CVS tag of the previous version:
OLD_TAG     = v5-10-6
# CVS tag of the current release:
TAG  = v5-10
#
# Procedure:
# - release-bump-version
# - release-commit
# - release-files or release-files-signed
#
# # # # # # # # # # End of release variables

all: lick info

lick:
	cd lisp && $(MAKE) EMACS="$(EMACS)" lispdir="$(lispdir)" all

install:
	cd lisp && $(MAKE) EMACS="$(EMACS)" lispdir="$(lispdir)" install
	cd texi && $(MAKE) EMACS="$(EMACS)" install
	cd etc && $(MAKE) EMACS="$(EMACS)" install

list-installed-shadows:
	cd lisp && $(MAKE) EMACS="$(EMACS)" lispdir="$(lispdir)" list-installed-shadows

remove-installed-shadows:
	cd lisp && $(MAKE) EMACS="$(EMACS)" lispdir="$(lispdir)" remove-installed-shadows

uninstall:
	cd lisp && $(MAKE) lispdir="$(lispdir)" uninstall
	cd texi && $(MAKE) uninstall
	cd etc && $(MAKE) uninstall

# Rule for Lars and nobody else.
some:
	cd lisp && $(MAKE) EMACS="$(EMACS)" some
l:
	cd lisp && $(MAKE) EMACS="$(EMACS)" clever

info:
	cd texi && $(MAKE) EMACS="$(EMACS)" all

clean:
	for i in lisp texi etc texi/ps; do (cd $$i; $(MAKE) clean); done
	rm -f *.orig *.rej *~

elclean:
	cd lisp && rm -f *.elc auto-autoloads.el custom-load.el gnus-load.el

x:
	$(MAKE) EMACS=$(XEMACS)

xsome:
	$(MAKE) EMACS="$(XEMACS)" some

distclean: clean
	for i in lisp texi etc texi/ps; do (cd $$i; $(MAKE) distclean); done
	rm -f config.log config.status config.cache Makefile

config.status: $(srcdir)/configure
	$(SHELL) ./config.status --recheck
$(srcdir)/configure: $(srcdir)/configure.in
	cd $(srcdir) && autoconf
Makefile: $(srcdir)/Makefile.in config.status
	CONFIG_FILES=$@ CONFIG_HEADERS= $(SHELL) ./config.status

# # # # # # # # # # Release targets (for maintainer only):

release-check-settings:
	@echo
	@echo "COMMITTER = '$(COMMITTER)'"
	@echo "  (example: 'Foo Bar  <foo.bar@somewhere.invalid>')"
	@echo "VERSION   = '$(VERSION)' (example: '5.10.6')"
	@echo "CODENAME  = '$(CODENAME)' (example: 'No ')"
	@echo "CIN       = '$(CIN)' (example: 'n', codename initial)"
	@echo "TAG       = '$(TAG)' (example: 'v5-10-6')"
	@echo "OLD_TAG   = '$(OLD_TAG)' (example: 'v5-10-5')"
	@echo
	@if [ x"$(VERSION)" = x ]; then echo "error: No VERSION given."; exit 1; else :; fi
	@if [ x"$(TAG)" = x ]; then echo "error: No TAG given."; exit 1; else :; fi
	@echo
	@echo Settings checked successfully.
	@echo
	@echo You may now make...
	@echo   release-bump-version
	@echo   release-commit
	@echo   release-files or release-files-signed

# Needs GNU grep and a recent GNU sed:
release-bump-version:	release-check-settings
	sed -i -e '/^(defconst gnus-version-number /s,"$(OLD_PATTERN)","$(VERSION)",' lisp/gnus.el
	sed -i -e '/^.newcommand{.gnusversionname}{[A-Za-z ]*Gnus v/s,$(OLD_PATTERN),$(VERSION),' texi/gnus.texi
	sed -i -e '/^This manual corresponds to [A-Za-z ]*Gnus v/s,$(OLD_PATTERN),$(VERSION),' texi/gnus.texi
	sed -i -e '/^corresponding to this manual is [A-Za-z ]*Gnus v/s,$(OLD_PATTERN),$(VERSION),' texi/message.texi
	sed -i -e 's,/[a-z]*gnus-[0-9.]*/,/$(CIN)gnus-$(VERSION)/,' README
	if [ "x$(CODENAME)" = "x" ]; then \
	  sed -i -e '/The current release ($(OLD_PATTERN)) should/s,$(OLD_PATTERN),$(VERSION),' texi/gnus-faq.texi; \
	else \
	  true; \
	fi
	@if grep -q "$(CODENAME)Gnus v$(VERSION) is released" ChangeLog; then \
	  echo "ChangeLog is already updated"; \
	else \
	  sed -i -e "1s|^|`date -I`  $(COMMITTER)\n\n\t* README: $(CODENAME)Gnus v$(VERSION) is released.\n\n|" ChangeLog; \
	fi
	@if grep -q "$(CODENAME)Gnus v$(VERSION) is released" texi/ChangeLog; then \
	  echo "texi/ChangeLog is already updated"; \
	else \
	  sed -i -e "1s|^|`date -I`  $(COMMITTER)\n\n\t* gnus.texi, gnus-faq.texi, message.texi: $(CODENAME)Gnus v$(VERSION) is released.\n\n|" texi/ChangeLog; \
	fi
	@if grep -q "$(CODENAME)Gnus v$(VERSION) is released" lisp/ChangeLog; then \
	  echo "lisp/ChangeLog is already updated"; \
	else \
	  sed -i -e "1s|^|`date -I`  $(COMMITTER)\n\n\t* gnus.el: $(CODENAME)Gnus v$(VERSION) is released.\n\n|" lisp/ChangeLog; \
	fi
	cvs diff -U0 $(RELEASE_COMMIT_FILES); sleep 2

release-revert-files:
	rm -i      $(RELEASE_COMMIT_FILES)
	cvs update $(RELEASE_COMMIT_FILES)

release-commit:	release-check-settings
	@echo -e '\007'
	@echo
	@echo "Really do the release-commit for $(CODENAME)Gnus v$(VERSION)?"
	@echo "Command line:"
	@echo "  " cvs commit -m "$(CODENAME)Gnus v$(VERSION) is released." $(RELEASE_COMMIT_FILES)
	@echo
	@echo "Hit RET to continue or Ctrl-C to abort."
	@echo
	@read dummy
	cvs commit -m "$(CODENAME)Gnus v$(VERSION) is released." $(RELEASE_COMMIT_FILES)
	@echo
	@echo "Add tag $(TAG) to CVS?"
	@echo "Command line:"
	@echo "  " cvs tag $(TAG)
	@echo "Hit RET to continue or Ctrl-C to abort."
	@echo
	@read dummy
	cvs tag $(TAG)

release-files:	release-make-tar-ball release-diff
	@echo "Release files have been created:"
	@ls -l $(CIN)gnus-$(VERSION).tar.gz
	@ls -l $(CIN)gnus-`echo $(OLD_TAG)%$(TAG)|tr - .|tr -d v|tr % -`.diff.gz

release-files-signed:	release-files release-sign-files

# Sign prepared files:
release-sign-files:
	if [ "x$$GPG_AGENT_INFO" = "x" ]; then \
	  read -sp "Enter pass phrase: " phrase ; \
	  gpg_opt="--passphrase-fd 0"; \
	else \
	  gpg_opt=""; phrase=""; \
	fi; \
	for i in $(CIN)gnus-$(VERSION).tar.gz \
	         $(CIN)gnus-`echo $(OLD_TAG)%$(TAG)|tr - .|tr -d v|tr % -`.diff.gz; do \
	   rm -f $$i.sig; \
	   echo "$$phrase" | gpg --detach-sign $$gpg_opt $$i; \
	done; true
	@ls -l $(CIN)gnus-$(VERSION).tar.gz{.sig,}
	@ls -l $(CIN)gnus-`echo $(OLD_TAG)%$(TAG)|tr - .|tr -d v|tr % -`.diff.gz{.sig,}
	@gpg --verify $(CIN)gnus-$(VERSION).tar.gz{.sig,}
	@gpg --verify $(CIN)gnus-`echo $(OLD_TAG)%$(TAG)|tr - .|tr -d v|tr % -`.diff.gz{.sig,}

release-cvs-export $(CIN)gnus-$(VERSION):	release-check-settings
	rm -fr $(CIN)gnus-$(VERSION)
	cvs export -d $(CIN)gnus-$(VERSION) -r $(TAG) gnus

release-make-tar-ball:	$(CIN)gnus-$(VERSION)
	[ -d $(CIN)gnus-$(VERSION) ] && cd $(CIN)gnus-$(VERSION) && \
	  rm $(CVS_IGNORE_FILES) || true
	tar zcvf $(CIN)gnus-$(VERSION).tar.gz $(CIN)gnus-$(VERSION)
	ls -l $(CIN)gnus-$(VERSION).tar.gz

# Make a diff between current and previous release.  Example:
# ftp://quimby.gnus.org/pub/gnus/ding-patches/gnus-5.10.5-5.10.6.diff.gz
release-diff:	release-check-settings
	cvs diff -r $(OLD_TAG) -r $(TAG) | sed -e '/^\? /d' > temp.diff || true
	mv temp.diff $(CIN)gnus-`echo $(OLD_TAG)%$(TAG)|tr - .|tr -d v|tr % -`.diff
	$(GZIP_PROG) $(CIN)gnus-`echo $(OLD_TAG)%$(TAG)|tr - .|tr -d v|tr % -`.diff
	ls -l        $(CIN)gnus-`echo $(OLD_TAG)%$(TAG)|tr - .|tr -d v|tr % -`.diff.gz

release-post-clean:	release-check-settings
	rm -fr $(CIN)gnus-$(TAG) temp.diff

# # # # # # # # # # End of release targets

# Tell versions [3.59,3.63) of GNU make to not export all variables.
# Otherwise a system limit (for SysV at least) may be exceeded.
.NOEXPORT:
