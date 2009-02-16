EXTRA_DIST = \
	hcal.rb \
	howm2 \
	tag2plan \
	howmkara \
	hsplit.rb \
	howm-rast-register.rb \
	howm-rast-search.rb

ext_DATA = $(EXTRA_DIST)

Makefile: $(srcdir)/Makefile.in  $(top_builddir)/config.status
	cd $(top_builddir) && \
	CONFIG_HEADERS= CONFIG_LINKS= \
	CONFIG_FILES=$(subdir)/$@ $(SHELL) ./config.status
