NAME=scheme-lookup
VERSION=$(shell darcs changes | grep -c ^[A-Z])

byte-compile: $(addsuffix c,$(wildcard *.el))

.el.elc:
	emacs -q --no-site-file -batch -eval "(add-to-list 'load-path \".\")" -f batch-byte-compile $<

clean:
	-rm -f *.elc 

dist: snapshots
	-rm -rf $(NAME)/
	darcs get $(PWD) $(NAME)
	for i in $(NAME)/*; do test -f $$i && sed -i -e 's/\$$Version\$$/$(VERSION)/g' $$i; done
	cd $(NAME); darcs changes >ChangeLog
	cd $(NAME); darcs add ChangeLog
	cd $(NAME); darcs rec -am 'You should never see this changelog comment.'
	cd $(NAME); darcs dist -d $(NAME)-$(VERSION)
	mv $(NAME)/$(NAME)-$(VERSION).tar.gz snapshots/$(NAME)-$(VERSION).tgz
	-rm -rf $(NAME)/
	ln -sf $(NAME)-$(VERSION).tgz snapshots/latest.tgz

snapshots:
	mkdir $@

.PHONY: byte-compile clean dist

.SUFFIXES: .el .elc

