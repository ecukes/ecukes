.PHONY : all test quick-test clean clean-elc ecukes

EMACS ?= emacs
SRC = $(filter-out %-pkg.el, $(wildcard *.el))
ELC = $(SRC:.el=.elc)
CASK ?= cask
PKG_DIR := $(shell ${CASK} package-directory)

all: test ecukes

test: clean-elc
	$(MAKE) quick-test
	$(MAKE) compile

quick-test: elpa
	cask exec ert-runner -l test/ecukes-test.el

elpa: ${PKG_DIR}
${PKG_DIR}: Cask
	${CASK} install
	touch $@

compile: $(ELC)
%.elc: %.el
	cask exec $(EMACS) -Q -batch -L . -f batch-byte-compile $<

clean: clean-elc
	rm -rf elpa

clean-elc:
	rm -rf *.elc test/*.elc

ecukes: features/projects/super-project/.cask
	cask exec ecukes --script features --dbg --tags ~@exclude
	cask exec ecukes --script features/reporters/* --dbg --tags ~@exclude

features/projects/super-project/.cask:
	cd features/projects/super-project && cask
