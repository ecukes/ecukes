.PHONY : all test quick-test clean clean-elc ecukes

EMACS ?= emacs
SRC = $(filter-out %-pkg.el, $(wildcard *.el reporters/*.el))
ELC = $(SRC:.el=.elc)
CASK ?= cask
PKG_DIR := $(shell ${CASK} package-directory)
FEATURES = $(wildcard features/*.feature features/reporters/*.feature)

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
	@cask exec $(EMACS) -Q --script ecukes-byte-compile.el $<

clean: clean-elc
	rm -rf ${PKG_DIR}

clean-elc:
	rm -rf *.elc test/*.elc reporters/*.elc

ecukes: features/projects/super-project/.cask
	cask exec ecukes --script $(FEATURES) --dbg --tags ~@exclude

features/projects/super-project/.cask:
	cd features/projects/super-project && cask
