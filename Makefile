.PHONY : all test clean
EMACS ?= emacs
SRC = $(filter-out %-pkg.el, $(wildcard *.el))
ELC = $(SRC:.el=.elc)

all: test

test: clean-elc
	$(MAKE) quick-test
	$(MAKE) compile

quick-test: elpa
	cask exec ert-runner run -l test/ecukes-test.el

elpa:
	cask install

compile: $(ELC)
%.elc: %.el
	cask exec $(EMACS) -Q -batch -L . -f batch-byte-compile $<

clean: clean-elc
	rm -rf elpa

clean-elc:
	rm -rf *.elc test/*.elc
