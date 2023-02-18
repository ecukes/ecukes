.PHONY : all test unit-test clean clean-elc ecukes

EMACS ?= emacs
CASK ?= cask
EASK ?= eask
FEATURES = $(wildcard features/*.feature features/reporters/*.feature)

all: test

test: clean-elc unit-test ecukes compile

unit-test:
	$(EASK) install-deps --dev
	$(EASK) exec ert-runner -L test -L . test/ecukes*.el

$(PKG_DIR):
	$(EASK) link add ecukes .
	touch $@

compile:
	@$(EASK) compile

clean: clean-elc
	rm -rf $(PKG_DIR)

clean-elc:
	@$(EASK) clean elc
	rm -rf test/*.elc reporters/*.elc

ecukes:
	cd features/projects/super-project
	$(EASK) install-deps --dev
	$(EASK) exec ecukes --script $(FEATURES)
