.PHONY : all test clean

all: test

test: clean-elc
	$(MAKE) quick-test
	$(MAKE) compile

quick-test: elpa
	carton exec ./test/ecukes-test

elpa:
	carton install

compile:
	carton exec emacs -Q -batch -L . -f batch-byte-compile *.el

clean: clean-elc
	rm -rf elpa

clean-elc:
	rm -rf *.elc test/*.elc
