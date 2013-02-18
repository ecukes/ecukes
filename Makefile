.PHONY : all test clean

all: test

test: elpa
	carton exec ./test/ecukes-test

elpa:
	carton install

compile:
	carton exec emacs -Q -batch -L . -f batch-byte-compile *.el

clean:
	rm -rf elpa
	rm -rf *.elc test/*.elc
