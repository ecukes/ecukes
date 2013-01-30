.PHONY : all test clean

all: test

test: elpa
	carton exec ./test/ecukes-test

elpa:
	carton install

clean:
	rm -rf elpa
	rm -rf *.elc test/*.elc
