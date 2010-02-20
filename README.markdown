# Ecukes - Cucumber for Emacs
There are plenty of unit and regression testing tools for Emacs out
there, and even some for functional testing. What Emacs is missing
though is a really good testing framework for integration
testing. This is where [Ecukes](http://github.com/rejeep/ecukes) comes in.

[Cucumber](http://cukes.info/) is a great integration testing tool,
used mostly for testing web applications. Ecukes is Cucumber for
Emacs. No, it's **not** a major mode for editing feature files. It is
a package that makes it possible to write Cucumber like tests for your
Emacs packages. **Note** that all Cucumber features are not
included. Only the once that makes sense to have in Emacs.

If you don't know anything about Cucumber I suggest you read up a bit
about it before continuing with Ecukes.

## Installation

## Examples

### Features
        
### Step definitions

## Running the features

## Contributing

### Testing
Before submitting a patch, make sure to write a test for it (if
possible). Ecukes is tested with a testing framework called
[Emacs Lisp Regression Testing](http://github.com/emacsmirror/ert).

To run the tests, you have to fetch two packages: **ert** and **el-mock**.
    $ cd /path/to/ecukes
    $ git submodule init
    $ git submodule update
    
Then run the tests with:
    $ /path/to/ecukes/test/ecukes-test
