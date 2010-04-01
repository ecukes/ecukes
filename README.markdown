# Ecukes - Cucumber for Emacs
There are plenty of unit/regression testing tools for Emacs, and even
some for functional testing. What Emacs is missing though is a really
good testing framework for integration testing. This is where
[Ecukes](http://github.com/rejeep/ecukes) comes in.

[Cucumber](http://cukes.info/) is a great integration testing tool,
used mostly for testing web applications. Ecukes is Cucumber for
Emacs. No, it's **not** a major mode to edit feature files. It is a
package that makes it possible to write Cucumber like tests for your
Emacs packages.

Ecukes is not a complete clone of Cucumber and is not intended to
be. If however Ecukes is missing some feature that you feel really
should be included, please make a bug report.

If you don't know anything about Cucumber I suggest you read up a bit
about it before continuing with Ecukes.


## Installation
Start by download Ecukes. In the base dir there's an executable file
called **ecukes**. That's the script you use to run your
features. Make sure that file is in your path:
    $ export PATH="$PATH:/path/to/ecukes"

Each step in your features needs to be defined so that Ecukes know
what to do with them. You will probably end up writing a lot of step
definitions yourself, but a good start is to use
[Espuds](http://github.com/rejeep/espuds), which includes the most
commonly used definitions.

### File Structure
Lets say you have a project, lets call it **super-project**, that you
want to test with Ecukes. Create a file structure looking like this:

    super-project/
    |-- README
    |-- features
    |   |-- step-definitions
    |   |   `-- super-project-steps.el
    |   |-- super-project.feature
    |   `-- support.el
    `-- super-project.el

You can also use a script that comes with Ecukes to create the
files. Go to your project root dir and enter:
    $ cd /path/to/super-project
    $ ecukes-init

### features
The features folder should contain:

* The file **support.el**
* The folder **step-definitions**
* A bunch of feature files

### support.el
**support.el** is loaded once before any feature is runned. If you use
**Espuds**, this is the place to load it. You should also load your
**project here. The super-project's support.el could look like this:

    (add-to-list 'load-path "/path/to/super-project")
    (require 'super-project)

    (add-to-list 'load-path "/path/to/espuds")
    (require 'espuds)
    
    
## Usage
See [Wiki](http://github.com/rejeep/ecukes/wikis)


## Running the features
Make sure you have the **ecukes** executable in your **PATH**. Then
run your features by giving them as arguments to the **ecukes** command:

Run single feature:
    $ ecukes super-project/features/super-project.feature

Run all features:
    $ ecukes super-project/features


## Gotchas

### Messages
Some of the functions you call in your step definitions produces
output that clutters the Ecukes output. To avoid this, the message
function has been adviced. Hence using message in your step
definitions will not work. Use some other Elisp function to procuce
output. For example: print, princ or prin1

However, if a function produces a message, you can reach it through
the variable **ecukes-last-message**, which holds the last message.


## Contributing
All contributions are much welcome and appreciated!

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

You can also run specific tests with:
    $ /path/to/ecukes/test/ecukes-test some-test.el ...
