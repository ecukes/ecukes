# Ecukes - Cucumber for Emacs
There are plenty of unit and regression testing tools for Emacs out
there, and even some for functional testing. What Emacs is missing
though is a really good testing framework for integration
testing. This is where [Ecukes](http://github.com/rejeep/ecukes) comes in.

[Cucumber](http://cukes.info/) is a great integration testing tool,
used mostly for testing web applications. Ecukes is Cucumber for
Emacs. No, it's **not** a major mode for editing feature files. It is
a package that makes it possible to write Cucumber like tests for your
Emacs packages. **Note** that Ecukes is **not** a complete clone of
Cucumber, so there might be some functionality missing. If Ecukes is
missing something that you feel really should be included, please make
a bug report.

If you don't know anything about Cucumber I suggest you read up a bit
about it before continuing with Ecukes.

## Installation
Start by download Ecukes. In the base dir of Ecukes, there's an
executable file called **ecukes**. That's the script you use to run
your features. Make sure that file is in your path:
    $ export PATH="$PATH:/path/to/ecukes"

You should now be able to run the **ecukes** command.

Ecukes is the program that runs the features. Each step in your
features needs to be defined so that Ecukes knows what to do. You'll
probably end up writing a lot of step definitions youself. But a good
start is to use
[Espuds](http://github.com/rejeep/espuds),
which includes the most commonly used definitions.

## Usage
Lets say you have a project, lets call it **super-project**, that you
want to test with Ecukes. Create a file structure looking something
like this:

    super-project/
    |-- README
    |-- features
    |   |-- step-definitions
    |   |   `-- super-project-steps.el
    |   |-- super-project.feature
    |   `-- support.el
    `-- super-project.el

### features
This folder should contain:

* The file support.el
* The folder step-definitions
* All feature files

### support.el
Loaded once before features are runned. If you use **Espuds**, this is
where you should load it.

### step-definitions
Project specific step definitions. All step files in this folder must
end with **-steps.el**.


## Example

### Feature
    Feature: Go to buffer
      In order to use more than one buffer
      As an Emacs user
      I want to switch buffer
      
      Scenario: Go to the messages buffer
        Given I am in "*scratch*" buffer
         When I press "C-h e"
         Then I should be in "*Messages*" buffer
        
### Step definitions
The corresponding step definitons for the feature above would be
something like this:

    (Given "I am in \"\\(.+\\)\" buffer"
           (lambda (buffer)
             (switch-to-buffer (get-buffer-create buffer))))
     
    (When "I press \"\\(.+\\)\""
         (lambda (key)
           (execute-kbd-macro (edmacro-parse-keys key))))
     
    (Then "I should be in \"\\(.+\\)\" buffer"
          (lambda (buffer)
            (assert (equal buffer (buffer-name)))))
    

## Running the features
If you have the **ecukes** executable in you **PATH**, to run your
features, you give them as arguments to the **ecukes** command:

Run single feature:
    $ ecukes super-project/features/super-project.feature
    
Run all features:
    $ ecukes super-project/features


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
