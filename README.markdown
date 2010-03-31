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


## Usage
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

### step-definitions
The **step-definitions** dir should contain all project specific step
definitions. All step files in this folder must end with **-steps.el**.


## How to define steps
There are three different kind of steps: **regular**, **py-string** and **table**.

You define a step definition by calling the function **Given**,
**When**, **Then**, **And** or **But** passing two arguments (all
functions are really the same and it does not matter which you
choose). The first argument is the match string and the second is a function.

### Regular step
A regular step always only consumes one line. The most basic step
looks like this:
    Given something is true

The step definition would be:
    (Given "something is true"
           (lambda ()
             ;; Do something
             ))

The second argument could also have been the symbol of a function name:
    (Given "something is true" 'do-something)

Steps can take arbitrary many arguments. The arguments sent to the
step definition function are all regular expression match groupings
from the matching of the step name and the match string.

Send one argument:
    Given I am in buffer "somebuffer"

The translation:
    (Given "I am in buffer \"\\(.+\\)\""
       (lambda (buffer)
         ;; Do something with buffer
         ))

Send more than one argument:
    Given I am in buffer "somebuffer" with text "Some text"

The translation:
    (Given "I am in buffer \"\\(.+\\)\" with text \"\\(.+\\)\""
       (lambda (buffer text)
         ;; Do something with buffer and text
         ))

### Py String
A "Py String" step looks like this
    Given the following text:
      """
      some text
      """

The translation:
    (Given "the following text:"
           (lambda (text)
             ;; Do something with text
             ))

It is also possible to send arguments to a py string step. The py
string text will be the last argument.
    Given the following text in buffer "some buffer":
      """
      some text
      """

The translation:
    (Given "the following text in buffer \"\\(.+\\)\":"
           (lambda (buffer text)
             ;; Do something with buffer and text
             ))

### Table
A table step looks like this
    Given these meals:
      | meal      | price |
      | Hamburger | $4.50 |
      | Pizza     | $5.30 |

The translation:
    (Given "the following meals:"
           (lambda (meals)
             ;; Do something with text
             ))

The argument **meals** is a **ecukes-table** struct with two
attributes **header** and **rows**.

The header would in the above case be:
    ("meal" "price")

And the rows would be:
    (
     ("Hamburger" "$4.50")
     ("Pizza" "$5.30")
     )
     
To pick out the header and rows attribute from a table you do:
    (let* ((table ...)
           (header (ecukes-table-header table))
           (rows (ecukes-table-rows table)))
      ;; Do something with header and rows
      )
     
It is also possible to send arguments to a table step. The table list
will be the last argument.
    Given these meals at "fast food":
      | meal      | price |
      | Hamburger | $4.50 |
      | Pizza     | $5.30 |

The translation:
    (Given "these meals at \"\\(.+\\)\":"
           (lambda (restaurant meals)
             ;; Do something with restaurant and meals
             ))

### Calling steps from steps
To keep you steps DRY you can call steps from other steps like this:
    (Given "I go to a"
           (lambda ()
             ;; Go to a
             ))
     
    (Given "I go to b"
           (lambda ()
             ;; Go to b
             ))
     
    (Given "I go from a to b"
           (lambda ()
             (Given "I go to a")
             (Given "I go to b")))


## Hooks
There are two hooks you can use: **before** and **after**. The before
hook is executed before each scenario and after is executed after each scenario.

Hooks are useful if you test your program and change the state. Since
all scenarios execute in the same environment, the state change will
affect all scenarios after. You can solve that by resetting the state
in a before or after hook.

    (Before
     ;; Everything in here will be executed before each scenario
     )
     
    (After
     ;; Everything in here will be executed after each scenario
     )
     
Hooks are best placed in your projects **features/support.el** file.


## Example
A simple example of how to create a feature and corresponding step definitions.

### Feature
    Feature: Go to buffer
      In order to use more than one buffer
      As an Emacs user
      I want to switch buffer

      Scenario: Go to the messages buffer
        Given I am in "*scratch*" buffer
         When I press "C-h e"
         Then I should be in "*Messages*" buffer

      # More buffer scenarios ...

### Step definitions
The corresponding step definitions for the feature above would look
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
