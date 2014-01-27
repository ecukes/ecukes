# Ecukes - Cucumber for Emacs [![Build Status](https://api.travis-ci.org/ecukes/ecukes.png?branch=master)](http://travis-ci.org/ecukes/ecukes)

[<img src="http://img.youtube.com/vi/4VcH4uAQJZI/0.jpg">](https://www.youtube.com/watch?v=4VcH4uAQJZI)

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

## Table of Contents

[Installation](#installation)  
[Usage](#usage)  

* [Modes](#modes)  
  * [Script (default)](#script-default)  
  * [No win](#no-win)  
  * [Win](#win)
* [Tags](#tags)  
* [Reporters](#reporters)  
* [Patterns and anti patterns](#patterns-and-anti-patterns)  
* [Only failing](#only-failing)

[Steps](#steps)  
* [Regular steps](#regular-steps)  
* [Table steps](#table-steps)  
* [Py String steps](#py-string-steps)  
* [Calling other steps](#calling-other-steps)  
* [Async steps](#async-steps)  
* [Listing steps](#listing-steps)

[Hooks](#hooks)
* [Setup](#setup)  
* [Before](#before)  
* [After](#after)  
* [Teardown](#teardown)  
* [Fail](#fail)

[Debugging](#debugging)  
[Example](#example)  
[Contribution](#contribution)  

## Installation

Add `ecukes` to your [Cask](https://github.com/rejeep/cask.el) file:

```lisp
(source melpa)

(package "super-duper" "0.0.1" "Super Duper.")

(development
 (depends-on "ecukes"))
```

Then run `cask` to install. To setup Ecukes for a project, run:

```
$ cask exec ecukes new

create features
create   step-definition
create     super-duper-steps.el
create   support
create     env.el
create   super-duper.feature
```

The project will now look like this:

```
$ ls super-duper

  super-duper/
  |-- README
  |-- features
  |   |-- step-definitions
  |   |   |-- super-duper-steps.el
  |   |-- super-duper.feature
  |   |-- support
  |       |-- env.el
  |-- super-duper.el
```

## Usage

To run Ecukes, use:

```bash
$ cask exec ecukes [COMMAND] [OPTIONS]
```

If you add a file called `.ecukes` in the project root, options are
automatically used for each run. For example:

```
--tags ~@exclude
--no-win
--reporter gangsta
```

### Modes

Ecukes can run in a few different modes: Script (`--script`), no win
(`--no-win`) and win (`--win`).

#### Script (default)

If no mode is specified, the features will run in script mode. This
will run the features as a batch job (`emacs --script` or `emacs --batch`).

This mode has a few quirks and it's recommended as a way to run
features in local development, because it's fast and the output is
printed directly (there's no way in Emacs to print to `stdout` or
`stderr` when not running as a batch job).

It is for example not possible to test font faces in script
mode. There's also no concept of lines so it's not possible to make
assertions on lines.

If you run features on travis, use `--no-win` mode.

Example:

```bash
$ cask exec ecukes
$ cask exec ecukes --script # same as above
```

#### No win

In this mode, Ecukes will run Emacs with the `-nw` option.

Example:

```bash
$ cask exec ecukes --no-win
```

#### Win

This will start up an Emacs window and run the features. To use this
mode, use the `--win` option.

Example:

```bash
$ cask exec ecukes --win
```

### Tags

Features and scenarios can be tagged using syntax `@tag`, for example:

```
@core
Feature: Foo

  @wip @io
  Scenario: Bar
  @io
  Scenario: Baz
```

All tags on the feature will be copied down to the scenarios.

You can choose to run scenarios tagged with a tag using:

```bash
$ cask exec ecukes --tags @io
```

or run scenarios **not** tagged with a tag using:

```bash
$ cask exec ecukes --tags ~@io
```

or combine the two:

```bash
$ cask exec ecukes --tags @io,~@wip
```

### Reporters

Ecukes has support for reporters so that you can get the output in a
few different ways depending on your philosophical bent. To list all
available reporters, run:

```bash
$ cask exec ecukes list-reporters
```

To use a specific reporter, run:

```bash
$ cask exec ecukes --reporter gangsta
```

Default reporter is `dot`.

### Patterns and anti patterns

You can run scenarios matching or not matching a pattern using
`--pattern` (`-p`) and `--anti-pattern` (`-a`) respectively. For example:

```bash
$ cask exec ecukes --pattern awesome
$ cask exec ecukes -p awesome almost-awesome
```

```bash
$ cask exec ecukes --anti-pattern awesome
$ cask exec ecukes -a awesome almost-awesome
```

### Only failing

If you have a failing scenario and you only want to run that, you can
use a tag, but you can also use the `--only-failing` (`-f`)
option. For example:

```bash
$ cask exec ecukes --only-failing
```

## Steps

There are three different kind of steps: Regular, table and py
string. Check out [Espuds](https://github.com/rejeep/espuds.el) for a
comprehensive collection of steps.

### Regular steps

A regular step always only consumes one line. The most basic step looks like this:

```
Given a known state
```

The corresponding step definition is:

```lisp
(Given "a known state"
  (lambda ()
    ;; ...
    ))
```

The second argument could also have been the symbol of a function name:

```lisp
(Given "a known state" 'do-something)
```

Steps accept arbitrary arguments. The arguments sent to the step
definition function are all regular expression match groupings from
the matching of the step name and the match string.

Single argument:

```
Given I am in buffer "buffer-name"
```

The corresponding step definition is:

```
(Given "I am in buffer \"\\(.+\\)\""
  (lambda (buffer-nane)
    ;; ...
    ))
```

Multiple arguments:

```
Given I am in buffer "buffer-name" with text "Foo"
```

The corresponding step definition is:

```lisp
(Given "I am in buffer \"\\(.+\\)\" with text \"\\(.+\\)\""
  (lambda (buffer text)
    ;; ...
    ))
```

### Table steps

A table step looks like this:

```
Given these meals:
  | meal      | price |
  | Hamburger | $4.50 |
  | Pizza     | $5.30 |
```

The corresponding step definition is:

```
(Given "these meals:"
  (lambda (meals)
    ;; ...
    ))
```

The argument `meals` is a simple list where the `car` of the list is
the header and the `cdr` or the list are the rows.

The header would in the above case be:

```lisp
("meal" "price")
```

And the rows would be:

```lisp
(("Hamburger" "$4.50")
 ("Pizza" "$5.30"))
```

To pick out the header and rows attribute from a table you do:

```
(let* ((table ...)
       (header (car table))
       (rows   (cdr table)))
  ;; ...
  )
```

It is also possible to send arguments to a table step. The table list
will be the last argument.

```
Given these meals at "fast food":
  | meal      | price |
  | Hamburger | $4.50 |
  | Pizza     | $5.30 |
```

The corresponding step definition is:

```lisp
(Given "these meals at \"\\(.+\\)\":"
  (lambda (restaurant meals)
    ;; ...
    ))
```

### Py String steps

A Py string step looks like this:

```
Given the following text:
  """
  some text
  """
```

The corresponding step definition is:

```lisp
(Given "the following text:"
  (lambda (text)
    ;; ...
    ))
```

It is also possible to send arguments to a Py string step. The
Py string will be the last argument.

```
Given the following text in buffer "buffer-name":
  """
  some text
  """
```

The corresponding step definition is:

```lisp
(Given "the following text in buffer \"\\(.+\\)\":"
  (lambda (buffer text)
    ;; ...
    ))
```

### Calling other steps

In order to keep your steps DRY, you can call steps from other steps
like this:

```lisp
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
```

You can also pass arguments when calling other steps, like this:

```lisp
(Given "I am in buffer \"%s\"" buffer-name-variable)
```

Or like this:

```lisp
(Given "I am in buffer \"buffer-name\"")
```

### Async steps

If you pass one extra parameter to a step definition, that is used as
a callback. Once called, the step is considered done.

```lisp
(When "^run command \\(.+\\)$"
  (lambda (command callback)
    (run-command command callback)))
```

If `callback` is not called within `10` seconds, it will fail
automatically. The timeout can be changed using the `--timeout`
(`-t`), for example:

```lisp
$ cask exec ecukes --timeout 30
```

Checkout the features for Async for examples how to use:
<https://github.com/rejeep/ecukes.el/blob/master/features/async.feature>

### Listing steps

You can list all defined steps with `list-steps`, for example:

```bash
$ cask exec ecukes list-steps
```

If you want to get the step description, use the `--with-doc` option:

```bash
$ cask exec ecukes list-steps --with-doc
```

And if you want to know where it is defined, use the `--with-file` option:

```bash
$ cask exec ecukes list-steps --with-file
```

### Hooks

Ecukes provides a few different hooks. They are useful if you test
your program and change the state in some feature. Since all scenarios
execute in the same environment, the state change will affect all
scenarios after. You can solve that by resetting the state in a before
or after hook.

Hooks should be placed in any file in the project `features/support`
directory.

#### Setup

Runs once before anything runs.

```lisp
(Setup
 ;; Run code
 )
```

#### Before

Runs once before each scenario runs.

```lisp
(Before
 ;; Run code
 )
```

#### After

Runs once after each scenario runs.

```lisp
(After
 ;; Run code
 )
```

#### Teardown

Runs once when everything is done.

```lisp
(Teardown
 ;; Run code
 )
```

#### Fail

Runs when a scenario fails.

```lisp
(Fail
 ;; Run code
 )
```

## Debugging

If you have a failing scenario and you cant figure out how why, first
off, try adding the `--debug` flag. For example:

```bash
$ cask exec ecukes --debug
```

Next step if that doesn't help, use the `--error-log` (`-l`)
argument. If an error occurs, the backtrace will be logged to that
file.

```bash
$ cask exec ecukes --error-log ecukes.err
```

## Example

To get an idea of how Ecukes can be used, here is an example from a
feature in [drag-stuff](https://github.com/rejeep/drag-stuff.el).

```
~/Code/drag-stuff $ cask exec ecukes features/line.feature --reporter spec
Feature: Drag line
  In order to move a line up and down
  As an Emacs user
  I want to drag it

  Background:
    Given I am in the buffer "*drag-stuff*"
    And the buffer is empty
    And I insert
    """
      line 1
      line 2
    """

  Scenario: Drag line up
    When I go to line "2"
    And I press "<M-up>"
    Then I should see:
    """
      line 2
      line 1
    """

  Scenario: Drag line down
    When I go to line "1"
    And I press "<M-down>"
    Then I should see:
    """
      line 2
      line 1
    """

2 scenarios (0 failed, 2 passed)
11 steps (0 failed, 11 passed)
```

## Contribution

All contributions are much welcome and appreciated!

Before submitting a patch, make sure to write a test for it (if
possible). Ecukes is unit tested with a testing framework called Ert
(Emacs Lisp Regression Testing). But most notably, it's tested using
Ecukes! :)

To run the tests, you have to install
[Cask](https://github.com/rejeep/cask.el) if you haven't already. Once
installed, run the `cask` command to install all dependencies.

To run all tests, simply run the `make` command.

You can always report issues on Github if you're not up to fixing it
yourself.

