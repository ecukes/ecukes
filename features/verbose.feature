Feature: Verbose
  
  Scenario: Princ verbose, explicit
    Given feature "foo":
      """
      Feature: Foo
        Scenario: Hello world
          When I princ hello world
      """
    Given step definition:
      """
      (When "^I princ hello world$"
       (lambda () (princ "hello world")))
      """
    When I run ecukes "features/foo.feature --reporter spec --verbose"
    Then I should see command output:
      """
      Feature: Foo
        Scenario: Hello world
      hello world    When I princ hello world

      1 scenarios (0 failed, 1 passed)
      1 steps (0 failed, 0 skipped, 1 passed)
      """

  Scenario: Princ verbose, implicit
    Given feature "foo":
      """
      Feature: Foo
        Scenario: Hello world
          When I princ hello world
      """
    Given step definition:
      """
      (When "^I princ hello world$"
       (lambda () (princ "hello world")))
      """
    When I run ecukes "features/foo.feature --reporter spec"
    Then I should see command output:
      """
      Feature: Foo
        Scenario: Hello world
      hello world    When I princ hello world

      1 scenarios (0 failed, 1 passed)
      1 steps (0 failed, 0 skipped, 1 passed)
      """

  Scenario: Princ quiet
    Given feature "foo":
      """
      Feature: Foo
        Scenario: Hello world
          When I princ hello world
      """
    Given step definition:
      """
      (When "^I princ hello world$"
       (lambda () (princ "hello world")))
      """
    When I run ecukes "features/foo.feature --reporter spec --quiet"
    Then I should see command output:
      """
      Feature: Foo
        Scenario: Hello world
          When I princ hello world

      1 scenarios (0 failed, 1 passed)
      1 steps (0 failed, 0 skipped, 1 passed)
      """
  
  Scenario: Message verbose, explicit
    Given feature "foo":
      """
      Feature: Foo
        Scenario: Hello world
          When I message hello world
      """
    Given step definition:
      """
      (When "^I message hello world$"
       (lambda () (message "hello world")))
      """
    When I run ecukes "features/foo.feature --reporter spec --verbose"
    Then I should see command output:
      """
      hello world
      Feature: Foo
        Scenario: Hello world
          When I message hello world

      1 scenarios (0 failed, 1 passed)
      1 steps (0 failed, 0 skipped, 1 passed)
      """

  Scenario: Message verbose, implicit
    Given feature "foo":
      """
      Feature: Foo
        Scenario: Hello world
          When I message hello world
      """
    Given step definition:
      """
      (When "^I message hello world$"
       (lambda () (message "hello world")))
      """
    When I run ecukes "features/foo.feature --reporter spec"
    Then I should see command output:
      """
      hello world
      Feature: Foo
        Scenario: Hello world
          When I message hello world

      1 scenarios (0 failed, 1 passed)
      1 steps (0 failed, 0 skipped, 1 passed)
      """

  Scenario: Message quiet
    Given feature "foo":
      """
      Feature: Foo
        Scenario: Hello world
          When I message hello world
      """
    Given step definition:
      """
      (When "^I message hello world$"
       (lambda () (message "hello world")))
      """
    When I run ecukes "features/foo.feature --reporter spec"
    Then I should see command output:
      """
      Feature: Foo
        Scenario: Hello world
          When I message hello world

      1 scenarios (0 failed, 1 passed)
      1 steps (0 failed, 0 skipped, 1 passed)
      """
