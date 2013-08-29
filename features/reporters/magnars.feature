Feature: Magnars

  Background:
    Given step definition:
      """
      (Given "^failure$"
       (lambda () (error "failure")))

      (Given "^huge failure$"
       (lambda () (error "huge\nfailure")))

      (Given "^a failing table:$"
       (lambda (table) (error "failing table")))

      (Given "^a successful table:$"
       (lambda (table) ))

      (Given "^a failing py string:$"
       (lambda (py-string) (error "failing py string")))

      (Given "^a successful py string:$"
       (lambda (py-string) ))

      (Given "^success$"
       (lambda ()))
      """

  Scenario: No scenarios, without description
    Given feature "foo":
      """
      Feature: Foo
      """
    When I run ecukes "features/foo.feature --reporter magnars"
    Then I should see command output:
      """
      Feature: Foo

      0 scenarios
      0 steps
      """

  Scenario: No scenarios, with description
    Given feature "foo":
      """
      Feature: Foo
        In order to do something
        As a someone
        Then I should be told to do that something
      """
    When I run ecukes "features/foo.feature --reporter magnars"
    Then I should see command output:
      """
      Feature: Foo
        In order to do something
        As a someone
        Then I should be told to do that something

      0 scenarios
      0 steps
      """

  Scenario: Feature with description, single scenario
    Given feature "foo":
      """
      Feature: Foo
        In order to do something
        As a someone
        Then I should be told to do that something

        Scenario: Bar
      """
    When I run ecukes "features/foo.feature --reporter spec"
    Then I should see command output:
      """
      Feature: Foo
        In order to do something
        As a someone
        Then I should be told to do that something

        Scenario: Bar

      1 scenarios (0 failed, 1 passed)
      0 steps
      """

  Scenario: Single scenario, no steps
    Given feature "foo":
      """
      Feature: Foo
        Scenario: Bar
      """
    When I run ecukes "features/foo.feature --reporter magnars"
    Then I should see command output:
      """
      Feature: Foo
        Scenario: Bar

      1 scenarios (0 failed, 1 passed)
      0 steps
      """

  Scenario: Multiple scenarios, no steps
    Given feature "foo":
      """
      Feature: Foo
        Scenario: Bar
        Scenario: Baz
      """
    When I run ecukes "features/foo.feature --reporter magnars"
    Then I should see command output:
      """
      Feature: Foo
        Scenario: Bar
        Scenario: Baz

      2 scenarios (0 failed, 2 passed)
      0 steps
      """

  Scenario: Single scenario, with passing steps
    Given feature "foo":
      """
      Feature: Foo
        Scenario: Bar
          Given success
      """
    When I run ecukes "features/foo.feature --reporter magnars"
    Then I should see command output:
      """
      Feature: Foo
        Scenario: Bar

      1 scenarios (0 failed, 1 passed)
      1 steps (0 failed, 0 skipped, 1 passed)
      """

  Scenario: Multiple scenarios, with passing steps
    Given feature "foo":
      """
      Feature: Foo
        Scenario: Bar
          Given success

        Scenario: Baz
          Given success
      """
    When I run ecukes "features/foo.feature --reporter magnars"
    Then I should see command output:
      """
      Feature: Foo
        Scenario: Bar
        Scenario: Baz

      2 scenarios (0 failed, 2 passed)
      2 steps (0 failed, 0 skipped, 2 passed)
      """

  Scenario: Single scenario, with failing steps
    Given feature "foo":
      """
      Feature: Foo
        Scenario: Bar
          Given failure
      """
    When I run ecukes "features/foo.feature --reporter magnars"
    Then I should see command error:
      """
      Feature: Foo
        Scenario: Bar
          Given failure
            failure


      1 scenarios (1 failed, 0 passed)
      1 steps (1 failed, 0 skipped, 0 passed)
      """

  Scenario: Multiple scenarios, with failing steps
    Given feature "foo":
      """
      Feature: Foo
        Scenario: Bar
          Given failure

        Scenario: Baz
          Given failure
      """
    When I run ecukes "features/foo.feature --reporter magnars"
    Then I should see command error:
      """
      Feature: Foo
        Scenario: Bar
          Given failure
            failure

        Scenario: Baz
          Given failure
            failure


      2 scenarios (2 failed, 0 passed)
      2 steps (2 failed, 0 skipped, 0 passed)
      """

  Scenario: Single scenario, with skipped steps
    Given feature "foo":
      """
      Feature: Foo
        Scenario: Bar
          Given failure
          Given success
      """
    When I run ecukes "features/foo.feature --reporter magnars"
    Then I should see command error:
      """
      Feature: Foo
        Scenario: Bar
          Given failure
            failure
          Given success


      1 scenarios (1 failed, 0 passed)
      2 steps (1 failed, 1 skipped, 0 passed)
      """

  Scenario: Multiple scenarios, with skipped steps
    Given feature "foo":
      """
      Feature: Foo
        Scenario: Bar
          Given failure
          Given success

        Scenario: Baz
          Given failure
          Given success
      """
    When I run ecukes "features/foo.feature --reporter magnars"
    Then I should see command error:
      """
      Feature: Foo
        Scenario: Bar
          Given failure
            failure
          Given success

        Scenario: Baz
          Given failure
            failure
          Given success


      2 scenarios (2 failed, 0 passed)
      4 steps (2 failed, 2 skipped, 0 passed)
      """

  Scenario: Single scenario, no steps, with tags
    Given feature "foo":
      """
      Feature: Foo
        @bar
        Scenario: Bar
      """
    When I run ecukes "features/foo.feature --reporter magnars"
    Then I should see command output:
      """
      Feature: Foo
        @bar
        Scenario: Bar

      1 scenarios (0 failed, 1 passed)
      0 steps
      """

  Scenario: Single scenario, with steps, with tags
    Given feature "foo":
      """
      Feature: Foo
        @bar
        Scenario: Bar
          Given success
      """
    When I run ecukes "features/foo.feature --reporter magnars"
    Then I should see command output:
      """
      Feature: Foo
        @bar
        Scenario: Bar

      1 scenarios (0 failed, 1 passed)
      1 steps (0 failed, 0 skipped, 1 passed)
      """

  Scenario: Multiple scenarios, no steps, with tags
    Given feature "foo":
      """
      Feature: Foo
        @bar
        Scenario: Bar

        @baz
        Scenario: Baz
      """
    When I run ecukes "features/foo.feature --reporter magnars"
    Then I should see command output:
      """
      Feature: Foo
        @bar
        Scenario: Bar
        @baz
        Scenario: Baz

      2 scenarios (0 failed, 2 passed)
      0 steps
      """

  Scenario: Multiple scenarios, with steps, with tags
    Given feature "foo":
      """
      Feature: Foo
        @bar
        Scenario: Bar
          Given success

        @baz
        Scenario: Baz
          Given success
      """
    When I run ecukes "features/foo.feature --reporter magnars"
    Then I should see command output:
      """
      Feature: Foo
        @bar
        Scenario: Bar
        @baz
        Scenario: Baz

      2 scenarios (0 failed, 2 passed)
      2 steps (0 failed, 0 skipped, 2 passed)
      """

  Scenario: Multiple tags
    Given feature "foo":
      """
      Feature: Foo
        @bar @baz @qux
        Scenario: Bar
      """
    When I run ecukes "features/foo.feature --reporter magnars"
    Then I should see command output:
      """
      Feature: Foo
        @bar @baz @qux
        Scenario: Bar

      1 scenarios (0 failed, 1 passed)
      0 steps
      """

  # # Scenario: Multiline error
  # #   Given feature "foo":
  # #     """
  # #     Feature: Foo
  # #       Scenario: Bar
  # #         Given huge failure
  # #     """
  # #   When I run ecukes "features/foo.feature --reporter magnars"
  # #   Then I should see command error:
  # #     """
  # #     Feature: Foo

  # #       Scenario: Bar
  # #         Given huge failure
  # #           huge
  # #           failure

  # #     1 scenarios (1 failed, 0 passed)
  # #     1 steps (1 failed, 0 skipped, 0 passed)
  # #     """

  Scenario: Multiple features
    Given feature "foo":
      """
      Feature: Foo
        Scenario: Bar
          Given success
      """
    Given feature "bar":
      """
      Feature: Bar
        Scenario: Foo
          Given success
      """
    When I run ecukes "features/foo.feature features/bar.feature --reporter magnars"
    Then I should see command output:
      """
      Feature: Bar
        Scenario: Foo

      Feature: Foo
        Scenario: Bar

      2 scenarios (0 failed, 2 passed)
      2 steps (0 failed, 0 skipped, 2 passed)
      """

  Scenario: Failing table
    Given feature "foo":
      """
      Feature: Foo
        Scenario: Bar
          Given a failing table:
            | baz |
            | qux |
      """
    When I run ecukes "features/foo.feature --reporter magnars"
    Then I should see command error:
      """
      Feature: Foo
        Scenario: Bar
          Given a failing table:
            | baz |
            | qux |
            failing table


      1 scenarios (1 failed, 0 passed)
      1 steps (1 failed, 0 skipped, 0 passed)
      """

  Scenario: Successful table
    Given feature "foo":
      """
      Feature: Foo
        Scenario: Bar
          Given a successful table:
            | baz |
            | qux |
      """
    When I run ecukes "features/foo.feature --reporter magnars"
    Then I should see command output:
      """
      Feature: Foo
        Scenario: Bar

      1 scenarios (0 failed, 1 passed)
      1 steps (0 failed, 0 skipped, 1 passed)
      """

  Scenario: Background no steps
    Given feature "foo":
      """
      Feature: Foo
        Background:
      """
    When I run ecukes "features/foo.feature --reporter magnars"
    Then I should see command output:
      """
      Feature: Foo
        Background:

      0 scenarios
      0 steps
      """

  Scenario: Background with scenarios
    Given feature "foo":
      """
      Feature: Foo
        Background:
          Given success

        Scenario: Bar
          Given success
      """
    When I run ecukes "features/foo.feature --reporter magnars"
    Then I should see command output:
      """
      Feature: Foo
        Background:
          Given success

        Scenario: Bar

      1 scenarios (0 failed, 1 passed)
      2 steps (0 failed, 0 skipped, 2 passed)
      """
