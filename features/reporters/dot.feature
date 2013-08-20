Feature: Dot

  Background:
    Given step definition:
      """
      (Given "^failure$"
       (lambda () (error "failure")))

      (Given "^a failing table:$"
       (lambda (table) (error "failing table")))

      (Given "^a successful table:$"
       (lambda (table) "table"))

      (Given "^a failing py string:$"
       (lambda (py-string) (error "failing py string")))

      (Given "^a successful py string:$"
       (lambda (py-string) "py-string"))

      (Given "^success$"
       (lambda () "success"))
      """

  Scenario: No scenarios
    Given feature "foo":
      """
      Feature: Foo
      """
    When I run ecukes "features/foo.feature --reporter dot"
    Then I should see command output:
      """
      0 scenarios
      0 steps
      """

  Scenario: Single scenario, no steps
    Given feature "foo":
      """
      Feature: Foo
        Scenario: Bar
      """
    When I run ecukes "features/foo.feature --reporter dot"
    Then I should see command output:
      """
      .

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
    When I run ecukes "features/foo.feature --reporter dot"
    Then I should see command output:
      """
      ..

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
    When I run ecukes "features/foo.feature --reporter dot"
    Then I should see command output:
      """
      .

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
    When I run ecukes "features/foo.feature --reporter dot"
    Then I should see command output:
      """
      ..

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
    When I run ecukes "features/foo.feature --reporter dot"
    Then I should see command error:
      """
      .

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
    When I run ecukes "features/foo.feature --reporter dot"
    Then I should see command error:
      """
      ..

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
    When I run ecukes "features/foo.feature --reporter dot"
    Then I should see command error:
      """
      .

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
    When I run ecukes "features/foo.feature --reporter dot"
    Then I should see command error:
      """
      ..

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

  Scenario: Failing table
    Given feature "foo":
      """
      Feature: Foo
        Scenario: Bar
          Given a failing table:
            | baz |
            | qux |
      """
    When I run ecukes "features/foo.feature --reporter dot"
    Then I should see command error:
      """
      .

        Scenario: Bar
          Given a failing table:
            | baz |
            | qux |
            failing table

      1 scenarios (1 failed, 0 passed)
      1 steps (1 failed, 0 skipped, 0 passed)
      """

  Scenario: Failing py string
    Given feature "foo":
      """
      Feature: Foo
        Scenario: Bar
          Given a failing py string:
            """
            failing
            """
      """
    When I run ecukes "features/foo.feature --reporter dot"
    Then I should see command error:
      """
      .

        Scenario: Bar
          Given a failing py string:
            """
            failing
            """
            failing py string

      1 scenarios (1 failed, 0 passed)
      1 steps (1 failed, 0 skipped, 0 passed)
      """
