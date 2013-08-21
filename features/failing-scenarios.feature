Feature: Failing Scenarios

  Scenario: No previously failing scenarios
    Given feature "foo":
      """
      Feature: Foo
        Scenario: Bar
      """
    When I run ecukes "features/foo.feature --reporter spec -f"
    Then I should see command output:
      """
      Feature: Foo
        Scenario: Bar

      1 scenarios (0 failed, 1 passed)
      0 steps
      """

  Scenario: Run previously failing scenarios
    Given feature "foo":
      """
      Feature: Foo
        Scenario: Bar
          Given failure

        Scenario: Baz
      """
    And step definition:
      """
      (Given "^failure$"
       (lambda () (error "failure")))
      """
    When I run ecukes "features/foo.feature --reporter spec"
    Then I should see command error:
      """
      Feature: Foo
        Scenario: Bar
          Given failure
            failure

        Scenario: Baz

      2 scenarios (1 failed, 1 passed)
      1 steps (1 failed, 0 skipped, 0 passed)
      """
    Given step definition:
      """
      (Given "^failure$"
       (lambda () ))
      """
    When I run ecukes "features/foo.feature --reporter spec -f"
    Then I should see command output:
      """
      Feature: Foo
        Scenario: Bar
          Given failure

      1 scenarios (0 failed, 1 passed)
      1 steps (0 failed, 0 skipped, 1 passed)
      """

  Scenario: Run previously failing scenarios fix and then run again
    Given feature "foo":
      """
      Feature: Foo
        Scenario: Bar
          Given failure

        Scenario: Baz
      """
    And step definition:
      """
      (Given "^failure$"
       (lambda () (error "failure")))
      """
    When I run ecukes "features/foo.feature --reporter spec"
    Then I should see command error:
      """
      Feature: Foo
        Scenario: Bar
          Given failure
            failure

        Scenario: Baz

      2 scenarios (1 failed, 1 passed)
      1 steps (1 failed, 0 skipped, 0 passed)
      """
    Given step definition:
      """
      (Given "^failure$"
       (lambda () ))
      """
    When I run ecukes "features/foo.feature --reporter spec -f"
    Then I should see command output:
      """
      Feature: Foo
        Scenario: Bar
          Given failure

      1 scenarios (0 failed, 1 passed)
      1 steps (0 failed, 0 skipped, 1 passed)
      """
    When I run ecukes "features/foo.feature --reporter spec -f"
    Then I should see command output:
      """
      Feature: Foo
        Scenario: Bar
          Given failure

        Scenario: Baz

      2 scenarios (0 failed, 2 passed)
      1 steps (0 failed, 0 skipped, 1 passed)
      """
