Feature: Patterns

  Scenario: Both pattern and anti pattern
    Given feature "foo":
      """
      Feature: Foo
        Scenario: Bar
        Scenario: Baz
        Scenario: Qux
      """
    When I run ecukes "features/foo.feature --reporter spec -p ba[rz] -a r"
    Then I should see command output:
      """
      Feature: Foo
        Scenario: Baz

      1 scenarios (0 failed, 1 passed)
      0 steps
      """
