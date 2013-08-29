Feature: Pattern

  Scenario: No scenario matching pattern
    Given feature "foo":
      """
      Feature: Foo
        Scenario: Bar
        Scenario: Baz
      """
    When I run ecukes "features/foo.feature --reporter spec -p qux"
    Then I should see command output:
      """
      Feature: Foo
      0 scenarios
      0 steps
      """

  Scenario: With scenario matching pattern
    Given feature "foo":
      """
      Feature: Foo
        Scenario: Bar
        Scenario: Baz
      """
    When I run ecukes "features/foo.feature --reporter spec -p bar"
    Then I should see command output:
      """
      Feature: Foo
        Scenario: Bar

      1 scenarios (0 failed, 1 passed)
      0 steps
      """

  Scenario: Multiple patterns
    Given feature "foo":
      """
      Feature: Foo
        Scenario: Bar
        Scenario: Baz
      """
    When I run ecukes "features/foo.feature --reporter spec -p bar baz"
    Then I should see command output:
      """
      Feature: Foo
        Scenario: Bar

        Scenario: Baz

      2 scenarios (0 failed, 2 passed)
      0 steps
      """

  Scenario: Regex
    Given feature "foo":
      """
      Feature: Foo
        Scenario: Bar
        Scenario: Baz
      """
    When I run ecukes "features/foo.feature --reporter spec -p ba[rz]"
    Then I should see command output:
      """
      Feature: Foo
        Scenario: Bar

        Scenario: Baz

      2 scenarios (0 failed, 2 passed)
      0 steps
      """
