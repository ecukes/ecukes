Feature: Anti Pattern

  Scenario: No scenario matching anti pattern
    Given feature "foo":
      """
      Feature: Foo
        Scenario: Bar
        Scenario: Baz
      """
    When I run ecukes "features/foo.feature --reporter spec -a qux"
    Then I should see command output:
      """
      Feature: Foo
        Scenario: Bar

        Scenario: Baz

      2 scenarios (0 failed, 2 passed)
      0 steps
      """

  Scenario: With scenario matching anti pattern
    Given feature "foo":
      """
      Feature: Foo
        Scenario: Bar
        Scenario: Baz
      """
    When I run ecukes "features/foo.feature --reporter spec -a bar"
    Then I should see command output:
      """
      Feature: Foo
        Scenario: Baz

      1 scenarios (0 failed, 1 passed)
      0 steps
      """

  Scenario: Multiple anti patterns
    Given feature "foo":
      """
      Feature: Foo
        Scenario: Bar
        Scenario: Baz
      """
    When I run ecukes "features/foo.feature --reporter spec -a bar baz"
    Then I should see command output:
      """
      Feature: Foo
      0 scenarios
      0 steps
      """

  Scenario: Regex
    Given feature "foo":
      """
      Feature: Foo
        Scenario: Bar
        Scenario: Baz
        Scenario: Qux
      """
    When I run ecukes "features/foo.feature --reporter spec -a ba[rz]"
    Then I should see command output:
      """
      Feature: Foo
        Scenario: Qux

      1 scenarios (0 failed, 1 passed)
      0 steps
      """
