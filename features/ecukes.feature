Feature: Ecukes

  Scenario: Run single feature
    Given feature "foo":
      """
      Feature: Foo
        Scenario: Bar
      """
    When I run ecukes "features/foo.feature --reporter spec"
    Then I should see command output:
      """
      Feature: Foo
        Scenario: Bar

      1 scenarios (0 failed, 1 passed)
      0 steps
      """

  Scenario: Run multiple features
    Given feature "foo":
      """
      Feature: Foo
        Scenario: Bar
      """
    And feature "bar":
      """
      Feature: Bar
        Scenario: Baz
      """
    When I run ecukes "features/foo.feature features/bar.feature --reporter spec"
    Then I should see command output:
      """
      Feature: Bar
        Scenario: Baz

      Feature: Foo
        Scenario: Bar

      2 scenarios (0 failed, 2 passed)
      0 steps
      """

  Scenario: Run all features - explicit
    Given feature "foo":
      """
      Feature: Foo
        Scenario: Bar
      """
    And feature "bar":
      """
      Feature: Bar
        Scenario: Baz
      """
    When I run ecukes "features --reporter spec"
    Then I should see command output:
      """
      Feature: Bar
        Scenario: Baz

      Feature: Foo
        Scenario: Bar

      2 scenarios (0 failed, 2 passed)
      0 steps
      """

  Scenario: Run all features - implicit
    Given feature "foo":
      """
      Feature: Foo
        Scenario: Bar
      """
    And feature "bar":
      """
      Feature: Bar
        Scenario: Baz
      """
    When I run ecukes "--reporter spec"
    Then I should see command output:
      """
      Feature: Bar
        Scenario: Baz

      Feature: Foo
        Scenario: Bar

      2 scenarios (0 failed, 2 passed)
      0 steps
      """

  Scenario: Run all features, recursive
    Given feature "foo":
      """
      Feature: Foo
        Scenario: Bar
      """
    And feature "deep/bar":
      """
      Feature: Bar
        Scenario: Baz
      """
    When I run ecukes "--reporter spec"
    Then I should see command output:
      """
      Feature: Bar
        Scenario: Baz

      Feature: Foo
        Scenario: Bar

      2 scenarios (0 failed, 2 passed)
      0 steps
      """
