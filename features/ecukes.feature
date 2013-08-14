Feature: Ecukes

  Scenario: Run single feature
    Given feature "foo":
      """
      Feature: Foo

        Scenario: Bar
      """
    When I run ecukes "features/foo.feature"
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
    When I run ecukes "features/foo.feature features/bar.feature"
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
    When I run ecukes "features"
    Then I should see command output:
      """
      Feature: Foo


        Scenario: Bar

      Feature: Bar


        Scenario: Baz

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
    When I run ecukes ""
    Then I should see command output:
      """
      Feature: Foo


        Scenario: Bar

      Feature: Bar


        Scenario: Baz

      2 scenarios (0 failed, 2 passed)
      0 steps
      """
