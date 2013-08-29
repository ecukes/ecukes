Feature: Config
  
  Scenario: Run single feature
    Given feature "foo":
      """
      Feature: Foo
        Scenario: Bar
        @wip
        Scenario: Baz
      """
    And I create file ".ecukes" with content:
      """
      --tags @wip
      """
    When I run ecukes "features/foo.feature --reporter spec"
    Then I should see command output:
      """
      Feature: Foo
        @wip
        Scenario: Baz

      1 scenarios (0 failed, 1 passed)
      0 steps
      """
