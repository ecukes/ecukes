Feature: Tags

  Scenario: Run only
    Given feature "foo":
      """
      Feature: Foo
        @wip
        Scenario: Bar
        Scenario: Baz
      """
    When I run ecukes "features/foo.feature --tags @wip --reporter spec"
    Then I should see command output:
      """
      Feature: Foo

        @wip
        Scenario: Bar

      1 scenarios (0 failed, 1 passed)
      0 steps
      """

  Scenario: Run except
    Given feature "foo":
      """
      Feature: Foo
        @wip
        Scenario: Bar
        Scenario: Baz
      """
    When I run ecukes "features/foo.feature --tags ~@wip --reporter spec"
    Then I should see command output:
      """
      Feature: Foo

        Scenario: Baz

      1 scenarios (0 failed, 1 passed)
      0 steps
      """
    
  Scenario: Run only and except
    Given feature "foo":
      """
      Feature: Foo
        @wip
        Scenario: Bar
        @tmp
        Scenario: Baz
      """
    When I run ecukes "features/foo.feature --tags @wip,~@tmp --reporter spec"
    Then I should see command output:
      """
      Feature: Foo

        @wip
        Scenario: Bar

      1 scenarios (0 failed, 1 passed)
      0 steps
      """
