Feature: Reporters

  Scenario: Invalid reporter
    Given feature "foo":
      """
      Feature: Foo
      """
    When I run ecukes "features/foo.feature --reporter ehh"
    Then I should see command error:
      """
      Invalid reporter: ehh
      """

  Scenario: List reporters
    When I run ecukes "list-reporters"
    Then I should see list of reporters:
