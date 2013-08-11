Feature: Steps

  Scenario: Missing step
    Given feature "foo":
      """
      Feature: Foo

        Scenario: Bar
          Given a missing step
      """
    When I run ecukes "features/foo.feature"
    Then I should see command output:
      """
      Some steps does not have a matching definition. Please implement the following step definitions:

      (Given "^a missing step$"
        (lambda ()

          ))
      """

  Scenario: Missing steps
    Given feature "foo":
      """
      Feature: Foo

        Scenario: Bar
          Given a missing step

        Scenario: Baz
          Given another missing step
      """
    When I run ecukes "features/foo.feature"
    Then I should see command output:
      """
      Some steps does not have a matching definition. Please implement the following step definitions:

      (Given "^a missing step$"
        (lambda ()

          ))


      (Given "^another missing step$"
        (lambda ()

          ))
      """
