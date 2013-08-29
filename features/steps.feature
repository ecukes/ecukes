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
      Please implement the following step definitions

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
      Please implement the following step definitions

      (Given "^a missing step$"
        (lambda ()

          ))

      (Given "^another missing step$"
        (lambda ()

          ))
      """

  Scenario: Missing step with argument
    Given feature "foo":
      """
      Feature: Foo

        Scenario: Bar
          Given some "thing"
      """
    When I run ecukes "features/foo.feature"
    Then I should see command output:
      """
      Please implement the following step definitions

      (Given "^some \"\\([^\"]+\\)\"$"
        (lambda (arg)

          ))
      """

  Scenario: Missing step with arguments
    Given feature "foo":
      """
      Feature: Foo

        Scenario: Bar
          Given some "thing" and some "one"
      """
    When I run ecukes "features/foo.feature"
    Then I should see command output:
      """
      Please implement the following step definitions

      (Given "^some \"\\([^\"]+\\)\" and some \"\\([^\"]+\\)\"$"
        (lambda (arg-1 arg-2)

          ))
      """
