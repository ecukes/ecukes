Feature: Verbose

  # Prints message for some unknown reason
  @exclude
  Scenario: Message - non verbose
    Given feature "output":
      """
      Feature: Output

        Scenario: Message
          When I print message
      """
    And step definition:
      """
      (When "I print message"
       (lambda () (message "hello world")))
      """
    When I run ecukes "features/output.feature"
    Then I should see command output:
      """
      Feature: Output


        Scenario: Message
          When I print message

      1 scenarios (0 failed, 1 passed)
      1 steps (0 failed, 0 skipped, 1 passed)
      """

  Scenario: Message - verbose
    Given feature "output":
      """
      Feature: Output

        Scenario: Message
          When I print message
      """
    And step definition:
      """
      (When "I print message"
       (lambda () (message "hello world")))
      """
    When I run ecukes "features/output.feature --verbose"
    Then I should see command output:
      """
      Feature: Output


        Scenario: Message
      hello world
          When I print message

      1 scenarios (0 failed, 1 passed)
      1 steps (0 failed, 0 skipped, 1 passed)
      """
