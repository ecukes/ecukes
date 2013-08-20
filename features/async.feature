Feature: Async
  As an Ecukes user
  I want to be able to define async step deefinitions

  Scenario: Callbacked
    Given feature "async":
      """
      Feature: Sleep for
        Scenario: Ten seconds
          Given I sleep for "1" second
      """
    Given step definition:
      """
      (Given "^I sleep for \"\\([0-9]+\\)\" seconds?$"
        (lambda (seconds callback)
          (sleep-for (string-to-number seconds))
          (funcall callback)))
      """
    When I run ecukes "features/async.feature --reporter dot"
    Then I should see command output:
      """
      .

      1 scenarios (0 failed, 1 passed)
      1 steps (0 failed, 0 skipped, 1 passed)
      """

  Scenario: Not callbacked
    Given feature "async":
      """
      Feature: Sleep for
        Scenario: Ten seconds
          Given I sleep for "1" second
      """
    Given step definition:
      """
      (Given "^I sleep for \"\\([0-9]+\\)\" seconds?$"
        (lambda (seconds callback)
          ;; not callbacked
          ))
      """
    When I run ecukes "features/async.feature --reporter dot --timeout 1"
    Then I should see command error:
      """
      .

        Scenario: Ten seconds
          Given I sleep for "1" second
            Did not callback async step within 1 seconds

      1 scenarios (1 failed, 0 passed)
      1 steps (1 failed, 0 skipped, 0 passed)
      """
