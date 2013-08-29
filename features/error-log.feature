Feature: Error Log

  # NOTE: The backtrace is captured by setting the `debugger' variable
  # to a function and catching the backtrace. The function is however
  # only called once (not sure why), so it's only one backtrace
  # available in the error log file.

  Scenario: Single error
    Given step definition:
      """
      (Given "^zero division$"
       (lambda () (/ 1 0)))
      """
    Given feature "math":
      """
      Feature: Math

        Scenario: Zero division
          Given zero division
      """
      When I run ecukes "features/math.feature --reporter spec --error-log error.log"
    Then I should see command error:
      """
      Feature: Math

        Scenario: Zero division
          Given zero division
            Arithmetic error

      1 scenarios (1 failed, 0 passed)
      1 steps (1 failed, 0 skipped, 0 passed)
      """
    And the file "error.log" should contain:
      """
        /(1 0)
        (lambda nil (/ 1 0))()
      """
