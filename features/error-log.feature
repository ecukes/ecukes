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


  # Emacs 25.1 and 25.2 added weird behaviour in cl-assert which broke ecukes
  # when it was used to assert in tests.
  Scenario: Using cl-assert
    Given step definition:
      """
      (Then "^asserting false$"
        (lambda () (cl-assert (= 1 2) nil "Expected 1 to equal %s" "2")))
      """
    Given feature "cl-assert":
      """
      Feature: cl-assert

        Scenario: Asserting
          Then asserting false
      """
    When I run ecukes "features/cl-assert.feature --reporter spec"
    Then I should see command error:
      """
      Feature: cl-assert

        Scenario: Asserting
          Then asserting false
            Expected 1 to equal 2

      1 scenarios (1 failed, 0 passed)
      1 steps (1 failed, 0 skipped, 0 passed)
      """
