Feature: Lexical binding
  Ecukes should support lexical binding packages

  Scenario: Support lexical binding
    Given feature "lexical-binding":
      """
      Feature: Lexical Binding
        Scenario: Lexical bound step definitions
          Given no arguments
      """
    Given step definition:
      """
      ;;; -*- lexical-binding: t; -*-
      (Given "^no arguments$"
        (lambda () (print "foo")))
      """
    When I run ecukes "features/lexical-binding.feature --reporter dot"
    Then I should see command output:
      """
      .

      1 scenarios (0 failed, 1 passed)
      1 steps (0 failed, 0 skipped, 1 passed)
      """
