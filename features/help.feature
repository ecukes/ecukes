Feature: Help

  Scenario Outline: Usage
    When I run ecukes "<argument>"
    Then I should see command output:
      """
      Usage: ecukes
      """

    Examples:
      | argument |
      | -h       |
      | --help   |
