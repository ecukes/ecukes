Feature: Help

  Scenario Outline: Usage
    When I run ecukes "<argument>"
    Then I should see command output:
      """
      USAGE: ecukes
      """

    Examples:
      | argument |
      | -h       |
      | --help   |
