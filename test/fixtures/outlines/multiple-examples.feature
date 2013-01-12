Feature: scenario outline

  Scenario Outline: multiple examples
    Given <foo> is <bar>

    Examples:
      | foo | bar |
      | 1   | 2   |
      | 3   | 4   |
      | 5   | 6   |
