Feature: scenario outlines

  Scenario Outline: single example given
    Given <foo> is <bar>

    Examples:
      | foo | bar |
      | 1   | 2   |
