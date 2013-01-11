Feature: scenario outlines

  @regression @ui
  Scenario Outline: with tags
    Given <foo> is <bar>

    Examples:
      | foo | bar |
      | 1   | 2   |
      | 3   | 4   |
