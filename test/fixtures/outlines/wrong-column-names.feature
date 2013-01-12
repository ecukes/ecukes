Feature: scenario outlines

  Scenario Outline: wrong column names
    Given <foo> is <bar>

    Examples:
      | baz | quux |
      | 1   | 2    |
