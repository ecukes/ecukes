Feature: scenario outlines

  Scenario Outline: weird indents
    Given <foo> is <bar>

  Examples:
  | foo |     bar |
    | 1 | 2 |
 | 3         |   4|
