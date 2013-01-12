Feature: scenario outlines

  Background:
    Given I do something

  Scenario Outline: with background
    Given <foo> is <bar>

    Examples:
      | foo | bar |
      | 1   | 2   |
