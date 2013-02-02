@debug @verbose
Feature: Some Feature
  As a programmer
  I love debugging

  @super
  Scenario: Some Scenario
    Given a known state

  @duper
  Scenario Outline: Some Scenario
    Given state <state>

    Examples:
      | state   |
      | known   |
      | unknown |
