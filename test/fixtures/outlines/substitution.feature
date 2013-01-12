Feature: scenario outlines

  Scenario Outline: py-string and table substitution
    Given I want to <activity>
    When I say:
      """
      You are <compliment>! I want to <activity> you.
      """
    Then the results are:
      | response   | desired   |
      | <response> | <desired> |

    Examples:
      | activity | compliment | response | desired |
      | marry    | great      | positive | true    |
      | marry    | not bad    | negative | false   |
      | divorce  | an idiot   | negative | true    |
