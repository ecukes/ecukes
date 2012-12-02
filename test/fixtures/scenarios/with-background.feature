Background:
  Given a known state
   When the key action
   Then observe outcomes

  Scenario: Add two positive numbers
    Given I have entered 50 into the calculator
      And I have entered 70 into the calculator
     When I press add
     Then the result should be 120 on the screen

  Scenario: Add two negative numbers
    Given I have entered -50 into the calculator
      And I have entered -70 into the calculator
     When I press add
     Then the result should be -120 on the screen
