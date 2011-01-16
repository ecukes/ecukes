Scenario: Add two numbers
  # foo
  Given I have entered 50 into the calculator
    # bar
    And I have entered 70 into the calculator
   # baz
   When I press add
   # qux
   Then the result should be 120 on the screen
