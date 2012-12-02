Background:
  Given I see something on the screen
  # Here I see something else on the screen
  And I see something else on the screen
  # But here I don't
  # see something on the screen
  But I dont see something on the screen
  When I see something on the screen
  Then I should see something on the screen
