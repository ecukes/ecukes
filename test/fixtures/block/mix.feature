Background:
  Given I see this on the screen:
  """
  Some awesome text...
  """
  And I see something else on the screen
  Then I should have these elements:
  | element |
  | h1      |
  | p       |
  | div     |
  And I should be happy
