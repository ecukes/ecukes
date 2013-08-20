Feature: List steps

  Background:
    Given step definition:
      """
      (Given "^hello world$" "Saying hello to the world" 'ignore)
      (Given "^sing hallelujah$" "Singing a song" 'ignore)
      """

  Scenario: Names only
    When I run ecukes "list-steps"
    Then I should see command output:
      """
      ^sing hallelujah$
      ^hello world$
      """

  Scenario: Names with doc
    When I run ecukes "list-steps --with-doc"
    Then I should see command output:
      """
      ^sing hallelujah$
      Singing a song

      ^hello world$
      Saying hello to the world
      """

  Scenario: Names with file
    When I run ecukes "list-steps --with-file"
    Then I should see command output:
      """
      features/step-definitions/super-project-steps.el: ^sing hallelujah$
      features/step-definitions/super-project-steps.el: ^hello world$
      """

  Scenario: Names with doc and file
    When I run ecukes "list-steps --with-file --with-doc"
    Then I should see command output:
      """
      features/step-definitions/super-project-steps.el: ^sing hallelujah$
      Singing a song

      features/step-definitions/super-project-steps.el: ^hello world$
      Saying hello to the world
      """
