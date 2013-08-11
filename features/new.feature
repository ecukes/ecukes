Feature: New

  Scenario: Non existing
    When I visit project "new"
    And I run ecukes "--new"
    Then I should see command output:
      """
      create features
      create   step-definition
      create     new-steps.el
      create   support
      create     env.el
      create   new.feature
      """
    And these files should exist:
      | file                                   |
      | features                               |
      | features/step-definitions              |
      | features/step-definitions/new-steps.el |
      | features/support                       |
      | features/support/env.el                |
      | features/new.feature                   |

  Scenario: Already exists
    When I visit project "new"
    And I run ecukes "--new"
    And I run ecukes "--new"
    Then I should see command error:
      """
      Ecukes already exists for this project
      """
