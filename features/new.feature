Feature: New

  Scenario: Non existing
    When I visit project "new"
    And I run ecukes "new"
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
    And the file "features/step-definitions/new-steps.el" should contain:
      """
      ;; This file contains your project specific step definitions. All
      ;; files in this directory whose names end with "-steps.el" will be
      ;; loaded automatically by Ecukes.

      (Given "^I have \"\\(.+\\)\"$"
        (lambda (something)
          ;; ...
          ))

      (When "^I have \"\\(.+\\)\"$"
        (lambda (something)
          ;; ...
          ))

      (Then "^I should have \"\\(.+\\)\"$"
        (lambda (something)
          ;; ...
          ))

      (And "^I have \"\\(.+\\)\"$"
        (lambda (something)
          ;; ...
          ))

      (But "^I should not have \"\\(.+\\)\"$"
        (lambda (something)
          ;; ...
          ))
      """
    And the file "features/support/env.el" should contain:
      """
      (require 'f)

      (defvar new-support-path
        (f-dirname load-file-name))

      (defvar new-features-path
        (f-parent new-support-path))

      (defvar new-root-path
        (f-parent new-features-path))

      (add-to-list 'load-path new-root-path)

      (require 'new)
      (require 'espuds)
      (require 'ert)

      (Setup
       ;; Before anything has run
       )

      (Before
       ;; Before each scenario is run
       )

      (After
       ;; After each scenario is run
       )

      (Teardown
       ;; After when everything has been run
       )
      """
    And the file "features/new.feature" should contain:
      """
      Feature: Do Some things
        In order to do something
        As a user
        I want to do something

        Scenario: Do Something
          Given I have "something"
          When I have "something"
          Then I should have "something"
          And I should have "something"
          But I should not have "something"
      """

  Scenario: Already exists
    When I visit project "new"
    And I run ecukes "new"
    And I run ecukes "new"
    Then I should see command error:
      """
      Ecukes already exists for this project
      """
