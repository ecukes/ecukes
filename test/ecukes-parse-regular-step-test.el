(ert-deftest parse-regular-step-given ()
  (let ((step (ecukes-test-parse-step "given.feature")))
    (should (equal "Given I see something on the screen" (ecukes-step-name step)))
    (should-be-regular-step step)))

(ert-deftest parse-regular-step-when ()
  (let ((step (ecukes-test-parse-step "when.feature")))
    (should (equal "When I see something on the screen" (ecukes-step-name step)))
    (should-be-regular-step step)))

(ert-deftest parse-regular-step-then ()
  (let ((step (ecukes-test-parse-step "then.feature")))
    (should (equal "Then I should see something on the screen" (ecukes-step-name step)))
    (should-be-regular-step step)))

(ert-deftest parse-regular-step-and ()
  (let ((step (ecukes-test-parse-step "and.feature")))
    (should (equal "And I see something else on the screen" (ecukes-step-name step)))
    (should-be-regular-step step)))

(ert-deftest parse-regular-step-but ()
  (let ((step (ecukes-test-parse-step "but.feature")))
    (should (equal "But I dont see something on the screen" (ecukes-step-name step)))
    (should-be-regular-step step)))
