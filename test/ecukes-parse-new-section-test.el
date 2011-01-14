(ert-deftest parse-new-section-end-of-buffer ()
  (with-mock
   (stub eobp => t)
   (stub ecukes-parse-line)
   (should (ecukes-parse-new-section-p))))

(ert-deftest parse-new-section-background ()
  (with-mock
   (stub eobp => nil)
   (stub ecukes-parse-line => "Background:")
   (should (ecukes-parse-new-section-p))))

(ert-deftest parse-new-section-scenario ()
  (with-mock
   (stub eobp => t)
   (stub ecukes-parse-line => "Scenario: Positive numbers")
   (should (ecukes-parse-new-section-p))))
