(ert-deftest parse-block-all-good ()
  (let ((steps (ecukes-test-parse-block-steps "all-good.feature")))
    (should (equal "Given I see something on the screen" (ecukes-step-name (nth 0 steps))))
    (should (equal "And I see something else on the screen" (ecukes-step-name (nth 1 steps))))
    (should (equal "But I dont see something on the screen" (ecukes-step-name (nth 2 steps))))
    (should (equal "When I see something on the screen" (ecukes-step-name (nth 3 steps))))
    (should (equal "Then I should see something on the screen" (ecukes-step-name (nth 4 steps))))))

(ert-deftest parse-block-with-blank-lines ()
  (let ((steps (ecukes-test-parse-block-steps "with-blank-lines.feature")))
    (should (equal "Given I see something on the screen" (ecukes-step-name (nth 0 steps))))
    (should (equal "And I see something else on the screen" (ecukes-step-name (nth 1 steps))))
    (should-not (nth 2 steps))))

(ert-deftest parse-block-with-comments ()
  (let ((steps (ecukes-test-parse-block-steps "with-comments.feature")))
    (should (equal "Given I see something on the screen" (ecukes-step-name (nth 0 steps))))
    (should (equal "And I see something else on the screen" (ecukes-step-name (nth 1 steps))))
    (should (equal "But I dont see something on the screen" (ecukes-step-name (nth 2 steps))))
    (should (equal "When I see something on the screen" (ecukes-step-name (nth 3 steps))))
    (should (equal "Then I should see something on the screen" (ecukes-step-name (nth 4 steps))))))

(ert-deftest parse-block-mix ()
  (let ((steps (ecukes-test-parse-block-steps "mix.feature")))
    ;; Py String
    (should (equal "Given I see this on the screen:" (ecukes-step-name (nth 0 steps))))
    (should (equal "Some awesome text..." (ecukes-step-arg (nth 0 steps))))

    ;; First regular step
    (should (equal "And I see something else on the screen" (ecukes-step-name (nth 1 steps))))

    ;; Table
    (let* ((table (nth 2 steps))
           (table (ecukes-step-arg table))
           (header (car table))
           (rows (cdr table)))
      (should (equal "element" (car header)))
      (should (equal "h1" (car (nth 0 rows))))
      (should (equal "p" (car (nth 1 rows))))
      (should (equal "div" (car (nth 2 rows)))))

    ;; Last regular step
    (should (equal "And I should be happy" (ecukes-step-name (nth 3 steps))))))
