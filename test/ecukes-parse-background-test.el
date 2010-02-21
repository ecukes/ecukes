(ert-deftest parse-background-all-good ()
  (ecukes-test-parse-feature-background
   "all-good.feature"
   (lambda (feature background steps)
     (should (equal 2 (length steps)))
     (should (equal "Given I cant fall asleep" (ecukes-step-name (nth 0 steps))))
     (should (equal "And drugs cant help me" (ecukes-step-name (nth 1 steps)))))))

(ert-deftest parse-background-with-intro ()
  (ecukes-test-parse-feature-background
   "with-intro.feature"
   (lambda (feature background steps)
     (should (equal 2 (length steps)))
     (should (equal "Given I cant fall asleep" (ecukes-step-name (nth 0 steps))))
     (should (equal "And drugs cant help me" (ecukes-step-name (nth 1 steps)))))))

(ert-deftest parse-background-wrong-indentation ()
  (ecukes-test-parse-feature-background
   "wrong-indentation.feature"
   (lambda (feature background steps)
     (should (equal 2 (length steps)))
     (should (equal "Given I cant fall asleep" (ecukes-step-name (nth 0 steps))))
     (should (equal "And drugs cant help me" (ecukes-step-name (nth 1 steps)))))))

(ert-deftest parse-background-line-breaks ()
  (ecukes-test-parse-feature-background
   "line-breaks.feature"
   (lambda (feature background steps)
     (should (equal 2 (length steps)))
     (should (equal "Given I cant fall asleep" (ecukes-step-name (nth 0 steps))))
     (should (equal "And drugs cant help me" (ecukes-step-name (nth 1 steps))))
     (should-not (nth 2 steps)))))
