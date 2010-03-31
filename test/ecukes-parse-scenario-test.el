(ert-deftest parse-scenario-no-scenarios ()
  (ecukes-test-parse-feature-scenario
   "no-scenarios.feature"
   (lambda (feature scenarios)
     (should-not scenarios))))

(ert-deftest parse-scenario-all-good ()
  (ecukes-test-parse-feature-scenario
   "all-good.feature"
   (lambda (feature scenarios)
     (let* ((scenario (car scenarios))
            (steps (ecukes-scenario-steps scenario)))
       (should (equal "Go fishing" (ecukes-scenario-name scenario)))
       (should (equal "Given I have a fishing pole" (ecukes-step-name (nth 0 steps))))
       (should (equal "And some bait" (ecukes-step-name (nth 1 steps))))
       (should (equal "Then I should get some fish" (ecukes-step-name (nth 2 steps))))))))

(ert-deftest parse-scenario-extra-space-in-header ()
  (ecukes-test-parse-feature-scenario
   "extra-space-in-header.feature"
   (lambda (feature scenarios)
     (let* ((scenario (car scenarios))
            (steps (ecukes-scenario-steps scenario)))
       (should (equal "Go fishing" (ecukes-scenario-name scenario)))
       (should (equal "Given I have a fishing pole" (ecukes-step-name (nth 0 steps))))
       (should (equal "And some bait" (ecukes-step-name (nth 1 steps))))
       (should (equal "Then I should get some fish" (ecukes-step-name (nth 2 steps))))))))

(ert-deftest parse-scenario-line-breaks ()
  (ecukes-test-parse-feature-scenario
   "line-breaks.feature"
   (lambda (feature scenarios)
     (let* ((scenario (car scenarios))
            (steps (ecukes-scenario-steps scenario)))
       (should (equal "Go fishing" (ecukes-scenario-name scenario)))
       (should (equal "Given I have a fishing pole" (ecukes-step-name (nth 0 steps))))
       (should (equal "And some bait" (ecukes-step-name (nth 1 steps))))
       (should-not (nth 2 steps))))))

(ert-deftest parse-scenario-no-space-in-header ()
  (ecukes-test-parse-feature-scenario
   "no-space-in-header.feature"
   (lambda (feature scenarios)
     (let* ((scenario (car scenarios))
            (steps (ecukes-scenario-steps scenario)))
       (should (equal "Go fishing" (ecukes-scenario-name scenario)))
       (should (equal "Given I have a fishing pole" (ecukes-step-name (nth 0 steps))))
       (should (equal "And some bait" (ecukes-step-name (nth 1 steps))))
       (should (equal "Then I should get some fish" (ecukes-step-name (nth 2 steps))))))))

(ert-deftest parse-scenario-with-intro ()
  (ecukes-test-parse-feature-scenario
   "with-intro.feature"
   (lambda (feature scenarios)
     (let* ((scenario (car scenarios))
            (steps (ecukes-scenario-steps scenario)))
       (should (equal "Go fishing" (ecukes-scenario-name scenario)))
       (should (equal "Given I have a fishing pole" (ecukes-step-name (nth 0 steps))))
       (should (equal "And some bait" (ecukes-step-name (nth 1 steps))))
       (should (equal "Then I should get some fish" (ecukes-step-name (nth 2 steps))))))))

(ert-deftest parse-scenario-wrong-indentation ()
  (ecukes-test-parse-feature-scenario
   "wrong-indentation.feature"
   (lambda (feature scenarios)
     (let* ((scenario (car scenarios))
            (steps (ecukes-scenario-steps scenario)))
       (should (equal "Go fishing" (ecukes-scenario-name scenario)))
       (should (equal "Given I have a fishing pole" (ecukes-step-name (nth 0 steps))))
       (should (equal "And some bait" (ecukes-step-name (nth 1 steps))))
       (should (equal "Then I should get some fish" (ecukes-step-name (nth 2 steps))))))))

(ert-deftest parse-scenario-multiple-scenarios ()
  (ecukes-test-parse-feature-scenario
   "multiple-scenarios.feature"
   (lambda (feature scenarios)
     (let ((scenario1 (nth 0 scenarios))
           (scenario2 (nth 1 scenarios)))
       (should (equal "Go fishing" (ecukes-scenario-name scenario1)))
       (let ((steps (ecukes-scenario-steps scenario1)))
         (should (equal "Given I have a fishing pole" (ecukes-step-name (nth 0 steps))))
         (should (equal "And some bait" (ecukes-step-name (nth 1 steps))))
         (should (equal "Then I should get some fish" (ecukes-step-name (nth 2 steps)))))
       (should (equal "Go hunting" (ecukes-scenario-name scenario2)))
       (let ((steps (ecukes-scenario-steps scenario2)))
         (should (equal "Given I have bazooka" (ecukes-step-name (nth 0 steps))))
         (should (equal "And some 60 cal" (ecukes-step-name (nth 1 steps))))
         (should (equal "Then I should get some meat" (ecukes-step-name (nth 2 steps)))))))))

(ert-deftest parse-scenario-multiple-scenarios ()
  (ecukes-test-parse-feature-scenario
   "multiple-scenarios-exactly-same.feature"
   (lambda (feature scenarios)
     (let ((scenario1 (nth 0 scenarios))
           (scenario2 (nth 1 scenarios)))
       (let ((fishing-validator))
         (setq fishing-validator
               (lambda (scenario)
                 (should (equal "Go fishing" (ecukes-scenario-name scenario1)))
                 (let ((steps (ecukes-scenario-steps scenario)))
                   (should (equal "Given I have a fishing pole" (ecukes-step-name (nth 0 steps))))
                   (should (equal "And some bait" (ecukes-step-name (nth 1 steps))))
                   (should (equal "Then I should get some fish" (ecukes-step-name (nth 2 steps)))))))
         (funcall fishing-validator scenario1)
         (funcall fishing-validator scenario2))))))

(ert-deftest parse-scenario-comment ()
  (ecukes-test-parse-feature-scenario
   "comment.feature"
   (lambda (feature scenarios)
     (should-not scenarios))))

(ert-deftest parse-scenario-single-tag ()
  (ecukes-test-parse-feature-scenario
   "tags-single.feature"
   (lambda (feature scenarios)
     (let ((scenario (car scenarios)))
       (should-have-tags scenario "debug")))))

(ert-deftest parse-scenario-multiple-tags ()
  (ecukes-test-parse-feature-scenario
   "tags-multiple.feature"
   (lambda (feature scenarios)
     (let ((scenario (car scenarios)))
       (should-have-tags scenario "debug" "verbose")))))

(ert-deftest parse-scenario-tags-whitespace ()
  (ecukes-test-parse-feature-scenario
   "tags-whitespace.feature"
   (lambda (feature scenarios)
     (let ((scenario (car scenarios)))
       (should-have-tags scenario "debug" "verbose")))))

(ert-deftest parse-scenario-tags-comment ()
  (ecukes-test-parse-feature-scenario
   "tags-comment.feature"
   (lambda (feature scenarios)
     (let ((scenario (car scenarios)))
       (should-have-tags scenario)))))

(ert-deftest parse-same-name ()
  (ecukes-test-parse-feature-scenario
   "same-name.feature"
   (lambda (feature scenarios)
     (let* ((scenario (car scenarios))
            (steps (ecukes-scenario-steps scenario)))
       (should (equal 2 (length steps)))
       (should (equal "Given something" (ecukes-step-name (car steps))))
       (should (equal "Given something" (ecukes-step-name (car (cdr steps)))))))))

(defun should-have-tags (scenario &rest tags)
  (let ((actual-tags (ecukes-scenario-tags scenario))
        (expected-tags tags))
    (should (equal (sort actual-tags 'string<) (sort expected-tags 'string<)))))
