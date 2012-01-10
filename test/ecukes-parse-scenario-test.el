(defun should-parse-scenario (name)
  (with-parse-scenario
   name
   (lambda (scenario name step-names tags)
     (should (equal name "Add two numbers"))
     (should
      (equal
       step-names
       '("Given I have entered 50 into the calculator"
         "And I have entered 70 into the calculator"
         "When I press add"
         "Then the result should be 120 on the screen"))))))


(ert-deftest parse-scenario-all-good ()
  "Should parse when all good."
  (should-parse-scenario "all-good"))

(ert-deftest parse-scenario-extra-space-in-header ()
  "Should parse when extra space in header."
  (should-parse-scenario "extra-space-in-header"))

(ert-deftest parse-scenario-no-space-in-header ()
  "Should parse when no space in header."
  (should-parse-scenario "no-space-in-header"))

(ert-deftest parse-scenario-line-breaks ()
  "Should parse when line breaks."
  (should-parse-scenario "line-breaks"))

(ert-deftest parse-scenario-wrong-indentation ()
  "Should parse when wrong indentation."
  (should-parse-scenario "wrong-indentation"))

(ert-deftest parse-scenario-comment-breaks ()
  "Should parse when comment breaks."
  (should-parse-scenario "comment-breaks"))

(ert-deftest parse-scenario-comments ()
  "Should not parse when comments."
  (with-parse-scenario
   "comments"
   (lambda (scenario name step-names tags)
     (should-not scenario))))

(ert-deftest parse-scenario-same-step-names ()
  "Should parse when same step names."
  (with-parse-scenario
   "same-step-names"
   (lambda (scenario name step-names tags)
     (should (equal (length step-names) 2)))))
