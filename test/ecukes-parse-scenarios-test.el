(defun with-parse-scenarios (name fn)
  (let* ((feature-file (feature-file-path "scenarios" name))
         (feature (ecukes-parse-feature feature-file))
         (scenarios (ecukes-feature-scenarios feature)))
    (funcall fn (mapcar 'ecukes-scenario-name scenarios))))

(defun should-parse-scenarios (name)
  (with-parse-scenarios
   name
   (lambda (scenario-names)
     (should
      (equal
       scenario-names
       '("Add two positive numbers" "Add two negative numbers"))))))

(ert-deftest parse-scenarios-no-scenarios ()
  "Should not parse scenarios when none."
  (with-parse-scenarios
   "none"
   (lambda (scenario-names)
     (should-not scenario-names))))

(ert-deftest parse-scenarios-all-good ()
  "Should parse scenarios when all good."
  (should-parse-scenarios "all-good"))

(ert-deftest parse-scenarios-with-intro ()
  "Should parse scenarios with intro."
  (should-parse-scenarios "with-intro"))

(ert-deftest parse-scenarios-with-background ()
  "Should parse scenarios with background."
  (should-parse-scenarios "with-background"))
