(require 'ecukes-helpers)

(ert-deftest run-feature-steps ()
  "Should return all steps."
  (with-parse-feature
   "simple"
   (lambda (feature intro scenarios background steps)
     (should (equal (ecukes-feature-steps (list feature)) steps)))))
