(ert-deftest parse-feature-empty ()
  "Should parse empty feature"
  (with-parse-feature
   "empty"
   (lambda (feature intro scenarios background steps)
     (should (equal feature (make-ecukes-feature :intro nil :background nil :scenarios nil))))))
