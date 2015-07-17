(ert-deftest parse-feature-empty ()
  "Should parse empty feature"
  (with-parse-feature
   "empty"
   (lambda (feature intro scenarios background steps)
     (should (equal feature (make-ecukes-feature :intro nil :background nil :scenarios nil))))))

(ert-deftest parse-multiple-features ()
  "Should warn when multiple features"
  (should-error (with-parse-feature "multiple" (lambda ()))))
