(require 'ecukes-helpers)

(ert-deftest run-feature-steps ()
  "Should return all steps."
  (with-parse-feature
   "simple"
   (lambda (feature intro scenarios background steps)
     (should (equal (ecukes-feature-steps (list feature)) steps)))))

(ert-deftest ecukes-helpers-test/format-quote ()
  (should (equal (ecukes-format-quote "foo") "foo"))
  (should (equal (ecukes-format-quote "foo %") "foo %%")))
