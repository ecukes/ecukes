(require 'ecukes-parse)

(ert-deftest parse-args-no-args ()
  "Should be empty when no step args."
  (let ((body "a state"))
    (should (equal (ecukes-parse-args body) nil))))

(ert-deftest parse-args-single-arg ()
  "Should return correct args when single arg."
  (let ((body "state \"known\""))
    (should (equal (ecukes-parse-args body) (list "known")))))

(ert-deftest parse-args-multiple-args ()
  "Should return correct args when multiple args."
  (let ((body "state \"known\" and \"unknown\""))
    (should (equal (ecukes-parse-args body) (list "known" "unknown")))))
