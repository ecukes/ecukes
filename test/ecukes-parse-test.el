(ert-deftest parse-intro-no-intro ()
  (ecukes-test-parse-feature-intro
   "no-intro.feature"
   (lambda (feature intro header description)
     (should-not intro))))

(ert-deftest parse-intro-all-good ()
  (ecukes-test-parse-feature-intro
   "all-good.feature"
   (lambda (feature intro header description)
     (should (equal "Addition of two numbers" header))
     (should (equal "In order to aviod silly mistakes" (nth 0 description)))
     (should (equal "As a math idiot" (nth 1 description)))
     (should (equal "I want to be told the sum of two numbers" (nth 2 description))))))

(ert-deftest parse-intro-spaces-above ()
  (ecukes-test-parse-feature-intro
   "spaces-above.feature"
   (lambda (feature intro header description)
     (should (equal "Addition of two numbers" header))
     (should (equal "In order to aviod silly mistakes" (nth 0 description)))
     (should (equal "As a math idiot" (nth 1 description)))
     (should (equal "I want to be told the sum of two numbers" (nth 2 description))))))

(ert-deftest parse-intro-comments-above ()
  (ecukes-test-parse-feature-intro
   "comments-above.feature"
   (lambda (feature intro header description)
     (should (equal "Addition of two numbers" header))
     (should (equal "In order to aviod silly mistakes" (nth 0 description)))
     (should (equal "As a math idiot" (nth 1 description)))
     (should (equal "I want to be told the sum of two numbers" (nth 2 description))))))

(ert-deftest parse-intro-no-space-in-header ()
  (ecukes-test-parse-feature-intro
   "no-space-in-header.feature"
   (lambda (feature intro header description)
     (should (equal "Addition of two numbers" header))
     (should (equal "In order to aviod silly mistakes" (nth 0 description)))
     (should (equal "As a math idiot" (nth 1 description)))
     (should (equal "I want to be told the sum of two numbers" (nth 2 description))))))

(ert-deftest parse-intro-extra-space-in-header ()
  (ecukes-test-parse-feature-intro
   "extra-space-in-header.feature"
   (lambda (feature intro header description)
     (should (equal "Addition of two numbers" header))
     (should (equal "In order to aviod silly mistakes" (nth 0 description)))
     (should (equal "As a math idiot" (nth 1 description)))
     (should (equal "I want to be told the sum of two numbers" (nth 2 description))))))

(ert-deftest parse-intro-wrong-indentation ()
  (ecukes-test-parse-feature-intro
   "wrong-indentation.feature"
   (lambda (feature intro header description)
     (should (equal "Addition of two numbers" header))
     (should (equal "In order to aviod silly mistakes" (nth 0 description)))
     (should (equal "As a math idiot" (nth 1 description)))
     (should (equal "I want to be told the sum of two numbers" (nth 2 description))))))

(ert-deftest parse-intro-fewer-description-lines ()
  (ecukes-test-parse-feature-intro
   "fewer-description-lines.feature"
   (lambda (feature intro header description)
     (should (equal "Addition of two numbers" header))
     (should (equal "As a math idiot I want to aviod silly mistakes and be told the sum of two numbers" (nth 0 description))))))

(ert-deftest parse-intro-more-description-lines ()
  (ecukes-test-parse-feature-intro
   "more-description-lines.feature"
   (lambda (feature intro header description)
     (should (equal "Addition of two numbers" header))
     (should (equal "In order to aviod silly mistakes" (nth 0 description)))
     (should (equal "As a math idiot" (nth 1 description)))
     (should (equal "And a programmer guru" (nth 2 description)))
     (should (equal "I want to be told the sum of two numbers" (nth 3 description)))
     (should (equal "In the morning" (nth 4 description))))))

(ert-deftest parse-intro-line-breaks ()
  (ecukes-test-parse-feature-intro
   "line-breaks.feature"
   (lambda (feature intro header description)
     (should (equal "Addition of two numbers" header))
     (should (equal "In order to aviod silly mistakes" (nth 0 description)))
     (should (equal "As a math idiot" (nth 1 description)))
     (should-not (nth 2 description)))))
