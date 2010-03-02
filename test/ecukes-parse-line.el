;; Testing ecukes-line
(ert-deftest parse-line-with-text ()
  (let ((line (ecukes-test-parse-line "with-text.feature")))
    (should (equal line "some text"))))

(ert-deftest parse-line-empty ()
  (let ((line (ecukes-test-parse-line "empty.feature")))
    (should (equal line ""))))

(ert-deftest parse-line-with-whitespace ()
  (let ((line (ecukes-test-parse-line "with-whitespace.feature")))
    (should (equal line "    some text    "))))

(ert-deftest parse-line-whitespace ()
  (let ((line (ecukes-test-parse-line "whitespace.feature")))
    (should (equal line "        "))))

(ert-deftest parse-line-next-line ()
  (let ((line (ecukes-test-parse-line "next-line.feature" 1)))
    (should (equal line "text"))))

;; Testing ecukes-blank-line
(ert-deftest parse-blank-line-with-text ()
  (let ((line (ecukes-test-parse-blank-line "with-text.feature")))
    (should (equal line "some text"))))

(ert-deftest parse-blank-line-empty ()
  (let ((line (ecukes-test-parse-blank-line "empty.feature")))
    (should (equal line ""))))

(ert-deftest parse-blank-line-with-whitespace ()
  (let ((line (ecukes-test-parse-blank-line "with-whitespace.feature")))
    (should (equal line "some text"))))

(ert-deftest parse-blank-line-whitespace ()
  (let ((line (ecukes-test-parse-blank-line "whitespace.feature")))
    (should (equal line ""))))

(ert-deftest parse-blank-line-next-line ()
  (let ((line (ecukes-test-parse-blank-line "next-line.feature" 1)))
    (should (equal line "text"))))
