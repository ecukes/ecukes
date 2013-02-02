(require 'ecukes-parse)

(ert-deftest parse-feature-tags-single-tag ()
  "Should parse single tag."
  (with-parse-feature
   "tags-single"
   (lambda (feature intro scenarios background steps)
     (should (equal (ecukes-scenario-tags (car scenarios)) '("debug"))))))

(ert-deftest parse-feature-tags-multiple-tags ()
  "Should parse multiple tags."
  (with-parse-feature
   "tags-multiple-tags"
   (lambda (feature intro scenarios background steps)
     (should (equal (ecukes-scenario-tags (car scenarios)) '("debug" "verbose"))))))

(ert-deftest parse-feature-tags-multiple-scenarios ()
  "Should parse multiple tags."
  (with-parse-feature
   "tags-multiple-scenarios"
   (lambda (feature intro scenarios background steps)
     (should (equal (ecukes-scenario-tags (nth 0 scenarios)) '("debug" "verbose")))
     (should (equal (ecukes-scenario-tags (nth 1 scenarios)) '("debug" "verbose"))))))

(ert-deftest parse-feature-tags-mixed ()
"Should parse multiple tags."
(with-parse-feature
 "tags-multiple-mixed"
 (lambda (feature intro scenarios background steps)
   (should (equal (ecukes-scenario-tags (nth 0 scenarios)) '("debug" "verbose" "super")))
   (should (equal (ecukes-scenario-tags (nth 1 scenarios)) '("debug" "verbose" "duper")))
   (should (equal (ecukes-scenario-tags (nth 2 scenarios)) '("debug" "verbose" "duper"))))))

(ert-deftest parse-feature-tags-tags-with-whitespace ()
  "Should parse multiple tags despite whitespace."
  (with-parse-feature
   "tags-whitespace"
   (lambda (feature intro scenarios background steps)
     (should (equal (ecukes-scenario-tags (car scenarios)) '("debug" "verbose"))))))

(ert-deftest parse-feature-tags-tags-with-same-name ()
  "Should parse tag with same name only once."
  (with-parse-feature
   "same-name"
   (lambda (feature intro scenarios background steps)
     (should (equal (ecukes-scenario-tags (car scenarios)) '("debug"))))))

(ert-deftest parse-feature-tags-tags-comment ()
  "Should not parse when comment."
  (with-parse-feature
   "tags-comment"
   (lambda (feature intro scenarios background steps)
     (should-not (ecukes-scenario-tags (car scenarios))))))

(ert-deftest parse-feature-tags-tags-none ()
  "Should parse when none."
  (with-parse-feature
   "tags-none"
   (lambda (feature intro scenarios background steps)
     (should-not (ecukes-scenario-tags (car scenarios))))))
