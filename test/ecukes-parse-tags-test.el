(ert-deftest parse-tags-single-tag ()
  "Should parse single tag."
  (with-parse-scenario
   "tags-single"
   (lambda (scenario name step-names tags)
     (should (equal tags '("debug"))))))

(ert-deftest parse-tags-multiple-tags ()
  "Should parse multiple tags."
  (with-parse-scenario
   "tags-multiple"
   (lambda (scenario name step-names tags)
     (should (equal tags '("debug" "verbose"))))))

(ert-deftest parse-tags-tags-with-whitespace ()
  "Should parse multiple tags despite whitespace."
  (with-parse-scenario
   "tags-whitespace"
   (lambda (scenario name step-names tags)
     (should (equal tags '("debug" "verbose"))))))

(ert-deftest parse-tags-tags-with-same-name ()
  "Should parse tag with same name only once."
  (with-parse-scenario
   "same-name"
   (lambda (scenario name step-names tags)
     (should (equal tags '("debug"))))))

(ert-deftest parse-tags-tags-comment ()
  "Should not parse when comment."
  (with-parse-scenario
   "tags-comment"
   (lambda (scenario name step-names tags)
     (should-not tags))))

(ert-deftest parse-tags-tags-none ()
  "Should parse when none."
  (with-parse-scenario
   "tags-none"
   (lambda (scenario name step-names tags)
     (should-not tags))))
