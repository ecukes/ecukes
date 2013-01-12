(require 'ecukes-parse)

(defun with-parse-outline (name fn)
  (let* ((feature-file (fixture-file-path "outlines" name))
         (feature (ecukes-parse-feature feature-file))
         (outlines (ecukes-feature-outlines feature))
         (scenarios (ecukes-feature-scenarios feature)))
    (funcall fn outlines scenarios)))

(defun should-parse-outline (name expected-examples &optional tests)
  (with-parse-outline
   name
   (lambda (outlines scenarios)
     (should (= 1 (length outlines)))
     (let* ((outline (car outlines))
            (tags (ecukes-outline-tags outline))
            (table (ecukes-outline-table outline)))
       (should (equal expected-examples (1- (length table))))
       (should (equal expected-examples (length scenarios)))
       (should (-all? (lambda (scenario) (equal tags (ecukes-scenario-tags scenario))) scenarios))
       (when (functionp tests) (funcall tests (car outlines) scenarios))))))

(ert-deftest parse-outline-no-examples ()
  "Should parse scenario outlines with no examples."
  (with-parse-outline
   "no-examples"
   (lambda (outlines scenarios)
     (should (= 1 (length outlines)))
     (should-not (ecukes-outline-table (car outlines)))
     (should (= 0 (length scenarios))))))

(ert-deftest parse-outline-one-example ()
  "Should parse scenario outline with one example."
  (should-parse-outline
   "one-example" 1
   (lambda (outline scenarios)
     (should (ecukes-outline-table outline))
     (should (= 1 (length scenarios))))))

(ert-deftest parse-outline-multiple-examples ()
  "Should parse scenario outlines with multiple examples."
  (should-parse-outline "multiple-examples" 3))

(ert-deftest parse-outline-tags ()
  "Should parse scenario outlines with tags."
  (should-parse-outline "tags" 2))

(ert-deftest parse-outline-background ()
  "Should parse scenario outlines with backgrounds."
  (should-parse-outline "background" 1))

(ert-deftest parse-outline-bad-indents ()
  "Should parse scenario outlines with bad indentation."
  (should-parse-outline "bad-indents" 2))

(ert-deftest parse-outline-substitution ()
  "Should substitute the values from examples into the generated scenarios."
  (should-parse-outline
   "substitution" 3
   (lambda (outline scenarios)
     (let* ((scenario (car scenarios))
            (simple-step (car (ecukes-scenario-steps scenario)))
            (py-step (nth 1 (ecukes-scenario-steps scenario)))
            (table-step (nth 2 (ecukes-scenario-steps scenario))))
       (should (string= "Given I want to marry" (ecukes-step-name simple-step)))
       (should (string= "I want to marry" (ecukes-step-body simple-step)))
       (should-not (ecukes-step-arg simple-step))

       (should (string= "You are great! I want to marry you." (ecukes-step-arg py-step)))

       (should (equal '(("response" "desired") ("positive" "true"))
                      (ecukes-step-arg table-step)))))))

(ert-deftest parse-outline-wrong-column-names ()
  "Generates scenarios without any substitutions if your column names are wrong."
  (should-parse-outline
   "wrong-column-names" 1
   (lambda (outline scenarios)
     (let ((step (car (ecukes-scenario-steps (car scenarios)))))
       (should (string= "Given <foo> is <bar>" (ecukes-step-name step)))))))
