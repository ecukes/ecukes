(defun ecukes-test-parse-feature-intro (feature-file fn)
  "Parses intro in FEATURE-FILE, yielding FN.
FN is a function that takes the following arguments:
feature     - An `ecukes-feature' object.
intro       - An `ecukes-intro' object.
header      - The intro header.
description - The intro description."
  (let* ((feature (ecukes-test-parse-feature (concat "intro/" feature-file)))
         (intro (ecukes-feature-intro feature))
         (header) (description))
    (when intro
      (setq header (ecukes-intro-header intro))
      (setq description(ecukes-intro-description intro)))
    (funcall fn feature intro header description)))

(defun ecukes-test-parse-feature-scenario (feature-file fn)
  "Parses all scenarios in FEATURE-FILE, yielding FN.
FN is a function that takes the following arguments:
feature   - An `ecukes-feature' object.
scenarios - All scenarios parsed (`ecukes-scenario' objects)."
  (let* ((feature (ecukes-test-parse-feature (concat "scenario/" feature-file)))
         (scenarios (ecukes-feature-scenarios feature)))
    (funcall fn feature scenarios)))

(defun ecukes-test-parse-step (feature-file)
  "Parses a single step in FEATURE-FILE and returns that step as an
`ecukes-step' object."
  (with-temp-buffer
    (insert-file-contents-literally (concat "features/step/" feature-file))
    (ecukes-parse-step)))

(defun ecukes-test-parse-background (feature-file fn)
  "Parses the background in FEATURE-FILE, yielding FN.
FN is a function that takes the following arguments:
feature    - An `ecukes-feature' object.
background - The background as an `ecukes-background' object.
steps      - All background steps (`ecukes-step' objects)."
  (let* ((feature (ecukes-test-parse-feature (concat "background/" feature-file)))
         (background (ecukes-feature-background feature))
         (steps))
    (if background
        (setq steps (ecukes-background-steps background)))
    (funcall fn feature background steps)))

(defun ecukes-test-parse-feature (feature-file)
  "Parses FEATURE-FILE and return an `ecukes-feature' object."
  (ecukes-parse-feature (concat ecukes-test-path "features/" feature-file)))

(defun ecukes-test-parse-line (feature-file &optional n)
  "Parses line, including blanks, with n offset, in FEATURE-FILE."
  (ecukes-test-parse-line-helper feature-file 'ecukes-line n))

(defun ecukes-test-parse-blank-line (feature-file &optional n)
  "Parses line, excluding blanks, with n offset, in FEATURE-FILE."
  (ecukes-test-parse-line-helper feature-file 'ecukes-blank-line n))

(defun ecukes-test-parse-line-helper (feature-file fn &optional n)
  "Generic helper for parsing single line in FEATURE-FILE."
  (with-temp-buffer
    (insert-file-contents-literally (concat ecukes-test-path "features/line/" feature-file))
    (funcall fn n)))

(defun ecukes-test-parse-block-steps (feature-file)
  "Parses a block in FEATURE-FILE and returns a list of all steps as
`ecukes-step' objects."
  (with-temp-buffer
    (insert-file-contents-literally (concat ecukes-test-path "features/block/" feature-file))
    (forward-line 1)
    (let ((steps (list)))
      (ecukes-parse-block
       (lambda (step)
         (add-to-list 'steps step t)))
      steps)))

(defun mock-intro ()
  "Creates an `ecukes-intro' mock object."
  (make-ecukes-intro :header "Addition"
                     :description '("In order to avoid silly mistakes"
                                    "As a match idiot"
                                    "I want to be told the sum of two numbers")))

(defun mock-scenario ()
  "Creates an `ecukes-scenario' mock object."
  (make-ecukes-scenario :name "Addition" :steps (list (mock-step))))

(defun mock-background ()
  "Creates an `ecukes-scenario' mock object."
  (make-ecukes-background :steps (list (mock-step))))

(defun mock-step (&optional name)
  "Creates an `ecukes-step' mock object.
Optional NAME is the name of the step."
  (make-ecukes-step :name (or name "Given I have this or that")))


(defun reset-stats ()
  "Resets all stats."
  (setq ecukes-stats-num-steps 0)
  (setq ecukes-stats-num-steps-failed 0)
  (setq ecukes-stats-num-steps-passed 0)
  (setq ecukes-stats-num-scenarios 0)
  (setq ecukes-stats-num-scenarios-failed 0)
  (setq ecukes-stats-num-scenarios-passed 0))


(defun should-be-regular-step (step)
  "Asserts that STEP is a regular step."
  (should-be-type step 'regular))

(defun should-be-py-string-step (step)
  "Asserts that STEP is a py-string step."
  (should-be-type step 'py-string))

(defun should-be-table-step (step)
  "Asserts that STEP is a table step."
  (should-be-type step 'table))

(defun should-be-type (step type)
  "Asserts that STEP is a TYPE step."
  (should (equal type (ecukes-step-type step))))

(defun should-have-tags (scenario &rest tags)
  "Asserts that SCENARIO has all tags in TAGS."
  (let ((actual-tags (ecukes-scenario-tags scenario))
        (expected-tags tags))
    (should (equal (sort actual-tags 'string<) (sort expected-tags 'string<)))))

(defun should-have-step-definition (key value)
  "Asserts that KEY has a step definition and that the return value of
that is VALUE."
  (let ((description (gethash key ecukes-steps-definitions)))
    (should description)
    (should (equal (funcall description) value))))

(defun should-find-definition (step ret-val)
  "Asserts that there's a definition for STEP with return value RET-VAL."
  (should-be-correct-definition (ecukes-steps-find-definition step) ret-val))

(defun should-find-definition-by-name (name ret-val)
  "Asserts that there's a definition for step with NAME with return
value RET-VAL."
  (should-be-correct-definition (ecukes-steps-find-definition-by-name name) ret-val))

(defun should-be-correct-definition (definition ret-val)
  "Generic helper for asserting that definition is found with a
certain return value."
  (should (equal ret-val (funcall (ecukes-step-def-fn definition)))))


