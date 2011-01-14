(defun with-parse-intro (name fn)
  (let* ((feature-file (feature-file-path "intro" name))
         (feature (ecukes-parse-feature feature-file))
         (intro (ecukes-feature-intro feature))
         (header) (description))
    (condition-case err
        (progn
          (setq header (ecukes-intro-header intro))
          (setq description (ecukes-intro-description intro)))
      (error))
    (funcall fn intro header description)))

(defun should-parse-number-addition (name)
  "Parsing NAME feature should parse number addition correctly."
  (with-parse-intro
   name
   (lambda (intro header description)
     (should (equal header "Addition of two numbers"))
     (should
      (equal
       description
       '("In order to aviod silly mistakes"
         "As a math idiot"
         "I want to be told the sum of two numbers"))))))

(ert-deftest parse-intro-no-intro ()
  "Should not parse intro none."
  (with-parse-intro
   "no-intro"
   (lambda (intro header description)
     (should-not intro))))

(ert-deftest parse-intro-comments ()
  "Should not parse when comments."
  (with-parse-intro
   "comments"
   (lambda (intro header description)
     (should-not intro))))

(ert-deftest parse-intro-all-good-new ()
  "Should parse when all good."
  (should-parse-number-addition "all-good"))

(ert-deftest parse-intro-spaces-above ()
  "Should parse when spaces above."
  (should-parse-number-addition "spaces-above"))

(ert-deftest parse-intro-comments-above ()
  "Should parse when commants above."
  (should-parse-number-addition "comments-above"))

(ert-deftest parse-intro-no-space-in-header ()
  "Should parse when no space in header."
  (should-parse-number-addition "no-space-in-header"))

(ert-deftest parse-intro-extra-space-in-header ()
  "Should parse when extra space in header."
  (should-parse-number-addition "extra-space-in-header"))

(ert-deftest parse-intro-wrong-indentation ()
  "Should parse when wrong indentation."
  (should-parse-number-addition "wrong-indentation"))

(ert-deftest parse-intro-line-breaks ()
  "Should parse when line breaks."
  (should-parse-number-addition "line-breaks"))

(ert-deftest parse-intro-new-section-background ()
  "Should stop when entering background."
  (should-parse-number-addition "section-background"))

(ert-deftest parse-intro-new-section-scenario ()
  "Should stop when entering scenario."
  (should-parse-number-addition "section-scenario"))

(ert-deftest parse-intro-fewer-description-lines ()
  "Should parse when fewer description lines."
  (with-parse-intro
   "fewer-description-lines"
   (lambda (intro header description)
     (should (equal header "Addition of two numbers"))
     (should
      (equal
       description
       '("As a math idiot I want to aviod silly mistakes and be told the sum of two numbers"))))))

(ert-deftest parse-intro-more-description-lines ()
  "Should parse when more description lines."
  (with-parse-intro
   "more-description-lines"
   (lambda (intro header description)
     (should (equal header "Addition of two numbers"))
     (should
      (equal
       description
       '("In order to aviod silly mistakes"
         "As a math idiot"
         "And as an idiot in general"
         "I want to be told the sum of two numbers"))))))
