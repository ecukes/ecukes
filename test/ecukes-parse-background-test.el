(defun with-parse-background (name fn)
  (let* ((feature-file (feature-file-path "background" name))
         (feature (ecukes-parse-feature feature-file))
         (background (ecukes-feature-background feature))
         (steps (if background (ecukes-background-steps background) ())))
    (funcall fn background (mapcar 'ecukes-step-name steps))))

(defun should-parse-background (name)
  "Parsing NAME feature should parse background correctly."
  (with-parse-background
   name
   (lambda (background step-names)
     (should
      (equal
       step-names
       '("Given a known state"
         "When the key action"
         "Then observe outcomes"))))))

(ert-deftest parse-background-all-good ()
  "Should parse background when all good."
  (should-parse-background "all-good"))

(ert-deftest parse-background-with-intro ()
  "Should parse background when intro."
  (should-parse-background "with-intro"))

(ert-deftest parse-background-wrong-indentation ()
  "Should parse background when wrong indentation."
  (should-parse-background "wrong-indentation"))

(ert-deftest parse-background-line-breaks ()
  "Should parse background when line breaks."
  (should-parse-background "line-breaks"))

(ert-deftest parse-background-comment-breaks ()
  "Should parse background when comment breaks."
  (should-parse-background "comment-breaks"))

(ert-deftest parse-background-comments ()
  "Should not parse background when comments."
  (with-parse-background
   "comments"
   (lambda (background step-names)
     (should-not background))))
