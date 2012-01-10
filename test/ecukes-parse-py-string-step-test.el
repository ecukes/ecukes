(ert-deftest parse-py-string-step-all-good ()
  "Should parse py string step when all good."
  (with-parse-step
   "py-string-all-good"
   (lambda (name type arg)
     (should (eq type 'py-string))
     (should (equal arg "Lorem ipsum dolor sit amet.\nCurabitur pellentesque iaculis eros.")))))

(ert-deftest parse-py-string-step-line-break ()
  "Should parse py string step with line break."
  (with-parse-step
   "py-string-line-break"
   (lambda (name type arg)
     (should (eq type 'py-string))
     (should (equal arg "Lorem ipsum dolor sit amet.\n\nCurabitur pellentesque iaculis eros.")))))

(ert-deftest parse-py-string-step-wrong-indentation ()
  "Should parse py string when wrong indentation."
  (with-parse-step
   "py-string-wrong-indentation"
   (lambda (name type arg)
     (should (eq type 'py-string))
     (should (equal arg "  Lorem ipsum dolor sit amet.\nrabitur pellentesque iaculis eros.")))))

(ert-deftest parse-py-string-step-same-lines ()
  "Should parse py string when same lines."
  (with-parse-step
   "py-string-same-lines"
   (lambda (name type arg)
     (should (eq type 'py-string))
     (should (equal arg "Lorem ipsum\nLorem ipsum")))))
