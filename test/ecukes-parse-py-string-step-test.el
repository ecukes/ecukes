(require 'ecukes-parse)

(ert-deftest parse-py-string-step-all-good ()
  "Should parse py string step when all good."
  (with-parse-step
   "py-string-all-good"
   (lambda (name head body type arg)
     (should (eq type 'py-string))
     (should (equal arg "Lorem ipsum dolor sit amet.\nCurabitur pellentesque iaculis eros.")))))

(ert-deftest parse-py-string-step-line-break ()
  "Should parse py string step with line break."
  (with-parse-step
   "py-string-line-break"
   (lambda (name head body type arg)
     (should (eq type 'py-string))
     (should (equal arg "Lorem ipsum dolor sit amet.\n\nCurabitur pellentesque iaculis eros.")))))

(ert-deftest parse-py-string-step-wrong-indentation ()
  "Should parse py string when wrong indentation."
  (with-parse-step
   "py-string-wrong-indentation"
   (lambda (name head body type arg)
     (should (eq type 'py-string))
     (should (equal arg "  Lorem ipsum dolor sit amet.\nrabitur pellentesque iaculis eros.")))))

(ert-deftest parse-py-string-step-same-lines ()
  "Should parse py string when same lines."
  (with-parse-step
   "py-string-same-lines"
   (lambda (name head body type arg)
     (should (eq type 'py-string))
     (should (equal arg "Lorem ipsum\nLorem ipsum")))))

(ert-deftest parse-py-string-step-two-empty-lines ()
  "Should parse py string with several empty lines."
  (with-parse-step
   "py-string-two-empty-lines"
   (lambda (name head body type arg)
     (should (eq type 'py-string))
     (should (equal arg "abc\n\ndef\n")))))

(ert-deftest parse-py-string-step-inside ()
  "Should parse py string with step inside."
  (with-parse-step
   "py-string-step-inside"
   (lambda (name head body type arg)
     (should (eq type 'py-string))
     (should (equal arg "And this thing\nAnd this thing")))))

(ert-deftest parse-py-string-in-py-string ()
  "Should parse py string in py string."
  (with-parse-step
   "py-string-in-py-string"
   (lambda (name head body type arg)
     (should (eq type 'py-string))
     (should (equal arg "Given that py string:\n  \"\"\"\n  Given this step\n  \"\"\"")))))

(ert-deftest parse-py-string-with-string ()
  "Should parse py string with string."
  (with-parse-step
   "py-string-with-string"
   (lambda (name head body type arg)
     (should (eq type 'py-string))
     (should (equal arg "some \" string")))))

(ert-deftest wip-parse-py-string-with-backslash ()
  "Should parse py string with backslash."
  (with-parse-step
   "py-string-with-backslash"
   (lambda (name head body type arg)
     (should (eq type 'py-string))
     (should (equal arg "some \\ string")))))
