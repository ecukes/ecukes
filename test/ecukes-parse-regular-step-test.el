(require 'ecukes-parse)

(ert-deftest parse-regular-step-given ()
  "Should parse given step."
  (with-parse-step
   "given"
   (lambda (name head body type arg)
     (should (eq type 'regular))
     (should (equal name "Given a known state"))
     (should (equal body "a known state")))))

(ert-deftest parse-regular-step-when ()
  "Should parse when step."
  (with-parse-step
   "when"
   (lambda (name head body type arg)
     (should (eq type 'regular))
     (should (equal name "When the key action"))
     (should (equal body "the key action")))))

(ert-deftest parse-regular-step-then ()
  "Should parse then step."
  (with-parse-step
   "then"
   (lambda (name head body type arg)
     (should (eq type 'regular))
     (should (equal name "Then observe outcomes"))
     (should (equal body "observe outcomes")))))

(ert-deftest parse-regular-step-and ()
  "Should parse and step."
  (with-parse-step
   "and"
   (lambda (name head body type arg)
     (should (eq type 'regular))
     (should (equal name "And another key action"))
     (should (equal body "another key action")))))

(ert-deftest parse-regular-step-but ()
  "Should parse but step."
  (with-parse-step
   "but"
   (lambda (name head body type arg)
     (should (eq type 'regular))
     (should (eq type 'regular))
     (should (equal name "But observe outcomes")))))

(ert-deftest parse-regular-step-with-string ()
  "Should parse but step."
  (with-parse-step
   "with-string"
   (lambda (name head body type arg)
     (should (eq type 'regular))
     (should (equal name "Given I have \"a \"neat\"\" attitude"))
     (should (equal body "I have \"a \"neat\"\" attitude")))))
