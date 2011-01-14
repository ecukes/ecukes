(ert-deftest parse-regular-step-given ()
  "Should parse given step."
  (with-parse-step
   "given"
   (lambda (name type arg)
     (should (eq type 'regular))
     (should (equal name "Given a known state")))))

(ert-deftest parse-regular-step-when ()
  "Should parse when step."
  (with-parse-step
   "when"
   (lambda (name type arg)
     (should (eq type 'regular))
     (should (equal name "When the key action")))))

(ert-deftest parse-regular-step-then ()
  "Should parse then step."
  (with-parse-step
   "then"
   (lambda (name type arg)
     (should (eq type 'regular))
     (should (equal name "Then observe outcomes")))))

(ert-deftest parse-regular-step-and ()
  "Should parse and step."
  (with-parse-step
   "and"
   (lambda (name type arg)
     (should (eq type 'regular))
     (should (equal name "And another key action")))))

(ert-deftest parse-regular-step-but ()
  "Should parse but step."
  (with-parse-step
   "but"
   (lambda (name type arg)
     (should (eq type 'regular))
     (should (equal name "But observe outcomes")))))
