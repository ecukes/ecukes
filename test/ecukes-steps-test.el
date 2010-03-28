(ert-deftest steps-given ()
  (let ((key "Some Key") (value "Some Value"))
    (Given key (lambda () value))
    (should-have-step-definition key value)))

(ert-deftest steps-when ()
  (let ((key "Some Key") (value "Some Value"))
    (When key (lambda () value))
    (should-have-step-definition key value)))

(ert-deftest steps-then ()
  (let ((key "Some Key") (value "Some Value"))
    (Then key (lambda () value))
    (should-have-step-definition key value)))

(ert-deftest steps-and ()
  (let ((key "Some Key") (value "Some Value"))
    (And key (lambda () value))
    (should-have-step-definition key value)))

(ert-deftest steps-but ()
  (let ((key "Some Key") (value "Some Value"))
    (But key (lambda () value))
    (should-have-step-definition key value)))

(ert-deftest steps-with-arguments ()
  (Given "^I have \\(.+\\) in my \\(.+\\)$" (lambda (a b)))
  (let* ((step (mock-step "Given I have a car in my kitchen"))
         (definition (ecukes-steps-find-definition step))
         (fn (car definition))
         (args (car (cdr definition))))
    (should (equal 2 (length args)))))

(ert-deftest steps-definition-does-not-exist ()
  (let* ((step (mock-step "Given this does not exist"))
         (definition (ecukes-steps-find-definition step)))
    (should-not definition)))

(ert-deftest steps-find-given-definition ()
  (Given "^I have given$" (lambda () "given"))
  (let ((step (mock-step "Given I have given")))
    (should-find-definition step "given")))

(ert-deftest steps-find-when-definition ()
  (When "^I have when$" (lambda () "when"))
  (let ((step (mock-step "When I have when")))
    (should-find-definition step "when")))

(ert-deftest steps-find-then-definition ()
  (Then "^I have then$" (lambda () "then"))
  (let ((step (mock-step "Then I have then")))
    (should-find-definition step "then")))

(ert-deftest steps-find-and-definition ()
  (And "^I have and$" (lambda () "and"))
  (let ((step (mock-step "And I have and")))
    (should-find-definition step "and")))

(ert-deftest steps-find-but-definition ()
  (But "^I have but$" (lambda () "but"))
  (let ((step (mock-step "But I have but")))
    (should-find-definition step "but")))


(defun should-have-step-definition (key value)
  (let ((description (gethash key ecukes-steps-definitions)))
    (should description)
    (should (equal (funcall description) value))))

(defun should-find-definition (step ret-val)
  (should (equal ret-val (funcall (car (ecukes-steps-find-definition step))))))
