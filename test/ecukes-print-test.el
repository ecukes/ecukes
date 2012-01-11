(ert-deftest print-undefined-steps-should-print-title ()
  "Should print undefined steps title when printing undefined steps."
  (with-mock
   (stub ecukes-print-message)
   (mock (ecukes-print-undefined-steps-title) :times 1)
   (ecukes-print-undefined-steps nil)))

(ert-deftest print-undefined-steps-should-print-steps ()
  "Should print all undefined steps."
  (with-mock
   (stub ecukes-print-message)
   (mock (ecukes-print-undefined-step) :times 2)
   (ecukes-print-undefined-steps '(0 0))))

(ert-deftest print-undefined-step ()
  "Should print undefined step."
  (let ((step (make-ecukes-step :name "Given a known state")))
    (with-mock
     (mock (ecukes-print-message) :times 2)
     (ecukes-print-undefined-step step))))

(ert-deftest print-undefined-step-args-no-argument ()
  "Should print undefined step arguments when no argument."
  (let ((step (make-ecukes-step :name "Given a known state")))
    (should
     (equal
      (ecukes-print-undefined-step-args step) ""))))

(ert-deftest print-undefined-step-args-single-argument ()
  "Should print undefined step arguments when single argument."
  (let ((step (make-ecukes-step :name "Given a \"known\" state")))
    (should
     (equal
      (ecukes-print-undefined-step-args step) "arg"))))

(ert-deftest print-undefined-step-args-multiple-arguments ()
  "Should print undefined step arguments when multiple arguments."
  (let ((step (make-ecukes-step :name "Given state \"known\" and \"unknown\"")))
    (should
     (equal
      (ecukes-print-undefined-step-args step) "arg arg"))))

(ert-deftest print-undefined-step-args-table ()
  "Should print undefined step arguments when table."
  (let ((step (make-ecukes-step :name "Given state \"known\":" :type 'table)))
    (should
     (equal
      (ecukes-print-undefined-step-args step) "arg arg"))))

(ert-deftest print-undefined-step-args-py-string ()
  "Should print undefined step arguments when py-string."
  (let ((step (make-ecukes-step :name "Given state \"known\":" :type 'py-string)))
    (should
     (equal
      (ecukes-print-undefined-step-args step) "arg arg"))))

(ert-deftest print-undefined-step-regex-no-argument ()
  "Should print undefined step regex when no argument."
  (should
   (equal
    (ecukes-print-undefined-step-regex "a known state") "a known state")))

(ert-deftest print-undefined-step-regex-single-argument ()
  "Should print undefined step regex when single argument."
  (should
   (equal
    (ecukes-print-undefined-step-regex "a \"known\" state")
    "a \\\\\"\\\\\\\\(.+\\\\\\\\)\\\\\" state")))

(ert-deftest print-undefined-step-regex-multiple-arguments ()
  "Should print undefined step regex when multiple arguments."
  (should
   (equal
    (ecukes-print-undefined-step-regex "state \"known\" and \"unknown\"")
    "state \\\\\"\\\\\\\\(.+\\\\\\\\)\\\\\" and \\\\\"\\\\\\\\(.+\\\\\\\\)\\\\\"")))

(ert-deftest print-intro ()
  "Should print intro header and description."
  (with-mock
   (let ((intro
          (make-ecukes-intro
           :header "Addition of two numbers"
           :description
           '("In order to aviod silly mistakes"
             "As a math idiot"
             "I want to be told the sum of two numbers"))))
     (mock (ecukes-print-message) :times 5)
     (ecukes-print-intro intro))))

(ert-deftest print-newline ()
  "Should print newline."
  (with-mock
   (mock (ecukes-print-message " "))
   (ecukes-print-newline)))

(ert-deftest print-background-header ()
  "Should print background header."
  (with-mock
   (mock (ecukes-print-message "Background:") :times 1)
   (ecukes-print-background-header)))

(ert-deftest print-step-success ()
  "Should print step success."
  (with-mock
   (mock (ecukes-print-step) :times 1)
   (ecukes-print-step-success nil)))

(ert-deftest print-step-failure ()
  "Should print step failure."
  (with-mock
   (stub ecukes-step-err => "err")
   (mock (ecukes-print-step) :times 1)
   (mock (ecukes-print-error) => "err")
   (ecukes-print-step-failure nil)))

(ert-deftest print-step-pending ()
  "Should print step pending."
  (with-mock
   (mock (ecukes-print-step) :times 1)
   (ecukes-print-step-pending nil)))

(ert-deftest print-error ()
  "Should print error."
  (with-mock
   (mock (ecukes-print-message) :times 1)
   (mock (ansi-red) :times 1)
   (ecukes-print-error "err")))

(ert-deftest print-step ()
  "Should print step."
  (with-mock
   (stub ecukes-step-name => "a known state")
   (mock (ecukes-print-message) :times 1)
   (ecukes-print-step nil 'ignore)))

(ert-deftest print-sceario-header ()
  "Should print header."
  (with-mock
   (stub ecukes-scenario-name => "Foo bar")
   (mock (ecukes-print-message "Scenario: %s" "Foo bar"))
   (ecukes-print-scenario-header nil)))
