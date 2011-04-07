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
  (should
   (equal
    (ecukes-print-undefined-step-args "a known state")
    "")))

(ert-deftest print-undefined-step-args-single-argument ()
  "Should print undefined step arguments when single argument."
  (should
   (equal
    (ecukes-print-undefined-step-args "a \"known\" state")
    "arg")))

(ert-deftest print-undefined-step-args-multiple-arguments ()
  "Should print undefined step arguments when multiple arguments."
  (should
   (equal
    (ecukes-print-undefined-step-args "state \"known\" and \"unknown\"")
    "arg arg")))

(ert-deftest print-undefined-step-regex-no-argument ()
  "Should print undefined step regex when no argument."
  (should
   (equal
    (ecukes-print-undefined-step-regex "a known state")
    "a known state")))

(ert-deftest print-undefined-step-regex-single-argument ()
  "Should print undefined step regex when single argument."
  (should
   (equal
    (ecukes-print-undefined-step-regex "a \"known\" state")
    "a \"\\\\(.+\\\\)\" state")))

(ert-deftest print-undefined-step-regex-multiple-arguments ()
  "Should print undefined step regex when multiple arguments."
  (should
   (equal
    (ecukes-print-undefined-step-regex "state \"known\" and \"unknown\"")
    "state \"\\\\(.+\\\\)\" and \"\\\\(.+\\\\)\"")))

(ert-deftest print-intro ()
  ""
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

;; TODO: Test newline