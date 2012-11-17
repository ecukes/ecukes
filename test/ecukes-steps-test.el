(require 'ecukes-steps)

(ert-deftest steps-define-step ()
  "Should define step."
  (with-steps
   (Given "^a known state$" 'ignore)
   (should
    (equal
     (make-ecukes-step-def :regex "^a known state$" :fn 'ignore)
     (car ecukes-steps-definitions)))))

(ert-deftest steps-define-same-step-twice ()
  "Should not define same step twice."
  (with-steps
   (Given "^a known state$" 'ignore)
   (Given "^a known state$" 'ignore)
   (should
    (equal (length ecukes-steps-definitions) 1))))

(ert-deftest steps-call-step-no-arguments ()
  "Should call step with no arguments."
  (with-steps
   (Given "^a known state$" (lambda () "x"))
   (should (equal (Given "a known state") "x"))))

(ert-deftest steps-call-step-single-argument ()
  "Should call step with single argument."
  (with-steps
   (Given "^a \\(.+\\) state$" 'identity)
   (should (equal (Given "a %s state" "known") "known"))))

(ert-deftest steps-call-step-multiple-arguments ()
  "Should call step with multiple arguments."
  (with-steps
   (Given "^state \\(.+\\) and \\(.+\\)$"
          (lambda (state-1 state-2)
            (format "%s-%s" state-1 state-2)))
   (should (equal (Given "state %s and %s" "known" "unknown") "known-unknown"))))

(ert-deftest steps-undefined-no-arguments ()
  "Should error when not defined, no arguments."
  (with-steps
   (with-mock
    (mock (error (ansi-red "Step not defined: `a known state`")) :times 1)
    (Given "a known state"))))

(ert-deftest steps-undefined-single-argument ()
  "Should error when not defined, single argument."
  (with-steps
   (with-mock
    (mock (error (ansi-red "Step not defined: `a known state`")) :times 1)
    (Given "a %s state" "known"))))

(ert-deftest steps-undefined-multiple-arguments ()
  "Should error when not defined, multiple arguments."
  (with-steps
   (with-mock
    (mock (error (ansi-red "Step not defined: `state known and unknown`")) :times 1)
    (Given "state %s and %s" "known" "unknown"))))

(ert-deftest steps-missing-definition-no-steps ()
  "Should return nil when no steps."
  (should-not (ecukes-steps-missing-definition nil)))

(ert-deftest steps-missing-definition-no-definitions ()
  "Should return all steps when all missing."
  (let ((steps (list (mock-step "Given a known state"))))
    (should (equal (ecukes-steps-missing-definition steps) steps))))

(ert-deftest steps-missing-definition-have-definitions ()
  "Should return nil when no steps missing."
  (with-steps
   (let ((steps (list (mock-step "Given a known state"))))
     (Given "^a known state$" 'ignore)
     (should-not (ecukes-steps-missing-definition steps)))))

(ert-deftest steps-missing-definition-have-definitions-with-argument ()
  "Should return nil when no steps missing with argument."
  (with-steps
   (let ((steps (list (mock-step "Given state \"known\""))))
     (Given "^state \"known\"$" 'ignore)
     (should-not (ecukes-steps-missing-definition steps)))))

(ert-deftest steps-missing-definition-some-missing ()
  "Should return missing steps when some missing."
  (with-steps
   (let* ((known
           (mock-step "Given a known state"))
          (unknown
           (mock-step "Given an unknown state"))
          (steps (list known unknown)))
     (Given "^a known state$" 'ignore)
     (should (equal (list unknown) (ecukes-steps-missing-definition steps))))))

(ert-deftest steps-missing-definition-same-steps ()
  "Should return uniq steps when same steps."
  (with-steps
   (let* ((step
           (mock-step "Given a known state"))
          (steps (list step step)))
     (should (equal steps (ecukes-steps-missing-definition steps))))))

(ert-deftest steps-args-no-args ()
  "Should return empty list when no args."
  (with-steps
   (Given "^a known state$" 'ignore)
   (let ((step (mock-step "Given a known state")))
     (should (equal (ecukes-steps-args step) nil)))))

(ert-deftest steps-args-single-arg ()
  "Should return args when single arg."
  (with-steps
   (Given "^state \"\\(.+\\)\"$" 'ignore)
   (let ((step (mock-step "Given state \"known\"")))
     (should (equal (ecukes-steps-args step) (list "known"))))))

(ert-deftest steps-args-multiple-args ()
  "Should return args when multiple args."
  (with-steps
   (Given "^state \"\\(.+\\)\" and \"\\(.+\\)\"$" 'ignore)
   (let ((step (mock-step "Given state \"known\" and \"unknown\"")))
     (should (equal (ecukes-steps-args step) (list "known" "unknown"))))))

(ert-deftest steps-args-without-quotes ()
  "Should return args when multiple (unquoted) args."
  (with-steps
   (Given "^state \\(.+\\) and \\(.+\\)$" 'ignore)
   (let ((step (mock-step "Given state known and unknown")))
     (should (equal (ecukes-steps-args step) (list "known" "unknown"))))))
