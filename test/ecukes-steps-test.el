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
  (let ((steps (list (make-ecukes-step :name "Given a known state" :type 'regular))))
    (should (equal (ecukes-steps-missing-definition steps) steps))))

(ert-deftest steps-missing-definition-have-definitions ()
  "Should return nil when no steps missing."
  (with-steps
   (let ((steps (list (make-ecukes-step :name "Given a known state" :type 'regular))))
     (Given "a known state" 'ignore)
     (should-not (ecukes-steps-missing-definition steps)))))

(ert-deftest steps-missing-definition-have-definitions-with-argument ()
  "Should return nil when no steps missing with argument."
  (with-steps
   (let ((steps (list (make-ecukes-step :name "Given state \"known\"" :type 'regular))))
     (Given "state \"known\"" 'ignore)
     (should-not (ecukes-steps-missing-definition steps)))))

(ert-deftest steps-missing-definition-some-missing ()
  "Should return missing steps when some missing."
  (with-steps
   (let* ((known
           (make-ecukes-step :name "Given a known state" :type 'regular))
          (unknown
           (make-ecukes-step :name "Given an unknown state" :type 'regular))
          (steps (list known unknown)))
     (Given "a known state" 'ignore)
     (should (equal (list unknown) (ecukes-steps-missing-definition steps))))))

(ert-deftest steps-missing-definition-same-steps ()
  "Should return uniq steps when same steps."
  (with-steps
   (let* ((step
           (make-ecukes-step :name "Given a known state" :type 'regular))
          (steps (list step step)))
     (should (equal (list step) (ecukes-steps-missing-definition steps))))))

(ert-deftest steps-missing-definition-same-step-name-different-args ()
  "Should return uniq steps when same name but different args."
  (with-steps
   (let* ((without-arg
           (make-ecukes-step :name "Given a known state" :type 'regular :arg nil))
          (with-arg
           (make-ecukes-step :name "Given a known state" :type 'regular :arg '(table-data)))
          (steps (list without-arg with-arg)))
     (should (equal steps (ecukes-steps-missing-definition steps))))))
