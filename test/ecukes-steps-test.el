(ert-deftest steps-define-step-with-no-args ()
  "Should define step with no args."
  (with-steps
   (let ((regex "^a known state$") (fn 'ignore))
     (ecukes-steps-step regex fn)
     (should
      (equal
       (ecukes-steps-find "Given a known state")
       (make-ecukes-step-def :fn fn :args nil))))))

(ert-deftest steps-define-step-with-single-arg ()
  "Should define step with single arg."
  (with-steps
   (let ((regex "^a \\(.+\\) state$") (fn 'ignore))
     (ecukes-steps-step regex fn)
     (should
      (equal
       (ecukes-steps-find "Given a known state")
       (make-ecukes-step-def :fn fn :args '("known")))))))

(ert-deftest steps-define-step-with-multiple-args ()
  "Should define step with multiple args."
  (with-steps
   (let ((regex "^state \\(.+\\) and \\(.+\\)$") (fn 'ignore))
     (ecukes-steps-step regex fn)
     (should
      (equal
       (ecukes-steps-find "Given state known and unknown")
       (make-ecukes-step-def :fn fn :args '("known" "unknown")))))))

(ert-deftest steps-call-step-no-arguments ()
  "Should call step with no arguments."
  (with-steps
   (Given "^a known state$" (lambda () "x"))
   (should (equal (Given "a known state") "x"))))

(ert-deftest steps-call-step-single-argument ()
  "Should call step with single argument."
  (with-steps
   (Given "^a \\(.+\\) state$" (lambda (state) state))
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
   (Given "^a known state$" 'ignore)
   (should-error (Given "an unknown state"))))

(ert-deftest steps-undefined-single-argument ()
  "Should error when not defined, single argument."
  (with-steps
   (Given "^a state \\(.+\\)$" 'ignore)
   (should-error (Given "an state: foo"))))

(ert-deftest steps-undefined-multiple-arguments ()
  "Should error when not defined, multiple arguments."
  (with-steps
   (Given "^a state \\(.+\\) and \\(.+\\)$" 'ignore)
   (should-error (Given "an state foo and bar"))))

(ert-deftest steps-undefined-some-undefined ()
  "Should find some steps when some are undefined."
  (with-steps
   (let ((step-known   (make-ecukes-step :name "Given a known state"))
         (step-unknown (make-ecukes-step :name "Given an unknown state")))
     (ecukes-steps-step "^a known state$" 'ignore)
     (should
      (equal
       (ecukes-steps-undefined (list step-known step-unknown))
       (list step-unknown))))))

(ert-deftest steps-undefined-none-undefined-ask-multiple-for-same ()
  "Should find single step when undefined, but asked for multiple times."
  (with-steps
   (let ((step (make-ecukes-step :name "Given a known state")))
     (should
      (equal
       (ecukes-steps-undefined (list step step step))
       (list step))))))

(ert-deftest ecukes-steps-query-with-prefix ()
  "Should correctly retrieve query with prefix."
  (should
   (equal
    (ecukes-steps-query "Given a known state")
    "a known state")))

(ert-deftest ecukes-steps-query-without-prefix ()
  "Should correctly retrieve query without prefix."
  (should
   (equal
    (ecukes-steps-query "a known state")
    "a known state")))

(ert-deftest ecukes-steps-query-step ()
  "Should correctly retrieve query from step."
  (let ((step (make-ecukes-step :name "Given a known state")))
    (should
     (equal
      (ecukes-steps-query step) "a known state"))))
