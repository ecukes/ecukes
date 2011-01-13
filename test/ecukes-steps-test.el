(defmacro with-steps (&rest body)
  `(let ((ecukes-steps-definitions))
     ,@body))


(ert-deftest steps-define-step ()
  "Should define step."
  (with-steps
   (let ((regex "a known state") (fn 'ignore))
     (ecukes-steps-step regex fn)
     (should (equal (ecukes-steps-find regex) fn)))))

(ert-deftest steps-call-step-when-not-defined ()
  "Should call step when not defined."
  (with-steps
   (should-not (Given "a known state"))))

(ert-deftest steps-call-step-no-arguments ()
  "Should call step with no arguments."
  (with-steps
   (Given "a known state" (lambda () "x"))
   (should (equal (Given "a known state") "x"))))

(ert-deftest steps-call-step-single-argument ()
  "Should call step with single argument."
  (with-steps
   (Given "a \\(.+\\) state" (lambda (state) state))
   (should (equal (Given "a %s state" "known") "known"))))

(ert-deftest steps-call-step-multiple-arguments ()
  "Should call step with multiple arguments."
  (with-steps
   (Given "state \\(.+\\) and \\(.+\\)"
          (lambda (state1 state2)
            (format "%s-%s" state1 state2)))
   (should (equal (Given "state %s and %s" "known" "unknown") "known-unknown"))))

(ert-deftest steps-undefined-none-undefined ()
  "Should not find any steps when none is undefined."
  (with-steps
   (let ((step (make-ecukes-step :name "a known state")))
     (ecukes-steps-step "a known state" 'ignore)
     (should-not (ecukes-steps-undefined (list step))))))

(ert-deftest steps-undefined-single-undefined ()
  "Should find single step when undefined."
  (with-steps
   (let ((step (make-ecukes-step :name "a known state")))
     (should
      (equal
       (ecukes-steps-undefined (list step))
       (list step))))))

(ert-deftest steps-undefined-multiple-undefined ()
  "Should find all steps when all are undefined."
  (with-steps
   (let ((step-known (make-ecukes-step :name "a known state"))
         (step-unknown (make-ecukes-step :name "an unknown state")))
     (should
      (equal
       (ecukes-steps-undefined (list step-known step-unknown))
       (list step-known step-unknown))))))

(ert-deftest steps-undefined-some-undefined ()
  "Should find some steps when some are undefined."
  (with-steps
   (let ((step-known (make-ecukes-step :name "a known state"))
         (step-unknown (make-ecukes-step :name "an unknown state")))
     (ecukes-steps-step "a known state" 'ignore)
     (should
      (equal
       (ecukes-steps-undefined (list step-known step-unknown))
       (list step-unknown))))))

(ert-deftest steps-undefined-none-undefined-ask-multiple-for-same ()
  "Should find single step when undefined, but asked for multiple times."
  (with-steps
   (let ((step (make-ecukes-step :name "a known state")))
     (should
      (equal
       (ecukes-steps-undefined (list step step step))
       (list step))))))