(require 'ecukes-steps)

(ert-deftest steps-define-step ()
  "Should define step."
  (with-steps
   (let ((load-file-name "DUMMY/FILE/NAME.el"))
     (Given "^a known state$" 'ignore))
   (should
    (equal
     (make-ecukes-step-def :regex "^a known state$" :fn 'ignore
                           :file "DUMMY/FILE/NAME.el")
     (car ecukes-steps-definitions)))))

(ert-deftest steps-defined-with-doc ()
  "Should record docstring if given."
  (with-steps
   (Given "^a known state$"
     "This step does what."
     'ignore)
   (should
    (equal (ecukes-step-def-doc (car ecukes-steps-definitions))
           "This step does what."))))

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

(ert-deftest steps-call-step-optional-argument ()
  "Should call step with optional argument (and not treat it as a callback if omittted)."
  (with-steps
   (Given "^a \\(un\\)?known state$" (lambda (x) x))
   (should (null (Given "a known state")))
   (should (equal (Given "a unknown state") "un"))))

(ert-deftest steps-call-step-optional-and-single-argument ()
  "Should call step with optional argument and another argument."
  (with-steps
   (Given "^a \\(un\\)?known \\(quantity\\|state\\)$" (lambda (x y) (concat x y)))
   (should (equal (Given "a known quantity") "quantity"))
   (should (equal (Given "a unknown quantity") "unquantity"))))

(ert-deftest steps-call-step-tickle1-optional-argument ()
  "Tickle optional regex."
  (with-steps
   (Given "bar\\(fab\\)?\\(foo\\)?\\(fuz\\)? buck\\(faz\\)?$" (lambda (w x y z) (list w x y  z)))
   (should (equal (Given "barfoo buckfaz") '(nil "foo" nil "faz")))))

(ert-deftest steps-call-step-tickle2-optional-argument ()
  "Tickle optional regex."
  (with-steps
   (Given "bar\\(fab\\)?\\(foo\\)?\\(fuz\\)? buck\\(faz\\)?" (lambda (w x y z) (list w x y z)))
   (should (equal (Given "barfoo buckfaz fabio") '(nil "foo" nil "faz")))))

(ert-deftest steps-call-step-tickle3-optional-argument ()
  "Tickle optional regex."
  (with-steps
   (with-mock
    (mock (error (ansi-red "Step not defined: `barfoo buckfaz fabio`")) :times 1)
    (Given "bar\\(fab\\)?\\(foo\\)?\\(fuz\\)? buck\\(faz\\)?$" #'ignore)
    (Given "barfoo buckfaz fabio"))))

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

(ert-deftest steps-call-step-line-arguments ()
  "Should call step with inline arguments."
  (with-steps
   (Given "^state \\(.+\\) and \\(.+\\)$"
          (lambda (state-1 state-2)
            (format "%s-%s" state-1 state-2)))
   (should (equal (Given "state known and unknown") "known-unknown"))))

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

(ert-deftest steps-args-not-defined-no-arg ()
  "Should return quoted args when not defined when no args."
  (with-steps
   (let ((step (mock-step "Given state known")))
     (should (equal (ecukes-steps-args step) nil)))))

(ert-deftest steps-args-not-defined-args ()
  "Should return quoted args when not defined when args."
  (with-steps
   (let ((step (mock-step "Given state \"known\" and \"unknown\"")))
     (should (equal (ecukes-steps-args step) (list "known" "unknown"))))))
