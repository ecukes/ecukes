(require 'ecukes-steps)

(ert-deftest lispy-steps-define-step ()
  "Should define step."
  (with-steps
   (let ((load-file-name "DUMMY/FILE/NAME.el"))
     (Given '(^a known state$) 'ignore))
   (should
    (equal
     (make-ecukes-step-def :regex "^a known state$" :fn 'ignore
                           :file "DUMMY/FILE/NAME.el")
     (car ecukes-steps-definitions)))))

(ert-deftest lispy-steps-defined-with-doc ()
  "Should record docstring if given."
  (with-steps
   (Given '(a known state)
     "This step does what."
     'ignore)
   (should
    (equal (ecukes-step-def-doc (car ecukes-steps-definitions))
           "This step does what."))))

(ert-deftest lispy-steps-define-same-step-twice ()
  "Should not define same step twice."
  (with-steps
   (Given '(a known state) 'ignore)
   (Given '(a known state) 'ignore)
   (should
    (equal (length ecukes-steps-definitions) 1))))

(ert-deftest lispy-steps-call-step-no-arguments ()
  "Should call step with no arguments."
  (with-steps
   (Given '(a known state) (lambda () "x"))
   (should (equal (Given "a known state") "x"))))

(ert-deftest lispy-steps-call-step-single-argument ()
  "Should call step with single argument."
  (with-steps
   (Given '(a 'symbol state) 'identity)
   (should (equal (Given "a %s state" "known") "known"))))

(ert-deftest lispy-steps-call-step-multiple-arguments ()
  "Should call step with multiple arguments."
  (with-steps
   (Given '(state 'symbol and 'symbol)
          (lambda (state-1 state-2)
            (format "%s-%s" state-1 state-2)))
   (should (equal (Given "state %s and %s" "known" "unknown") "known-unknown"))))

(ert-deftest lispy-steps-args-no-args ()
  "Should return empty list when no args."
  (with-steps
   (Given '(a known state) 'ignore)
   (let ((step (mock-step "Given a known state")))
     (should (equal (ecukes-steps-args step) nil)))))

(ert-deftest lispy-steps-args-single-arg ()
  "Should return args when single arg."
  (with-steps
   (Given '(state 'string) 'ignore)
   (let ((step (mock-step "Given state \"known\"")))
     (should (equal (ecukes-steps-args step) (list "known"))))))

(ert-deftest lispy-steps-args-multiple-args ()
  "Should return args when multiple args."
  (with-steps
   (Given '(state 'string and 'string) 'ignore)
   (let ((step (mock-step "Given state \"known\" and \"unknown\"")))
     (should (equal (ecukes-steps-args step) (list "known" "unknown"))))))

(ert-deftest lispy-steps-args-without-quotes ()
  "Should return args when multiple (unquoted) args."
  (with-steps
   (Given '(state 'symbol and 'symbol) 'ignore)
   (let ((step (mock-step "Given state known and unknown")))
     (should (equal (ecukes-steps-args step) (list "known" "unknown"))))))

(ert-deftest lispy-steps-several-strings ()
  (with-steps
   (Given '(a 'string best 'string rest 'string)
     (lambda (lorem ipsum merol)
       (format "%s-%s-%s" lorem ipsum merol)))
   (should (equal (Given "a \"%s\" best \"%s\" rest \"%s\""
                    "lorem ipsum"
                    "ipsum lorem"
                    "merol muspi")
                  "lorem ipsum-ipsum lorem-merol muspi"))))

(ert-deftest lispy-steps-several-symbols ()
  (with-steps
   (Given '(a 'symbol best 'symbol rest 'symbol)
     (lambda (lorem ipsum merol)
       (format "%s-%s-%s" lorem ipsum merol)))
   (should (equal (Given "a \"%s\" best \"%s\" rest \"%s\""
                    "lorem"
                    "ipsum"
                    "merol")
                  "lorem-ipsum-merol"))))

(ert-deftest lispy-steps-string-with-quote ()
  (with-steps
   (Given '(a 'string state)
     (lambda (string)
       (format "%s" string)))
   (should (equal (Given "a \"%s\" state"
                    "lorem \" ipsum")
                  "lorem \" ipsum"))))

(ert-deftest lispy-steps-strange-symbol ()
  "Strange symbol but still correct (means without a space
inside)."
  (with-steps
   (Given '(a 'symbol state)
     (lambda (symbol)
       (format "%s" symbol)))
   (should (equal (Given "a %s state"
                    ";.,p&[{}(=h+))*731%9190242!##")
                  ";.,p&[{}(=h+))*731%9190242!##"))))

(ert-deftest lispy-steps-several-strings-with-quotes ()
  (with-steps
   (Given '(a 'string best 'string rest 'string)
     (lambda (lorem ipsum merol)
       (format "%s-%s-%s" lorem ipsum merol)))
   (should (equal (Given "a \"%s\" best \"%s\" rest \"%s\""
                    "lorem \" ipsum"
                    "ipsum \" lorem"
                    "merol \" muspi")
                  "lorem \" ipsum-ipsum \" lorem-merol \" muspi"))))
