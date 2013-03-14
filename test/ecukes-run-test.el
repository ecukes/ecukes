(require 'ecukes-run)
(require 'ecukes-hooks)
(require 'ecukes-stats)

(ert-deftest run-features-should-run-setup-hooks ()
  "Should run setup hooks."
  (with-mock
   (stub ecukes-run-feature)
   (stub ecukes-print-stats-summary)
   (mock (setup-mock) :times 1)
   (with-hooks
    (Setup (setup-mock))
    (with-parse-feature
     "simple"
     (lambda (feature intro scenarios background steps)
       (ecukes-run-features (list feature)))))))

(ert-deftest run-features-should-run-teardown-hooks ()
  "Should run teardown hooks."
  (with-mock
   (stub ecukes-run-feature)
   (stub ecukes-print-stats-summary)
   (mock (teardown-mock) :times 1)
   (with-hooks
    (Teardown (teardown-mock))
    (with-parse-feature
     "simple"
     (lambda (feature intro scenarios background steps)
       (ecukes-run-features (list feature)))))))

(ert-deftest run-features-print-summary-once ()
  "Should print summary once."
  (with-mock
   (stub ecukes-run-feature)
   (mock (ecukes-print-stats-summary) :times 1)
   (let ((features
          (list
           (make-ecukes-feature)
           (make-ecukes-feature))))
     (ecukes-run-features features))))

(ert-deftest run-feature-no-background ()
  "Should run feature when no background."
  (with-messages
   (lambda (messages)
     (with-parse-feature
      "no-background"
      (lambda (feature intro scenarios background steps)
        (with-mock
         (stub ecukes-feature-background => nil)
         (stub ecukes-feature-scenarios => nil)
         (mock (ecukes-print-intro intro) :times 1)
         (not-called ecukes-run-background)
         (ecukes-run-feature feature)))))))

(ert-deftest run-feature ()
  "Should run feature."
  (with-messages
   (lambda (messages)
     (with-parse-feature
      "simple"
      (lambda (feature intro scenarios background steps)
        (with-mock
         (mock (ecukes-print-intro intro) :times 1)
         (mock (ecukes-run-background) :times 1)
         (mock (ecukes-run-scenario) :times 1)
         (ecukes-run-feature feature)))))))

(ert-deftest run-feature-hooks-with-background ()
  "Should run feature hooks with background."
  (with-messages
   (lambda (messages)
     (with-mock
      (stub ecukes-print-intro)
      (mock (before-mock) :times 2)
      (mock (after-mock) :times 2)
      (with-hooks
       (Before (before-mock))
       (After (after-mock))
       (let ((feature
              (make-ecukes-feature
               :background (make-ecukes-background)
               :scenarios
               (list
                (make-ecukes-scenario)
                (make-ecukes-scenario)))))
         (ecukes-run-feature feature)))))))

(ert-deftest run-feature-hooks-without-background ()
  "Should run feature hooks without background."
  (with-messages
   (lambda (messages)
     (with-mock
      (stub ecukes-print-intro)
      (mock (before-mock) :times 2)
      (mock (after-mock) :times 2)
      (with-hooks
       (Before (before-mock))
       (After (after-mock))
       (let ((feature
              (make-ecukes-feature
               :scenarios
               (list
                (make-ecukes-scenario)
                (make-ecukes-scenario)))))
         (ecukes-run-feature feature)))))))

(ert-deftest run-feature-no-intro ()
  "Should run feature when no intro."
  (with-mock
   (not-called ecukes-print-intro)
   (let ((feature
          (make-ecukes-feature)))
     (ecukes-run-feature feature))))

(ert-deftest run-scenarios-with-include-tags ()
  "Should run scenarios matching tags."
  (with-messages
   (lambda (messages)
     (with-mock
      (stub ecukes-print-intro)
      (mock (ecukes-run-scenario) :times 1)
      (let ((ecukes-include-tags (list "foo"))
            (feature
             (make-ecukes-feature
              :background (make-ecukes-background)
              :scenarios
              (list
               (make-ecukes-scenario :tags (list "foo"))
               (make-ecukes-scenario :tags (list "bar"))
               (make-ecukes-scenario :tags (list "baz"))))))
        (ecukes-run-feature feature))))))

(ert-deftest run-scenarios-with-exclude-tags ()
  "Should run scenarios non-matching tags."
  (with-messages
   (lambda (messages)
     (with-mock
      (stub ecukes-print-intro)
      (mock (ecukes-run-scenario) :times 2)
      (let ((ecukes-exclude-tags (list "foo"))
            (feature
             (make-ecukes-feature
              :background (make-ecukes-background)
              :scenarios
              (list
               (make-ecukes-scenario :tags (list "foo"))
               (make-ecukes-scenario :tags (list "bar"))
               (make-ecukes-scenario :tags (list "baz"))))))
        (ecukes-run-feature feature))))))

(ert-deftest run-scenarios-with-complex-tags ()
  "Should run scenarios matching tags."
  (with-messages
   (lambda (messages)
     (with-mock
      (stub ecukes-print-intro)
      (mock (ecukes-run-scenario) :times 1)
      (let ((ecukes-include-tags (list "foo"))
            (ecukes-exclude-tags (list "baz"))
            (feature
             (make-ecukes-feature
              :background (make-ecukes-background)
              :scenarios
              (list
               (make-ecukes-scenario :tags (list "foo"))
               (make-ecukes-scenario :tags (list "foo" "baz"))
               (make-ecukes-scenario :tags (list "bar" "baz"))))))
        (ecukes-run-feature feature))))))

(ert-deftest run-background ()
  "Should run background."
  (with-mock
   (mock (ecukes-run-step) => t :times 2)
   (with-messages
    (lambda (messages)
      (let ((success
             (ecukes-run-background
              (make-ecukes-background
               :steps
               (list
                (mock-step "Given a known state")
                (mock-step "Given an unknown state")))))
            (expected
             (list
              "  Background:"
              (s-concat "    " (ansi-green "Given a known state"))
              (s-concat "    " (ansi-green "Given an unknown state"))
              " ")))
        (should (equal success t))
        (should (equal expected messages)))))))

(ert-deftest run-scenario ()
  "Should run scenario."
  (with-mock
   (mock (ecukes-run-step) => t :times 2)
   (with-messages
    (lambda (messages)
      (ecukes-run-scenario
       (make-ecukes-scenario
        :name "Simple"
        :steps
        (list
         (mock-step "Given a known state")
         (mock-step "Given an unknown state")))
       t)
      (let ((expected
             (list
              "  Scenario: Simple"
              (s-concat "    " (ansi-green "Given a known state"))
              (s-concat "    " (ansi-green "Given an unknown state"))
              " ")))
        (should (equal expected messages)))))))

(ert-deftest run-background-before-scenarios ()
  "Should run background before each scenario."
  (with-mock
   (stub ecukes-print-intro)
   (stub ecukes-run-background => t)
   (mock (ecukes-run-scenario) :times 2)
   (mock (ecukes-run-background-steps) :times 1)
   (with-messages
    (lambda (messages)
      (with-steps
       (with-stats
        (let ((feature
               (make-ecukes-feature
                :background (make-ecukes-background)
                :scenarios
                (list
                 (make-ecukes-scenario)
                 (make-ecukes-scenario)))))
          (ecukes-run-feature feature))))))))

(ert-deftest run-background-steps ()
  "Should run background steps."
  (with-mock
   (mock (ecukes-run-step) :times 2)
   (let ((background
          (make-ecukes-background
           :steps
           (list
            (make-ecukes-step)
            (make-ecukes-step)))))
     (ecukes-run-background-steps background))))

(ert-deftest run-background-with-successful-steps-stats ()
  "Should update step stats count when successful steps."
  (with-messages
   (lambda (messages)
     (with-steps
      (with-stats
       (Given "a known state" 'ignore)
       (Given "an unknown state" 'ignore)
       (ecukes-run-background
        (make-ecukes-background
         :steps
         (list
          (mock-step "Given a known state")
          (mock-step "Given an unknown state"))))
       (should (equal ecukes-stats-steps 2))
       (should (equal ecukes-stats-steps-passed 2))
       (should (equal ecukes-stats-steps-failed 0))
       (should (equal ecukes-stats-steps-skipped 0)))))))

(ert-deftest run-background-with-failing-step-stats ()
  "Should update step stats count when failing steps."
  (with-messages
   (lambda (messages)
     (with-steps
      (with-stats
       (Given "a known state" (lambda () (error "ERROR")))
       (Given "an unknown state" 'ignore)
       (ecukes-run-background
        (make-ecukes-background
         :steps
         (list
          (mock-step "Given a known state")
          (mock-step "Given an unknown state"))))
       (should (equal ecukes-stats-steps 2))
       (should (equal ecukes-stats-steps-passed 0))
       (should (equal ecukes-stats-steps-failed 1))
       (should (equal ecukes-stats-steps-skipped 1)))))))

(ert-deftest run-scenario-stats ()
  "Should update scenario stats count."
  (with-messages
   (lambda (messages)
     (with-stats
      (ecukes-run-scenario (make-ecukes-scenario) t)
      (should (equal ecukes-stats-scenarios 1))
      (should (equal ecukes-stats-scenarios-passed 1))
      (should (equal ecukes-stats-scenarios-failed 0))))))

(ert-deftest run-scenario-with-successful-steps-stats ()
  "Should update scenario and step stats count when successful steps."
  (with-messages
   (lambda (messages)
     (with-steps
      (with-stats
       (Given "a known state" 'ignore)
       (Given "an unknown state" 'ignore)
       (ecukes-run-scenario
        (make-ecukes-scenario
         :steps
         (list
          (mock-step "Given a known state")
          (mock-step "Given an unknown state")))
        t)
       (should (equal ecukes-stats-scenarios 1))
       (should (equal ecukes-stats-scenarios-passed 1))
       (should (equal ecukes-stats-scenarios-failed 0))
       (should (equal ecukes-stats-steps 2))
       (should (equal ecukes-stats-steps-passed 2))
       (should (equal ecukes-stats-steps-failed 0))
       (should (equal ecukes-stats-steps-skipped 0)))))))

(ert-deftest run-scenario-with-failing-step-stats ()
  "Should update scenario and step stats count when failing steps."
  (with-messages
   (lambda (messages)
     (with-steps
      (with-stats
       (Given "a known state" (lambda () (error "ERROR")))
       (Given "an unknown state" 'ignore)
       (ecukes-run-scenario
        (make-ecukes-scenario
         :steps
         (list
          (mock-step "Given a known state")
          (mock-step "Given an unknown state")))
        t)
       (should (equal ecukes-stats-scenarios 1))
       (should (equal ecukes-stats-scenarios-passed 0))
       (should (equal ecukes-stats-scenarios-failed 1))
       (should (equal ecukes-stats-steps 2))
       (should (equal ecukes-stats-steps-passed 0))
       (should (equal ecukes-stats-steps-failed 1))
       (should (equal ecukes-stats-steps-skipped 1)))))))

(ert-deftest run-feature-successful-steps-stats ()
  "Should update stats count when running feature all successful."
  (with-mock
   (stub ecukes-print-intro)
   (with-messages
    (lambda (messages)
      (with-steps
       (with-stats
        (Given "a known state" 'ignore)
        (Given "an unknown state" 'ignore)
        (let* ((background
                (make-ecukes-background
                 :steps
                 (list
                  (mock-step "Given a known state"))))
               (scenarios
                (list
                 (make-ecukes-scenario
                  :steps
                  (list
                   (mock-step "Given an unknown state")))))
               (feature
                (make-ecukes-feature
                 :background background
                 :scenarios scenarios)))
          (ecukes-run-feature feature))
        (should (equal ecukes-stats-scenarios 1))
        (should (equal ecukes-stats-scenarios-passed 1))
        (should (equal ecukes-stats-scenarios-failed 0))
        (should (equal ecukes-stats-steps 2))
        (should (equal ecukes-stats-steps-passed 2))
        (should (equal ecukes-stats-steps-failed 0))
        (should (equal ecukes-stats-steps-skipped 0))))))))

(ert-deftest run-feature-with-failing-background-step-stats ()
  "Should update stats count when running feature failing in background."
  (with-mock
   (stub ecukes-print-intro)
   (with-messages
    (lambda (messages)
      (with-steps
       (with-stats
        (Given "a known state" (lambda () (error "ERROR")))
        (Given "an unknown state" 'ignore)
        (let* ((background
                (make-ecukes-background
                 :steps
                 (list
                  (mock-step "Given a known state"))))
               (scenarios
                (list
                 (make-ecukes-scenario
                  :steps
                  (list
                   (mock-step "Given an unknown state")))))
               (feature
                (make-ecukes-feature
                 :background background
                 :scenarios scenarios)))
          (ecukes-run-feature feature))
        (should (equal ecukes-stats-scenarios 1))
        (should (equal ecukes-stats-scenarios-passed 0))
        (should (equal ecukes-stats-scenarios-failed 1))
        (should (equal ecukes-stats-steps 2))
        (should (equal ecukes-stats-steps-passed 0))
        (should (equal ecukes-stats-steps-failed 1))
        (should (equal ecukes-stats-steps-skipped 1))))))))

(ert-deftest run-steps-success ()
  "Should run steps and return t when all successful."
  (with-mock
   (mock (ecukes-print-step) :times 2)
   (with-stats
    (with-steps
     (Given "a known state" 'ignore)
     (Given "an unknown state" 'ignore)
     (let ((steps
            (list
             (mock-step "Given a known state")
             (mock-step "Given an unknown state"))))
       (should (equal (ecukes-run-steps steps t) t)))))))

(ert-deftest run-steps-failure ()
  "Should run steps and return nil when failure."
  (with-mock
   (mock (ecukes-print-step) :times 2)
   (with-stats
    (with-steps
     (Given "a known state" 'ignore)
     (Given "an unknown state" (lambda () (error "ERROR")))
     (let ((steps
            (list
             (mock-step "Given a known state")
             (mock-step "Given an unknown state"))))
       (should (equal (ecukes-run-steps steps t) nil)))))))

(ert-deftest run-step-no-args ()
  "Should run step when no args."
  (with-steps
   (with-mock
    (mock (run-mock) :times 1)
    (Given "a known state" 'run-mock)
    (should
     (ecukes-run-step
      (mock-step "Given a known state"))))))

(ert-deftest run-step-when-args ()
  "Should run step when args."
  (with-steps
   (with-mock
    (mock (run-mock "known" "unknown") :times 1)
    (Given "state \"\\(.+\\)\" and \"\\(.+\\)\"" 'run-mock)
    (should
     (ecukes-run-step
      (mock-step "Given state \"known\" and \"unknown\""))))))

(ert-deftest run-step-error ()
  "Should run failing step and set error."
  (with-steps
   (Given "a known state" (lambda () (error "ERROR")))
   (let ((step (mock-step "Given a known state")))
     (should-not
      (ecukes-run-step step))
     (should
      (equal (ecukes-step-err step) "ERROR")))))

(ert-deftest run-step-when-arg ()
  "Should run step when arg."
  (with-steps
   (with-mock
    (mock (run-mock "py-string") :times 1)
    (Given "this:" 'run-mock)
    (should
     (ecukes-run-step
      (mock-step "Given this:" :type 'table :arg "py-string"))))))

(ert-deftest run-step-with-callback ()
  "Should run step with callback."
  (with-steps
   (Given "^command \\(.+\\)$"
          (lambda (command callback)
            (funcall callback)))
   (should
    (ecukes-run-step
     (mock-step "Given command azmzz")))))
