(require 'ecukes-run)

(setq ecukes-async-timeout 0.1)
(setq ecukes-include-tags nil)
(setq ecukes-exclude-tags nil)


;;;; ecukes-run

(ert-deftest ecukes-run-test/run-steps-missing-definition-hook ()
  (with-mock
   (with-reporter-hooks
    (stub ecukes-steps-without-definition => "steps")
    (let (hook-has-run)
      (add-hook 'ecukes-reporter-steps-without-definition-hook
                (lambda (steps)
                  (setq hook-has-run t)
                  (should (equal steps "steps"))))
      (ecukes-run nil)
      (should hook-has-run)))))

(ert-deftest ecukes-run-test/run-start-hook ()
  (with-mock
   (with-reporter-hooks
    (stub ecukes-steps-without-definition)
    (let (hook-has-run)
      (add-hook 'ecukes-reporter-start-hook
                (lambda ()
                  (setq hook-has-run t)))
      (ecukes-run nil)
      (should hook-has-run)))))

(ert-deftest ecukes-run-test/run-end-hook ()
  (with-mock
   (with-stats
    (ecukes-stats-scenario-pass)
    (ecukes-stats-scenario-fail)
    (ecukes-stats-step-pass)
    (ecukes-stats-step-fail)
    (ecukes-stats-step-skip)
    (with-reporter-hooks
     (stub ecukes-steps-without-definition)
     (let (hook-has-run)
       (add-hook 'ecukes-reporter-end-hook
                 (lambda (stats)
                   (should
                    (equal
                     stats
                     '((scenarios        . 2)
                       (scenarios-passed . 1)
                       (scenarios-failed . 1)
                       (steps            . 3)
                       (steps-passed     . 1)
                       (steps-failed     . 1)
                       (steps-skipped    . 1))))
                   (setq hook-has-run t)))
       (ecukes-run nil)
       (should hook-has-run))))))

(ert-deftest ecukes-run-test/run-setup-hook ()
  (with-mock
   (stub ecukes-steps-without-definition)
   (mock (ecukes-hooks-run-setup) :times 1)
   (ecukes-run nil)))

(ert-deftest ecukes-run-test/run-teardown-hook ()
  (with-mock
   (stub ecukes-steps-without-definition)
   (mock (ecukes-hooks-run-teardown) :times 1)
   (ecukes-run nil)))


;;;; ecukes-run-features

(ert-deftest ecukes-run-test/run-features-before-first-feature-hook ()
  (with-reporter-hooks
   (with-mock
    (stub ecukes-run-feature)
    (let (hook-has-run)
      (add-hook 'ecukes-reporter-before-first-feature-hook
                (lambda (feature)
                  (should (eq feature 'feature-1))
                  (setq hook-has-run t)))
      (ecukes-run-features '(feature-1 feature-2))
      (should hook-has-run)))))

(ert-deftest ecukes-run-test/run-features-before-last-feature-hook ()
  (with-reporter-hooks
   (with-mock
    (stub ecukes-run-feature)
    (let (hook-has-run)
      (add-hook 'ecukes-reporter-before-last-feature-hook
                (lambda (feature)
                  (should (eq feature 'feature-2))
                  (setq hook-has-run t)))
      (ecukes-run-features '(feature-1 feature-2))
      (should hook-has-run)))))

(ert-deftest ecukes-run-test/run-features-before-feature-hook ()
  (with-reporter-hooks
   (with-mock
    (stub ecukes-run-feature)
    (let (hook-has-run feature-1 feature-2)
      (add-hook 'ecukes-reporter-before-feature-hook
                (lambda (feature)
                  (if (eq feature 'feature-1) (setq feature-1 t))
                  (if (eq feature 'feature-2) (setq feature-2 t))
                  (setq hook-has-run t)))
      (ecukes-run-features '(feature-1 feature-2))
      (should hook-has-run)
      (should feature-1)
      (should feature-2)))))

(ert-deftest ecukes-run-test/run-features-after-first-feature-hook ()
  (with-reporter-hooks
   (with-mock
    (stub ecukes-run-feature)
    (let (hook-has-run)
      (add-hook 'ecukes-reporter-after-first-feature-hook
                (lambda (feature)
                  (should (eq feature 'feature-1))
                  (setq hook-has-run t)))
      (ecukes-run-features '(feature-1 feature-2))
      (should hook-has-run)))))

(ert-deftest ecukes-run-test/run-features-after-last-feature-hook ()
  (with-reporter-hooks
   (with-mock
    (stub ecukes-run-feature)
    (let (hook-has-run)
      (add-hook 'ecukes-reporter-after-last-feature-hook
                (lambda (feature)
                  (should (eq feature 'feature-2))
                  (setq hook-has-run t)))
      (ecukes-run-features '(feature-1 feature-2))
      (should hook-has-run)))))

(ert-deftest ecukes-run-test/run-features-after-feature-hook ()
  (with-reporter-hooks
   (with-mock
    (stub ecukes-run-feature)
    (let (hook-has-run feature-1 feature-2)
      (add-hook 'ecukes-reporter-after-feature-hook
                (lambda (feature)
                  (if (eq feature 'feature-1) (setq feature-1 t))
                  (if (eq feature 'feature-2) (setq feature-2 t))
                  (setq hook-has-run t)))
      (ecukes-run-features '(feature-1 feature-2))
      (should hook-has-run)
      (should feature-1)
      (should feature-2)))))


;;;; ecukes-run-feature

(ert-deftest ecukes-run-test/run-feature-before-first-scenario-hook ()
  (with-mock
   (with-reporter-hooks
    (let* (hook-has-run
           (scenario-1 (make-ecukes-scenario))
           (scenario-2 (make-ecukes-scenario))
           (feature (make-ecukes-feature :scenarios (list scenario-1 scenario-2))))
      (add-hook 'ecukes-reporter-before-first-scenario-hook
                (lambda (scenario)
                  (should (equal scenario scenario-1))
                  (setq hook-has-run t)))
      (ecukes-run-feature feature)
      (should hook-has-run)))))

(ert-deftest ecukes-run-test/run-feature-before-last-scenario-hook ()
  (with-mock
   (with-reporter-hooks
    (let* (hook-has-run
           (scenario-1 (make-ecukes-scenario))
           (scenario-2 (make-ecukes-scenario))
           (feature (make-ecukes-feature :scenarios (list scenario-1 scenario-2))))
      (add-hook 'ecukes-reporter-before-first-scenario-hook
                (lambda (scenario)
                  (should (equal scenario scenario-2))
                  (setq hook-has-run t)))
      (ecukes-run-feature feature)
      (should hook-has-run)))))

(ert-deftest ecukes-run-test/run-feature-before-scenario-hook ()
  (with-mock
   (with-reporter-hooks
    (let* (hook-has-run
           scenario-1-in-hook
           scenario-2-in-hook
           (scenario-1 (make-ecukes-scenario))
           (scenario-2 (make-ecukes-scenario))
           (feature (make-ecukes-feature :scenarios (list scenario-1 scenario-2))))
      (add-hook 'ecukes-reporter-before-scenario-hook
                (lambda (scenario)
                  (cond ((eq scenario scenario-1)
                         (setq scenario-1-in-hook t))
                        ((eq scenario scenario-2)
                         (setq scenario-2-in-hook t)))
                  (setq hook-has-run t)))
      (ecukes-run-feature feature)
      (should hook-has-run)
      (should scenario-1-in-hook)
      (should scenario-2-in-hook)))))

(ert-deftest ecukes-run-test/run-feature-after-first-scenario-hook ()
  (with-mock
   (with-reporter-hooks
    (let* (hook-has-run
           (scenario-1 (make-ecukes-scenario))
           (scenario-2 (make-ecukes-scenario))
           (feature (make-ecukes-feature :scenarios (list scenario-1 scenario-2))))
      (add-hook 'ecukes-reporter-after-first-scenario-hook
                (lambda (scenario)
                  (should (equal scenario scenario-1))
                  (setq hook-has-run t)))
      (ecukes-run-feature feature)
      (should hook-has-run)))))

(ert-deftest ecukes-run-test/run-feature-after-last-scenario-hook ()
  (with-mock
   (with-reporter-hooks
    (let* (hook-has-run
           (scenario-1 (make-ecukes-scenario))
           (scenario-2 (make-ecukes-scenario))
           (feature (make-ecukes-feature :scenarios (list scenario-1 scenario-2))))
      (add-hook 'ecukes-reporter-after-first-scenario-hook
                (lambda (scenario)
                  (should (equal scenario scenario-2))
                  (setq hook-has-run t)))
      (ecukes-run-feature feature)
      (should hook-has-run)))))

(ert-deftest ecukes-run-test/run-feature-after-scenario-hook ()
  (with-mock
   (with-reporter-hooks
    (let* (hook-has-run
           scenario-1-in-hook
           scenario-2-in-hook
           (scenario-1 (make-ecukes-scenario))
           (scenario-2 (make-ecukes-scenario))
           (feature (make-ecukes-feature :scenarios (list scenario-1 scenario-2))))
      (add-hook 'ecukes-reporter-after-scenario-hook
                (lambda (scenario)
                  (cond ((eq scenario scenario-1)
                         (setq scenario-1-in-hook t))
                        ((eq scenario scenario-2)
                         (setq scenario-2-in-hook t)))
                  (setq hook-has-run t)))
      (ecukes-run-feature feature)
      (should hook-has-run)
      (should scenario-1-in-hook)
      (should scenario-2-in-hook)))))

(ert-deftest ecukes-run-test/run-feature-before-background-hook ()
  (with-mock
   (with-reporter-hooks
    (let* (hook-has-run (feature (make-ecukes-feature :background (make-ecukes-background))))
      (add-hook 'ecukes-reporter-before-background-hook
                (lambda ()
                  (setq hook-has-run t)))
      (ecukes-run-feature feature)
      (should hook-has-run)))))

(ert-deftest ecukes-run-test/run-feature-after-background-hook ()
  (with-mock
   (with-reporter-hooks
    (let* (hook-has-run (feature (make-ecukes-feature :background (make-ecukes-background))))
      (add-hook 'ecukes-reporter-after-background-hook
                (lambda ()
                  (setq hook-has-run t)))
      (ecukes-run-feature feature)
      (should hook-has-run)))))

(ert-deftest ecukes-run-test/run-feature-before-hooks ()
  (with-mock
   (mock (ecukes-hooks-run-before) :times 1)
   (ecukes-run-feature
    (make-ecukes-feature :scenarios (list (make-ecukes-scenario))))))

(ert-deftest ecukes-run-test/run-feature-after-hooks ()
  (with-mock
   (mock (ecukes-hooks-run-after) :times 1)
   (ecukes-run-feature
    (make-ecukes-feature :scenarios (list (make-ecukes-scenario))))))

(ert-deftest ecukes-run-test/run-feature-include-tags ()
  (with-reporter-hooks
   (let* (scenario-2-in-hook
          scenario-3-in-hook
          (scenario-1 (make-ecukes-scenario))
          (scenario-2 (make-ecukes-scenario :tags '(foo)))
          (scenario-3 (make-ecukes-scenario :tags '(foo bar)))
          (feature (make-ecukes-feature
                    :scenarios (list scenario-1 scenario-2 scenario-3))))
     (add-hook 'ecukes-reporter-before-scenario-hook
               (lambda (scenario)
                 (cond ((eq scenario scenario-1)
                        (should-not "include scenario-1"))
                       ((eq scenario scenario-2)
                        (setq scenario-2-in-hook t))
                       ((eq scenario scenario-3)
                        (setq scenario-3-in-hook t)))))
     (let ((ecukes-include-tags '(foo)))
       (ecukes-run-feature feature))
     (should scenario-2-in-hook)
     (should scenario-3-in-hook))))

(ert-deftest ecukes-run-test/run-feature-exclude-tags ()
  (with-reporter-hooks
   (let* ((scenario-1 (make-ecukes-scenario))
          (scenario-2 (make-ecukes-scenario :tags '(foo)))
          (scenario-3 (make-ecukes-scenario :tags '(foo bar)))
          (feature (make-ecukes-feature
                    :scenarios (list scenario-1 scenario-2 scenario-3))))
     (add-hook 'ecukes-reporter-before-scenario-hook
               (lambda (scenario)
                 (should (equal scenario scenario-1))))
     (let ((ecukes-exclude-tags '(foo)))
       (ecukes-run-feature feature)))))

(ert-deftest ecukes-run-test/run-feature-include-and-exclude-tags ()
  (with-reporter-hooks
   (let* (scenario-2-in-hook
          (scenario-1 (make-ecukes-scenario))
          (scenario-2 (make-ecukes-scenario :tags '(foo)))
          (scenario-3 (make-ecukes-scenario :tags '(foo bar)))
          (feature (make-ecukes-feature
                    :scenarios (list scenario-1 scenario-2 scenario-3))))
     (add-hook 'ecukes-reporter-before-scenario-hook
               (lambda (scenario)
                 (cond ((eq scenario scenario-1)
                        (should-not "include scenario-1"))
                       ((eq scenario scenario-2)
                        (setq scenario-2-in-hook t))
                       ((eq scenario scenario-3)
                        (should-not "include scenario-2")))))
     (let ((ecukes-include-tags '(foo))
           (ecukes-exclude-tags '(bar)))
       (ecukes-run-feature feature))
     (should scenario-2-in-hook))))

(ert-deftest ecukes-run-test/run-feature-background-no-scenarios-foo ()
  (with-mock
   (mock (ecukes-run-background) :times 1)
   (not-called ecukes-run-background-steps)
   (let ((feature (make-ecukes-feature :background (make-ecukes-background))))
     (ecukes-run-feature feature))))

(ert-deftest ecukes-run-test/run-feature-background-single-scenario-foo ()
  (with-mock
   (stub ecukes-run-scenario => t)
   (mock (ecukes-run-background) :times 1)
   (not-called ecukes-run-background-steps)
   (let ((feature (make-ecukes-feature
                   :background (make-ecukes-background)
                   :scenarios (list (make-ecukes-scenario)))))
     (ecukes-run-feature feature))))

(ert-deftest ecukes-run-test/run-feature-background-multiple-scenarios-foo ()
  (with-mock
   (stub ecukes-run-scenario => t)
   (stub ecukes-run-background => t)
   (mock (ecukes-run-background-steps) :times 1)
   (let ((feature (make-ecukes-feature
                   :background (make-ecukes-background)
                   :scenarios (list (make-ecukes-scenario) (make-ecukes-scenario)))))
     (ecukes-run-feature feature))))


;;;; ecukes-run-background-steps

(ert-deftest ecukes-run-test/run-background-steps ()
  (with-mock
   (mock (ecukes-run-step) :times 2)
   (ecukes-run-background-steps
    (make-ecukes-background :steps '(step-1 step-2)))))


;;;; ecukes-run-background

(ert-deftest ecukes-run-test/run-background ()
  (with-mock
   (mock (ecukes-run-steps '(step-1 step-2)) :times 1)
   (ecukes-run-background
    (make-ecukes-background :steps '(step-1 step-2)))))

(ert-deftest ecukes-run-test/run-background-return-true-on-success ()
  (with-mock
   (stub ecukes-run-steps => t)
   (should
    (ecukes-run-background
     (make-ecukes-background :steps '(step-1 step-2))))))

(ert-deftest ecukes-run-test/run-background-return-false-on-failure ()
  (with-mock
   (stub ecukes-run-steps)
   (should-not
    (ecukes-run-background
     (make-ecukes-background :steps '(step-1 step-2))))))


;;;; ecukes-run-scenario

(ert-deftest ecukes-run-test/run-scenario-stats-success ()
  (with-mock
   (with-stats
    (with-reporter-hooks
     (stub ecukes-run-steps => t)
     (not-called ecukes-stats-scenario-fail)
     (mock (ecukes-stats-scenario-pass) :times 1)
     (ecukes-run-scenario
      (make-ecukes-scenario :steps '(step-1 step-2)) t)))))

(ert-deftest ecukes-run-test/run-scenario-stats-failure ()
  (with-mock
   (with-stats
    (with-reporter-hooks
     (stub ecukes-run-steps => nil)
     (not-called ecukes-stats-scenario-pass)
     (mock (ecukes-stats-scenario-fail) :times 1)
     (ecukes-run-scenario
      (make-ecukes-scenario :steps '(step-1 step-2)) t)))))

(ert-deftest ecukes-run-test/run-scenario-hook-success ()
  (with-mock
   (with-stats
    (with-reporter-hooks
     (stub ecukes-run-steps => t)
     (stub ecukes-stats-scenario-fail)
     (stub ecukes-stats-scenario-pass)
     (let (hook-has-run (scenario (make-ecukes-scenario :steps '(step-1 step-2))))
       (add-hook 'ecukes-reporter-scenario-failed-hook
                 (lambda (scenario)
                   (should-not "call this")))
       (add-hook 'ecukes-reporter-scenario-passed-hook
                 (lambda (_scenario)
                   (should (equal scenario _scenario))
                   (setq hook-has-run t)))
       (ecukes-run-scenario scenario t)
       (should hook-has-run))))))

(ert-deftest ecukes-run-test/run-scenario-hook-failure ()
  (with-mock
   (with-stats
    (with-reporter-hooks
     (stub ecukes-run-steps => nil)
     (stub ecukes-stats-scenario-fail)
     (stub ecukes-stats-scenario-pass)
     (let (hook-has-run (scenario (make-ecukes-scenario :steps '(step-1 step-2))))
       (add-hook 'ecukes-reporter-scenario-passed-hook
                 (lambda (scenario)
                   (should-not "call this")))
       (add-hook 'ecukes-reporter-scenario-failed-hook
                 (lambda (_scenario)
                   (should (equal scenario _scenario))
                   (setq hook-has-run t)))
       (ecukes-run-scenario scenario t)
       (should hook-has-run))))))


;;;; ecukes-run-steps

(ert-deftest ecukes-run-test/run-steps-before-first-step-hook ()
  (with-mock
   (stub ecukes-run-step)
   (let (hook-has-run
         (step-1 (make-ecukes-step))
         (step-2 (make-ecukes-step)))
     (with-reporter-hooks
      (add-hook 'ecukes-reporter-before-first-step-hook
                (lambda (step success)
                  (should (equal step step-1))
                  (should success)
                  (setq hook-has-run t)))
      (ecukes-run-steps (list step-1 step-2) t)
      (should hook-has-run)))))

(ert-deftest ecukes-run-test/run-steps-before-last-step-hook ()
  (with-mock
   (stub ecukes-run-step)
   (let (hook-has-run
         (step-1 (make-ecukes-step))
         (step-2 (make-ecukes-step)))
     (with-reporter-hooks
      (add-hook 'ecukes-reporter-before-last-step-hook
                (lambda (step success)
                  (should (equal step step-2))
                  (should success)
                  (setq hook-has-run t)))
      (ecukes-run-steps (list step-1 step-2) t)
      (should hook-has-run)))))

(ert-deftest ecukes-run-test/run-steps-before-step-hook ()
  (with-mock
   (stub ecukes-run-step)
   (let (hook-has-run step-1 step-2)
     (with-reporter-hooks
      (add-hook 'ecukes-reporter-before-last-step-hook
                (lambda (step success)
                  (setq step-1 t)
                  (setq step-2 t)
                  (setq hook-has-run t)))
      (ecukes-run-steps (list (make-ecukes-step) (make-ecukes-step)) t)
      (should hook-has-run)
      (should step-1)
      (should step-2)))))

(ert-deftest ecukes-run-test/run-steps-after-first-step-hook ()
  (with-mock
   (stub ecukes-run-step)
   (let (hook-has-run
         (step-1 (make-ecukes-step))
         (step-2 (make-ecukes-step)))
     (with-reporter-hooks
      (add-hook 'ecukes-reporter-after-first-step-hook
                (lambda (step success)
                  (should (equal step step-1))
                  (should success)
                  (setq hook-has-run t)))
      (ecukes-run-steps (list step-1 step-2) t)
      (should hook-has-run)))))

(ert-deftest ecukes-run-test/run-steps-after-last-step-hook ()
  (with-mock
   (stub ecukes-run-step)
   (let (hook-has-run
         (step-1 (make-ecukes-step :name "step-1"))
         (step-2 (make-ecukes-step :name "step-2")))
     (with-reporter-hooks
      (add-hook 'ecukes-reporter-after-last-step-hook
                (lambda (step success)
                  ;; must assert on name since status is updated
                  (should (equal (ecukes-step-name step) "step-2"))
                  (should success)
                  (setq hook-has-run t)))
      (ecukes-run-steps (list step-1 step-2) t)
      (should hook-has-run)))))

(ert-deftest ecukes-run-test/run-steps-after-step-success-hook ()
  (with-mock
   (stub ecukes-run-step => t)
   (let (hook-has-run (step (make-ecukes-step)))
     (with-reporter-hooks
      (add-hook 'ecukes-reporter-after-step-success-hook
                (lambda (_step)
                  (should (equal step _step))
                  (setq hook-has-run t)))
      (add-hook 'ecukes-reporter-after-step-failed-hook
                (lambda (step)
                  (should-not "run this hook")))
      (add-hook 'ecukes-reporter-after-step-skipped-hook
                (lambda (step)
                  (should-not "run this hook")))
      (ecukes-run-steps (list step) t)
      (should hook-has-run)))))

(ert-deftest ecukes-run-test/run-steps-after-step-failure-hook ()
  (with-mock
   (stub ecukes-run-step)
   (let (hook-has-run (step (make-ecukes-step)))
     (with-reporter-hooks
      (add-hook 'ecukes-reporter-after-step-success-hook
                (lambda (step)
                  (should-not "run this hook")))
      (add-hook 'ecukes-reporter-after-step-failed-hook
                (lambda (_step)
                  (should (equal step _step))
                  (setq hook-has-run t)))
      (add-hook 'ecukes-reporter-after-step-skipped-hook
                (lambda (step)
                  (should-not "run this hook")))
      (ecukes-run-steps (list step) t)
      (should hook-has-run)))))

(ert-deftest ecukes-run-test/run-steps-after-step-skipped-hook ()
  (with-mock
   (stub ecukes-run-step)
   (let (hook-has-run (step (make-ecukes-step)))
     (with-reporter-hooks
      (add-hook 'ecukes-reporter-after-step-success-hook
                (lambda (step)
                  (should-not "run this hook")))
      (add-hook 'ecukes-reporter-after-step-failed-hook
                (lambda (step)
                  (should-not "run this hook")))
      (add-hook 'ecukes-reporter-after-step-skipped-hook
                (lambda (_step)
                  (should (equal step _step))
                  (setq hook-has-run t)))
      (ecukes-run-steps (list step) nil)
      (should hook-has-run)))))

(ert-deftest ecukes-run-test/run-steps-after-step-hook ()
  (with-mock
   (stub ecukes-run-step)
   (let (hook-has-run step-1 step-2)
     (with-reporter-hooks
      (add-hook 'ecukes-reporter-after-step-hook
                (lambda (step success)
                  (setq step-1 t)
                  (setq step-2 t)
                  (setq hook-has-run t)))
      (ecukes-run-steps (list (make-ecukes-step) (make-ecukes-step)) t)
      (should hook-has-run)
      (should step-1)
      (should step-2)))))

(ert-deftest ecukes-run-test/run-steps-stats-pass ()
  (with-mock
   (stub ecukes-run-step => t)
   (mock (ecukes-stats-step-pass) :times 1)
   (with-reporter-hooks
    (ecukes-run-steps (list (make-ecukes-step)) t))))

(ert-deftest ecukes-run-test/run-steps-stats-fail ()
  (with-mock
   (stub ecukes-run-step)
   (mock (ecukes-stats-step-fail) :times 1)
   (with-reporter-hooks
    (ecukes-run-steps (list (make-ecukes-step)) t))))

(ert-deftest ecukes-run-test/run-steps-stats-skip ()
  (with-mock
   (stub ecukes-run-step => t)
   (mock (ecukes-stats-step-skip) :times 1)
   (with-reporter-hooks
    (ecukes-run-steps (list (make-ecukes-step)) nil))))

(ert-deftest ecukes-run-test/run-steps-set-status-success ()
  (with-mock
   (stub ecukes-run-step => t)
   (with-reporter-hooks
    (let ((step (make-ecukes-step)))
      (should (ecukes-run-steps (list step) t))
      (should (equal (ecukes-step-status step) 'success))))))

(ert-deftest ecukes-run-test/run-steps-set-status-failure ()
  (with-mock
   (stub ecukes-run-step => nil)
   (with-reporter-hooks
    (let ((step (make-ecukes-step)))
      (should-not (ecukes-run-steps (list step) t))
      (should (equal (ecukes-step-status step) 'failure))))))

(ert-deftest ecukes-run-test/run-steps-set-status-skipped ()
  (with-mock
   (stub ecukes-run-step)
   (with-reporter-hooks
    (let ((step (make-ecukes-step)))
      (should-not (ecukes-run-steps (list step) nil))
      (should (equal (ecukes-step-status step) 'skipped))))))


;;;; ecukes-run-step

(ert-deftest ecukes-run-test/run-step-fail-hook ()
  (with-mock
   (stub ecukes-steps-args)
   (mock (ecukes-hooks-run-fail))
   (stub ecukes-steps-find =>
         (make-ecukes-step-def
          :fn
          (lambda ()
            (error "*- boom -*"))))
   (should-not (ecukes-run-step (make-ecukes-step)))))

(ert-deftest ecukes-run-test/run-step-error-message ()
  (with-mock
   (stub ecukes-steps-args)
   (stub ecukes-steps-find =>
         (make-ecukes-step-def
          :fn
          (lambda ()
            (error "*- boom -*"))))
   (let ((step (make-ecukes-step)))
     (should-not (ecukes-run-step step))
     (should (equal (ecukes-step-err step) "*- boom -*")))))

(ert-deftest ecukes-run-test/run-step-successful ()
  (with-mock
   (stub ecukes-steps-args)
   (stub ecukes-steps-find =>
         (make-ecukes-step-def :fn 'ignore))
   (should (ecukes-run-step (make-ecukes-step)))))

(ert-deftest ecukes-run-test/run-step-failed ()
  (with-mock
   (stub ecukes-steps-args)
   (stub ecukes-steps-find =>
         (make-ecukes-step-def
          :fn
          (lambda ()
            (error "*- boom -*"))))
   (should-not (ecukes-run-step (make-ecukes-step)))))

(ert-deftest ecukes-run-test/run-step-keyboard-quit ()
  (with-mock
   (stub ecukes-steps-args)
   (stub ecukes-steps-find =>
         (make-ecukes-step-def :fn 'keyboard-quit))
   (should-not (ecukes-run-step (make-ecukes-step)))))

(ert-deftest ecukes-run-test/run-step-async-callbacked-no-arg ()
  (with-mock
   (stub ecukes-steps-args)
   (stub ecukes-steps-find =>
         (make-ecukes-step-def
          :fn
          (lambda (callback)
            (funcall callback))))
   (should (ecukes-run-step (make-ecukes-step)))))

(ert-deftest ecukes-run-test/run-step-async-callbacked-with-arg ()
  (with-mock
   (stub ecukes-steps-args)
   (stub ecukes-steps-find =>
         (make-ecukes-step-def
          :fn
          (lambda (arg callback)
            (should (eq arg 'arg))
            (funcall callback))))
   (should (ecukes-run-step (make-ecukes-step :arg 'arg)))))

(ert-deftest ecukes-run-test/run-step-async-callbacked-with-arg-and-args ()
  (with-mock
   (stub ecukes-steps-args => '(arg-1))
   (stub ecukes-steps-find =>
         (make-ecukes-step-def
          :fn
          (lambda (arg-1 arg-2 callback)
            (should (eq arg-1 'arg-1))
            (should (eq arg-2 'arg-2))
            (funcall callback))))
   (should (ecukes-run-step (make-ecukes-step :arg 'arg-2)))))

(ert-deftest ecukes-run-test/run-step-async-with-timeout ()
  (with-mock
   (stub ecukes-steps-args)
   (stub ecukes-steps-find =>
         (make-ecukes-step-def
          :fn
          (lambda (callback)
            ;; not callbacked
            )))
   (let ((step (make-ecukes-step)))
     (should-not
      (ecukes-run-step step))
     (let ((expected "Did not callback async step within 0.1 seconds")
           (actual (ecukes-step-err step)))
       (should (equal expected actual))))))
