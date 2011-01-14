(defmacro with-run (&rest body)
  `(let ((ecukes-run-buffers))
     (with-mock
      (stub ecukes-run-set-up)
      (stub ecukes-run-clean-up)
      ,@body)))

(ert-deftest run-features-should-run-setup-hooks ()
  "Should run setup hooks."
  (with-mock
   (mock (setup-mock) :times 1)
   (with-hooks
    (Setup (setup-mock))
    (ecukes-run-features ()))))

(ert-deftest run-features-should-run-teardown-hooks ()
  "Should run teardown hooks."
  (with-mock
   (mock (teardown-mock) :times 1)
   (with-hooks
    (Teardown (teardown-mock))
    (ecukes-run-features ()))))

(ert-deftest run-features-should-run-features ()
  "Should run features."
  (with-mock
   (mock (ecukes-run-feature) :times 2)
   (ecukes-run-features '(0 0))))

(ert-deftest run-feature-should-run-scenarios ()
  "Should run scenarios."
  (with-mock
   (stub ecukes-feature-scenarios => '(0 0))
   (mock (ecukes-run-scenario) :times 2)
   (ecukes-run-feature nil)))

(ert-deftest run-scenario-should-run-before-hooks ()
  "Should run before hooks."
  (with-run
   (mock (before-mock) :times 1)
   (stub ecukes-scenario-steps)
   (with-hooks
    (Before (before-mock))
    (ecukes-run-scenario nil))))

(ert-deftest run-scenario-should-run-after-hooks ()
  "Should run after hooks."
  (with-run
   (mock (after-mock) :times 1)
   (stub ecukes-scenario-steps)
   (with-hooks
    (After (after-mock))
    (ecukes-run-scenario nil))))

(ert-deftest run-scenario-should-clean-up ()
  "Should clean up."
  (with-mock
   (mock (ecukes-run-clean-up) :times 1)
   (stub ecukes-run-set-up)
   (stub ecukes-scenario-steps)
   (ecukes-run-scenario nil)))

(ert-deftest run-scenario-should-set-up ()
  "Should set up."
  (with-mock
   (mock (ecukes-run-set-up) :times 1)
   (stub ecukes-run-clean-up)
   (stub ecukes-scenario-steps)
   (ecukes-run-scenario nil)))

(ert-deftest run-scenario-should-run-steps ()
  "Should run steps."
  (with-run
   (mock (ecukes-run-step) :times 2)
   (stub ecukes-scenario-steps => '(0 0))
   (ecukes-run-scenario nil)))

(ert-deftest run-background-should-run-steps ()
  "Should run steps."
  (with-mock
   (mock (ecukes-run-step) :times 2)
   (stub ecukes-background-steps => '(0 0))
   (ecukes-run-background nil)))

(ert-deftest run-set-up-should-store-all-buffers ()
  "Should store all buffers."
  (with-mock
   (let ((buffers '("foo" "bar")))
     (stub buffer-list => buffers)
     (let ((ecukes-run-buffers))
       (ecukes-run-set-up)
       (should (equal ecukes-run-buffers buffers))))))

(ert-deftest run-clean-up-should-kill-all-buffers ()
  "Should kill all buffers."
  (with-mock
   (let ((ecukes-run-buffers '("foo" "bar")))
     (mock (kill-buffer) :times 2)
     (stub buffer-list => '("foo" "bar" "baz" "qux"))
     (ecukes-run-clean-up)
     (should (equal ecukes-run-buffers '("foo" "bar"))))))

(ert-deftest run-step-no-arguments ()
  "Should run step with no arguments."
  (with-mock
   (with-steps
    (mock (step-mock) :times 1)
    (Given "a known state" (lambda () (step-mock)))
    (ecukes-run-step
     (make-ecukes-step :name "a known state")))))

(ert-deftest run-step-single-argument ()
  "Should run step with single argument."
  (with-mock
   (with-steps
    (mock (step-mock "known") :times 1)
    (Given "a \\(.+\\) state" (lambda (state) (step-mock state)))
    (ecukes-run-step
     (make-ecukes-step :name "a known state")))))

(ert-deftest run-step-multiple-arguments ()
  "Should run step with multiple arguments."
  (with-mock
   (with-steps
    (mock (step-mock "known" "unknown") :times 1)
    (Given "state \\(.+\\) and \\(.+\\)"
           (lambda (state-1 state-2) (step-mock state-1 state-2)))
    (ecukes-run-step
     (make-ecukes-step :name "state known and unknown")))))

(ert-deftest run-step-should-return-true-on-success ()
  "Should return true on success."
  (with-mock
   (with-steps
    (stub ecukes-steps-find => (make-ecukes-step-def :fn 'ignore))
    (let ((step (make-ecukes-step :name "a success")))
      (should (ecukes-run-step step))))))

(ert-deftest run-step-should-return-false-on-failure ()
  "Should return false on failure."
  (with-mock
   (with-steps
    (let ((fn (lambda () (/ 1 0))))
      (stub ecukes-steps-find => (make-ecukes-step-def :fn fn))
      (let ((step (make-ecukes-step :name "a failure")))
        (should-not (ecukes-run-step step)))))))
