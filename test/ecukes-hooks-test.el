(ert-deftest hooks-before ()
  "Should run before hooks."
  (with-mock
   (mock (before-mock) :times 1)
   (with-hooks
    (Before
     (before-mock))
    (ecukes-hooks-run-before))))

(ert-deftest hooks-after ()
  "Should run after hooks."
  (with-mock
   (mock (after-mock) :times 1)
   (with-hooks
    (After
     (after-mock))
    (ecukes-hooks-run-after))))

(ert-deftest hooks-setup ()
  "Should run setup hooks."
  (with-mock
   (mock (setup-mock) :times 1)
   (with-hooks
    (Setup
     (setup-mock))
    (ecukes-hooks-run-setup))))

(ert-deftest hooks-teardown ()
  "Should run teardown hooks."
  (with-mock
   (mock (teardown-mock) :times 1)
   (with-hooks
    (Teardown
     (teardown-mock))
    (ecukes-hooks-run-teardown))))