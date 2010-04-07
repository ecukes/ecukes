(ert-deftest hooks-add-before-hook ()
  (Before "before")
  (should (equal "before" (funcall (car ecukes-hooks-before)))))

(ert-deftest hooks-add-after-hook ()
  (After "after")
  (should (equal "after" (funcall (car ecukes-hooks-after)))))

(ert-deftest hooks-add-setup-hook ()
  (Setup "setup")
  (should (equal "setup" (funcall (car ecukes-hooks-setup)))))

(ert-deftest hooks-add-teardown-hook ()
  (Teardown "teardown")
  (should (equal "teardown" (funcall (car ecukes-hooks-teardown)))))

(ert-deftest hooks-add-before-hooks ()
  (Before
   (setq before-one 1))
  (Before
   (setq before-two 2))
  (ecukes-hooks-run-before)
  (should (equal 1 before-one))
  (should (equal 2 before-two)))

(ert-deftest hooks-add-after-hooks ()
  (After
   (setq after-one 1))
  (After
   (setq after-two 2))
  (ecukes-hooks-run-after)
  (should (equal 1 after-one))
  (should (equal 2 after-two)))

(ert-deftest hooks-add-setup-hooks ()
  (Setup
   (setq setup-one 1))
  (Setup
   (setq setup-two 2))
  (ecukes-hooks-run-setup)
  (should (equal 1 setup-one))
  (should (equal 2 setup-two)))

(ert-deftest hooks-add-teardown-hooks ()
  (Teardown
   (setq teardown-one 1))
  (Teardown
   (setq teardown-two 2))
  (ecukes-hooks-run-teardown)
  (should (equal 1 teardown-one))
  (should (equal 2 teardown-two)))
