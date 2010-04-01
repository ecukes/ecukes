(ert-deftest hooks-add-before-hook ()
  (Before "before")
  (should (equal "before" (funcall (car ecukes-before-hooks)))))

(ert-deftest hooks-add-after-hook ()
  (After "after")
  (should (equal "after" (funcall (car ecukes-after-hooks)))))

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
