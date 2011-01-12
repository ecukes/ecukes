(ert-deftest new-features-does-exist ()
  "Should exist when there is a features directory."
  (with-mock
   (stub file-directory-p => t)
   (should (ecukes-new-exists-p))))

(ert-deftest new-features-does-not-exist ()
  "Should not exist when there is no features directory."
  (with-mock
   (stub file-directory-p => nil)
   (should-not (ecukes-new-exists-p))))

(ert-deftest new-should-create-root ()
  "Should create features directory."
  (with-mock
   (mock (make-directory ecukes-new-features-path) :times 1)
   (ecukes-new-create-root)))

(ert-deftest new-should-create-step-definitions ()
  "Should create step definitions directory and step definition."
  (with-mock
   (mock (make-directory) :times 1)
   (mock (ecukes-template-write) :times 1)
   (ecukes-new-create-step-definitions)))

(ert-deftest new-should-create-support ()
  "Should create support directory with env file."
  (with-mock
   (mock (make-directory) :times 1)
   (mock (ecukes-template-write) :times 1)
   (ecukes-new-create-support)))

(ert-deftest new-should-create-feature ()
  "Should create feature file."
  (with-mock
   (mock (ecukes-template-write) :times 1)
   (ecukes-new-create-feature)))