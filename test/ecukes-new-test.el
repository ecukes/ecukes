(require 'ansi)
(require 'ecukes-new)

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

(ert-deftest new-should-print-error-message-when-already-exists ()
  "Should print error message when already exists."
  (with-mock
   (stub ecukes-new-exists-p => t)
   (mock (error (ansi-red "Ecukes already exists for this project") :times 1))
   (ecukes-new)))

(ert-deftest new-should-create-setup-when-not-exists ()
  "Should create setup when not exist."
  (with-mock
   (stub ecukes-new-exists-p => nil)
   (stub ecukes-new-message)
   (mock (ecukes-new-create-root) :times 1)
   (mock (ecukes-new-create-step-definitions) :times 1)
   (mock (ecukes-new-create-support) :times 1)
   (mock (ecukes-new-create-feature) :times 1)
   (ecukes-new)))

(ert-deftest new-should-create-root ()
  "Should create features directory."
  (with-mock
   (mock (make-directory) :times 1)
   (mock (ecukes-new-message) :times 1)
   (ecukes-new-create-root)))

(ert-deftest new-should-create-step-definitions ()
  "Should create step definitions directory and step definition."
  (with-mock
   (mock (make-directory) :times 1)
   (mock (ecukes-template-write) :times 1)
   (mock (ecukes-new-message) :times 2)
   (ecukes-new-create-step-definitions)))

(ert-deftest new-should-create-support ()
  "Should create support directory with env file."
  (with-mock
   (mock (make-directory) :times 1)
   (mock (ecukes-template-write) :times 1)
   (mock (ecukes-new-message) :times 2)
   (ecukes-new-create-support)))

(ert-deftest new-should-create-feature ()
  "Should create feature file."
  (with-mock
   (mock (ecukes-template-write) :times 1)
   (mock (ecukes-new-message) :times 1)
   (ecukes-new-create-feature)))
