(require 'ansi)
(require 'ecukes-setup)

(ert-deftest setup-setup ()
  "Should validate and setup."
  (with-mock
   (mock (ecukes-setup-help) :times 1)
   (mock (ecukes-setup-features-dir-exist) :times 1)
   (mock (ecukes-setup-load) :times 1)
   (ecukes-setup)))

(ert-deftest setup-help-short-flag ()
  "Should show usage information when argv contains '-h'"
  (let ((argv '("-h")))
    (with-mock
     (mock (usage) :times 1)
     (ecukes-setup-help))))

(ert-deftest setup-help-long-flag ()
  "Should show usage information when argv contains '--help'"
  (let ((argv '("--help")))
    (with-mock
     (mock (usage) :times 1)
     (ecukes-setup-help))))

(ert-deftest setup-features-directory-exist-when-no-features-dir ()
  "Should print message when no features dir exist."
  (with-mock
   (stub file-directory-p => nil)
   (mock (usage) :times 1)
   (mock (ansi-red "Missing `features` directory.") :times 1)
   (ecukes-setup-features-dir-exist)))

(ert-deftest setup-load ()
  "Should load support and step definitions"
  (with-mock
   (mock (ecukes-setup-load-support) :times 1)
   (mock (ecukes-setup-load-step-definitions) :times 1)
   (ecukes-setup-load)))

(ert-deftest setup-load-support ()
  "Should load support first and only once, then the rest."
  (with-mock
   (stub expand-file-name => "/path/to/project/features/support/env.el")
   (stub
    directory-files =>
    '("/path/to/project/features/support/env.el"
      "/path/to/project/features/support/foo.el"
      "/path/to/project/features/support/bar.el"))
   (mock (load) :times 3)
   (ecukes-setup-load-support)))

(ert-deftest setup-load-step-definitions ()
  "Should load all step definitions."
  (with-mock
   (stub
    directory-files =>
    '("/path/to/project/features/step-definitions/project-steps.el"
      "/path/to/project/features/step-definitions/misc-steps.el"))
   (mock (load) :times 2)
   (ecukes-setup-load-step-definitions)))
