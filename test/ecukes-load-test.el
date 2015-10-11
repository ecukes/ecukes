(require 'ansi)
(require 'ecukes-load)

(ert-deftest ecukes-load/load-with-features-dir ()
  (with-mock
   (stub f-dir? => t)
   (mock (ecukes-load-support) :times 1)
   (mock (ecukes-load-step-definitions) :times 1)
   (ecukes-load)))

(ert-deftest ecukes-load/load-without-features-dir ()
  (with-mock
   (stub f-dir? => nil)
   (mock (ecukes-load-support) :times 1)
   (mock (ecukes-load-step-definitions) :times 1)
   (mock (ansi-red "Missing `features` directory.") :times 1)
   (mock (ecukes-quit 1) :times 1)
   (ecukes-load)))

(ert-deftest ecukes-load/support ()
  (with-mock
   (with-project
    (stub expand-file-name => "/path/to/project/features/support/env.el")
    (stub
     f-files =>
     '("/path/to/project/features/support/env.el"
       "/path/to/project/features/support/foo.el"
       "/path/to/project/features/support/bar.el"))
    (mock (load * nil t) :times 4)
    (ecukes-load-support))))

(ert-deftest ecukes-load/load-step-definitions ()
  "Should load all step definitions."
  (with-mock
   (stub
    f-files =>
    '("/path/to/project/features/step-definitions/project-steps.el"
      "/path/to/project/features/step-definitions/misc-steps.el"))
   (mock (load * nil t) :times 2)
   (ecukes-load-step-definitions)))

(ert-deftest ecukes-load/skip-hidden-temporal-files ()
  "Should skip hidden and temporal files."
  (with-mock
   (stub
    f-files =>
    '("/path/to/project/features/support/.bar-steps.el"
      "/path/to/project/features/support/#misc-steps.el#"
      "/path/to/project/features/step-definitions/bar-steps.el"
      "/path/to/project/features/step-definitions/misc-steps.el"))
   (mock (load * nil t) :times 2)
   (ecukes-load-step-definitions)))

