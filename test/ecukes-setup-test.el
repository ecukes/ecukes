(require 'ansi)
(require 'ecukes-setup)

(ert-deftest setup-ecukes-quit-failure ()
  "Should quit with exit code 1 by default."
  (with-mock
   (stub getenv => nil)
   (mock (kill-emacs 1) :times 1)
   (ecukes-quit)))

(ert-deftest setup-ecukes-quit-success ()
  "Should quit with exit code 0 on success."
  (with-mock
   (stub getenv => nil)
   (mock (kill-emacs 0) :times 1)
   (ecukes-quit 0)))

(ert-deftest setup-ecukes-quit-graphical ()
  "Should write to file before quit when graphical."
  (with-mock
   (stub getenv => "/tmp/ecukes.XYZ")
   (mock (kill-emacs 1) :times 1)
   (mock (write-file "/tmp/ecukes.XYZ") :times 1)
   (ecukes-quit)))

(ert-deftest setup ()
  "Should validate and setup."
  (with-mock
   (mock (ecukes-setup-features-dir-exist) :times 1)
   (mock (ecukes-setup-load) :times 1)
   (mock (ecukes-setup-argv) :times 1)
   (ecukes-setup)))

(ert-deftest setup-argv-empty ()
  "Should leave as is when empty."
  (let ((argv))
    (ecukes-setup-argv)
    (should (equal argv nil))))

(ert-deftest setup-argv-non-options ()
  "Should leave as is when non options."
  (let* ((argv (list "features"))
         (orig argv))
    (ecukes-setup-argv)
    (should (equal argv orig)))
  (let* ((argv (list "features/a.feature" "features/b.feature"))
         (orig argv))
    (ecukes-setup-argv)
    (should (equal argv orig))))

(ert-deftest setup-argv-dbg ()
  "Should enable debug (and verbose)."
  (let ((debug-on-entry nil)
        (debug-on-error nil)
        (argv (list "--dbg" "features")))
    (ecukes-setup-argv)
    (should (equal argv (list "features")))
    (should (equal debug-on-entry t))
    (should (equal debug-on-error t))
    (should (equal ecukes-verbose t))))

(ert-deftest setup-argv-verbose ()
  "Should enable verbose."
  (let ((ecukes-verbose nil)
        (argv (list "features/a.feature" "--verbose")))
    (ecukes-setup-argv)
    (should (equal ecukes-verbose t))
    (should (equal argv (list "features/a.feature")))))

(ert-deftest setup-argv-tags-single ()
  "Should set tags when single."
  (let ((ecukes-tags nil)
        (argv (list "features/a.feature" "--tags" "@foo")))
    (ecukes-setup-argv)
    (should (equal ecukes-tags (list "foo")))
    (should (equal argv (list "features/a.feature")))))

(ert-deftest setup-argv-tags-multiple ()
  "Should set tags when multiple."
  (let ((ecukes-tags nil)
        (argv (list "features/a.feature" "--tags" "@foo,@bar")))
    (ecukes-setup-argv)
    (should (equal ecukes-tags (list "foo" "bar")))
    (should (equal argv (list "features/a.feature")))))

(ert-deftest setup-argv-tags-double-multiple ()
  "Should set tags when double multiple."
  (let ((ecukes-tags nil)
        (argv (list "features/a.feature" "--tags" "@foo,@bar" "--verbose" "--tags" "@baz")))
    (ecukes-setup-argv)
    (should (equal ecukes-tags (list "foo" "bar" "baz")))
    (should (equal argv (list "features/a.feature")))))

(ert-deftest setup-argv-win ()
  "Should run with win."
  (let ((argv (list "--win" "features/a.feature")))
    (ecukes-setup-argv)
    (should (equal argv (list "features/a.feature")))))

(ert-deftest setup-features-directory-exist-when-no-features-dir ()
  "Should print message when no features dir exist."
  (with-mock
   (stub file-directory-p => nil)
   (stub message)
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
