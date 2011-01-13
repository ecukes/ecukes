(ert-deftest startup-load-support ()
  "Should load support files."
  (with-mock
   (stub directory-files => '("env.el" "foo.el" "bar.el"))
   (mock (load) :times 3)
   (ecukes-startup-load-support)))

(ert-deftest startup-load-step-definitions ()
  "Should load step definition files."
  (with-mock
   (stub directory-files => '("foo-steps.el" "bar-steps.el"))
   (mock (load) :times 2)
   (ecukes-startup-load-step-definitions)))

(ert-deftest startup-features-no-argument ()
  "Should return all feature files in features directory."
  (with-mock
   (stub file-directory-p => t)
   (let ((feature-files '("foo.feature" "bar.feature")))
     (stub directory-files => feature-files)
     (should (equal (ecukes-startup-features ()) feature-files)))))

(ert-deftest startup-features-directory-as-argument ()
  "Should return all feature files in given directory."
  (with-mock
   (stub file-directory-p => t)
   (let ((feature-files '("foo.feature" "bar.feature")))
     (stub directory-files => feature-files)
     (should (equal (ecukes-startup-features '("features")) feature-files)))))

(ert-deftest startup-features-files-as-argument ()
  "Should return all given feature files."
  (with-mock
   (stub file-directory-p => nil)
   (let ((feature-files '("foo.feature" "bar.feature")))
     (stub directory-files => feature-files)
     (should (equal (ecukes-startup-features feature-files) feature-files)))))

(ert-deftest startup-run-dont-run-when-switch ()
  "Should not run when valid switch."
  (let ((command-switch-alist '(("--new" . ignore))) (argv '("--new")))
    (should-not (ecukes-startup-run-p))))

(ert-deftest startup-run-when-no-switch ()
  "Should not when no switch."
  (let ((command-switch-alist '(("--new" . ignore))))
    (should (ecukes-startup-run-p))))