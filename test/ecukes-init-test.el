(ert-deftest init-does-not-exist ()
  (track-output
    (with-mock
     (stub ecukes-init-good-to-go => t)
     (stub ecukes-init-create-root => t)
     (stub ecukes-init-create-step-definitions => t)
     (stub ecukes-init-create-support => t)
     (stub ecukes-init-create-feature => t)
     (quiet-message (ecukes-init))
     (should
      (member
       (ecukes-color-green "Successfully created Ecukes setup")
       message-output)))))

(ert-deftest init-already-exists ()
  (track-output
    (with-mock
     (stub ecukes-init-good-to-go => nil)
     (quiet-message (ecukes-init))
     (should
      (member
       (ecukes-color-red "Ecukes setup already exists")
       message-output)))))

(ert-deftest init-good-to-go ()
  (with-mock
   (stub file-directory-p => nil)
   (should (ecukes-init-good-to-go))))

(ert-deftest init-not-good-to-go ()
  (with-mock
   (stub file-directory-p => t)
   (should-not (ecukes-init-good-to-go))))

(ert-deftest init-set-project-path-ending-slash ()
  (with-mock
   (stub expand-file-name => "/path/to/project/")
   (ecukes-init-set-project-path)
   (should (equal "/path/to/project" ecukes-init-project-path))))

(ert-deftest init-set-project-path-no-ending-slash ()
  (with-mock
   (stub expand-file-name => "/path/to/project")
   (ecukes-init-set-project-path)
   (should (equal "/path/to/project" ecukes-init-project-path))))

(ert-deftest init-set-project-path-as-argument ()
  (let ((command-line-args-left '("/path/to/project/")))
    (ecukes-init-set-project-path)
    (should (equal "/path/to/project" ecukes-init-project-path))))

(ert-deftest init-set-project-path-default-directory ()
  (let ((command-line-args-left) (default-directory "/path/to/project/"))
    (ecukes-init-set-project-path)
    (should (equal "/path/to/project" ecukes-init-project-path))))

(ert-deftest init-set-features-path ()
  (let ((ecukes-init-project-path "/path/to/project"))
    (ecukes-init-set-features-path)
    (should (equal "/path/to/project/features" ecukes-init-features-path))))

(ert-deftest init-set-project-name ()
  (let ((ecukes-init-project-path "/path/to/project"))
    (ecukes-init-set-project-name)
    (should (equal "project" ecukes-init-project-name))))
