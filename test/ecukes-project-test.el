(require 'ecukes-project)

(ert-deftest project-path ()
  "Should return correct project path."
  (let ((default-directory "/path/to/project/"))
    (should (equal "/path/to/project" (ecukes-project-path)))))

(ert-deftest project-name ()
  "Should return correct project name."
  (let ((default-directory "/path/to/project/"))
    (should (equal "project" (ecukes-project-name)))))

(ert-deftest project-features-path ()
  "Should return correct project features path name."
  (let ((default-directory "/path/to/project"))
    (should (equal "/path/to/project/features" (ecukes-project-features-path)))))
