(require 'ecukes-project)

(ert-deftest project-path ()
  "Should return correct project path."
  (with-mock
   (stub file-directory-p => t)
   (let ((default-directory "/path/to/project"))
     (should (equal "/path/to/project" (ecukes-project-path))))))

(ert-deftest project-name ()
  "Should return correct project name."
  (with-project
   (should (equal "project" (ecukes-project-name)))))

(ert-deftest project-features-path ()
  "Should return correct project features path."
  (with-project
   (should (equal "/path/to/project/features" (ecukes-project-features-path)))))

(ert-deftest project-support-path ()
  "Should return correct project support path."
  (with-project
   (should (equal "/path/to/project/features/support" (ecukes-project-support-path)))))

(ert-deftest project-step-definition-path ()
  "Should return correct project step definition path."
  (with-project
   (should (equal "/path/to/project/features/step-definitions" (ecukes-project-step-definitions-path)))))
