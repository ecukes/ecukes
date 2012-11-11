;;; ecukes-project.el --- Project helpers

(defun ecukes-project-path ()
  "Path to project."
  (directory-file-name (expand-file-name default-directory)))

(defun ecukes-project-name ()
  "Name of the project."
  (file-name-nondirectory (ecukes-project-path)))

(defun ecukes-project-features-path ()
  "Path to project features dir."
  (directory-file-name (expand-file-name "features" (ecukes-project-path))))

(provide 'ecukes-project)

;;; ecukes-project.el ends here
