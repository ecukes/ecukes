;;; ecukes-new.el --- Setup up Ecukes for a project


(defvar ecukes-new-project-path
  (directory-file-name (expand-file-name default-directory))
  "Path to the project.")

(defvar ecukes-new-project-name
  (file-name-nondirectory ecukes-new-project-path)
  "Name of the project.")

(defvar ecukes-new-features-path
  (expand-file-name "features" ecukes-new-project-path)
  "Path to the project features directory.")


(defun ecukes-new-handler (switch)
  "Handler for --new option."
  (ecukes-new))

(defun ecukes-new ()
  "Create new Ecukes setup."
  (if (ecukes-new-exists-p)
      (message
       (ansi-red "Ecukes already exists for this project"))
    (progn
      (ecukes-new-create-root)
      (ecukes-new-create-step-definitions)
      (ecukes-new-create-support)
      (ecukes-new-create-feature)
      (message
       (ansi-green "Ecukes setup successfull")))))

(defun ecukes-new-exists-p ()
  "Check if there already exist an Ecukes setup."
  (file-directory-p ecukes-new-features-path))

(defun ecukes-new-create-root ()
  "Create features directory."
  (make-directory ecukes-new-features-path))

(defun ecukes-new-create-step-definitions ()
  "Create step-definitions directory and step definition."
  (let ((step-definitions-path (expand-file-name "step-definitions" ecukes-new-features-path)))
    (make-directory step-definitions-path)
    (let ((step-definition
           (expand-file-name (format "%s-steps.el" ecukes-new-project-name) step-definitions-path)))
      (ecukes-template-write step-definition 'step-definition))))

(defun ecukes-new-create-support ()
  "Create support directory."
  (let ((support (expand-file-name "support" ecukes-new-features-path)))
    (make-directory support)
    (let ((env (expand-file-name "env.el" support)))
      (ecukes-template-write env 'env `(("project-name" . ,ecukes-new-project-name))))))

(defun ecukes-new-create-feature ()
  "Create feature file."
  (let ((feature
         (expand-file-name
          (format "%s.feature" ecukes-new-project-name) ecukes-new-features-path)))
    (ecukes-template-write feature 'feature)))


(provide 'ecukes-new)

;;; ecukes-new.el ends here
