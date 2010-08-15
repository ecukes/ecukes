;;; ecukes-init.el --- Create an Ecukes file structure skeleton


(defvar ecukes-init-project-path nil
  "Path to the project.")

(defvar ecukes-init-features-path nil
  "Path to the features directory.")

(defvar ecukes-init-project-name nil
  "Name of the project.")


(defun ecukes-init-handler (switch)
  "Handler for --init option."
  (ecukes-init))

(defun ecukes-init ()
  "Create an Ecukes file structure skeleton."
  (ecukes-init-set-project-path)
  (ecukes-init-set-features-path)
  (ecukes-init-set-project-name)
  (cond ((ecukes-init-good-to-go)
         (ecukes-init-create-root)
         (ecukes-init-create-step-definitions)
         (ecukes-init-create-support)
         (ecukes-init-create-feature)
         (ecukes-output-green "Successfully created Ecukes setup"))
        (t (ecukes-output-red "Ecukes setup already exists"))))

(defun ecukes-init-set-project-path ()
  "Set the project path."
  (let ((project-path (expand-file-name (or (car command-line-args-left) default-directory))))
    (setq ecukes-init-project-path (replace-regexp-in-string "/$" "" project-path))))

(defun ecukes-init-set-features-path ()
  "Set the project features path."
  (let ((features-path (expand-file-name "features" ecukes-init-project-path)))
    (setq ecukes-init-features-path features-path)))

(defun ecukes-init-set-project-name ()
  "Set the project name."
  (let ((project-name (file-name-nondirectory ecukes-init-project-path)))
    (setq ecukes-init-project-name project-name)))

(defun ecukes-init-good-to-go ()
  "Returns t if project can be created, nil otherwise."
  (not (file-directory-p ecukes-init-features-path)))

(defun ecukes-init-create-root ()
  "Creates the root directory."
  (make-directory ecukes-init-features-path))

(defun ecukes-init-create-step-definitions ()
  "Creates step-definitions directory with empty step definition file in it."
  (let ((step-definitions-path (expand-file-name "step-definitions" ecukes-init-features-path)))
    (make-directory step-definitions-path)
    (let ((step-definitions-file
           (expand-file-name (concat ecukes-init-project-name "-steps.el") step-definitions-path)))
      (with-temp-file step-definitions-file
        (insert (ecukes-template-get "step-definition"))))))

(defun ecukes-init-create-feature ()
  "Creates an empty feature file."
  (let ((feature-file
         (expand-file-name
          (concat ecukes-init-project-name ".feature") ecukes-init-features-path)))
    (with-temp-file feature-file
      (insert (ecukes-template-get "feature")))))

(defun ecukes-init-create-support ()
  "Creates support.el including basic setup."
  (let* ((support-dir-path
	  (expand-file-name "support" ecukes-init-features-path))
	 (support-file-path
	  (expand-file-name "support.el" support-dir-path)))
      (make-directory support-dir-path)
      (with-temp-file support-file-path
	(let ((replacements `(("PROJECT-NAME" . ,ecukes-init-project-name))))
	  (insert (ecukes-template-get "support" replacements))))))


(provide 'ecukes-init)

;;; ecukes-init.el ends here
