;;; ecukes-startup.el --- Helper functions when starting up Ecukes


(defvar ecukes-startup-project-path
  (directory-file-name (expand-file-name default-directory))
  "Path to the project.")

(defvar ecukes-startup-project-name
  (file-name-nondirectory ecukes-startup-project-path)
  "Name of the project.")

(defvar ecukes-startup-features-path
  (expand-file-name "features" ecukes-startup-project-path)
  "Path to the project features directory.")

(defvar ecukes-startup-support-path
  (expand-file-name "support" ecukes-startup-features-path)
  "Path to the features support directory.")

(defvar ecukes-startup-step-definitions-path
  (expand-file-name "step-definitions" ecukes-startup-features-path)
  "Path to the features step definitions directory.")


(defun ecukes-startup-load ()
  "Load project Ecukes files."
  (ecukes-startup-load-support)
  (ecukes-startup-load-step-definitions))

(defun ecukes-startup-load-support ()
  "Load project support files."
  (load (expand-file-name "env.el" ecukes-startup-support-path) nil t)
  (let ((support-files (remove "env.el" (directory-files ecukes-startup-support-path t "\\.el$"))))
    (mapc
     (lambda (support-file)
       (load support-file nil t))
     support-files)))

(defun ecukes-startup-load-step-definitions ()
  "Load project step definition files."
  (let ((step-definition-files (directory-files ecukes-startup-step-definitions-path t "-steps\\.el$")))
    (mapc
     (lambda (step-definition-file)
       (load step-definition-file nil t))
     step-definition-files)))

(defun ecukes-startup-features (files)
  "Return a list of feature files to run."
  (if files
      (let ((file (car files)))
        (if (file-directory-p file)
            (directory-files file t "\\.feature$")
          files))
    (ecukes-startup-features (list ecukes-startup-features-path))))

(defun ecukes-startup-run-p ()
  "Should we run feature or was there an option."
  (let ((switches (mapcar 'car command-switch-alist)))
    (not
     (find-if
      (lambda (switch)
        (member switch argv))
      switches))))


(provide 'ecukes-startup)

;;; ecukes-startup.el ends here
