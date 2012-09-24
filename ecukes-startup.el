;;; ecukes-startup.el --- Helper functions when starting up Ecukes

(defvar *ecukes-last-project-dir*
  "Ecukes will attempt to find a dir named 'features' to
  determine the root directory of the current project. Once it's
  found a root directory, it caches it here so that it's possible
  and convenient to re-run ecukes outside of the project
  directory structure if needed")

(defun ecukes-find-project-dir () 
  "Return path to closest 'features' directory or nil if no
'features' dir is found in any parent directories"
  (let* ((plv-project-file "features")
         (features-path (plv-find-project-file "." "")))
    (if features-path
        (setq *ecukes-last-project-dir* 
              (expand-file-name (concat features-path "/.."))))
      *ecukes-last-project-dir*))

(defun ecukes-startup-project-path ()
  "Path to the project."
  (let ((project-dir (ecukes-find-project-dir)))
    (if (null project-dir)
        (error "Unable to find 'features' directory. Are you sure
        you're inside a Ecukes Project Directory?")
      (directory-file-name (ecukes-find-project-dir)))))

(defun ecukes-startup-project-name ()
  "Name of the project."
  (file-name-nondirectory (ecukes-startup-project-path)))

(defun ecukes-startup-features-path ()
  "Path to the project features directory."
  (expand-file-name "features" (ecukes-startup-project-path)))

(defun ecukes-startup-support-path ()
  "Path to the features support directory."
  (expand-file-name "support" (ecukes-startup-features-path)))

(defun ecukes-startup-step-definitions-path ()
  "Path to the features step definitions directory."
  (expand-file-name "step-definitions" (ecukes-startup-features-path)))


(defun ecukes-startup-load ()
  "Load project Ecukes files."
  (ecukes-startup-load-support)
  (ecukes-startup-load-step-definitions))

(defun ecukes-startup-load-support ()
  "Load project support files."
  (load (expand-file-name "env.el" (ecukes-startup-support-path)) nil t)
  (let ((support-files (remove "env.el" (directory-files (ecukes-startup-support-path) t "\\.el$"))))
    (mapc
     (lambda (support-file)
       (load support-file nil t))
     support-files)))

(defun ecukes-startup-load-step-definitions ()
  "Load project step definition files."
  (let ((step-definition-files (directory-files (ecukes-startup-step-definitions-path) t "-steps\\.el$")))
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
    (ecukes-startup-features (list (ecukes-startup-features-path)))))

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
