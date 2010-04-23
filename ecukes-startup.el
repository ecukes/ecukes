;;; ecukes-startup.el --- TODO


(defconst ecukes-root-regex "\\(?:^[[:alpha:]]:/$\\|^/$\\)"
  "Regular expression matching a file system root.")


(defun ecukes-feature-files (arguments)
  "Given ecukes arguments, return a list of all feature files.

ARGUMENTS can be either a list of .feature files or a directory. If a
directory, all .feature files in that directory will be included."
  (when arguments
    (let ((feature-files))
      (dolist (argument arguments)
        (when (file-exists-p argument)
          (if (file-directory-p argument)
              (dolist (feature-file (directory-files argument t "\\.feature$"))
                (add-to-list 'feature-files (expand-file-name feature-file) t))
            (add-to-list 'feature-files (expand-file-name argument) t))))
      feature-files)))

(defun ecukes-load-project (arguments)
  "Loads all Ecukes files in project."
  (let* ((project-file (car arguments))
         (features-root (ecukes-features-root project-file)))
    (cond (features-root
           (load (expand-file-name "support.el" features-root))
           (dolist (step (directory-files (expand-file-name "step-definitions" features-root) t "-steps\\.el$"))
             (load step)))
          (t (ecukes-output-red "Could not find features root")))))

(defun ecukes-features-root (file)
  "Returns the project features directory from FILE in project."
  (let ((project-root (ecukes-project-root file)))
    (if project-root (expand-file-name "features" project-root))))

(defun ecukes-project-root (file)
  "Returns the project root directory from FILE in project."
  (if (file-regular-p file)
      (ecukes-project-root (file-name-directory dir))
    (if (file-directory-p (expand-file-name "features" file))
        file
      (let ((new-dir (expand-file-name (file-name-as-directory "..") file)))
        (unless (string-match-p ecukes-root-regex file)
          (ecukes-project-root new-dir))))))

(defun ecukes-options-p ()
  "Returns t if any options was sent to ecukes, nil otherwise."
  (let ((opts (mapcar (lambda (e) (car e)) command-switch-alist))
        (match) (arguments (copy-sequence argv)))
    (while (and (not match) arguments)
      (setq arg (car arguments))
      (if (member arg opts)
          (setq match t))
      (setq arguments (cdr arguments)))
    match))


(provide 'ecukes-startup)

;;; ecukes-startup.el ends here
