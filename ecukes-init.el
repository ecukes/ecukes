;;; ecukes-init.el --- Initialization for Ecukes

(defconst ecukes-init-root-regex "\\(^[[:alpha:]]:/$\\|^/[^\/]+:\\|^/$\\)"
  "Regular expression matching a file system root.")

(defun ecukes-init-feature-files (features)
  "Returns a list of feature files given user input.

FEATURES is a list where an item can be either a .feature file or a
directory. If a directory, all .feature files in that directory are included."
  (let ((feature-files '()))
    (dolist (feature features)
      (if (file-directory-p feature)
          (dolist (feature-file (directory-files feature t "\\.feature$"))
            (add-to-list 'feature-files feature-file))
        (if (file-exists-p feature)
            (add-to-list 'feature-files (expand-file-name feature)))))
    feature-files))

(defun ecukes-init-project-root (dir)
  "Returns project root."
  (if (file-regular-p dir)
      (ecukes-init-project-root (file-name-directory dir))
    (if (file-directory-p (expand-file-name "features" dir))
        dir
      (let ((new-dir (expand-file-name (file-name-as-directory "..") dir)))
        (unless (string-match-p ecukes-init-root-regex dir)
          (ecukes-init-project-root new-dir))))))

(provide 'ecukes-init)

;;; ecukes-init.el ends here
