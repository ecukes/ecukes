;;; ecukes-init.el --- Initialization for Ecukes

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

(provide 'ecukes-init)

;;; ecukes-init.el ends here
