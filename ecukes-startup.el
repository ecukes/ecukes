;;; ecukes-startup.el --- TODO

;; TODO: Rename init functions!
;; TODO: Test functions!

;; TODO: Need to group?
(defconst ecukes-init-root-regex "\\(?:^[[:alpha:]]:/$\\|^/$\\)"
  "Regular expression matching a file system root.")


(defun ecukes-init-feature-files (features)
  "Returns a list of feature files given user input.
FEATURES is a list where an item can be either a .feature file or a
directory. If a directory, all .feature files in that directory are included."
  (let ((feature-files))
    (dolist (feature features)
      (if (file-directory-p feature)
          (dolist (feature-file (directory-files feature t "\\.feature$"))
            (add-to-list 'feature-files feature-file))
        (if (file-exists-p feature)
            (add-to-list 'feature-files (expand-file-name feature)))))
    feature-files))

(defun ecukes-init-load-project (argv)
  "Loads all project Ecukes files."
  (let ((features-root (ecukes-init-features-root (car argv))))
    (cond (features-root
           (load (expand-file-name "support.el" features-root))
           (dolist (step (directory-files (expand-file-name "step-definitions" features-root) t "-steps\\.el$"))
             (load step)))
          (t (error "Could not find any features root")))))

(defun ecukes-init-features-root (dir)
  "Returns project features root directory."
  (let ((project-root (ecukes-init-project-root dir)))
    (if project-root (expand-file-name "features" project-root))))

(defun ecukes-init-project-root (dir)
  "Returns project root directory."
  (if (file-regular-p dir)
      (ecukes-init-project-root (file-name-directory dir))
    (if (file-directory-p (expand-file-name "features" dir))
        dir
      (let ((new-dir (expand-file-name (file-name-as-directory "..") dir)))
        (unless (string-match-p ecukes-init-root-regex dir)
          (ecukes-init-project-root new-dir))))))

(defun ecukes-options-p ()
  "TODO"
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
