;;; ecukes-load.el --- Helpers for loading project Ecukes files

(require 'f)
(require 's)
(require 'dash)
(require 'ansi)

(require 'ecukes-project)
(require 'ecukes-core)

(defun ecukes-load ()
  "Load support and step definitions."
  (unless (f-dir? (ecukes-project-features-path))
    (let ((ecukes-message t))
      (message (ansi-red "Missing `features` directory."))
      (ecukes-quit 1)))
  (ecukes-load-support)
  (ecukes-load-step-definitions))

(defun ecukes-load-support ()
  "Load project support files."
  (let* ((env-file (f-expand "env" (ecukes-project-support-path)))
         (support-files
          (f-files (ecukes-project-support-path)
                   (lambda (file)
                     (not (f-same? env-file file))))))
    (load env-file nil t)
    (--each support-files
      (load it nil t))))

(defun ecukes-load-step-definitions ()
  "Load project step definition files."
  (let* ((step-definition-files
          (f-glob "*-steps.el" (ecukes-project-step-definitions-path)))
         (step-definition-files (-map 'f-no-ext step-definition-files)))
    (--each step-definition-files (load it nil t))))

(provide 'ecukes-load)

;;; ecukes-load.el ends here
