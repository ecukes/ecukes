;;; ecukes-term.el --- Cucumber for Emacs

(require 'f)

(defvar ecukes-path (f-dirname load-file-name)
  "Path to ecukes.")

(add-to-list 'load-path ecukes-path)

(require 'ecukes-run)
(require 'ecukes-stats)
(require 'ecukes-setup)

(ecukes-setup)

(let ((feature-files
       (progn
         (or argv (setq argv (list "features")))
         (if (f-dir? (car argv))
             (f-glob "\\.feature$" (ecukes-project-features-path))
           argv))))
  (ecukes-run feature-files))

(ecukes-quit
 (if (> ecukes-stats-steps-failed 0) 1 0))

;;; ecukes-term.el ends here
