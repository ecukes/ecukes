;;; ecukes-reporter-magnars.el --- Minimal version of spec, designed by @magnars

(require 's)
(require 'dash)

(require 'ecukes-reporter)

(defvar ecukes-reporter-in-background nil
  "True if currently in background, false otherwise.")

(add-hook 'ecukes-reporter-before-feature-hook
          (lambda (feature)
            (ecukes-reporter-print-feature-header feature)))

(add-hook 'ecukes-reporter-before-background-hook
          (lambda ()
            (setq ecukes-reporter-in-background t)
            (ecukes-reporter-print-background-header)))

(add-hook 'ecukes-reporter-after-background-hook
          (lambda ()
            (ecukes-reporter-print-newline)))

(add-hook 'ecukes-reporter-after-step-hook
          (lambda (step status)
            (when ecukes-reporter-in-background
              (ecukes-reporter-print-step step))))

(add-hook 'ecukes-reporter-before-scenario-hook
          (lambda (scenario)
            (setq ecukes-reporter-in-background nil)
            (ecukes-reporter-print-scenario-header scenario)))

(add-hook 'ecukes-reporter-scenario-failed-hook
          (lambda (scenario)
            (-each (ecukes-scenario-steps scenario) 'ecukes-reporter-print-step)
            (ecukes-reporter-print-newline)))

(add-hook 'ecukes-reporter-after-last-scenario-hook
          (lambda (scenario)
            (ecukes-reporter-print-newline)))

(add-hook 'ecukes-reporter-end-hook
          (lambda (stats)
            (ecukes-reporter-print-summary stats)))

(provide 'ecukes-reporter-magnars)

;;; ecukes-reporter-magnars.el ends here
