;;; ecukes-reporter-dot.el --- One dot per scenario reporter

(require 'ansi)

(require 'ecukes-reporter)

(add-hook 'ecukes-reporter-scenario-passed-hook
          (lambda (scenario)
            (ecukes-reporter-print (ansi-green "."))))

(add-hook 'ecukes-reporter-scenario-failed-hook
          (lambda (scenario)
            (ecukes-reporter-print (ansi-red "."))))

(add-hook 'ecukes-reporter-end-hook
          (lambda (stats)
            (ecukes-reporter-print-newline)
            (ecukes-reporter-print-failing-scenarios-summary)
            (ecukes-reporter-print-newline)
            (ecukes-reporter-print-summary stats)
            (ecukes-reporter-print-newline)))

(provide 'ecukes-reporter-dot)

;;; ecukes-reporter-dot.el ends here
