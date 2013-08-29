(require 'ansi)

(require 'ecukes-reporter)

(defvar ecukes-reporter-is-first-scenario t
  "Is the first scenario currently running or not.")

(defvar ecukes-reporter-dot-string "\u22C5"
  "Dots shown after plane.")

(defvar ecukes-reporter-plane-string "\u2708"
  "The plane.")

(add-hook 'ecukes-reporter-start-hook
          (lambda (stats)
            (let ((scenarios (cdr (assoc 'scenarios stats))))
              (unless (zerop scenarios)
                (ecukes-reporter-print (ansi-apply 90 (s-repeat scenarios "-")))
                (ecukes-reporter-print-newline)
                (ecukes-reporter-print-newline)
                (ecukes-reporter-print (ansi-apply 90 (s-repeat scenarios "-")))
                (ecukes-reporter-print (ansi-backward scenarios))
                (ecukes-reporter-print (ansi-up))))))

(add-hook 'ecukes-reporter-pending-scenario-hook
          (lambda (scenario)
            (ecukes-reporter-print (ansi-backward))
            (ecukes-reporter-print (ansi-cyan ecukes-reporter-dot-string))
            (ecukes-reporter-print (ansi-forward))))

(add-hook 'ecukes-reporter-scenario-passed-hook
          (lambda (scenario)
            (ecukes-reporter-print (ansi-green ecukes-reporter-dot-string))))

(add-hook 'ecukes-reporter-scenario-failed-hook
          (lambda (scenario)
            (ecukes-reporter-print (ansi-red ecukes-reporter-dot-string))))

(add-hook 'ecukes-reporter-after-scenario-hook
          (lambda (scenario)
            (ecukes-reporter-print ecukes-reporter-plane-string)))

(add-hook 'ecukes-reporter-before-scenario-hook
          (lambda (scenario)
            (unless ecukes-reporter-is-first-scenario
              (ecukes-reporter-print (ansi-backward)))
            (setq ecukes-reporter-is-first-scenario nil)))

(add-hook 'ecukes-reporter-end-hook
          (lambda (stats)
            (ecukes-reporter-print-newline)
            (ecukes-reporter-print-newline)
            (ecukes-reporter-print-failing-scenarios-summary)
            (ecukes-reporter-print-newline)
            (ecukes-reporter-print-summary stats)))

(provide 'ecukes-reporter-landing)
