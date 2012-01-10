;;; ecukes-stats.el --- Statistics about the passed and failed scenarios and steps

(defvar ecukes-stats-steps 0
  "Number of steps that have be runned.")

(defvar ecukes-stats-steps-passed 0
  "Number of steps that have passed.")

(defvar ecukes-stats-steps-failed 0
  "Number of steps that have failed.")

(defvar ecukes-stats-steps-skipped 0
  "Number of steps that were skipped.")

(defvar ecukes-stats-scenarios 0
  "Number of scenarios that have been runned.")

(defvar ecukes-stats-scenarios-passed 0
  "Number of scenarios that have passed.")

(defvar ecukes-stats-scenarios-failed 0
  "Number of scenarios that have failed.")


(defmacro ecukes-stats-step (&rest body)
  `(progn
     (setq ecukes-stats-steps (1+ ecukes-stats-steps))
     ,@body))

(defmacro ecukes-stats-scenario (&rest body)
  `(progn
     (setq ecukes-stats-scenarios (1+ ecukes-stats-scenarios))
     ,@body))


(defun ecukes-stats-step-pass ()
  "Step passed."
  (ecukes-stats-step
   (setq ecukes-stats-steps-passed (1+ ecukes-stats-steps-passed))))

(defun ecukes-stats-step-fail ()
  "Step failed."
  (ecukes-stats-step
   (setq ecukes-stats-steps-failed (1+ ecukes-stats-steps-failed))))

(defun ecukes-stats-step-skip ()
  "Step skipped."
  (ecukes-stats-step
   (setq ecukes-stats-steps-skipped (1+ ecukes-stats-steps-skipped))))

(defun ecukes-stats-scenario-pass ()
  "Scenario passed."
  (ecukes-stats-scenario
   (setq ecukes-stats-scenarios-passed (1+ ecukes-stats-scenarios-passed))))

(defun ecukes-stats-scenario-fail ()
  "Scenario failed."
  (ecukes-stats-scenario
   (setq ecukes-stats-scenarios-failed (1+ ecukes-stats-scenarios-failed))))

(defun ecukes-stats-print-summary ()
  "Print scenario and step summary."
  (message
   "\n%s\n%s"
   (ecukes-stats-scenario-summary)
   (ecukes-stats-step-summary)))

(defun ecukes-stats-scenario-summary ()
  "Return scenario summary as a string."
  (let ((scenarios (number-to-string ecukes-stats-scenarios))
        (passed (number-to-string ecukes-stats-scenarios-passed))
        (failed (number-to-string ecukes-stats-scenarios-failed)))
    (with-ansi
     (let* ((ansi-failed
             (red "%s failed" failed))
            (ansi-passed
             (green "%s passed" passed))
            (ansi-scenarios
             (remove-if 'not (list ansi-failed ansi-passed))))
       (if (> ecukes-stats-scenarios 0)
           (format "%s scenarios (%s)" scenarios (mapconcat 'identity ansi-scenarios ", "))
         "0 scenarios")))))

(defun ecukes-stats-step-summary ()
  "Return step summary as a string."
  (let ((steps (number-to-string ecukes-stats-steps))
        (passed (number-to-string ecukes-stats-steps-passed))
        (failed (number-to-string ecukes-stats-steps-failed))
        (skipped (number-to-string ecukes-stats-steps-skipped)))
    (with-ansi
     (let* ((ansi-failed
             (red "%s failed" failed))
            (ansi-skipped
             (cyan "%s skipped" skipped))
            (ansi-passed
             (green "%s passed" passed))
            (ansi-steps
             (remove-if 'not (list ansi-failed ansi-skipped ansi-passed))))
       (if (> ecukes-stats-steps 0)
           (format "%s steps (%s)" steps (mapconcat 'identity ansi-steps ", "))
         "0 steps")))))


(provide 'ecukes-stats)

;;; ecukes-stats.el ends here
