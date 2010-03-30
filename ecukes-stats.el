;;; ecukes-stats.el --- Statistics about the tests

(defvar ecukes-stats-num-steps 0
  "Number of steps that has runned.")

(defvar ecukes-stats-num-steps-failed 0
  "Number of steps that failed to run.")

(defvar ecukes-stats-num-steps-passed 0
  "Number of steps that passed to run.")

(defvar ecukes-stats-num-scenarios 0
  "Number of scenarios that has runned.")

(defvar ecukes-stats-num-scenarios-failed 0
  "Number of scenarios that failed to run.")

(defvar ecukes-stats-num-scenarios-passed 0
  "Number of scenarios that passed to run.")


(defun ecukes-stats-update-steps (success)
  "Updates the steps count according to SUCCESS."
  (setq ecukes-stats-num-steps (1+ ecukes-stats-num-steps))
  (if success
      (setq ecukes-stats-num-steps-passed (1+ ecukes-stats-num-steps-passed))
    (setq ecukes-stats-num-steps-failed (1+ ecukes-stats-num-steps-failed))))

(defun ecukes-stats-update-scenarios (success)
  "Updates the scenarios count according to SUCCESS."
  (setq ecukes-stats-num-scenarios (1+ ecukes-stats-num-scenarios))
  (if success
      (setq ecukes-stats-num-scenarios-passed (1+ ecukes-stats-num-scenarios-passed))
    (setq ecukes-stats-num-scenarios-failed (1+ ecukes-stats-num-scenarios-failed))))

(defun ecukes-stats-print-summary ()
  "Prints a summary with the number of runned/passed/failed scenarios and steps."
  (ecukes-output-no-indent
   (ecukes-stats-scenarios-summary)
   (ecukes-stats-steps-summary)))

(defun ecukes-stats-scenarios-summary ()
  "Returns a summary with the number of runned/passed/failed scenarios."
  (concat
   (ecukes-color-white (number-to-string ecukes-stats-num-scenarios) " scenarios (")
   (ecukes-color-red (number-to-string ecukes-stats-num-scenarios-failed) " failed")
   (ecukes-color-white ", ")
   (ecukes-color-green (number-to-string ecukes-stats-num-scenarios-passed) " passed")
   (ecukes-color-white ")\n")))

(defun ecukes-stats-steps-summary ()
  "Returns a summary with the number of runned/passed/failed steps."
  (concat
   (ecukes-color-white (number-to-string ecukes-stats-num-steps) " steps (")
   (ecukes-color-red (number-to-string ecukes-stats-num-steps-failed) " failed")
   (ecukes-color-white ", ")
   (ecukes-color-green (number-to-string ecukes-stats-num-steps-passed) " passed")
   (ecukes-color-white ")\n")))

(provide 'ecukes-stats)

;;; ecukes-stats.el ends here
