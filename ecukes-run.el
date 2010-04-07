;;; ecukes-run.el --- Functions for running stuff

(defun ecukes-run-features (features)
  "Runs all FEATURES."
  (let ((background-run))
    (dolist (feature features)
      (let ((background (ecukes-feature-background feature))
            (scenarios (ecukes-feature-scenarios feature))
            (intro (ecukes-feature-intro feature)))
        (if intro (ecukes-output-intro intro))
        (dolist (scenario scenarios)
          (ecukes-hooks-run-before)
          (when background
            (if background-run
                (ecukes-run-background background)
              (progn
                (ecukes-run-and-print-background background)
                (setq background-run t))))
          (ecukes-run-and-print-scenario scenario)
          (ecukes-message-clear)
          (ecukes-hooks-run-after))))
    (ecukes-stats-print-summary)))

(defun ecukes-run-scenario (scenario fn)
  "Runs all steps in SCENARIO, yielding each step and success flag."
  (dolist (step (ecukes-scenario-steps scenario))
    (let ((success (ecukes-run-step step)))
      (funcall fn step success))))

(defun ecukes-run-background (background &optional fn)
  "Runs all steps in BACKGROUND, yielding each step and success flag if FN."
  (dolist (step (ecukes-background-steps background))
    (let ((success (ecukes-run-step step)))
      (if fn (funcall fn step success)))))

(defun ecukes-run-and-print-background (background)
  "Prints a background header and then runs all BACKGROUND's steps."
  (ecukes-output-background
   (ecukes-run-background
    background
    (lambda (step success)
      (ecukes-stats-update-steps success)
      (ecukes-output-step step success)))))

(defun ecukes-run-and-print-scenario (scenario)
  "Prints SCENARIO's header and then runs all SCENARIO's steps."
  (let ((success-scenario t))
    (ecukes-output-scenario
     scenario
     (ecukes-run-scenario
      scenario
      (lambda (step success-step)
        (unless success-step (setq success-scenario nil))
        (ecukes-stats-update-steps success-step)
        (ecukes-output-step step success-step))))
    (ecukes-stats-update-scenarios success-scenario)))

(defun ecukes-run-step (step)
  "Runs STEP.
If running step was not a success, the err field in the step is
updated with the error message. This function returns t if the step
was successful, nil otherwise."
  (condition-case err
      (progn
        (let* ((arg (ecukes-step-arg step))
               (definition (ecukes-steps-find-definition step))
               (fn (ecukes-step-def-fn definition))
               (args (ecukes-step-def-args definition)))
          (if arg (add-to-list 'args arg t))
          (apply fn args)))
    (error
     (setf (ecukes-step-err step) (error-message-string err))))
  (not (ecukes-step-err step)))


(provide 'ecukes-run)

;;; ecukes-run.el ends here
