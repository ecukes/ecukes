;;; ecukes-run.el --- Run functions

(defun ecukes-run-features (features)
  "Runs all FEATURES."
  (dolist (feature features)
    (let ((background (ecukes-feature-background feature))
          (scenarios (ecukes-feature-scenarios feature)))

      ;; Print the intro, if any.
      (let ((intro (ecukes-feature-intro feature)))
        (if intro (ecukes-output-intro intro)))

      ;; Run and output background
      (if background
          (ecukes-output-background
           (ecukes-run-background
            background
            (lambda (step success)
              (ecukes-stats-update-steps success)
              (ecukes-output-step step success)))))

      (dolist (scenario scenarios)
        ;; Run before hooks
        (ecukes-hooks-run-before)

        ;; Turn on/off debug.
        (let ((debug (member "debug" (ecukes-scenario-tags scenario))))
          (ecukes-message-advice (not debug)))

        ;; Run background
        (if background (ecukes-run-background background))

        ;; Run and output scenario
        (let ((success-scenario t))
          (ecukes-output-scenario
           scenario
           (ecukes-run-scenario
            scenario
            (lambda (step success-step)
              (unless success-step (setq success-scenario nil))
              (ecukes-stats-update-steps success-step)
              (ecukes-output-step step success-step))))
          (ecukes-stats-update-scenarios success-scenario))

        ;; Clear the list of messages
        (setq ecukes-messages '())

        ;; Run after hooks
        (ecukes-hooks-run-after))))
  (ecukes-stats-print-summary))

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
