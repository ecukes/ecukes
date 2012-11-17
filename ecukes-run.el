;;; ecukes-run.el --- Run features, scenarios, steps etc...

(require 'ecukes-parse)
(require 'ecukes-steps)
(require 'ecukes-print)
(require 'ecukes-stats)

;; TODO: !!!TEMPORARY!!!
(setq debug-on-error t)

(defun ecukes-run (feature-files)
  "Parse and run FEATURE-FILES if no steps are missing."
  (let* ((features
          (--map (ecukes-parse-feature it) feature-files))
         (steps
          (ecukes-feature-steps features))
         (steps-missing
          (ecukes-steps-missing-definition steps)))
    (if steps-missing
        (ecukes-print-missing-steps steps-missing)
      (ecukes-run-features features))))

(defun ecukes-run-features (features)
  "Run FEATURES."
  (ecukes-hooks-run-setup)
  (-each features 'ecukes-run-feature)
  (ecukes-hooks-run-teardown))

(defun ecukes-run-feature (feature)
  "Run FEATURE."
  (let ((intro (ecukes-feature-intro feature))
        (background (ecukes-feature-background feature))
        (scenarios (ecukes-feature-scenarios feature)))
    (ecukes-print-intro intro)
    (let ((background-success t))
      (when background
        (setq background-success (ecukes-run-background background)))
      (-each
       scenarios
       (lambda (scenario)
         (ecukes-run-scenario scenario background-success))))
    (ecukes-print-stats-summary)))

(defun ecukes-run-background (background)
  "Run BACKGROUND."
  (ecukes-print-background-header)
  (let* ((steps (ecukes-background-steps background))
         (success (ecukes-run-steps steps t)))
    (ecukes-print-newline)
    success))

(defun ecukes-run-scenario (scenario background-success)
  "Run SCENARIO."
  (ecukes-print-scenario-header scenario)
  (let* ((steps (ecukes-scenario-steps scenario))
         (success (ecukes-run-steps steps background-success)))
    (if success
        (ecukes-stats-scenario-pass)
      (ecukes-stats-scenario-fail)))
  (ecukes-print-newline))

(defun ecukes-run-steps (steps success)
  "Run and print STEPS and return `t' if all was successful, `nil' otherwise."
  (let ((status (if success 'success 'skipped)))
    (-each
     steps
     (lambda (step)
       (if success
           (progn
             (setq success (ecukes-run-step step))
             (unless success
               (setq status 'failure)))
         (setq status 'skipped))
       (cond ((eq status 'success)
              (ecukes-stats-step-pass))
             ((eq status 'failure)
              (ecukes-stats-step-fail))
             ((eq status 'skipped)
              (ecukes-stats-step-skip)))
       (ecukes-print-step step status)))
    success))

(defun ecukes-run-step (step)
  "Run STEP. Return `t' if success and `nil' otherwise."
  (let ((success))
    (condition-case err
        (progn
          (let* ((body (ecukes-step-body step))
                 (arg (ecukes-step-arg step))
                 (args (ecukes-step-args step))
                 (args (if arg (cons arg args) args))
                 (step-def (ecukes-steps-find body))
                 (fn (ecukes-step-def-fn step-def)))
            (apply fn args))
          (setq success t))
      (error
       (setf (ecukes-step-err step) (error-message-string err)))
      (quit))
    success))

(provide 'ecukes-run)

;;; ecukes-run.el ends here
