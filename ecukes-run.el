;;; ecukes-run.el --- Run features, scenarios, steps etc...

(require 'dash)

(require 'ecukes-parse)
(require 'ecukes-steps)
(require 'ecukes-stats)
(require 'ecukes-helpers)
(require 'ecukes-hooks)
(require 'ecukes-reporter)

(eval-when-compile
  (defvar ecukes-include-tags)
  (defvar ecukes-exclude-tags)
  (defvar ecukes-async-timeout))

(defun ecukes-run (feature-files)
  "Parse and run FEATURE-FILES if no steps are missing."
  (let* ((features (-map 'ecukes-parse-feature feature-files))
         (steps-without-definition
          (ecukes-steps-without-definition (ecukes-feature-steps features))))
    (cond (steps-without-definition
           (run-hook-with-args 'ecukes-reporter-steps-without-definition-hook steps-without-definition))
          (:else
           (run-hooks 'ecukes-reporter-start-hook)
           (ecukes-hooks-run-setup)
           (ecukes-run-features features)
           (ecukes-hooks-run-teardown)
           (run-hook-with-args
            'ecukes-reporter-end-hook
            `((scenarios        . ,ecukes-stats-scenarios)
              (scenarios-passed . ,ecukes-stats-scenarios-passed)
              (scenarios-failed . ,ecukes-stats-scenarios-failed)
              (steps            . ,ecukes-stats-steps)
              (steps-passed     . ,ecukes-stats-steps-passed)
              (steps-failed     . ,ecukes-stats-steps-failed)
              (steps-skipped    . ,ecukes-stats-steps-skipped)))))))

(defun ecukes-run-features (features)
  "Run FEATURES."
  (-each
   features
   (lambda (feature)
     (let ((first (eq (-first-item features) feature))
           (last (eq (-last-item features) feature)))
       (when first
         (run-hook-with-args 'ecukes-reporter-before-first-feature-hook feature))
       (when last
         (run-hook-with-args 'ecukes-reporter-before-last-feature-hook feature))
       (run-hook-with-args 'ecukes-reporter-before-feature-hook feature)
       (ecukes-run-feature feature)
       (when first
         (run-hook-with-args 'ecukes-reporter-after-first-feature-hook feature))
       (when last
         (run-hook-with-args 'ecukes-reporter-after-last-feature-hook feature))
       (run-hook-with-args 'ecukes-reporter-after-feature-hook feature)))))

(defun ecukes-run-feature (feature)
  "Run FEATURE."
  (let ((background (ecukes-feature-background feature))
        (scenarios
         (-select
          (lambda (scenario)
            (let ((tags (ecukes-scenario-tags scenario)))
              (and (or (not ecukes-include-tags)
                       (-intersection ecukes-include-tags tags))
                   (not (-intersection ecukes-exclude-tags tags)))))
          (ecukes-feature-scenarios feature))))
    (let ((background-success t)
          (background-should-run (not background)))
      (when background
        (ecukes-hooks-run-before)
        (run-hooks 'ecukes-reporter-before-background-hook)
        (setq background-success (ecukes-run-background background))
        (run-hooks 'ecukes-reporter-after-background-hook))
      (-each
       scenarios
       (lambda (scenario)
         (let ((first (equal (-first-item scenarios) scenario))
               (last (equal (-last-item scenarios) scenario)))
           (when background-should-run (ecukes-hooks-run-before))
           (when (and background background-success background-should-run)
             (ecukes-run-background-steps background))
           (when first
             (run-hook-with-args 'ecukes-reporter-before-first-scenario-hook scenario))
           (when last
             (run-hook-with-args 'ecukes-reporter-before-last-scenario-hook scenario))
           (run-hook-with-args 'ecukes-reporter-before-scenario-hook scenario)
           (ecukes-run-scenario scenario background-success)
           (when first
             (run-hook-with-args 'ecukes-reporter-after-first-scenario-hook scenario))
           (when last
             (run-hook-with-args 'ecukes-reporter-after-last-scenario-hook scenario))
           (run-hook-with-args 'ecukes-reporter-after-scenario-hook scenario)
           (ecukes-hooks-run-after)
           (setq background-should-run t)))))))

(defun ecukes-run-background-steps (background)
  "Run BACKGROUND steps."
  (-each (ecukes-background-steps background) 'ecukes-run-step))

(defun ecukes-run-background (background)
  "Run BACKGROUND."
  (ecukes-run-steps (ecukes-background-steps background) t))

(defun ecukes-run-scenario (scenario background-success)
  "Run SCENARIO."
  (let* ((steps (ecukes-scenario-steps scenario))
         (success (ecukes-run-steps steps background-success)))
    (cond (success
           (ecukes-stats-scenario-pass)
           (run-hook-with-args 'ecukes-reporter-scenario-passed-hook scenario))
          (:else
           (ecukes-stats-scenario-fail)
           (run-hook-with-args 'ecukes-reporter-scenario-failed-hook scenario)))))

(defun ecukes-run-steps (steps success)
  "Run STEPS and return true if all steps were successful, false otherwise."
  (let ((status (if success 'success 'skipped)))
    (-each
     steps
     (lambda (step)
       (let ((first (equal (-first-item steps) step))
             (last (equal (-last-item steps) step)))
         (when first
           (run-hook-with-args 'ecukes-reporter-before-first-step-hook step status))
         (when last
           (run-hook-with-args 'ecukes-reporter-before-last-step-hook step status))
         (run-hook-with-args 'ecukes-reporter-before-step-hook step status)
         (if success
             (progn
               (setq success (ecukes-run-step step))
               (unless success
                 (setq status 'failure)))
           (setq status 'skipped))
         (setf (ecukes-step-status step) status)
         (when first
           (run-hook-with-args 'ecukes-reporter-after-first-step-hook step status))
         (when last
           (run-hook-with-args 'ecukes-reporter-after-last-step-hook step status))
         (cond ((eq status 'success)
                (ecukes-stats-step-pass)
                (run-hook-with-args 'ecukes-reporter-after-step-success-hook step))
               ((eq status 'failure)
                (ecukes-stats-step-fail)
                (run-hook-with-args 'ecukes-reporter-after-step-failed-hook step))
               ((eq status 'skipped)
                (ecukes-stats-step-skip)
                (run-hook-with-args 'ecukes-reporter-after-step-skipped-hook step)))
         (run-hook-with-args 'ecukes-reporter-after-step-hook step status))))
    success))

(defun ecukes-run-step (step)
  "Run STEP and return true if successful, false otherwise."
  (let (success)
    (condition-case err
        (let* ((body (ecukes-step-body step))
               (arg (ecukes-step-arg step))
               (args (ecukes-steps-args step))
               (args (if arg (append args (list arg)) args))
               (step-def (ecukes-steps-find body))
               (fn (ecukes-step-def-fn step-def))
               (fn-args-count
                (length
                 (if (byte-code-function-p fn)
                     (aref fn 0)
                   (if (listp fn)
                       (cadr fn))))))
          (if (and (not (symbolp fn)) (> fn-args-count (length args)))
              (progn
                (let ((wait t))
                  (add-to-list 'args (lambda (&rest args) (setq wait nil)) t)
                  (apply fn args)
                  (with-timeout
                      (ecukes-async-timeout
                       (error "Did not callback async step within %s seconds" ecukes-async-timeout))
                    (while wait
                      (accept-process-output nil 0.005)))))
            (apply fn args))
          (setq success t))
      (error
       (setf (ecukes-step-err step) (error-message-string err))
       (ecukes-hooks-run-fail))
      (quit)) ;; allow `keyboard-quit' in step definitions
    success))

(provide 'ecukes-run)

;;; ecukes-run.el ends here
