;;; ecukes-run.el --- Run features, scenarios, steps etc...

;; TODO: Some of these functions might not be used anywhere

(defvar ecukes-run-buffers ()
  "List of buffers, which is a part of the init state.")


(defun ecukes-run-features (features)
  "Run FEATURES."
  (ecukes-hooks-run-setup)
  (mapc 'ecukes-run-feature features)
  (ecukes-hooks-run-teardown))

(defun ecukes-run-feature (feature)
  "Run FEATURE."
  (let ((scenarios (ecukes-feature-scenarios feature)))
    (mapc 'ecukes-run-scenario scenarios)))

(defun ecukes-run-scenario (scenario)
  "Run SCENARIO."
  (ecukes-run-set-up)
  (ecukes-hooks-run-before)
  (let ((steps (ecukes-scenario-steps scenario)))
    (mapc 'ecukes-run-step steps))
  (ecukes-hooks-run-after)
  (ecukes-run-clean-up))

(defun ecukes-run-background (background)
  "Run BACKGROUND."
  (let ((steps (ecukes-background-steps background)))
    (mapc 'ecukes-run-step steps)))

(defun ecukes-run-step (step)
  "Run STEP. Return t if success and nil otherwise."
  (let ((status))
    (condition-case err
        (progn
          (let* ((name (ecukes-step-name step))
                 (arg (ecukes-step-arg step))
                 (def (ecukes-steps-find name))
                 (fn (ecukes-step-def-fn def))
                 (args (ecukes-step-def-args def))
                 (args (if arg (cons arg args) args)))
            (apply fn args))
          (setq status t))
      (error
       (setf (ecukes-step-err step) (error-message-string err)))
      (quit))
    status))

(defun ecukes-run-set-up ()
  "Set up the state that was before anything has been runned."
  (setq ecukes-run-buffers (buffer-list)))

(defun ecukes-run-clean-up ()
  "Clean up to the set up state."
  (let ((buffers (set-difference (buffer-list) ecukes-run-buffers :test 'equal)))
    (mapc 'kill-buffer buffers)))

(defun run-step (step &optional print background)
  "Run STEP, including increasing counters."
  (if previous-step-success
      (cond
       ((ecukes-run-step step)
        (unless background (ecukes-stats-step-pass))
        (if print
            (ecukes-print-step-success step)))
       (t
        (unless background (ecukes-stats-step-fail))
        (setq step-has-failed t)
        (if print
            (ecukes-print-step-failure step))
        (setq previous-step-success nil)))
    (ecukes-stats-step-skip)
    (if print
        (ecukes-print-step-pending step))))

(defun ecukes-run-default ()

  (when (ecukes-startup-run-p)

  (let ((feature-files (ecukes-startup-features argv)))
    (cond (feature-files

           (ecukes-startup-load)

           (ecukes-hooks-run-setup)

           (dolist (feature-file feature-files)
             (let* ((feature (ecukes-parse-feature feature-file))
                    (background (ecukes-feature-background feature))
                    (scenarios (ecukes-feature-scenarios feature))
                    (steps
                     (apply
                      'append
                      (if background (ecukes-background-steps background))
                      (mapcar 'ecukes-scenario-steps scenarios)))
                    (undefined (ecukes-steps-undefined steps)))

               (setq step-has-failed nil)
               (setq previous-step-success t)
               (setq background-runned nil)

               (cond (undefined
                      (ecukes-print-undefined-steps undefined))
                     ((let ((intro (ecukes-feature-intro feature)))
                        (ecukes-print-intro intro)

                        (when background
                          (ecukes-print-background-header)
                          (dolist (step (ecukes-background-steps background))
                            (run-step step t))
                          (setq background-runned t))

                        (dolist (scenario scenarios)
                          (ecukes-hooks-run-before)

                          (when background
                            (dolist (step (ecukes-background-steps background))
                              (run-step step nil t)))

                          (ecukes-print-newline)
                          (ecukes-print-scenario-header scenario)
                          (dolist (step (ecukes-scenario-steps scenario))
                            (run-step step t))

                          (if step-has-failed
                              (ecukes-stats-scenario-fail)
                            (ecukes-stats-scenario-pass))

                          (setq previous-step-success t)
                          (setq step-has-failed nil)

                          (ecukes-hooks-run-after)))))))

           (ecukes-hooks-run-teardown)

           (ecukes-stats-print-summary))
          (t
           (ecukes-print-message
            (ansi-red "You did not provide any features to run")))))))

(provide 'ecukes-run)

;;; ecukes-run.el ends here
