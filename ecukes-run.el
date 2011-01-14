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

(provide 'ecukes-run)

;;; ecukes-run.el ends here
