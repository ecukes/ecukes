;;; ecukes-run.el --- Run functions

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
               (fn (car definition))
               (args (car (cdr definition))))
          (if arg (add-to-list 'args arg t))
          (apply fn args)))
    (error
     (setf (ecukes-step-err step) (error-message-string err))))
  (not (ecukes-step-err step)))

(provide 'ecukes-run)

;;; ecukes-run.el ends here
