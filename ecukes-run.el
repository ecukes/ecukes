;;; ecukes-run.el --- Helpers for running stuff

(add-to-list 'load-path (file-name-directory load-file-name))
(require 'cl)
(require 'ecukes-def)
(require 'ecukes-dump)
(require 'ecukes-output)


(defun ecukes-run-and-print-background ()
  "Runs and prints all background steps."
  (let ((ecukes-output-offset (ecukes-dump-read-offset)))
    (message (concat "\n" (ecukes-output-white "Background:")))
    (let* ((background (ecukes-dump-read-background))
           (steps (ecukes-background-steps background)))
      (setq ecukes-output-offset (+ ecukes-output-offset 2))
      (dolist (step steps)
        (ecukes-run-and-print-step step)))))

(defun ecukes-run-step (step)
  ""
  ;; TODO: Run step
  )

(defun ecukes-run-and-print-step (step)
  "Runs and prints step. If running step results in an error, the
error message is printed."
  (condition-case err
      (progn

        (ecukes-run-step step)
        ;; TODO: Print step
        )

    (error

     ;; TODO: Print (error-message-string err)
     ;; TODO: Print step

     )))

(defun ecukes-run-background ()
  "Runs all background steps."
  (let* ((background (ecukes-dump-read-background))
         (steps (ecukes-background-steps background)))
    (dolist (step steps)
      (ecukes-run-step step))))

(defun ecukes-run-and-print-scenario ()
  ""
  (let ((scenario (ecukes-dump-read-scenario))
        (ecukes-output-offset (ecukes-dump-read-offset)))
    (message (ecukes-output-white (concat "Scenario: " (ecukes-scenario-name scenario))))
    (setq ecukes-output-offset (+ ecukes-output-offset 2))
    (dolist (step (ecukes-scenario-steps scenario))
      (ecukes-run-and-print-step step))))

(provide 'ecukes-run)

;;; ecukes-run.el ends here
