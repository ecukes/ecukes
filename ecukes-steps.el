;;; ecukes-steps.el --- Function to define and call step definitions


(defvar ecukes-steps-definitions ()
  "All defined step definitions.")


(defalias 'Given 'ecukes-steps-step
  "Put the system in a known state.")

(defalias 'When 'ecukes-steps-step
  "Describe the key action.")

(defalias 'Then 'ecukes-steps-step
  "Observe outcomes.")

(defalias 'And 'ecukes-steps-step
  "Make Given/When/Then read more fluently.")

(defalias 'But 'ecukes-steps-step
  "Make Given/When/Then read more fluently.")

(defun ecukes-steps-step (regex &rest args)
  "Define or call step."
  (let ((arg (car args)))
    (if (functionp arg)
        (add-to-list 'ecukes-steps-definitions `(,regex . ,arg))
      (let* ((query (apply 'format regex args))
             (fn (ecukes-steps-find query)))
        (if fn (apply fn args))))))

(defun ecukes-steps-find (query)
  "Find step definition bound to query. Return its function."
  (cdr
   (find-if
    (lambda (regex)
      (string-match-p (car regex) query))
    ecukes-steps-definitions)))

(defun ecukes-steps-undefined (steps)
  "Remove all items that are not defined in STEPS."
  (delete-dups
   (remove-if
    (lambda (step)
      (let ((name (ecukes-step-name step)))
        (ecukes-steps-find name)))
    steps)))


(provide 'ecukes-steps)

;;; ecukes-steps.el ends here
