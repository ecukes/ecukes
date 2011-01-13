;;; ecukes-steps.el --- Function to define and call step definitions


(defvar ecukes-steps-definitions
  (make-hash-table :test 'equal)
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
        (puthash regex arg ecukes-steps-definitions)
      (let* ((query (apply 'format regex args))
             (fn (ecukes-steps-find query)))
        (if fn (apply fn args))))))

(defun ecukes-steps-find (query)
  "Find step definition bound to query. Return its function."
  (let ((result))
    (maphash
     (lambda (regex fn)
       (if (string-match-p regex query)
           (setq result fn)))
     ecukes-steps-definitions)
    result))

(defun ecukes-steps-missing-definition (steps)
  "Remove all items that are defined in STEPS."
  (let ((missing))
    (mapc
     (lambda (step)
       (unless (ecukes-steps-find (ecukes-step-name step))
         (add-to-list 'missing step)))
     steps)
    missing))


(provide 'ecukes-steps)

;;; ecukes-steps.el ends here
