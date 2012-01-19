;;; ecukes-steps.el --- Functions to define and call step definitions


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
             (def (ecukes-steps-find query)))
        (if def
            (let ((fn (ecukes-step-def-fn def)))
              (apply fn args))
          (error "Definition '%s' have not been defined" query))))))

(defun ecukes-steps-find (name)
  "Find step definition bound to NAME."
  (let* ((query (ecukes-steps-query name))
         (definition
           (find-if
            (lambda (def)
              (string-match-p (car def) query))
            ecukes-steps-definitions))
         (regex (car definition))
         (fn (cdr definition))
         (index 1)
         (args))
    (when definition
      (string-match regex query)
      (while (match-string index query)
        (add-to-list 'args (match-string index query) t 'eq)
        (setq index (1+ index)))
      (make-ecukes-step-def :fn fn :args args))))

(defun ecukes-steps-undefined (steps)
  "Remove all items that are not defined in STEPS."
  (delete-dups
   (remove-if
    (lambda (step)
      (let ((name (ecukes-step-name step)))
        (ecukes-steps-find name)))
    steps)))

(defun ecukes-steps-query (name-or-step)
  "Returns a query based of NAME-OR-STEP."
  (let ((name
         (if (ecukes-step-p name-or-step)
             (ecukes-step-name step)
           name-or-step)))
    (if (string-match ecukes-parse-step-re name)
        (match-string 2 name)
      name)))


(provide 'ecukes-steps)

;;; ecukes-steps.el ends here
