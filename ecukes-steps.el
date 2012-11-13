;;; ecukes-steps.el --- Functions to define and call step definitions

(require 'ecukes-parse)

(defvar ecukes-steps-definitions nil
  "All defined step definitions.")


(defalias 'Given 'ecukes-steps-define-or-call-step
  "Put the system in a known state.")

(defalias 'When 'ecukes-steps-define-or-call-step
  "Describe the key action.")

(defalias 'Then 'ecukes-steps-define-or-call-step
  "Observe outcomes.")

(defalias 'And 'ecukes-steps-define-or-call-step
  "Make Given/When/Then read more fluently.")

(defalias 'But 'ecukes-steps-define-or-call-step
  "Make Given/When/Then read more fluently.")

(defun ecukes-steps-define-or-call-step (name &rest args)
  "Define or call step."
  (let ((arg (car args)))
    (if (functionp arg)
        (ecukes-steps-define name arg)
      (ecukes-steps-call name args))))

(defun ecukes-steps-define (regex fn)
  "Define step."
  (unless (--any? (equal regex it) ecukes-steps-definitions)
    (add-to-list
     'ecukes-steps-definitions
     (make-ecukes-step-def :regex regex :fn fn))))

(defun ecukes-steps-call (name args)
  "Call step"
  (let* ((query (apply 'format (cons name args)))
         (step-def (ecukes-steps-find query)))
    (if step-def
        (apply (ecukes-step-def-fn step-def) args)
      (error (ansi-red "Step not defined: `%s`" query)))))

(defun ecukes-steps-missing-definition (steps)
  "Return from STEPS those who have not been defined."
  (let ((-compare-fn
         (lambda (step-1 step-2)
           (equal
            (ecukes-step-body step-1)
            (ecukes-step-body step-2)))))
    (-distinct
     (-filter
      (lambda (step)
        (not (ecukes-steps-find (ecukes-step-body step))))
      steps))))

(defun ecukes-steps-find (name)
  "Find step by name."
  (-first
   (lambda (step-def)
     (s-matches? (ecukes-step-def-regex step-def) name))
   ecukes-steps-definitions))

(provide 'ecukes-steps)

;;; ecukes-steps.el ends here
