;;; ecukes-steps.el --- Functions to define and call step definitions

(eval-when-compile (require 'cl))

(require 'ansi)

(require 'ecukes-parse)

(defvar ecukes-steps-definitions nil
  "All defined step definitions.")


;;;###autoload
(defalias 'Given 'ecukes-steps-define-or-call-step
  "Put the system in a known state.")

;;;###autoload
(defalias 'When 'ecukes-steps-define-or-call-step
  "Describe the key action.")

;;;###autoload
(defalias 'Then 'ecukes-steps-define-or-call-step
  "Observe outcomes.")

;;;###autoload
(defalias 'And 'ecukes-steps-define-or-call-step
  "Make Given/When/Then read more fluently.")

;;;###autoload
(defalias 'But 'ecukes-steps-define-or-call-step
  "Make Given/When/Then read more fluently.")

;;;###autoload
(defun ecukes-steps-define-or-call-step (name &rest args)
  "Define or call step."
  (let ((arg (car args)))
    (if (functionp arg)
        (ecukes-steps-define name arg)
      (ecukes-steps-call name args))))

;;;###autoload
(put 'ecukes-steps-define-or-call-step 'lisp-indent-function 'defun)
;;;###autoload
(put 'ecukes-steps-define-or-call-step 'doc-string-elt 2)

(defun ecukes-steps-define (regex fn)
  "Define step."
  (unless (-any?
           (lambda (step-def)
             (equal regex step-def)) ecukes-steps-definitions)
    (add-to-list
     'ecukes-steps-definitions
     (make-ecukes-step-def :regex regex :fn fn))))

(defun ecukes-steps-call (name args)
  "Call step"
  (let* ((query (apply 'format (cons name args)))
         (step-def (ecukes-steps-find query)))
    (if step-def
        (apply (ecukes-step-def-fn step-def)
               (or args
                   (ecukes-steps-args
                    (make-ecukes-step :body name))))
      (error (ansi-red "Step not defined: `%s`" query)))))

(defun ecukes-steps-missing-definition (steps)
  "Return from STEPS those who have not been defined."
  (-reject
   (lambda (step)
     (ecukes-steps-find (ecukes-step-body step))) steps))

(defun ecukes-steps-find (name)
  "Find step by name."
  (-first
   (lambda (step-def)
     (s-matches? (ecukes-step-def-regex step-def) name))
   ecukes-steps-definitions))

(defun ecukes-steps-args (step)
  "Return args from step BODY."
  (let* ((body (ecukes-step-body step))
         (step-def (ecukes-steps-find body)))
    (if step-def
        (cdr (s-match (ecukes-step-def-regex step-def) body))
      (loop for sub on (cdr (split-string body "\""))
            by (function cddr)
            collect (car sub)))))

(provide 'ecukes-steps)

;;; ecukes-steps.el ends here
