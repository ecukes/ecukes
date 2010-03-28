;;; ecukes-steps.el --- Step definition stuff

;; TODO: Why do we in this file have access to ecukes-step-re?

(defvar ecukes-steps-definitions (make-hash-table :test 'equal)
  "Hash table containing all step definitions.")

(defun ecukes-step-define (description fn)
  "Defines a step with DESCRIPTION and a function FN.
 Do not use this function directly in step definitions. Use one of:
`Given', `Then', `When', `And' or `But' instead."
  (puthash description fn ecukes-steps-definitions))

(defalias 'Given 'ecukes-step-define
  "Use this to put the system in a known state. So, given what you
say, the system should know that this is the case.")

(defalias 'When 'ecukes-step-define
  "The purpose of When is to describe the key action the user
performs. So, when the user does something. Can be pressing a key,
killing a buffer or selecting a region.")

(defalias 'Then 'ecukes-step-define
  "This is about observing the outcomes. So, given something, then
something happens. Can be a buffer change or a font-size increasing.")

(defalias 'And 'ecukes-step-define
  "If you have several givens, whens or thens. Then you can write use
And to make the text read more fluently.")

(defalias 'But 'ecukes-step-define
  "If you have several givens, whens or thens. Then you can write use
But to make the text read more fluently.")

(defun ecukes-steps-find-definition (step)
  "Finds step definition associated to STEP."
  (let* ((name (ecukes-step-name step)) args fn (count 1) part)
    (string-match ecukes-step-re name)
    (setq part (match-string 1 name))
    (maphash
     (lambda (key value)
       (when (string-match key part)
         (setq fn value)
         (while (match-string count part)
           (add-to-list 'args (match-string count part) t)
           (setq count (1+ count)))))
     ecukes-steps-definitions)
    (if fn (values fn args))))

(provide 'ecukes-steps)

;;; ecukes-steps.el ends here
