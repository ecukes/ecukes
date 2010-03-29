;;; ecukes-steps.el --- Step definition stuff

(defvar ecukes-steps-definitions (make-hash-table :test 'equal)
  "Hash table containing all step definitions.")

(defun ecukes-steps-define (description &optional fn)
  "Defines a step with DESCRIPTION and function FN.
 Do not use this function directly in step definitions. Use one of:
`Given', `Then', `When', `And' or `But' instead."
  (puthash description fn ecukes-steps-definitions))

(defalias 'Given 'ecukes-steps-define
  "Use this to put the system in a known state. Given what you say,
the system should know that this is the case.")

(defalias 'When 'ecukes-steps-define
  "Describes the key action the user performs. When the user does
something. Can be pressing a key, killing a buffer or selecting a region.")

(defalias 'Then 'ecukes-steps-define
  "Observing the outcomes. Given something, then something else
happens. Can be a buffer change or a font-size increasing.")

(defalias 'And 'ecukes-steps-define
  "If you have multiple given, when or then in a row. Then you can use
And to make the text read more fluently.")

(defalias 'But 'ecukes-steps-define
  "If you have multiple given, when or then in a row. Then you can use
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
