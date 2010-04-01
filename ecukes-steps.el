;;; ecukes-steps.el --- Step definition stuff

(defvar ecukes-steps-definitions (make-hash-table :test 'equal)
  "Hash table containing all step definitions.")


(defun ecukes-steps-define (description &optional fn)
  "With the FN argument, define a step with DESCRIPTION and function
  FN and without call the step definitions function with DESCRIPTION.
 Do not use this function directly in step definitions. Use one of:
`Given', `Then', `When', `And' or `But' instead."
  (if fn
      (puthash description fn ecukes-steps-definitions)
    (let ((step-def (ecukes-steps-find-definition-by-name description)))
      (if step-def
          (funcall (ecukes-step-def-fn step-def))
        (error (concat "Step definition \"" description "\" not found"))))))

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
  (let ((name (ecukes-step-name step)))
    (string-match ecukes-step-re name)
    (ecukes-steps-find-definition-by-name (match-string 2 name))))

(defun ecukes-steps-find-definition-by-name (name)
  "Finds step definition associated to a steps NAME."
  (let ((count 1) args fn)
    (maphash
     (lambda (key value)
       (when (string-match key name)
         (setq fn value)
         (while (match-string count name)
           (add-to-list 'args (match-string count name) t)
           (setq count (1+ count)))))
     ecukes-steps-definitions)
    (if fn (make-ecukes-step-def :fn fn :args args))))

(defun ecukes-steps-missing (features)
  "Goes through all FEATURES steps and returns a list of the once that
are missing a step definition."
  (let ((missing))
    (dolist (feature features)
      (let ((background (ecukes-feature-background feature))
            (scenarios (ecukes-feature-scenarios feature)))
        ;; Background steps
        (when background
          (dolist (step (ecukes-background-steps background))
            (unless (ecukes-steps-find-definition step)
              (add-to-list 'missing step t 'eq))))
        ;; Scenario steps
        (dolist (scenario scenarios)
          (dolist (step (ecukes-scenario-steps scenario))
            (unless (ecukes-steps-find-definition step)
              (add-to-list 'missing step t 'eq))))))
    missing))


(provide 'ecukes-steps)

;;; ecukes-steps.el ends here
