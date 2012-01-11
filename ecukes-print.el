;;; ecukes-print.el --- Print various stuff on screen

(defvar ecukes-print-offset 0
  "Current indentation offset (number of spaces).")


(defun ecukes-print-undefined-steps (steps)
  "Print STEPS as undefined."
  (ecukes-print-undefined-steps-title)
  (mapc 'ecukes-print-undefined-step steps))

(defun ecukes-print-undefined-step (step)
  "Print STEP as undefined."
  (let ((name (ecukes-step-name step)))
    (string-match ecukes-parse-step-re name)
    (let* ((fn (match-string 1 name))
           (regex (ecukes-print-undefined-step-regex step))
           (args (ecukes-print-undefined-step-args step))
           (contents
            (ecukes-template-get
             'step
             `(("regex" . ,regex)
               ("args" . ,args)
               ("fn" . ,fn)))))
      (ecukes-print-message (ansi-yellow contents))
      (ecukes-print-newline))))

(defun ecukes-print-undefined-step-args (step)
  "Return STEP arguments as a list."
  (let* ((query (ecukes-steps-query step))
         (quotes
          (loop for sub on (cdr (split-string query "\""))
                by (function cddr)
                collect (car sub)))
         (num-quotes (length quotes)))
    (if (or (equal (ecukes-step-type step) 'table)
            (equal (ecukes-step-type step) 'py-string))
        (setq num-quotes (1+ num-quotes)))
    (mapconcat 'identity (make-list num-quotes "arg") " ")))

(defun ecukes-print-undefined-step-regex (step)
  "Return step regex."
  (let* ((query (ecukes-steps-query step))
         (slashes "\\\\\\\\\\\\\\\\")
         (regex "\"[^\"]+\"")
         (rep (format "\\\\\\\\\"%s(.+%s)\\\\\\\\\"" slashes slashes)))
    (replace-regexp-in-string regex rep query)))

(defun ecukes-print-undefined-steps-title ()
  "Print missing steps title."
  (ecukes-print-message
   (ansi-yellow
    "Some steps does not have a matching definition. Please implement the following step definitions:\n")))

(defun ecukes-print-intro (intro)
  "Print intro."
  (setq ecukes-print-offset 0)
  (let ((header (ecukes-intro-header intro)))
    (ecukes-print-message "Feature: %s" header))
  (setq ecukes-print-offset 2)
  (dolist (description (ecukes-intro-description intro))
    (ecukes-print-message description))
  (ecukes-print-newline))

(defun ecukes-print-newline ()
  "Print newline."
  (ecukes-print-message " "))

(defun ecukes-print-message (string &rest objects)
  "Print message taking offset into account."
  (let* ((ascii-space 32)
         (spaces (make-string ecukes-print-offset ascii-space)))
    (apply 'message (cons (concat spaces string) objects))))

(defun ecukes-print-background-header ()
  "Print background header."
  (ecukes-print-message "Background:"))

(defun ecukes-print-step-success (step)
  "Print step as success."
  (ecukes-print-step step 'ansi-green))

(defun ecukes-print-step-failure (step)
  "Print step as failure. Also print the error."
  (ecukes-print-step step 'ansi-red)
  (let ((err (ecukes-step-err step)))
    (ecukes-print-error err)))

(defun ecukes-print-step-pending (step)
  "Print step as pending."
  (ecukes-print-step step 'ansi-yellow))

(defun ecukes-print-error (err)
  "Print error messag ERR."
  (setq ecukes-print-offset (+ ecukes-print-offset 4))
  (ecukes-print-message (ansi-red err))
  (setq ecukes-print-offset (- ecukes-print-offset 4)))

(defun ecukes-print-step (step color)
  "Print STEP in COLOR."
  (setq ecukes-print-offset (+ ecukes-print-offset 2))
  (let ((name (ecukes-step-name step)))
    (ecukes-print-message (funcall color name)))
  (setq ecukes-print-offset (- ecukes-print-offset 2)))

(defun ecukes-print-scenario-header (scenario)
  "Print SCENARIO header."
  (let ((name (ecukes-scenario-name scenario)))
    (ecukes-print-message "Scenario: %s" name)))


(provide 'ecukes-print)

;;; ecukes-print.el ends here
