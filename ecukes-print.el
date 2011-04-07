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
    (let* ((state (match-string 2 name))
           (fn (match-string 1 name))
           (regex (ecukes-print-undefined-step-regex state))
           (args (ecukes-print-undefined-step-args state))
           (contents
            (ecukes-template-get
             'step
             `(("regex" . ,regex)
               ("args" . ,args)
               ("fn" . ,fn)))))
      (ecukes-print-message (ansi-yellow contents))
      (ecukes-print-newline))))

(defun ecukes-print-undefined-step-args (state)
  "Return step arguments as a list."
  (let* ((quotes
          (loop for sub on (cdr (split-string state "\""))
                by (function cddr)
                collect (car sub)))
         (num-quotes (length quotes)))
    (mapconcat 'identity (make-list num-quotes "arg") " ")))

(defun ecukes-print-undefined-step-regex (state)
  "Return step regex."
  (let* ((slashes "\\\\\\\\\\\\\\\\")
         (regex "\"[^\"]+\"")
         (rep (format "\"%s(.+%s)\"" slashes slashes)))
    (replace-regexp-in-string regex rep state)))

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
    (message (concat spaces string) objects)))

(provide 'ecukes-print)

;;; ecukes-print.el ends here
