;;; ecukes-output.el --- Helpers for printing feature stuff

(defvar ecukes-output-offset 0
  "Current offset (number of spaces).")

(defconst ecukes-output-color-white 37
  "White color code.")

(defconst ecukes-output-color-red 31
  "Red color code.")

(defconst ecukes-output-color-green 32
  "Green color code.")

(defun ecukes-output-intro (intro)
  "Outputs the feature intro"
  (setq ecukes-output-offset 0)
  (let ((header (ecukes-intro-header intro)))
    (ecukes-output-white (concat "Feature: " header)))
  (setq ecukes-output-offset 2)
  (dolist (description (ecukes-intro-description intro))
    (ecukes-output-white description))
  (ecukes-output-newline))

(defun ecukes-output-scenario (scenario)
  "Output scenario header."
  (ecukes-output-header (ecukes-scenario-name scenario)))

(defun ecukes-output-background ()
  "Output background header."
  (ecukes-output-header "Background"))

(defun ecukes-output-header (header)
  "Output HEADER and increase offset size."
  (let ((output (message (concat header ":"))))
    (setq ecukes-output-offset (+ ecukes-output-offset 2))
    output))

(defun ecukes-output-step (step success)
  "Output STEP."
  (let ((name (ecukes-step-name step))
        (output-fn (if success 'ecukes-output-green 'ecukes-output-red)))
    (funcall output-fn name)
    ;; TODO: Output step arg if any.
    ;; TODO: Output step err if any.
    ))

(defun ecukes-output-newline ()
  "Outputs a newline."
  (message ""))

(defun ecukes-output-white (text)
  "Outputs TEXT in white."
  (ecukes-output-color text ecukes-output-color-white))

(defun ecukes-output-red (text)
  "Outputs TEXT in red."
  (ecukes-output-color text ecukes-output-color-red))

(defun ecukes-output-green (text)
  "Outputs TEXT in green."
  (ecukes-output-color text ecukes-output-color-green))

(defun ecukes-output-color (text color)
  "Outputs TEXT in COLOR."
  (message (concat "\e[" (number-to-string color) "m" (make-string ecukes-output-offset 32) text "\e[0m")))

(provide 'ecukes-output)

;;; ecukes-output.el ends here
