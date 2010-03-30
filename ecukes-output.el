;;; ecukes-output.el --- Helpers for printing

(defvar ecukes-output-offset 0
  "Current offset (number of spaces).")

(defconst ecukes-output-color-white 37
  "White color code.")

(defconst ecukes-output-color-red 31
  "Red color code.")

(defconst ecukes-output-color-green 32
  "Green color code.")

(defun ecukes-output-intro (intro)
  "Outputs the feature INTRO."
  (setq ecukes-output-offset 0)
  (let ((header (ecukes-intro-header intro)))
    (ecukes-output-white (concat "Feature: " header)))
  (setq ecukes-output-offset 2)
  (dolist (description (ecukes-intro-description intro))
    (ecukes-output-white description))
  (ecukes-output-newline))

(defmacro ecukes-output-scenario (scenario &rest body)
  "Output SCENARIO header, then execute BODY."
  `(let ((scenario-name (concat "Scenario: " (ecukes-scenario-name ,scenario))))
     (ecukes-output-block scenario-name ,@body)))

(defmacro ecukes-output-background (&rest body)
  "Output background header, then execute BODY."
  `(ecukes-output-block "Background:" ,@body))

(defmacro ecukes-output-block (header &rest body)
  "Output HEADER, execute BODY and end with newline."
  `(let ((output (ecukes-output-white ,header))
         (ecukes-output-offset (+ ecukes-output-offset 2)))
     ,@body
     (ecukes-output-newline)))

(defun ecukes-output-step (step success)
  "Output STEP including error if not SUCCESS."
  (let ((type (ecukes-step-type step))
        (name (ecukes-step-name step))
        (arg (ecukes-step-arg step))
        (output-fn (if success 'ecukes-output-green 'ecukes-output-red)))

    (funcall output-fn name)

    (unless success
      (ecukes-output-red (ecukes-step-err step)))

    (let ((ecukes-output-offset (+ ecukes-output-offset 2)))
      (cond ((equal type 'py-string)
             (ecukes-output-py-string arg))
            ((equal type 'table)
             (ecukes-output-table arg))))))

(defun ecukes-output-py-string (py-string)
  "Outputs PY-STRING."
  (let ((py-string "\"\"\""))
    (funcall output-fn py-string)
    (dolist (line (split-string arg "\n"))
      (funcall output-fn line))
    (funcall output-fn py-string)))

(defun ecukes-output-table (table)
  "Outputs TABLE."
  (let ((widths)
        (header (ecukes-table-header table))
        (rows (ecukes-table-rows table)))

    ;; Calculate the maximum width for each column.
    (let ((count-x (length header)))
      (dotimes (i count-x)
        (let ((max 0))
          (dolist (row rows)
            (setq max (max (length (nth i row)) max)))
          (add-to-list 'widths max t))))

    ;; Print the table.
    (funcall output-fn (ecukes-output-table-row header widths))
    (dolist (row rows)
      (funcall output-fn (ecukes-output-table-row row widths)))

    ))

(defun ecukes-output-table-row (row widths)
  "Prints table ROW according to WIDTHS."
  (let ((row-to-s "| ") (count 0))
    (dolist (col row)
      (setq row-to-s (concat row-to-s col (make-string (- (nth count widths) (length col)) 32) " | "))
      (setq count (1+ count)))
    row-to-s))

(defun ecukes-output-newline ()
  "Outputs a newline."
  (ecukes-output-text ""))

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
  (ecukes-output-text (concat "\e[" (number-to-string color) "m" text "\e[0m")))

(defun ecukes-output-text (text)
  "Outputs TEXT."
  (message (concat (make-string ecukes-output-offset 32) text)))

(provide 'ecukes-output)

;;; ecukes-output.el ends here
