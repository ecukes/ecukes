;;; ecukes-output.el --- Helpers for outputting stuff

(defvar ecukes-output-offset 0
  "Current offset (number of spaces).")


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

    ;; Print the step name
    (if success (ecukes-output-green name) (ecukes-output-red name))

    ;; Print error if any
    (unless success (ecukes-output-red (ecukes-step-err step)))

    ;; Print py-string or table
    (let ((ecukes-output-offset (+ ecukes-output-offset 2)))
      (cond ((equal type 'py-string)
             (ecukes-output-py-string arg output-fn))
            ((equal type 'table)
             (ecukes-output-table arg output-fn))))
    ))

(defun ecukes-output-py-string (py-string output-fn)
  "Outputs PY-STRING."
  (let ((py-string "\"\"\""))
    (funcall output-fn py-string)
    (dolist (line (split-string arg "\n"))
      (funcall output-fn line))
    (funcall output-fn py-string)))

(defun ecukes-output-table (table output-fn)
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

(defun ecukes-output-missing-steps (steps)
  "Prints all missing steps."
  (ecukes-output-yellow
   "Some steps does not have a matching definition. Please implement the following step definitions:")
  (ecukes-output-newline)
  (dolist (step steps)
    (ecukes-output-yellow
     (ecukes-output-missing-step step))))

(defun ecukes-output-missing-step (step)
  "Return a string for how to define missing STEP."
  (let ((name (ecukes-step-name step)))
    (string-match ecukes-step-re name)
    (let* ((match (match-string-no-properties 2 name))
           (prefix (match-string-no-properties 1 name))
           (suffix (replace-regexp-in-string "\"[^\"]+\"" "\\\\\"\\\\\\\\(.+\\\\\\\\)\\\\\"" match))
           (args (ecukes-output-missing-step-args step)))
      (concat "(" prefix " \"" suffix "\"\n       (lambda (" args ")\n\n         ))\n"))))

(defun ecukes-output-missing-step-args (step)
  "For missing STEP, return a string with it's step definition arguments."
  (let ((count 0))
    ;; TODO: Find a better way to find out the number of inline strings.
    (setq count (length (loop for sub on (cdr (split-string match "\"")) by (function cddr) collect (car sub))))
    (if (member (ecukes-step-type step) '(py-string table))
        (setq count (1+ count)))
    (cond ((= count 0) "")
          ((= count 1) "arg")
          ((> count 1)
           (let ((result))
             (dotimes (i count)
               (add-to-list 'result (concat "arg" (number-to-string (1+ i))) t))
             (mapconcat 'identity result " "))))))

(defun ecukes-output-white (text)
  "Outputs TEXT in white."
  (ecukes-output-text (ecukes-color-white text)))

(defun ecukes-output-red (text)
  "Outputs TEXT in red."
  (ecukes-output-text (ecukes-color-red text)))

(defun ecukes-output-green (text)
  "Outputs TEXT in green."
  (ecukes-output-text (ecukes-color-green text)))

(defun ecukes-output-yellow (text)
  "Outputs TEXT in yellow."
  (ecukes-output-text (ecukes-color-yellow text)))

(defun ecukes-output-text (&rest body)
  "Outputs TEXT according to `ecukes-output-offset'."
  (ecukes-ouput-message (apply 'concat (cons (make-string ecukes-output-offset 32) body))))

(defun ecukes-output-no-indent (&rest body)
  "Outputs TEXT, just as it is."
  (ecukes-ouput-message (apply 'concat body)))

(defun ecukes-output-newline ()
  "Outputs a newline."
  (ecukes-ouput-message " "))

(defun ecukes-ouput-message (format-string &rest args)
  "Exactly as `message' only that it uses `princ' for printing the message.
This is used because message is advised to avoid clobbering the Ecukes
output with output from stuff in the step definitions."
  (princ (concat (apply 'format format-string args) "\n")))


(provide 'ecukes-output)

;;; ecukes-output.el ends here
