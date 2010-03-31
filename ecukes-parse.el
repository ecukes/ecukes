;;; ecukes-parse.el --- Feature file parser

(defconst ecukes-beg-re "^[[:blank:]]*"
  "Regular expression matching beginning of a statement.")

(defconst ecukes-feature-re (concat ecukes-beg-re "Feature:[[:blank:]]*\\(.+[^ ]\\)[[:blank:]]*$")
  "Regular expression matching a feature header.")

(defconst ecukes-background-re (concat ecukes-beg-re "Background:")
  "Regular expression matching a background header.")

(defconst ecukes-scenario-re (concat ecukes-beg-re "Scenario:[[:blank:]]*\\(.+[^ ]\\)[[:blank:]]*$")
  "Regular expression matching a scenario header.")

(defconst ecukes-tags-re (concat ecukes-beg-re "\\(?:@[^ \t]+\\)")
  "Regular expression matching tags line.")

(defconst ecukes-py-string-re "^\\([[:blank:]]*\\)\"\"\""
  "Regular expression matching a py string step with grouping for
whitespace at the beginning.")

(defconst ecukes-table-re (concat ecukes-beg-re "|.+|")
  "Regular expression matching a table step.")

(defconst ecukes-step-re (concat ecukes-beg-re "\\(?:Given\\|When\\|Then\\|And\\|But\\)[[:blank:]]*\\(.+\\)[[:blank:]]*")
  "Regular expression matching a step name.")

(defun ecukes-parse-feature (feature-file)
  "Parses FEATURE-FILE and returns an `ecukes-feature' object."
  (with-temp-buffer
    (insert-file-contents-literally feature-file)
    (let ((intro (ecukes-parse-intro))
          (background (ecukes-parse-background))
          (scenarios (ecukes-parse-scenarios)))
      (make-ecukes-feature :intro intro :background background :scenarios scenarios))))

(defun ecukes-parse-intro ()
  "Parses the feature intro."
  (goto-char (point-min))
  (when (re-search-forward ecukes-feature-re nil t)
    (let ((header (match-string-no-properties 1))
          (description))
      (forward-line 1)
      (while (not (string= (ecukes-blank-line) ""))
        (add-to-list 'description (ecukes-blank-line) t)
        (forward-line 1))
      (make-ecukes-intro :header header :description description))))

(defun ecukes-parse-background ()
  "Parses the feature background."
  (goto-char (point-min))
  (when (re-search-forward ecukes-background-re nil t)
    (forward-line 1)
    (let ((steps))
      (ecukes-parse-block
       (lambda (step)
         (add-to-list 'steps step t)))
      (make-ecukes-background :steps steps))))

(defun ecukes-parse-scenarios ()
  "Parses the feature scenario."
  (let ((scenarios))
    (while (re-search-forward ecukes-scenario-re nil t)
      (let ((steps) (name) (tags-line (ecukes-blank-line -1)) (tags))
        (if (string-match-p ecukes-tags-re tags-line)
            (setq tags (split-string (replace-regexp-in-string "@" "" tags-line) "[[:blank:]]+")))
        (setq name (match-string-no-properties 1))
        (forward-line 1)
        (ecukes-parse-block
         (lambda (step)
           (add-to-list 'steps step t)))
        (add-to-list 'scenarios (make-ecukes-scenario :name name :steps steps :tags tags) t)))
    scenarios))

(defun ecukes-parse-block (fn)
  "Parses a block."
  (while (not (string= (ecukes-blank-line) ""))
    (if (string-match-p ecukes-step-re (ecukes-blank-line))
        (let ((step (ecukes-parse-step)))
          (funcall fn step)))
    (forward-line 1)))

(defun ecukes-parse-step ()
  "Parses a step."
  (let ((peek (ecukes-line 1)) (name (ecukes-blank-line)) arg type)
    (cond ((string-match-p ecukes-py-string-re peek)
           (setq arg (ecukes-parse-py-string))
           (setq type 'py-string))
          ((string-match-p ecukes-table-re peek)
           (setq arg (ecukes-parse-table))
           (setq type 'table)))
    (make-ecukes-step :name name :arg arg :type (or type 'regular))))

(defun ecukes-parse-py-string ()
  "Parses a py string step."
  (let ((peek (ecukes-line 1)) (lines) (offset))
    (string-match ecukes-py-string-re peek)
    (setq offset (length (match-string-no-properties 1 peek)))
    (forward-line 2)
    (while (not (string-match-p ecukes-py-string-re (ecukes-line)))
      (let ((line (replace-regexp-in-string (concat "^[[:blank:]]" "\\{" (number-to-string offset) "\\}") "" (ecukes-line))))
        (add-to-list 'lines line t)
        (forward-line 1)))
    (mapconcat 'identity lines "\n")))

(defun ecukes-parse-table ()
  "Parses a table step."
  (forward-line 1)
  (let ((header (ecukes-parse-table-row (ecukes-blank-line))) rows)
    (forward-line 1)
    (while (string-match-p ecukes-table-re (ecukes-line))
      (add-to-list 'rows (ecukes-parse-table-row (ecukes-blank-line)) t)
      (forward-line 1))
    (forward-line -1)
    (make-ecukes-table :header header :rows rows)))

(defun ecukes-parse-table-row (row)
  "Parses the table ROW."
  (delete "" (split-string row "[[:blank:]]*|[[:blank:]]*")))

(defun ecukes-line (&optional n)
  "Returns current line with offset N."
  (save-excursion
    (forward-line (or n 0))
    (buffer-substring-no-properties (line-beginning-position) (line-end-position))))

(defun ecukes-blank-line (&optional n)
  "Returns current line with offset N, excluding whitespace at the
beginning and end."
  (let ((line (ecukes-line n)))
    (replace-regexp-in-string "\\(^[[:blank:]]*\\|[[:blank:]]*$\\)" "" line)))

(provide 'ecukes-parse)

;;; ecukes-parse.el ends here
