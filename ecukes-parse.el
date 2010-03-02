;;; ecukes-parse.el --- Parser stuff

(defstruct ecukes-feature intro background scenarios)
(defstruct ecukes-intro header description)
(defstruct ecukes-background steps)
(defstruct ecukes-scenario name steps)
(defstruct ecukes-step name arg)

(defconst ecukes-feature-re "Feature: *\\(.+[^ ]\\) *$"
  "Regular expression matching a feature header.")

(defconst ecukes-background-re "Background:"
  "Regular expression matching a background.")

(defconst ecukes-scenario-re "Scenario: *\\(.+[^ ]\\) *$"
  "Regular expression matching a scenario header.")

(defconst ecukes-py-string-re "\\(.+\\)\"\"\""
  "Regular expression matching a py string step with grouping for
  whitespace at the beginning.")

(defconst ecukes-table-re "^ *|.+|"
  "Regular expression matching a table step.")

(defun ecukes-parse-feature (feature-file)
  (with-temp-buffer
    (insert-file-contents-literally feature-file)
    (let ((intro (ecukes-parse-intro))
          (background (ecukes-parse-background))
          (scenarios (ecukes-parse-scenarios)))
      (make-ecukes-feature :intro intro :background background :scenarios scenarios))))

(defun ecukes-parse-intro ()
  "Parses the intro of a feature."
  (goto-char (point-min))
  (when (re-search-forward ecukes-feature-re nil t)
    (let ((header (match-string-no-properties 1))
          (description (list)))
      (forward-line 1)
      (while (not (string= (ecukes-blank-line) ""))
        (add-to-list 'description (ecukes-blank-line) t)
        (forward-line 1))
      (make-ecukes-intro :header header :description description))))

(defun ecukes-parse-background ()
  "Parses a feature background."
  (goto-char (point-min))
  (when (re-search-forward ecukes-background-re nil t)
    (forward-line 1)
    (let ((steps (list)))
      (ecukes-parse-block
       (lambda (step)
         (add-to-list 'steps step t)))
      (make-ecukes-background :steps steps))))

(defun ecukes-parse-scenarios ()
  "Parses a feature scenario."
  (let ((scenarios (list)))
    (while (re-search-forward ecukes-scenario-re nil t)
      (let ((steps (list)) (name))
        (setq name (match-string-no-properties 1))
        (forward-line 1)
        (ecukes-parse-block
         (lambda (step)
           (add-to-list 'steps step t)))
        (add-to-list 'scenarios (make-ecukes-scenario :name name :steps steps) t)))
    scenarios))

(defun ecukes-parse-block (fn)
  "Parses a block."
  (while (not (string= (ecukes-blank-line) ""))
    ;; TODO: Check if step
    (let ((step (ecukes-parse-step)))
      (funcall fn step))
    (forward-line 1)))

(defun ecukes-parse-step ()
  "Parses a step."
  (let ((peek (ecukes-line 1)) (name (ecukes-blank-line)) (arg))
    (cond ((string-match ecukes-py-string-re peek)
           (setq arg (ecukes-parse-py-string)))
          ((string-match ecukes-table-re peek)
           (setq arg (ecukes-parse-table))))
    (make-ecukes-step :name name :arg arg)))

(defun ecukes-parse-py-string ()
  "Parses a py string step"
  (let ((lines) (offset (length (match-string-no-properties 1 (ecukes-line 1)))))
    (forward-line 2)
    (while (not (string-match-p ecukes-py-string-re (ecukes-line)))
      (let ((line (replace-regexp-in-string (concat "^[[:space:]]" "\\{" (number-to-string offset) "\\}") "" (ecukes-line))))
        (add-to-list 'lines line t)
        (forward-line 1)))
    (mapconcat 'identity lines "\n")))

(defun ecukes-parse-table ()
  "Parses a table step"
  (forward-line 2)
  (let ((rows (list)))
    (while (string-match-p ecukes-table-re (ecukes-line))
      (let ((cols (delete "" (split-string (ecukes-blank-line) "[[:blank:]]*|[[:blank:]]*"))))
        (add-to-list 'rows cols t)
        (forward-line 1)))
    (forward-line -1)
    rows))

(defun ecukes-line (&optional n)
  (save-excursion
    (forward-line (or n 0))
    (buffer-substring-no-properties (line-beginning-position) (line-end-position))))

(defun ecukes-blank-line (&optional n)
  (let ((line (ecukes-line n)))
    (replace-regexp-in-string "\\(^[[:space:]]*\\|[[:space:]]*$\\)" "" line)))

(provide 'ecukes-parse)

;;; ecukes-parse.el ends here
