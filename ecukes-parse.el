;;; ecukes-parse.el --- Parser for feature files

;; (defconst ecukes-table-re
;;   (concat ecukes-beg-re "|.+|")
;;   "Regular expression matching a table step.")

(defconst ecukes-parse-intro-re
  "^\\s-*Feature:\\s-*\\(.+[^ ]\\)\\s-*$"
  "Regexp matching feature header.")

(defconst ecukes-parse-background-re
  "^\\s-*Background:"
  "Regexp matching background header.")

(defconst ecukes-parse-scenario-re
  "^\\s-*Scenario:\\s-*\\(.+[^ ]\\)\\s-*$"
  "Regexp matching scenario header.")

(defconst ecukes-parse-step-re
  "^\\s-*\\(?:Given\\|When\\|Then\\|And\\|But\\)\\s-+\\(.+[^ ]\\)\\s-*$"
  "Regexp matching step.")

(defconst ecukes-parse-tags-re
  "^\\s-*@"
  "Regexp matching scenario tags.")

(defconst ecukes-parse-py-string-re
  "^\\s-*\"\"\""
  "Regexp matching py string.")

(defconst ecukes-parse-table-re
  "^\\s-*|.+|"
  "")


(defun ecukes-parse-feature (feature)
  "Parse FEATURE."
  (with-temp-buffer
    (insert-file-contents-literally feature)
    (goto-char (point-min))
    (let ((intro (ecukes-parse-intro))
          (background (ecukes-parse-background))
          (scenarios (ecukes-parse-scenarios)))
      (make-ecukes-feature :intro intro :background background :scenarios scenarios))))

(defun ecukes-parse-intro ()
  "Parse intro."
  (when (re-search-forward ecukes-parse-intro-re nil t)
    (let ((header (match-string 1)) (description))
      (while (not (progn (forward-line 1) (ecukes-parse-new-section-p)))
        (let ((line (ecukes-parse-line t)))
          (if line (add-to-list 'description line t))))
      (make-ecukes-intro :header header :description description))))

(defun ecukes-parse-background ()
  "Parse background."
  (when (re-search-forward ecukes-parse-background-re nil t)
    (let ((steps (ecukes-parse-block-steps)))
      (make-ecukes-background :steps steps))))

(defun ecukes-parse-scenarios ()
  "Parse scenarios."
  (let ((scenarios))
    (while (re-search-forward ecukes-parse-scenario-re nil t)
      (add-to-list 'scenarios (ecukes-parse-scenario) t))
    scenarios))

(defun ecukes-parse-scenario ()
  "Parse scenario."
  (let ((name (ecukes-parse-scenario-name))
        (tags (ecukes-parse-scenario-tags))
        (steps (ecukes-parse-block-steps)))
    (make-ecukes-scenario :name name :steps steps :tags tags)))

(defun ecukes-parse-scenario-name ()
  "Parse scenario name."
  (save-excursion
    (let ((line (ecukes-parse-line t)))
      (string-match ecukes-parse-scenario-re line)
      (match-string 1 line))))

(defun ecukes-parse-scenario-tags ()
  "Parse tags."
  (save-excursion
    (forward-line -1)
    (let ((line (ecukes-parse-line t)))
      (when (and line (string-match ecukes-parse-tags-re line))
        (delete-dups
         (mapcar
          (lambda (tag)
            (substring tag 1))
          (split-string line "\\s-+")))))))

(defun ecukes-parse-block-steps ()
  "Parse steps in block."
  (let ((steps))
    (while (not (progn (forward-line 1) (ecukes-parse-new-section-p)))
      (let ((line (ecukes-parse-line t)))
        (if line (add-to-list 'steps (ecukes-parse-step) t))))
    steps))

(defun ecukes-parse-step ()
  "Parse step."
  (let ((name) (arg) (type) (line (ecukes-parse-line t)))
    (when (string-match-p ecukes-parse-step-re line)
      (setq name line))
    (cond
     ((ecukes-parse-py-string-step-p)
      (setq arg (ecukes-parse-py-string-step))
      (setq type 'py-string))
     ((ecukes-parse-table-step-p)
      (setq arg (ecukes-parse-table-step))
      (setq type 'table))
     (t (setq type 'regular)))
    (make-ecukes-step :name name :type type :arg arg)))

(defun ecukes-parse-table-step-p ()
  "Check if step is a table step or not."
  (save-excursion
    (forward-line 1)
    (let ((line (ecukes-parse-line t)))
      (and line (string-match-p ecukes-parse-table-re line)))))

(defun ecukes-parse-table-step ()
  "Parse table step."
  (forward-line 1)
  (let ((rows))
    (while (string-match-p ecukes-parse-table-re (or (ecukes-parse-line t) ""))
      (add-to-list 'rows (ecukes-parse-table-step-row) t)
      (forward-line 1))
    (forward-line -1)
    rows))

(defun ecukes-parse-table-step-row ()
  "Parse row in table."
  (let ((row (ecukes-parse-line t)))
    (delete "" (split-string row "\\s-*|\\s-*"))))

(defun ecukes-parse-py-string-step-p ()
  "Check if step is a py string step or not."
  (save-excursion
    (forward-line 1)
    (let ((line (ecukes-parse-line t)))
      (and line (string-match-p ecukes-parse-py-string-re line)))))

(defun ecukes-parse-py-string-step ()
  "Parse py string step"
  (forward-line 1)
  (let ((whites
         (save-excursion
           (back-to-indentation)
           (current-column)))
        (lines))
    (forward-line 1)
    (while (not (string-match-p ecukes-parse-py-string-re (or (ecukes-parse-line t) "")))
      (let ((line (ecukes-parse-line)))
        (if (<= whites (length line))
            (add-to-list 'lines (substring line whites) t)
          (add-to-list 'lines nil t)))
      (forward-line 1))
    (mapconcat 'identity lines "\n")))

(defun ecukes-parse-line (&optional strip-whitespace)
  "Parse current line."
  (let* ((raw (buffer-substring (line-beginning-position) (line-end-position)))
         (line
          (if strip-whitespace
              (replace-regexp-in-string "^\\s-*\\|\\s-*$" "" raw)
            raw)))
    (if (equal line "") nil line)))

(defun ecukes-parse-new-section-p ()
  "Checks if current line is the start of a new section."
  (let ((line (or (ecukes-parse-line t) "")))
    (or
     (eobp)
     (string-match-p ecukes-parse-background-re line)
     (string-match-p ecukes-parse-scenario-re line))))


(provide 'ecukes-parse)

;;; ecukes-parse.el ends here
