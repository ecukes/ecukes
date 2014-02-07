;;; ecukes-mode.el --- Mode for ecukes fontification

(defvar ecukes-mode-load-hook nil
  "*Hook to run when `ecukes-mode' is loaded.")

(defvar ecukes-mode-hook nil
  "*Hook to setup `ecukes-mode'.")

(defvar ecukes-mode-syntax-table nil)

(unless ecukes-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?#  "<" table)
    (modify-syntax-entry ?\n ">" table)
    (setq ecukes-mode-syntax-table table)))

(eval-and-compile
  
  (defvar ecukes-feature-keyword
    "Feature"
    "Keyword for the Feature entry.")
  
  (defvar ecukes-scenario-keywords 
    '("Scenario" "Background")
    "Keyword for Scenario and Background entries.")

  (defvar ecukes-step-keywords 
    '("Given" "When" "Then" "And" "But")
    "Keyword for the step definion entries.")

  (defvar ecukes-font-lock-keywords
    (eval-when-compile
      `((,(regexp-opt (list ecukes-feature-keyword) 'words)
         (1 font-lock-type-face))
        (,(regexp-opt ecukes-scenario-keywords 'words)
         (1 font-lock-keyword-face))
        (,(regexp-opt ecukes-step-keywords 'words)
         (1 font-lock-variable-name-face))))
  "Expressions to highlight in `ecukes-mode'."))

;;;###autoload
(defun ecukes-mode ()
  "Mode for ecukes fontification."
  (interactive)

  (kill-all-local-variables)
  (set-syntax-table ecukes-mode-syntax-table)
  (setq mode-name "ecukes"
        major-mode 'ecukes-mode
        comment-start "#"
        comment-end   "")

  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(ecukes-font-lock-keywords))

  (run-hooks 'ecukes-mode-hook)
  nil)

(run-hooks 'ecukes-mode-load-hook)

(provide 'ecukes-mode)

;;; ecukes-mode.el ends here
