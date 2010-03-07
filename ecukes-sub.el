;;; ecukes-sub.el --- For running an Emacs sub process

(defun ecukes-sub-run-process (&rest functions)
  "Runs FUNCTIONS in an Emacs sub process."
  (let ((args) (buffer "*Ecukes*"))
    ;; Clear the buffer
    (when (get-buffer buffer)
      (set-buffer buffer)
      (erase-buffer))
    
    (setq args `(,nil ,buffer ,t "-Q" "--batch" "-l" "ecukes-run.el"))

    ;; Add all functions as arguments
    (dolist (function functions)
      (add-to-list 'args (copy-seq "-f") t 'eq)
      (add-to-list 'args (symbol-name function) t 'eq))

    ;; Run the sub process
    (apply 'call-process "emacs" args)

    (set-buffer buffer)
    (message (buffer-substring-no-properties (point-min) (point-max)))))

(provide 'ecukes-sub)

;;; ecukes-sub.el ends here
