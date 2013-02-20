;;; ecukes-cli.el --- Cucumber for Emacs

(defvar ecukes-path
  (file-name-directory load-file-name)
  "Path to ecukes.")

(add-to-list 'load-path ecukes-path)

(require 'ecukes)

(defun ecukes-cli-print-steps ()
  (let ((has (lambda (flag)
               (when (member flag command-line-args-left)
                 (setq command-line-args-left
                       (delete flag command-line-args-left))
                 t))))
    (ecukes-print-steps (funcall has "--with-doc")
                        (funcall has "--with-file"))))

;;; ecukes-cli.el ends here
