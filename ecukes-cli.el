;;; ecukes-cli.el --- Cucumber for Emacs

(defvar ecukes-path
  (file-name-directory load-file-name)
  "Path to ecukes.")

(add-to-list 'load-path ecukes-path)

(require 'ecukes-helpers)
(require 'ecukes-setup)
(require 'ecukes-project)
(require 'ecukes-print)

(ecukes-setup)

(defun ecukes-cli-print-steps ()
  (let ((has (lambda (flag)
               (when (member flag command-line-args-left)
                 (setq command-line-args-left
                       (delete flag command-line-args-left))
                 t))))
    (ecukes-print-steps
     (funcall has "--with-doc")
     (funcall has "--with-file"))))

;;; ecukes-cli.el ends here
