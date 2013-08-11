;;; ecukes-cli.el --- Cucumber for Emacs

(require 'f)

(defvar ecukes-path (f-dirname load-file-name)
  "Path to ecukes.")

(add-to-list 'load-path ecukes-path)

(require 'ecukes-helpers)
(require 'ecukes-setup)
(require 'ecukes-project)
(require 'ecukes-print)
(require 'ecukes-new)

(defun ecukes-cli-print-steps ()
  (ecukes-setup)
  (let ((has (lambda (flag)
               (when (member flag command-line-args-left)
                 (setq command-line-args-left
                       (delete flag command-line-args-left))
                 t))))
    (ecukes-print-steps
     (funcall has "--with-doc")
     (funcall has "--with-file"))))

;;; ecukes-cli.el ends here
