;;; ecukes-cli.el --- Cucumber for Emacs

(defvar ecukes-path
  (file-name-directory load-file-name)
  "Path to ecukes.")

(add-to-list 'load-path ecukes-path)

(require 'ecukes)

(defun ecukes-print-steps (&optional with-doc with-file)
  "Print all available steps defined for this project.
Include docstring when WITH-DOC is non-nil."
  (ecukes-assert-in-project)
  (ecukes-setup)
  (-map
   (lambda (step-def)
     (when with-file
       (let ((file (ecukes-step-file-name step-def t)))
         (princ file)
         (princ ": ")))
     (princ (ansi-green (ecukes-step-def-regex step-def)))
     (princ "\n")
     (when (and with-doc (ecukes-step-def-doc step-def))
       (princ (ecukes-step-def-doc step-def))
       (princ "\n")))
   ecukes-steps-definitions))

(defun ecukes-cli-print-steps ()
  (let ((has (lambda (flag)
               (when (member flag command-line-args-left)
                 (setq command-line-args-left
                       (delete flag command-line-args-left))
                 t))))
    (ecukes-print-steps (funcall has "--with-doc")
                        (funcall has "--with-file"))))

;;; ecukes-cli.el ends here
