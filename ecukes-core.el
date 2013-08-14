;;; ecukes-core.el --- Core functionality common to all Ecukes components

(defvar ecukes-message nil
  "If true message is internal Ecukes message, otherwise external.")

(defvar ecukes-verbose nil
  "If true, show all message output, otherwise hide.")

(defvar ecukes-internal-message-log nil
  "List with `message' output.")

(defvar ecukes-message-log nil
  "List with `message' output (only from external code).")



(defadvice message (around message-around activate)
  (let ((message
         (if (car (ad-get-args 0))
             (apply 'format (ad-get-args 0))
           "")))
    (unless ecukes-message
      (add-to-list 'ecukes-message-log message t 'eq))
    (when (or ecukes-message ecukes-verbose)
      (add-to-list 'ecukes-internal-message-log `(message . ,message) t 'eq))
    ad-do-it))

(defadvice print (around print-around activate)
  (add-to-list 'ecukes-internal-message-log `(print . ,ad-do-it) t 'eq))

(defun ecukes-quit (&optional exit-code)
  "Quit Emacs with EXIT-CODE and write to file if in graphical mode."
  (or exit-code (setq exit-code 1))
  (let ((outfile (getenv "ECUKES_OUTFILE"))
        (output
         (-each
          ecukes-internal-message-log
          (lambda (log)
            (let ((type (car log))
                  (message (cdr log)))
              (if (eq type 'print)
                  (prin1-to-string message)
                message))))))
    (when outfile
      (f-write outfile (s-concat (s-join "\n" output) "\n"))))
  (kill-emacs exit-code))

(provide 'ecukes-core)

;;; ecukes-core.el ends here

