;;; ecukes-core.el --- Core functionality common to all Ecukes components

(require 'dash)

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
  (let ((outfile (getenv "ECUKES_OUTFILE")))
    (when outfile
      (let ((output
             (-each
              ecukes-internal-message-log
              (lambda (log)
                (let ((type (car log))
                      (message (cdr log)))
                  (if (or (eq type 'print) (eq type 'princ))
                      (prin1-to-string message)
                    message))))))
        (f-write-text 'utf-8 (s-concat (s-join "\n" output) "\n") outfile))))
  (kill-emacs exit-code))

(defun ecukes-fail (format-string &rest objects)
  "Print error message and quit."
  (let ((ecukes-message t))
    (message (apply 'ansi-red (cons format-string objects)))
    (ecukes-quit 1)))

(provide 'ecukes-core)

;;; ecukes-core.el ends here

