;;; ecukes-hooks.el --- Before and after hooks

(defvar ecukes-before-hooks '()
  "List of before hooks.")

(defvar ecukes-after-hooks '()
  "List of after hooks.")


(defmacro Before (&rest body)
  "Adds BODY to be executed before each scenario."
  `(add-to-list 'ecukes-before-hooks (lambda () ,@body) t))

(defmacro After (&rest body)
  "Adds BODY to be executed after each scenario."
  `(add-to-list 'ecukes-after-hooks (lambda () ,@body) t))

(defun ecukes-hooks-run-before ()
  "Runs all before hooks."
  (ecukes-hooks-run ecukes-before-hooks))

(defun ecukes-hooks-run-after ()
  "Runs all after hooks."
  (ecukes-hooks-run ecukes-after-hooks))

(defun ecukes-hooks-run (hooks)
  "Runs all HOOKS."
  (dolist (hook hooks)
    (funcall hook)))


(provide 'ecukes-hooks)

;;; ecukes-hooks.el ends here
