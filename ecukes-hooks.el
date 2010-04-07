;;; ecukes-hooks.el --- Before and After hooks

(defvar ecukes-hooks-before '()
  "List of before hooks.")

(defvar ecukes-hooks-after '()
  "List of after hooks.")

(defvar ecukes-hooks-setup '()
  "List of setup hooks.")

(defvar ecukes-hooks-teardown '()
  "List of teardown hooks.")


(defmacro Before (&rest body)
  "Adds BODY to be executed before each scenario."
  `(add-to-list 'ecukes-hooks-before (lambda () ,@body) t))

(defmacro After (&rest body)
  "Adds BODY to be executed after each scenario."
  `(add-to-list 'ecukes-hooks-after (lambda () ,@body) t))

(defmacro Setup (&rest body)
  "Adds BODY to be executed before all features."
  `(add-to-list 'ecukes-hooks-setup (lambda () ,@body) t))

(defmacro Teardown (&rest body)
  "Adds BODY to be executed after all features."
  `(add-to-list 'ecukes-hooks-teardown (lambda () ,@body) t))


(defun ecukes-hooks-run-before ()
  "Executes all before hooks."
  (ecukes-hooks-run ecukes-hooks-before))

(defun ecukes-hooks-run-after ()
  "Executes all after hooks."
  (ecukes-hooks-run ecukes-hooks-after))

(defun ecukes-hooks-run-setup ()
  "Executes all setup hooks."
  (ecukes-hooks-run ecukes-hooks-setup))

(defun ecukes-hooks-run-teardown ()
  "Executes all teardown hooks."
  (ecukes-hooks-run ecukes-hooks-teardown))


(defun ecukes-hooks-run (hooks)
  "Executes all HOOKS."
  (dolist (hook hooks)
    (funcall hook)))


(provide 'ecukes-hooks)

;;; ecukes-hooks.el ends here
