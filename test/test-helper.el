(require 'ecukes-parse)

(defmacro with-steps (&rest body)
  `(let ((ecukes-steps-definitions))
     ,@body))

(defmacro with-hooks (&rest body)
  `(let ((ecukes-hooks-before)
         (ecukes-hooks-after)
         (ecukes-hooks-setup)
         (ecukes-hooks-teardown)
         (ecukes-hooks-fail))
     ,@body))

(defun mock-step (name &rest overrides)
  (let* ((matches (s-match ecukes-parse-step-re name))
         (name (or (plist-get overrides :name) name))
         (head (or (plist-get overrides :head) (nth 1 matches)))
         (body (or (plist-get overrides :body) (nth 2 matches)))
         (type (or (plist-get overrides :type) 'regular))
         (arg (plist-get overrides :arg))
         (err (plist-get overrides :err))
         (properties
          (list :name name :head head :body body :arg arg :type type :err err)))
    (apply 'make-ecukes-step properties)))

(defun with-parse-step (name fn)
  (let* ((feature-file (fixture-file-path "step" name))
         (feature (ecukes-parse-feature feature-file))
         (scenarios (ecukes-feature-scenarios feature))
         (scenario (car scenarios))
         (steps (ecukes-scenario-steps scenario))
         (step (car steps))
         (name (ecukes-step-name step))
         (head (ecukes-step-head step))
         (body (ecukes-step-body step))
         (type (ecukes-step-type step))
         (arg (ecukes-step-arg step)))
    (funcall fn name head body type arg)))

(defun with-parse-scenario (name fn)
  (let* ((feature-file (fixture-file-path "scenario" name))
         (feature (ecukes-parse-feature feature-file))
         (scenarios (ecukes-feature-scenarios feature))
         (scenario (car scenarios))
         (name) (step-names) (tags))
    (condition-case err
        (progn
          (setq name (ecukes-scenario-name scenario))
          (setq step-names (mapcar 'ecukes-step-name (ecukes-scenario-steps scenario)))
          (setq tags (ecukes-scenario-tags scenario)))
      (error))
    (funcall fn scenario name step-names tags)))

(defun with-parse-feature (name fn)
  (let* ((feature-file (fixture-file-path "feature" name))
         (feature (ecukes-parse-feature feature-file))
         (intro (ecukes-feature-intro feature))
         (scenarios (ecukes-feature-scenarios feature))
         (background (ecukes-feature-background feature))
         (steps
          (-concat
           (if background
               (ecukes-background-steps background))
           (if scenarios
               (-flatten
                (-map
                 (lambda (scenario)
                   (ecukes-scenario-steps scenario)) scenarios))))))
    (funcall fn feature intro scenarios background steps)))

(defun with-messages (callback)
  (let ((messages))
    (flet ((ecukes-print-message
            (format-string &rest args)
            (add-to-list 'messages (apply 'ecukes-print-format (cons format-string args)) t 'eq)))
      (funcall callback messages))))

(defmacro with-stats (&rest body)
  `(let ((ecukes-stats-steps 0)
         (ecukes-stats-steps-passed 0)
         (ecukes-stats-steps-failed 0)
         (ecukes-stats-steps-skipped 0)
         (ecukes-stats-scenarios 0)
         (ecukes-stats-scenarios-passed 0)
         (ecukes-stats-scenarios-failed 0))
     ,@body))

(defmacro with-project (&rest body)
  `(with-mock
    (stub ecukes-project-path => "/path/to/project")
    ,@body))

(defun fixture-file-path (category name)
  (let ((category-path (expand-file-name category ecukes-test/fixtures-path)))
    (expand-file-name (format "%s.feature" name) category-path)))
