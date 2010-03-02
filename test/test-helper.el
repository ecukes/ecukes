(defun ecukes-test-parse-feature-intro (feature-file fn)
  (let* ((feature (ecukes-test-parse-feature (concat "intro/" feature-file)))
         (intro (ecukes-feature-intro feature))
         (header) (description))
    (when intro
      (setq header (ecukes-intro-header intro))
      (setq description(ecukes-intro-description intro)))
    (funcall fn feature intro header description)))

(defun ecukes-test-parse-feature-scenario (feature-file fn)
  (let* ((feature (ecukes-test-parse-feature (concat "scenario/" feature-file)))
         (scenarios (ecukes-feature-scenarios feature)))
    (funcall fn feature scenarios)))

(defun ecukes-test-parse-feature-background (feature-file fn)
  (let* ((feature (ecukes-test-parse-feature (concat "background/" feature-file)))
         (background (ecukes-feature-background feature))
         (steps))
    (when background
      (setq steps (ecukes-background-steps background)))
    (funcall fn feature background steps)))

(defun ecukes-test-parse-feature (feature-file)
  (ecukes-parse-feature (concat ecukes-test-path "features/" feature-file)))

(defun ecukes-test-parse-line (feature-file &optional n)
  (ecukes-test-parse-line-helper feature-file 'ecukes-line n))

(defun ecukes-test-parse-blank-line (feature-file &optional n)
  (ecukes-test-parse-line-helper feature-file 'ecukes-blank-line n))

(defun ecukes-test-parse-line-helper (feature-file fn &optional n)
  (with-temp-buffer
    (insert-file-contents-literally (concat ecukes-test-path "features/line/" feature-file))
    (funcall fn n)))
