(defun ecukes-test-parse-feature-intro (feature-file fn)
  (let* ((feature (ecukes-test-parse-feature (concat "intro/" feature-file)))
         (intro (ecukes-feature-intro feature))
         (header) (description))
    (when intro
      (setq header (ecukes-intro-header intro))
      (setq description(ecukes-intro-description intro)))
    (funcall fn feature intro header description)))

(defun ecukes-test-parse-feature (feature-file)
  (ecukes-parse-feature (concat ecukes-test-path "features/" feature-file)))
