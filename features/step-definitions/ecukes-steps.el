(defun ecukes-should-match (needle haystack)
  (should (s-contains? needle haystack)))

(When "^I run ecukes \"\\([^\"]*\\)\"$"
  (lambda (command)
    (let* ((buffer-name "*ecukes-output*")
           (buffer
            (progn
              (when (get-buffer buffer-name)
                (kill-buffer buffer-name))
              (get-buffer-create buffer-name)))
           (default-directory (file-name-as-directory ecukes-project-path))
           (args
            (unless (equal command "")
              (s-split " " command)))
           (exit-code
            (apply
             'call-process
             (append (list ecukes-executable nil buffer nil) args))))
      (with-current-buffer buffer
        (let ((content (ansi-color-filter-apply (buffer-string))))
          (cond ((= exit-code 0)
                 (setq ecukes-stdout content))
                (t
                 (setq ecukes-stderr content))))))))

(Then "^I should see command output:$"
  (lambda (expected)
    (ecukes-should-match expected ecukes-stdout)))

(Then "^I should see command error:$"
  (lambda (expected)
    (ecukes-should-match expected ecukes-stderr)))

(Given "^feature \"\\([^\"]+\\)\":$"
  (lambda (name content)
    (let ((path (f-expand (s-concat name ".feature") ecukes-project-features-path)))
      (f-write path (s-concat content "\n")))))

(Given "^step definition:$"
  (lambda (code)
    (let ((path (f-expand "super-project-steps.el" ecukes-project-step-definitions-path)))
      (f-write path (s-concat code "\n")))))

(Given "^these files should exist:$"
  (lambda (table)
    (let ((files (cdr table)))
      (-each
       files
       (lambda (file)
         (should (f-exists? (f-expand (car file) ecukes-project-path))))))))

(When "^I visit project \"\\([^\"]+\\)\"$"
  (lambda (name)
    (setq ecukes-project-path (f-expand name ecukes-projects-path))
    (unless (f-dir? ecukes-project-path)
      (f-mkdir ecukes-project-path))))

(Then "^the file \"\\([^\"]+\\)\" should contain:$"
  (lambda (file content)
    (ecukes-should-match content (f-read (f-expand file ecukes-project-path)))))
