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
    (let ((dirs (-reject 's-blank? (s-split "/" (f-parent name))))
          (path (f-expand (s-concat name ".feature") ecukes-project-features-path))
          (default-directory ecukes-project-features-path))
      (unless (f-dir? (f-parent name))
        (apply 'f-mkdir dirs))
      (f-write-text (s-concat content "\n") 'utf-8 path))))

(Given "^step definition:$"
  (lambda (code)
    (let ((path (f-expand "super-project-steps.el" ecukes-project-step-definitions-path)))
      (f-write-text (s-concat code "\n") 'utf-8 path))))

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
    (ecukes-should-match content (f-read-text (f-expand file ecukes-project-path) 'utf-8))))

(Then "^I should see list of reporters:$"
  (lambda ()
    (-each
     '("dot" "spec" "landing" "progress" "magnars" "gangsta")
     (lambda (reporter)
       (Then "I should see command output:" (s-concat reporter " - "))))))

(When "^I create file \"\\([^\"]+\\)\" with content:$"
  (lambda (file content)
    (let ((path (f-expand file ecukes-project-path)))
      (f-write-text content 'utf-8 path))))

(When "^I byte compile \"\\([^\"]+\\)\"$"
  (lambda (file)
    (byte-compile-file (f-expand file ecukes-project-path))))
