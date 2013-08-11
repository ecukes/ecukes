(require 's)

(defvar ecukes-stderr "")
(defvar ecukes-stdout "")

(When "^I run ecukes \"\\([^\"]+\\)\"$"
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
        (let ((content (buffer-string)))
          (cond ((= exit-code 0)
                 (setq ecukes-stdout content))
                (t
                 (setq ecukes-stderr content))))))))

(Then "^I should see command output:$"
  (lambda (expected)
    (should (s-matches? (regexp-quote expected) ecukes-stdout))))

(Then "^I should see command error:$"
  (lambda (expected)
    (should (s-matches? (regexp-quote expected) ecukes-stderr))))
