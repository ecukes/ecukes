(defvar ecukes-stderr "")
(defvar ecukes-stdout "")

(defun ecukes-should-match (needle haystack)
  (let ((orig-needle needle)
        (orig-haystack haystack)
        (needle (s-replace "\n" "" needle))
        (needle (s-replace " " "" needle))
        (haystack (s-replace "\n" "" haystack))
        (haystack (s-replace " " "" haystack)))
    (unless (s-contains? needle haystack)
      (should (s-contains? orig-needle orig-haystack)))))

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
           (args (if (or (equal command "-h")
                         (equal command "--help"))
                     args
                   (cons "--script" args)))
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
    (f-write (f-expand (s-concat name ".feature") ecukes-project-features-path) content)))
