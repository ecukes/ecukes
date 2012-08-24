(defvar ecukes-path
  (file-name-directory load-file-name)
  "Path to ecukes.")

(defvar ecukes-vendor-path
  (expand-file-name "vendor" ecukes-path)
  "Path to ecukes vendor.")

(add-to-list 'load-path ecukes-path)
(add-to-list 'load-path ecukes-vendor-path)

(require 'ecukes-setup)
(require 'ansi-color)

(defun ecukes ()
  (interactive)
  
  (ecukes-run-default)

  (if (getenv "ECUKES_OUTFILE")
      (progn
        (with-temp-buffer
          (mapcar (lambda (line)
                    (insert line) (insert "\n"))
                  *ecukes-message-log*)
          ;; ecukes-tmp-file-target needs to get set from somewhere else
          (write-file (getenv "ECUKES_OUTFILE")))
        ;; kill emacs needs to happen because when ecukes-tmp-file-target
        ;; is set, emacs is running as graphical and -q
        (kill-emacs))
    (progn
      (switch-to-buffer (get-buffer-create "*ecukes-output-buffer*"))
      (erase-buffer)
      (mapcar (lambda (line)
                (insert (ansi-color-apply line))
                (insert "\n"))
              *ecukes-message-log*)
      (font-lock-mode t))))

(provide 'ecukes)
