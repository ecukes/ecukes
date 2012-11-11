;;; ecukes.el --- Cucumber for Emacs

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
  (progn
    (setq *ecukes-message-log* (list ""))
    (ecukes-stats-reset)
    (ecukes-run-default)
    (switch-to-buffer (get-buffer-create "*ecukes-output-buffer*"))
    (erase-buffer)
    (mapcar (lambda (line)
              (insert (ansi-color-apply line))
              (insert "\n"))
            *ecukes-message-log*)
    (font-lock-mode t)))

(provide 'ecukes)

;;; ecukes.el ends here
