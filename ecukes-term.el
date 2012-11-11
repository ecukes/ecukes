;;; ecukes-term.el --- Cucumber for Emacs

(defvar ecukes-path
  (file-name-directory load-file-name)
  "Path to ecukes.")

(add-to-list 'load-path ecukes-path)

(require 'ecukes-setup)

(add-to-list 'command-switch-alist '("--new" . ecukes-new-handler))

(ecukes-run-default)

(when (getenv "ECUKES_OUTFILE")
  (with-temp-buffer
    (mapcar (lambda (line)
              (insert line) (insert "\n"))
            *ecukes-message-log*)
    ;; ecukes-tmp-file-target needs to get set from somewhere else
    (write-file (getenv "ECUKES_OUTFILE")))
  ;; kill emacs needs to happen because when ecukes-tmp-file-target
  ;; is set, emacs is running as graphical and -q
  (kill-emacs))

;;; ecukes-term.el ends here
