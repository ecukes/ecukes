;;; ecukes-term.el --- Cucumber for Emacs

(defvar ecukes-path
  (file-name-directory load-file-name)
  "Path to ecukes.")

(add-to-list 'load-path ecukes-path)

(require 'ecukes-run)
(require 'ecukes-stats)
(require 'ecukes-setup)

(ecukes-setup)
;; Send what files to run...!
(ecukes-run)

(ecukes-quit
 (if (> ecukes-stats-steps-failed 0) 1 0))

;;; ecukes-term.el ends here
