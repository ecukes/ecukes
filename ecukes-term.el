;;; ecukes-term.el --- Cucumber for Emacs

(defvar ecukes-path
  (file-name-directory load-file-name)
  "Path to ecukes.")

(add-to-list 'load-path ecukes-path)

(require 'ecukes-run)
(require 'ecukes-setup)

(ecukes-setup)
;; Send what files to run...!
(ecukes-run)

;; TODO:
;; Check for failing steps and return exit code thereafter
(ecukes-quit)

;;; ecukes-term.el ends here
