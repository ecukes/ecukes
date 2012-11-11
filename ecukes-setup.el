;;; ecukes-setup.el --- Common setup

(eval-when-compile
  (require 'cl))
(require 'dash)
(require 'ansi)
(require 'ecukes-project)
(require 'ecukes-template)

(defun usage ()
  "Show usage information and quit."
  (message
   (ecukes-template-get 'usage))
  (kill-emacs))

(when (or (-contains? argv "-h") (-contains? argv "--help"))
  (usage))

(unless (file-directory-p (ecukes-project-features-path))
  (message
   (ansi-red "Missing `features` directory."))
  (usage))

(provide 'ecukes-setup)
