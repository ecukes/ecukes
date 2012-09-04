;;; ecukes-setup.el --- Common setup used when running in terminal and
;;; in emacs

(setq debug-on-error t)
(setq debug-on-entry t)

(defvar *ecukes-message-log* (list ""))

(defadvice message  (after ecukes-log-messages-to-buffer
                           activate)
  (when ad-return-value
    (setf (cdr (last *ecukes-message-log*))
          (cons ad-return-value nil))))

(eval-when-compile
  (require 'cl))

(require 'ansi)
(require 'ecukes-template)
(require 'ecukes-new)
(require 'ecukes-def)
(require 'ecukes-startup)
(require 'ecukes-parse)
(require 'ecukes-steps)
(require 'ecukes-run)
(require 'ecukes-print)
(require 'ecukes-hooks)
(require 'ecukes-stats)

(provide 'ecukes-setup)
