(setq debug-on-error t)
(setq debug-on-entry t)

(defvar *ecukes-message-log* (list ""))

(defadvice message  (after ecukes-log-messages-to-buffer
                           activate)
  (when ad-return-value
    (setf (cdr (last *ecukes-message-log*))
          (cons ad-return-value nil))))

(defvar ecukes-path
  (file-name-directory load-file-name)
  "Path to ecukes.")

(defvar ecukes-vendor-path
  (expand-file-name "vendor" ecukes-path)
  "Path to ecukes vendor.")

(add-to-list 'load-path ecukes-path)
(add-to-list 'load-path ecukes-vendor-path)

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
