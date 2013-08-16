;;; ecukes-reporter.el --- generic reporter interface and helper functions

(require 'f)
(require 's)
(require 'dash)

(require 'ecukes-core)



(defconst ecukes-reporters
  '((dot . "one colored dot per scenario")
    (spec . "full blown spec")
    (landing . "landing plane (not yet implemented)")
    (nyan . "nyan cat (not yet implemented)")
    (progress . "progress bar (not yet implemented)")
    (magnars . "@magnars stripped spec (not yet implemented)")
    (gangsta . "gangsta talk"))
  "List of available reporters, with description.")

(defconst ecukes-reporters-path
  (f-expand "reporters" ecukes-path)
  "Path to reporters directory.")





(defun ecukes-reporter-valid? (reporter)
  "Return if REPORTER is valid or not."
  (let ((reporters (--map (symbol-name (car it)) ecukes-reporters)))
    (-contains? reporters reporter)))

(defun ecukes-reporter-use (reporter)
  "Use REPORTER."
  (unless (ecukes-reporter-valid? reporter)
    (ecukes-fail "Invalid reporter: %s" reporter))
  (let ((full-reporter (format "ecukes-reporter-%s" reporter)))
    (require (intern full-reporter)
             (f-expand full-reporter ecukes-reporters-path))))


(provide 'ecukes-reporter)

;;; ecukes-reporter.el ends here
