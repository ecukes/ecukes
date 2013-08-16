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



(provide 'ecukes-reporter)

;;; ecukes-reporter.el ends here
