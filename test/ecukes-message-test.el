(ert-deftest message-add-to-history ()
  (let ((mess "mess..."))
    (quiet-message
     (ecukes-output-message mess))
    (should (member mess ecukes-message-history))))
